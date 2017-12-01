open Cil
(*open Callgraph *)
open Pretty
open Utils

let do_debug = ref false

type location_variable = LArg of int
                       | LTarget of location_variable
type location_constant = LLoc of int list
                       | LNil
type abstract_location = LLocVar of location_variable
                       | LLocCons of location_constant

let lNilCons = LLocCons(LNil)

type term = TVar of int
          | TDrf of term
          | TAlloc of int list
          | TNil

type pconstraint = CTrue
                 | CFalse
                 | CConjunction of pconstraint * pconstraint
                 | CDisjunction of pconstraint * pconstraint
                 | CNegation of pconstraint
                 | CEquality of term * term

type  bracketing_constraint = pconstraint * pconstraint

let cTruePair = CTrue, CTrue
let cFalsePair = CFalse, CFalse

type abstract_value = (abstract_location * bracketing_constraint)

module OrderedAbstractValue =
  struct
    type t = abstract_value
    let compare = compare
  end
module OrderedAbstractValueSet = Set.Make(OrderedAbstractValue)

type abstract_value_set = OrderedAbstractValueSet.t

module OrderedLocationVariable =
  struct
    type t = location_variable
    let compare = compare
  end
module OrderedLocationVariableSet = Set.Make(OrderedLocationVariable)
module OrderedLocationVariableMap = Map.Make(OrderedLocationVariable)

type location_variable_set = OrderedLocationVariableSet.t

module OrderedAbstractLocation =
  struct
    type t = abstract_location
    let compare = compare
  end
module OrderedAbstractLocationSet = Set.Make(OrderedAbstractLocation)
module OrderedAbstractLocationMap = Map.Make(OrderedAbstractLocation)

type alias_partition_environment = location_variable_set OrderedLocationVariableMap.t

module OrderedVarInfo =
  struct
    type t = Cil.varinfo
    let compare v1 v2 = compare v1.vid v2.vid
  end
module OrderedVarInfoMap = Map.Make(OrderedVarInfo)

type abstract_environment = abstract_location OrderedVarInfoMap.t

type abstract_store = abstract_value_set OrderedAbstractLocationMap.t

type instantiation_environment = abstract_value_set OrderedLocationVariableMap.t

type function_summary = (bracketing_constraint * abstract_store)

module OrderedString = 
  struct 
    type t = string
    let compare = compare
  end
module OrderedStringMap = Map.Make(OrderedString)

type global_summary_environment = function_summary OrderedStringMap.t


let print_int_list (l : int list) : unit =
    print_string (String.concat " :: " (List.map string_of_int l))

let rec print_term (t : term) : unit =
    match t with
    | TVar(
    index) -> Printf.printf "v%d" index
    | TDrf(t') ->
        print_string "drf(";
        print_term t';
        print_string ")"
    | TAlloc(rho_list) ->
        print_string"alloc(";
        print_int_list rho_list;
        print_string ")"
    | TNil -> print_string "nil"

let rec print_pconstraint (phi : pconstraint) : unit =
    match phi with
    | CTrue -> Printf.printf "True"
    | CFalse -> Printf.printf "False"
    | CConjunction(phi1, phi2) -> 
        print_string "(";
        print_pconstraint phi1;
        print_string " /\\ ";
        print_pconstraint phi2;
        print_string ")"
    | CDisjunction(phi1, phi2) ->
        print_string "(";
        print_pconstraint phi1;
        print_string " \\/ ";
        print_pconstraint phi2;
        print_string ")"
    | CNegation(phi1) ->
        print_string "~";
        print_string "(";
        print_pconstraint phi1;
        print_string ")"
    | CEquality(t1, t2) ->
        print_term t1;
        print_string " = ";
        print_term t2

let print_bracketing_constraint (phi : bracketing_constraint) : unit =
    let phi1 = fst phi in
    let phi2 = snd phi in
    print_string "<";
    print_pconstraint phi1;
    print_string ", ";
    print_pconstraint phi2;
    print_string ">"

let rec print_location_variable (alpha : location_variable) : unit =
    match alpha with
    | LArg(index) -> Printf.printf "v%d" index
    | LTarget(alpha') ->
        print_string "*";
        print_location_variable alpha'

let print_location_constant (l : location_constant) : unit =
    match l with
    | LLoc(rho_list) ->
        print_string "LLoc(";
        print_int_list rho_list;
        print_string ")"
    | LNil -> print_string "LNil"

let print_abstract_location (pi : abstract_location) : unit =
    match pi with
    | LLocVar(alpha) -> print_location_variable alpha
    | LLocCons(l) -> print_location_constant l

let print_abstract_value_set (theta : abstract_value_set) : unit =
    let theta_list = ref [] in
    OrderedAbstractValueSet.iter (fun cur_abstract_value -> theta_list := cur_abstract_value :: !theta_list) theta;
    let theta_list_length = List.length !theta_list in
    let i = ref 1 in
    print_string "[";
    List.iter (fun (pi, phi) ->
        print_string "(";
        print_abstract_location pi;
        print_string ", ";
        print_bracketing_constraint phi;
        print_string ")";
        (if !i < theta_list_length
        then print_string ", \n"
        else ());
        i := !i + 1
    ) !theta_list;
    print_string "]"

let print_store_pair (pi : abstract_location) (theta : abstract_value_set) : unit =
    print_abstract_location pi;
    print_string " : ";
    print_abstract_value_set theta

let print_store (s : abstract_store) : unit =
    print_string "{";
    OrderedAbstractLocationMap.iter (fun pi theta ->
        print_string "\n\t";
        print_store_pair pi theta        
    ) s;
    print_string "}"

let print_location_variable_set (alpha_alias_set : location_variable_set) : unit =
    let alpha_alias_list = ref [] in
    OrderedLocationVariableSet.iter (fun cur_location_variable -> 
        alpha_alias_list := cur_location_variable :: !alpha_alias_list) alpha_alias_set;
    let alpha_alias_list_length = List.length !alpha_alias_list in
    let i = ref 1 in
    print_string "[";
    List.iter (fun alpha' ->
        print_location_variable alpha';
        (if !i < alpha_alias_list_length
        then print_string ", "
        else ());
        i := !i + 1
    ) !alpha_alias_list;
    print_string "]"


let print_alias_partition_environment_pair (alpha : location_variable) 
                                           (alpha_alias_set : location_variable_set) : 
                                           unit =
    print_location_variable alpha;
    print_string " : ";
    print_location_variable_set alpha_alias_set

let print_alias_partition_environment (a : alias_partition_environment) : unit =
    print_string "{";
    OrderedLocationVariableMap.iter (fun alpha alpha_alias_set -> 
        print_string "\n\t";
        print_alias_partition_environment_pair alpha alpha_alias_set
    ) a;
    print_string "}"

let print_summary (delta : function_summary) : unit =
    let phi = fst delta in
    let s = snd delta in
    print_string "========================================\n";
    print_string "Precondition phi: ";
    print_bracketing_constraint phi;
    print_newline ();
    print_string "Abstract store S: ";
    print_store s;
    print_string "\n========================================\n\n\n"

let print_summary_environment (g : global_summary_environment) : unit =
    OrderedStringMap.iter (fun function_name delta ->
        print_string function_name;
        print_newline ();
        print_summary delta
    ) g


let rec lift (l : abstract_location) : term = 
    match l with
    | LLocVar(LArg(index)) -> TVar(index)
    | LLocVar(LTarget(alpha)) -> TDrf(lift (LLocVar(alpha)))
    | LLocCons(LNil) -> TNil
    | LLocCons(LLoc(rho_list)) -> TAlloc(rho_list)

let get_location_variable_from_abstract_location (pi : abstract_location) : location_variable =
    match pi with
    | LLocVar(alpha) -> alpha
    | LLocCons(_) -> assert false; LArg(0)

let rec lift_inv (t : term) : abstract_location =
    match t with
    | TVar(index) -> LLocVar(LArg(index))
    | TDrf(t') -> LLocVar(LTarget(get_location_variable_from_abstract_location (lift_inv t')))
    | TNil -> lNilCons
    | TAlloc(rho_list) -> LLocCons(LLoc(rho_list))

let get_function_name (func_exp : exp) : string = 
  match func_exp with 
  | Lval(Var(variable_info), _) -> variable_info.vname
  | _ -> assert false; ""

let bracketing_constraint_conjunction (phi1 : bracketing_constraint) 
                                      (phi2 : bracketing_constraint) : 
                                      bracketing_constraint =
    CConjunction(fst phi1, fst phi2), CConjunction(snd phi1, snd phi2)

let bracketing_constraint_disjunction (phi1 : bracketing_constraint) 
                                      (phi2 : bracketing_constraint) : 
                                      bracketing_constraint =
    CDisjunction(fst phi1, fst phi2), CDisjunction(snd phi1, snd phi2)

let bracketing_constraint_negation (phi : bracketing_constraint) :
                                   bracketing_constraint =
    CNegation(snd phi), CNegation(fst phi)

let pconstraint_to_bracketing_constraint (phi : pconstraint) :
                                         bracketing_constraint =
    phi, phi

let domain_extend (s1 : abstract_store) (s2 : abstract_store) : abstract_store =
    let dom_s1 = OrderedAbstractLocationMap.fold (fun pi _ acc -> 
        OrderedAbstractLocationSet.add pi acc) s1 OrderedAbstractLocationSet.empty in
    OrderedAbstractLocationMap.fold (fun pi theta acc ->
        let pi_is_in_s1 = OrderedAbstractLocationSet.mem pi dom_s1 in
        let s1_pi = if pi_is_in_s1 then OrderedAbstractLocationMap.find pi s1 else OrderedAbstractValueSet.empty in
        let pi_bindings = OrderedAbstractValueSet.fold (fun (pi_i, _) acc ->
            if pi_is_in_s1
            then
                let pi_i_binding_in_s1 = OrderedAbstractValueSet.fold (fun (pi_i', phi) acc ->
                    if acc <> None 
                    then acc
                    else
                        if pi_i = pi_i'
                        then Some(phi)
                        else None
                ) s1_pi None in
                match pi_i_binding_in_s1 with
                | None -> OrderedAbstractValueSet.add (pi_i, cFalsePair) acc
                | Some(pi_i_binding_in_s1) -> OrderedAbstractValueSet.add (pi_i, pi_i_binding_in_s1) acc
            else OrderedAbstractValueSet.add (pi_i, cFalsePair) acc
        ) theta s1_pi in
        OrderedAbstractLocationMap.add pi pi_bindings acc
    ) s2 s1

let join_abstract_store (s1 : abstract_store) (s2 : abstract_store) : abstract_store = 
    let s1' = domain_extend s1 s2 in
    let s2' = domain_extend s2 s1 in
    let merger pi theta1 theta2 =
        match theta1, theta2 with
        | Some(theta1), Some(theta2) ->
            Some (OrderedAbstractValueSet.fold (fun (pi1, phi1) acc -> 
                let matching_subset_of_theta2 = OrderedAbstractValueSet.fold (fun (pi2, phi2) acc ->
                    if pi1 = pi2
                    then 
                        let phi_may1 = fst phi1 in
                        let phi_must1 = snd phi1 in
                        let phi_may2 = fst phi2 in
                        let phi_must2 = snd phi2 in
                        let phi_may = CDisjunction(phi_may1, phi_may2) in
                        let phi_must = CConjunction(phi_must1, phi_must2) in
                        let phi = phi_may, phi_must in
                        OrderedAbstractValueSet.add (pi1, phi) acc
                    else acc ) theta2 OrderedAbstractValueSet.empty in
                OrderedAbstractValueSet.union matching_subset_of_theta2 acc) theta1 OrderedAbstractValueSet.empty)
        | _, _ -> Printf.printf "Invariant broken: s1 and s2 have different domains\n"; exit 1; None
    in 
    OrderedAbstractLocationMap.merge merger s1' s2'

let conjoin_abstract_value_set_and_constraint (theta : abstract_value_set) (phi : bracketing_constraint) : abstract_value_set =
    OrderedAbstractValueSet.map (fun (pi_i, phi_i) -> pi_i, bracketing_constraint_conjunction phi_i phi) theta

let look_up_store_theta (theta : abstract_value_set) (s : abstract_store) : abstract_value_set =
    OrderedAbstractValueSet.fold (fun (pi_i, phi_i) acc -> 
        let s_pi_i = OrderedAbstractLocationMap.find pi_i s in
        let s_pi_i_conjunct_phi_i = conjoin_abstract_value_set_and_constraint s_pi_i phi_i in
        OrderedAbstractValueSet.union acc s_pi_i_conjunct_phi_i) theta OrderedAbstractValueSet.empty

let rec get_order_of_location_variable (alpha : location_variable) : int =
    match alpha with
    | LArg(index) -> index
    | LTarget(alpha') -> get_order_of_location_variable alpha'

let total_order_le (alpha1 : location_variable) (alpha2 : location_variable) : bool =
    (get_order_of_location_variable alpha1) <= (get_order_of_location_variable alpha2)

let total_order_lt (alpha1 : location_variable) (alpha2 : location_variable) : bool =
    (get_order_of_location_variable alpha1) < (get_order_of_location_variable alpha2)

let initialize_heap (formals : varinfo list) 
                    (a : alias_partition_environment) : 
                    abstract_environment * abstract_store = 
    let e, _ = List.fold_left (fun (acc, index) var -> OrderedVarInfoMap.add var (LLocVar (LArg index)) acc, index + 1) (OrderedVarInfoMap.empty, 1) formals in
    let s = OrderedLocationVariableMap.fold (fun alpha_i alpha_i_alias_set acc ->
        let s_alpha_i_value = OrderedLocationVariableSet.fold (fun alpha_k acc ->
            if total_order_le alpha_k alpha_i
            then begin
                let points_to_target = LTarget alpha_k in
                let phi = CEquality(lift (LLocVar (LTarget alpha_i)), lift (LLocVar (LTarget alpha_k))) in
                let phi' = OrderedLocationVariableSet.fold (fun alpha_j acc ->
                    if total_order_lt alpha_j alpha_k
                    then CConjunction(CNegation(CEquality(lift (LLocVar (LTarget alpha_i)), lift (LLocVar (LTarget alpha_j)))), acc)
                    else acc
                ) alpha_i_alias_set phi in
                OrderedAbstractValueSet.add (LLocVar points_to_target, pconstraint_to_bracketing_constraint phi') acc end
            else acc) alpha_i_alias_set OrderedAbstractValueSet.empty in
        OrderedAbstractLocationMap.add (LLocVar alpha_i) s_alpha_i_value acc
    ) a OrderedAbstractLocationMap.empty in
    e, s

let get_location_variables_at_all_levels (v : varinfo) (index : int) : location_variable_set =
    let rec helper t acc s = 
        (match t with
        | TPtr(t', _) -> helper t' (LTarget acc) (OrderedLocationVariableSet.add acc s)
        | TInt(_, _) -> s
        | _ -> assert false; s
        ) in
    helper v.vtype (LArg index) OrderedLocationVariableSet.empty

let rec get_location_variable_level (alpha : location_variable) : int = 
    match alpha with
    | LTarget(alpha') -> 1 + get_location_variable_level alpha'
    | LArg(_) -> 0

let get_location_variable_type (alpha : location_variable) : int = 
    get_location_variable_level alpha

let location_variable_same_type (alpha1 : location_variable) (alpha2: location_variable) : bool =
    (get_location_variable_type alpha1) = (get_location_variable_type alpha2)

let get_location_variables_of_same_type (alpha : location_variable) (a : alias_partition_environment) : location_variable_set =
    OrderedLocationVariableMap.fold (fun alpha' _ acc ->
        if location_variable_same_type alpha alpha'
        then OrderedLocationVariableSet.add alpha' acc
        else acc
    ) a OrderedLocationVariableSet.empty

let compute_alias_partition_environment (formals : varinfo list) : alias_partition_environment =
    fst (List.fold_left (fun (acc, index) cur_formal ->
            let location_variables = get_location_variables_at_all_levels cur_formal index in
            let acc' = OrderedLocationVariableSet.fold (fun alpha acc ->
                let alpha_added_acc = OrderedLocationVariableMap.add alpha (OrderedLocationVariableSet.singleton alpha) acc in
                let alpha_alias_set = get_location_variables_of_same_type alpha acc in
                OrderedLocationVariableSet.fold (fun alpha_alias acc ->
                    let alpha_value = OrderedLocationVariableMap.find alpha acc in
                    let alpha_value' = OrderedLocationVariableSet.add alpha_alias alpha_value in
                    let alpha_alias_value = OrderedLocationVariableMap.find alpha_alias acc in
                    let alpha_alias_value' = OrderedLocationVariableSet.add alpha alpha_alias_value in
                    let temp = OrderedLocationVariableMap.add alpha alpha_value' acc in
                    OrderedLocationVariableMap.add alpha_alias alpha_alias_value' temp
                ) alpha_alias_set alpha_added_acc
            ) location_variables acc in
            acc', index + 1
        ) (OrderedLocationVariableMap.empty, 1) formals)

let get_pointer_from_pointee (star_alpha : location_variable) : location_variable =
    match star_alpha with
    | LArg(_) -> Printf.printf "Error: Cannot extract the pointer from a non-pointee\n"; exit 1; star_alpha
    | LTarget(alpha) -> alpha

let get_argument_var_list (argument_list : exp list) : varinfo list =
    List.map (fun cur_argument_exp ->
        match cur_argument_exp with
        | Lval(Var(var), NoOffset) -> var
        | _ -> assert false; makeVarinfo false "" (TVoid [])
    ) argument_list

let rec map_loc_helper (star_alpha : location_variable) 
                       (star_alpha_t : typ) 
                       (s : abstract_store) 
                       (i : instantiation_environment) : 
                       instantiation_environment =
    match star_alpha_t with
    | TInt(_, _) ->
        let i_alpha = OrderedLocationVariableMap.find (get_pointer_from_pointee star_alpha) i in
        let s_i_alpha = look_up_store_theta i_alpha s in
        OrderedLocationVariableMap.add star_alpha s_i_alpha i
    | TPtr(star_alpha_t', _) ->
        let i_alpha = OrderedLocationVariableMap.find (get_pointer_from_pointee star_alpha) i in
        let s_i_alpha = look_up_store_theta i_alpha s in
        let i' = OrderedLocationVariableMap.add star_alpha s_i_alpha i in
        map_loc_helper (LTarget star_alpha) star_alpha_t' s i'
    | _ -> assert false ; i

let map_loc (nu : location_variable) 
            (nu_t : typ) 
            (s : abstract_store) 
            (i : instantiation_environment) : 
            instantiation_environment =
    match nu_t with
    | TInt(_, _) -> i
    | TPtr(nu_t', _) -> map_loc_helper (LTarget nu) nu_t' s i
    | _ -> assert false; i

let map_args (arguments : varinfo list) 
             (e : abstract_environment) 
             (s : abstract_store) : 
             instantiation_environment =
    fst (List.fold_left (fun (acc, index) cur_argument ->
            let nu_i = LArg index in
            let nu_i_t = cur_argument.vtype in
            let e_v_i = OrderedVarInfoMap.find cur_argument e in
            let nu_i_value = OrderedAbstractValueSet.singleton (e_v_i, cTruePair) in
            let i = OrderedLocationVariableMap.add nu_i nu_i_value acc in
            let i' = map_loc nu_i nu_i_t s i in
            i', index + 1
        ) (OrderedLocationVariableMap.empty, 1) arguments)

let inst_loc (pi : abstract_location) 
             (i : instantiation_environment) 
             (rho : int) : 
             abstract_value_set =
    match pi with
    | LLocVar(alpha) -> OrderedLocationVariableMap.find alpha i
    | LLocCons(LNil) -> OrderedAbstractValueSet.singleton (lNilCons, cTruePair)
    | LLocCons(LLoc(rho_list)) ->
        if List.mem rho rho_list
        then OrderedAbstractValueSet.singleton (pi, (CTrue, CFalse))
        else OrderedAbstractValueSet.singleton (LLocCons(LLoc(rho :: rho_list)), cTruePair)

let inst_phi (phi : bracketing_constraint)
             (i : instantiation_environment)
             (rho : int) :
             bracketing_constraint =
    cTruePair
    (*let phi_may = fst phi in
    let phi_must = snd phi in
    let phi_may', phi_may'' = inst_phi_helper phi_may in
    let phi_must', phi_must'' = inst_phi_helper phi_must in
    let phi_may_result = quantifier_elimination CConjunction()*)

let inst_theta (theta : abstract_value_set)
               (i : instantiation_environment)
               (rho :int) :
               abstract_value_set =
    OrderedAbstractValueSet.fold (fun (pi_i, phi_i) acc ->
        let theta_i = inst_loc pi_i i rho in
        let theta_i_and_phi_i = conjoin_abstract_value_set_and_constraint theta_i phi_i in
        OrderedAbstractValueSet.union theta_i_and_phi_i acc
    ) theta OrderedAbstractValueSet.empty

let is_allocation_location (pi : abstract_location) : bool =
    match pi with
    | LLocCons(LLoc _) -> true
    | _ -> false

let compose_partial_heap (pi : abstract_location) 
                         (theta : abstract_value_set)
                         (s : abstract_store)
                         (i : instantiation_environment)
                         (rho : int) :
                         abstract_store =
    let theta_s = inst_loc pi i rho in
    let theta_t = inst_theta theta i rho in
    OrderedAbstractValueSet.fold (fun (pi_i, phi_i) acc ->
        let phi_i_set = conjoin_abstract_value_set_and_constraint theta_t phi_i in
        let s_pi_i = if is_allocation_location pi_i then OrderedAbstractValueSet.empty else OrderedAbstractLocationMap.find pi_i s in
        let not_phi_i_set = conjoin_abstract_value_set_and_constraint s_pi_i (bracketing_constraint_negation phi_i) in
        let pi_i_value_set = OrderedAbstractValueSet.union phi_i_set not_phi_i_set in
        OrderedAbstractLocationMap.add pi_i pi_i_value_set acc
    ) theta_s s

let compose_heap (s_f : abstract_store) 
                 (s : abstract_store)
                 (i : instantiation_environment)
                 (rho : int) :
                 abstract_store =
    OrderedAbstractLocationMap.fold (fun pi_i theta_i acc ->
        compose_partial_heap pi_i theta_i acc i rho
    ) s_f s

let rec transform_instruction (instruction : instr)
                              (e : abstract_environment)
                              (s : abstract_store)
                              (g : global_summary_environment)
                              (phi : bracketing_constraint) :
                              abstract_store * bracketing_constraint = 
    match instruction with
    (* Simple assignment *)
    | Set((Var(var1), NoOffset), Lval(Var(var2), NoOffset), _) ->
        let pi1 = OrderedVarInfoMap.find var1 e in
        let pi2 = OrderedVarInfoMap.find var2 e in
        let theta = OrderedAbstractLocationMap.find pi2 s in
        let s' = OrderedAbstractLocationMap.add pi1 theta s in
        s', phi
    (* Load *)
    | Set((Var(var1), NoOffset), Lval(Mem(Lval(Var(var2), NoOffset)), NoOffset), _) ->
        let pi1 = OrderedVarInfoMap.find var1 e in
        let pi2 = OrderedVarInfoMap.find var2 e in
        let theta = OrderedAbstractLocationMap.find pi2 s in
        let s_theta = look_up_store_theta theta s in
        let s' = OrderedAbstractLocationMap.add pi1 s_theta s in
        s', phi
    (* Store *)
    | Set((Mem(Lval(Var(var1), NoOffset)), NoOffset), Lval(Var(var2), NoOffset), _) ->
        let pi1 = OrderedVarInfoMap.find var1 e in
        let pi2 = OrderedVarInfoMap.find var2 e in
        let theta1 = OrderedAbstractLocationMap.find pi1 s in
        let theta2 = OrderedAbstractLocationMap.find pi2 s in
        let s' = OrderedAbstractValueSet.fold (fun (pi_i, phi_i) acc ->
            let phi_i_set = conjoin_abstract_value_set_and_constraint theta2  phi_i in
            let s_pi_i = OrderedAbstractLocationMap.find pi_i s in
            let not_phi_i_set = conjoin_abstract_value_set_and_constraint s_pi_i (bracketing_constraint_negation phi_i) in
            let pi_i_value_set = OrderedAbstractValueSet.union phi_i_set not_phi_i_set in
            OrderedAbstractLocationMap.add pi_i pi_i_value_set acc
        ) theta1 s in
        s', phi
    (* Type cast *)
    | Set(lvalue, CastE(_, rhs_exp), loc) ->
        let instruction_no_cast = Set(lvalue, rhs_exp, loc) in
        transform_instruction instruction_no_cast e s g phi
    | Call(lvalue, func_exp, argument_list, loc) ->
        let callee_name = get_function_name func_exp in
        begin
        match callee_name with 
        | "alias_assert" -> s, phi 
        | "malloc" ->
            begin
            match lvalue with
            | None -> s, phi
            | Some(Var(var), NoOffset) ->
                let pi = OrderedVarInfoMap.find var e in
                let loc_rho = LLocCons(LLoc([loc.byte])) in
                let s' = OrderedAbstractLocationMap.add pi (OrderedAbstractValueSet.singleton (loc_rho, cTruePair)) s in
                let s' = OrderedAbstractLocationMap.add loc_rho (OrderedAbstractValueSet.singleton (lNilCons, cTruePair)) s' in
                s', phi
            | _ -> assert false; s, phi
            end
        (* Regular function calls TODO: extern functions *)
        | _ ->
            match lvalue with
            | Some(_, _) -> Printf.printf "Error: Lvalue of function calls not supported\n"; exit 1; s, phi
            | None ->
                let phi_f, s_f = OrderedStringMap.find callee_name g in
                let argument_var_list = get_argument_var_list argument_list in
                let i = map_args argument_var_list e s in
                let phi_f' = inst_phi phi_f i loc.byte in
                let s' = compose_heap s_f s i loc.byte in
                s', bracketing_constraint_conjunction phi phi_f'
        end
    | _ -> s, phi
and transform_statement (statement : stmt) 
                        (e : abstract_environment)
                        (s : abstract_store)
                        (g : global_summary_environment)
                        (phi : bracketing_constraint) :
                        abstract_store * bracketing_constraint =
    match statement.skind with
    | Instr(il) -> 
        List.fold_left (fun acc cur_instruction -> transform_instruction cur_instruction e (fst acc) g (snd acc)) (s, phi) il
    | If(_, block1, block2, _) ->
        let s1, phi1 = transform_block block1 e s g phi in
        let s2, phi2 = transform_block block2 e s g phi in
        (join_abstract_store s1 s2), (bracketing_constraint_conjunction phi1 phi2)
    | _ -> s, phi
and transform_block (blck : block)
                    (e : abstract_environment)
                    (s : abstract_store)
                    (g : global_summary_environment)
                    (phi : bracketing_constraint) :
                    abstract_store * bracketing_constraint =
    List.fold_left (fun acc statement -> transform_statement statement e (fst acc) g (snd acc)) (s, phi) blck.bstmts
and transform_function_body (f : fundec)
                            (e : abstract_environment)
                            (s : abstract_store)
                            (g : global_summary_environment)
                            (phi : bracketing_constraint) :
                            abstract_store * bracketing_constraint =
    let e', s', _ = List.fold_left (fun (e_acc, s_acc, temp_index) cur_local -> 
        let rho = cur_local.vdecl.byte in
        let rho' = if rho = -1 then temp_index else rho in
        let cur_local_location = LLocCons(LLoc [rho']) in
        OrderedVarInfoMap.add cur_local cur_local_location e_acc, 
        OrderedAbstractLocationMap.add cur_local_location (OrderedAbstractValueSet.singleton (lNilCons, cTruePair)) s_acc,
        if rho = -1 then temp_index - 1 else temp_index
    ) (e, s, -1) f.slocals in
    transform_block f.sbody e' s' g phi



let generate_summary (f : fundec) 
                     (a : alias_partition_environment) 
                     (g : global_summary_environment) : 
                     function_summary =
    let a = compute_alias_partition_environment f.sformals in
    let e, s = initialize_heap f.sformals a in
    let rec generate_summary_helper phi_f s_f g_f =
        let s', phi' =  transform_function_body f e s_f g_f phi_f in
        phi', s'
        (*if true
        (*if contraint_implication phi phi' && abstract_store_contains s s'*)
        then phi_f, s_f
        else phi_f, s_f
            (* let phi'' = bracketing_constraint_conjunction phi_f, phi' in
            let s'' = join_abstract_store s_f s' in
            let g' = OrderedStringMap.add f.svar.vname s'' in
            generate_summary_helper phi'' s'' f e g*)*)
    in
    generate_summary_helper cTruePair s g


let generate_summary_for_partition (partition : fundec list) 
                                   (a : alias_partition_environment) 
                                   (g : global_summary_environment) :
                                   global_summary_environment =
    let head = List.hd partition in
    let delta = generate_summary head a g in
    OrderedStringMap.singleton head.svar.vname delta


let rec toposort (edges : (string * string) list) (acc : string list list) : string list list = 
  if (List.length edges) = 0
  then acc
  else let vertices_with_duplicates = List.fold_left (fun acc (n1, n2) -> acc @ [n1; n2]) [] edges in 
       let vertices_without_duplicates = List.fold_left (fun acc n -> if List.mem n acc then acc else n :: acc) [] vertices_with_duplicates in 
       let indegree_is_zero n edges = List.fold_left (fun acc (n1, n2) -> if n2 = n then false else acc) true edges in
       let vertices_with_zero_indegree = List.fold_left (fun acc n -> if indegree_is_zero n edges then n :: acc else acc) [] vertices_without_duplicates in 
       if (List.length vertices_with_zero_indegree) = 0
       then acc (* FIXEME *)
       else let vertex_to_be_removed = List.hd vertices_with_zero_indegree in 
            toposort (List.filter (fun n -> fst n <> vertex_to_be_removed) edges) ([vertex_to_be_removed] :: acc)

(*let get_call_graph_partitions_in_rev_topo_order (cur_file : file) : string list list =
  let call_graph = computeGraph cur_file in
  let get_callee_name_list callees = Inthash.fold (fun _ node acc -> (nodeName node.cnInfo) :: acc) callees [] in   
  let edges = ref [] in 
  Hashtbl.iter (fun cur_node_name node -> 
    let callee_name_list = get_callee_name_list node.cnCallees in 
    let new_edges = List.map (fun callee_name -> (cur_node_name, callee_name)) callee_name_list in 
    edges := !edges @ new_edges
  ) call_graph;
  toposort !edges []
*)

let get_function_by_name (function_name : string) (cur_file : file) : fundec =
    let f = List.fold_left (fun acc cur_global -> 
        if acc <> None 
        then acc 
    else 
        begin
        match cur_global with 
        | GFun(cur_function, _) -> if cur_function.svar.vname = function_name then Some(cur_function) else acc
        | _ -> acc
        end) None cur_file.globals in
    match f with
    | None -> Printf.printf "Error: Function not found by name: %s\n" function_name; exit 1; dummyFunDec
    | Some(f') -> f'

let do_heap_analysis (cur_file : file) : unit =
    let partitions =[["main"]; ["foo"]; ["bar"]] (*get_call_graph_partitions_in_rev_topo_order cur_file*) in
    let summary_environment = List.fold_left (fun acc partitions ->
        let functions = List.map (fun function_name -> get_function_by_name function_name cur_file) partitions in
        let new_summary_environment = generate_summary_for_partition functions OrderedLocationVariableMap.empty acc in
        OrderedStringMap.union (fun _ _ _ -> Printf.printf "Error: Repeating function summaries\n"; exit 1; None) acc new_summary_environment
    ) OrderedStringMap.empty partitions in
    print_summary_environment summary_environment

let main () = begin
    let usage   = "Usage: " ^ Sys.argv.(0) ^ " [options] filename" in
    let options =
        [ "--debug", Arg.Set(do_debug), " enable debugging messages"; ]
    in
    let options = Arg.align options in
    let args = ref [] in
    Arg.parse options (fun x -> args := x :: !args) usage;
    let filename =
    match List.rev !args with
    | [filename] -> filename
    | _ ->
        debug "pointer: specify a pre-preprocessed C file\n" ;
        Arg.usage options usage;
        exit 1 
    in

    let file = load_c_file filename in

    do_heap_analysis file
end ;;

main () ;;

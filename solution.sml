structure Dictionary :> DICTIONARY =
struct
exception NotImplemented

    type (''key, 'value) dict = (''key * 'value) list

    val empty = []

    fun exists dict key = List.exists (fn (n, v) => (n = key)) dict

    fun isEmpty dict  =  case (dict) of [] => true
	|  x::xs => false 
    
	fun size dict = List.length dict 
    (* Returns SOME value corresponding to the specified key
       or NONE if the key doesn't exist. *)
	fun get dict key = if isSome(List.find (fn (n, v)=> (n =key)) dict) 
							then  SOME (#2 (valOf(List.find (fn (n, v)=> (n =key)) dict)))
						else NONE
						
						

							
	(* Returns the value corresponding to the specified key
       or a default value if the key doesn't exist. 

 *)
	   
    fun getOrDefault dict key default = if isSome(List.find (fn (n, v)=> (n =key)) dict) 
											then  #2 (valOf(List.find (fn (n, v)=> (n =key)) dict))
										else default
						
	
	   (* Stores the (key, value) pair and returns the updated dictionary.
       If a pair with the specified key already exists, then it overwrites
       the existing value. *)

    fun set dict key value = 
	let
	fun setAux checked ((k, v) :: others) =
	    if k  = key then
	      (k, value) :: others
	    else
	      setAux ((k, v) :: checked) others
	  | setAux checked [] = (key, value) :: checked
      in
	(setAux [] dict)
      end;
	  
	  
    fun remove dict key = 
	 let
        fun removeAux checked ((k, v) :: others) =
            if k = key
            then removeAux checked others
	    else removeAux ((k, v) :: checked) others
	  | removeAux checked [] = checked
      in
	(removeAux [] dict )
      end
	
	(* Returns a list of keys in the dictionary.
	stdIn:81.5-81.27 Warning: type vars not generalized because of
   value restriction are instantiated to dummy types (X1,X2,...)
val m1 = [] : ?.X1 list
	*)
    fun keys ((k, v)::xs) =  k:: (keys(xs))
		| keys (empty)  =  []
	 
    fun values ((k, v)::xs ) = v:: values(xs)
		| values ([]) = []
    
	(* Returns a list of (key, value) pairs in the dictionary. *)
	fun toList dict = dict
	
    
	    (* Creates a dictionary from a list of (key, value) pairs.
       If there are multiple pairs with the same key,
       the rightmost value overrides all previous values. *)
	fun fromList m = 
	let 
		fun aux( ((k,v)::xs), acc )= aux ( xs,  set acc k v )
			| aux( [] , acc) = acc 

	in 
	 aux ( m , empty)
	 end 
	 
	(* Overrides or adds (key, value) pairs from the right dictionary into the left one. *)
    fun merge a dict = 
	let 
		fun aux( ((k,v)::xs)  , acc )=aux ( xs,  set acc k v )
		| aux ([], acc) = acc

	in 
	 aux ( a , [])
	 end 
	 
	(* Keeps only the (key, value) pairs for which the specified function returns true. Something wrong  map (fn (k, v) => (f k, f v)) dict*)
    fun filter f dict  =  List.filter f dict
	
	fun map  f dict  = List.map f dict 

end

structure Cookbook :> COOKBOOK =
struct
    type ingredient = string
    type stock = (ingredient, int) Dictionary.dict
    type pricelist = (ingredient, real) Dictionary.dict
    type recipe = ingredient * stock  (* that means  "reciptr" [()]*)
    type cookbook = (ingredient, stock) Dictionary.dict (* this means [( strng , []) , .....]*)

    exception NoPriceException
	exception NotImplemented

    fun makeIngredient name = name 
	
    (* Create a stock from a list of (ingredient, quantity) pairs. *)
    fun makeStock ingredients  = Dictionary.fromList ingredients
	
	    (* Create a price list from a list of (ingredient, price) pairs. *)
    fun makePricelist pricelist = Dictionary.fromList pricelist
	    (* Create a recipe from a name and a stock of ingredients. *)
    fun makeRecipe (x: ingredient , y:stock) =  ( x , y)
	 (* Create a cookbook from a list of recipes. *)
    fun makeCookbook recipes= Dictionary.fromList recipes
	
	  (* Print the ingredient to a string. *)
    fun ingredientToString ingredient = ingredient
	
	    (* Print the stock to a string in the following form:

           firstIngredientName: firstIngredientQuantity
           secondIngredientName: secondIngredientQuantity
           ...: ...

       Ingredients have to be sorted alphabetically (ASCII order) 
       Don't print lines with negative or zero values.
       Don't forget newlines.  List.foldl ( fn (acc, x)=> ) " " (Dictionary.map (fn (x,y)=> (x , x ^ " : " ^ Int.toString y ^ "\n") stock)
ListMergeSort.sort (fn(x,y)=>(x>y)) Dictionary.filter (fn (x, y) => (y>0) )
	   *)
	   fun fold  f  acc xs = case xs of 
								[]=> acc 
							| (k,v)::t => fold  f (f(acc, (k,v))) t  
(*							
		  fun foldRecipe  f  acc xs = case xs of 
								[]=> acc 
							| (s , (k,v)::t) => fold  f (f(acc, (s ,(k,v)))) t  *)
							
	    fun stockToString stock = fold (fn (acc , (k,v))=>  acc ^ k ^ " : " ^ Int.toString v ^ "\n") " " 
								(ListMergeSort.sort (fn((x1,y1), (x2,y2))=>(x1>x2)) 
										(Dictionary.toList 
											(Dictionary.filter (fn (x, y) => (y>0))stock)))


		
    (* Print the recipe to a string in the following form:

           === recipeName ===
           firstIngredientName: firstIngredientQuantity
           secondIngredientName: secondIngredientQuantity
           ...: ...

       Ingredients have to be sorted alphabetically (ASCII order).
       Don't forget newlines. *)
    fun pricelistToString pricelist =  fold (fn (acc , (k,v))=>  acc ^ k ^ " : " ^ Real.toString v ^ "\n" ) " " 
								(ListMergeSort.sort (fn((x1,y1), (x2,y2))=>(x1>x2)) 
										(Dictionary.toList pricelist))
											
											
	    (* Print the recipe to a string in the following form:

           === recipeName ===
           firstIngredientName: firstIngredientQuantity
           secondIngredientName: secondIngredientQuantity
           ...: ...
		   
		   foldRecipe (fn (acc ,(s, (x1 , y1)))	=>  acc ^" ====== " ^ s ^" ====== "^ "\n" ^  x1 ^ " : " ^ Real.toString y1 ^ "\n" ) " " 
								( ListMergeSort.sort (fn(( s1 ,(x1,y1)), ( s2 ,(x2,y2)))=>(x1>x2)) 
										(Dictionary.toList  y )	)
	

       Ingredients have to be sorted alphabetically (ASCII order).
       Don't forget newlines. *)										
    fun recipeToString  (x: ingredient , y:stock)  =  "=============="^ x ^ "===============" ^ 
							fold (fn (acc , (k,v))=>  acc ^ k ^ " : " ^ Int.toString v ^ "\n" ) " " 
									(ListMergeSort.sort (fn((x1,y1), (x2,y2))=>(x1>x2)) 
												(Dictionary.toList y))
    (* Print the cookbook to a string in the following form:

           === firstRecipeName ===
           firstIngredientName: firstIngredientQuantity
           secondIngredientName: secondIngredientQuantity
           ...: ...

           === secondRecipeName ===
           firstIngredientName: firstIngredientQuantity
           secondIngredientName: secondIngredientQuantity
           ...: ...
		   
		   (ListMergeSort.sort (fn((x1,y1), (x2,y2))=>(x1>x2)) 
												(Dictionary.toList cookbook)) 
												case cookbook of 
										 empty => " "
										| r::t => recipeToString ( r) ^ cookbookToString(t)

       Recipes have to be sorderd alphabetically (ASCII order).
       Ingredients in each recipe have to be sorted
       alphabetically (ASCII order).
       Don't forget newlines. *)
    fun cookbookToString cookbook =  let 
										val lsSortDict = ListMergeSort.sort (fn ((x1, y1),(x2, y2))=> (x1>x2)) (Dictionary.toList (cookbook))
										fun auxMain ( lsSortDict , acc) = 
										 case  lsSortDict  of 
											[]=> acc 
											| (k, v)::t => auxMain ( t , acc^recipeToString (k,v))
										
										in auxMain(lsSortDict , "")
										end 
										
										
	   (* Returns true if there's enough ingredients in the stock
       for the specified recipe. 
	   The exercise requires you to check if there is enough ingredients in the stock for the specific recipe. 
You have to go through the recipe and check for the required quantity in the stock.
https://stackoverflow.com/questions/32491517s/why-does-i-to-get-elements-in-tuples-work-on-command-line-but-not-program?rq=1

	   *)
	   fun auxFirst(x,v) = x
	   fun auxSecond (x , v) = v 
	   fun auxCheck (ingredient , value) stock = List.exists (fn (k,v)=> ((k=ingredient) andalso (v >=value)) ) (Dictionary.toList ( stock ))
	   fun hasEnoughIngredients stock  recipe =  case ( auxFirst(recipe) , (Dictionary.toList (auxSecond( recipe))) )of
											( _ ,[])=> false
											|( _ ,(k,v)::[]) =>  auxCheck (k,v) stock
											|( m, (k,v)::t ) =>  if auxCheck (k,v) stock then hasEnoughIngredients stock (m , (Dictionary.fromList t))  else false
											
											
	    (* Cooks the ingredients in the recipe and returns
       an updated stock with the product of the recipe added
       and its ingredients removed.

let 
							fun auxMain ( lsStock , lsRecipe, acc ) =
							case (Dictionary.toList recipe, Dictionary.toList stock ) of 
								([], [] )=> acc 
								| ((s, m)::n , (k,v)::t) => if auxCheck ( (k,v) m) then auxMain( Dictionary.toList recipe , t , Dictionary.fromList [(k, v-m)]@acc )
															else auxMain( Dictionary.toList recipe , t , Dictionary.fromList [(k, v)]@acc )
								
							in auxMain(Dictionary.toList (stock) , Dictionary.toList (recipe)  , Dictionary.empty)
							end 
							
	   *)
    fun cook (ing , st) stock =  if (hasEnoughIngredients stock (ing , st ) ) = false  (*This function works fine! *)
								then  
								(* Here I need one additional function that will *)
										Dictionary.map( fn (k, v)=> (k ,~v ) )  (Dictionary.set st ing 1 )  (* This doesn't work !! Why? *)
							else  	let fun aux (lsRecipeStock , acc ) = 
									(*val  s = Dictionary.set stock ing  1 *)
									case lsRecipeStock of 
									[]=> acc 
									| (k, v)::t => aux ( t , Dictionary.set  stock k  ((valOf(Dictionary.get stock k)) - v)) (* In test result I saw that this part he didn't call recursively. Why? *)

									in 
							
									 aux( Dictionary.toList st  , Dictionary.set stock ing 1 ) 
									
									
									end 
								



	fun auxCheckPrice (ingredient , value) pricelist = List.exists (fn (k,v)=> ((k=ingredient)) ) (Dictionary.toList ( pricelist ))
	   

		
	fun FindQuality pricelist ingredient = let 
												fun Aux ( (k,v )::t , acc ) =if k=ingredient then v else Aux(t, acc) 
												|	Aux([], acc )= acc 
										
										in Aux (Dictionary.toList pricelist, 0.0) 
										end 
							
    fun priceOfStock stock pricelist =  let
											fun auxMain ( lsStock  , acc)= 
											case  lsStock of 
												[] =>  acc
											| (k,v)::t => if auxCheckPrice (k,v) pricelist then  auxMain ( t ,  acc + ((Real.fromInt v) * (FindQuality pricelist k ) ))
											else raise NoPriceException
											
										 in 
											auxMain( (Dictionary.toList (stock)) , 0.0)
										end 
	
	  (* Returns the price of a recipe - the sum of the prices of
       the ingredients according to the specified pricelist. If
       an ingredient is not on the price list, the function should
       raise NoPriceException. *)
    fun priceOfRecipe recipe pricelist = priceOfStock (auxSecond recipe) pricelist
	
	(* Returns the number of ingredients that are missing for
       a given recipe. If there's enough ingredients available
       in the stock, it returns an empty stock. *)
	   
	(*fun filter (f , xs ) = 
							case xs of 
								[]=>[]
								| (k,v)::t=>  if f(k,v) then (k,v)::filter(f , t) else filter(f , t)
								
	fun  stock ingredient = let 
											fun Aux ( lsStock , acc )=
												case lsStock of 
													[]=> acc
													| (k,v )::t => if k=ingredient then v else Aux(t, acc) 

													in Aux (Dictionary.toList(stock), 0) 
										end 
		*)					
		(* Something like Dictionary.get 
			But I'm really not motivated to optimize something that doesn't work plus I didn't see the reason "why" !!!! :( 
		*)
	fun FindValueInt stock ingredient = let 
											fun Aux ( (k,v )::t , acc )=if k=ingredient then v else Aux(t, acc) 
											| Aux([], acc ) = acc  
											
										in Aux (Dictionary.toList (stock) , 0) 
										end
										
	fun auxFilterMissing ( (k,v) , stock , output ) = let 
													val OutputDict = Dictionary.fromList (output)
													in 
													case  Dictionary.toList (stock) of 
													[] => Dictionary.toList ( Dictionary.set OutputDict k (~v))
													| (m, n)::s => if Dictionary.exists stock k then 
																		if v < (FindValueInt stock k ) 
																		then Dictionary.toList (Dictionary.remove OutputDict k)
																		else Dictionary.toList (Dictionary.set OutputDict k (n-v))
																		
																	else 
																		Dictionary.toList (Dictionary.set OutputDict k (~v))
									
													end 
											
								
    fun missingIngredients recipe stock =  let 
												fun aux ( lsRecipe , acc ) =
												case (lsRecipe ) of 
													[]=>  acc 
													|(k,v)::t=>aux ( t ,  auxFilterMissing ((k,v) , stock, acc)  ) 
													(*(auxFilterMissing ((k,v) , stock, acc))*)
											in 
											makeStock (	aux ( Dictionary.toList (auxSecond(recipe)),  Dictionary.toList (auxSecond(recipe)) ) )
											
											end  
	(* Returns a list of products from the cookbook that we can
       cook with a given stock - the recipes that have no missing
       ingredients. Each recipe in the cookbook should be examined
       independently, not sequentially. In other words, the recipes
       we examined before have no effect on the outcome for the
	   
	   if hasEnoughIngredients  stock ((hd cookbook):recipe) then makeCookbook ((hd cookbook):recipe)
										else makeCookbook _  
       current recipe. *)
    
	
    fun possibleRecipes cookbook stock = raise NotImplemented
										
 
	
	
	
    (* Returns a cookbook of all recipes we can cook given a set of
       possible substitutions. The substitutions are given in the form
       of equivalence classes:

       [
           [chicken, beef, veal, pork],
           [red pepper, green pepper],
           ...			
       ]

       If the recipe requires e.g. a chicken, we can use pork instead.
       If the recipe requires e.g. a green pepper, we can use
       a red pepper instead. There can be multiple substitutions
       (e.g. pepper and meat).

	   
       - The returned cookbook must also contain the recipe without
         any substitutions.
       - The recipe may contain ingredients that are not present in any
         given equivalence class.
       - You can assume that in an equivalence class each ingredient is
         specified only once.
       - You can assume that a recipe will contain at most one
         ingredient from each equivalence class. 
		 
		 *)
     (* Too long description !!! You need to optimize it :D *)
	fun generateVariants recipe substitutions = Dictionary.empty
    (* Returns the cheapest recipe in the cookbook, or NONE, if the
       cookbook is empty. *)

	fun cheapestRecipe cookbook pricelist = let 
                    fun auxMain(lsCookbook, acc) =
                         case lsCookbook of
                            [] => NONE
                            |h::[] => if (priceOfRecipe h pricelist) < (priceOfRecipe (valOf(acc)) pricelist)
                                    then SOME h
                                    else acc
                            |h::t => if (priceOfRecipe h pricelist) < (priceOfRecipe (valOf(acc)) pricelist)
                                    then auxMain(t, SOME h)
                                    else auxMain(t, acc)
  
                    in
                        auxMain(Dictionary.toList (cookbook) , SOME (hd(Dictionary.toList cookbook)) )
                    end
 end
	

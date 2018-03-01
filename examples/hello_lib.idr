module Hello_lib

export
hi : String
hi = "Hello world"

export
hello : String -> String
hello _ = hi

--l : Vect n a -> n
--l lst =
--  let aux : Vect n a -> Int -> n
--      aux Nil c = c
--      aux (x :: xs) c = aux xs $ c + 1
--  in aux lst 0


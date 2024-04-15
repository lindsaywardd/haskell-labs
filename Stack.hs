
-- stack and simple stack progrr
module Stack (push, pop, top, isEmpty, initStack, Stack(..)) where
-- add Stack ^ in hopes imports for viewTasks type dec
data Stack a = EmptyStack |
        NotEmpty a (Stack a)

--manual show def'n bc error - look over
instance Show a => Show (Stack a) where
    show EmptyStack = "EmptyStack"
    show (NotEmpty a b) = show a ++ show b


push x EmptyStack = NotEmpty x EmptyStack
push x y = NotEmpty x y

top (NotEmpty x y) = x

pop (NotEmpty x y) = y
pop e = e

isEmpty EmptyStack = True
isEmpty _ = False

initStack = EmptyStack




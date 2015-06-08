# Problems 2.10

function stars (n :: Int)
    for i in 1:n
        println (repeat ("*",i))
    end
end

# Problem 3.1

function print_array (xs :: Array)
    for i in xs
        println (i)
    end
end

# Problem 3.2

function revs (xs :: Array)
    if isempty (xs)
        return []
    else return vcat (revs (xs [2:end]), [(xs [1])])
    end
end




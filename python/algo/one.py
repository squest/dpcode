import time

def stars (n):
    m = ""
    for i in range (1,n):
        m = m ++ "*"
        println (m)
        


def timex (f,x):
    start_time = time.time()
    tmp = f(x)
    print("--- %s seconds ---" % (time.time() - start_time))
    return tmp
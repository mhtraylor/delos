; (op (expr) (expr))
((>= age 10) 
    ((|| (>= size 5000) (<= size 1000)) (= price 500))
    ((<= size 5000) (= price 500)))

product In [1131,12314,15151] 
    When age (2,10) And size (1,5000)

product In [4141,51515,151651,16161]
    When dwelling In [SFR,MOBILE,CONDO] And size (5000,1000)


rule product := 
    (((in [0,1,2]) 
        (dwelling (in [SFR,CONDO])) 
        (size (5000,1000)))
     ((in [3,4,5]) 
        (dwelling (in [MOBI,PLEX]))
        (size (1,5000))
        (age (2,10))))

    (((in (0 1 2)) (dwelling (in (SFR CONDO))) (size (5000 1000))) ((in (3 4 5)) (dwelling (in (MOBI PLEX))) (size (1 5000)) (age (2 10))))

rule product :=
    in [0,1,2]
        dwelling in [SFR,CONDO]
        size (5000,1000)
    in [3,4,5]
        company in [AHS]
        dwelling in [MOBI,PLEX]
        size (1,5000)
        age (2,10)
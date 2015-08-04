data Temp = Cold | Hot
data Season = Spring | Summer | Autumn | Winter
weather :: Season -> Temp
weather Summer = Hot
weather _ = Cold


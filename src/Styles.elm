module Styles exposing (filterDayBtn)


filterDayBtn : Bool -> String
filterDayBtn active =
    if active then
        "mr-1 text-white bg-orange-500 border border-orange-500 py-1 px-3 rounded"

    else
        "mr-1 text-orange-500 border border-orange-500 py-1 px-3 rounded"

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Glue.Types.AggFunction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.AggFunction
  ( AggFunction
      ( ..,
        AggFunction_Avg,
        AggFunction_Count,
        AggFunction_CountDistinct,
        AggFunction_First,
        AggFunction_Kurtosis,
        AggFunction_Last,
        AggFunction_Max,
        AggFunction_Min,
        AggFunction_Skewness,
        AggFunction_Stddev_pop,
        AggFunction_Stddev_samp,
        AggFunction_Sum,
        AggFunction_SumDistinct,
        AggFunction_Var_pop,
        AggFunction_Var_samp
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AggFunction = AggFunction'
  { fromAggFunction ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern AggFunction_Avg :: AggFunction
pattern AggFunction_Avg = AggFunction' "avg"

pattern AggFunction_Count :: AggFunction
pattern AggFunction_Count = AggFunction' "count"

pattern AggFunction_CountDistinct :: AggFunction
pattern AggFunction_CountDistinct = AggFunction' "countDistinct"

pattern AggFunction_First :: AggFunction
pattern AggFunction_First = AggFunction' "first"

pattern AggFunction_Kurtosis :: AggFunction
pattern AggFunction_Kurtosis = AggFunction' "kurtosis"

pattern AggFunction_Last :: AggFunction
pattern AggFunction_Last = AggFunction' "last"

pattern AggFunction_Max :: AggFunction
pattern AggFunction_Max = AggFunction' "max"

pattern AggFunction_Min :: AggFunction
pattern AggFunction_Min = AggFunction' "min"

pattern AggFunction_Skewness :: AggFunction
pattern AggFunction_Skewness = AggFunction' "skewness"

pattern AggFunction_Stddev_pop :: AggFunction
pattern AggFunction_Stddev_pop = AggFunction' "stddev_pop"

pattern AggFunction_Stddev_samp :: AggFunction
pattern AggFunction_Stddev_samp = AggFunction' "stddev_samp"

pattern AggFunction_Sum :: AggFunction
pattern AggFunction_Sum = AggFunction' "sum"

pattern AggFunction_SumDistinct :: AggFunction
pattern AggFunction_SumDistinct = AggFunction' "sumDistinct"

pattern AggFunction_Var_pop :: AggFunction
pattern AggFunction_Var_pop = AggFunction' "var_pop"

pattern AggFunction_Var_samp :: AggFunction
pattern AggFunction_Var_samp = AggFunction' "var_samp"

{-# COMPLETE
  AggFunction_Avg,
  AggFunction_Count,
  AggFunction_CountDistinct,
  AggFunction_First,
  AggFunction_Kurtosis,
  AggFunction_Last,
  AggFunction_Max,
  AggFunction_Min,
  AggFunction_Skewness,
  AggFunction_Stddev_pop,
  AggFunction_Stddev_samp,
  AggFunction_Sum,
  AggFunction_SumDistinct,
  AggFunction_Var_pop,
  AggFunction_Var_samp,
  AggFunction'
  #-}

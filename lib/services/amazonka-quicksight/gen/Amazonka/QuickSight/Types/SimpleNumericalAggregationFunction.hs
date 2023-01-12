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
-- Module      : Amazonka.QuickSight.Types.SimpleNumericalAggregationFunction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SimpleNumericalAggregationFunction
  ( SimpleNumericalAggregationFunction
      ( ..,
        SimpleNumericalAggregationFunction_AVERAGE,
        SimpleNumericalAggregationFunction_COUNT,
        SimpleNumericalAggregationFunction_DISTINCT_COUNT,
        SimpleNumericalAggregationFunction_MAX,
        SimpleNumericalAggregationFunction_MEDIAN,
        SimpleNumericalAggregationFunction_MIN,
        SimpleNumericalAggregationFunction_STDEV,
        SimpleNumericalAggregationFunction_STDEVP,
        SimpleNumericalAggregationFunction_SUM,
        SimpleNumericalAggregationFunction_VAR,
        SimpleNumericalAggregationFunction_VARP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SimpleNumericalAggregationFunction = SimpleNumericalAggregationFunction'
  { fromSimpleNumericalAggregationFunction ::
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

pattern SimpleNumericalAggregationFunction_AVERAGE :: SimpleNumericalAggregationFunction
pattern SimpleNumericalAggregationFunction_AVERAGE = SimpleNumericalAggregationFunction' "AVERAGE"

pattern SimpleNumericalAggregationFunction_COUNT :: SimpleNumericalAggregationFunction
pattern SimpleNumericalAggregationFunction_COUNT = SimpleNumericalAggregationFunction' "COUNT"

pattern SimpleNumericalAggregationFunction_DISTINCT_COUNT :: SimpleNumericalAggregationFunction
pattern SimpleNumericalAggregationFunction_DISTINCT_COUNT = SimpleNumericalAggregationFunction' "DISTINCT_COUNT"

pattern SimpleNumericalAggregationFunction_MAX :: SimpleNumericalAggregationFunction
pattern SimpleNumericalAggregationFunction_MAX = SimpleNumericalAggregationFunction' "MAX"

pattern SimpleNumericalAggregationFunction_MEDIAN :: SimpleNumericalAggregationFunction
pattern SimpleNumericalAggregationFunction_MEDIAN = SimpleNumericalAggregationFunction' "MEDIAN"

pattern SimpleNumericalAggregationFunction_MIN :: SimpleNumericalAggregationFunction
pattern SimpleNumericalAggregationFunction_MIN = SimpleNumericalAggregationFunction' "MIN"

pattern SimpleNumericalAggregationFunction_STDEV :: SimpleNumericalAggregationFunction
pattern SimpleNumericalAggregationFunction_STDEV = SimpleNumericalAggregationFunction' "STDEV"

pattern SimpleNumericalAggregationFunction_STDEVP :: SimpleNumericalAggregationFunction
pattern SimpleNumericalAggregationFunction_STDEVP = SimpleNumericalAggregationFunction' "STDEVP"

pattern SimpleNumericalAggregationFunction_SUM :: SimpleNumericalAggregationFunction
pattern SimpleNumericalAggregationFunction_SUM = SimpleNumericalAggregationFunction' "SUM"

pattern SimpleNumericalAggregationFunction_VAR :: SimpleNumericalAggregationFunction
pattern SimpleNumericalAggregationFunction_VAR = SimpleNumericalAggregationFunction' "VAR"

pattern SimpleNumericalAggregationFunction_VARP :: SimpleNumericalAggregationFunction
pattern SimpleNumericalAggregationFunction_VARP = SimpleNumericalAggregationFunction' "VARP"

{-# COMPLETE
  SimpleNumericalAggregationFunction_AVERAGE,
  SimpleNumericalAggregationFunction_COUNT,
  SimpleNumericalAggregationFunction_DISTINCT_COUNT,
  SimpleNumericalAggregationFunction_MAX,
  SimpleNumericalAggregationFunction_MEDIAN,
  SimpleNumericalAggregationFunction_MIN,
  SimpleNumericalAggregationFunction_STDEV,
  SimpleNumericalAggregationFunction_STDEVP,
  SimpleNumericalAggregationFunction_SUM,
  SimpleNumericalAggregationFunction_VAR,
  SimpleNumericalAggregationFunction_VARP,
  SimpleNumericalAggregationFunction'
  #-}

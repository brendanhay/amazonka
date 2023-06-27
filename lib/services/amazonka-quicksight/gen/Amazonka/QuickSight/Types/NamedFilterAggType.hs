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
-- Module      : Amazonka.QuickSight.Types.NamedFilterAggType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.NamedFilterAggType
  ( NamedFilterAggType
      ( ..,
        NamedFilterAggType_AVERAGE,
        NamedFilterAggType_COUNT,
        NamedFilterAggType_DISTINCT_COUNT,
        NamedFilterAggType_MAX,
        NamedFilterAggType_MEDIAN,
        NamedFilterAggType_MIN,
        NamedFilterAggType_NO_AGGREGATION,
        NamedFilterAggType_STDEV,
        NamedFilterAggType_STDEVP,
        NamedFilterAggType_SUM,
        NamedFilterAggType_VAR,
        NamedFilterAggType_VARP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype NamedFilterAggType = NamedFilterAggType'
  { fromNamedFilterAggType ::
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

pattern NamedFilterAggType_AVERAGE :: NamedFilterAggType
pattern NamedFilterAggType_AVERAGE = NamedFilterAggType' "AVERAGE"

pattern NamedFilterAggType_COUNT :: NamedFilterAggType
pattern NamedFilterAggType_COUNT = NamedFilterAggType' "COUNT"

pattern NamedFilterAggType_DISTINCT_COUNT :: NamedFilterAggType
pattern NamedFilterAggType_DISTINCT_COUNT = NamedFilterAggType' "DISTINCT_COUNT"

pattern NamedFilterAggType_MAX :: NamedFilterAggType
pattern NamedFilterAggType_MAX = NamedFilterAggType' "MAX"

pattern NamedFilterAggType_MEDIAN :: NamedFilterAggType
pattern NamedFilterAggType_MEDIAN = NamedFilterAggType' "MEDIAN"

pattern NamedFilterAggType_MIN :: NamedFilterAggType
pattern NamedFilterAggType_MIN = NamedFilterAggType' "MIN"

pattern NamedFilterAggType_NO_AGGREGATION :: NamedFilterAggType
pattern NamedFilterAggType_NO_AGGREGATION = NamedFilterAggType' "NO_AGGREGATION"

pattern NamedFilterAggType_STDEV :: NamedFilterAggType
pattern NamedFilterAggType_STDEV = NamedFilterAggType' "STDEV"

pattern NamedFilterAggType_STDEVP :: NamedFilterAggType
pattern NamedFilterAggType_STDEVP = NamedFilterAggType' "STDEVP"

pattern NamedFilterAggType_SUM :: NamedFilterAggType
pattern NamedFilterAggType_SUM = NamedFilterAggType' "SUM"

pattern NamedFilterAggType_VAR :: NamedFilterAggType
pattern NamedFilterAggType_VAR = NamedFilterAggType' "VAR"

pattern NamedFilterAggType_VARP :: NamedFilterAggType
pattern NamedFilterAggType_VARP = NamedFilterAggType' "VARP"

{-# COMPLETE
  NamedFilterAggType_AVERAGE,
  NamedFilterAggType_COUNT,
  NamedFilterAggType_DISTINCT_COUNT,
  NamedFilterAggType_MAX,
  NamedFilterAggType_MEDIAN,
  NamedFilterAggType_MIN,
  NamedFilterAggType_NO_AGGREGATION,
  NamedFilterAggType_STDEV,
  NamedFilterAggType_STDEVP,
  NamedFilterAggType_SUM,
  NamedFilterAggType_VAR,
  NamedFilterAggType_VARP,
  NamedFilterAggType'
  #-}

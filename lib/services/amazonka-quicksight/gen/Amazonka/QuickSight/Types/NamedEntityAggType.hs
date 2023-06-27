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
-- Module      : Amazonka.QuickSight.Types.NamedEntityAggType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.NamedEntityAggType
  ( NamedEntityAggType
      ( ..,
        NamedEntityAggType_AVERAGE,
        NamedEntityAggType_COUNT,
        NamedEntityAggType_CUSTOM,
        NamedEntityAggType_DISTINCT_COUNT,
        NamedEntityAggType_MAX,
        NamedEntityAggType_MEDIAN,
        NamedEntityAggType_MIN,
        NamedEntityAggType_PERCENTILE,
        NamedEntityAggType_STDEV,
        NamedEntityAggType_STDEVP,
        NamedEntityAggType_SUM,
        NamedEntityAggType_VAR,
        NamedEntityAggType_VARP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype NamedEntityAggType = NamedEntityAggType'
  { fromNamedEntityAggType ::
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

pattern NamedEntityAggType_AVERAGE :: NamedEntityAggType
pattern NamedEntityAggType_AVERAGE = NamedEntityAggType' "AVERAGE"

pattern NamedEntityAggType_COUNT :: NamedEntityAggType
pattern NamedEntityAggType_COUNT = NamedEntityAggType' "COUNT"

pattern NamedEntityAggType_CUSTOM :: NamedEntityAggType
pattern NamedEntityAggType_CUSTOM = NamedEntityAggType' "CUSTOM"

pattern NamedEntityAggType_DISTINCT_COUNT :: NamedEntityAggType
pattern NamedEntityAggType_DISTINCT_COUNT = NamedEntityAggType' "DISTINCT_COUNT"

pattern NamedEntityAggType_MAX :: NamedEntityAggType
pattern NamedEntityAggType_MAX = NamedEntityAggType' "MAX"

pattern NamedEntityAggType_MEDIAN :: NamedEntityAggType
pattern NamedEntityAggType_MEDIAN = NamedEntityAggType' "MEDIAN"

pattern NamedEntityAggType_MIN :: NamedEntityAggType
pattern NamedEntityAggType_MIN = NamedEntityAggType' "MIN"

pattern NamedEntityAggType_PERCENTILE :: NamedEntityAggType
pattern NamedEntityAggType_PERCENTILE = NamedEntityAggType' "PERCENTILE"

pattern NamedEntityAggType_STDEV :: NamedEntityAggType
pattern NamedEntityAggType_STDEV = NamedEntityAggType' "STDEV"

pattern NamedEntityAggType_STDEVP :: NamedEntityAggType
pattern NamedEntityAggType_STDEVP = NamedEntityAggType' "STDEVP"

pattern NamedEntityAggType_SUM :: NamedEntityAggType
pattern NamedEntityAggType_SUM = NamedEntityAggType' "SUM"

pattern NamedEntityAggType_VAR :: NamedEntityAggType
pattern NamedEntityAggType_VAR = NamedEntityAggType' "VAR"

pattern NamedEntityAggType_VARP :: NamedEntityAggType
pattern NamedEntityAggType_VARP = NamedEntityAggType' "VARP"

{-# COMPLETE
  NamedEntityAggType_AVERAGE,
  NamedEntityAggType_COUNT,
  NamedEntityAggType_CUSTOM,
  NamedEntityAggType_DISTINCT_COUNT,
  NamedEntityAggType_MAX,
  NamedEntityAggType_MEDIAN,
  NamedEntityAggType_MIN,
  NamedEntityAggType_PERCENTILE,
  NamedEntityAggType_STDEV,
  NamedEntityAggType_STDEVP,
  NamedEntityAggType_SUM,
  NamedEntityAggType_VAR,
  NamedEntityAggType_VARP,
  NamedEntityAggType'
  #-}

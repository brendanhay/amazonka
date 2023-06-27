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
-- Module      : Amazonka.QuickSight.Types.DefaultAggregation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DefaultAggregation
  ( DefaultAggregation
      ( ..,
        DefaultAggregation_AVERAGE,
        DefaultAggregation_COUNT,
        DefaultAggregation_DISTINCT_COUNT,
        DefaultAggregation_MAX,
        DefaultAggregation_MIN,
        DefaultAggregation_SUM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DefaultAggregation = DefaultAggregation'
  { fromDefaultAggregation ::
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

pattern DefaultAggregation_AVERAGE :: DefaultAggregation
pattern DefaultAggregation_AVERAGE = DefaultAggregation' "AVERAGE"

pattern DefaultAggregation_COUNT :: DefaultAggregation
pattern DefaultAggregation_COUNT = DefaultAggregation' "COUNT"

pattern DefaultAggregation_DISTINCT_COUNT :: DefaultAggregation
pattern DefaultAggregation_DISTINCT_COUNT = DefaultAggregation' "DISTINCT_COUNT"

pattern DefaultAggregation_MAX :: DefaultAggregation
pattern DefaultAggregation_MAX = DefaultAggregation' "MAX"

pattern DefaultAggregation_MIN :: DefaultAggregation
pattern DefaultAggregation_MIN = DefaultAggregation' "MIN"

pattern DefaultAggregation_SUM :: DefaultAggregation
pattern DefaultAggregation_SUM = DefaultAggregation' "SUM"

{-# COMPLETE
  DefaultAggregation_AVERAGE,
  DefaultAggregation_COUNT,
  DefaultAggregation_DISTINCT_COUNT,
  DefaultAggregation_MAX,
  DefaultAggregation_MIN,
  DefaultAggregation_SUM,
  DefaultAggregation'
  #-}

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
-- Module      : Amazonka.QuickSight.Types.AuthorSpecifiedAggregation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AuthorSpecifiedAggregation
  ( AuthorSpecifiedAggregation
      ( ..,
        AuthorSpecifiedAggregation_AVERAGE,
        AuthorSpecifiedAggregation_COUNT,
        AuthorSpecifiedAggregation_DISTINCT_COUNT,
        AuthorSpecifiedAggregation_MAX,
        AuthorSpecifiedAggregation_MEDIAN,
        AuthorSpecifiedAggregation_MIN,
        AuthorSpecifiedAggregation_PERCENTILE,
        AuthorSpecifiedAggregation_STDEV,
        AuthorSpecifiedAggregation_STDEVP,
        AuthorSpecifiedAggregation_SUM,
        AuthorSpecifiedAggregation_VAR,
        AuthorSpecifiedAggregation_VARP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AuthorSpecifiedAggregation = AuthorSpecifiedAggregation'
  { fromAuthorSpecifiedAggregation ::
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

pattern AuthorSpecifiedAggregation_AVERAGE :: AuthorSpecifiedAggregation
pattern AuthorSpecifiedAggregation_AVERAGE = AuthorSpecifiedAggregation' "AVERAGE"

pattern AuthorSpecifiedAggregation_COUNT :: AuthorSpecifiedAggregation
pattern AuthorSpecifiedAggregation_COUNT = AuthorSpecifiedAggregation' "COUNT"

pattern AuthorSpecifiedAggregation_DISTINCT_COUNT :: AuthorSpecifiedAggregation
pattern AuthorSpecifiedAggregation_DISTINCT_COUNT = AuthorSpecifiedAggregation' "DISTINCT_COUNT"

pattern AuthorSpecifiedAggregation_MAX :: AuthorSpecifiedAggregation
pattern AuthorSpecifiedAggregation_MAX = AuthorSpecifiedAggregation' "MAX"

pattern AuthorSpecifiedAggregation_MEDIAN :: AuthorSpecifiedAggregation
pattern AuthorSpecifiedAggregation_MEDIAN = AuthorSpecifiedAggregation' "MEDIAN"

pattern AuthorSpecifiedAggregation_MIN :: AuthorSpecifiedAggregation
pattern AuthorSpecifiedAggregation_MIN = AuthorSpecifiedAggregation' "MIN"

pattern AuthorSpecifiedAggregation_PERCENTILE :: AuthorSpecifiedAggregation
pattern AuthorSpecifiedAggregation_PERCENTILE = AuthorSpecifiedAggregation' "PERCENTILE"

pattern AuthorSpecifiedAggregation_STDEV :: AuthorSpecifiedAggregation
pattern AuthorSpecifiedAggregation_STDEV = AuthorSpecifiedAggregation' "STDEV"

pattern AuthorSpecifiedAggregation_STDEVP :: AuthorSpecifiedAggregation
pattern AuthorSpecifiedAggregation_STDEVP = AuthorSpecifiedAggregation' "STDEVP"

pattern AuthorSpecifiedAggregation_SUM :: AuthorSpecifiedAggregation
pattern AuthorSpecifiedAggregation_SUM = AuthorSpecifiedAggregation' "SUM"

pattern AuthorSpecifiedAggregation_VAR :: AuthorSpecifiedAggregation
pattern AuthorSpecifiedAggregation_VAR = AuthorSpecifiedAggregation' "VAR"

pattern AuthorSpecifiedAggregation_VARP :: AuthorSpecifiedAggregation
pattern AuthorSpecifiedAggregation_VARP = AuthorSpecifiedAggregation' "VARP"

{-# COMPLETE
  AuthorSpecifiedAggregation_AVERAGE,
  AuthorSpecifiedAggregation_COUNT,
  AuthorSpecifiedAggregation_DISTINCT_COUNT,
  AuthorSpecifiedAggregation_MAX,
  AuthorSpecifiedAggregation_MEDIAN,
  AuthorSpecifiedAggregation_MIN,
  AuthorSpecifiedAggregation_PERCENTILE,
  AuthorSpecifiedAggregation_STDEV,
  AuthorSpecifiedAggregation_STDEVP,
  AuthorSpecifiedAggregation_SUM,
  AuthorSpecifiedAggregation_VAR,
  AuthorSpecifiedAggregation_VARP,
  AuthorSpecifiedAggregation'
  #-}

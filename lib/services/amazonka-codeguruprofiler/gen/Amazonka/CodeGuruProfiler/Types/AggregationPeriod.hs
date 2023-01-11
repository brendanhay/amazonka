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
-- Module      : Amazonka.CodeGuruProfiler.Types.AggregationPeriod
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruProfiler.Types.AggregationPeriod
  ( AggregationPeriod
      ( ..,
        AggregationPeriod_P1D,
        AggregationPeriod_PT1H,
        AggregationPeriod_PT5M
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AggregationPeriod = AggregationPeriod'
  { fromAggregationPeriod ::
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

pattern AggregationPeriod_P1D :: AggregationPeriod
pattern AggregationPeriod_P1D = AggregationPeriod' "P1D"

pattern AggregationPeriod_PT1H :: AggregationPeriod
pattern AggregationPeriod_PT1H = AggregationPeriod' "PT1H"

pattern AggregationPeriod_PT5M :: AggregationPeriod
pattern AggregationPeriod_PT5M = AggregationPeriod' "PT5M"

{-# COMPLETE
  AggregationPeriod_P1D,
  AggregationPeriod_PT1H,
  AggregationPeriod_PT5M,
  AggregationPeriod'
  #-}

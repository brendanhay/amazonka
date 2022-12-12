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
-- Module      : Amazonka.QuickSight.Types.DateAggregationFunction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DateAggregationFunction
  ( DateAggregationFunction
      ( ..,
        DateAggregationFunction_COUNT,
        DateAggregationFunction_DISTINCT_COUNT,
        DateAggregationFunction_MAX,
        DateAggregationFunction_MIN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DateAggregationFunction = DateAggregationFunction'
  { fromDateAggregationFunction ::
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

pattern DateAggregationFunction_COUNT :: DateAggregationFunction
pattern DateAggregationFunction_COUNT = DateAggregationFunction' "COUNT"

pattern DateAggregationFunction_DISTINCT_COUNT :: DateAggregationFunction
pattern DateAggregationFunction_DISTINCT_COUNT = DateAggregationFunction' "DISTINCT_COUNT"

pattern DateAggregationFunction_MAX :: DateAggregationFunction
pattern DateAggregationFunction_MAX = DateAggregationFunction' "MAX"

pattern DateAggregationFunction_MIN :: DateAggregationFunction
pattern DateAggregationFunction_MIN = DateAggregationFunction' "MIN"

{-# COMPLETE
  DateAggregationFunction_COUNT,
  DateAggregationFunction_DISTINCT_COUNT,
  DateAggregationFunction_MAX,
  DateAggregationFunction_MIN,
  DateAggregationFunction'
  #-}

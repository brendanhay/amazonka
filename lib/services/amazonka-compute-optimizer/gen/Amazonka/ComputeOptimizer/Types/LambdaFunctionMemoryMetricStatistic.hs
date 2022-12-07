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
-- Module      : Amazonka.ComputeOptimizer.Types.LambdaFunctionMemoryMetricStatistic
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.LambdaFunctionMemoryMetricStatistic
  ( LambdaFunctionMemoryMetricStatistic
      ( ..,
        LambdaFunctionMemoryMetricStatistic_Expected,
        LambdaFunctionMemoryMetricStatistic_LowerBound,
        LambdaFunctionMemoryMetricStatistic_UpperBound
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LambdaFunctionMemoryMetricStatistic = LambdaFunctionMemoryMetricStatistic'
  { fromLambdaFunctionMemoryMetricStatistic ::
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

pattern LambdaFunctionMemoryMetricStatistic_Expected :: LambdaFunctionMemoryMetricStatistic
pattern LambdaFunctionMemoryMetricStatistic_Expected = LambdaFunctionMemoryMetricStatistic' "Expected"

pattern LambdaFunctionMemoryMetricStatistic_LowerBound :: LambdaFunctionMemoryMetricStatistic
pattern LambdaFunctionMemoryMetricStatistic_LowerBound = LambdaFunctionMemoryMetricStatistic' "LowerBound"

pattern LambdaFunctionMemoryMetricStatistic_UpperBound :: LambdaFunctionMemoryMetricStatistic
pattern LambdaFunctionMemoryMetricStatistic_UpperBound = LambdaFunctionMemoryMetricStatistic' "UpperBound"

{-# COMPLETE
  LambdaFunctionMemoryMetricStatistic_Expected,
  LambdaFunctionMemoryMetricStatistic_LowerBound,
  LambdaFunctionMemoryMetricStatistic_UpperBound,
  LambdaFunctionMemoryMetricStatistic'
  #-}

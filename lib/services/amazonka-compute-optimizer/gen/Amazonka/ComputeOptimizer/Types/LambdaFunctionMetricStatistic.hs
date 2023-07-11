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
-- Module      : Amazonka.ComputeOptimizer.Types.LambdaFunctionMetricStatistic
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.LambdaFunctionMetricStatistic
  ( LambdaFunctionMetricStatistic
      ( ..,
        LambdaFunctionMetricStatistic_Average,
        LambdaFunctionMetricStatistic_Maximum
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LambdaFunctionMetricStatistic = LambdaFunctionMetricStatistic'
  { fromLambdaFunctionMetricStatistic ::
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

pattern LambdaFunctionMetricStatistic_Average :: LambdaFunctionMetricStatistic
pattern LambdaFunctionMetricStatistic_Average = LambdaFunctionMetricStatistic' "Average"

pattern LambdaFunctionMetricStatistic_Maximum :: LambdaFunctionMetricStatistic
pattern LambdaFunctionMetricStatistic_Maximum = LambdaFunctionMetricStatistic' "Maximum"

{-# COMPLETE
  LambdaFunctionMetricStatistic_Average,
  LambdaFunctionMetricStatistic_Maximum,
  LambdaFunctionMetricStatistic'
  #-}

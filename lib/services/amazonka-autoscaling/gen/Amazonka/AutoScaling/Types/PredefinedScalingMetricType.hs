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
-- Module      : Amazonka.AutoScaling.Types.PredefinedScalingMetricType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.PredefinedScalingMetricType
  ( PredefinedScalingMetricType
      ( ..,
        PredefinedScalingMetricType_ALBRequestCountPerTarget,
        PredefinedScalingMetricType_ASGAverageCPUUtilization,
        PredefinedScalingMetricType_ASGAverageNetworkIn,
        PredefinedScalingMetricType_ASGAverageNetworkOut
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PredefinedScalingMetricType = PredefinedScalingMetricType'
  { fromPredefinedScalingMetricType ::
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

pattern PredefinedScalingMetricType_ALBRequestCountPerTarget :: PredefinedScalingMetricType
pattern PredefinedScalingMetricType_ALBRequestCountPerTarget = PredefinedScalingMetricType' "ALBRequestCountPerTarget"

pattern PredefinedScalingMetricType_ASGAverageCPUUtilization :: PredefinedScalingMetricType
pattern PredefinedScalingMetricType_ASGAverageCPUUtilization = PredefinedScalingMetricType' "ASGAverageCPUUtilization"

pattern PredefinedScalingMetricType_ASGAverageNetworkIn :: PredefinedScalingMetricType
pattern PredefinedScalingMetricType_ASGAverageNetworkIn = PredefinedScalingMetricType' "ASGAverageNetworkIn"

pattern PredefinedScalingMetricType_ASGAverageNetworkOut :: PredefinedScalingMetricType
pattern PredefinedScalingMetricType_ASGAverageNetworkOut = PredefinedScalingMetricType' "ASGAverageNetworkOut"

{-# COMPLETE
  PredefinedScalingMetricType_ALBRequestCountPerTarget,
  PredefinedScalingMetricType_ASGAverageCPUUtilization,
  PredefinedScalingMetricType_ASGAverageNetworkIn,
  PredefinedScalingMetricType_ASGAverageNetworkOut,
  PredefinedScalingMetricType'
  #-}

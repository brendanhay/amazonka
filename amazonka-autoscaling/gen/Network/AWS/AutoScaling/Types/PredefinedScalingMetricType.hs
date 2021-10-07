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
-- Module      : Network.AWS.AutoScaling.Types.PredefinedScalingMetricType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.PredefinedScalingMetricType
  ( PredefinedScalingMetricType
      ( ..,
        PredefinedScalingMetricType_ALBRequestCountPerTarget,
        PredefinedScalingMetricType_ASGAverageCPUUtilization,
        PredefinedScalingMetricType_ASGAverageNetworkIn,
        PredefinedScalingMetricType_ASGAverageNetworkOut
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype PredefinedScalingMetricType = PredefinedScalingMetricType'
  { fromPredefinedScalingMetricType ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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

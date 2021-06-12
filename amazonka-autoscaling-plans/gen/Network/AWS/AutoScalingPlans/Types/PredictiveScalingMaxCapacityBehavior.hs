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
-- Module      : Network.AWS.AutoScalingPlans.Types.PredictiveScalingMaxCapacityBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.PredictiveScalingMaxCapacityBehavior
  ( PredictiveScalingMaxCapacityBehavior
      ( ..,
        PredictiveScalingMaxCapacityBehavior_SetForecastCapacityToMaxCapacity,
        PredictiveScalingMaxCapacityBehavior_SetMaxCapacityAboveForecastCapacity,
        PredictiveScalingMaxCapacityBehavior_SetMaxCapacityToForecastCapacity
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype PredictiveScalingMaxCapacityBehavior = PredictiveScalingMaxCapacityBehavior'
  { fromPredictiveScalingMaxCapacityBehavior ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern PredictiveScalingMaxCapacityBehavior_SetForecastCapacityToMaxCapacity :: PredictiveScalingMaxCapacityBehavior
pattern PredictiveScalingMaxCapacityBehavior_SetForecastCapacityToMaxCapacity = PredictiveScalingMaxCapacityBehavior' "SetForecastCapacityToMaxCapacity"

pattern PredictiveScalingMaxCapacityBehavior_SetMaxCapacityAboveForecastCapacity :: PredictiveScalingMaxCapacityBehavior
pattern PredictiveScalingMaxCapacityBehavior_SetMaxCapacityAboveForecastCapacity = PredictiveScalingMaxCapacityBehavior' "SetMaxCapacityAboveForecastCapacity"

pattern PredictiveScalingMaxCapacityBehavior_SetMaxCapacityToForecastCapacity :: PredictiveScalingMaxCapacityBehavior
pattern PredictiveScalingMaxCapacityBehavior_SetMaxCapacityToForecastCapacity = PredictiveScalingMaxCapacityBehavior' "SetMaxCapacityToForecastCapacity"

{-# COMPLETE
  PredictiveScalingMaxCapacityBehavior_SetForecastCapacityToMaxCapacity,
  PredictiveScalingMaxCapacityBehavior_SetMaxCapacityAboveForecastCapacity,
  PredictiveScalingMaxCapacityBehavior_SetMaxCapacityToForecastCapacity,
  PredictiveScalingMaxCapacityBehavior'
  #-}

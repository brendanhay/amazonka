{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype PredictiveScalingMaxCapacityBehavior = PredictiveScalingMaxCapacityBehavior'
  { fromPredictiveScalingMaxCapacityBehavior ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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

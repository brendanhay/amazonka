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
-- Module      : Amazonka.AutoScalingPlans.Types.PredictiveScalingMaxCapacityBehavior
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScalingPlans.Types.PredictiveScalingMaxCapacityBehavior
  ( PredictiveScalingMaxCapacityBehavior
      ( ..,
        PredictiveScalingMaxCapacityBehavior_SetForecastCapacityToMaxCapacity,
        PredictiveScalingMaxCapacityBehavior_SetMaxCapacityAboveForecastCapacity,
        PredictiveScalingMaxCapacityBehavior_SetMaxCapacityToForecastCapacity
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PredictiveScalingMaxCapacityBehavior = PredictiveScalingMaxCapacityBehavior'
  { fromPredictiveScalingMaxCapacityBehavior ::
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

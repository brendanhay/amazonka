{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ScalingBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ScalingBehavior
  ( ScalingBehavior
      ( ScalingBehavior',
        ScalingBehaviorDefault,
        ScalingBehaviorStretchToOutput,
        fromScalingBehavior
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Specify how the service handles outputs that have a different aspect ratio from the input aspect ratio. Choose Stretch to output (STRETCH_TO_OUTPUT) to have the service stretch your video image to fit. Keep the setting Default (DEFAULT) to have the service letterbox your video instead. This setting overrides any value that you specify for the setting Selection placement (position) in this output.
newtype ScalingBehavior = ScalingBehavior'
  { fromScalingBehavior ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ScalingBehaviorDefault :: ScalingBehavior
pattern ScalingBehaviorDefault = ScalingBehavior' "DEFAULT"

pattern ScalingBehaviorStretchToOutput :: ScalingBehavior
pattern ScalingBehaviorStretchToOutput = ScalingBehavior' "STRETCH_TO_OUTPUT"

{-# COMPLETE
  ScalingBehaviorDefault,
  ScalingBehaviorStretchToOutput,
  ScalingBehavior'
  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Reason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Reason
  ( Reason
      ( Reason',
        ReasonExceedsMaxFaces,
        ReasonExtremePose,
        ReasonLowBrightness,
        ReasonLowSharpness,
        ReasonLowConfidence,
        ReasonSmallBoundingBox,
        ReasonLowFaceQuality,
        fromReason
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype Reason = Reason' {fromReason :: Core.Text}
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

pattern ReasonExceedsMaxFaces :: Reason
pattern ReasonExceedsMaxFaces = Reason' "EXCEEDS_MAX_FACES"

pattern ReasonExtremePose :: Reason
pattern ReasonExtremePose = Reason' "EXTREME_POSE"

pattern ReasonLowBrightness :: Reason
pattern ReasonLowBrightness = Reason' "LOW_BRIGHTNESS"

pattern ReasonLowSharpness :: Reason
pattern ReasonLowSharpness = Reason' "LOW_SHARPNESS"

pattern ReasonLowConfidence :: Reason
pattern ReasonLowConfidence = Reason' "LOW_CONFIDENCE"

pattern ReasonSmallBoundingBox :: Reason
pattern ReasonSmallBoundingBox = Reason' "SMALL_BOUNDING_BOX"

pattern ReasonLowFaceQuality :: Reason
pattern ReasonLowFaceQuality = Reason' "LOW_FACE_QUALITY"

{-# COMPLETE
  ReasonExceedsMaxFaces,
  ReasonExtremePose,
  ReasonLowBrightness,
  ReasonLowSharpness,
  ReasonLowConfidence,
  ReasonSmallBoundingBox,
  ReasonLowFaceQuality,
  Reason'
  #-}

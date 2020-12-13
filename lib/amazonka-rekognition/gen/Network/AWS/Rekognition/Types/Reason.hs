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
        ExceedsMaxFaces,
        ExtremePose,
        LowBrightness,
        LowSharpness,
        LowConfidence,
        SmallBoundingBox,
        LowFaceQuality
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype Reason = Reason' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern ExceedsMaxFaces :: Reason
pattern ExceedsMaxFaces = Reason' "EXCEEDS_MAX_FACES"

pattern ExtremePose :: Reason
pattern ExtremePose = Reason' "EXTREME_POSE"

pattern LowBrightness :: Reason
pattern LowBrightness = Reason' "LOW_BRIGHTNESS"

pattern LowSharpness :: Reason
pattern LowSharpness = Reason' "LOW_SHARPNESS"

pattern LowConfidence :: Reason
pattern LowConfidence = Reason' "LOW_CONFIDENCE"

pattern SmallBoundingBox :: Reason
pattern SmallBoundingBox = Reason' "SMALL_BOUNDING_BOX"

pattern LowFaceQuality :: Reason
pattern LowFaceQuality = Reason' "LOW_FACE_QUALITY"

{-# COMPLETE
  ExceedsMaxFaces,
  ExtremePose,
  LowBrightness,
  LowSharpness,
  LowConfidence,
  SmallBoundingBox,
  LowFaceQuality,
  Reason'
  #-}

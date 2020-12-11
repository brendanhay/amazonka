-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264DynamicSubGop
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264DynamicSubGop
  ( H264DynamicSubGop
      ( H264DynamicSubGop',
        HDSGAdaptive,
        HDSGStatic
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Choose Adaptive to improve subjective video quality for high-motion content. This will cause the service to use fewer B-frames (which infer information based on other frames) for high-motion portions of the video and more B-frames for low-motion portions. The maximum number of B-frames is limited by the value you provide for the setting B frames between reference frames (numberBFramesBetweenReferenceFrames).
newtype H264DynamicSubGop = H264DynamicSubGop' Lude.Text
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

pattern HDSGAdaptive :: H264DynamicSubGop
pattern HDSGAdaptive = H264DynamicSubGop' "ADAPTIVE"

pattern HDSGStatic :: H264DynamicSubGop
pattern HDSGStatic = H264DynamicSubGop' "STATIC"

{-# COMPLETE
  HDSGAdaptive,
  HDSGStatic,
  H264DynamicSubGop'
  #-}

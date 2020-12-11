-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2DynamicSubGop
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mpeg2DynamicSubGop
  ( Mpeg2DynamicSubGop
      ( Mpeg2DynamicSubGop',
        MDSGAdaptive,
        MDSGStatic
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Choose Adaptive to improve subjective video quality for high-motion content. This will cause the service to use fewer B-frames (which infer information based on other frames) for high-motion portions of the video and more B-frames for low-motion portions. The maximum number of B-frames is limited by the value you provide for the setting B frames between reference frames (numberBFramesBetweenReferenceFrames).
newtype Mpeg2DynamicSubGop = Mpeg2DynamicSubGop' Lude.Text
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

pattern MDSGAdaptive :: Mpeg2DynamicSubGop
pattern MDSGAdaptive = Mpeg2DynamicSubGop' "ADAPTIVE"

pattern MDSGStatic :: Mpeg2DynamicSubGop
pattern MDSGStatic = Mpeg2DynamicSubGop' "STATIC"

{-# COMPLETE
  MDSGAdaptive,
  MDSGStatic,
  Mpeg2DynamicSubGop'
  #-}

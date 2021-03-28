{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2DynamicSubGop
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Mpeg2DynamicSubGop
  ( Mpeg2DynamicSubGop
    ( Mpeg2DynamicSubGop'
    , Mpeg2DynamicSubGopAdaptive
    , Mpeg2DynamicSubGopStatic
    , fromMpeg2DynamicSubGop
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Choose Adaptive to improve subjective video quality for high-motion content. This will cause the service to use fewer B-frames (which infer information based on other frames) for high-motion portions of the video and more B-frames for low-motion portions. The maximum number of B-frames is limited by the value you provide for the setting B frames between reference frames (numberBFramesBetweenReferenceFrames).
newtype Mpeg2DynamicSubGop = Mpeg2DynamicSubGop'{fromMpeg2DynamicSubGop
                                                 :: Core.Text}
                               deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                               Core.Generic)
                               deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                 Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                 Core.FromJSON, Core.ToXML, Core.FromXML,
                                                 Core.ToText, Core.FromText, Core.ToByteString,
                                                 Core.ToQuery, Core.ToHeader)

pattern Mpeg2DynamicSubGopAdaptive :: Mpeg2DynamicSubGop
pattern Mpeg2DynamicSubGopAdaptive = Mpeg2DynamicSubGop' "ADAPTIVE"

pattern Mpeg2DynamicSubGopStatic :: Mpeg2DynamicSubGop
pattern Mpeg2DynamicSubGopStatic = Mpeg2DynamicSubGop' "STATIC"

{-# COMPLETE 
  Mpeg2DynamicSubGopAdaptive,

  Mpeg2DynamicSubGopStatic,
  Mpeg2DynamicSubGop'
  #-}

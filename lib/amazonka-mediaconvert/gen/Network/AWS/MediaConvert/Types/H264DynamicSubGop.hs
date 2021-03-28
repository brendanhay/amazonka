{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264DynamicSubGop
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H264DynamicSubGop
  ( H264DynamicSubGop
    ( H264DynamicSubGop'
    , H264DynamicSubGopAdaptive
    , H264DynamicSubGopStatic
    , fromH264DynamicSubGop
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Choose Adaptive to improve subjective video quality for high-motion content. This will cause the service to use fewer B-frames (which infer information based on other frames) for high-motion portions of the video and more B-frames for low-motion portions. The maximum number of B-frames is limited by the value you provide for the setting B frames between reference frames (numberBFramesBetweenReferenceFrames).
newtype H264DynamicSubGop = H264DynamicSubGop'{fromH264DynamicSubGop
                                               :: Core.Text}
                              deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                              Core.Generic)
                              deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                Core.FromJSON, Core.ToXML, Core.FromXML,
                                                Core.ToText, Core.FromText, Core.ToByteString,
                                                Core.ToQuery, Core.ToHeader)

pattern H264DynamicSubGopAdaptive :: H264DynamicSubGop
pattern H264DynamicSubGopAdaptive = H264DynamicSubGop' "ADAPTIVE"

pattern H264DynamicSubGopStatic :: H264DynamicSubGop
pattern H264DynamicSubGopStatic = H264DynamicSubGop' "STATIC"

{-# COMPLETE 
  H264DynamicSubGopAdaptive,

  H264DynamicSubGopStatic,
  H264DynamicSubGop'
  #-}

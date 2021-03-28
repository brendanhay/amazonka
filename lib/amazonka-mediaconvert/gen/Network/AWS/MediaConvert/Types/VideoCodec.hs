{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.VideoCodec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.VideoCodec
  ( VideoCodec
    ( VideoCodec'
    , VideoCodecAV1
    , VideoCodecAvcIntra
    , VideoCodecFrameCapture
    , VideoCodecH264
    , VideoCodecH265
    , VideoCodecMPEG2
    , VideoCodecProres
    , VideoCodecVC3
    , VideoCodecVP8
    , VideoCodecVP9
    , fromVideoCodec
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Type of video codec
newtype VideoCodec = VideoCodec'{fromVideoCodec :: Core.Text}
                       deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                       Core.Generic)
                       deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                         Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                         Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                         Core.FromText, Core.ToByteString, Core.ToQuery,
                                         Core.ToHeader)

pattern VideoCodecAV1 :: VideoCodec
pattern VideoCodecAV1 = VideoCodec' "AV1"

pattern VideoCodecAvcIntra :: VideoCodec
pattern VideoCodecAvcIntra = VideoCodec' "AVC_INTRA"

pattern VideoCodecFrameCapture :: VideoCodec
pattern VideoCodecFrameCapture = VideoCodec' "FRAME_CAPTURE"

pattern VideoCodecH264 :: VideoCodec
pattern VideoCodecH264 = VideoCodec' "H_264"

pattern VideoCodecH265 :: VideoCodec
pattern VideoCodecH265 = VideoCodec' "H_265"

pattern VideoCodecMPEG2 :: VideoCodec
pattern VideoCodecMPEG2 = VideoCodec' "MPEG2"

pattern VideoCodecProres :: VideoCodec
pattern VideoCodecProres = VideoCodec' "PRORES"

pattern VideoCodecVC3 :: VideoCodec
pattern VideoCodecVC3 = VideoCodec' "VC3"

pattern VideoCodecVP8 :: VideoCodec
pattern VideoCodecVP8 = VideoCodec' "VP8"

pattern VideoCodecVP9 :: VideoCodec
pattern VideoCodecVP9 = VideoCodec' "VP9"

{-# COMPLETE 
  VideoCodecAV1,

  VideoCodecAvcIntra,

  VideoCodecFrameCapture,

  VideoCodecH264,

  VideoCodecH265,

  VideoCodecMPEG2,

  VideoCodecProres,

  VideoCodecVC3,

  VideoCodecVP8,

  VideoCodecVP9,
  VideoCodec'
  #-}

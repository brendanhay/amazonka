{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ColorSpace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.ColorSpace
  ( ColorSpace
    ( ColorSpace'
    , ColorSpaceFollow
    , ColorSpaceRec601
    , ColorSpaceRec709
    , ColorSpaceHDR10
    , ColorSpaceHlg2020
    , fromColorSpace
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | If your input video has accurate color space metadata, or if you don't know about color space, leave this set to the default value Follow (FOLLOW). The service will automatically detect your input color space. If your input video has metadata indicating the wrong color space, specify the accurate color space here. If your input video is HDR 10 and the SMPTE ST 2086 Mastering Display Color Volume static metadata isn't present in your video stream, or if that metadata is present but not accurate, choose Force HDR 10 (FORCE_HDR10) here and specify correct values in the input HDR 10 metadata (Hdr10Metadata) settings. For more information about MediaConvert HDR jobs, see https://docs.aws.amazon.com/console/mediaconvert/hdr.
newtype ColorSpace = ColorSpace'{fromColorSpace :: Core.Text}
                       deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                       Core.Generic)
                       deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                         Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                         Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                         Core.FromText, Core.ToByteString, Core.ToQuery,
                                         Core.ToHeader)

pattern ColorSpaceFollow :: ColorSpace
pattern ColorSpaceFollow = ColorSpace' "FOLLOW"

pattern ColorSpaceRec601 :: ColorSpace
pattern ColorSpaceRec601 = ColorSpace' "REC_601"

pattern ColorSpaceRec709 :: ColorSpace
pattern ColorSpaceRec709 = ColorSpace' "REC_709"

pattern ColorSpaceHDR10 :: ColorSpace
pattern ColorSpaceHDR10 = ColorSpace' "HDR10"

pattern ColorSpaceHlg2020 :: ColorSpace
pattern ColorSpaceHlg2020 = ColorSpace' "HLG_2020"

{-# COMPLETE 
  ColorSpaceFollow,

  ColorSpaceRec601,

  ColorSpaceRec709,

  ColorSpaceHDR10,

  ColorSpaceHlg2020,
  ColorSpace'
  #-}

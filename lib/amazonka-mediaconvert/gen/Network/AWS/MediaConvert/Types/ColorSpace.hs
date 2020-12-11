-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ColorSpace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ColorSpace
  ( ColorSpace
      ( ColorSpace',
        Follow,
        HDR10,
        Hlg2020,
        Rec601,
        Rec709
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | If your input video has accurate color space metadata, or if you don't know about color space, leave this set to the default value Follow (FOLLOW). The service will automatically detect your input color space. If your input video has metadata indicating the wrong color space, specify the accurate color space here. If your input video is HDR 10 and the SMPTE ST 2086 Mastering Display Color Volume static metadata isn't present in your video stream, or if that metadata is present but not accurate, choose Force HDR 10 (FORCE_HDR10) here and specify correct values in the input HDR 10 metadata (Hdr10Metadata) settings. For more information about MediaConvert HDR jobs, see https://docs.aws.amazon.com/console/mediaconvert/hdr.
newtype ColorSpace = ColorSpace' Lude.Text
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

pattern Follow :: ColorSpace
pattern Follow = ColorSpace' "FOLLOW"

pattern HDR10 :: ColorSpace
pattern HDR10 = ColorSpace' "HDR10"

pattern Hlg2020 :: ColorSpace
pattern Hlg2020 = ColorSpace' "HLG_2020"

pattern Rec601 :: ColorSpace
pattern Rec601 = ColorSpace' "REC_601"

pattern Rec709 :: ColorSpace
pattern Rec709 = ColorSpace' "REC_709"

{-# COMPLETE
  Follow,
  HDR10,
  Hlg2020,
  Rec601,
  Rec709,
  ColorSpace'
  #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaConvert.Types.ColorSpace
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.ColorSpace
  ( ColorSpace
      ( ..,
        ColorSpace_FOLLOW,
        ColorSpace_HDR10,
        ColorSpace_HLG_2020,
        ColorSpace_P3D65_HDR,
        ColorSpace_P3D65_SDR,
        ColorSpace_P3DCI,
        ColorSpace_REC_601,
        ColorSpace_REC_709
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | If your input video has accurate color space metadata, or if you don\'t
-- know about color space: Keep the default value, Follow. MediaConvert
-- will automatically detect your input color space. If your input video
-- has metadata indicating the wrong color space, or has missing metadata:
-- Specify the accurate color space here. If your input video is HDR 10 and
-- the SMPTE ST 2086 Mastering Display Color Volume static metadata isn\'t
-- present in your video stream, or if that metadata is present but not
-- accurate: Choose Force HDR 10. Specify correct values in the input HDR
-- 10 metadata settings. For more information about HDR jobs, see
-- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/hdr. When you
-- specify an input color space, MediaConvert uses the following color
-- space metadata, which includes color primaries, transfer
-- characteristics, and matrix coefficients: * HDR 10: BT.2020, PQ, BT.2020
-- non-constant * HLG 2020: BT.2020, HLG, BT.2020 non-constant * P3DCI
-- (Theater): DCIP3, SMPTE 428M, BT.709 * P3D65 (SDR): Display P3, sRGB,
-- BT.709 * P3D65 (HDR): Display P3, PQ, BT.709
newtype ColorSpace = ColorSpace'
  { fromColorSpace ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern ColorSpace_FOLLOW :: ColorSpace
pattern ColorSpace_FOLLOW = ColorSpace' "FOLLOW"

pattern ColorSpace_HDR10 :: ColorSpace
pattern ColorSpace_HDR10 = ColorSpace' "HDR10"

pattern ColorSpace_HLG_2020 :: ColorSpace
pattern ColorSpace_HLG_2020 = ColorSpace' "HLG_2020"

pattern ColorSpace_P3D65_HDR :: ColorSpace
pattern ColorSpace_P3D65_HDR = ColorSpace' "P3D65_HDR"

pattern ColorSpace_P3D65_SDR :: ColorSpace
pattern ColorSpace_P3D65_SDR = ColorSpace' "P3D65_SDR"

pattern ColorSpace_P3DCI :: ColorSpace
pattern ColorSpace_P3DCI = ColorSpace' "P3DCI"

pattern ColorSpace_REC_601 :: ColorSpace
pattern ColorSpace_REC_601 = ColorSpace' "REC_601"

pattern ColorSpace_REC_709 :: ColorSpace
pattern ColorSpace_REC_709 = ColorSpace' "REC_709"

{-# COMPLETE
  ColorSpace_FOLLOW,
  ColorSpace_HDR10,
  ColorSpace_HLG_2020,
  ColorSpace_P3D65_HDR,
  ColorSpace_P3D65_SDR,
  ColorSpace_P3DCI,
  ColorSpace_REC_601,
  ColorSpace_REC_709,
  ColorSpace'
  #-}

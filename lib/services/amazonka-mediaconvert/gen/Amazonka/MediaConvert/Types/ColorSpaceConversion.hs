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
-- Module      : Amazonka.MediaConvert.Types.ColorSpaceConversion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.ColorSpaceConversion
  ( ColorSpaceConversion
      ( ..,
        ColorSpaceConversion_FORCE_601,
        ColorSpaceConversion_FORCE_709,
        ColorSpaceConversion_FORCE_HDR10,
        ColorSpaceConversion_FORCE_HLG_2020,
        ColorSpaceConversion_FORCE_P3D65_HDR,
        ColorSpaceConversion_FORCE_P3D65_SDR,
        ColorSpaceConversion_FORCE_P3DCI,
        ColorSpaceConversion_NONE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify the color space you want for this output. The service supports
-- conversion between HDR formats, between SDR formats, from SDR to HDR,
-- and from HDR to SDR. SDR to HDR conversion doesn\'t upgrade the dynamic
-- range. The converted video has an HDR format, but visually appears the
-- same as an unconverted output. HDR to SDR conversion uses tone mapping
-- to approximate the outcome of manually regrading from HDR to SDR. When
-- you specify an output color space, MediaConvert uses the following color
-- space metadata, which includes color primaries, transfer
-- characteristics, and matrix coefficients: * HDR 10: BT.2020, PQ, BT.2020
-- non-constant * HLG 2020: BT.2020, HLG, BT.2020 non-constant * P3DCI
-- (Theater): DCIP3, SMPTE 428M, BT.709 * P3D65 (SDR): Display P3, sRGB,
-- BT.709 * P3D65 (HDR): Display P3, PQ, BT.709
newtype ColorSpaceConversion = ColorSpaceConversion'
  { fromColorSpaceConversion ::
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

pattern ColorSpaceConversion_FORCE_601 :: ColorSpaceConversion
pattern ColorSpaceConversion_FORCE_601 = ColorSpaceConversion' "FORCE_601"

pattern ColorSpaceConversion_FORCE_709 :: ColorSpaceConversion
pattern ColorSpaceConversion_FORCE_709 = ColorSpaceConversion' "FORCE_709"

pattern ColorSpaceConversion_FORCE_HDR10 :: ColorSpaceConversion
pattern ColorSpaceConversion_FORCE_HDR10 = ColorSpaceConversion' "FORCE_HDR10"

pattern ColorSpaceConversion_FORCE_HLG_2020 :: ColorSpaceConversion
pattern ColorSpaceConversion_FORCE_HLG_2020 = ColorSpaceConversion' "FORCE_HLG_2020"

pattern ColorSpaceConversion_FORCE_P3D65_HDR :: ColorSpaceConversion
pattern ColorSpaceConversion_FORCE_P3D65_HDR = ColorSpaceConversion' "FORCE_P3D65_HDR"

pattern ColorSpaceConversion_FORCE_P3D65_SDR :: ColorSpaceConversion
pattern ColorSpaceConversion_FORCE_P3D65_SDR = ColorSpaceConversion' "FORCE_P3D65_SDR"

pattern ColorSpaceConversion_FORCE_P3DCI :: ColorSpaceConversion
pattern ColorSpaceConversion_FORCE_P3DCI = ColorSpaceConversion' "FORCE_P3DCI"

pattern ColorSpaceConversion_NONE :: ColorSpaceConversion
pattern ColorSpaceConversion_NONE = ColorSpaceConversion' "NONE"

{-# COMPLETE
  ColorSpaceConversion_FORCE_601,
  ColorSpaceConversion_FORCE_709,
  ColorSpaceConversion_FORCE_HDR10,
  ColorSpaceConversion_FORCE_HLG_2020,
  ColorSpaceConversion_FORCE_P3D65_HDR,
  ColorSpaceConversion_FORCE_P3D65_SDR,
  ColorSpaceConversion_FORCE_P3DCI,
  ColorSpaceConversion_NONE,
  ColorSpaceConversion'
  #-}

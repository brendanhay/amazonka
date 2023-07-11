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
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.ColorSpaceConversion
  ( ColorSpaceConversion
      ( ..,
        ColorSpaceConversion_FORCE_601,
        ColorSpaceConversion_FORCE_709,
        ColorSpaceConversion_FORCE_HDR10,
        ColorSpaceConversion_FORCE_HLG_2020,
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
-- same as an unconverted output. HDR to SDR conversion uses Elemental tone
-- mapping technology to approximate the outcome of manually regrading from
-- HDR to SDR. Select Force P3D65 (SDR) to set the output color space
-- metadata to the following: * Color primaries: Display P3 * Transfer
-- characteristics: SMPTE 428M * Matrix coefficients: BT.709
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
  ColorSpaceConversion_FORCE_P3D65_SDR,
  ColorSpaceConversion_FORCE_P3DCI,
  ColorSpaceConversion_NONE,
  ColorSpaceConversion'
  #-}

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ColorSpaceConversion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.ColorSpaceConversion
  ( ColorSpaceConversion
    ( ColorSpaceConversion'
    , ColorSpaceConversionNone
    , ColorSpaceConversionForce601
    , ColorSpaceConversionForce709
    , ColorSpaceConversionForceHDR10
    , ColorSpaceConversionForceHlg2020
    , fromColorSpaceConversion
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Specify the color space you want for this output. The service supports conversion between HDR formats, between SDR formats, from SDR to HDR, and from HDR to SDR. SDR to HDR conversion doesn't upgrade the dynamic range. The converted video has an HDR format, but visually appears the same as an unconverted output. HDR to SDR conversion uses Elemental tone mapping technology to approximate the outcome of manually regrading from HDR to SDR.
newtype ColorSpaceConversion = ColorSpaceConversion'{fromColorSpaceConversion
                                                     :: Core.Text}
                                 deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                 Core.Generic)
                                 deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                   Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                   Core.FromJSON, Core.ToXML, Core.FromXML,
                                                   Core.ToText, Core.FromText, Core.ToByteString,
                                                   Core.ToQuery, Core.ToHeader)

pattern ColorSpaceConversionNone :: ColorSpaceConversion
pattern ColorSpaceConversionNone = ColorSpaceConversion' "NONE"

pattern ColorSpaceConversionForce601 :: ColorSpaceConversion
pattern ColorSpaceConversionForce601 = ColorSpaceConversion' "FORCE_601"

pattern ColorSpaceConversionForce709 :: ColorSpaceConversion
pattern ColorSpaceConversionForce709 = ColorSpaceConversion' "FORCE_709"

pattern ColorSpaceConversionForceHDR10 :: ColorSpaceConversion
pattern ColorSpaceConversionForceHDR10 = ColorSpaceConversion' "FORCE_HDR10"

pattern ColorSpaceConversionForceHlg2020 :: ColorSpaceConversion
pattern ColorSpaceConversionForceHlg2020 = ColorSpaceConversion' "FORCE_HLG_2020"

{-# COMPLETE 
  ColorSpaceConversionNone,

  ColorSpaceConversionForce601,

  ColorSpaceConversionForce709,

  ColorSpaceConversionForceHDR10,

  ColorSpaceConversionForceHlg2020,
  ColorSpaceConversion'
  #-}

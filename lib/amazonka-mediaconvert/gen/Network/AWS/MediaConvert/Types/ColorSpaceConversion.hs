-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ColorSpaceConversion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ColorSpaceConversion
  ( ColorSpaceConversion
      ( ColorSpaceConversion',
        CSCForce601,
        CSCForce709,
        CSCForceHDR10,
        CSCForceHlg2020,
        CSCNone
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specify the color space you want for this output. The service supports conversion between HDR formats, between SDR formats, from SDR to HDR, and from HDR to SDR. SDR to HDR conversion doesn't upgrade the dynamic range. The converted video has an HDR format, but visually appears the same as an unconverted output. HDR to SDR conversion uses Elemental tone mapping technology to approximate the outcome of manually regrading from HDR to SDR.
newtype ColorSpaceConversion = ColorSpaceConversion' Lude.Text
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

pattern CSCForce601 :: ColorSpaceConversion
pattern CSCForce601 = ColorSpaceConversion' "FORCE_601"

pattern CSCForce709 :: ColorSpaceConversion
pattern CSCForce709 = ColorSpaceConversion' "FORCE_709"

pattern CSCForceHDR10 :: ColorSpaceConversion
pattern CSCForceHDR10 = ColorSpaceConversion' "FORCE_HDR10"

pattern CSCForceHlg2020 :: ColorSpaceConversion
pattern CSCForceHlg2020 = ColorSpaceConversion' "FORCE_HLG_2020"

pattern CSCNone :: ColorSpaceConversion
pattern CSCNone = ColorSpaceConversion' "NONE"

{-# COMPLETE
  CSCForce601,
  CSCForce709,
  CSCForceHDR10,
  CSCForceHlg2020,
  CSCNone,
  ColorSpaceConversion'
  #-}

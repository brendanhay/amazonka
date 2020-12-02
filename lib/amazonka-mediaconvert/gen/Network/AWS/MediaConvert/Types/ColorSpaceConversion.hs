{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ColorSpaceConversion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ColorSpaceConversion where

import Network.AWS.Prelude

-- | Specify the color space you want for this output. The service supports conversion between HDR formats, between SDR formats, from SDR to HDR, and from HDR to SDR. SDR to HDR conversion doesn't upgrade the dynamic range. The converted video has an HDR format, but visually appears the same as an unconverted output. HDR to SDR conversion uses Elemental tone mapping technology to approximate the outcome of manually regrading from HDR to SDR.
data ColorSpaceConversion
  = CSCForce601
  | CSCForce709
  | CSCForceHDR10
  | CSCForceHlg2020
  | CSCNone
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText ColorSpaceConversion where
  parser =
    takeLowerText >>= \case
      "force_601" -> pure CSCForce601
      "force_709" -> pure CSCForce709
      "force_hdr10" -> pure CSCForceHDR10
      "force_hlg_2020" -> pure CSCForceHlg2020
      "none" -> pure CSCNone
      e ->
        fromTextError $
          "Failure parsing ColorSpaceConversion from value: '" <> e
            <> "'. Accepted values: force_601, force_709, force_hdr10, force_hlg_2020, none"

instance ToText ColorSpaceConversion where
  toText = \case
    CSCForce601 -> "FORCE_601"
    CSCForce709 -> "FORCE_709"
    CSCForceHDR10 -> "FORCE_HDR10"
    CSCForceHlg2020 -> "FORCE_HLG_2020"
    CSCNone -> "NONE"

instance Hashable ColorSpaceConversion

instance NFData ColorSpaceConversion

instance ToByteString ColorSpaceConversion

instance ToQuery ColorSpaceConversion

instance ToHeader ColorSpaceConversion

instance ToJSON ColorSpaceConversion where
  toJSON = toJSONText

instance FromJSON ColorSpaceConversion where
  parseJSON = parseJSONText "ColorSpaceConversion"

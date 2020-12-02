{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3StereoDownmix
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3StereoDownmix where

import Network.AWS.Prelude

-- | Choose how the service does stereo downmixing. This setting only applies if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Stereo downmix (Eac3StereoDownmix).
data Eac3StereoDownmix
  = ESDDPL2
  | ESDLoRo
  | ESDLtRt
  | ESDNotIndicated
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

instance FromText Eac3StereoDownmix where
  parser =
    takeLowerText >>= \case
      "dpl2" -> pure ESDDPL2
      "lo_ro" -> pure ESDLoRo
      "lt_rt" -> pure ESDLtRt
      "not_indicated" -> pure ESDNotIndicated
      e ->
        fromTextError $
          "Failure parsing Eac3StereoDownmix from value: '" <> e
            <> "'. Accepted values: dpl2, lo_ro, lt_rt, not_indicated"

instance ToText Eac3StereoDownmix where
  toText = \case
    ESDDPL2 -> "DPL2"
    ESDLoRo -> "LO_RO"
    ESDLtRt -> "LT_RT"
    ESDNotIndicated -> "NOT_INDICATED"

instance Hashable Eac3StereoDownmix

instance NFData Eac3StereoDownmix

instance ToByteString Eac3StereoDownmix

instance ToQuery Eac3StereoDownmix

instance ToHeader Eac3StereoDownmix

instance ToJSON Eac3StereoDownmix where
  toJSON = toJSONText

instance FromJSON Eac3StereoDownmix where
  parseJSON = parseJSONText "Eac3StereoDownmix"

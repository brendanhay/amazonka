{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Eac3StereoDownmix
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Eac3StereoDownmix where

import Network.AWS.Prelude

-- | Eac3 Stereo Downmix
data Eac3StereoDownmix
  = DPL2
  | LoRo
  | LtRt
  | NotIndicated
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
      "dpl2" -> pure DPL2
      "lo_ro" -> pure LoRo
      "lt_rt" -> pure LtRt
      "not_indicated" -> pure NotIndicated
      e ->
        fromTextError $
          "Failure parsing Eac3StereoDownmix from value: '" <> e
            <> "'. Accepted values: dpl2, lo_ro, lt_rt, not_indicated"

instance ToText Eac3StereoDownmix where
  toText = \case
    DPL2 -> "DPL2"
    LoRo -> "LO_RO"
    LtRt -> "LT_RT"
    NotIndicated -> "NOT_INDICATED"

instance Hashable Eac3StereoDownmix

instance NFData Eac3StereoDownmix

instance ToByteString Eac3StereoDownmix

instance ToQuery Eac3StereoDownmix

instance ToHeader Eac3StereoDownmix

instance ToJSON Eac3StereoDownmix where
  toJSON = toJSONText

instance FromJSON Eac3StereoDownmix where
  parseJSON = parseJSONText "Eac3StereoDownmix"

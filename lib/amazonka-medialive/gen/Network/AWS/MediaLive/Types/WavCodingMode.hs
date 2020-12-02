{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.WavCodingMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.WavCodingMode where

import Network.AWS.Prelude

-- | Wav Coding Mode
data WavCodingMode
  = WCMCodingMode10
  | WCMCodingMode20
  | WCMCodingMode40
  | WCMCodingMode80
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

instance FromText WavCodingMode where
  parser =
    takeLowerText >>= \case
      "coding_mode_1_0" -> pure WCMCodingMode10
      "coding_mode_2_0" -> pure WCMCodingMode20
      "coding_mode_4_0" -> pure WCMCodingMode40
      "coding_mode_8_0" -> pure WCMCodingMode80
      e ->
        fromTextError $
          "Failure parsing WavCodingMode from value: '" <> e
            <> "'. Accepted values: coding_mode_1_0, coding_mode_2_0, coding_mode_4_0, coding_mode_8_0"

instance ToText WavCodingMode where
  toText = \case
    WCMCodingMode10 -> "CODING_MODE_1_0"
    WCMCodingMode20 -> "CODING_MODE_2_0"
    WCMCodingMode40 -> "CODING_MODE_4_0"
    WCMCodingMode80 -> "CODING_MODE_8_0"

instance Hashable WavCodingMode

instance NFData WavCodingMode

instance ToByteString WavCodingMode

instance ToQuery WavCodingMode

instance ToHeader WavCodingMode

instance ToJSON WavCodingMode where
  toJSON = toJSONText

instance FromJSON WavCodingMode where
  parseJSON = parseJSONText "WavCodingMode"

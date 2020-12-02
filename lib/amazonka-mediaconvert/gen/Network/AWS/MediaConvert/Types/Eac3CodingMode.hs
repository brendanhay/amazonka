{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3CodingMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3CodingMode where

import Network.AWS.Prelude

-- | Dolby Digital Plus coding mode. Determines number of channels.
data Eac3CodingMode
  = ECMCodingMode10
  | ECMCodingMode20
  | ECMCodingMode32
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

instance FromText Eac3CodingMode where
  parser =
    takeLowerText >>= \case
      "coding_mode_1_0" -> pure ECMCodingMode10
      "coding_mode_2_0" -> pure ECMCodingMode20
      "coding_mode_3_2" -> pure ECMCodingMode32
      e ->
        fromTextError $
          "Failure parsing Eac3CodingMode from value: '" <> e
            <> "'. Accepted values: coding_mode_1_0, coding_mode_2_0, coding_mode_3_2"

instance ToText Eac3CodingMode where
  toText = \case
    ECMCodingMode10 -> "CODING_MODE_1_0"
    ECMCodingMode20 -> "CODING_MODE_2_0"
    ECMCodingMode32 -> "CODING_MODE_3_2"

instance Hashable Eac3CodingMode

instance NFData Eac3CodingMode

instance ToByteString Eac3CodingMode

instance ToQuery Eac3CodingMode

instance ToHeader Eac3CodingMode

instance ToJSON Eac3CodingMode where
  toJSON = toJSONText

instance FromJSON Eac3CodingMode where
  parseJSON = parseJSONText "Eac3CodingMode"

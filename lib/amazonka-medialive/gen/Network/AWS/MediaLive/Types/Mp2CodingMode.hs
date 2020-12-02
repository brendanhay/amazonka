{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Mp2CodingMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Mp2CodingMode where

import Network.AWS.Prelude

-- | Mp2 Coding Mode
data Mp2CodingMode
  = MCMCodingMode10
  | MCMCodingMode20
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

instance FromText Mp2CodingMode where
  parser =
    takeLowerText >>= \case
      "coding_mode_1_0" -> pure MCMCodingMode10
      "coding_mode_2_0" -> pure MCMCodingMode20
      e ->
        fromTextError $
          "Failure parsing Mp2CodingMode from value: '" <> e
            <> "'. Accepted values: coding_mode_1_0, coding_mode_2_0"

instance ToText Mp2CodingMode where
  toText = \case
    MCMCodingMode10 -> "CODING_MODE_1_0"
    MCMCodingMode20 -> "CODING_MODE_2_0"

instance Hashable Mp2CodingMode

instance NFData Mp2CodingMode

instance ToByteString Mp2CodingMode

instance ToQuery Mp2CodingMode

instance ToHeader Mp2CodingMode

instance ToJSON Mp2CodingMode where
  toJSON = toJSONText

instance FromJSON Mp2CodingMode where
  parseJSON = parseJSONText "Mp2CodingMode"

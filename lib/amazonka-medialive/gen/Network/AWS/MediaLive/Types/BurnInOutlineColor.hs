{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BurnInOutlineColor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BurnInOutlineColor where

import Network.AWS.Prelude

-- | Burn In Outline Color
data BurnInOutlineColor
  = BIOCBlack
  | BIOCBlue
  | BIOCGreen
  | BIOCRed
  | BIOCWhite
  | BIOCYellow
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

instance FromText BurnInOutlineColor where
  parser =
    takeLowerText >>= \case
      "black" -> pure BIOCBlack
      "blue" -> pure BIOCBlue
      "green" -> pure BIOCGreen
      "red" -> pure BIOCRed
      "white" -> pure BIOCWhite
      "yellow" -> pure BIOCYellow
      e ->
        fromTextError $
          "Failure parsing BurnInOutlineColor from value: '" <> e
            <> "'. Accepted values: black, blue, green, red, white, yellow"

instance ToText BurnInOutlineColor where
  toText = \case
    BIOCBlack -> "BLACK"
    BIOCBlue -> "BLUE"
    BIOCGreen -> "GREEN"
    BIOCRed -> "RED"
    BIOCWhite -> "WHITE"
    BIOCYellow -> "YELLOW"

instance Hashable BurnInOutlineColor

instance NFData BurnInOutlineColor

instance ToByteString BurnInOutlineColor

instance ToQuery BurnInOutlineColor

instance ToHeader BurnInOutlineColor

instance ToJSON BurnInOutlineColor where
  toJSON = toJSONText

instance FromJSON BurnInOutlineColor where
  parseJSON = parseJSONText "BurnInOutlineColor"

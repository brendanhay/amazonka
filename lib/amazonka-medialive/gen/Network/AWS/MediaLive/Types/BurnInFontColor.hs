{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BurnInFontColor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BurnInFontColor where

import Network.AWS.Prelude

-- | Burn In Font Color
data BurnInFontColor
  = BIFCBlack
  | BIFCBlue
  | BIFCGreen
  | BIFCRed
  | BIFCWhite
  | BIFCYellow
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

instance FromText BurnInFontColor where
  parser =
    takeLowerText >>= \case
      "black" -> pure BIFCBlack
      "blue" -> pure BIFCBlue
      "green" -> pure BIFCGreen
      "red" -> pure BIFCRed
      "white" -> pure BIFCWhite
      "yellow" -> pure BIFCYellow
      e ->
        fromTextError $
          "Failure parsing BurnInFontColor from value: '" <> e
            <> "'. Accepted values: black, blue, green, red, white, yellow"

instance ToText BurnInFontColor where
  toText = \case
    BIFCBlack -> "BLACK"
    BIFCBlue -> "BLUE"
    BIFCGreen -> "GREEN"
    BIFCRed -> "RED"
    BIFCWhite -> "WHITE"
    BIFCYellow -> "YELLOW"

instance Hashable BurnInFontColor

instance NFData BurnInFontColor

instance ToByteString BurnInFontColor

instance ToQuery BurnInFontColor

instance ToHeader BurnInFontColor

instance ToJSON BurnInFontColor where
  toJSON = toJSONText

instance FromJSON BurnInFontColor where
  parseJSON = parseJSONText "BurnInFontColor"

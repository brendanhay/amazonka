{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.BurninSubtitleShadowColor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.BurninSubtitleShadowColor where

import Network.AWS.Prelude

-- | Specifies the color of the shadow cast by the captions.
--
-- All burn-in and DVB-Sub font settings must match.
data BurninSubtitleShadowColor
  = BSSCBlack
  | BSSCNone
  | BSSCWhite
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

instance FromText BurninSubtitleShadowColor where
  parser =
    takeLowerText >>= \case
      "black" -> pure BSSCBlack
      "none" -> pure BSSCNone
      "white" -> pure BSSCWhite
      e ->
        fromTextError $
          "Failure parsing BurninSubtitleShadowColor from value: '" <> e
            <> "'. Accepted values: black, none, white"

instance ToText BurninSubtitleShadowColor where
  toText = \case
    BSSCBlack -> "BLACK"
    BSSCNone -> "NONE"
    BSSCWhite -> "WHITE"

instance Hashable BurninSubtitleShadowColor

instance NFData BurninSubtitleShadowColor

instance ToByteString BurninSubtitleShadowColor

instance ToQuery BurninSubtitleShadowColor

instance ToHeader BurninSubtitleShadowColor

instance ToJSON BurninSubtitleShadowColor where
  toJSON = toJSONText

instance FromJSON BurninSubtitleShadowColor where
  parseJSON = parseJSONText "BurninSubtitleShadowColor"

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BurnInShadowColor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BurnInShadowColor where

import Network.AWS.Prelude

-- | Burn In Shadow Color
data BurnInShadowColor
  = BISCBlack
  | BISCNone
  | BISCWhite
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

instance FromText BurnInShadowColor where
  parser =
    takeLowerText >>= \case
      "black" -> pure BISCBlack
      "none" -> pure BISCNone
      "white" -> pure BISCWhite
      e ->
        fromTextError $
          "Failure parsing BurnInShadowColor from value: '" <> e
            <> "'. Accepted values: black, none, white"

instance ToText BurnInShadowColor where
  toText = \case
    BISCBlack -> "BLACK"
    BISCNone -> "NONE"
    BISCWhite -> "WHITE"

instance Hashable BurnInShadowColor

instance NFData BurnInShadowColor

instance ToByteString BurnInShadowColor

instance ToQuery BurnInShadowColor

instance ToHeader BurnInShadowColor

instance ToJSON BurnInShadowColor where
  toJSON = toJSONText

instance FromJSON BurnInShadowColor where
  parseJSON = parseJSONText "BurnInShadowColor"

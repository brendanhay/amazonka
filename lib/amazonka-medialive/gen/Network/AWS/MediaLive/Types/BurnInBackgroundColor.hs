{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BurnInBackgroundColor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BurnInBackgroundColor where

import Network.AWS.Prelude

-- | Burn In Background Color
data BurnInBackgroundColor
  = BIBCBlack
  | BIBCNone
  | BIBCWhite
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

instance FromText BurnInBackgroundColor where
  parser =
    takeLowerText >>= \case
      "black" -> pure BIBCBlack
      "none" -> pure BIBCNone
      "white" -> pure BIBCWhite
      e ->
        fromTextError $
          "Failure parsing BurnInBackgroundColor from value: '" <> e
            <> "'. Accepted values: black, none, white"

instance ToText BurnInBackgroundColor where
  toText = \case
    BIBCBlack -> "BLACK"
    BIBCNone -> "NONE"
    BIBCWhite -> "WHITE"

instance Hashable BurnInBackgroundColor

instance NFData BurnInBackgroundColor

instance ToByteString BurnInBackgroundColor

instance ToQuery BurnInBackgroundColor

instance ToHeader BurnInBackgroundColor

instance ToJSON BurnInBackgroundColor where
  toJSON = toJSONText

instance FromJSON BurnInBackgroundColor where
  parseJSON = parseJSONText "BurnInBackgroundColor"

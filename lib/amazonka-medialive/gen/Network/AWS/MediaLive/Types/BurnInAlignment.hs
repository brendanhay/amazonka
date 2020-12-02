{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BurnInAlignment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BurnInAlignment where

import Network.AWS.Prelude

-- | Burn In Alignment
data BurnInAlignment
  = BIACentered
  | BIALeft'
  | BIASmart
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

instance FromText BurnInAlignment where
  parser =
    takeLowerText >>= \case
      "centered" -> pure BIACentered
      "left" -> pure BIALeft'
      "smart" -> pure BIASmart
      e ->
        fromTextError $
          "Failure parsing BurnInAlignment from value: '" <> e
            <> "'. Accepted values: centered, left, smart"

instance ToText BurnInAlignment where
  toText = \case
    BIACentered -> "CENTERED"
    BIALeft' -> "LEFT"
    BIASmart -> "SMART"

instance Hashable BurnInAlignment

instance NFData BurnInAlignment

instance ToByteString BurnInAlignment

instance ToQuery BurnInAlignment

instance ToHeader BurnInAlignment

instance ToJSON BurnInAlignment where
  toJSON = toJSONText

instance FromJSON BurnInAlignment where
  parseJSON = parseJSONText "BurnInAlignment"

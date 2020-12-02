{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.NestingLevelValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.NestingLevelValue where

import Network.AWS.Prelude

data NestingLevelValue
  = None
  | One
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

instance FromText NestingLevelValue where
  parser =
    takeLowerText >>= \case
      "none" -> pure None
      "one" -> pure One
      e ->
        fromTextError $
          "Failure parsing NestingLevelValue from value: '" <> e
            <> "'. Accepted values: none, one"

instance ToText NestingLevelValue where
  toText = \case
    None -> "none"
    One -> "one"

instance Hashable NestingLevelValue

instance NFData NestingLevelValue

instance ToByteString NestingLevelValue

instance ToQuery NestingLevelValue

instance ToHeader NestingLevelValue

instance ToJSON NestingLevelValue where
  toJSON = toJSONText

instance FromJSON NestingLevelValue where
  parseJSON = parseJSONText "NestingLevelValue"

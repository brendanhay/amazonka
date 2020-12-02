{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.BehaviorEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.BehaviorEnum where

import Network.AWS.Prelude

data BehaviorEnum
  = Cache
  | DontCache
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

instance FromText BehaviorEnum where
  parser =
    takeLowerText >>= \case
      "cache" -> pure Cache
      "dont-cache" -> pure DontCache
      e ->
        fromTextError $
          "Failure parsing BehaviorEnum from value: '" <> e
            <> "'. Accepted values: cache, dont-cache"

instance ToText BehaviorEnum where
  toText = \case
    Cache -> "cache"
    DontCache -> "dont-cache"

instance Hashable BehaviorEnum

instance NFData BehaviorEnum

instance ToByteString BehaviorEnum

instance ToQuery BehaviorEnum

instance ToHeader BehaviorEnum

instance ToJSON BehaviorEnum where
  toJSON = toJSONText

instance FromJSON BehaviorEnum where
  parseJSON = parseJSONText "BehaviorEnum"

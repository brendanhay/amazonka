{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.EventStatusCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.EventStatusCode where

import Network.AWS.Prelude

data EventStatusCode
  = Closed
  | Open
  | Upcoming
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

instance FromText EventStatusCode where
  parser =
    takeLowerText >>= \case
      "closed" -> pure Closed
      "open" -> pure Open
      "upcoming" -> pure Upcoming
      e ->
        fromTextError $
          "Failure parsing EventStatusCode from value: '" <> e
            <> "'. Accepted values: closed, open, upcoming"

instance ToText EventStatusCode where
  toText = \case
    Closed -> "closed"
    Open -> "open"
    Upcoming -> "upcoming"

instance Hashable EventStatusCode

instance NFData EventStatusCode

instance ToByteString EventStatusCode

instance ToQuery EventStatusCode

instance ToHeader EventStatusCode

instance ToJSON EventStatusCode where
  toJSON = toJSONText

instance FromJSON EventStatusCode where
  parseJSON = parseJSONText "EventStatusCode"

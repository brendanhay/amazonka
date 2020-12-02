{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.EventSourceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.EventSourceState where

import Network.AWS.Prelude

data EventSourceState
  = Active
  | Deleted
  | Pending
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

instance FromText EventSourceState where
  parser =
    takeLowerText >>= \case
      "active" -> pure Active
      "deleted" -> pure Deleted
      "pending" -> pure Pending
      e ->
        fromTextError $
          "Failure parsing EventSourceState from value: '" <> e
            <> "'. Accepted values: active, deleted, pending"

instance ToText EventSourceState where
  toText = \case
    Active -> "ACTIVE"
    Deleted -> "DELETED"
    Pending -> "PENDING"

instance Hashable EventSourceState

instance NFData EventSourceState

instance ToByteString EventSourceState

instance ToQuery EventSourceState

instance ToHeader EventSourceState

instance FromJSON EventSourceState where
  parseJSON = parseJSONText "EventSourceState"

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.TopicStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.TopicStatus where

import Network.AWS.Prelude

data TopicStatus
  = TDeleted
  | TFailed
  | TRegistered
  | TTopicNotFound
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

instance FromText TopicStatus where
  parser =
    takeLowerText >>= \case
      "deleted" -> pure TDeleted
      "failed" -> pure TFailed
      "registered" -> pure TRegistered
      "topic not found" -> pure TTopicNotFound
      e ->
        fromTextError $
          "Failure parsing TopicStatus from value: '" <> e
            <> "'. Accepted values: deleted, failed, registered, topic not found"

instance ToText TopicStatus where
  toText = \case
    TDeleted -> "Deleted"
    TFailed -> "Failed"
    TRegistered -> "Registered"
    TTopicNotFound -> "Topic not found"

instance Hashable TopicStatus

instance NFData TopicStatus

instance ToByteString TopicStatus

instance ToQuery TopicStatus

instance ToHeader TopicStatus

instance FromJSON TopicStatus where
  parseJSON = parseJSONText "TopicStatus"

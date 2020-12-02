{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.WebhookFilterType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.WebhookFilterType where

import Network.AWS.Prelude

data WebhookFilterType
  = ActorAccountId
  | BaseRef
  | CommitMessage
  | Event
  | FilePath
  | HeadRef
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

instance FromText WebhookFilterType where
  parser =
    takeLowerText >>= \case
      "actor_account_id" -> pure ActorAccountId
      "base_ref" -> pure BaseRef
      "commit_message" -> pure CommitMessage
      "event" -> pure Event
      "file_path" -> pure FilePath
      "head_ref" -> pure HeadRef
      e ->
        fromTextError $
          "Failure parsing WebhookFilterType from value: '" <> e
            <> "'. Accepted values: actor_account_id, base_ref, commit_message, event, file_path, head_ref"

instance ToText WebhookFilterType where
  toText = \case
    ActorAccountId -> "ACTOR_ACCOUNT_ID"
    BaseRef -> "BASE_REF"
    CommitMessage -> "COMMIT_MESSAGE"
    Event -> "EVENT"
    FilePath -> "FILE_PATH"
    HeadRef -> "HEAD_REF"

instance Hashable WebhookFilterType

instance NFData WebhookFilterType

instance ToByteString WebhookFilterType

instance ToQuery WebhookFilterType

instance ToHeader WebhookFilterType

instance ToJSON WebhookFilterType where
  toJSON = toJSONText

instance FromJSON WebhookFilterType where
  parseJSON = parseJSONText "WebhookFilterType"

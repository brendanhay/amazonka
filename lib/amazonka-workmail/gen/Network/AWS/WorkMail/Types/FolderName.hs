{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.FolderName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.FolderName where

import Network.AWS.Prelude

data FolderName
  = DeletedItems
  | Drafts
  | Inbox
  | JunkEmail
  | SentItems
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

instance FromText FolderName where
  parser =
    takeLowerText >>= \case
      "deleted_items" -> pure DeletedItems
      "drafts" -> pure Drafts
      "inbox" -> pure Inbox
      "junk_email" -> pure JunkEmail
      "sent_items" -> pure SentItems
      e ->
        fromTextError $
          "Failure parsing FolderName from value: '" <> e
            <> "'. Accepted values: deleted_items, drafts, inbox, junk_email, sent_items"

instance ToText FolderName where
  toText = \case
    DeletedItems -> "DELETED_ITEMS"
    Drafts -> "DRAFTS"
    Inbox -> "INBOX"
    JunkEmail -> "JUNK_EMAIL"
    SentItems -> "SENT_ITEMS"

instance Hashable FolderName

instance NFData FolderName

instance ToByteString FolderName

instance ToQuery FolderName

instance ToHeader FolderName

instance ToJSON FolderName where
  toJSON = toJSONText

instance FromJSON FolderName where
  parseJSON = parseJSONText "FolderName"

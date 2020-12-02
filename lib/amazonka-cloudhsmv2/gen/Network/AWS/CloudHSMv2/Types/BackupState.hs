{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.Types.BackupState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSMv2.Types.BackupState where

import Network.AWS.Prelude

data BackupState
  = CreateInProgress
  | Deleted
  | PendingDeletion
  | Ready
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

instance FromText BackupState where
  parser =
    takeLowerText >>= \case
      "create_in_progress" -> pure CreateInProgress
      "deleted" -> pure Deleted
      "pending_deletion" -> pure PendingDeletion
      "ready" -> pure Ready
      e ->
        fromTextError $
          "Failure parsing BackupState from value: '" <> e
            <> "'. Accepted values: create_in_progress, deleted, pending_deletion, ready"

instance ToText BackupState where
  toText = \case
    CreateInProgress -> "CREATE_IN_PROGRESS"
    Deleted -> "DELETED"
    PendingDeletion -> "PENDING_DELETION"
    Ready -> "READY"

instance Hashable BackupState

instance NFData BackupState

instance ToByteString BackupState

instance ToQuery BackupState

instance ToHeader BackupState

instance FromJSON BackupState where
  parseJSON = parseJSONText "BackupState"

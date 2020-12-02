{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.AccountRoleStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.AccountRoleStatus where

import Network.AWS.Prelude

data AccountRoleStatus
  = Creating
  | Deleted
  | Deleting
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

instance FromText AccountRoleStatus where
  parser =
    takeLowerText >>= \case
      "creating" -> pure Creating
      "deleted" -> pure Deleted
      "deleting" -> pure Deleting
      "pending_deletion" -> pure PendingDeletion
      "ready" -> pure Ready
      e ->
        fromTextError $
          "Failure parsing AccountRoleStatus from value: '" <> e
            <> "'. Accepted values: creating, deleted, deleting, pending_deletion, ready"

instance ToText AccountRoleStatus where
  toText = \case
    Creating -> "CREATING"
    Deleted -> "DELETED"
    Deleting -> "DELETING"
    PendingDeletion -> "PENDING_DELETION"
    Ready -> "READY"

instance Hashable AccountRoleStatus

instance NFData AccountRoleStatus

instance ToByteString AccountRoleStatus

instance ToQuery AccountRoleStatus

instance ToHeader AccountRoleStatus

instance FromJSON AccountRoleStatus where
  parseJSON = parseJSONText "AccountRoleStatus"

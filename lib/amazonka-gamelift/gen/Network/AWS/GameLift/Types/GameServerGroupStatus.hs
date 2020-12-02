{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameServerGroupStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameServerGroupStatus where

import Network.AWS.Prelude

data GameServerGroupStatus
  = GSGSActivating
  | GSGSActive
  | GSGSDeleteScheduled
  | GSGSDeleted
  | GSGSDeleting
  | GSGSError'
  | GSGSNew
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

instance FromText GameServerGroupStatus where
  parser =
    takeLowerText >>= \case
      "activating" -> pure GSGSActivating
      "active" -> pure GSGSActive
      "delete_scheduled" -> pure GSGSDeleteScheduled
      "deleted" -> pure GSGSDeleted
      "deleting" -> pure GSGSDeleting
      "error" -> pure GSGSError'
      "new" -> pure GSGSNew
      e ->
        fromTextError $
          "Failure parsing GameServerGroupStatus from value: '" <> e
            <> "'. Accepted values: activating, active, delete_scheduled, deleted, deleting, error, new"

instance ToText GameServerGroupStatus where
  toText = \case
    GSGSActivating -> "ACTIVATING"
    GSGSActive -> "ACTIVE"
    GSGSDeleteScheduled -> "DELETE_SCHEDULED"
    GSGSDeleted -> "DELETED"
    GSGSDeleting -> "DELETING"
    GSGSError' -> "ERROR"
    GSGSNew -> "NEW"

instance Hashable GameServerGroupStatus

instance NFData GameServerGroupStatus

instance ToByteString GameServerGroupStatus

instance ToQuery GameServerGroupStatus

instance ToHeader GameServerGroupStatus

instance FromJSON GameServerGroupStatus where
  parseJSON = parseJSONText "GameServerGroupStatus"

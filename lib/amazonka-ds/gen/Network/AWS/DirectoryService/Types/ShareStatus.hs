{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.ShareStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.ShareStatus where

import Network.AWS.Prelude

data ShareStatus
  = SSDeleted
  | SSDeleting
  | SSPendingAcceptance
  | SSRejectFailed
  | SSRejected
  | SSRejecting
  | SSShareFailed
  | SSShared
  | SSSharing
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

instance FromText ShareStatus where
  parser =
    takeLowerText >>= \case
      "deleted" -> pure SSDeleted
      "deleting" -> pure SSDeleting
      "pendingacceptance" -> pure SSPendingAcceptance
      "rejectfailed" -> pure SSRejectFailed
      "rejected" -> pure SSRejected
      "rejecting" -> pure SSRejecting
      "sharefailed" -> pure SSShareFailed
      "shared" -> pure SSShared
      "sharing" -> pure SSSharing
      e ->
        fromTextError $
          "Failure parsing ShareStatus from value: '" <> e
            <> "'. Accepted values: deleted, deleting, pendingacceptance, rejectfailed, rejected, rejecting, sharefailed, shared, sharing"

instance ToText ShareStatus where
  toText = \case
    SSDeleted -> "Deleted"
    SSDeleting -> "Deleting"
    SSPendingAcceptance -> "PendingAcceptance"
    SSRejectFailed -> "RejectFailed"
    SSRejected -> "Rejected"
    SSRejecting -> "Rejecting"
    SSShareFailed -> "ShareFailed"
    SSShared -> "Shared"
    SSSharing -> "Sharing"

instance Hashable ShareStatus

instance NFData ShareStatus

instance ToByteString ShareStatus

instance ToQuery ShareStatus

instance ToHeader ShareStatus

instance FromJSON ShareStatus where
  parseJSON = parseJSONText "ShareStatus"

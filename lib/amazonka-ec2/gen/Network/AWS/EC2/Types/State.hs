{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.State
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.State where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data State
  = SAvailable
  | SDeleted
  | SDeleting
  | SExpired
  | SFailed
  | SPending
  | SPendingAcceptance
  | SRejected
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

instance FromText State where
  parser =
    takeLowerText >>= \case
      "available" -> pure SAvailable
      "deleted" -> pure SDeleted
      "deleting" -> pure SDeleting
      "expired" -> pure SExpired
      "failed" -> pure SFailed
      "pending" -> pure SPending
      "pendingacceptance" -> pure SPendingAcceptance
      "rejected" -> pure SRejected
      e ->
        fromTextError $
          "Failure parsing State from value: '" <> e
            <> "'. Accepted values: available, deleted, deleting, expired, failed, pending, pendingacceptance, rejected"

instance ToText State where
  toText = \case
    SAvailable -> "Available"
    SDeleted -> "Deleted"
    SDeleting -> "Deleting"
    SExpired -> "Expired"
    SFailed -> "Failed"
    SPending -> "Pending"
    SPendingAcceptance -> "PendingAcceptance"
    SRejected -> "Rejected"

instance Hashable State

instance NFData State

instance ToByteString State

instance ToQuery State

instance ToHeader State

instance FromXML State where
  parseXML = parseXMLText "State"

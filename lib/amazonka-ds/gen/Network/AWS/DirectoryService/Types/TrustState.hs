{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.TrustState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.TrustState where

import Network.AWS.Prelude

data TrustState
  = TSCreated
  | TSCreating
  | TSDeleted
  | TSDeleting
  | TSFailed
  | TSUpdateFailed
  | TSUpdated
  | TSUpdating
  | TSVerified
  | TSVerifyFailed
  | TSVerifying
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

instance FromText TrustState where
  parser =
    takeLowerText >>= \case
      "created" -> pure TSCreated
      "creating" -> pure TSCreating
      "deleted" -> pure TSDeleted
      "deleting" -> pure TSDeleting
      "failed" -> pure TSFailed
      "updatefailed" -> pure TSUpdateFailed
      "updated" -> pure TSUpdated
      "updating" -> pure TSUpdating
      "verified" -> pure TSVerified
      "verifyfailed" -> pure TSVerifyFailed
      "verifying" -> pure TSVerifying
      e ->
        fromTextError $
          "Failure parsing TrustState from value: '" <> e
            <> "'. Accepted values: created, creating, deleted, deleting, failed, updatefailed, updated, updating, verified, verifyfailed, verifying"

instance ToText TrustState where
  toText = \case
    TSCreated -> "Created"
    TSCreating -> "Creating"
    TSDeleted -> "Deleted"
    TSDeleting -> "Deleting"
    TSFailed -> "Failed"
    TSUpdateFailed -> "UpdateFailed"
    TSUpdated -> "Updated"
    TSUpdating -> "Updating"
    TSVerified -> "Verified"
    TSVerifyFailed -> "VerifyFailed"
    TSVerifying -> "Verifying"

instance Hashable TrustState

instance NFData TrustState

instance ToByteString TrustState

instance ToQuery TrustState

instance ToHeader TrustState

instance FromJSON TrustState where
  parseJSON = parseJSONText "TrustState"

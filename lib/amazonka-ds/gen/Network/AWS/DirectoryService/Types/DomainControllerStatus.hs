{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.DomainControllerStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.DomainControllerStatus where

import Network.AWS.Prelude

data DomainControllerStatus
  = Active
  | Creating
  | Deleted
  | Deleting
  | Failed
  | Impaired
  | Restoring
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

instance FromText DomainControllerStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure Active
      "creating" -> pure Creating
      "deleted" -> pure Deleted
      "deleting" -> pure Deleting
      "failed" -> pure Failed
      "impaired" -> pure Impaired
      "restoring" -> pure Restoring
      e ->
        fromTextError $
          "Failure parsing DomainControllerStatus from value: '" <> e
            <> "'. Accepted values: active, creating, deleted, deleting, failed, impaired, restoring"

instance ToText DomainControllerStatus where
  toText = \case
    Active -> "Active"
    Creating -> "Creating"
    Deleted -> "Deleted"
    Deleting -> "Deleting"
    Failed -> "Failed"
    Impaired -> "Impaired"
    Restoring -> "Restoring"

instance Hashable DomainControllerStatus

instance NFData DomainControllerStatus

instance ToByteString DomainControllerStatus

instance ToQuery DomainControllerStatus

instance ToHeader DomainControllerStatus

instance FromJSON DomainControllerStatus where
  parseJSON = parseJSONText "DomainControllerStatus"

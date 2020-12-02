{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.DirectoryStage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.DirectoryStage where

import Network.AWS.Prelude

data DirectoryStage
  = DSActive
  | DSCreated
  | DSCreating
  | DSDeleted
  | DSDeleting
  | DSFailed
  | DSImpaired
  | DSInoperable
  | DSRequested
  | DSRestoreFailed
  | DSRestoring
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

instance FromText DirectoryStage where
  parser =
    takeLowerText >>= \case
      "active" -> pure DSActive
      "created" -> pure DSCreated
      "creating" -> pure DSCreating
      "deleted" -> pure DSDeleted
      "deleting" -> pure DSDeleting
      "failed" -> pure DSFailed
      "impaired" -> pure DSImpaired
      "inoperable" -> pure DSInoperable
      "requested" -> pure DSRequested
      "restorefailed" -> pure DSRestoreFailed
      "restoring" -> pure DSRestoring
      e ->
        fromTextError $
          "Failure parsing DirectoryStage from value: '" <> e
            <> "'. Accepted values: active, created, creating, deleted, deleting, failed, impaired, inoperable, requested, restorefailed, restoring"

instance ToText DirectoryStage where
  toText = \case
    DSActive -> "Active"
    DSCreated -> "Created"
    DSCreating -> "Creating"
    DSDeleted -> "Deleted"
    DSDeleting -> "Deleting"
    DSFailed -> "Failed"
    DSImpaired -> "Impaired"
    DSInoperable -> "Inoperable"
    DSRequested -> "Requested"
    DSRestoreFailed -> "RestoreFailed"
    DSRestoring -> "Restoring"

instance Hashable DirectoryStage

instance NFData DirectoryStage

instance ToByteString DirectoryStage

instance ToQuery DirectoryStage

instance ToHeader DirectoryStage

instance FromJSON DirectoryStage where
  parseJSON = parseJSONText "DirectoryStage"

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.SnapshotStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.SnapshotStatus where

import Network.AWS.Prelude

data SnapshotStatus
  = SSCompleted
  | SSCreating
  | SSFailed
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

instance FromText SnapshotStatus where
  parser =
    takeLowerText >>= \case
      "completed" -> pure SSCompleted
      "creating" -> pure SSCreating
      "failed" -> pure SSFailed
      e ->
        fromTextError $
          "Failure parsing SnapshotStatus from value: '" <> e
            <> "'. Accepted values: completed, creating, failed"

instance ToText SnapshotStatus where
  toText = \case
    SSCompleted -> "Completed"
    SSCreating -> "Creating"
    SSFailed -> "Failed"

instance Hashable SnapshotStatus

instance NFData SnapshotStatus

instance ToByteString SnapshotStatus

instance ToQuery SnapshotStatus

instance ToHeader SnapshotStatus

instance FromJSON SnapshotStatus where
  parseJSON = parseJSONText "SnapshotStatus"

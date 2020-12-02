{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.DiskSnapshotState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.DiskSnapshotState where

import Network.AWS.Prelude

data DiskSnapshotState
  = DSSCompleted
  | DSSError'
  | DSSPending
  | DSSUnknown
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

instance FromText DiskSnapshotState where
  parser =
    takeLowerText >>= \case
      "completed" -> pure DSSCompleted
      "error" -> pure DSSError'
      "pending" -> pure DSSPending
      "unknown" -> pure DSSUnknown
      e ->
        fromTextError $
          "Failure parsing DiskSnapshotState from value: '" <> e
            <> "'. Accepted values: completed, error, pending, unknown"

instance ToText DiskSnapshotState where
  toText = \case
    DSSCompleted -> "completed"
    DSSError' -> "error"
    DSSPending -> "pending"
    DSSUnknown -> "unknown"

instance Hashable DiskSnapshotState

instance NFData DiskSnapshotState

instance ToByteString DiskSnapshotState

instance ToQuery DiskSnapshotState

instance ToHeader DiskSnapshotState

instance FromJSON DiskSnapshotState where
  parseJSON = parseJSONText "DiskSnapshotState"

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.AutoSnapshotStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.AutoSnapshotStatus where

import Network.AWS.Prelude

data AutoSnapshotStatus
  = ASSFailed
  | ASSInProgress
  | ASSNotFound
  | ASSSuccess
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

instance FromText AutoSnapshotStatus where
  parser =
    takeLowerText >>= \case
      "failed" -> pure ASSFailed
      "inprogress" -> pure ASSInProgress
      "notfound" -> pure ASSNotFound
      "success" -> pure ASSSuccess
      e ->
        fromTextError $
          "Failure parsing AutoSnapshotStatus from value: '" <> e
            <> "'. Accepted values: failed, inprogress, notfound, success"

instance ToText AutoSnapshotStatus where
  toText = \case
    ASSFailed -> "Failed"
    ASSInProgress -> "InProgress"
    ASSNotFound -> "NotFound"
    ASSSuccess -> "Success"

instance Hashable AutoSnapshotStatus

instance NFData AutoSnapshotStatus

instance ToByteString AutoSnapshotStatus

instance ToQuery AutoSnapshotStatus

instance ToHeader AutoSnapshotStatus

instance FromJSON AutoSnapshotStatus where
  parseJSON = parseJSONText "AutoSnapshotStatus"

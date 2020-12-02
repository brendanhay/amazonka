{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ExportSnapshotRecordSourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ExportSnapshotRecordSourceType where

import Network.AWS.Prelude

data ExportSnapshotRecordSourceType
  = DiskSnapshot
  | InstanceSnapshot
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

instance FromText ExportSnapshotRecordSourceType where
  parser =
    takeLowerText >>= \case
      "disksnapshot" -> pure DiskSnapshot
      "instancesnapshot" -> pure InstanceSnapshot
      e ->
        fromTextError $
          "Failure parsing ExportSnapshotRecordSourceType from value: '" <> e
            <> "'. Accepted values: disksnapshot, instancesnapshot"

instance ToText ExportSnapshotRecordSourceType where
  toText = \case
    DiskSnapshot -> "DiskSnapshot"
    InstanceSnapshot -> "InstanceSnapshot"

instance Hashable ExportSnapshotRecordSourceType

instance NFData ExportSnapshotRecordSourceType

instance ToByteString ExportSnapshotRecordSourceType

instance ToQuery ExportSnapshotRecordSourceType

instance ToHeader ExportSnapshotRecordSourceType

instance FromJSON ExportSnapshotRecordSourceType where
  parseJSON = parseJSONText "ExportSnapshotRecordSourceType"

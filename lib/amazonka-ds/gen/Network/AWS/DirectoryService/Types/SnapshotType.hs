{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.SnapshotType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.SnapshotType where

import Network.AWS.Prelude

data SnapshotType
  = Auto
  | Manual
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

instance FromText SnapshotType where
  parser =
    takeLowerText >>= \case
      "auto" -> pure Auto
      "manual" -> pure Manual
      e ->
        fromTextError $
          "Failure parsing SnapshotType from value: '" <> e
            <> "'. Accepted values: auto, manual"

instance ToText SnapshotType where
  toText = \case
    Auto -> "Auto"
    Manual -> "Manual"

instance Hashable SnapshotType

instance NFData SnapshotType

instance ToByteString SnapshotType

instance ToQuery SnapshotType

instance ToHeader SnapshotType

instance FromJSON SnapshotType where
  parseJSON = parseJSONText "SnapshotType"

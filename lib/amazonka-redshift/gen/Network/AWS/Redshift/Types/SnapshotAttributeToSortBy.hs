{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.SnapshotAttributeToSortBy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.SnapshotAttributeToSortBy where

import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

data SnapshotAttributeToSortBy
  = CreateTime
  | SourceType
  | TotalSize
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

instance FromText SnapshotAttributeToSortBy where
  parser =
    takeLowerText >>= \case
      "create_time" -> pure CreateTime
      "source_type" -> pure SourceType
      "total_size" -> pure TotalSize
      e ->
        fromTextError $
          "Failure parsing SnapshotAttributeToSortBy from value: '" <> e
            <> "'. Accepted values: create_time, source_type, total_size"

instance ToText SnapshotAttributeToSortBy where
  toText = \case
    CreateTime -> "CREATE_TIME"
    SourceType -> "SOURCE_TYPE"
    TotalSize -> "TOTAL_SIZE"

instance Hashable SnapshotAttributeToSortBy

instance NFData SnapshotAttributeToSortBy

instance ToByteString SnapshotAttributeToSortBy

instance ToQuery SnapshotAttributeToSortBy

instance ToHeader SnapshotAttributeToSortBy

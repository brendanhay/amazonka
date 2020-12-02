{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.PartitionIndexStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.PartitionIndexStatus where

import Network.AWS.Prelude

data PartitionIndexStatus
  = PISActive
  | PISCreating
  | PISDeleting
  | PISFailed
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

instance FromText PartitionIndexStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure PISActive
      "creating" -> pure PISCreating
      "deleting" -> pure PISDeleting
      "failed" -> pure PISFailed
      e ->
        fromTextError $
          "Failure parsing PartitionIndexStatus from value: '" <> e
            <> "'. Accepted values: active, creating, deleting, failed"

instance ToText PartitionIndexStatus where
  toText = \case
    PISActive -> "ACTIVE"
    PISCreating -> "CREATING"
    PISDeleting -> "DELETING"
    PISFailed -> "FAILED"

instance Hashable PartitionIndexStatus

instance NFData PartitionIndexStatus

instance ToByteString PartitionIndexStatus

instance ToQuery PartitionIndexStatus

instance ToHeader PartitionIndexStatus

instance FromJSON PartitionIndexStatus where
  parseJSON = parseJSONText "PartitionIndexStatus"

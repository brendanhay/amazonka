{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.BackupStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.BackupStatus where

import Network.AWS.Prelude

data BackupStatus
  = Available
  | Creating
  | Deleted
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

instance FromText BackupStatus where
  parser =
    takeLowerText >>= \case
      "available" -> pure Available
      "creating" -> pure Creating
      "deleted" -> pure Deleted
      e ->
        fromTextError $
          "Failure parsing BackupStatus from value: '" <> e
            <> "'. Accepted values: available, creating, deleted"

instance ToText BackupStatus where
  toText = \case
    Available -> "AVAILABLE"
    Creating -> "CREATING"
    Deleted -> "DELETED"

instance Hashable BackupStatus

instance NFData BackupStatus

instance ToByteString BackupStatus

instance ToQuery BackupStatus

instance ToHeader BackupStatus

instance FromJSON BackupStatus where
  parseJSON = parseJSONText "BackupStatus"

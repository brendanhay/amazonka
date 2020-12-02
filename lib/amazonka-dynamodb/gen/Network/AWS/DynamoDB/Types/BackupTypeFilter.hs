{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.BackupTypeFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.BackupTypeFilter where

import Network.AWS.Prelude

data BackupTypeFilter
  = BTFAWSBackup
  | BTFAll
  | BTFSystem
  | BTFUser
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

instance FromText BackupTypeFilter where
  parser =
    takeLowerText >>= \case
      "aws_backup" -> pure BTFAWSBackup
      "all" -> pure BTFAll
      "system" -> pure BTFSystem
      "user" -> pure BTFUser
      e ->
        fromTextError $
          "Failure parsing BackupTypeFilter from value: '" <> e
            <> "'. Accepted values: aws_backup, all, system, user"

instance ToText BackupTypeFilter where
  toText = \case
    BTFAWSBackup -> "AWS_BACKUP"
    BTFAll -> "ALL"
    BTFSystem -> "SYSTEM"
    BTFUser -> "USER"

instance Hashable BackupTypeFilter

instance NFData BackupTypeFilter

instance ToByteString BackupTypeFilter

instance ToQuery BackupTypeFilter

instance ToHeader BackupTypeFilter

instance ToJSON BackupTypeFilter where
  toJSON = toJSONText

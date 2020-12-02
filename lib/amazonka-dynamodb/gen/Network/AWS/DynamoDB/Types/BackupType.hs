{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.BackupType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.BackupType where

import Network.AWS.Prelude

data BackupType
  = AWSBackup
  | System
  | User
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

instance FromText BackupType where
  parser =
    takeLowerText >>= \case
      "aws_backup" -> pure AWSBackup
      "system" -> pure System
      "user" -> pure User
      e ->
        fromTextError $
          "Failure parsing BackupType from value: '" <> e
            <> "'. Accepted values: aws_backup, system, user"

instance ToText BackupType where
  toText = \case
    AWSBackup -> "AWS_BACKUP"
    System -> "SYSTEM"
    User -> "USER"

instance Hashable BackupType

instance NFData BackupType

instance ToByteString BackupType

instance ToQuery BackupType

instance ToHeader BackupType

instance FromJSON BackupType where
  parseJSON = parseJSONText "BackupType"

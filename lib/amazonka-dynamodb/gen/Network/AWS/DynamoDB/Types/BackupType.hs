{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.BackupType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.BackupType
  ( BackupType
      ( BackupType',
        BackupTypeUser,
        BackupTypeSystem,
        BackupTypeAwsBackup,
        fromBackupType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype BackupType = BackupType' {fromBackupType :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern BackupTypeUser :: BackupType
pattern BackupTypeUser = BackupType' "USER"

pattern BackupTypeSystem :: BackupType
pattern BackupTypeSystem = BackupType' "SYSTEM"

pattern BackupTypeAwsBackup :: BackupType
pattern BackupTypeAwsBackup = BackupType' "AWS_BACKUP"

{-# COMPLETE
  BackupTypeUser,
  BackupTypeSystem,
  BackupTypeAwsBackup,
  BackupType'
  #-}

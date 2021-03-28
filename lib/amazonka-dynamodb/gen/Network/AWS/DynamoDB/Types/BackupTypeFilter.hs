{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.BackupTypeFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.BackupTypeFilter
  ( BackupTypeFilter
    ( BackupTypeFilter'
    , BackupTypeFilterUser
    , BackupTypeFilterSystem
    , BackupTypeFilterAwsBackup
    , BackupTypeFilterAll
    , fromBackupTypeFilter
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype BackupTypeFilter = BackupTypeFilter'{fromBackupTypeFilter
                                             :: Core.Text}
                             deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                             Core.Generic)
                             deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                               Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                               Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                               Core.FromText, Core.ToByteString, Core.ToQuery,
                                               Core.ToHeader)

pattern BackupTypeFilterUser :: BackupTypeFilter
pattern BackupTypeFilterUser = BackupTypeFilter' "USER"

pattern BackupTypeFilterSystem :: BackupTypeFilter
pattern BackupTypeFilterSystem = BackupTypeFilter' "SYSTEM"

pattern BackupTypeFilterAwsBackup :: BackupTypeFilter
pattern BackupTypeFilterAwsBackup = BackupTypeFilter' "AWS_BACKUP"

pattern BackupTypeFilterAll :: BackupTypeFilter
pattern BackupTypeFilterAll = BackupTypeFilter' "ALL"

{-# COMPLETE 
  BackupTypeFilterUser,

  BackupTypeFilterSystem,

  BackupTypeFilterAwsBackup,

  BackupTypeFilterAll,
  BackupTypeFilter'
  #-}

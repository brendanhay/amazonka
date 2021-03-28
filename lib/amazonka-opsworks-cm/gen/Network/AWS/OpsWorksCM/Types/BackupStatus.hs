{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.Types.BackupStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorksCM.Types.BackupStatus
  ( BackupStatus
    ( BackupStatus'
    , BackupStatusInProgress
    , BackupStatusOK
    , BackupStatusFailed
    , BackupStatusDeleting
    , fromBackupStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype BackupStatus = BackupStatus'{fromBackupStatus :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                         Core.Generic)
                         deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                           Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                           Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                           Core.FromText, Core.ToByteString, Core.ToQuery,
                                           Core.ToHeader)

pattern BackupStatusInProgress :: BackupStatus
pattern BackupStatusInProgress = BackupStatus' "IN_PROGRESS"

pattern BackupStatusOK :: BackupStatus
pattern BackupStatusOK = BackupStatus' "OK"

pattern BackupStatusFailed :: BackupStatus
pattern BackupStatusFailed = BackupStatus' "FAILED"

pattern BackupStatusDeleting :: BackupStatus
pattern BackupStatusDeleting = BackupStatus' "DELETING"

{-# COMPLETE 
  BackupStatusInProgress,

  BackupStatusOK,

  BackupStatusFailed,

  BackupStatusDeleting,
  BackupStatus'
  #-}

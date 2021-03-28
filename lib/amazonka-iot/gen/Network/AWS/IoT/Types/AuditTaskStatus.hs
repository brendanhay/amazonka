{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditTaskStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.AuditTaskStatus
  ( AuditTaskStatus
    ( AuditTaskStatus'
    , AuditTaskStatusInProgress
    , AuditTaskStatusCompleted
    , AuditTaskStatusFailed
    , AuditTaskStatusCanceled
    , fromAuditTaskStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype AuditTaskStatus = AuditTaskStatus'{fromAuditTaskStatus ::
                                           Core.Text}
                            deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                            Core.Generic)
                            deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                              Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                              Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                              Core.FromText, Core.ToByteString, Core.ToQuery,
                                              Core.ToHeader)

pattern AuditTaskStatusInProgress :: AuditTaskStatus
pattern AuditTaskStatusInProgress = AuditTaskStatus' "IN_PROGRESS"

pattern AuditTaskStatusCompleted :: AuditTaskStatus
pattern AuditTaskStatusCompleted = AuditTaskStatus' "COMPLETED"

pattern AuditTaskStatusFailed :: AuditTaskStatus
pattern AuditTaskStatusFailed = AuditTaskStatus' "FAILED"

pattern AuditTaskStatusCanceled :: AuditTaskStatus
pattern AuditTaskStatusCanceled = AuditTaskStatus' "CANCELED"

{-# COMPLETE 
  AuditTaskStatusInProgress,

  AuditTaskStatusCompleted,

  AuditTaskStatusFailed,

  AuditTaskStatusCanceled,
  AuditTaskStatus'
  #-}

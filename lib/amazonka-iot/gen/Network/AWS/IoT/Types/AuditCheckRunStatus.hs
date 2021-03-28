{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditCheckRunStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.AuditCheckRunStatus
  ( AuditCheckRunStatus
    ( AuditCheckRunStatus'
    , AuditCheckRunStatusInProgress
    , AuditCheckRunStatusWaitingForDataCollection
    , AuditCheckRunStatusCanceled
    , AuditCheckRunStatusCompletedCompliant
    , AuditCheckRunStatusCompletedNonCompliant
    , AuditCheckRunStatusFailed
    , fromAuditCheckRunStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype AuditCheckRunStatus = AuditCheckRunStatus'{fromAuditCheckRunStatus
                                                   :: Core.Text}
                                deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                Core.Generic)
                                deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                  Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                  Core.FromJSON, Core.ToXML, Core.FromXML,
                                                  Core.ToText, Core.FromText, Core.ToByteString,
                                                  Core.ToQuery, Core.ToHeader)

pattern AuditCheckRunStatusInProgress :: AuditCheckRunStatus
pattern AuditCheckRunStatusInProgress = AuditCheckRunStatus' "IN_PROGRESS"

pattern AuditCheckRunStatusWaitingForDataCollection :: AuditCheckRunStatus
pattern AuditCheckRunStatusWaitingForDataCollection = AuditCheckRunStatus' "WAITING_FOR_DATA_COLLECTION"

pattern AuditCheckRunStatusCanceled :: AuditCheckRunStatus
pattern AuditCheckRunStatusCanceled = AuditCheckRunStatus' "CANCELED"

pattern AuditCheckRunStatusCompletedCompliant :: AuditCheckRunStatus
pattern AuditCheckRunStatusCompletedCompliant = AuditCheckRunStatus' "COMPLETED_COMPLIANT"

pattern AuditCheckRunStatusCompletedNonCompliant :: AuditCheckRunStatus
pattern AuditCheckRunStatusCompletedNonCompliant = AuditCheckRunStatus' "COMPLETED_NON_COMPLIANT"

pattern AuditCheckRunStatusFailed :: AuditCheckRunStatus
pattern AuditCheckRunStatusFailed = AuditCheckRunStatus' "FAILED"

{-# COMPLETE 
  AuditCheckRunStatusInProgress,

  AuditCheckRunStatusWaitingForDataCollection,

  AuditCheckRunStatusCanceled,

  AuditCheckRunStatusCompletedCompliant,

  AuditCheckRunStatusCompletedNonCompliant,

  AuditCheckRunStatusFailed,
  AuditCheckRunStatus'
  #-}

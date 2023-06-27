{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudTrailData.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudTrailData.Lens
  ( -- * Operations

    -- ** PutAuditEvents
    putAuditEvents_externalId,
    putAuditEvents_auditEvents,
    putAuditEvents_channelArn,
    putAuditEventsResponse_httpStatus,
    putAuditEventsResponse_failed,
    putAuditEventsResponse_successful,

    -- * Types

    -- ** AuditEvent
    auditEvent_eventDataChecksum,
    auditEvent_eventData,
    auditEvent_id,

    -- ** AuditEventResultEntry
    auditEventResultEntry_eventID,
    auditEventResultEntry_id,

    -- ** ResultErrorEntry
    resultErrorEntry_errorCode,
    resultErrorEntry_errorMessage,
    resultErrorEntry_id,
  )
where

import Amazonka.CloudTrailData.PutAuditEvents
import Amazonka.CloudTrailData.Types.AuditEvent
import Amazonka.CloudTrailData.Types.AuditEventResultEntry
import Amazonka.CloudTrailData.Types.ResultErrorEntry

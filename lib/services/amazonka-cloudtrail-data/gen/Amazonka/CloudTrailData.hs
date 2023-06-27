{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.CloudTrailData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2021-08-11@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- The CloudTrail Data Service lets you ingest events into CloudTrail from
-- any source in your hybrid environments, such as in-house or SaaS
-- applications hosted on-premises or in the cloud, virtual machines, or
-- containers. You can store, access, analyze, troubleshoot and take action
-- on this data without maintaining multiple log aggregators and reporting
-- tools. After you run @PutAuditEvents@ to ingest your application
-- activity into CloudTrail, you can use CloudTrail Lake to search, query,
-- and analyze the data that is logged from your applications.
module Amazonka.CloudTrailData
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ChannelInsufficientPermission
    _ChannelInsufficientPermission,

    -- ** ChannelNotFound
    _ChannelNotFound,

    -- ** ChannelUnsupportedSchema
    _ChannelUnsupportedSchema,

    -- ** DuplicatedAuditEventId
    _DuplicatedAuditEventId,

    -- ** InvalidChannelARN
    _InvalidChannelARN,

    -- ** UnsupportedOperationException
    _UnsupportedOperationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** PutAuditEvents
    PutAuditEvents (PutAuditEvents'),
    newPutAuditEvents,
    PutAuditEventsResponse (PutAuditEventsResponse'),
    newPutAuditEventsResponse,

    -- * Types

    -- ** AuditEvent
    AuditEvent (AuditEvent'),
    newAuditEvent,

    -- ** AuditEventResultEntry
    AuditEventResultEntry (AuditEventResultEntry'),
    newAuditEventResultEntry,

    -- ** ResultErrorEntry
    ResultErrorEntry (ResultErrorEntry'),
    newResultErrorEntry,
  )
where

import Amazonka.CloudTrailData.Lens
import Amazonka.CloudTrailData.PutAuditEvents
import Amazonka.CloudTrailData.Types
import Amazonka.CloudTrailData.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'CloudTrailData'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.

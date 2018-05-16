{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS X-Ray provides APIs for managing debug traces and retrieving service maps and other data created by processing those traces.
--
--
module Network.AWS.XRay
    (
    -- * Service Configuration
      xRay

    -- * Errors
    -- $errors

    -- ** InvalidRequestException
    , _InvalidRequestException

    -- ** ThrottledException
    , _ThrottledException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** PutEncryptionConfig
    , module Network.AWS.XRay.PutEncryptionConfig

    -- ** GetServiceGraph (Paginated)
    , module Network.AWS.XRay.GetServiceGraph

    -- ** GetTraceSummaries (Paginated)
    , module Network.AWS.XRay.GetTraceSummaries

    -- ** PutTraceSegments
    , module Network.AWS.XRay.PutTraceSegments

    -- ** BatchGetTraces (Paginated)
    , module Network.AWS.XRay.BatchGetTraces

    -- ** GetEncryptionConfig
    , module Network.AWS.XRay.GetEncryptionConfig

    -- ** PutTelemetryRecords
    , module Network.AWS.XRay.PutTelemetryRecords

    -- ** GetTraceGraph (Paginated)
    , module Network.AWS.XRay.GetTraceGraph

    -- * Types

    -- ** EncryptionStatus
    , EncryptionStatus (..)

    -- ** EncryptionType
    , EncryptionType (..)

    -- ** Alias
    , Alias
    , alias
    , aNames
    , aName
    , aType

    -- ** AnnotationValue
    , AnnotationValue
    , annotationValue
    , avNumberValue
    , avStringValue
    , avBooleanValue

    -- ** BackendConnectionErrors
    , BackendConnectionErrors
    , backendConnectionErrors
    , bceOtherCount
    , bceTimeoutCount
    , bceHTTPCode5XXCount
    , bceConnectionRefusedCount
    , bceHTTPCode4XXCount
    , bceUnknownHostCount

    -- ** Edge
    , Edge
    , edge
    , eStartTime
    , eAliases
    , eResponseTimeHistogram
    , eReferenceId
    , eEndTime
    , eSummaryStatistics

    -- ** EdgeStatistics
    , EdgeStatistics
    , edgeStatistics
    , esFaultStatistics
    , esOKCount
    , esTotalResponseTime
    , esErrorStatistics
    , esTotalCount

    -- ** EncryptionConfig
    , EncryptionConfig
    , encryptionConfig
    , ecStatus
    , ecKeyId
    , ecType

    -- ** ErrorStatistics
    , ErrorStatistics
    , errorStatistics
    , eOtherCount
    , eThrottleCount
    , eTotalCount

    -- ** FaultStatistics
    , FaultStatistics
    , faultStatistics
    , fsOtherCount
    , fsTotalCount

    -- ** HTTP
    , HTTP
    , hTTP
    , httpHTTPMethod
    , httpHTTPStatus
    , httpClientIP
    , httpUserAgent
    , httpHTTPURL

    -- ** HistogramEntry
    , HistogramEntry
    , histogramEntry
    , heCount
    , heValue

    -- ** Segment
    , Segment
    , segment
    , sDocument
    , sId

    -- ** ServiceId
    , ServiceId
    , serviceId
    , siAccountId
    , siNames
    , siName
    , siType

    -- ** ServiceInfo
    , ServiceInfo
    , serviceInfo
    , sState
    , sStartTime
    , sRoot
    , sResponseTimeHistogram
    , sDurationHistogram
    , sReferenceId
    , sAccountId
    , sNames
    , sName
    , sEndTime
    , sType
    , sEdges
    , sSummaryStatistics

    -- ** ServiceStatistics
    , ServiceStatistics
    , serviceStatistics
    , ssFaultStatistics
    , ssOKCount
    , ssTotalResponseTime
    , ssErrorStatistics
    , ssTotalCount

    -- ** TelemetryRecord
    , TelemetryRecord
    , telemetryRecord
    , trSegmentsReceivedCount
    , trSegmentsSentCount
    , trSegmentsSpilloverCount
    , trSegmentsRejectedCount
    , trBackendConnectionErrors
    , trTimestamp

    -- ** Trace
    , Trace
    , trace
    , tId
    , tSegments
    , tDuration

    -- ** TraceSummary
    , TraceSummary
    , traceSummary
    , tsAnnotations
    , tsHasThrottle
    , tsUsers
    , tsHasFault
    , tsServiceIds
    , tsIsPartial
    , tsHasError
    , tsId
    , tsHTTP
    , tsDuration
    , tsResponseTime

    -- ** TraceUser
    , TraceUser
    , traceUser
    , tuServiceIds
    , tuUserName

    -- ** UnprocessedTraceSegment
    , UnprocessedTraceSegment
    , unprocessedTraceSegment
    , utsErrorCode
    , utsId
    , utsMessage

    -- ** ValueWithServiceIds
    , ValueWithServiceIds
    , valueWithServiceIds
    , vwsiServiceIds
    , vwsiAnnotationValue
    ) where

import Network.AWS.XRay.BatchGetTraces
import Network.AWS.XRay.GetEncryptionConfig
import Network.AWS.XRay.GetServiceGraph
import Network.AWS.XRay.GetTraceGraph
import Network.AWS.XRay.GetTraceSummaries
import Network.AWS.XRay.PutEncryptionConfig
import Network.AWS.XRay.PutTelemetryRecords
import Network.AWS.XRay.PutTraceSegments
import Network.AWS.XRay.Types
import Network.AWS.XRay.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'XRay'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}

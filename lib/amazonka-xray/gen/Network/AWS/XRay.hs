{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS X-Ray provides APIs for managing debug traces and retrieving service maps and other data created by processing those traces.
module Network.AWS.XRay
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- ** InvalidRequestException
    , _InvalidRequestException

    -- ** RuleLimitExceededException
    , _RuleLimitExceededException

    -- ** ThrottledException
    , _ThrottledException

    -- ** TooManyTagsException
    , _TooManyTagsException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** PutEncryptionConfig 
    , module Network.AWS.XRay.PutEncryptionConfig

    -- ** GetServiceGraph (Paginated)
    , module Network.AWS.XRay.GetServiceGraph

    -- ** GetSamplingTargets 
    , module Network.AWS.XRay.GetSamplingTargets

    -- ** ListTagsForResource 
    , module Network.AWS.XRay.ListTagsForResource

    -- ** GetTraceSummaries (Paginated)
    , module Network.AWS.XRay.GetTraceSummaries

    -- ** PutTraceSegments 
    , module Network.AWS.XRay.PutTraceSegments

    -- ** BatchGetTraces (Paginated)
    , module Network.AWS.XRay.BatchGetTraces

    -- ** GetInsight 
    , module Network.AWS.XRay.GetInsight

    -- ** GetTimeSeriesServiceStatistics (Paginated)
    , module Network.AWS.XRay.GetTimeSeriesServiceStatistics

    -- ** GetEncryptionConfig 
    , module Network.AWS.XRay.GetEncryptionConfig

    -- ** GetInsightImpactGraph 
    , module Network.AWS.XRay.GetInsightImpactGraph

    -- ** UpdateSamplingRule 
    , module Network.AWS.XRay.UpdateSamplingRule

    -- ** DeleteSamplingRule 
    , module Network.AWS.XRay.DeleteSamplingRule

    -- ** GetInsightEvents 
    , module Network.AWS.XRay.GetInsightEvents

    -- ** GetGroups (Paginated)
    , module Network.AWS.XRay.GetGroups

    -- ** GetInsightSummaries 
    , module Network.AWS.XRay.GetInsightSummaries

    -- ** PutTelemetryRecords 
    , module Network.AWS.XRay.PutTelemetryRecords

    -- ** GetSamplingRules (Paginated)
    , module Network.AWS.XRay.GetSamplingRules

    -- ** TagResource 
    , module Network.AWS.XRay.TagResource

    -- ** GetTraceGraph (Paginated)
    , module Network.AWS.XRay.GetTraceGraph

    -- ** CreateGroup 
    , module Network.AWS.XRay.CreateGroup

    -- ** UntagResource 
    , module Network.AWS.XRay.UntagResource

    -- ** DeleteGroup 
    , module Network.AWS.XRay.DeleteGroup

    -- ** UpdateGroup 
    , module Network.AWS.XRay.UpdateGroup

    -- ** GetGroup 
    , module Network.AWS.XRay.GetGroup

    -- ** GetSamplingStatisticSummaries (Paginated)
    , module Network.AWS.XRay.GetSamplingStatisticSummaries

    -- ** CreateSamplingRule 
    , module Network.AWS.XRay.CreateSamplingRule

    -- * Types

    -- ** AnomalousService
    , AnomalousService (..)
    , mkAnomalousService
    , asServiceId

    -- ** SegmentDocument
    , SegmentDocument (..)

    -- ** HistogramEntry
    , HistogramEntry (..)
    , mkHistogramEntry
    , heCount
    , heValue

    -- ** ClientID
    , ClientID (..)

    -- ** UnprocessedStatistics
    , UnprocessedStatistics (..)
    , mkUnprocessedStatistics
    , usErrorCode
    , usMessage
    , usRuleName

    -- ** EntitySelectorExpression
    , EntitySelectorExpression (..)

    -- ** ErrorRootCauseEntity
    , ErrorRootCauseEntity (..)
    , mkErrorRootCauseEntity
    , erceExceptions
    , erceName
    , erceRemote

    -- ** TraceId
    , TraceId (..)

    -- ** ResponseTimeRootCauseService
    , ResponseTimeRootCauseService (..)
    , mkResponseTimeRootCauseService
    , rtrcsAccountId
    , rtrcsEntityPath
    , rtrcsInferred
    , rtrcsName
    , rtrcsNames
    , rtrcsType

    -- ** EncryptionType
    , EncryptionType (..)

    -- ** Group
    , Group (..)
    , mkGroup
    , gFilterExpression
    , gGroupARN
    , gGroupName
    , gInsightsConfiguration

    -- ** FaultStatistics
    , FaultStatistics (..)
    , mkFaultStatistics
    , fsOtherCount
    , fsTotalCount

    -- ** Hostname
    , Hostname (..)

    -- ** SamplingStrategyName
    , SamplingStrategyName (..)

    -- ** AttributeValue
    , AttributeValue (..)

    -- ** HTTPMethod
    , HTTPMethod (..)

    -- ** Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- ** ResponseTimeRootCauseEntity
    , ResponseTimeRootCauseEntity (..)
    , mkResponseTimeRootCauseEntity
    , rtrceCoverage
    , rtrceName
    , rtrceRemote

    -- ** ValueWithServiceIds
    , ValueWithServiceIds (..)
    , mkValueWithServiceIds
    , vwsiAnnotationValue
    , vwsiServiceIds

    -- ** ErrorRootCauseService
    , ErrorRootCauseService (..)
    , mkErrorRootCauseService
    , ercsAccountId
    , ercsEntityPath
    , ercsInferred
    , ercsName
    , ercsNames
    , ercsType

    -- ** FilterExpression
    , FilterExpression (..)

    -- ** TraceSegmentDocument
    , TraceSegmentDocument (..)

    -- ** TraceSummary
    , TraceSummary (..)
    , mkTraceSummary
    , tsAnnotations
    , tsAvailabilityZones
    , tsDuration
    , tsEntryPoint
    , tsErrorRootCauses
    , tsFaultRootCauses
    , tsHasError
    , tsHasFault
    , tsHasThrottle
    , tsHttp
    , tsId
    , tsInstanceIds
    , tsIsPartial
    , tsMatchedEventTime
    , tsResourceARNs
    , tsResponseTime
    , tsResponseTimeRootCauses
    , tsRevision
    , tsServiceIds
    , tsUsers

    -- ** FaultRootCause
    , FaultRootCause (..)
    , mkFaultRootCause
    , frcClientImpacting
    , frcServices

    -- ** ServiceInfo
    , ServiceInfo (..)
    , mkServiceInfo
    , sAccountId
    , sDurationHistogram
    , sEdges
    , sEndTime
    , sName
    , sNames
    , sReferenceId
    , sResponseTimeHistogram
    , sRoot
    , sStartTime
    , sState
    , sSummaryStatistics
    , sType

    -- ** InsightImpactGraphService
    , InsightImpactGraphService (..)
    , mkInsightImpactGraphService
    , iigsAccountId
    , iigsEdges
    , iigsName
    , iigsNames
    , iigsReferenceId
    , iigsType

    -- ** EncryptionKeyId
    , EncryptionKeyId (..)

    -- ** SamplingRule
    , SamplingRule (..)
    , mkSamplingRule
    , srResourceARN
    , srPriority
    , srFixedRate
    , srReservoirSize
    , srServiceName
    , srServiceType
    , srHost
    , srHTTPMethod
    , srURLPath
    , srVersion
    , srAttributes
    , srRuleARN
    , srRuleName

    -- ** InsightsConfiguration
    , InsightsConfiguration (..)
    , mkInsightsConfiguration
    , icInsightsEnabled
    , icNotificationsEnabled

    -- ** EventSummaryText
    , EventSummaryText (..)

    -- ** RootCauseException
    , RootCauseException (..)
    , mkRootCauseException
    , rceMessage
    , rceName

    -- ** EC2InstanceId
    , EC2InstanceId (..)

    -- ** AnnotationKey
    , AnnotationKey (..)

    -- ** Token
    , Token (..)

    -- ** InsightId
    , InsightId (..)

    -- ** UnprocessedTraceSegment
    , UnprocessedTraceSegment (..)
    , mkUnprocessedTraceSegment
    , utsErrorCode
    , utsId
    , utsMessage

    -- ** Alias
    , Alias (..)
    , mkAlias
    , aName
    , aNames
    , aType

    -- ** FaultRootCauseEntity
    , FaultRootCauseEntity (..)
    , mkFaultRootCauseEntity
    , frceExceptions
    , frceName
    , frceRemote

    -- ** ForecastStatistics
    , ForecastStatistics (..)
    , mkForecastStatistics
    , fsFaultCountHigh
    , fsFaultCountLow

    -- ** TimeSeriesServiceStatistics
    , TimeSeriesServiceStatistics (..)
    , mkTimeSeriesServiceStatistics
    , tsssEdgeSummaryStatistics
    , tsssResponseTimeHistogram
    , tsssServiceForecastStatistics
    , tsssServiceSummaryStatistics
    , tsssTimestamp

    -- ** Insight
    , Insight (..)
    , mkInsight
    , iCategories
    , iClientRequestImpactStatistics
    , iEndTime
    , iGroupARN
    , iGroupName
    , iInsightId
    , iRootCauseServiceId
    , iRootCauseServiceRequestImpactStatistics
    , iStartTime
    , iState
    , iSummary
    , iTopAnomalousServices

    -- ** RequestImpactStatistics
    , RequestImpactStatistics (..)
    , mkRequestImpactStatistics
    , risFaultCount
    , risOkCount
    , risTotalCount

    -- ** InsightCategory
    , InsightCategory (..)

    -- ** RuleName
    , RuleName (..)

    -- ** AnnotationValue
    , AnnotationValue (..)
    , mkAnnotationValue
    , avBooleanValue
    , avNumberValue
    , avStringValue

    -- ** SamplingRuleUpdate
    , SamplingRuleUpdate (..)
    , mkSamplingRuleUpdate
    , sruAttributes
    , sruFixedRate
    , sruHTTPMethod
    , sruHost
    , sruPriority
    , sruReservoirSize
    , sruResourceARN
    , sruRuleARN
    , sruRuleName
    , sruServiceName
    , sruServiceType
    , sruURLPath

    -- ** ResourceARN
    , ResourceARN (..)

    -- ** InsightSummaryText
    , InsightSummaryText (..)

    -- ** ErrorRootCause
    , ErrorRootCause (..)
    , mkErrorRootCause
    , ercClientImpacting
    , ercServices

    -- ** InsightEvent
    , InsightEvent (..)
    , mkInsightEvent
    , ieClientRequestImpactStatistics
    , ieEventTime
    , ieRootCauseServiceRequestImpactStatistics
    , ieSummary
    , ieTopAnomalousServices

    -- ** TimeRangeType
    , TimeRangeType (..)

    -- ** ServiceName
    , ServiceName (..)

    -- ** InsightSummary
    , InsightSummary (..)
    , mkInsightSummary
    , isCategories
    , isClientRequestImpactStatistics
    , isEndTime
    , isGroupARN
    , isGroupName
    , isInsightId
    , isLastUpdateTime
    , isRootCauseServiceId
    , isRootCauseServiceRequestImpactStatistics
    , isStartTime
    , isState
    , isSummary
    , isTopAnomalousServices

    -- ** TelemetryRecord
    , TelemetryRecord (..)
    , mkTelemetryRecord
    , trTimestamp
    , trBackendConnectionErrors
    , trSegmentsReceivedCount
    , trSegmentsRejectedCount
    , trSegmentsSentCount
    , trSegmentsSpilloverCount

    -- ** GroupARN
    , GroupARN (..)

    -- ** ServiceType
    , ServiceType (..)

    -- ** InstanceIdDetail
    , InstanceIdDetail (..)
    , mkInstanceIdDetail
    , iidId

    -- ** EncryptionConfig
    , EncryptionConfig (..)
    , mkEncryptionConfig
    , ecKeyId
    , ecStatus
    , ecType

    -- ** ErrorStatistics
    , ErrorStatistics (..)
    , mkErrorStatistics
    , eOtherCount
    , eThrottleCount
    , eTotalCount

    -- ** SamplingTargetDocument
    , SamplingTargetDocument (..)
    , mkSamplingTargetDocument
    , stdFixedRate
    , stdInterval
    , stdReservoirQuota
    , stdReservoirQuotaTTL
    , stdRuleName

    -- ** TagKey
    , TagKey (..)

    -- ** SamplingStrategy
    , SamplingStrategy (..)
    , mkSamplingStrategy
    , ssName
    , ssValue

    -- ** Http
    , Http (..)
    , mkHttp
    , hClientIp
    , hHttpMethod
    , hHttpStatus
    , hHttpURL
    , hUserAgent

    -- ** SamplingRuleRecord
    , SamplingRuleRecord (..)
    , mkSamplingRuleRecord
    , srrCreatedAt
    , srrModifiedAt
    , srrSamplingRule

    -- ** Host
    , Host (..)

    -- ** ServiceStatistics
    , ServiceStatistics (..)
    , mkServiceStatistics
    , ssErrorStatistics
    , ssFaultStatistics
    , ssOkCount
    , ssTotalCount
    , ssTotalResponseTime

    -- ** TraceUser
    , TraceUser (..)
    , mkTraceUser
    , tuServiceIds
    , tuUserName

    -- ** GroupName
    , GroupName (..)

    -- ** FaultRootCauseService
    , FaultRootCauseService (..)
    , mkFaultRootCauseService
    , frcsAccountId
    , frcsEntityPath
    , frcsInferred
    , frcsName
    , frcsNames
    , frcsType

    -- ** ServiceId
    , ServiceId (..)
    , mkServiceId
    , siAccountId
    , siName
    , siNames
    , siType

    -- ** GroupSummary
    , GroupSummary (..)
    , mkGroupSummary
    , gsFilterExpression
    , gsGroupARN
    , gsGroupName
    , gsInsightsConfiguration

    -- ** Segment
    , Segment (..)
    , mkSegment
    , sDocument
    , sId

    -- ** AttributeKey
    , AttributeKey (..)

    -- ** Edge
    , Edge (..)
    , mkEdge
    , eAliases
    , eEndTime
    , eReferenceId
    , eResponseTimeHistogram
    , eStartTime
    , eSummaryStatistics

    -- ** InsightImpactGraphEdge
    , InsightImpactGraphEdge (..)
    , mkInsightImpactGraphEdge
    , iigeReferenceId

    -- ** AmazonResourceName
    , AmazonResourceName (..)

    -- ** EncryptionStatus
    , EncryptionStatus (..)

    -- ** EdgeStatistics
    , EdgeStatistics (..)
    , mkEdgeStatistics
    , esErrorStatistics
    , esFaultStatistics
    , esOkCount
    , esTotalCount
    , esTotalResponseTime

    -- ** AvailabilityZoneDetail
    , AvailabilityZoneDetail (..)
    , mkAvailabilityZoneDetail
    , azdName

    -- ** ResourceARNDetail
    , ResourceARNDetail (..)
    , mkResourceARNDetail
    , rarndARN

    -- ** URLPath
    , URLPath (..)

    -- ** SamplingStatisticsDocument
    , SamplingStatisticsDocument (..)
    , mkSamplingStatisticsDocument
    , ssdRuleName
    , ssdClientID
    , ssdTimestamp
    , ssdRequestCount
    , ssdSampledCount
    , ssdBorrowCount

    -- ** BackendConnectionErrors
    , BackendConnectionErrors (..)
    , mkBackendConnectionErrors
    , bceConnectionRefusedCount
    , bceHTTPCode4XXCount
    , bceHTTPCode5XXCount
    , bceOtherCount
    , bceTimeoutCount
    , bceUnknownHostCount

    -- ** InsightState
    , InsightState (..)

    -- ** SamplingStatisticSummary
    , SamplingStatisticSummary (..)
    , mkSamplingStatisticSummary
    , sssBorrowCount
    , sssRequestCount
    , sssRuleName
    , sssSampledCount
    , sssTimestamp

    -- ** ResponseTimeRootCause
    , ResponseTimeRootCause (..)
    , mkResponseTimeRootCause
    , rtrcClientImpacting
    , rtrcServices

    -- ** Trace
    , Trace (..)
    , mkTrace
    , tDuration
    , tId
    , tLimitExceeded
    , tSegments

    -- ** NextToken
    , NextToken (..)

    -- ** Key
    , Key (..)

    -- ** Value
    , Value (..)

    -- ** Summary
    , Summary (..)

    -- ** Id
    , Id (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.XRay.Types
import Network.AWS.XRay.Waiters
import Network.AWS.XRay.PutEncryptionConfig
import Network.AWS.XRay.GetServiceGraph
import Network.AWS.XRay.GetSamplingTargets
import Network.AWS.XRay.ListTagsForResource
import Network.AWS.XRay.GetTraceSummaries
import Network.AWS.XRay.PutTraceSegments
import Network.AWS.XRay.BatchGetTraces
import Network.AWS.XRay.GetInsight
import Network.AWS.XRay.GetTimeSeriesServiceStatistics
import Network.AWS.XRay.GetEncryptionConfig
import Network.AWS.XRay.GetInsightImpactGraph
import Network.AWS.XRay.UpdateSamplingRule
import Network.AWS.XRay.DeleteSamplingRule
import Network.AWS.XRay.GetInsightEvents
import Network.AWS.XRay.GetGroups
import Network.AWS.XRay.GetInsightSummaries
import Network.AWS.XRay.PutTelemetryRecords
import Network.AWS.XRay.GetSamplingRules
import Network.AWS.XRay.TagResource
import Network.AWS.XRay.GetTraceGraph
import Network.AWS.XRay.CreateGroup
import Network.AWS.XRay.UntagResource
import Network.AWS.XRay.DeleteGroup
import Network.AWS.XRay.UpdateGroup
import Network.AWS.XRay.GetGroup
import Network.AWS.XRay.GetSamplingStatisticSummaries
import Network.AWS.XRay.CreateSamplingRule
import qualified Network.AWS.Prelude as Lude

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

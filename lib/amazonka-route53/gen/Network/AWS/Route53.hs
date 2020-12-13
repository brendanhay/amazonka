{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Route 53 is a scalable Domain Name System (DNS) web service. It provides secure and reliable routing to your infrastructure that uses Amazon Web Services (AWS) products, such as Amazon Elastic Compute Cloud (Amazon EC2), Elastic Load Balancing, or Amazon Simple Storage Service (Amazon S3). You can also use Amazon Route 53 to route users to your infrastructure outside of AWS.
module Network.AWS.Route53
  ( -- * Service configuration
    route53Service,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- ** ResourceRecordSetsChanged
    mkResourceRecordSetsChanged,

    -- * Operations
    -- $operations

    -- ** GetHostedZoneLimit
    module Network.AWS.Route53.GetHostedZoneLimit,

    -- ** AssociateVPCWithHostedZone
    module Network.AWS.Route53.AssociateVPCWithHostedZone,

    -- ** DeleteTrafficPolicy
    module Network.AWS.Route53.DeleteTrafficPolicy,

    -- ** GetCheckerIPRanges
    module Network.AWS.Route53.GetCheckerIPRanges,

    -- ** GetTrafficPolicyInstance
    module Network.AWS.Route53.GetTrafficPolicyInstance,

    -- ** GetHealthCheckLastFailureReason
    module Network.AWS.Route53.GetHealthCheckLastFailureReason,

    -- ** DeleteReusableDelegationSet
    module Network.AWS.Route53.DeleteReusableDelegationSet,

    -- ** ListHostedZonesByName
    module Network.AWS.Route53.ListHostedZonesByName,

    -- ** ListReusableDelegationSets
    module Network.AWS.Route53.ListReusableDelegationSets,

    -- ** ListTagsForResource
    module Network.AWS.Route53.ListTagsForResource,

    -- ** ListQueryLoggingConfigs (Paginated)
    module Network.AWS.Route53.ListQueryLoggingConfigs,

    -- ** ListTrafficPolicyInstances
    module Network.AWS.Route53.ListTrafficPolicyInstances,

    -- ** CreateTrafficPolicyInstance
    module Network.AWS.Route53.CreateTrafficPolicyInstance,

    -- ** GetChange
    module Network.AWS.Route53.GetChange,

    -- ** ChangeResourceRecordSets
    module Network.AWS.Route53.ChangeResourceRecordSets,

    -- ** DeleteHealthCheck
    module Network.AWS.Route53.DeleteHealthCheck,

    -- ** UpdateHealthCheck
    module Network.AWS.Route53.UpdateHealthCheck,

    -- ** CreateHostedZone
    module Network.AWS.Route53.CreateHostedZone,

    -- ** CreateVPCAssociationAuthorization
    module Network.AWS.Route53.CreateVPCAssociationAuthorization,

    -- ** ListVPCAssociationAuthorizations (Paginated)
    module Network.AWS.Route53.ListVPCAssociationAuthorizations,

    -- ** ListTrafficPolicyInstancesByPolicy
    module Network.AWS.Route53.ListTrafficPolicyInstancesByPolicy,

    -- ** DisassociateVPCFromHostedZone
    module Network.AWS.Route53.DisassociateVPCFromHostedZone,

    -- ** CreateHealthCheck
    module Network.AWS.Route53.CreateHealthCheck,

    -- ** DeleteVPCAssociationAuthorization
    module Network.AWS.Route53.DeleteVPCAssociationAuthorization,

    -- ** ChangeTagsForResource
    module Network.AWS.Route53.ChangeTagsForResource,

    -- ** ListHostedZones (Paginated)
    module Network.AWS.Route53.ListHostedZones,

    -- ** GetTrafficPolicyInstanceCount
    module Network.AWS.Route53.GetTrafficPolicyInstanceCount,

    -- ** ListGeoLocations
    module Network.AWS.Route53.ListGeoLocations,

    -- ** GetHostedZone
    module Network.AWS.Route53.GetHostedZone,

    -- ** GetHealthCheck
    module Network.AWS.Route53.GetHealthCheck,

    -- ** ListResourceRecordSets (Paginated)
    module Network.AWS.Route53.ListResourceRecordSets,

    -- ** CreateReusableDelegationSet
    module Network.AWS.Route53.CreateReusableDelegationSet,

    -- ** CreateQueryLoggingConfig
    module Network.AWS.Route53.CreateQueryLoggingConfig,

    -- ** GetHealthCheckCount
    module Network.AWS.Route53.GetHealthCheckCount,

    -- ** UpdateTrafficPolicyComment
    module Network.AWS.Route53.UpdateTrafficPolicyComment,

    -- ** GetHostedZoneCount
    module Network.AWS.Route53.GetHostedZoneCount,

    -- ** GetAccountLimit
    module Network.AWS.Route53.GetAccountLimit,

    -- ** DeleteQueryLoggingConfig
    module Network.AWS.Route53.DeleteQueryLoggingConfig,

    -- ** GetQueryLoggingConfig
    module Network.AWS.Route53.GetQueryLoggingConfig,

    -- ** GetReusableDelegationSet
    module Network.AWS.Route53.GetReusableDelegationSet,

    -- ** DeleteTrafficPolicyInstance
    module Network.AWS.Route53.DeleteTrafficPolicyInstance,

    -- ** UpdateTrafficPolicyInstance
    module Network.AWS.Route53.UpdateTrafficPolicyInstance,

    -- ** UpdateHostedZoneComment
    module Network.AWS.Route53.UpdateHostedZoneComment,

    -- ** GetHealthCheckStatus
    module Network.AWS.Route53.GetHealthCheckStatus,

    -- ** ListHostedZonesByVPC
    module Network.AWS.Route53.ListHostedZonesByVPC,

    -- ** GetReusableDelegationSetLimit
    module Network.AWS.Route53.GetReusableDelegationSetLimit,

    -- ** CreateTrafficPolicyVersion
    module Network.AWS.Route53.CreateTrafficPolicyVersion,

    -- ** TestDNSAnswer
    module Network.AWS.Route53.TestDNSAnswer,

    -- ** ListHealthChecks (Paginated)
    module Network.AWS.Route53.ListHealthChecks,

    -- ** GetTrafficPolicy
    module Network.AWS.Route53.GetTrafficPolicy,

    -- ** ListTrafficPolicyVersions
    module Network.AWS.Route53.ListTrafficPolicyVersions,

    -- ** DeleteHostedZone
    module Network.AWS.Route53.DeleteHostedZone,

    -- ** GetGeoLocation
    module Network.AWS.Route53.GetGeoLocation,

    -- ** ListTagsForResources
    module Network.AWS.Route53.ListTagsForResources,

    -- ** CreateTrafficPolicy
    module Network.AWS.Route53.CreateTrafficPolicy,

    -- ** ListTrafficPolicyInstancesByHostedZone
    module Network.AWS.Route53.ListTrafficPolicyInstancesByHostedZone,

    -- ** ListTrafficPolicies
    module Network.AWS.Route53.ListTrafficPolicies,

    -- * Types

    -- ** Common
    module Network.AWS.Route53.Internal,

    -- ** AccountLimitType
    AccountLimitType (..),

    -- ** ChangeAction
    ChangeAction (..),

    -- ** ChangeStatus
    ChangeStatus (..),

    -- ** CloudWatchRegion
    CloudWatchRegion (..),

    -- ** ComparisonOperator
    ComparisonOperator (..),

    -- ** Failover
    Failover (..),

    -- ** HealthCheckRegion
    HealthCheckRegion (..),

    -- ** HealthCheckType
    HealthCheckType (..),

    -- ** HostedZoneLimitType
    HostedZoneLimitType (..),

    -- ** InsufficientDataHealthStatus
    InsufficientDataHealthStatus (..),

    -- ** RecordType
    RecordType (..),

    -- ** ResettableElementName
    ResettableElementName (..),

    -- ** ReusableDelegationSetLimitType
    ReusableDelegationSetLimitType (..),

    -- ** Statistic
    Statistic (..),

    -- ** TagResourceType
    TagResourceType (..),

    -- ** VPCRegion
    VPCRegion (..),

    -- ** AccountLimit
    AccountLimit (..),
    mkAccountLimit,
    alValue,
    alType,

    -- ** AlarmIdentifier
    AlarmIdentifier (..),
    mkAlarmIdentifier,
    aiName,
    aiRegion,

    -- ** AliasTarget
    AliasTarget (..),
    mkAliasTarget,
    atHostedZoneId,
    atEvaluateTargetHealth,
    atDNSName,

    -- ** Change
    Change (..),
    mkChange,
    cAction,
    cResourceRecordSet,

    -- ** ChangeBatch
    ChangeBatch (..),
    mkChangeBatch,
    cbChanges,
    cbComment,

    -- ** ChangeInfo
    ChangeInfo (..),
    mkChangeInfo,
    ciStatus,
    ciSubmittedAt,
    ciId,
    ciComment,

    -- ** CloudWatchAlarmConfiguration
    CloudWatchAlarmConfiguration (..),
    mkCloudWatchAlarmConfiguration,
    cwacPeriod,
    cwacEvaluationPeriods,
    cwacMetricName,
    cwacNamespace,
    cwacComparisonOperator,
    cwacThreshold,
    cwacDimensions,
    cwacStatistic,

    -- ** DelegationSet
    DelegationSet (..),
    mkDelegationSet,
    dsId,
    dsCallerReference,
    dsNameServers,

    -- ** Dimension
    Dimension (..),
    mkDimension,
    dValue,
    dName,

    -- ** GeoLocation
    GeoLocation (..),
    mkGeoLocation,
    glSubdivisionCode,
    glCountryCode,
    glContinentCode,

    -- ** GeoLocationDetails
    GeoLocationDetails (..),
    mkGeoLocationDetails,
    gldSubdivisionName,
    gldSubdivisionCode,
    gldCountryName,
    gldCountryCode,
    gldContinentCode,
    gldContinentName,

    -- ** HealthCheck
    HealthCheck (..),
    mkHealthCheck,
    hcLinkedService,
    hcHealthCheckConfig,
    hcCloudWatchAlarmConfiguration,
    hcId,
    hcHealthCheckVersion,
    hcCallerReference,

    -- ** HealthCheckConfig
    HealthCheckConfig (..),
    mkHealthCheckConfig,
    hccFailureThreshold,
    hccIPAddress,
    hccEnableSNI,
    hccDisabled,
    hccSearchString,
    hccHealthThreshold,
    hccRegions,
    hccResourcePath,
    hccInsufficientDataHealthStatus,
    hccType,
    hccAlarmIdentifier,
    hccMeasureLatency,
    hccInverted,
    hccFullyQualifiedDomainName,
    hccChildHealthChecks,
    hccRequestInterval,
    hccPort,

    -- ** HealthCheckObservation
    HealthCheckObservation (..),
    mkHealthCheckObservation,
    hcoIPAddress,
    hcoStatusReport,
    hcoRegion,

    -- ** HostedZone
    HostedZone (..),
    mkHostedZone,
    hzLinkedService,
    hzConfig,
    hzName,
    hzId,
    hzResourceRecordSetCount,
    hzCallerReference,

    -- ** HostedZoneConfig
    HostedZoneConfig (..),
    mkHostedZoneConfig,
    hzcPrivateZone,
    hzcComment,

    -- ** HostedZoneLimit
    HostedZoneLimit (..),
    mkHostedZoneLimit,
    hzlValue,
    hzlType,

    -- ** HostedZoneOwner
    HostedZoneOwner (..),
    mkHostedZoneOwner,
    hzoOwningAccount,
    hzoOwningService,

    -- ** HostedZoneSummary
    HostedZoneSummary (..),
    mkHostedZoneSummary,
    hzsHostedZoneId,
    hzsOwner,
    hzsName,

    -- ** LinkedService
    LinkedService (..),
    mkLinkedService,
    lsServicePrincipal,
    lsDescription,

    -- ** QueryLoggingConfig
    QueryLoggingConfig (..),
    mkQueryLoggingConfig,
    qlcHostedZoneId,
    qlcCloudWatchLogsLogGroupARN,
    qlcId,

    -- ** ResourceRecord
    ResourceRecord (..),
    mkResourceRecord,
    rrValue,

    -- ** ResourceRecordSet
    ResourceRecordSet (..),
    mkResourceRecordSet,
    rrsTTL,
    rrsResourceRecords,
    rrsAliasTarget,
    rrsWeight,
    rrsTrafficPolicyInstanceId,
    rrsSetIdentifier,
    rrsFailover,
    rrsName,
    rrsHealthCheckId,
    rrsRegion,
    rrsType,
    rrsGeoLocation,
    rrsMultiValueAnswer,

    -- ** ResourceTagSet
    ResourceTagSet (..),
    mkResourceTagSet,
    rtsResourceId,
    rtsResourceType,
    rtsTags,

    -- ** ReusableDelegationSetLimit
    ReusableDelegationSetLimit (..),
    mkReusableDelegationSetLimit,
    rdslValue,
    rdslType,

    -- ** StatusReport
    StatusReport (..),
    mkStatusReport,
    srStatus,
    srCheckedTime,

    -- ** Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- ** TrafficPolicy
    TrafficPolicy (..),
    mkTrafficPolicy,
    tpDocument,
    tpName,
    tpVersion,
    tpId,
    tpType,
    tpComment,

    -- ** TrafficPolicyInstance
    TrafficPolicyInstance (..),
    mkTrafficPolicyInstance,
    tpiTTL,
    tpiState,
    tpiTrafficPolicyVersion,
    tpiHostedZoneId,
    tpiName,
    tpiId,
    tpiTrafficPolicyType,
    tpiTrafficPolicyId,
    tpiMessage,

    -- ** TrafficPolicySummary
    TrafficPolicySummary (..),
    mkTrafficPolicySummary,
    tpsTrafficPolicyCount,
    tpsName,
    tpsId,
    tpsType,
    tpsLatestVersion,

    -- ** VPC
    VPC (..),
    mkVPC,
    vpcVPCRegion,
    vpcVPCId,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.DateTime,
    Lude.Timestamp,
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.AssociateVPCWithHostedZone
import Network.AWS.Route53.ChangeResourceRecordSets
import Network.AWS.Route53.ChangeTagsForResource
import Network.AWS.Route53.CreateHealthCheck
import Network.AWS.Route53.CreateHostedZone
import Network.AWS.Route53.CreateQueryLoggingConfig
import Network.AWS.Route53.CreateReusableDelegationSet
import Network.AWS.Route53.CreateTrafficPolicy
import Network.AWS.Route53.CreateTrafficPolicyInstance
import Network.AWS.Route53.CreateTrafficPolicyVersion
import Network.AWS.Route53.CreateVPCAssociationAuthorization
import Network.AWS.Route53.DeleteHealthCheck
import Network.AWS.Route53.DeleteHostedZone
import Network.AWS.Route53.DeleteQueryLoggingConfig
import Network.AWS.Route53.DeleteReusableDelegationSet
import Network.AWS.Route53.DeleteTrafficPolicy
import Network.AWS.Route53.DeleteTrafficPolicyInstance
import Network.AWS.Route53.DeleteVPCAssociationAuthorization
import Network.AWS.Route53.DisassociateVPCFromHostedZone
import Network.AWS.Route53.GetAccountLimit
import Network.AWS.Route53.GetChange
import Network.AWS.Route53.GetCheckerIPRanges
import Network.AWS.Route53.GetGeoLocation
import Network.AWS.Route53.GetHealthCheck
import Network.AWS.Route53.GetHealthCheckCount
import Network.AWS.Route53.GetHealthCheckLastFailureReason
import Network.AWS.Route53.GetHealthCheckStatus
import Network.AWS.Route53.GetHostedZone
import Network.AWS.Route53.GetHostedZoneCount
import Network.AWS.Route53.GetHostedZoneLimit
import Network.AWS.Route53.GetQueryLoggingConfig
import Network.AWS.Route53.GetReusableDelegationSet
import Network.AWS.Route53.GetReusableDelegationSetLimit
import Network.AWS.Route53.GetTrafficPolicy
import Network.AWS.Route53.GetTrafficPolicyInstance
import Network.AWS.Route53.GetTrafficPolicyInstanceCount
import Network.AWS.Route53.Internal
import Network.AWS.Route53.ListGeoLocations
import Network.AWS.Route53.ListHealthChecks
import Network.AWS.Route53.ListHostedZones
import Network.AWS.Route53.ListHostedZonesByName
import Network.AWS.Route53.ListHostedZonesByVPC
import Network.AWS.Route53.ListQueryLoggingConfigs
import Network.AWS.Route53.ListResourceRecordSets
import Network.AWS.Route53.ListReusableDelegationSets
import Network.AWS.Route53.ListTagsForResource
import Network.AWS.Route53.ListTagsForResources
import Network.AWS.Route53.ListTrafficPolicies
import Network.AWS.Route53.ListTrafficPolicyInstances
import Network.AWS.Route53.ListTrafficPolicyInstancesByHostedZone
import Network.AWS.Route53.ListTrafficPolicyInstancesByPolicy
import Network.AWS.Route53.ListTrafficPolicyVersions
import Network.AWS.Route53.ListVPCAssociationAuthorizations
import Network.AWS.Route53.TestDNSAnswer
import Network.AWS.Route53.Types
import Network.AWS.Route53.UpdateHealthCheck
import Network.AWS.Route53.UpdateHostedZoneComment
import Network.AWS.Route53.UpdateTrafficPolicyComment
import Network.AWS.Route53.UpdateTrafficPolicyInstance
import Network.AWS.Route53.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Route53'.

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

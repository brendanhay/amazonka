-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types
  ( -- * Service configuration
    route53Service,

    -- * Errors

    -- * Re-exported types
    module Network.AWS.Route53.Internal,

    -- * AccountLimitType
    AccountLimitType (..),

    -- * ChangeAction
    ChangeAction (..),

    -- * ChangeStatus
    ChangeStatus (..),

    -- * CloudWatchRegion
    CloudWatchRegion (..),

    -- * ComparisonOperator
    ComparisonOperator (..),

    -- * Failover
    Failover (..),

    -- * HealthCheckRegion
    HealthCheckRegion (..),

    -- * HealthCheckType
    HealthCheckType (..),

    -- * HostedZoneLimitType
    HostedZoneLimitType (..),

    -- * InsufficientDataHealthStatus
    InsufficientDataHealthStatus (..),

    -- * RecordType
    RecordType (..),

    -- * ResettableElementName
    ResettableElementName (..),

    -- * ReusableDelegationSetLimitType
    ReusableDelegationSetLimitType (..),

    -- * Statistic
    Statistic (..),

    -- * TagResourceType
    TagResourceType (..),

    -- * VPCRegion
    VPCRegion (..),

    -- * AccountLimit
    AccountLimit (..),
    mkAccountLimit,
    alType,
    alValue,

    -- * AlarmIdentifier
    AlarmIdentifier (..),
    mkAlarmIdentifier,
    aiRegion,
    aiName,

    -- * AliasTarget
    AliasTarget (..),
    mkAliasTarget,
    atHostedZoneId,
    atDNSName,
    atEvaluateTargetHealth,

    -- * Change
    Change (..),
    mkChange,
    cAction,
    cResourceRecordSet,

    -- * ChangeBatch
    ChangeBatch (..),
    mkChangeBatch,
    cbComment,
    cbChanges,

    -- * ChangeInfo
    ChangeInfo (..),
    mkChangeInfo,
    ciComment,
    ciId,
    ciStatus,
    ciSubmittedAt,

    -- * CloudWatchAlarmConfiguration
    CloudWatchAlarmConfiguration (..),
    mkCloudWatchAlarmConfiguration,
    cwacDimensions,
    cwacEvaluationPeriods,
    cwacThreshold,
    cwacComparisonOperator,
    cwacPeriod,
    cwacMetricName,
    cwacNamespace,
    cwacStatistic,

    -- * DelegationSet
    DelegationSet (..),
    mkDelegationSet,
    dsId,
    dsCallerReference,
    dsNameServers,

    -- * Dimension
    Dimension (..),
    mkDimension,
    dName,
    dValue,

    -- * GeoLocation
    GeoLocation (..),
    mkGeoLocation,
    glSubdivisionCode,
    glCountryCode,
    glContinentCode,

    -- * GeoLocationDetails
    GeoLocationDetails (..),
    mkGeoLocationDetails,
    gldSubdivisionName,
    gldSubdivisionCode,
    gldCountryName,
    gldCountryCode,
    gldContinentCode,
    gldContinentName,

    -- * HealthCheck
    HealthCheck (..),
    mkHealthCheck,
    hcLinkedService,
    hcCloudWatchAlarmConfiguration,
    hcId,
    hcCallerReference,
    hcHealthCheckConfig,
    hcHealthCheckVersion,

    -- * HealthCheckConfig
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
    hccAlarmIdentifier,
    hccMeasureLatency,
    hccInverted,
    hccFullyQualifiedDomainName,
    hccChildHealthChecks,
    hccRequestInterval,
    hccPort,
    hccType,

    -- * HealthCheckObservation
    HealthCheckObservation (..),
    mkHealthCheckObservation,
    hcoIPAddress,
    hcoStatusReport,
    hcoRegion,

    -- * HostedZone
    HostedZone (..),
    mkHostedZone,
    hzLinkedService,
    hzConfig,
    hzResourceRecordSetCount,
    hzId,
    hzName,
    hzCallerReference,

    -- * HostedZoneConfig
    HostedZoneConfig (..),
    mkHostedZoneConfig,
    hzcPrivateZone,
    hzcComment,

    -- * HostedZoneLimit
    HostedZoneLimit (..),
    mkHostedZoneLimit,
    hzlType,
    hzlValue,

    -- * HostedZoneOwner
    HostedZoneOwner (..),
    mkHostedZoneOwner,
    hzoOwningAccount,
    hzoOwningService,

    -- * HostedZoneSummary
    HostedZoneSummary (..),
    mkHostedZoneSummary,
    hzsHostedZoneId,
    hzsName,
    hzsOwner,

    -- * LinkedService
    LinkedService (..),
    mkLinkedService,
    lsServicePrincipal,
    lsDescription,

    -- * QueryLoggingConfig
    QueryLoggingConfig (..),
    mkQueryLoggingConfig,
    qlcId,
    qlcHostedZoneId,
    qlcCloudWatchLogsLogGroupARN,

    -- * ResourceRecord
    ResourceRecord (..),
    mkResourceRecord,
    rrValue,

    -- * ResourceRecordSet
    ResourceRecordSet (..),
    mkResourceRecordSet,
    rrsTTL,
    rrsResourceRecords,
    rrsAliasTarget,
    rrsWeight,
    rrsTrafficPolicyInstanceId,
    rrsSetIdentifier,
    rrsFailover,
    rrsHealthCheckId,
    rrsRegion,
    rrsGeoLocation,
    rrsMultiValueAnswer,
    rrsName,
    rrsType,

    -- * ResourceTagSet
    ResourceTagSet (..),
    mkResourceTagSet,
    rtsResourceId,
    rtsResourceType,
    rtsTags,

    -- * ReusableDelegationSetLimit
    ReusableDelegationSetLimit (..),
    mkReusableDelegationSetLimit,
    rdslType,
    rdslValue,

    -- * StatusReport
    StatusReport (..),
    mkStatusReport,
    srStatus,
    srCheckedTime,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * TrafficPolicy
    TrafficPolicy (..),
    mkTrafficPolicy,
    tpComment,
    tpId,
    tpVersion,
    tpName,
    tpType,
    tpDocument,

    -- * TrafficPolicyInstance
    TrafficPolicyInstance (..),
    mkTrafficPolicyInstance,
    tpiId,
    tpiHostedZoneId,
    tpiName,
    tpiTTL,
    tpiState,
    tpiMessage,
    tpiTrafficPolicyId,
    tpiTrafficPolicyVersion,
    tpiTrafficPolicyType,

    -- * TrafficPolicySummary
    TrafficPolicySummary (..),
    mkTrafficPolicySummary,
    tpsId,
    tpsName,
    tpsType,
    tpsLatestVersion,
    tpsTrafficPolicyCount,

    -- * VPC
    VPC (..),
    mkVPC,
    vpcVPCRegion,
    vpcVPCId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.AccountLimit
import Network.AWS.Route53.Types.AccountLimitType
import Network.AWS.Route53.Types.AlarmIdentifier
import Network.AWS.Route53.Types.AliasTarget
import Network.AWS.Route53.Types.Change
import Network.AWS.Route53.Types.ChangeAction
import Network.AWS.Route53.Types.ChangeBatch
import Network.AWS.Route53.Types.ChangeInfo
import Network.AWS.Route53.Types.ChangeStatus
import Network.AWS.Route53.Types.CloudWatchAlarmConfiguration
import Network.AWS.Route53.Types.CloudWatchRegion
import Network.AWS.Route53.Types.ComparisonOperator
import Network.AWS.Route53.Types.DelegationSet
import Network.AWS.Route53.Types.Dimension
import Network.AWS.Route53.Types.Failover
import Network.AWS.Route53.Types.GeoLocation
import Network.AWS.Route53.Types.GeoLocationDetails
import Network.AWS.Route53.Types.HealthCheck
import Network.AWS.Route53.Types.HealthCheckConfig
import Network.AWS.Route53.Types.HealthCheckObservation
import Network.AWS.Route53.Types.HealthCheckRegion
import Network.AWS.Route53.Types.HealthCheckType
import Network.AWS.Route53.Types.HostedZone
import Network.AWS.Route53.Types.HostedZoneConfig
import Network.AWS.Route53.Types.HostedZoneLimit
import Network.AWS.Route53.Types.HostedZoneLimitType
import Network.AWS.Route53.Types.HostedZoneOwner
import Network.AWS.Route53.Types.HostedZoneSummary
import Network.AWS.Route53.Types.InsufficientDataHealthStatus
import Network.AWS.Route53.Types.LinkedService
import Network.AWS.Route53.Types.QueryLoggingConfig
import Network.AWS.Route53.Types.RecordType
import Network.AWS.Route53.Types.ResettableElementName
import Network.AWS.Route53.Types.ResourceRecord
import Network.AWS.Route53.Types.ResourceRecordSet
import Network.AWS.Route53.Types.ResourceTagSet
import Network.AWS.Route53.Types.ReusableDelegationSetLimit
import Network.AWS.Route53.Types.ReusableDelegationSetLimitType
import Network.AWS.Route53.Types.Statistic
import Network.AWS.Route53.Types.StatusReport
import Network.AWS.Route53.Types.Tag
import Network.AWS.Route53.Types.TagResourceType
import Network.AWS.Route53.Types.TrafficPolicy
import Network.AWS.Route53.Types.TrafficPolicyInstance
import Network.AWS.Route53.Types.TrafficPolicySummary
import Network.AWS.Route53.Types.VPC
import Network.AWS.Route53.Types.VPCRegion
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2013-04-01@ of the Amazon Route 53 SDK configuration.
route53Service :: Lude.Service
route53Service =
  Lude.Service
    { Lude._svcAbbrev = "Route53",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "route53",
      Lude._svcVersion = "2013-04-01",
      Lude._svcEndpoint = Lude.defaultEndpoint route53Service,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseXMLError "Route53",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "request_limit_exceeded"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          (Lude.hasCode "PriorRequestNotComplete" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "still_processing"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing

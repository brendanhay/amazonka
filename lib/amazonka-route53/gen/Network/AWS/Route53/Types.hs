{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types
  ( -- * Service Configuration
    route53,

    -- * Errors

    -- * Re-exported Types
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
    AccountLimit,
    accountLimit,
    alType,
    alValue,

    -- * AlarmIdentifier
    AlarmIdentifier,
    alarmIdentifier,
    aiRegion,
    aiName,

    -- * AliasTarget
    AliasTarget,
    aliasTarget,
    atHostedZoneId,
    atDNSName,
    atEvaluateTargetHealth,

    -- * Change
    Change,
    change,
    cAction,
    cResourceRecordSet,

    -- * ChangeBatch
    ChangeBatch,
    changeBatch,
    cbComment,
    cbChanges,

    -- * ChangeInfo
    ChangeInfo,
    changeInfo,
    ciComment,
    ciId,
    ciStatus,
    ciSubmittedAt,

    -- * CloudWatchAlarmConfiguration
    CloudWatchAlarmConfiguration,
    cloudWatchAlarmConfiguration,
    cwacDimensions,
    cwacEvaluationPeriods,
    cwacThreshold,
    cwacComparisonOperator,
    cwacPeriod,
    cwacMetricName,
    cwacNamespace,
    cwacStatistic,

    -- * DelegationSet
    DelegationSet,
    delegationSet,
    dsId,
    dsCallerReference,
    dsNameServers,

    -- * Dimension
    Dimension,
    dimension,
    dName,
    dValue,

    -- * GeoLocation
    GeoLocation,
    geoLocation,
    glSubdivisionCode,
    glCountryCode,
    glContinentCode,

    -- * GeoLocationDetails
    GeoLocationDetails,
    geoLocationDetails,
    gldSubdivisionName,
    gldSubdivisionCode,
    gldCountryName,
    gldCountryCode,
    gldContinentCode,
    gldContinentName,

    -- * HealthCheck
    HealthCheck,
    healthCheck,
    hcLinkedService,
    hcCloudWatchAlarmConfiguration,
    hcId,
    hcCallerReference,
    hcHealthCheckConfig,
    hcHealthCheckVersion,

    -- * HealthCheckConfig
    HealthCheckConfig,
    healthCheckConfig,
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
    HealthCheckObservation,
    healthCheckObservation,
    hcoIPAddress,
    hcoStatusReport,
    hcoRegion,

    -- * HostedZone
    HostedZone,
    hostedZone,
    hzLinkedService,
    hzConfig,
    hzResourceRecordSetCount,
    hzId,
    hzName,
    hzCallerReference,

    -- * HostedZoneConfig
    HostedZoneConfig,
    hostedZoneConfig,
    hzcPrivateZone,
    hzcComment,

    -- * HostedZoneLimit
    HostedZoneLimit,
    hostedZoneLimit,
    hzlType,
    hzlValue,

    -- * HostedZoneOwner
    HostedZoneOwner,
    hostedZoneOwner,
    hzoOwningAccount,
    hzoOwningService,

    -- * HostedZoneSummary
    HostedZoneSummary,
    hostedZoneSummary,
    hzsHostedZoneId,
    hzsName,
    hzsOwner,

    -- * LinkedService
    LinkedService,
    linkedService,
    lsServicePrincipal,
    lsDescription,

    -- * QueryLoggingConfig
    QueryLoggingConfig,
    queryLoggingConfig,
    qlcId,
    qlcHostedZoneId,
    qlcCloudWatchLogsLogGroupARN,

    -- * ResourceRecord
    ResourceRecord,
    resourceRecord,
    rrValue,

    -- * ResourceRecordSet
    ResourceRecordSet,
    resourceRecordSet,
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
    ResourceTagSet,
    resourceTagSet,
    rtsResourceId,
    rtsResourceType,
    rtsTags,

    -- * ReusableDelegationSetLimit
    ReusableDelegationSetLimit,
    reusableDelegationSetLimit,
    rdslType,
    rdslValue,

    -- * StatusReport
    StatusReport,
    statusReport,
    srStatus,
    srCheckedTime,

    -- * Tag
    Tag,
    tag,
    tagValue,
    tagKey,

    -- * TrafficPolicy
    TrafficPolicy,
    trafficPolicy,
    tpComment,
    tpId,
    tpVersion,
    tpName,
    tpType,
    tpDocument,

    -- * TrafficPolicyInstance
    TrafficPolicyInstance,
    trafficPolicyInstance,
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
    TrafficPolicySummary,
    trafficPolicySummary,
    tpsId,
    tpsName,
    tpsType,
    tpsLatestVersion,
    tpsTrafficPolicyCount,

    -- * VPC
    VPC,
    vpc,
    vpcVPCRegion,
    vpcVPCId,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
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
import Network.AWS.Sign.V4

-- | API version @2013-04-01@ of the Amazon Route 53 SDK configuration.
route53 :: Service
route53 =
  Service
    { _svcAbbrev = "Route53",
      _svcSigner = v4,
      _svcPrefix = "route53",
      _svcVersion = "2013-04-01",
      _svcEndpoint = defaultEndpoint route53,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseXMLError "Route53",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasCode "Throttling" . hasStatus 400) e =
        Just "request_limit_exceeded"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "PriorRequestNotComplete" . hasStatus 400) e =
        Just "still_processing"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

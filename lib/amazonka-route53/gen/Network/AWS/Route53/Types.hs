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
    mkServiceConfig,

    -- * Errors
    _HealthCheckVersionMismatch,
    _NoSuchQueryLoggingConfig,
    _HostedZoneNotPrivate,
    _InvalidInput,
    _HostedZoneNotEmpty,
    _InvalidArgument,
    _TrafficPolicyInstanceAlreadyExists,
    _ConflictingTypes,
    _QueryLoggingConfigAlreadyExists,
    _ConcurrentModification,
    _DelegationSetAlreadyReusable,
    _NotAuthorizedException,
    _InsufficientCloudWatchLogsResourcePolicy,
    _NoSuchCloudWatchLogsLogGroup,
    _PriorRequestNotComplete,
    _InvalidChangeBatch,
    _TooManyVPCAssociationAuthorizations,
    _TrafficPolicyAlreadyExists,
    _InvalidTrafficPolicyDocument,
    _InvalidPaginationToken,
    _DelegationSetNotReusable,
    _InvalidDomainName,
    _NoSuchTrafficPolicy,
    _HostedZoneNotFound,
    _DelegationSetInUse,
    _NoSuchDelegationSet,
    _HealthCheckAlreadyExists,
    _TooManyTrafficPolicies,
    _VPCAssociationAuthorizationNotFound,
    _NoSuchGeoLocation,
    _DelegationSetNotAvailable,
    _VPCAssociationNotFound,
    _ThrottlingException,
    _NoSuchChange,
    _LimitsExceeded,
    _TooManyTrafficPolicyInstances,
    _NoSuchTrafficPolicyInstance,
    _IncompatibleVersion,
    _PublicZoneVPCAssociation,
    _NoSuchHostedZone,
    _TooManyHostedZones,
    _HealthCheckInUse,
    _DelegationSetAlreadyCreated,
    _ConflictingDomainExists,
    _LastVPCAssociation,
    _TooManyHealthChecks,
    _NoSuchHealthCheck,
    _TrafficPolicyInUse,
    _InvalidVPCId,
    _HostedZoneAlreadyExists,
    _TooManyTrafficPolicyVersionsForCurrentPolicy,

    -- * Re-exported types
    module Network.AWS.Route53.Internal,

    -- * AlarmName
    AlarmName (..),

    -- * AccountLimit
    AccountLimit (..),
    mkAccountLimit,
    alType,
    alValue,

    -- * ResourceURI
    ResourceURI (..),

    -- * Status
    Status (..),

    -- * LinkedService
    LinkedService (..),
    mkLinkedService,
    lsDescription,
    lsServicePrincipal,

    -- * PaginationToken
    PaginationToken (..),

    -- * ServicePrincipal
    ServicePrincipal (..),

    -- * AliasTarget
    AliasTarget (..),
    mkAliasTarget,
    atHostedZoneId,
    atDNSName,
    atEvaluateTargetHealth,

    -- * IPAddress
    IPAddress (..),

    -- * GeoLocationCountryName
    GeoLocationCountryName (..),

    -- * ResourceRecord
    ResourceRecord (..),
    mkResourceRecord,
    rrValue,

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * GeoLocationDetails
    GeoLocationDetails (..),
    mkGeoLocationDetails,
    gldContinentCode,
    gldContinentName,
    gldCountryCode,
    gldCountryName,
    gldSubdivisionCode,
    gldSubdivisionName,

    -- * ResettableElementName
    ResettableElementName (..),

    -- * DimensionField
    DimensionField (..),

    -- * HostedZoneLimitType
    HostedZoneLimitType (..),

    -- * HealthCheck
    HealthCheck (..),
    mkHealthCheck,
    hcId,
    hcCallerReference,
    hcHealthCheckConfig,
    hcHealthCheckVersion,
    hcCloudWatchAlarmConfiguration,
    hcLinkedService,

    -- * Nameserver
    Nameserver (..),

    -- * TagResourceId
    TagResourceId (..),

    -- * TrafficPolicy
    TrafficPolicy (..),
    mkTrafficPolicy,
    tpId,
    tpVersion,
    tpName,
    tpType,
    tpDocument,
    tpComment,

    -- * VPCRegion
    VPCRegion (..),

    -- * TransportProtocol
    TransportProtocol (..),

    -- * GeoLocationCountryCode
    GeoLocationCountryCode (..),

    -- * ChangeAction
    ChangeAction (..),

    -- * TagResourceType
    TagResourceType (..),

    -- * HealthCheckConfig
    HealthCheckConfig (..),
    mkHealthCheckConfig,
    hccType,
    hccAlarmIdentifier,
    hccChildHealthChecks,
    hccDisabled,
    hccEnableSNI,
    hccFailureThreshold,
    hccFullyQualifiedDomainName,
    hccHealthThreshold,
    hccIPAddress,
    hccInsufficientDataHealthStatus,
    hccInverted,
    hccMeasureLatency,
    hccPort,
    hccRegions,
    hccRequestInterval,
    hccResourcePath,
    hccSearchString,

    -- * CloudWatchRegion
    CloudWatchRegion (..),

    -- * Dimension
    Dimension (..),
    mkDimension,
    dName,
    dValue,

    -- * VPCId
    VPCId (..),

    -- * MetricName
    MetricName (..),

    -- * Namespace
    Namespace (..),

    -- * SearchString
    SearchString (..),

    -- * CloudWatchAlarmConfiguration
    CloudWatchAlarmConfiguration (..),
    mkCloudWatchAlarmConfiguration,
    cwacEvaluationPeriods,
    cwacThreshold,
    cwacComparisonOperator,
    cwacPeriod,
    cwacMetricName,
    cwacNamespace,
    cwacStatistic,
    cwacDimensions,

    -- * HostedZoneSummary
    HostedZoneSummary (..),
    mkHostedZoneSummary,
    hzsHostedZoneId,
    hzsName,
    hzsOwner,

    -- * QueryLoggingConfig
    QueryLoggingConfig (..),
    mkQueryLoggingConfig,
    qlcId,
    qlcHostedZoneId,
    qlcCloudWatchLogsLogGroupArn,

    -- * GeoLocationContinentCode
    GeoLocationContinentCode (..),

    -- * ResourceRecordSetIdentifier
    ResourceRecordSetIdentifier (..),

    -- * QueryLoggingConfigId
    QueryLoggingConfigId (..),

    -- * ReusableDelegationSetLimitType
    ReusableDelegationSetLimitType (..),

    -- * Change
    Change (..),
    mkChange,
    cAction,
    cResourceRecordSet,

    -- * CloudWatchLogsLogGroupArn
    CloudWatchLogsLogGroupArn (..),

    -- * ComparisonOperator
    ComparisonOperator (..),

    -- * TrafficPolicyInstanceId
    TrafficPolicyInstanceId (..),

    -- * Failover
    Failover (..),

    -- * AWSAccountID
    AWSAccountID (..),

    -- * AccountLimitType
    AccountLimitType (..),

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

    -- * ResourceDescription
    ResourceDescription (..),

    -- * HostedZoneOwningService
    HostedZoneOwningService (..),

    -- * SubnetMask
    SubnetMask (..),

    -- * HostedZone
    HostedZone (..),
    mkHostedZone,
    hzId,
    hzName,
    hzCallerReference,
    hzConfig,
    hzLinkedService,
    hzResourceRecordSetCount,

    -- * ResourcePath
    ResourcePath (..),

    -- * HealthCheckId
    HealthCheckId (..),

    -- * ResourceTagSet
    ResourceTagSet (..),
    mkResourceTagSet,
    rtsResourceId,
    rtsResourceType,
    rtsTags,

    -- * ChangeStatus
    ChangeStatus (..),

    -- * HealthCheckRegion
    HealthCheckRegion (..),

    -- * TrafficPolicyDocument
    TrafficPolicyDocument (..),

    -- * AssociateVPCComment
    AssociateVPCComment (..),

    -- * ChangeBatch
    ChangeBatch (..),
    mkChangeBatch,
    cbChanges,
    cbComment,

    -- * HostedZoneLimit
    HostedZoneLimit (..),
    mkHostedZoneLimit,
    hzlType,
    hzlValue,

    -- * TrafficPolicyName
    TrafficPolicyName (..),

    -- * StatusReport
    StatusReport (..),
    mkStatusReport,
    srCheckedTime,
    srStatus,

    -- * HealthCheckType
    HealthCheckType (..),

    -- * VPC
    VPC (..),
    mkVPC,
    vpcVPCId,
    vpcVPCRegion,

    -- * HostedZoneConfig
    HostedZoneConfig (..),
    mkHostedZoneConfig,
    hzcComment,
    hzcPrivateZone,

    -- * InsufficientDataHealthStatus
    InsufficientDataHealthStatus (..),

    -- * TrafficPolicyVersionMarker
    TrafficPolicyVersionMarker (..),

    -- * ResourceRecordSet
    ResourceRecordSet (..),
    mkResourceRecordSet,
    rrsName,
    rrsType,
    rrsAliasTarget,
    rrsFailover,
    rrsGeoLocation,
    rrsHealthCheckId,
    rrsMultiValueAnswer,
    rrsRegion,
    rrsResourceRecords,
    rrsSetIdentifier,
    rrsTTL,
    rrsTrafficPolicyInstanceId,
    rrsWeight,

    -- * TagKey
    TagKey (..),

    -- * DelegationSet
    DelegationSet (..),
    mkDelegationSet,
    dsNameServers,
    dsCallerReference,
    dsId,

    -- * ChangeInfo
    ChangeInfo (..),
    mkChangeInfo,
    ciId,
    ciStatus,
    ciSubmittedAt,
    ciComment,

    -- * GeoLocationSubdivisionCode
    GeoLocationSubdivisionCode (..),

    -- * RecordType
    RecordType (..),

    -- * AlarmIdentifier
    AlarmIdentifier (..),
    mkAlarmIdentifier,
    aiRegion,
    aiName,

    -- * GeoLocation
    GeoLocation (..),
    mkGeoLocation,
    glContinentCode,
    glCountryCode,
    glSubdivisionCode,

    -- * TrafficPolicyId
    TrafficPolicyId (..),

    -- * HealthCheckObservation
    HealthCheckObservation (..),
    mkHealthCheckObservation,
    hcoIPAddress,
    hcoRegion,
    hcoStatusReport,

    -- * Message
    Message (..),

    -- * FullyQualifiedDomainName
    FullyQualifiedDomainName (..),

    -- * IPAddressCidr
    IPAddressCidr (..),

    -- * ReusableDelegationSetLimit
    ReusableDelegationSetLimit (..),
    mkReusableDelegationSetLimit,
    rdslType,
    rdslValue,

    -- * DNSName
    DNSName (..),

    -- * HostedZoneOwner
    HostedZoneOwner (..),
    mkHostedZoneOwner,
    hzoOwningAccount,
    hzoOwningService,

    -- * MaxResults
    MaxResults (..),

    -- * Statistic
    Statistic (..),

    -- * RecordDataEntry
    RecordDataEntry (..),

    -- * TrafficPolicySummary
    TrafficPolicySummary (..),
    mkTrafficPolicySummary,
    tpsId,
    tpsName,
    tpsType,
    tpsLatestVersion,
    tpsTrafficPolicyCount,

    -- * HostedZoneIdMarker
    HostedZoneIdMarker (..),

    -- * MaxItems
    MaxItems (..),

    -- * TrafficPolicyInstanceNameMarker
    TrafficPolicyInstanceNameMarker (..),

    -- * Description
    Description (..),

    -- * DelegationSetId
    DelegationSetId (..),

    -- * Marker
    Marker (..),

    -- * CallerReference
    CallerReference (..),

    -- * HostedZoneId
    HostedZoneId (..),

    -- * Comment
    Comment (..),

    -- * Value
    Value (..),

    -- * Key
    Key (..),

    -- * NextContinentCode
    NextContinentCode (..),

    -- * NextCountryCode
    NextCountryCode (..),

    -- * NextSubdivisionCode
    NextSubdivisionCode (..),

    -- * ContinentCode
    ContinentCode (..),

    -- * ContinentName
    ContinentName (..),

    -- * CountryCode
    CountryCode (..),

    -- * SubdivisionCode
    SubdivisionCode (..),

    -- * SubdivisionName
    SubdivisionName (..),

    -- * Name
    Name (..),

    -- * Id
    Id (..),

    -- * Document
    Document (..),

    -- * StartRecordIdentifier
    StartRecordIdentifier (..),

    -- * StartRecordName
    StartRecordName (..),

    -- * StartContinentCode
    StartContinentCode (..),

    -- * StartSubdivisionCode
    StartSubdivisionCode (..),

    -- * NextMarker
    NextMarker (..),

    -- * NextDNSName
    NextDNSName (..),

    -- * State
    State (..),

    -- * NextRecordName
    NextRecordName (..),

    -- * RecordName
    RecordName (..),

    -- * TrafficPolicyIdMarker
    TrafficPolicyIdMarker (..),

    -- * ResponseCode
    ResponseCode (..),
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.AWSAccountID
import Network.AWS.Route53.Types.AccountLimit
import Network.AWS.Route53.Types.AccountLimitType
import Network.AWS.Route53.Types.AlarmIdentifier
import Network.AWS.Route53.Types.AlarmName
import Network.AWS.Route53.Types.AliasTarget
import Network.AWS.Route53.Types.AssociateVPCComment
import Network.AWS.Route53.Types.CallerReference
import Network.AWS.Route53.Types.Change
import Network.AWS.Route53.Types.ChangeAction
import Network.AWS.Route53.Types.ChangeBatch
import Network.AWS.Route53.Types.ChangeInfo
import Network.AWS.Route53.Types.ChangeStatus
import Network.AWS.Route53.Types.CloudWatchAlarmConfiguration
import Network.AWS.Route53.Types.CloudWatchLogsLogGroupArn
import Network.AWS.Route53.Types.CloudWatchRegion
import Network.AWS.Route53.Types.Comment
import Network.AWS.Route53.Types.ComparisonOperator
import Network.AWS.Route53.Types.ContinentCode
import Network.AWS.Route53.Types.ContinentName
import Network.AWS.Route53.Types.CountryCode
import Network.AWS.Route53.Types.DNSName
import Network.AWS.Route53.Types.DelegationSet
import Network.AWS.Route53.Types.DelegationSetId
import Network.AWS.Route53.Types.Description
import Network.AWS.Route53.Types.Dimension
import Network.AWS.Route53.Types.DimensionField
import Network.AWS.Route53.Types.Document
import Network.AWS.Route53.Types.Failover
import Network.AWS.Route53.Types.FullyQualifiedDomainName
import Network.AWS.Route53.Types.GeoLocation
import Network.AWS.Route53.Types.GeoLocationContinentCode
import Network.AWS.Route53.Types.GeoLocationCountryCode
import Network.AWS.Route53.Types.GeoLocationCountryName
import Network.AWS.Route53.Types.GeoLocationDetails
import Network.AWS.Route53.Types.GeoLocationSubdivisionCode
import Network.AWS.Route53.Types.HealthCheck
import Network.AWS.Route53.Types.HealthCheckConfig
import Network.AWS.Route53.Types.HealthCheckId
import Network.AWS.Route53.Types.HealthCheckObservation
import Network.AWS.Route53.Types.HealthCheckRegion
import Network.AWS.Route53.Types.HealthCheckType
import Network.AWS.Route53.Types.HostedZone
import Network.AWS.Route53.Types.HostedZoneConfig
import Network.AWS.Route53.Types.HostedZoneId
import Network.AWS.Route53.Types.HostedZoneIdMarker
import Network.AWS.Route53.Types.HostedZoneLimit
import Network.AWS.Route53.Types.HostedZoneLimitType
import Network.AWS.Route53.Types.HostedZoneOwner
import Network.AWS.Route53.Types.HostedZoneOwningService
import Network.AWS.Route53.Types.HostedZoneSummary
import Network.AWS.Route53.Types.IPAddress
import Network.AWS.Route53.Types.IPAddressCidr
import Network.AWS.Route53.Types.Id
import Network.AWS.Route53.Types.InsufficientDataHealthStatus
import Network.AWS.Route53.Types.Key
import Network.AWS.Route53.Types.LinkedService
import Network.AWS.Route53.Types.Marker
import Network.AWS.Route53.Types.MaxItems
import Network.AWS.Route53.Types.MaxResults
import Network.AWS.Route53.Types.Message
import Network.AWS.Route53.Types.MetricName
import Network.AWS.Route53.Types.Name
import Network.AWS.Route53.Types.Nameserver
import Network.AWS.Route53.Types.Namespace
import Network.AWS.Route53.Types.NextContinentCode
import Network.AWS.Route53.Types.NextCountryCode
import Network.AWS.Route53.Types.NextDNSName
import Network.AWS.Route53.Types.NextMarker
import Network.AWS.Route53.Types.NextRecordName
import Network.AWS.Route53.Types.NextSubdivisionCode
import Network.AWS.Route53.Types.PaginationToken
import Network.AWS.Route53.Types.QueryLoggingConfig
import Network.AWS.Route53.Types.QueryLoggingConfigId
import Network.AWS.Route53.Types.RecordDataEntry
import Network.AWS.Route53.Types.RecordName
import Network.AWS.Route53.Types.RecordType
import Network.AWS.Route53.Types.ResettableElementName
import Network.AWS.Route53.Types.ResourceDescription
import Network.AWS.Route53.Types.ResourcePath
import Network.AWS.Route53.Types.ResourceRecord
import Network.AWS.Route53.Types.ResourceRecordSet
import Network.AWS.Route53.Types.ResourceRecordSetIdentifier
import Network.AWS.Route53.Types.ResourceTagSet
import Network.AWS.Route53.Types.ResourceURI
import Network.AWS.Route53.Types.ResponseCode
import Network.AWS.Route53.Types.ReusableDelegationSetLimit
import Network.AWS.Route53.Types.ReusableDelegationSetLimitType
import Network.AWS.Route53.Types.SearchString
import Network.AWS.Route53.Types.ServicePrincipal
import Network.AWS.Route53.Types.StartContinentCode
import Network.AWS.Route53.Types.StartRecordIdentifier
import Network.AWS.Route53.Types.StartRecordName
import Network.AWS.Route53.Types.StartSubdivisionCode
import Network.AWS.Route53.Types.State
import Network.AWS.Route53.Types.Statistic
import Network.AWS.Route53.Types.Status
import Network.AWS.Route53.Types.StatusReport
import Network.AWS.Route53.Types.SubdivisionCode
import Network.AWS.Route53.Types.SubdivisionName
import Network.AWS.Route53.Types.SubnetMask
import Network.AWS.Route53.Types.Tag
import Network.AWS.Route53.Types.TagKey
import Network.AWS.Route53.Types.TagResourceId
import Network.AWS.Route53.Types.TagResourceType
import Network.AWS.Route53.Types.TrafficPolicy
import Network.AWS.Route53.Types.TrafficPolicyDocument
import Network.AWS.Route53.Types.TrafficPolicyId
import Network.AWS.Route53.Types.TrafficPolicyIdMarker
import Network.AWS.Route53.Types.TrafficPolicyInstance
import Network.AWS.Route53.Types.TrafficPolicyInstanceId
import Network.AWS.Route53.Types.TrafficPolicyInstanceNameMarker
import Network.AWS.Route53.Types.TrafficPolicyName
import Network.AWS.Route53.Types.TrafficPolicySummary
import Network.AWS.Route53.Types.TrafficPolicyVersionMarker
import Network.AWS.Route53.Types.TransportProtocol
import Network.AWS.Route53.Types.VPC
import Network.AWS.Route53.Types.VPCId
import Network.AWS.Route53.Types.VPCRegion
import Network.AWS.Route53.Types.Value
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2013-04-01@ of the Amazon Route 53 SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "Route53",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "route53",
      Core._svcVersion = "2013-04-01",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseXMLError "Route53",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "request_limit_exceeded"
      | Lens.has
          (Core.hasCode "PriorRequestNotComplete" Core.. Core.hasStatus 400)
          e =
        Core.Just "still_processing"
      | Core.otherwise = Core.Nothing

-- | The value of @HealthCheckVersion@ in the request doesn't match the value of @HealthCheckVersion@ in the health check.
_HealthCheckVersionMismatch :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_HealthCheckVersionMismatch =
  Core._MatchServiceError
    mkServiceConfig
    "HealthCheckVersionMismatch"
    Core.. Core.hasStatues 409
{-# DEPRECATED _HealthCheckVersionMismatch "Use generic-lens or generic-optics instead." #-}

-- | There is no DNS query logging configuration with the specified ID.
_NoSuchQueryLoggingConfig :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchQueryLoggingConfig =
  Core._MatchServiceError
    mkServiceConfig
    "NoSuchQueryLoggingConfig"
    Core.. Core.hasStatues 404
{-# DEPRECATED _NoSuchQueryLoggingConfig "Use generic-lens or generic-optics instead." #-}

-- | The specified hosted zone is a public hosted zone, not a private hosted zone.
_HostedZoneNotPrivate :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_HostedZoneNotPrivate =
  Core._MatchServiceError mkServiceConfig "HostedZoneNotPrivate"
{-# DEPRECATED _HostedZoneNotPrivate "Use generic-lens or generic-optics instead." #-}

-- | The input is not valid.
_InvalidInput :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidInput =
  Core._MatchServiceError mkServiceConfig "InvalidInput"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidInput "Use generic-lens or generic-optics instead." #-}

-- | The hosted zone contains resource records that are not SOA or NS records.
_HostedZoneNotEmpty :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_HostedZoneNotEmpty =
  Core._MatchServiceError mkServiceConfig "HostedZoneNotEmpty"
    Core.. Core.hasStatues 400
{-# DEPRECATED _HostedZoneNotEmpty "Use generic-lens or generic-optics instead." #-}

-- | Parameter name is invalid.
_InvalidArgument :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidArgument =
  Core._MatchServiceError mkServiceConfig "InvalidArgument"
{-# DEPRECATED _InvalidArgument "Use generic-lens or generic-optics instead." #-}

-- | There is already a traffic policy instance with the specified ID.
_TrafficPolicyInstanceAlreadyExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TrafficPolicyInstanceAlreadyExists =
  Core._MatchServiceError
    mkServiceConfig
    "TrafficPolicyInstanceAlreadyExists"
    Core.. Core.hasStatues 409
{-# DEPRECATED _TrafficPolicyInstanceAlreadyExists "Use generic-lens or generic-optics instead." #-}

-- | You tried to update a traffic policy instance by using a traffic policy version that has a different DNS type than the current type for the instance. You specified the type in the JSON document in the @CreateTrafficPolicy@ or @CreateTrafficPolicyVersion@ request.
_ConflictingTypes :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConflictingTypes =
  Core._MatchServiceError mkServiceConfig "ConflictingTypes"
    Core.. Core.hasStatues 400
{-# DEPRECATED _ConflictingTypes "Use generic-lens or generic-optics instead." #-}

-- | You can create only one query logging configuration for a hosted zone, and a query logging configuration already exists for this hosted zone.
_QueryLoggingConfigAlreadyExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_QueryLoggingConfigAlreadyExists =
  Core._MatchServiceError
    mkServiceConfig
    "QueryLoggingConfigAlreadyExists"
    Core.. Core.hasStatues 409
{-# DEPRECATED _QueryLoggingConfigAlreadyExists "Use generic-lens or generic-optics instead." #-}

-- | Another user submitted a request to create, update, or delete the object at the same time that you did. Retry the request.
_ConcurrentModification :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConcurrentModification =
  Core._MatchServiceError mkServiceConfig "ConcurrentModification"
    Core.. Core.hasStatues 400
{-# DEPRECATED _ConcurrentModification "Use generic-lens or generic-optics instead." #-}

-- | The specified delegation set has already been marked as reusable.
_DelegationSetAlreadyReusable :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DelegationSetAlreadyReusable =
  Core._MatchServiceError
    mkServiceConfig
    "DelegationSetAlreadyReusable"
{-# DEPRECATED _DelegationSetAlreadyReusable "Use generic-lens or generic-optics instead." #-}

-- | Associating the specified VPC with the specified hosted zone has not been authorized.
_NotAuthorizedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotAuthorizedException =
  Core._MatchServiceError mkServiceConfig "NotAuthorizedException"
    Core.. Core.hasStatues 401
{-# DEPRECATED _NotAuthorizedException "Use generic-lens or generic-optics instead." #-}

-- | Amazon Route 53 doesn't have the permissions required to create log streams and send query logs to log streams. Possible causes include the following:
--
--
--     * There is no resource policy that specifies the log group ARN in the value for @Resource@ .
--
--
--     * The resource policy that includes the log group ARN in the value for @Resource@ doesn't have the necessary permissions.
--
--
--     * The resource policy hasn't finished propagating yet.
_InsufficientCloudWatchLogsResourcePolicy :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InsufficientCloudWatchLogsResourcePolicy =
  Core._MatchServiceError
    mkServiceConfig
    "InsufficientCloudWatchLogsResourcePolicy"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InsufficientCloudWatchLogsResourcePolicy "Use generic-lens or generic-optics instead." #-}

-- | There is no CloudWatch Logs log group with the specified ARN.
_NoSuchCloudWatchLogsLogGroup :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchCloudWatchLogsLogGroup =
  Core._MatchServiceError
    mkServiceConfig
    "NoSuchCloudWatchLogsLogGroup"
    Core.. Core.hasStatues 404
{-# DEPRECATED _NoSuchCloudWatchLogsLogGroup "Use generic-lens or generic-optics instead." #-}

-- | If Amazon Route 53 can't process a request before the next request arrives, it will reject subsequent requests for the same hosted zone and return an @HTTP 400 error@ (@Bad request@ ). If Route 53 returns this error repeatedly for the same request, we recommend that you wait, in intervals of increasing duration, before you try the request again.
_PriorRequestNotComplete :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PriorRequestNotComplete =
  Core._MatchServiceError mkServiceConfig "PriorRequestNotComplete"
    Core.. Core.hasStatues 400
{-# DEPRECATED _PriorRequestNotComplete "Use generic-lens or generic-optics instead." #-}

-- | This exception contains a list of messages that might contain one or more error messages. Each error message indicates one error in the change batch.
_InvalidChangeBatch :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidChangeBatch =
  Core._MatchServiceError mkServiceConfig "InvalidChangeBatch"
{-# DEPRECATED _InvalidChangeBatch "Use generic-lens or generic-optics instead." #-}

-- | You've created the maximum number of authorizations that can be created for the specified hosted zone. To authorize another VPC to be associated with the hosted zone, submit a @DeleteVPCAssociationAuthorization@ request to remove an existing authorization. To get a list of existing authorizations, submit a @ListVPCAssociationAuthorizations@ request.
_TooManyVPCAssociationAuthorizations :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyVPCAssociationAuthorizations =
  Core._MatchServiceError
    mkServiceConfig
    "TooManyVPCAssociationAuthorizations"
    Core.. Core.hasStatues 400
{-# DEPRECATED _TooManyVPCAssociationAuthorizations "Use generic-lens or generic-optics instead." #-}

-- | A traffic policy that has the same value for @Name@ already exists.
_TrafficPolicyAlreadyExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TrafficPolicyAlreadyExists =
  Core._MatchServiceError
    mkServiceConfig
    "TrafficPolicyAlreadyExists"
    Core.. Core.hasStatues 409
{-# DEPRECATED _TrafficPolicyAlreadyExists "Use generic-lens or generic-optics instead." #-}

-- | The format of the traffic policy document that you specified in the @Document@ element is invalid.
_InvalidTrafficPolicyDocument :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTrafficPolicyDocument =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidTrafficPolicyDocument"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidTrafficPolicyDocument "Use generic-lens or generic-optics instead." #-}

-- | The value that you specified to get the second or subsequent page of results is invalid.
_InvalidPaginationToken :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidPaginationToken =
  Core._MatchServiceError mkServiceConfig "InvalidPaginationToken"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidPaginationToken "Use generic-lens or generic-optics instead." #-}

-- | A reusable delegation set with the specified ID does not exist.
_DelegationSetNotReusable :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DelegationSetNotReusable =
  Core._MatchServiceError
    mkServiceConfig
    "DelegationSetNotReusable"
{-# DEPRECATED _DelegationSetNotReusable "Use generic-lens or generic-optics instead." #-}

-- | The specified domain name is not valid.
_InvalidDomainName :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDomainName =
  Core._MatchServiceError mkServiceConfig "InvalidDomainName"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidDomainName "Use generic-lens or generic-optics instead." #-}

-- | No traffic policy exists with the specified ID.
_NoSuchTrafficPolicy :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchTrafficPolicy =
  Core._MatchServiceError mkServiceConfig "NoSuchTrafficPolicy"
    Core.. Core.hasStatues 404
{-# DEPRECATED _NoSuchTrafficPolicy "Use generic-lens or generic-optics instead." #-}

-- | The specified HostedZone can't be found.
_HostedZoneNotFound :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_HostedZoneNotFound =
  Core._MatchServiceError mkServiceConfig "HostedZoneNotFound"
{-# DEPRECATED _HostedZoneNotFound "Use generic-lens or generic-optics instead." #-}

-- | The specified delegation contains associated hosted zones which must be deleted before the reusable delegation set can be deleted.
_DelegationSetInUse :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DelegationSetInUse =
  Core._MatchServiceError mkServiceConfig "DelegationSetInUse"
{-# DEPRECATED _DelegationSetInUse "Use generic-lens or generic-optics instead." #-}

-- | A reusable delegation set with the specified ID does not exist.
_NoSuchDelegationSet :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchDelegationSet =
  Core._MatchServiceError mkServiceConfig "NoSuchDelegationSet"
{-# DEPRECATED _NoSuchDelegationSet "Use generic-lens or generic-optics instead." #-}

-- | The health check you're attempting to create already exists. Amazon Route 53 returns this error when you submit a request that has the following values:
--
--
--     * The same value for @CallerReference@ as an existing health check, and one or more values that differ from the existing health check that has the same caller reference.
--
--
--     * The same value for @CallerReference@ as a health check that you created and later deleted, regardless of the other settings in the request.
_HealthCheckAlreadyExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_HealthCheckAlreadyExists =
  Core._MatchServiceError
    mkServiceConfig
    "HealthCheckAlreadyExists"
    Core.. Core.hasStatues 409
{-# DEPRECATED _HealthCheckAlreadyExists "Use generic-lens or generic-optics instead." #-}

-- | This traffic policy can't be created because the current account has reached the limit on the number of traffic policies.
--
-- For information about default limits, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html Limits> in the /Amazon Route 53 Developer Guide/ .
-- To get the current limit for an account, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetAccountLimit.html GetAccountLimit> .
-- To request a higher limit, <http://aws.amazon.com/route53-request create a case> with the AWS Support Center.
_TooManyTrafficPolicies :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyTrafficPolicies =
  Core._MatchServiceError mkServiceConfig "TooManyTrafficPolicies"
    Core.. Core.hasStatues 400
{-# DEPRECATED _TooManyTrafficPolicies "Use generic-lens or generic-optics instead." #-}

-- | The VPC that you specified is not authorized to be associated with the hosted zone.
_VPCAssociationAuthorizationNotFound :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_VPCAssociationAuthorizationNotFound =
  Core._MatchServiceError
    mkServiceConfig
    "VPCAssociationAuthorizationNotFound"
    Core.. Core.hasStatues 404
{-# DEPRECATED _VPCAssociationAuthorizationNotFound "Use generic-lens or generic-optics instead." #-}

-- | Amazon Route 53 doesn't support the specified geographic location. For a list of supported geolocation codes, see the <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GeoLocation.html GeoLocation> data type.
_NoSuchGeoLocation :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchGeoLocation =
  Core._MatchServiceError mkServiceConfig "NoSuchGeoLocation"
    Core.. Core.hasStatues 404
{-# DEPRECATED _NoSuchGeoLocation "Use generic-lens or generic-optics instead." #-}

-- | You can create a hosted zone that has the same name as an existing hosted zone (example.com is common), but there is a limit to the number of hosted zones that have the same name. If you get this error, Amazon Route 53 has reached that limit. If you own the domain name and Route 53 generates this error, contact Customer Support.
_DelegationSetNotAvailable :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DelegationSetNotAvailable =
  Core._MatchServiceError
    mkServiceConfig
    "DelegationSetNotAvailable"
{-# DEPRECATED _DelegationSetNotAvailable "Use generic-lens or generic-optics instead." #-}

-- | The specified VPC and hosted zone are not currently associated.
_VPCAssociationNotFound :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_VPCAssociationNotFound =
  Core._MatchServiceError mkServiceConfig "VPCAssociationNotFound"
    Core.. Core.hasStatues 404
{-# DEPRECATED _VPCAssociationNotFound "Use generic-lens or generic-optics instead." #-}

-- | The limit on the number of requests per second was exceeded.
_ThrottlingException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError mkServiceConfig "ThrottlingException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _ThrottlingException "Use generic-lens or generic-optics instead." #-}

-- | A change with the specified change ID does not exist.
_NoSuchChange :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchChange =
  Core._MatchServiceError mkServiceConfig "NoSuchChange"
    Core.. Core.hasStatues 404
{-# DEPRECATED _NoSuchChange "Use generic-lens or generic-optics instead." #-}

-- | This operation can't be completed either because the current account has reached the limit on reusable delegation sets that it can create or because you've reached the limit on the number of Amazon VPCs that you can associate with a private hosted zone. To get the current limit on the number of reusable delegation sets, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetAccountLimit.html GetAccountLimit> . To get the current limit on the number of Amazon VPCs that you can associate with a private hosted zone, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetHostedZoneLimit.html GetHostedZoneLimit> . To request a higher limit, <http://aws.amazon.com/route53-request create a case> with the AWS Support Center.
_LimitsExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitsExceeded =
  Core._MatchServiceError mkServiceConfig "LimitsExceeded"
{-# DEPRECATED _LimitsExceeded "Use generic-lens or generic-optics instead." #-}

-- | This traffic policy instance can't be created because the current account has reached the limit on the number of traffic policy instances.
--
-- For information about default limits, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html Limits> in the /Amazon Route 53 Developer Guide/ .
-- For information about how to get the current limit for an account, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetAccountLimit.html GetAccountLimit> .
-- To request a higher limit, <http://aws.amazon.com/route53-request create a case> with the AWS Support Center.
_TooManyTrafficPolicyInstances :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyTrafficPolicyInstances =
  Core._MatchServiceError
    mkServiceConfig
    "TooManyTrafficPolicyInstances"
    Core.. Core.hasStatues 400
{-# DEPRECATED _TooManyTrafficPolicyInstances "Use generic-lens or generic-optics instead." #-}

-- | No traffic policy instance exists with the specified ID.
_NoSuchTrafficPolicyInstance :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchTrafficPolicyInstance =
  Core._MatchServiceError
    mkServiceConfig
    "NoSuchTrafficPolicyInstance"
    Core.. Core.hasStatues 404
{-# DEPRECATED _NoSuchTrafficPolicyInstance "Use generic-lens or generic-optics instead." #-}

-- | The resource you're trying to access is unsupported on this Amazon Route 53 endpoint.
_IncompatibleVersion :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IncompatibleVersion =
  Core._MatchServiceError mkServiceConfig "IncompatibleVersion"
    Core.. Core.hasStatues 400
{-# DEPRECATED _IncompatibleVersion "Use generic-lens or generic-optics instead." #-}

-- | You're trying to associate a VPC with a public hosted zone. Amazon Route 53 doesn't support associating a VPC with a public hosted zone.
_PublicZoneVPCAssociation :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PublicZoneVPCAssociation =
  Core._MatchServiceError
    mkServiceConfig
    "PublicZoneVPCAssociation"
    Core.. Core.hasStatues 400
{-# DEPRECATED _PublicZoneVPCAssociation "Use generic-lens or generic-optics instead." #-}

-- | No hosted zone exists with the ID that you specified.
_NoSuchHostedZone :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchHostedZone =
  Core._MatchServiceError mkServiceConfig "NoSuchHostedZone"
    Core.. Core.hasStatues 404
{-# DEPRECATED _NoSuchHostedZone "Use generic-lens or generic-optics instead." #-}

-- | This operation can't be completed either because the current account has reached the limit on the number of hosted zones or because you've reached the limit on the number of hosted zones that can be associated with a reusable delegation set.
--
-- For information about default limits, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html Limits> in the /Amazon Route 53 Developer Guide/ .
-- To get the current limit on hosted zones that can be created by an account, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetAccountLimit.html GetAccountLimit> .
-- To get the current limit on hosted zones that can be associated with a reusable delegation set, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetReusableDelegationSetLimit.html GetReusableDelegationSetLimit> .
-- To request a higher limit, <http://aws.amazon.com/route53-request create a case> with the AWS Support Center.
_TooManyHostedZones :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyHostedZones =
  Core._MatchServiceError mkServiceConfig "TooManyHostedZones"
    Core.. Core.hasStatues 400
{-# DEPRECATED _TooManyHostedZones "Use generic-lens or generic-optics instead." #-}

-- | This error code is not in use.
_HealthCheckInUse :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_HealthCheckInUse =
  Core._MatchServiceError mkServiceConfig "HealthCheckInUse"
    Core.. Core.hasStatues 400
{-# DEPRECATED _HealthCheckInUse "Use generic-lens or generic-optics instead." #-}

-- | A delegation set with the same owner and caller reference combination has already been created.
_DelegationSetAlreadyCreated :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DelegationSetAlreadyCreated =
  Core._MatchServiceError
    mkServiceConfig
    "DelegationSetAlreadyCreated"
{-# DEPRECATED _DelegationSetAlreadyCreated "Use generic-lens or generic-optics instead." #-}

-- | The cause of this error depends on the operation that you're performing:
--
--
--     * __Create a public hosted zone:__ Two hosted zones that have the same name or that have a parent/child relationship (example.com and test.example.com) can't have any common name servers. You tried to create a hosted zone that has the same name as an existing hosted zone or that's the parent or child of an existing hosted zone, and you specified a delegation set that shares one or more name servers with the existing hosted zone. For more information, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateReusableDelegationSet.html CreateReusableDelegationSet> .
--
--
--     * __Create a private hosted zone:__ A hosted zone with the specified name already exists and is already associated with the Amazon VPC that you specified.
--
--
--     * __Associate VPCs with a private hosted zone:__ The VPC that you specified is already associated with another hosted zone that has the same name.
_ConflictingDomainExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConflictingDomainExists =
  Core._MatchServiceError mkServiceConfig "ConflictingDomainExists"
{-# DEPRECATED _ConflictingDomainExists "Use generic-lens or generic-optics instead." #-}

-- | The VPC that you're trying to disassociate from the private hosted zone is the last VPC that is associated with the hosted zone. Amazon Route 53 doesn't support disassociating the last VPC from a hosted zone.
_LastVPCAssociation :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LastVPCAssociation =
  Core._MatchServiceError mkServiceConfig "LastVPCAssociation"
    Core.. Core.hasStatues 400
{-# DEPRECATED _LastVPCAssociation "Use generic-lens or generic-optics instead." #-}

-- | This health check can't be created because the current account has reached the limit on the number of active health checks.
--
-- For information about default limits, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html Limits> in the /Amazon Route 53 Developer Guide/ .
-- For information about how to get the current limit for an account, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetAccountLimit.html GetAccountLimit> . To request a higher limit, <http://aws.amazon.com/route53-request create a case> with the AWS Support Center.
-- You have reached the maximum number of active health checks for an AWS account. To request a higher limit, <http://aws.amazon.com/route53-request create a case> with the AWS Support Center.
_TooManyHealthChecks :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyHealthChecks =
  Core._MatchServiceError mkServiceConfig "TooManyHealthChecks"
{-# DEPRECATED _TooManyHealthChecks "Use generic-lens or generic-optics instead." #-}

-- | No health check exists with the specified ID.
_NoSuchHealthCheck :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchHealthCheck =
  Core._MatchServiceError mkServiceConfig "NoSuchHealthCheck"
    Core.. Core.hasStatues 404
{-# DEPRECATED _NoSuchHealthCheck "Use generic-lens or generic-optics instead." #-}

-- | One or more traffic policy instances were created by using the specified traffic policy.
_TrafficPolicyInUse :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TrafficPolicyInUse =
  Core._MatchServiceError mkServiceConfig "TrafficPolicyInUse"
    Core.. Core.hasStatues 400
{-# DEPRECATED _TrafficPolicyInUse "Use generic-lens or generic-optics instead." #-}

-- | The VPC ID that you specified either isn't a valid ID or the current account is not authorized to access this VPC.
_InvalidVPCId :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidVPCId =
  Core._MatchServiceError mkServiceConfig "InvalidVPCId"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidVPCId "Use generic-lens or generic-optics instead." #-}

-- | The hosted zone you're trying to create already exists. Amazon Route 53 returns this error when a hosted zone has already been created with the specified @CallerReference@ .
_HostedZoneAlreadyExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_HostedZoneAlreadyExists =
  Core._MatchServiceError mkServiceConfig "HostedZoneAlreadyExists"
    Core.. Core.hasStatues 409
{-# DEPRECATED _HostedZoneAlreadyExists "Use generic-lens or generic-optics instead." #-}

-- | This traffic policy version can't be created because you've reached the limit of 1000 on the number of versions that you can create for the current traffic policy.
--
-- To create more traffic policy versions, you can use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetTrafficPolicy.html GetTrafficPolicy> to get the traffic policy document for a specified traffic policy version, and then use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateTrafficPolicy.html CreateTrafficPolicy> to create a new traffic policy using the traffic policy document.
_TooManyTrafficPolicyVersionsForCurrentPolicy :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyTrafficPolicyVersionsForCurrentPolicy =
  Core._MatchServiceError
    mkServiceConfig
    "TooManyTrafficPolicyVersionsForCurrentPolicy"
    Core.. Core.hasStatues 400
{-# DEPRECATED _TooManyTrafficPolicyVersionsForCurrentPolicy "Use generic-lens or generic-optics instead." #-}

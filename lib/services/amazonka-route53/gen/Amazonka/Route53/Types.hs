{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Route53.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _CidrBlockInUseException,
    _CidrCollectionAlreadyExistsException,
    _CidrCollectionInUseException,
    _CidrCollectionVersionMismatchException,
    _ConcurrentModification,
    _ConflictingDomainExists,
    _ConflictingTypes,
    _DNSSECNotFound,
    _DelegationSetAlreadyCreated,
    _DelegationSetAlreadyReusable,
    _DelegationSetInUse,
    _DelegationSetNotAvailable,
    _DelegationSetNotReusable,
    _HealthCheckAlreadyExists,
    _HealthCheckInUse,
    _HealthCheckVersionMismatch,
    _HostedZoneAlreadyExists,
    _HostedZoneNotEmpty,
    _HostedZoneNotFound,
    _HostedZoneNotPrivate,
    _HostedZonePartiallyDelegated,
    _IncompatibleVersion,
    _InsufficientCloudWatchLogsResourcePolicy,
    _InvalidArgument,
    _InvalidChangeBatch,
    _InvalidDomainName,
    _InvalidInput,
    _InvalidKMSArn,
    _InvalidKeySigningKeyName,
    _InvalidKeySigningKeyStatus,
    _InvalidPaginationToken,
    _InvalidSigningStatus,
    _InvalidTrafficPolicyDocument,
    _InvalidVPCId,
    _KeySigningKeyAlreadyExists,
    _KeySigningKeyInParentDSRecord,
    _KeySigningKeyInUse,
    _KeySigningKeyWithActiveStatusNotFound,
    _LastVPCAssociation,
    _LimitsExceeded,
    _NoSuchChange,
    _NoSuchCidrCollectionException,
    _NoSuchCidrLocationException,
    _NoSuchCloudWatchLogsLogGroup,
    _NoSuchDelegationSet,
    _NoSuchGeoLocation,
    _NoSuchHealthCheck,
    _NoSuchHostedZone,
    _NoSuchKeySigningKey,
    _NoSuchQueryLoggingConfig,
    _NoSuchTrafficPolicy,
    _NoSuchTrafficPolicyInstance,
    _NotAuthorizedException,
    _PriorRequestNotComplete,
    _PublicZoneVPCAssociation,
    _QueryLoggingConfigAlreadyExists,
    _ThrottlingException,
    _TooManyHealthChecks,
    _TooManyHostedZones,
    _TooManyKeySigningKeys,
    _TooManyTrafficPolicies,
    _TooManyTrafficPolicyInstances,
    _TooManyTrafficPolicyVersionsForCurrentPolicy,
    _TooManyVPCAssociationAuthorizations,
    _TrafficPolicyAlreadyExists,
    _TrafficPolicyInUse,
    _TrafficPolicyInstanceAlreadyExists,
    _VPCAssociationAuthorizationNotFound,
    _VPCAssociationNotFound,

    -- * Re-exported Types
    module Amazonka.Route53.Internal,

    -- * AccountLimitType
    AccountLimitType (..),

    -- * ChangeAction
    ChangeAction (..),

    -- * ChangeStatus
    ChangeStatus (..),

    -- * CidrCollectionChangeAction
    CidrCollectionChangeAction (..),

    -- * CloudWatchRegion
    CloudWatchRegion (..),

    -- * ComparisonOperator
    ComparisonOperator (..),

    -- * HealthCheckRegion
    HealthCheckRegion (..),

    -- * HealthCheckType
    HealthCheckType (..),

    -- * HostedZoneLimitType
    HostedZoneLimitType (..),

    -- * InsufficientDataHealthStatus
    InsufficientDataHealthStatus (..),

    -- * RRType
    RRType (..),

    -- * ResettableElementName
    ResettableElementName (..),

    -- * ResourceRecordSetFailover
    ResourceRecordSetFailover (..),

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
    newAccountLimit,
    accountLimit_type,
    accountLimit_value,

    -- * AlarmIdentifier
    AlarmIdentifier (..),
    newAlarmIdentifier,
    alarmIdentifier_region,
    alarmIdentifier_name,

    -- * AliasTarget
    AliasTarget (..),
    newAliasTarget,
    aliasTarget_hostedZoneId,
    aliasTarget_dNSName,
    aliasTarget_evaluateTargetHealth,

    -- * Change
    Change (..),
    newChange,
    change_action,
    change_resourceRecordSet,

    -- * ChangeBatch
    ChangeBatch (..),
    newChangeBatch,
    changeBatch_comment,
    changeBatch_changes,

    -- * ChangeInfo
    ChangeInfo (..),
    newChangeInfo,
    changeInfo_comment,
    changeInfo_id,
    changeInfo_status,
    changeInfo_submittedAt,

    -- * CidrBlockSummary
    CidrBlockSummary (..),
    newCidrBlockSummary,
    cidrBlockSummary_cidrBlock,
    cidrBlockSummary_locationName,

    -- * CidrCollection
    CidrCollection (..),
    newCidrCollection,
    cidrCollection_arn,
    cidrCollection_id,
    cidrCollection_name,
    cidrCollection_version,

    -- * CidrCollectionChange
    CidrCollectionChange (..),
    newCidrCollectionChange,
    cidrCollectionChange_locationName,
    cidrCollectionChange_action,
    cidrCollectionChange_cidrList,

    -- * CidrRoutingConfig
    CidrRoutingConfig (..),
    newCidrRoutingConfig,
    cidrRoutingConfig_collectionId,
    cidrRoutingConfig_locationName,

    -- * CloudWatchAlarmConfiguration
    CloudWatchAlarmConfiguration (..),
    newCloudWatchAlarmConfiguration,
    cloudWatchAlarmConfiguration_dimensions,
    cloudWatchAlarmConfiguration_evaluationPeriods,
    cloudWatchAlarmConfiguration_threshold,
    cloudWatchAlarmConfiguration_comparisonOperator,
    cloudWatchAlarmConfiguration_period,
    cloudWatchAlarmConfiguration_metricName,
    cloudWatchAlarmConfiguration_namespace,
    cloudWatchAlarmConfiguration_statistic,

    -- * CollectionSummary
    CollectionSummary (..),
    newCollectionSummary,
    collectionSummary_arn,
    collectionSummary_id,
    collectionSummary_name,
    collectionSummary_version,

    -- * DNSSECStatus
    DNSSECStatus (..),
    newDNSSECStatus,
    dNSSECStatus_serveSignature,
    dNSSECStatus_statusMessage,

    -- * DelegationSet
    DelegationSet (..),
    newDelegationSet,
    delegationSet_callerReference,
    delegationSet_id,
    delegationSet_nameServers,

    -- * Dimension
    Dimension (..),
    newDimension,
    dimension_name,
    dimension_value,

    -- * GeoLocation
    GeoLocation (..),
    newGeoLocation,
    geoLocation_continentCode,
    geoLocation_countryCode,
    geoLocation_subdivisionCode,

    -- * GeoLocationDetails
    GeoLocationDetails (..),
    newGeoLocationDetails,
    geoLocationDetails_continentCode,
    geoLocationDetails_continentName,
    geoLocationDetails_countryCode,
    geoLocationDetails_countryName,
    geoLocationDetails_subdivisionCode,
    geoLocationDetails_subdivisionName,

    -- * HealthCheck
    HealthCheck (..),
    newHealthCheck,
    healthCheck_cloudWatchAlarmConfiguration,
    healthCheck_linkedService,
    healthCheck_id,
    healthCheck_callerReference,
    healthCheck_healthCheckConfig,
    healthCheck_healthCheckVersion,

    -- * HealthCheckConfig
    HealthCheckConfig (..),
    newHealthCheckConfig,
    healthCheckConfig_alarmIdentifier,
    healthCheckConfig_childHealthChecks,
    healthCheckConfig_disabled,
    healthCheckConfig_enableSNI,
    healthCheckConfig_failureThreshold,
    healthCheckConfig_fullyQualifiedDomainName,
    healthCheckConfig_healthThreshold,
    healthCheckConfig_iPAddress,
    healthCheckConfig_insufficientDataHealthStatus,
    healthCheckConfig_inverted,
    healthCheckConfig_measureLatency,
    healthCheckConfig_port,
    healthCheckConfig_regions,
    healthCheckConfig_requestInterval,
    healthCheckConfig_resourcePath,
    healthCheckConfig_routingControlArn,
    healthCheckConfig_searchString,
    healthCheckConfig_type,

    -- * HealthCheckObservation
    HealthCheckObservation (..),
    newHealthCheckObservation,
    healthCheckObservation_iPAddress,
    healthCheckObservation_region,
    healthCheckObservation_statusReport,

    -- * HostedZone
    HostedZone (..),
    newHostedZone,
    hostedZone_config,
    hostedZone_linkedService,
    hostedZone_resourceRecordSetCount,
    hostedZone_id,
    hostedZone_name,
    hostedZone_callerReference,

    -- * HostedZoneConfig
    HostedZoneConfig (..),
    newHostedZoneConfig,
    hostedZoneConfig_comment,
    hostedZoneConfig_privateZone,

    -- * HostedZoneLimit
    HostedZoneLimit (..),
    newHostedZoneLimit,
    hostedZoneLimit_type,
    hostedZoneLimit_value,

    -- * HostedZoneOwner
    HostedZoneOwner (..),
    newHostedZoneOwner,
    hostedZoneOwner_owningAccount,
    hostedZoneOwner_owningService,

    -- * HostedZoneSummary
    HostedZoneSummary (..),
    newHostedZoneSummary,
    hostedZoneSummary_hostedZoneId,
    hostedZoneSummary_name,
    hostedZoneSummary_owner,

    -- * KeySigningKey
    KeySigningKey (..),
    newKeySigningKey,
    keySigningKey_createdDate,
    keySigningKey_dNSKEYRecord,
    keySigningKey_dSRecord,
    keySigningKey_digestAlgorithmMnemonic,
    keySigningKey_digestAlgorithmType,
    keySigningKey_digestValue,
    keySigningKey_flag,
    keySigningKey_keyTag,
    keySigningKey_kmsArn,
    keySigningKey_lastModifiedDate,
    keySigningKey_name,
    keySigningKey_publicKey,
    keySigningKey_signingAlgorithmMnemonic,
    keySigningKey_signingAlgorithmType,
    keySigningKey_status,
    keySigningKey_statusMessage,

    -- * LinkedService
    LinkedService (..),
    newLinkedService,
    linkedService_description,
    linkedService_servicePrincipal,

    -- * LocationSummary
    LocationSummary (..),
    newLocationSummary,
    locationSummary_locationName,

    -- * QueryLoggingConfig
    QueryLoggingConfig (..),
    newQueryLoggingConfig,
    queryLoggingConfig_id,
    queryLoggingConfig_hostedZoneId,
    queryLoggingConfig_cloudWatchLogsLogGroupArn,

    -- * ResourceRecord
    ResourceRecord (..),
    newResourceRecord,
    resourceRecord_value,

    -- * ResourceRecordSet
    ResourceRecordSet (..),
    newResourceRecordSet,
    resourceRecordSet_aliasTarget,
    resourceRecordSet_cidrRoutingConfig,
    resourceRecordSet_failover,
    resourceRecordSet_geoLocation,
    resourceRecordSet_healthCheckId,
    resourceRecordSet_multiValueAnswer,
    resourceRecordSet_region,
    resourceRecordSet_resourceRecords,
    resourceRecordSet_setIdentifier,
    resourceRecordSet_ttl,
    resourceRecordSet_trafficPolicyInstanceId,
    resourceRecordSet_weight,
    resourceRecordSet_name,
    resourceRecordSet_type,

    -- * ResourceTagSet
    ResourceTagSet (..),
    newResourceTagSet,
    resourceTagSet_resourceId,
    resourceTagSet_resourceType,
    resourceTagSet_tags,

    -- * ReusableDelegationSetLimit
    ReusableDelegationSetLimit (..),
    newReusableDelegationSetLimit,
    reusableDelegationSetLimit_type,
    reusableDelegationSetLimit_value,

    -- * StatusReport
    StatusReport (..),
    newStatusReport,
    statusReport_checkedTime,
    statusReport_status,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TrafficPolicy
    TrafficPolicy (..),
    newTrafficPolicy,
    trafficPolicy_comment,
    trafficPolicy_id,
    trafficPolicy_version,
    trafficPolicy_name,
    trafficPolicy_type,
    trafficPolicy_document,

    -- * TrafficPolicyInstance
    TrafficPolicyInstance (..),
    newTrafficPolicyInstance,
    trafficPolicyInstance_id,
    trafficPolicyInstance_hostedZoneId,
    trafficPolicyInstance_name,
    trafficPolicyInstance_ttl,
    trafficPolicyInstance_state,
    trafficPolicyInstance_message,
    trafficPolicyInstance_trafficPolicyId,
    trafficPolicyInstance_trafficPolicyVersion,
    trafficPolicyInstance_trafficPolicyType,

    -- * TrafficPolicySummary
    TrafficPolicySummary (..),
    newTrafficPolicySummary,
    trafficPolicySummary_id,
    trafficPolicySummary_name,
    trafficPolicySummary_type,
    trafficPolicySummary_latestVersion,
    trafficPolicySummary_trafficPolicyCount,

    -- * VPC
    VPC (..),
    newVPC,
    vpc_vPCId,
    vpc_vPCRegion,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal
import Amazonka.Route53.Types.AccountLimit
import Amazonka.Route53.Types.AccountLimitType
import Amazonka.Route53.Types.AlarmIdentifier
import Amazonka.Route53.Types.AliasTarget
import Amazonka.Route53.Types.Change
import Amazonka.Route53.Types.ChangeAction
import Amazonka.Route53.Types.ChangeBatch
import Amazonka.Route53.Types.ChangeInfo
import Amazonka.Route53.Types.ChangeStatus
import Amazonka.Route53.Types.CidrBlockSummary
import Amazonka.Route53.Types.CidrCollection
import Amazonka.Route53.Types.CidrCollectionChange
import Amazonka.Route53.Types.CidrCollectionChangeAction
import Amazonka.Route53.Types.CidrRoutingConfig
import Amazonka.Route53.Types.CloudWatchAlarmConfiguration
import Amazonka.Route53.Types.CloudWatchRegion
import Amazonka.Route53.Types.CollectionSummary
import Amazonka.Route53.Types.ComparisonOperator
import Amazonka.Route53.Types.DNSSECStatus
import Amazonka.Route53.Types.DelegationSet
import Amazonka.Route53.Types.Dimension
import Amazonka.Route53.Types.GeoLocation
import Amazonka.Route53.Types.GeoLocationDetails
import Amazonka.Route53.Types.HealthCheck
import Amazonka.Route53.Types.HealthCheckConfig
import Amazonka.Route53.Types.HealthCheckObservation
import Amazonka.Route53.Types.HealthCheckRegion
import Amazonka.Route53.Types.HealthCheckType
import Amazonka.Route53.Types.HostedZone
import Amazonka.Route53.Types.HostedZoneConfig
import Amazonka.Route53.Types.HostedZoneLimit
import Amazonka.Route53.Types.HostedZoneLimitType
import Amazonka.Route53.Types.HostedZoneOwner
import Amazonka.Route53.Types.HostedZoneSummary
import Amazonka.Route53.Types.InsufficientDataHealthStatus
import Amazonka.Route53.Types.KeySigningKey
import Amazonka.Route53.Types.LinkedService
import Amazonka.Route53.Types.LocationSummary
import Amazonka.Route53.Types.QueryLoggingConfig
import Amazonka.Route53.Types.RRType
import Amazonka.Route53.Types.ResettableElementName
import Amazonka.Route53.Types.ResourceRecord
import Amazonka.Route53.Types.ResourceRecordSet
import Amazonka.Route53.Types.ResourceRecordSetFailover
import Amazonka.Route53.Types.ResourceTagSet
import Amazonka.Route53.Types.ReusableDelegationSetLimit
import Amazonka.Route53.Types.ReusableDelegationSetLimitType
import Amazonka.Route53.Types.Statistic
import Amazonka.Route53.Types.StatusReport
import Amazonka.Route53.Types.Tag
import Amazonka.Route53.Types.TagResourceType
import Amazonka.Route53.Types.TrafficPolicy
import Amazonka.Route53.Types.TrafficPolicyInstance
import Amazonka.Route53.Types.TrafficPolicySummary
import Amazonka.Route53.Types.VPC
import Amazonka.Route53.Types.VPCRegion
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2013-04-01@ of the Amazon Route 53 SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Route53",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "route53",
      Core.signingName = "route53",
      Core.version = "2013-04-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseXMLError "Route53",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "PriorRequestNotComplete"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "still_processing"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | This CIDR block is already in use.
_CidrBlockInUseException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CidrBlockInUseException =
  Core._MatchServiceError
    defaultService
    "CidrBlockInUseException"
    Prelude.. Core.hasStatus 400

-- | A CIDR collection with this name and a different caller reference
-- already exists in this account.
_CidrCollectionAlreadyExistsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CidrCollectionAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "CidrCollectionAlreadyExistsException"

-- | This CIDR collection is in use, and isn\'t empty.
_CidrCollectionInUseException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CidrCollectionInUseException =
  Core._MatchServiceError
    defaultService
    "CidrCollectionInUseException"
    Prelude.. Core.hasStatus 400

-- | The CIDR collection version you provided, doesn\'t match the one in the
-- @ListCidrCollections@ operation.
_CidrCollectionVersionMismatchException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CidrCollectionVersionMismatchException =
  Core._MatchServiceError
    defaultService
    "CidrCollectionVersionMismatchException"
    Prelude.. Core.hasStatus 409

-- | Another user submitted a request to create, update, or delete the object
-- at the same time that you did. Retry the request.
_ConcurrentModification :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConcurrentModification =
  Core._MatchServiceError
    defaultService
    "ConcurrentModification"
    Prelude.. Core.hasStatus 400

-- | The cause of this error depends on the operation that you\'re
-- performing:
--
-- -   __Create a public hosted zone:__ Two hosted zones that have the same
--     name or that have a parent\/child relationship (example.com and
--     test.example.com) can\'t have any common name servers. You tried to
--     create a hosted zone that has the same name as an existing hosted
--     zone or that\'s the parent or child of an existing hosted zone, and
--     you specified a delegation set that shares one or more name servers
--     with the existing hosted zone. For more information, see
--     <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateReusableDelegationSet.html CreateReusableDelegationSet>.
--
-- -   __Create a private hosted zone:__ A hosted zone with the specified
--     name already exists and is already associated with the Amazon VPC
--     that you specified.
--
-- -   __Associate VPCs with a private hosted zone:__ The VPC that you
--     specified is already associated with another hosted zone that has
--     the same name.
_ConflictingDomainExists :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictingDomainExists =
  Core._MatchServiceError
    defaultService
    "ConflictingDomainExists"

-- | You tried to update a traffic policy instance by using a traffic policy
-- version that has a different DNS type than the current type for the
-- instance. You specified the type in the JSON document in the
-- @CreateTrafficPolicy@ or @CreateTrafficPolicyVersion@request.
_ConflictingTypes :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictingTypes =
  Core._MatchServiceError
    defaultService
    "ConflictingTypes"
    Prelude.. Core.hasStatus 400

-- | The hosted zone doesn\'t have any DNSSEC resources.
_DNSSECNotFound :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DNSSECNotFound =
  Core._MatchServiceError
    defaultService
    "DNSSECNotFound"
    Prelude.. Core.hasStatus 400

-- | A delegation set with the same owner and caller reference combination
-- has already been created.
_DelegationSetAlreadyCreated :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DelegationSetAlreadyCreated =
  Core._MatchServiceError
    defaultService
    "DelegationSetAlreadyCreated"

-- | The specified delegation set has already been marked as reusable.
_DelegationSetAlreadyReusable :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DelegationSetAlreadyReusable =
  Core._MatchServiceError
    defaultService
    "DelegationSetAlreadyReusable"

-- | The specified delegation contains associated hosted zones which must be
-- deleted before the reusable delegation set can be deleted.
_DelegationSetInUse :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DelegationSetInUse =
  Core._MatchServiceError
    defaultService
    "DelegationSetInUse"

-- | You can create a hosted zone that has the same name as an existing
-- hosted zone (example.com is common), but there is a limit to the number
-- of hosted zones that have the same name. If you get this error, Amazon
-- Route 53 has reached that limit. If you own the domain name and Route 53
-- generates this error, contact Customer Support.
_DelegationSetNotAvailable :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DelegationSetNotAvailable =
  Core._MatchServiceError
    defaultService
    "DelegationSetNotAvailable"

-- | A reusable delegation set with the specified ID does not exist.
_DelegationSetNotReusable :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DelegationSetNotReusable =
  Core._MatchServiceError
    defaultService
    "DelegationSetNotReusable"

-- | The health check you\'re attempting to create already exists. Amazon
-- Route 53 returns this error when you submit a request that has the
-- following values:
--
-- -   The same value for @CallerReference@ as an existing health check,
--     and one or more values that differ from the existing health check
--     that has the same caller reference.
--
-- -   The same value for @CallerReference@ as a health check that you
--     created and later deleted, regardless of the other settings in the
--     request.
_HealthCheckAlreadyExists :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_HealthCheckAlreadyExists =
  Core._MatchServiceError
    defaultService
    "HealthCheckAlreadyExists"
    Prelude.. Core.hasStatus 409

-- | This error code is not in use.
_HealthCheckInUse :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_HealthCheckInUse =
  Core._MatchServiceError
    defaultService
    "HealthCheckInUse"
    Prelude.. Core.hasStatus 400

-- | The value of @HealthCheckVersion@ in the request doesn\'t match the
-- value of @HealthCheckVersion@ in the health check.
_HealthCheckVersionMismatch :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_HealthCheckVersionMismatch =
  Core._MatchServiceError
    defaultService
    "HealthCheckVersionMismatch"
    Prelude.. Core.hasStatus 409

-- | The hosted zone you\'re trying to create already exists. Amazon Route 53
-- returns this error when a hosted zone has already been created with the
-- specified @CallerReference@.
_HostedZoneAlreadyExists :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_HostedZoneAlreadyExists =
  Core._MatchServiceError
    defaultService
    "HostedZoneAlreadyExists"
    Prelude.. Core.hasStatus 409

-- | The hosted zone contains resource records that are not SOA or NS
-- records.
_HostedZoneNotEmpty :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_HostedZoneNotEmpty =
  Core._MatchServiceError
    defaultService
    "HostedZoneNotEmpty"
    Prelude.. Core.hasStatus 400

-- | The specified HostedZone can\'t be found.
_HostedZoneNotFound :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_HostedZoneNotFound =
  Core._MatchServiceError
    defaultService
    "HostedZoneNotFound"

-- | The specified hosted zone is a public hosted zone, not a private hosted
-- zone.
_HostedZoneNotPrivate :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_HostedZoneNotPrivate =
  Core._MatchServiceError
    defaultService
    "HostedZoneNotPrivate"

-- | The hosted zone nameservers don\'t match the parent nameservers. The
-- hosted zone and parent must have the same nameservers.
_HostedZonePartiallyDelegated :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_HostedZonePartiallyDelegated =
  Core._MatchServiceError
    defaultService
    "HostedZonePartiallyDelegated"

-- | The resource you\'re trying to access is unsupported on this Amazon
-- Route 53 endpoint.
_IncompatibleVersion :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_IncompatibleVersion =
  Core._MatchServiceError
    defaultService
    "IncompatibleVersion"
    Prelude.. Core.hasStatus 400

-- | Amazon Route 53 doesn\'t have the permissions required to create log
-- streams and send query logs to log streams. Possible causes include the
-- following:
--
-- -   There is no resource policy that specifies the log group ARN in the
--     value for @Resource@.
--
-- -   The resource policy that includes the log group ARN in the value for
--     @Resource@ doesn\'t have the necessary permissions.
--
-- -   The resource policy hasn\'t finished propagating yet.
--
-- -   The Key management service (KMS) key you specified doesn’t exist or
--     it can’t be used with the log group associated with query log.
--     Update or provide a resource policy to grant permissions for the KMS
--     key.
--
-- -   The Key management service (KMS) key you specified is marked as
--     disabled for the log group associated with query log. Update or
--     provide a resource policy to grant permissions for the KMS key.
_InsufficientCloudWatchLogsResourcePolicy :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InsufficientCloudWatchLogsResourcePolicy =
  Core._MatchServiceError
    defaultService
    "InsufficientCloudWatchLogsResourcePolicy"
    Prelude.. Core.hasStatus 400

-- | Parameter name is not valid.
_InvalidArgument :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidArgument =
  Core._MatchServiceError
    defaultService
    "InvalidArgument"

-- | This exception contains a list of messages that might contain one or
-- more error messages. Each error message indicates one error in the
-- change batch.
_InvalidChangeBatch :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidChangeBatch =
  Core._MatchServiceError
    defaultService
    "InvalidChangeBatch"

-- | The specified domain name is not valid.
_InvalidDomainName :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDomainName =
  Core._MatchServiceError
    defaultService
    "InvalidDomainName"
    Prelude.. Core.hasStatus 400

-- | The input is not valid.
_InvalidInput :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidInput =
  Core._MatchServiceError
    defaultService
    "InvalidInput"
    Prelude.. Core.hasStatus 400

-- | The KeyManagementServiceArn that you specified isn\'t valid to use with
-- DNSSEC signing.
_InvalidKMSArn :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidKMSArn =
  Core._MatchServiceError
    defaultService
    "InvalidKMSArn"

-- | The key-signing key (KSK) name that you specified isn\'t a valid name.
_InvalidKeySigningKeyName :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidKeySigningKeyName =
  Core._MatchServiceError
    defaultService
    "InvalidKeySigningKeyName"
    Prelude.. Core.hasStatus 400

-- | The key-signing key (KSK) status isn\'t valid or another KSK has the
-- status @INTERNAL_FAILURE@.
_InvalidKeySigningKeyStatus :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidKeySigningKeyStatus =
  Core._MatchServiceError
    defaultService
    "InvalidKeySigningKeyStatus"
    Prelude.. Core.hasStatus 400

-- | The value that you specified to get the second or subsequent page of
-- results is invalid.
_InvalidPaginationToken :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidPaginationToken =
  Core._MatchServiceError
    defaultService
    "InvalidPaginationToken"
    Prelude.. Core.hasStatus 400

-- | Your hosted zone status isn\'t valid for this operation. In the hosted
-- zone, change the status to enable @DNSSEC@ or disable @DNSSEC@.
_InvalidSigningStatus :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidSigningStatus =
  Core._MatchServiceError
    defaultService
    "InvalidSigningStatus"

-- | The format of the traffic policy document that you specified in the
-- @Document@ element is not valid.
_InvalidTrafficPolicyDocument :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidTrafficPolicyDocument =
  Core._MatchServiceError
    defaultService
    "InvalidTrafficPolicyDocument"
    Prelude.. Core.hasStatus 400

-- | The VPC ID that you specified either isn\'t a valid ID or the current
-- account is not authorized to access this VPC.
_InvalidVPCId :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidVPCId =
  Core._MatchServiceError
    defaultService
    "InvalidVPCId"
    Prelude.. Core.hasStatus 400

-- | You\'ve already created a key-signing key (KSK) with this name or with
-- the same customer managed key ARN.
_KeySigningKeyAlreadyExists :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_KeySigningKeyAlreadyExists =
  Core._MatchServiceError
    defaultService
    "KeySigningKeyAlreadyExists"
    Prelude.. Core.hasStatus 409

-- | The key-signing key (KSK) is specified in a parent DS record.
_KeySigningKeyInParentDSRecord :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_KeySigningKeyInParentDSRecord =
  Core._MatchServiceError
    defaultService
    "KeySigningKeyInParentDSRecord"
    Prelude.. Core.hasStatus 400

-- | The key-signing key (KSK) that you specified can\'t be deactivated
-- because it\'s the only KSK for a currently-enabled DNSSEC. Disable
-- DNSSEC signing, or add or enable another KSK.
_KeySigningKeyInUse :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_KeySigningKeyInUse =
  Core._MatchServiceError
    defaultService
    "KeySigningKeyInUse"

-- | A key-signing key (KSK) with @ACTIVE@ status wasn\'t found.
_KeySigningKeyWithActiveStatusNotFound :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_KeySigningKeyWithActiveStatusNotFound =
  Core._MatchServiceError
    defaultService
    "KeySigningKeyWithActiveStatusNotFound"

-- | The VPC that you\'re trying to disassociate from the private hosted zone
-- is the last VPC that is associated with the hosted zone. Amazon Route 53
-- doesn\'t support disassociating the last VPC from a hosted zone.
_LastVPCAssociation :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LastVPCAssociation =
  Core._MatchServiceError
    defaultService
    "LastVPCAssociation"
    Prelude.. Core.hasStatus 400

-- | This operation can\'t be completed because the current account has
-- reached the limit on the resource you are trying to create. To request a
-- higher limit, <http://aws.amazon.com/route53-request create a case> with
-- the Amazon Web Services Support Center.
_LimitsExceeded :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitsExceeded =
  Core._MatchServiceError
    defaultService
    "LimitsExceeded"

-- | A change with the specified change ID does not exist.
_NoSuchChange :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoSuchChange =
  Core._MatchServiceError
    defaultService
    "NoSuchChange"
    Prelude.. Core.hasStatus 404

-- | The CIDR collection you specified, doesn\'t exist.
_NoSuchCidrCollectionException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoSuchCidrCollectionException =
  Core._MatchServiceError
    defaultService
    "NoSuchCidrCollectionException"
    Prelude.. Core.hasStatus 404

-- | The CIDR collection location doesn\'t match any locations in your
-- account.
_NoSuchCidrLocationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoSuchCidrLocationException =
  Core._MatchServiceError
    defaultService
    "NoSuchCidrLocationException"
    Prelude.. Core.hasStatus 404

-- | There is no CloudWatch Logs log group with the specified ARN.
_NoSuchCloudWatchLogsLogGroup :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoSuchCloudWatchLogsLogGroup =
  Core._MatchServiceError
    defaultService
    "NoSuchCloudWatchLogsLogGroup"
    Prelude.. Core.hasStatus 404

-- | A reusable delegation set with the specified ID does not exist.
_NoSuchDelegationSet :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoSuchDelegationSet =
  Core._MatchServiceError
    defaultService
    "NoSuchDelegationSet"

-- | Amazon Route 53 doesn\'t support the specified geographic location. For
-- a list of supported geolocation codes, see the
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GeoLocation.html GeoLocation>
-- data type.
_NoSuchGeoLocation :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoSuchGeoLocation =
  Core._MatchServiceError
    defaultService
    "NoSuchGeoLocation"
    Prelude.. Core.hasStatus 404

-- | No health check exists with the specified ID.
_NoSuchHealthCheck :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoSuchHealthCheck =
  Core._MatchServiceError
    defaultService
    "NoSuchHealthCheck"
    Prelude.. Core.hasStatus 404

-- | No hosted zone exists with the ID that you specified.
_NoSuchHostedZone :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoSuchHostedZone =
  Core._MatchServiceError
    defaultService
    "NoSuchHostedZone"
    Prelude.. Core.hasStatus 404

-- | The specified key-signing key (KSK) doesn\'t exist.
_NoSuchKeySigningKey :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoSuchKeySigningKey =
  Core._MatchServiceError
    defaultService
    "NoSuchKeySigningKey"
    Prelude.. Core.hasStatus 404

-- | There is no DNS query logging configuration with the specified ID.
_NoSuchQueryLoggingConfig :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoSuchQueryLoggingConfig =
  Core._MatchServiceError
    defaultService
    "NoSuchQueryLoggingConfig"
    Prelude.. Core.hasStatus 404

-- | No traffic policy exists with the specified ID.
_NoSuchTrafficPolicy :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoSuchTrafficPolicy =
  Core._MatchServiceError
    defaultService
    "NoSuchTrafficPolicy"
    Prelude.. Core.hasStatus 404

-- | No traffic policy instance exists with the specified ID.
_NoSuchTrafficPolicyInstance :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoSuchTrafficPolicyInstance =
  Core._MatchServiceError
    defaultService
    "NoSuchTrafficPolicyInstance"
    Prelude.. Core.hasStatus 404

-- | Associating the specified VPC with the specified hosted zone has not
-- been authorized.
_NotAuthorizedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NotAuthorizedException =
  Core._MatchServiceError
    defaultService
    "NotAuthorizedException"
    Prelude.. Core.hasStatus 401

-- | If Amazon Route 53 can\'t process a request before the next request
-- arrives, it will reject subsequent requests for the same hosted zone and
-- return an @HTTP 400 error@ (@Bad request@). If Route 53 returns this
-- error repeatedly for the same request, we recommend that you wait, in
-- intervals of increasing duration, before you try the request again.
_PriorRequestNotComplete :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PriorRequestNotComplete =
  Core._MatchServiceError
    defaultService
    "PriorRequestNotComplete"
    Prelude.. Core.hasStatus 400

-- | You\'re trying to associate a VPC with a public hosted zone. Amazon
-- Route 53 doesn\'t support associating a VPC with a public hosted zone.
_PublicZoneVPCAssociation :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PublicZoneVPCAssociation =
  Core._MatchServiceError
    defaultService
    "PublicZoneVPCAssociation"
    Prelude.. Core.hasStatus 400

-- | You can create only one query logging configuration for a hosted zone,
-- and a query logging configuration already exists for this hosted zone.
_QueryLoggingConfigAlreadyExists :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_QueryLoggingConfigAlreadyExists =
  Core._MatchServiceError
    defaultService
    "QueryLoggingConfigAlreadyExists"
    Prelude.. Core.hasStatus 409

-- | The limit on the number of requests per second was exceeded.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 400

-- | This health check can\'t be created because the current account has
-- reached the limit on the number of active health checks.
--
-- For information about default limits, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html Limits>
-- in the /Amazon Route 53 Developer Guide/.
--
-- For information about how to get the current limit for an account, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetAccountLimit.html GetAccountLimit>.
-- To request a higher limit,
-- <http://aws.amazon.com/route53-request create a case> with the Amazon
-- Web Services Support Center.
--
-- You have reached the maximum number of active health checks for an
-- Amazon Web Services account. To request a higher limit,
-- <http://aws.amazon.com/route53-request create a case> with the Amazon
-- Web Services Support Center.
_TooManyHealthChecks :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyHealthChecks =
  Core._MatchServiceError
    defaultService
    "TooManyHealthChecks"

-- | This operation can\'t be completed either because the current account
-- has reached the limit on the number of hosted zones or because you\'ve
-- reached the limit on the number of hosted zones that can be associated
-- with a reusable delegation set.
--
-- For information about default limits, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html Limits>
-- in the /Amazon Route 53 Developer Guide/.
--
-- To get the current limit on hosted zones that can be created by an
-- account, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetAccountLimit.html GetAccountLimit>.
--
-- To get the current limit on hosted zones that can be associated with a
-- reusable delegation set, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetReusableDelegationSetLimit.html GetReusableDelegationSetLimit>.
--
-- To request a higher limit,
-- <http://aws.amazon.com/route53-request create a case> with the Amazon
-- Web Services Support Center.
_TooManyHostedZones :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyHostedZones =
  Core._MatchServiceError
    defaultService
    "TooManyHostedZones"
    Prelude.. Core.hasStatus 400

-- | You\'ve reached the limit for the number of key-signing keys (KSKs).
-- Remove at least one KSK, and then try again.
_TooManyKeySigningKeys :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyKeySigningKeys =
  Core._MatchServiceError
    defaultService
    "TooManyKeySigningKeys"

-- | This traffic policy can\'t be created because the current account has
-- reached the limit on the number of traffic policies.
--
-- For information about default limits, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html Limits>
-- in the /Amazon Route 53 Developer Guide/.
--
-- To get the current limit for an account, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetAccountLimit.html GetAccountLimit>.
--
-- To request a higher limit,
-- <http://aws.amazon.com/route53-request create a case> with the Amazon
-- Web Services Support Center.
_TooManyTrafficPolicies :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyTrafficPolicies =
  Core._MatchServiceError
    defaultService
    "TooManyTrafficPolicies"
    Prelude.. Core.hasStatus 400

-- | This traffic policy instance can\'t be created because the current
-- account has reached the limit on the number of traffic policy instances.
--
-- For information about default limits, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html Limits>
-- in the /Amazon Route 53 Developer Guide/.
--
-- For information about how to get the current limit for an account, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetAccountLimit.html GetAccountLimit>.
--
-- To request a higher limit,
-- <http://aws.amazon.com/route53-request create a case> with the Amazon
-- Web Services Support Center.
_TooManyTrafficPolicyInstances :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyTrafficPolicyInstances =
  Core._MatchServiceError
    defaultService
    "TooManyTrafficPolicyInstances"
    Prelude.. Core.hasStatus 400

-- | This traffic policy version can\'t be created because you\'ve reached
-- the limit of 1000 on the number of versions that you can create for the
-- current traffic policy.
--
-- To create more traffic policy versions, you can use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetTrafficPolicy.html GetTrafficPolicy>
-- to get the traffic policy document for a specified traffic policy
-- version, and then use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateTrafficPolicy.html CreateTrafficPolicy>
-- to create a new traffic policy using the traffic policy document.
_TooManyTrafficPolicyVersionsForCurrentPolicy :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyTrafficPolicyVersionsForCurrentPolicy =
  Core._MatchServiceError
    defaultService
    "TooManyTrafficPolicyVersionsForCurrentPolicy"
    Prelude.. Core.hasStatus 400

-- | You\'ve created the maximum number of authorizations that can be created
-- for the specified hosted zone. To authorize another VPC to be associated
-- with the hosted zone, submit a @DeleteVPCAssociationAuthorization@
-- request to remove an existing authorization. To get a list of existing
-- authorizations, submit a @ListVPCAssociationAuthorizations@ request.
_TooManyVPCAssociationAuthorizations :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyVPCAssociationAuthorizations =
  Core._MatchServiceError
    defaultService
    "TooManyVPCAssociationAuthorizations"
    Prelude.. Core.hasStatus 400

-- | A traffic policy that has the same value for @Name@ already exists.
_TrafficPolicyAlreadyExists :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TrafficPolicyAlreadyExists =
  Core._MatchServiceError
    defaultService
    "TrafficPolicyAlreadyExists"
    Prelude.. Core.hasStatus 409

-- | One or more traffic policy instances were created by using the specified
-- traffic policy.
_TrafficPolicyInUse :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TrafficPolicyInUse =
  Core._MatchServiceError
    defaultService
    "TrafficPolicyInUse"
    Prelude.. Core.hasStatus 400

-- | There is already a traffic policy instance with the specified ID.
_TrafficPolicyInstanceAlreadyExists :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TrafficPolicyInstanceAlreadyExists =
  Core._MatchServiceError
    defaultService
    "TrafficPolicyInstanceAlreadyExists"
    Prelude.. Core.hasStatus 409

-- | The VPC that you specified is not authorized to be associated with the
-- hosted zone.
_VPCAssociationAuthorizationNotFound :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_VPCAssociationAuthorizationNotFound =
  Core._MatchServiceError
    defaultService
    "VPCAssociationAuthorizationNotFound"
    Prelude.. Core.hasStatus 404

-- | The specified VPC and hosted zone are not currently associated.
_VPCAssociationNotFound :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_VPCAssociationNotFound =
  Core._MatchServiceError
    defaultService
    "VPCAssociationNotFound"
    Prelude.. Core.hasStatus 404

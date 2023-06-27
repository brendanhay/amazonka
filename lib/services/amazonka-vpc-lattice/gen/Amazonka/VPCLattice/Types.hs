{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.VPCLattice.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VPCLattice.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _ValidationException,

    -- * AuthPolicyState
    AuthPolicyState (..),

    -- * AuthType
    AuthType (..),

    -- * HealthCheckProtocolVersion
    HealthCheckProtocolVersion (..),

    -- * IpAddressType
    IpAddressType (..),

    -- * ListenerProtocol
    ListenerProtocol (..),

    -- * ServiceNetworkServiceAssociationStatus
    ServiceNetworkServiceAssociationStatus (..),

    -- * ServiceNetworkVpcAssociationStatus
    ServiceNetworkVpcAssociationStatus (..),

    -- * ServiceStatus
    ServiceStatus (..),

    -- * TargetGroupProtocol
    TargetGroupProtocol (..),

    -- * TargetGroupProtocolVersion
    TargetGroupProtocolVersion (..),

    -- * TargetGroupStatus
    TargetGroupStatus (..),

    -- * TargetGroupType
    TargetGroupType (..),

    -- * TargetStatus
    TargetStatus (..),

    -- * AccessLogSubscriptionSummary
    AccessLogSubscriptionSummary (..),
    newAccessLogSubscriptionSummary,
    accessLogSubscriptionSummary_arn,
    accessLogSubscriptionSummary_createdAt,
    accessLogSubscriptionSummary_destinationArn,
    accessLogSubscriptionSummary_id,
    accessLogSubscriptionSummary_lastUpdatedAt,
    accessLogSubscriptionSummary_resourceArn,
    accessLogSubscriptionSummary_resourceId,

    -- * DnsEntry
    DnsEntry (..),
    newDnsEntry,
    dnsEntry_domainName,
    dnsEntry_hostedZoneId,

    -- * FixedResponseAction
    FixedResponseAction (..),
    newFixedResponseAction,
    fixedResponseAction_statusCode,

    -- * ForwardAction
    ForwardAction (..),
    newForwardAction,
    forwardAction_targetGroups,

    -- * HeaderMatch
    HeaderMatch (..),
    newHeaderMatch,
    headerMatch_caseSensitive,
    headerMatch_match,
    headerMatch_name,

    -- * HeaderMatchType
    HeaderMatchType (..),
    newHeaderMatchType,
    headerMatchType_contains,
    headerMatchType_exact,
    headerMatchType_prefix,

    -- * HealthCheckConfig
    HealthCheckConfig (..),
    newHealthCheckConfig,
    healthCheckConfig_enabled,
    healthCheckConfig_healthCheckIntervalSeconds,
    healthCheckConfig_healthCheckTimeoutSeconds,
    healthCheckConfig_healthyThresholdCount,
    healthCheckConfig_matcher,
    healthCheckConfig_path,
    healthCheckConfig_port,
    healthCheckConfig_protocol,
    healthCheckConfig_protocolVersion,
    healthCheckConfig_unhealthyThresholdCount,

    -- * HttpMatch
    HttpMatch (..),
    newHttpMatch,
    httpMatch_headerMatches,
    httpMatch_method,
    httpMatch_pathMatch,

    -- * ListenerSummary
    ListenerSummary (..),
    newListenerSummary,
    listenerSummary_arn,
    listenerSummary_createdAt,
    listenerSummary_id,
    listenerSummary_lastUpdatedAt,
    listenerSummary_name,
    listenerSummary_port,
    listenerSummary_protocol,

    -- * Matcher
    Matcher (..),
    newMatcher,
    matcher_httpCode,

    -- * PathMatch
    PathMatch (..),
    newPathMatch,
    pathMatch_caseSensitive,
    pathMatch_match,

    -- * PathMatchType
    PathMatchType (..),
    newPathMatchType,
    pathMatchType_exact,
    pathMatchType_prefix,

    -- * RuleAction
    RuleAction (..),
    newRuleAction,
    ruleAction_fixedResponse,
    ruleAction_forward,

    -- * RuleMatch
    RuleMatch (..),
    newRuleMatch,
    ruleMatch_httpMatch,

    -- * RuleSummary
    RuleSummary (..),
    newRuleSummary,
    ruleSummary_arn,
    ruleSummary_createdAt,
    ruleSummary_id,
    ruleSummary_isDefault,
    ruleSummary_lastUpdatedAt,
    ruleSummary_name,
    ruleSummary_priority,

    -- * RuleUpdate
    RuleUpdate (..),
    newRuleUpdate,
    ruleUpdate_action,
    ruleUpdate_match,
    ruleUpdate_priority,
    ruleUpdate_ruleIdentifier,

    -- * RuleUpdateFailure
    RuleUpdateFailure (..),
    newRuleUpdateFailure,
    ruleUpdateFailure_failureCode,
    ruleUpdateFailure_failureMessage,
    ruleUpdateFailure_ruleIdentifier,

    -- * RuleUpdateSuccess
    RuleUpdateSuccess (..),
    newRuleUpdateSuccess,
    ruleUpdateSuccess_action,
    ruleUpdateSuccess_arn,
    ruleUpdateSuccess_id,
    ruleUpdateSuccess_isDefault,
    ruleUpdateSuccess_match,
    ruleUpdateSuccess_name,
    ruleUpdateSuccess_priority,

    -- * ServiceNetworkServiceAssociationSummary
    ServiceNetworkServiceAssociationSummary (..),
    newServiceNetworkServiceAssociationSummary,
    serviceNetworkServiceAssociationSummary_arn,
    serviceNetworkServiceAssociationSummary_createdAt,
    serviceNetworkServiceAssociationSummary_createdBy,
    serviceNetworkServiceAssociationSummary_customDomainName,
    serviceNetworkServiceAssociationSummary_dnsEntry,
    serviceNetworkServiceAssociationSummary_id,
    serviceNetworkServiceAssociationSummary_serviceArn,
    serviceNetworkServiceAssociationSummary_serviceId,
    serviceNetworkServiceAssociationSummary_serviceName,
    serviceNetworkServiceAssociationSummary_serviceNetworkArn,
    serviceNetworkServiceAssociationSummary_serviceNetworkId,
    serviceNetworkServiceAssociationSummary_serviceNetworkName,
    serviceNetworkServiceAssociationSummary_status,

    -- * ServiceNetworkSummary
    ServiceNetworkSummary (..),
    newServiceNetworkSummary,
    serviceNetworkSummary_arn,
    serviceNetworkSummary_createdAt,
    serviceNetworkSummary_id,
    serviceNetworkSummary_lastUpdatedAt,
    serviceNetworkSummary_name,
    serviceNetworkSummary_numberOfAssociatedServices,
    serviceNetworkSummary_numberOfAssociatedVPCs,

    -- * ServiceNetworkVpcAssociationSummary
    ServiceNetworkVpcAssociationSummary (..),
    newServiceNetworkVpcAssociationSummary,
    serviceNetworkVpcAssociationSummary_arn,
    serviceNetworkVpcAssociationSummary_createdAt,
    serviceNetworkVpcAssociationSummary_createdBy,
    serviceNetworkVpcAssociationSummary_id,
    serviceNetworkVpcAssociationSummary_lastUpdatedAt,
    serviceNetworkVpcAssociationSummary_serviceNetworkArn,
    serviceNetworkVpcAssociationSummary_serviceNetworkId,
    serviceNetworkVpcAssociationSummary_serviceNetworkName,
    serviceNetworkVpcAssociationSummary_status,
    serviceNetworkVpcAssociationSummary_vpcId,

    -- * ServiceSummary
    ServiceSummary (..),
    newServiceSummary,
    serviceSummary_arn,
    serviceSummary_createdAt,
    serviceSummary_customDomainName,
    serviceSummary_dnsEntry,
    serviceSummary_id,
    serviceSummary_lastUpdatedAt,
    serviceSummary_name,
    serviceSummary_status,

    -- * Target
    Target (..),
    newTarget,
    target_port,
    target_id,

    -- * TargetFailure
    TargetFailure (..),
    newTargetFailure,
    targetFailure_failureCode,
    targetFailure_failureMessage,
    targetFailure_id,
    targetFailure_port,

    -- * TargetGroupConfig
    TargetGroupConfig (..),
    newTargetGroupConfig,
    targetGroupConfig_healthCheck,
    targetGroupConfig_ipAddressType,
    targetGroupConfig_protocolVersion,
    targetGroupConfig_port,
    targetGroupConfig_protocol,
    targetGroupConfig_vpcIdentifier,

    -- * TargetGroupSummary
    TargetGroupSummary (..),
    newTargetGroupSummary,
    targetGroupSummary_arn,
    targetGroupSummary_createdAt,
    targetGroupSummary_id,
    targetGroupSummary_ipAddressType,
    targetGroupSummary_lastUpdatedAt,
    targetGroupSummary_name,
    targetGroupSummary_port,
    targetGroupSummary_protocol,
    targetGroupSummary_serviceArns,
    targetGroupSummary_status,
    targetGroupSummary_type,
    targetGroupSummary_vpcIdentifier,

    -- * TargetSummary
    TargetSummary (..),
    newTargetSummary,
    targetSummary_id,
    targetSummary_port,
    targetSummary_reasonCode,
    targetSummary_status,

    -- * WeightedTargetGroup
    WeightedTargetGroup (..),
    newWeightedTargetGroup,
    weightedTargetGroup_weight,
    weightedTargetGroup_targetGroupIdentifier,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign
import Amazonka.VPCLattice.Types.AccessLogSubscriptionSummary
import Amazonka.VPCLattice.Types.AuthPolicyState
import Amazonka.VPCLattice.Types.AuthType
import Amazonka.VPCLattice.Types.DnsEntry
import Amazonka.VPCLattice.Types.FixedResponseAction
import Amazonka.VPCLattice.Types.ForwardAction
import Amazonka.VPCLattice.Types.HeaderMatch
import Amazonka.VPCLattice.Types.HeaderMatchType
import Amazonka.VPCLattice.Types.HealthCheckConfig
import Amazonka.VPCLattice.Types.HealthCheckProtocolVersion
import Amazonka.VPCLattice.Types.HttpMatch
import Amazonka.VPCLattice.Types.IpAddressType
import Amazonka.VPCLattice.Types.ListenerProtocol
import Amazonka.VPCLattice.Types.ListenerSummary
import Amazonka.VPCLattice.Types.Matcher
import Amazonka.VPCLattice.Types.PathMatch
import Amazonka.VPCLattice.Types.PathMatchType
import Amazonka.VPCLattice.Types.RuleAction
import Amazonka.VPCLattice.Types.RuleMatch
import Amazonka.VPCLattice.Types.RuleSummary
import Amazonka.VPCLattice.Types.RuleUpdate
import Amazonka.VPCLattice.Types.RuleUpdateFailure
import Amazonka.VPCLattice.Types.RuleUpdateSuccess
import Amazonka.VPCLattice.Types.ServiceNetworkServiceAssociationStatus
import Amazonka.VPCLattice.Types.ServiceNetworkServiceAssociationSummary
import Amazonka.VPCLattice.Types.ServiceNetworkSummary
import Amazonka.VPCLattice.Types.ServiceNetworkVpcAssociationStatus
import Amazonka.VPCLattice.Types.ServiceNetworkVpcAssociationSummary
import Amazonka.VPCLattice.Types.ServiceStatus
import Amazonka.VPCLattice.Types.ServiceSummary
import Amazonka.VPCLattice.Types.Target
import Amazonka.VPCLattice.Types.TargetFailure
import Amazonka.VPCLattice.Types.TargetGroupConfig
import Amazonka.VPCLattice.Types.TargetGroupProtocol
import Amazonka.VPCLattice.Types.TargetGroupProtocolVersion
import Amazonka.VPCLattice.Types.TargetGroupStatus
import Amazonka.VPCLattice.Types.TargetGroupSummary
import Amazonka.VPCLattice.Types.TargetGroupType
import Amazonka.VPCLattice.Types.TargetStatus
import Amazonka.VPCLattice.Types.TargetSummary
import Amazonka.VPCLattice.Types.WeightedTargetGroup

-- | API version @2022-11-30@ of the Amazon VPC Lattice SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "VPCLattice",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "vpc-lattice",
      Core.signingName = "vpc-lattice",
      Core.version = "2022-11-30",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "VPCLattice",
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
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
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

-- | The user does not have sufficient access to perform this action.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The request conflicts with the current state of the resource. Updating
-- or deleting a resource can cause an inconsistent state.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | An unexpected error occurred while processing the request.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The request references a resource that does not exist.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The request would cause a service quota to be exceeded.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The limit on the number of requests per second was exceeded.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The input does not satisfy the constraints specified by an Amazon Web
-- Services service.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

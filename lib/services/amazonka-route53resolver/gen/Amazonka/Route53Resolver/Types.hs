{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Route53Resolver.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Resolver.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServiceErrorException,
    _InvalidNextTokenException,
    _InvalidParameterException,
    _InvalidPolicyDocument,
    _InvalidRequestException,
    _InvalidTagException,
    _LimitExceededException,
    _ResourceExistsException,
    _ResourceInUseException,
    _ResourceNotFoundException,
    _ResourceUnavailableException,
    _ThrottlingException,
    _UnknownResourceException,
    _ValidationException,

    -- * Action
    Action (..),

    -- * AutodefinedReverseFlag
    AutodefinedReverseFlag (..),

    -- * BlockOverrideDnsType
    BlockOverrideDnsType (..),

    -- * BlockResponse
    BlockResponse (..),

    -- * FirewallDomainImportOperation
    FirewallDomainImportOperation (..),

    -- * FirewallDomainListStatus
    FirewallDomainListStatus (..),

    -- * FirewallDomainUpdateOperation
    FirewallDomainUpdateOperation (..),

    -- * FirewallFailOpenStatus
    FirewallFailOpenStatus (..),

    -- * FirewallRuleGroupAssociationStatus
    FirewallRuleGroupAssociationStatus (..),

    -- * FirewallRuleGroupStatus
    FirewallRuleGroupStatus (..),

    -- * IpAddressStatus
    IpAddressStatus (..),

    -- * MutationProtectionStatus
    MutationProtectionStatus (..),

    -- * ResolverAutodefinedReverseStatus
    ResolverAutodefinedReverseStatus (..),

    -- * ResolverDNSSECValidationStatus
    ResolverDNSSECValidationStatus (..),

    -- * ResolverEndpointDirection
    ResolverEndpointDirection (..),

    -- * ResolverEndpointStatus
    ResolverEndpointStatus (..),

    -- * ResolverQueryLogConfigAssociationError
    ResolverQueryLogConfigAssociationError (..),

    -- * ResolverQueryLogConfigAssociationStatus
    ResolverQueryLogConfigAssociationStatus (..),

    -- * ResolverQueryLogConfigStatus
    ResolverQueryLogConfigStatus (..),

    -- * ResolverRuleAssociationStatus
    ResolverRuleAssociationStatus (..),

    -- * ResolverRuleStatus
    ResolverRuleStatus (..),

    -- * RuleTypeOption
    RuleTypeOption (..),

    -- * ShareStatus
    ShareStatus (..),

    -- * SortOrder
    SortOrder (..),

    -- * Validation
    Validation (..),

    -- * Filter
    Filter (..),
    newFilter,
    filter_name,
    filter_values,

    -- * FirewallConfig
    FirewallConfig (..),
    newFirewallConfig,
    firewallConfig_firewallFailOpen,
    firewallConfig_id,
    firewallConfig_ownerId,
    firewallConfig_resourceId,

    -- * FirewallDomainList
    FirewallDomainList (..),
    newFirewallDomainList,
    firewallDomainList_arn,
    firewallDomainList_creationTime,
    firewallDomainList_creatorRequestId,
    firewallDomainList_domainCount,
    firewallDomainList_id,
    firewallDomainList_managedOwnerName,
    firewallDomainList_modificationTime,
    firewallDomainList_name,
    firewallDomainList_status,
    firewallDomainList_statusMessage,

    -- * FirewallDomainListMetadata
    FirewallDomainListMetadata (..),
    newFirewallDomainListMetadata,
    firewallDomainListMetadata_arn,
    firewallDomainListMetadata_creatorRequestId,
    firewallDomainListMetadata_id,
    firewallDomainListMetadata_managedOwnerName,
    firewallDomainListMetadata_name,

    -- * FirewallRule
    FirewallRule (..),
    newFirewallRule,
    firewallRule_action,
    firewallRule_blockOverrideDnsType,
    firewallRule_blockOverrideDomain,
    firewallRule_blockOverrideTtl,
    firewallRule_blockResponse,
    firewallRule_creationTime,
    firewallRule_creatorRequestId,
    firewallRule_firewallDomainListId,
    firewallRule_firewallRuleGroupId,
    firewallRule_modificationTime,
    firewallRule_name,
    firewallRule_priority,

    -- * FirewallRuleGroup
    FirewallRuleGroup (..),
    newFirewallRuleGroup,
    firewallRuleGroup_arn,
    firewallRuleGroup_creationTime,
    firewallRuleGroup_creatorRequestId,
    firewallRuleGroup_id,
    firewallRuleGroup_modificationTime,
    firewallRuleGroup_name,
    firewallRuleGroup_ownerId,
    firewallRuleGroup_ruleCount,
    firewallRuleGroup_shareStatus,
    firewallRuleGroup_status,
    firewallRuleGroup_statusMessage,

    -- * FirewallRuleGroupAssociation
    FirewallRuleGroupAssociation (..),
    newFirewallRuleGroupAssociation,
    firewallRuleGroupAssociation_arn,
    firewallRuleGroupAssociation_creationTime,
    firewallRuleGroupAssociation_creatorRequestId,
    firewallRuleGroupAssociation_firewallRuleGroupId,
    firewallRuleGroupAssociation_id,
    firewallRuleGroupAssociation_managedOwnerName,
    firewallRuleGroupAssociation_modificationTime,
    firewallRuleGroupAssociation_mutationProtection,
    firewallRuleGroupAssociation_name,
    firewallRuleGroupAssociation_priority,
    firewallRuleGroupAssociation_status,
    firewallRuleGroupAssociation_statusMessage,
    firewallRuleGroupAssociation_vpcId,

    -- * FirewallRuleGroupMetadata
    FirewallRuleGroupMetadata (..),
    newFirewallRuleGroupMetadata,
    firewallRuleGroupMetadata_arn,
    firewallRuleGroupMetadata_creatorRequestId,
    firewallRuleGroupMetadata_id,
    firewallRuleGroupMetadata_name,
    firewallRuleGroupMetadata_ownerId,
    firewallRuleGroupMetadata_shareStatus,

    -- * IpAddressRequest
    IpAddressRequest (..),
    newIpAddressRequest,
    ipAddressRequest_ip,
    ipAddressRequest_subnetId,

    -- * IpAddressResponse
    IpAddressResponse (..),
    newIpAddressResponse,
    ipAddressResponse_creationTime,
    ipAddressResponse_ip,
    ipAddressResponse_ipId,
    ipAddressResponse_modificationTime,
    ipAddressResponse_status,
    ipAddressResponse_statusMessage,
    ipAddressResponse_subnetId,

    -- * IpAddressUpdate
    IpAddressUpdate (..),
    newIpAddressUpdate,
    ipAddressUpdate_ip,
    ipAddressUpdate_ipId,
    ipAddressUpdate_subnetId,

    -- * ResolverConfig
    ResolverConfig (..),
    newResolverConfig,
    resolverConfig_autodefinedReverse,
    resolverConfig_id,
    resolverConfig_ownerId,
    resolverConfig_resourceId,

    -- * ResolverDnssecConfig
    ResolverDnssecConfig (..),
    newResolverDnssecConfig,
    resolverDnssecConfig_id,
    resolverDnssecConfig_ownerId,
    resolverDnssecConfig_resourceId,
    resolverDnssecConfig_validationStatus,

    -- * ResolverEndpoint
    ResolverEndpoint (..),
    newResolverEndpoint,
    resolverEndpoint_arn,
    resolverEndpoint_creationTime,
    resolverEndpoint_creatorRequestId,
    resolverEndpoint_direction,
    resolverEndpoint_hostVPCId,
    resolverEndpoint_id,
    resolverEndpoint_ipAddressCount,
    resolverEndpoint_modificationTime,
    resolverEndpoint_name,
    resolverEndpoint_securityGroupIds,
    resolverEndpoint_status,
    resolverEndpoint_statusMessage,

    -- * ResolverQueryLogConfig
    ResolverQueryLogConfig (..),
    newResolverQueryLogConfig,
    resolverQueryLogConfig_arn,
    resolverQueryLogConfig_associationCount,
    resolverQueryLogConfig_creationTime,
    resolverQueryLogConfig_creatorRequestId,
    resolverQueryLogConfig_destinationArn,
    resolverQueryLogConfig_id,
    resolverQueryLogConfig_name,
    resolverQueryLogConfig_ownerId,
    resolverQueryLogConfig_shareStatus,
    resolverQueryLogConfig_status,

    -- * ResolverQueryLogConfigAssociation
    ResolverQueryLogConfigAssociation (..),
    newResolverQueryLogConfigAssociation,
    resolverQueryLogConfigAssociation_creationTime,
    resolverQueryLogConfigAssociation_error,
    resolverQueryLogConfigAssociation_errorMessage,
    resolverQueryLogConfigAssociation_id,
    resolverQueryLogConfigAssociation_resolverQueryLogConfigId,
    resolverQueryLogConfigAssociation_resourceId,
    resolverQueryLogConfigAssociation_status,

    -- * ResolverRule
    ResolverRule (..),
    newResolverRule,
    resolverRule_arn,
    resolverRule_creationTime,
    resolverRule_creatorRequestId,
    resolverRule_domainName,
    resolverRule_id,
    resolverRule_modificationTime,
    resolverRule_name,
    resolverRule_ownerId,
    resolverRule_resolverEndpointId,
    resolverRule_ruleType,
    resolverRule_shareStatus,
    resolverRule_status,
    resolverRule_statusMessage,
    resolverRule_targetIps,

    -- * ResolverRuleAssociation
    ResolverRuleAssociation (..),
    newResolverRuleAssociation,
    resolverRuleAssociation_id,
    resolverRuleAssociation_name,
    resolverRuleAssociation_resolverRuleId,
    resolverRuleAssociation_status,
    resolverRuleAssociation_statusMessage,
    resolverRuleAssociation_vPCId,

    -- * ResolverRuleConfig
    ResolverRuleConfig (..),
    newResolverRuleConfig,
    resolverRuleConfig_name,
    resolverRuleConfig_resolverEndpointId,
    resolverRuleConfig_targetIps,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TargetAddress
    TargetAddress (..),
    newTargetAddress,
    targetAddress_port,
    targetAddress_ip,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53Resolver.Types.Action
import Amazonka.Route53Resolver.Types.AutodefinedReverseFlag
import Amazonka.Route53Resolver.Types.BlockOverrideDnsType
import Amazonka.Route53Resolver.Types.BlockResponse
import Amazonka.Route53Resolver.Types.Filter
import Amazonka.Route53Resolver.Types.FirewallConfig
import Amazonka.Route53Resolver.Types.FirewallDomainImportOperation
import Amazonka.Route53Resolver.Types.FirewallDomainList
import Amazonka.Route53Resolver.Types.FirewallDomainListMetadata
import Amazonka.Route53Resolver.Types.FirewallDomainListStatus
import Amazonka.Route53Resolver.Types.FirewallDomainUpdateOperation
import Amazonka.Route53Resolver.Types.FirewallFailOpenStatus
import Amazonka.Route53Resolver.Types.FirewallRule
import Amazonka.Route53Resolver.Types.FirewallRuleGroup
import Amazonka.Route53Resolver.Types.FirewallRuleGroupAssociation
import Amazonka.Route53Resolver.Types.FirewallRuleGroupAssociationStatus
import Amazonka.Route53Resolver.Types.FirewallRuleGroupMetadata
import Amazonka.Route53Resolver.Types.FirewallRuleGroupStatus
import Amazonka.Route53Resolver.Types.IpAddressRequest
import Amazonka.Route53Resolver.Types.IpAddressResponse
import Amazonka.Route53Resolver.Types.IpAddressStatus
import Amazonka.Route53Resolver.Types.IpAddressUpdate
import Amazonka.Route53Resolver.Types.MutationProtectionStatus
import Amazonka.Route53Resolver.Types.ResolverAutodefinedReverseStatus
import Amazonka.Route53Resolver.Types.ResolverConfig
import Amazonka.Route53Resolver.Types.ResolverDNSSECValidationStatus
import Amazonka.Route53Resolver.Types.ResolverDnssecConfig
import Amazonka.Route53Resolver.Types.ResolverEndpoint
import Amazonka.Route53Resolver.Types.ResolverEndpointDirection
import Amazonka.Route53Resolver.Types.ResolverEndpointStatus
import Amazonka.Route53Resolver.Types.ResolverQueryLogConfig
import Amazonka.Route53Resolver.Types.ResolverQueryLogConfigAssociation
import Amazonka.Route53Resolver.Types.ResolverQueryLogConfigAssociationError
import Amazonka.Route53Resolver.Types.ResolverQueryLogConfigAssociationStatus
import Amazonka.Route53Resolver.Types.ResolverQueryLogConfigStatus
import Amazonka.Route53Resolver.Types.ResolverRule
import Amazonka.Route53Resolver.Types.ResolverRuleAssociation
import Amazonka.Route53Resolver.Types.ResolverRuleAssociationStatus
import Amazonka.Route53Resolver.Types.ResolverRuleConfig
import Amazonka.Route53Resolver.Types.ResolverRuleStatus
import Amazonka.Route53Resolver.Types.RuleTypeOption
import Amazonka.Route53Resolver.Types.ShareStatus
import Amazonka.Route53Resolver.Types.SortOrder
import Amazonka.Route53Resolver.Types.Tag
import Amazonka.Route53Resolver.Types.TargetAddress
import Amazonka.Route53Resolver.Types.Validation
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-04-01@ of the Amazon Route 53 Resolver SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Route53Resolver",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "route53resolver",
      Core.signingName = "route53resolver",
      Core.version = "2018-04-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Route53Resolver",
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

-- | The current account doesn\'t have the IAM permissions required to
-- perform the specified Resolver operation.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | We encountered an unknown error. Try again in a few minutes.
_InternalServiceErrorException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServiceErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServiceErrorException"

-- | The value that you specified for @NextToken@ in a @List@ request isn\'t
-- valid.
_InvalidNextTokenException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | One or more parameters in this request are not valid.
_InvalidParameterException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | The specified Resolver rule policy is invalid.
_InvalidPolicyDocument :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidPolicyDocument =
  Core._MatchServiceError
    defaultService
    "InvalidPolicyDocument"

-- | The request is invalid.
_InvalidRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | The specified tag is invalid.
_InvalidTagException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidTagException =
  Core._MatchServiceError
    defaultService
    "InvalidTagException"

-- | The request caused one or more limits to be exceeded.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The resource that you tried to create already exists.
_ResourceExistsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceExistsException"

-- | The resource that you tried to update or delete is currently in use.
_ResourceInUseException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

-- | The specified resource doesn\'t exist.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The specified resource isn\'t available.
_ResourceUnavailableException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ResourceUnavailableException"

-- | The request was throttled. Try again in a few minutes.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- | The specified resource doesn\'t exist.
_UnknownResourceException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnknownResourceException =
  Core._MatchServiceError
    defaultService
    "UnknownResourceException"

_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"

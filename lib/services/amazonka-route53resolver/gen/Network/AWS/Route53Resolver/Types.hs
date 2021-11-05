{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Route53Resolver.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Resolver.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidTagException,
    _ValidationException,
    _AccessDeniedException,
    _ResourceUnavailableException,
    _InvalidParameterException,
    _InvalidRequestException,
    _ConflictException,
    _ThrottlingException,
    _InvalidNextTokenException,
    _InternalServiceErrorException,
    _ResourceExistsException,
    _UnknownResourceException,
    _InvalidPolicyDocument,
    _ResourceNotFoundException,
    _LimitExceededException,
    _ResourceInUseException,

    -- * Action
    Action (..),

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
    filter_values,
    filter_name,

    -- * FirewallConfig
    FirewallConfig (..),
    newFirewallConfig,
    firewallConfig_resourceId,
    firewallConfig_ownerId,
    firewallConfig_id,
    firewallConfig_firewallFailOpen,

    -- * FirewallDomainList
    FirewallDomainList (..),
    newFirewallDomainList,
    firewallDomainList_creationTime,
    firewallDomainList_status,
    firewallDomainList_arn,
    firewallDomainList_creatorRequestId,
    firewallDomainList_managedOwnerName,
    firewallDomainList_domainCount,
    firewallDomainList_modificationTime,
    firewallDomainList_statusMessage,
    firewallDomainList_name,
    firewallDomainList_id,

    -- * FirewallDomainListMetadata
    FirewallDomainListMetadata (..),
    newFirewallDomainListMetadata,
    firewallDomainListMetadata_arn,
    firewallDomainListMetadata_creatorRequestId,
    firewallDomainListMetadata_managedOwnerName,
    firewallDomainListMetadata_name,
    firewallDomainListMetadata_id,

    -- * FirewallRule
    FirewallRule (..),
    newFirewallRule,
    firewallRule_creationTime,
    firewallRule_blockOverrideDnsType,
    firewallRule_firewallRuleGroupId,
    firewallRule_priority,
    firewallRule_blockResponse,
    firewallRule_creatorRequestId,
    firewallRule_modificationTime,
    firewallRule_action,
    firewallRule_blockOverrideTtl,
    firewallRule_name,
    firewallRule_blockOverrideDomain,
    firewallRule_firewallDomainListId,

    -- * FirewallRuleGroup
    FirewallRuleGroup (..),
    newFirewallRuleGroup,
    firewallRuleGroup_creationTime,
    firewallRuleGroup_status,
    firewallRuleGroup_arn,
    firewallRuleGroup_creatorRequestId,
    firewallRuleGroup_modificationTime,
    firewallRuleGroup_shareStatus,
    firewallRuleGroup_ownerId,
    firewallRuleGroup_statusMessage,
    firewallRuleGroup_name,
    firewallRuleGroup_id,
    firewallRuleGroup_ruleCount,

    -- * FirewallRuleGroupAssociation
    FirewallRuleGroupAssociation (..),
    newFirewallRuleGroupAssociation,
    firewallRuleGroupAssociation_creationTime,
    firewallRuleGroupAssociation_status,
    firewallRuleGroupAssociation_mutationProtection,
    firewallRuleGroupAssociation_firewallRuleGroupId,
    firewallRuleGroupAssociation_priority,
    firewallRuleGroupAssociation_arn,
    firewallRuleGroupAssociation_vpcId,
    firewallRuleGroupAssociation_creatorRequestId,
    firewallRuleGroupAssociation_managedOwnerName,
    firewallRuleGroupAssociation_modificationTime,
    firewallRuleGroupAssociation_statusMessage,
    firewallRuleGroupAssociation_name,
    firewallRuleGroupAssociation_id,

    -- * FirewallRuleGroupMetadata
    FirewallRuleGroupMetadata (..),
    newFirewallRuleGroupMetadata,
    firewallRuleGroupMetadata_arn,
    firewallRuleGroupMetadata_creatorRequestId,
    firewallRuleGroupMetadata_shareStatus,
    firewallRuleGroupMetadata_ownerId,
    firewallRuleGroupMetadata_name,
    firewallRuleGroupMetadata_id,

    -- * IpAddressRequest
    IpAddressRequest (..),
    newIpAddressRequest,
    ipAddressRequest_ip,
    ipAddressRequest_subnetId,

    -- * IpAddressResponse
    IpAddressResponse (..),
    newIpAddressResponse,
    ipAddressResponse_creationTime,
    ipAddressResponse_status,
    ipAddressResponse_modificationTime,
    ipAddressResponse_subnetId,
    ipAddressResponse_ip,
    ipAddressResponse_ipId,
    ipAddressResponse_statusMessage,

    -- * IpAddressUpdate
    IpAddressUpdate (..),
    newIpAddressUpdate,
    ipAddressUpdate_subnetId,
    ipAddressUpdate_ip,
    ipAddressUpdate_ipId,

    -- * ResolverDnssecConfig
    ResolverDnssecConfig (..),
    newResolverDnssecConfig,
    resolverDnssecConfig_resourceId,
    resolverDnssecConfig_ownerId,
    resolverDnssecConfig_validationStatus,
    resolverDnssecConfig_id,

    -- * ResolverEndpoint
    ResolverEndpoint (..),
    newResolverEndpoint,
    resolverEndpoint_creationTime,
    resolverEndpoint_status,
    resolverEndpoint_securityGroupIds,
    resolverEndpoint_direction,
    resolverEndpoint_arn,
    resolverEndpoint_creatorRequestId,
    resolverEndpoint_modificationTime,
    resolverEndpoint_ipAddressCount,
    resolverEndpoint_statusMessage,
    resolverEndpoint_name,
    resolverEndpoint_id,
    resolverEndpoint_hostVPCId,

    -- * ResolverQueryLogConfig
    ResolverQueryLogConfig (..),
    newResolverQueryLogConfig,
    resolverQueryLogConfig_creationTime,
    resolverQueryLogConfig_status,
    resolverQueryLogConfig_associationCount,
    resolverQueryLogConfig_arn,
    resolverQueryLogConfig_creatorRequestId,
    resolverQueryLogConfig_destinationArn,
    resolverQueryLogConfig_shareStatus,
    resolverQueryLogConfig_ownerId,
    resolverQueryLogConfig_name,
    resolverQueryLogConfig_id,

    -- * ResolverQueryLogConfigAssociation
    ResolverQueryLogConfigAssociation (..),
    newResolverQueryLogConfigAssociation,
    resolverQueryLogConfigAssociation_creationTime,
    resolverQueryLogConfigAssociation_status,
    resolverQueryLogConfigAssociation_resolverQueryLogConfigId,
    resolverQueryLogConfigAssociation_resourceId,
    resolverQueryLogConfigAssociation_error,
    resolverQueryLogConfigAssociation_id,
    resolverQueryLogConfigAssociation_errorMessage,

    -- * ResolverRule
    ResolverRule (..),
    newResolverRule,
    resolverRule_creationTime,
    resolverRule_status,
    resolverRule_arn,
    resolverRule_resolverEndpointId,
    resolverRule_creatorRequestId,
    resolverRule_targetIps,
    resolverRule_modificationTime,
    resolverRule_shareStatus,
    resolverRule_ownerId,
    resolverRule_domainName,
    resolverRule_statusMessage,
    resolverRule_name,
    resolverRule_id,
    resolverRule_ruleType,

    -- * ResolverRuleAssociation
    ResolverRuleAssociation (..),
    newResolverRuleAssociation,
    resolverRuleAssociation_status,
    resolverRuleAssociation_resolverRuleId,
    resolverRuleAssociation_vPCId,
    resolverRuleAssociation_statusMessage,
    resolverRuleAssociation_name,
    resolverRuleAssociation_id,

    -- * ResolverRuleConfig
    ResolverRuleConfig (..),
    newResolverRuleConfig,
    resolverRuleConfig_resolverEndpointId,
    resolverRuleConfig_targetIps,
    resolverRuleConfig_name,

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
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53Resolver.Types.Action
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
    { Core._serviceAbbrev =
        "Route53Resolver",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "route53resolver",
      Core._serviceSigningName = "route53resolver",
      Core._serviceVersion = "2018-04-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "Route53Resolver",
      Core._serviceRetry = retry
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
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified tag is invalid.
_InvalidTagException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTagException =
  Core._MatchServiceError
    defaultService
    "InvalidTagException"

-- |
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"

-- | The current account doesn\'t have the IAM permissions required to
-- perform the specified Resolver operation.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | The specified resource isn\'t available.
_ResourceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ResourceUnavailableException"

-- | One or more parameters in this request are not valid.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | The request is invalid.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- |
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | The request was throttled. Try again in a few minutes.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- | The value that you specified for @NextToken@ in a @List@ request isn\'t
-- valid.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | We encountered an unknown error. Try again in a few minutes.
_InternalServiceErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServiceErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServiceErrorException"

-- | The resource that you tried to create already exists.
_ResourceExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceExistsException"

-- | The specified resource doesn\'t exist.
_UnknownResourceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnknownResourceException =
  Core._MatchServiceError
    defaultService
    "UnknownResourceException"

-- | The specified Resolver rule policy is invalid.
_InvalidPolicyDocument :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPolicyDocument =
  Core._MatchServiceError
    defaultService
    "InvalidPolicyDocument"

-- | The specified resource doesn\'t exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The request caused one or more limits to be exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The resource that you tried to update or delete is currently in use.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Route53Resolver.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Resolver.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ResourceUnavailableException,
    _AccessDeniedException,
    _InvalidPolicyDocument,
    _UnknownResourceException,
    _ResourceNotFoundException,
    _ResourceInUseException,
    _LimitExceededException,
    _InvalidNextTokenException,
    _ConflictException,
    _ThrottlingException,
    _ResourceExistsException,
    _ValidationException,
    _InvalidTagException,
    _InternalServiceErrorException,
    _InvalidRequestException,
    _InvalidParameterException,

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
    firewallConfig_resourceId,
    firewallConfig_ownerId,
    firewallConfig_firewallFailOpen,
    firewallConfig_id,

    -- * FirewallDomainList
    FirewallDomainList (..),
    newFirewallDomainList,
    firewallDomainList_name,
    firewallDomainList_domainCount,
    firewallDomainList_modificationTime,
    firewallDomainList_managedOwnerName,
    firewallDomainList_arn,
    firewallDomainList_status,
    firewallDomainList_id,
    firewallDomainList_creatorRequestId,
    firewallDomainList_creationTime,
    firewallDomainList_statusMessage,

    -- * FirewallDomainListMetadata
    FirewallDomainListMetadata (..),
    newFirewallDomainListMetadata,
    firewallDomainListMetadata_name,
    firewallDomainListMetadata_managedOwnerName,
    firewallDomainListMetadata_arn,
    firewallDomainListMetadata_id,
    firewallDomainListMetadata_creatorRequestId,

    -- * FirewallRule
    FirewallRule (..),
    newFirewallRule,
    firewallRule_name,
    firewallRule_blockResponse,
    firewallRule_modificationTime,
    firewallRule_firewallRuleGroupId,
    firewallRule_blockOverrideTtl,
    firewallRule_blockOverrideDnsType,
    firewallRule_creatorRequestId,
    firewallRule_firewallDomainListId,
    firewallRule_blockOverrideDomain,
    firewallRule_priority,
    firewallRule_action,
    firewallRule_creationTime,

    -- * FirewallRuleGroup
    FirewallRuleGroup (..),
    newFirewallRuleGroup,
    firewallRuleGroup_name,
    firewallRuleGroup_ownerId,
    firewallRuleGroup_shareStatus,
    firewallRuleGroup_modificationTime,
    firewallRuleGroup_arn,
    firewallRuleGroup_status,
    firewallRuleGroup_id,
    firewallRuleGroup_creatorRequestId,
    firewallRuleGroup_ruleCount,
    firewallRuleGroup_creationTime,
    firewallRuleGroup_statusMessage,

    -- * FirewallRuleGroupAssociation
    FirewallRuleGroupAssociation (..),
    newFirewallRuleGroupAssociation,
    firewallRuleGroupAssociation_name,
    firewallRuleGroupAssociation_modificationTime,
    firewallRuleGroupAssociation_firewallRuleGroupId,
    firewallRuleGroupAssociation_managedOwnerName,
    firewallRuleGroupAssociation_arn,
    firewallRuleGroupAssociation_status,
    firewallRuleGroupAssociation_id,
    firewallRuleGroupAssociation_mutationProtection,
    firewallRuleGroupAssociation_creatorRequestId,
    firewallRuleGroupAssociation_priority,
    firewallRuleGroupAssociation_creationTime,
    firewallRuleGroupAssociation_vpcId,
    firewallRuleGroupAssociation_statusMessage,

    -- * FirewallRuleGroupMetadata
    FirewallRuleGroupMetadata (..),
    newFirewallRuleGroupMetadata,
    firewallRuleGroupMetadata_name,
    firewallRuleGroupMetadata_ownerId,
    firewallRuleGroupMetadata_shareStatus,
    firewallRuleGroupMetadata_arn,
    firewallRuleGroupMetadata_id,
    firewallRuleGroupMetadata_creatorRequestId,

    -- * IpAddressRequest
    IpAddressRequest (..),
    newIpAddressRequest,
    ipAddressRequest_ip,
    ipAddressRequest_subnetId,

    -- * IpAddressResponse
    IpAddressResponse (..),
    newIpAddressResponse,
    ipAddressResponse_ipId,
    ipAddressResponse_subnetId,
    ipAddressResponse_modificationTime,
    ipAddressResponse_ip,
    ipAddressResponse_status,
    ipAddressResponse_creationTime,
    ipAddressResponse_statusMessage,

    -- * IpAddressUpdate
    IpAddressUpdate (..),
    newIpAddressUpdate,
    ipAddressUpdate_ipId,
    ipAddressUpdate_subnetId,
    ipAddressUpdate_ip,

    -- * ResolverConfig
    ResolverConfig (..),
    newResolverConfig,
    resolverConfig_resourceId,
    resolverConfig_ownerId,
    resolverConfig_id,
    resolverConfig_autodefinedReverse,

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
    resolverEndpoint_name,
    resolverEndpoint_securityGroupIds,
    resolverEndpoint_modificationTime,
    resolverEndpoint_arn,
    resolverEndpoint_ipAddressCount,
    resolverEndpoint_status,
    resolverEndpoint_id,
    resolverEndpoint_creatorRequestId,
    resolverEndpoint_creationTime,
    resolverEndpoint_statusMessage,
    resolverEndpoint_direction,
    resolverEndpoint_hostVPCId,

    -- * ResolverQueryLogConfig
    ResolverQueryLogConfig (..),
    newResolverQueryLogConfig,
    resolverQueryLogConfig_name,
    resolverQueryLogConfig_ownerId,
    resolverQueryLogConfig_shareStatus,
    resolverQueryLogConfig_arn,
    resolverQueryLogConfig_associationCount,
    resolverQueryLogConfig_status,
    resolverQueryLogConfig_id,
    resolverQueryLogConfig_creatorRequestId,
    resolverQueryLogConfig_creationTime,
    resolverQueryLogConfig_destinationArn,

    -- * ResolverQueryLogConfigAssociation
    ResolverQueryLogConfigAssociation (..),
    newResolverQueryLogConfigAssociation,
    resolverQueryLogConfigAssociation_resourceId,
    resolverQueryLogConfigAssociation_errorMessage,
    resolverQueryLogConfigAssociation_status,
    resolverQueryLogConfigAssociation_resolverQueryLogConfigId,
    resolverQueryLogConfigAssociation_id,
    resolverQueryLogConfigAssociation_creationTime,
    resolverQueryLogConfigAssociation_error,

    -- * ResolverRule
    ResolverRule (..),
    newResolverRule,
    resolverRule_name,
    resolverRule_ownerId,
    resolverRule_shareStatus,
    resolverRule_domainName,
    resolverRule_modificationTime,
    resolverRule_arn,
    resolverRule_targetIps,
    resolverRule_status,
    resolverRule_id,
    resolverRule_creatorRequestId,
    resolverRule_ruleType,
    resolverRule_creationTime,
    resolverRule_statusMessage,
    resolverRule_resolverEndpointId,

    -- * ResolverRuleAssociation
    ResolverRuleAssociation (..),
    newResolverRuleAssociation,
    resolverRuleAssociation_name,
    resolverRuleAssociation_status,
    resolverRuleAssociation_id,
    resolverRuleAssociation_resolverRuleId,
    resolverRuleAssociation_vPCId,
    resolverRuleAssociation_statusMessage,

    -- * ResolverRuleConfig
    ResolverRuleConfig (..),
    newResolverRuleConfig,
    resolverRuleConfig_name,
    resolverRuleConfig_targetIps,
    resolverRuleConfig_resolverEndpointId,

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
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified resource isn\'t available.
_ResourceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ResourceUnavailableException"

-- | The current account doesn\'t have the IAM permissions required to
-- perform the specified Resolver operation.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | The specified Resolver rule policy is invalid.
_InvalidPolicyDocument :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPolicyDocument =
  Core._MatchServiceError
    defaultService
    "InvalidPolicyDocument"

-- | The specified resource doesn\'t exist.
_UnknownResourceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnknownResourceException =
  Core._MatchServiceError
    defaultService
    "UnknownResourceException"

-- | The specified resource doesn\'t exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The resource that you tried to update or delete is currently in use.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

-- | The request caused one or more limits to be exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The value that you specified for @NextToken@ in a @List@ request isn\'t
-- valid.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"

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

-- | The resource that you tried to create already exists.
_ResourceExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceExistsException"

-- |
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"

-- | The specified tag is invalid.
_InvalidTagException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTagException =
  Core._MatchServiceError
    defaultService
    "InvalidTagException"

-- | We encountered an unknown error. Try again in a few minutes.
_InternalServiceErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServiceErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServiceErrorException"

-- | The request is invalid.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | One or more parameters in this request are not valid.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"

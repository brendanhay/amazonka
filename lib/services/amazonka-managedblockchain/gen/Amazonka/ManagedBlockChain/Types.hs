{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ManagedBlockChain.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _IllegalActionException,
    _InternalServiceErrorException,
    _InvalidRequestException,
    _ResourceAlreadyExistsException,
    _ResourceLimitExceededException,
    _ResourceNotFoundException,
    _ResourceNotReadyException,
    _ThrottlingException,
    _TooManyTagsException,

    -- * AccessorStatus
    AccessorStatus (..),

    -- * AccessorType
    AccessorType (..),

    -- * Edition
    Edition (..),

    -- * Framework
    Framework (..),

    -- * InvitationStatus
    InvitationStatus (..),

    -- * MemberStatus
    MemberStatus (..),

    -- * NetworkStatus
    NetworkStatus (..),

    -- * NodeStatus
    NodeStatus (..),

    -- * ProposalStatus
    ProposalStatus (..),

    -- * StateDBType
    StateDBType (..),

    -- * ThresholdComparator
    ThresholdComparator (..),

    -- * VoteValue
    VoteValue (..),

    -- * Accessor
    Accessor (..),
    newAccessor,
    accessor_arn,
    accessor_billingToken,
    accessor_creationDate,
    accessor_id,
    accessor_status,
    accessor_type,

    -- * AccessorSummary
    AccessorSummary (..),
    newAccessorSummary,
    accessorSummary_arn,
    accessorSummary_creationDate,
    accessorSummary_id,
    accessorSummary_status,
    accessorSummary_type,

    -- * ApprovalThresholdPolicy
    ApprovalThresholdPolicy (..),
    newApprovalThresholdPolicy,
    approvalThresholdPolicy_proposalDurationInHours,
    approvalThresholdPolicy_thresholdComparator,
    approvalThresholdPolicy_thresholdPercentage,

    -- * Invitation
    Invitation (..),
    newInvitation,
    invitation_arn,
    invitation_creationDate,
    invitation_expirationDate,
    invitation_invitationId,
    invitation_networkSummary,
    invitation_status,

    -- * InviteAction
    InviteAction (..),
    newInviteAction,
    inviteAction_principal,

    -- * LogConfiguration
    LogConfiguration (..),
    newLogConfiguration,
    logConfiguration_enabled,

    -- * LogConfigurations
    LogConfigurations (..),
    newLogConfigurations,
    logConfigurations_cloudwatch,

    -- * Member
    Member (..),
    newMember,
    member_arn,
    member_creationDate,
    member_description,
    member_frameworkAttributes,
    member_id,
    member_kmsKeyArn,
    member_logPublishingConfiguration,
    member_name,
    member_networkId,
    member_status,
    member_tags,

    -- * MemberConfiguration
    MemberConfiguration (..),
    newMemberConfiguration,
    memberConfiguration_description,
    memberConfiguration_kmsKeyArn,
    memberConfiguration_logPublishingConfiguration,
    memberConfiguration_tags,
    memberConfiguration_name,
    memberConfiguration_frameworkConfiguration,

    -- * MemberFabricAttributes
    MemberFabricAttributes (..),
    newMemberFabricAttributes,
    memberFabricAttributes_adminUsername,
    memberFabricAttributes_caEndpoint,

    -- * MemberFabricConfiguration
    MemberFabricConfiguration (..),
    newMemberFabricConfiguration,
    memberFabricConfiguration_adminUsername,
    memberFabricConfiguration_adminPassword,

    -- * MemberFabricLogPublishingConfiguration
    MemberFabricLogPublishingConfiguration (..),
    newMemberFabricLogPublishingConfiguration,
    memberFabricLogPublishingConfiguration_caLogs,

    -- * MemberFrameworkAttributes
    MemberFrameworkAttributes (..),
    newMemberFrameworkAttributes,
    memberFrameworkAttributes_fabric,

    -- * MemberFrameworkConfiguration
    MemberFrameworkConfiguration (..),
    newMemberFrameworkConfiguration,
    memberFrameworkConfiguration_fabric,

    -- * MemberLogPublishingConfiguration
    MemberLogPublishingConfiguration (..),
    newMemberLogPublishingConfiguration,
    memberLogPublishingConfiguration_fabric,

    -- * MemberSummary
    MemberSummary (..),
    newMemberSummary,
    memberSummary_arn,
    memberSummary_creationDate,
    memberSummary_description,
    memberSummary_id,
    memberSummary_isOwned,
    memberSummary_name,
    memberSummary_status,

    -- * Network
    Network (..),
    newNetwork,
    network_arn,
    network_creationDate,
    network_description,
    network_framework,
    network_frameworkAttributes,
    network_frameworkVersion,
    network_id,
    network_name,
    network_status,
    network_tags,
    network_votingPolicy,
    network_vpcEndpointServiceName,

    -- * NetworkEthereumAttributes
    NetworkEthereumAttributes (..),
    newNetworkEthereumAttributes,
    networkEthereumAttributes_chainId,

    -- * NetworkFabricAttributes
    NetworkFabricAttributes (..),
    newNetworkFabricAttributes,
    networkFabricAttributes_edition,
    networkFabricAttributes_orderingServiceEndpoint,

    -- * NetworkFabricConfiguration
    NetworkFabricConfiguration (..),
    newNetworkFabricConfiguration,
    networkFabricConfiguration_edition,

    -- * NetworkFrameworkAttributes
    NetworkFrameworkAttributes (..),
    newNetworkFrameworkAttributes,
    networkFrameworkAttributes_ethereum,
    networkFrameworkAttributes_fabric,

    -- * NetworkFrameworkConfiguration
    NetworkFrameworkConfiguration (..),
    newNetworkFrameworkConfiguration,
    networkFrameworkConfiguration_fabric,

    -- * NetworkSummary
    NetworkSummary (..),
    newNetworkSummary,
    networkSummary_arn,
    networkSummary_creationDate,
    networkSummary_description,
    networkSummary_framework,
    networkSummary_frameworkVersion,
    networkSummary_id,
    networkSummary_name,
    networkSummary_status,

    -- * Node
    Node (..),
    newNode,
    node_arn,
    node_availabilityZone,
    node_creationDate,
    node_frameworkAttributes,
    node_id,
    node_instanceType,
    node_kmsKeyArn,
    node_logPublishingConfiguration,
    node_memberId,
    node_networkId,
    node_stateDB,
    node_status,
    node_tags,

    -- * NodeConfiguration
    NodeConfiguration (..),
    newNodeConfiguration,
    nodeConfiguration_availabilityZone,
    nodeConfiguration_logPublishingConfiguration,
    nodeConfiguration_stateDB,
    nodeConfiguration_instanceType,

    -- * NodeEthereumAttributes
    NodeEthereumAttributes (..),
    newNodeEthereumAttributes,
    nodeEthereumAttributes_httpEndpoint,
    nodeEthereumAttributes_webSocketEndpoint,

    -- * NodeFabricAttributes
    NodeFabricAttributes (..),
    newNodeFabricAttributes,
    nodeFabricAttributes_peerEndpoint,
    nodeFabricAttributes_peerEventEndpoint,

    -- * NodeFabricLogPublishingConfiguration
    NodeFabricLogPublishingConfiguration (..),
    newNodeFabricLogPublishingConfiguration,
    nodeFabricLogPublishingConfiguration_chaincodeLogs,
    nodeFabricLogPublishingConfiguration_peerLogs,

    -- * NodeFrameworkAttributes
    NodeFrameworkAttributes (..),
    newNodeFrameworkAttributes,
    nodeFrameworkAttributes_ethereum,
    nodeFrameworkAttributes_fabric,

    -- * NodeLogPublishingConfiguration
    NodeLogPublishingConfiguration (..),
    newNodeLogPublishingConfiguration,
    nodeLogPublishingConfiguration_fabric,

    -- * NodeSummary
    NodeSummary (..),
    newNodeSummary,
    nodeSummary_arn,
    nodeSummary_availabilityZone,
    nodeSummary_creationDate,
    nodeSummary_id,
    nodeSummary_instanceType,
    nodeSummary_status,

    -- * Proposal
    Proposal (..),
    newProposal,
    proposal_actions,
    proposal_arn,
    proposal_creationDate,
    proposal_description,
    proposal_expirationDate,
    proposal_networkId,
    proposal_noVoteCount,
    proposal_outstandingVoteCount,
    proposal_proposalId,
    proposal_proposedByMemberId,
    proposal_proposedByMemberName,
    proposal_status,
    proposal_tags,
    proposal_yesVoteCount,

    -- * ProposalActions
    ProposalActions (..),
    newProposalActions,
    proposalActions_invitations,
    proposalActions_removals,

    -- * ProposalSummary
    ProposalSummary (..),
    newProposalSummary,
    proposalSummary_arn,
    proposalSummary_creationDate,
    proposalSummary_description,
    proposalSummary_expirationDate,
    proposalSummary_proposalId,
    proposalSummary_proposedByMemberId,
    proposalSummary_proposedByMemberName,
    proposalSummary_status,

    -- * RemoveAction
    RemoveAction (..),
    newRemoveAction,
    removeAction_memberId,

    -- * VoteSummary
    VoteSummary (..),
    newVoteSummary,
    voteSummary_memberId,
    voteSummary_memberName,
    voteSummary_vote,

    -- * VotingPolicy
    VotingPolicy (..),
    newVotingPolicy,
    votingPolicy_approvalThresholdPolicy,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ManagedBlockChain.Types.Accessor
import Amazonka.ManagedBlockChain.Types.AccessorStatus
import Amazonka.ManagedBlockChain.Types.AccessorSummary
import Amazonka.ManagedBlockChain.Types.AccessorType
import Amazonka.ManagedBlockChain.Types.ApprovalThresholdPolicy
import Amazonka.ManagedBlockChain.Types.Edition
import Amazonka.ManagedBlockChain.Types.Framework
import Amazonka.ManagedBlockChain.Types.Invitation
import Amazonka.ManagedBlockChain.Types.InvitationStatus
import Amazonka.ManagedBlockChain.Types.InviteAction
import Amazonka.ManagedBlockChain.Types.LogConfiguration
import Amazonka.ManagedBlockChain.Types.LogConfigurations
import Amazonka.ManagedBlockChain.Types.Member
import Amazonka.ManagedBlockChain.Types.MemberConfiguration
import Amazonka.ManagedBlockChain.Types.MemberFabricAttributes
import Amazonka.ManagedBlockChain.Types.MemberFabricConfiguration
import Amazonka.ManagedBlockChain.Types.MemberFabricLogPublishingConfiguration
import Amazonka.ManagedBlockChain.Types.MemberFrameworkAttributes
import Amazonka.ManagedBlockChain.Types.MemberFrameworkConfiguration
import Amazonka.ManagedBlockChain.Types.MemberLogPublishingConfiguration
import Amazonka.ManagedBlockChain.Types.MemberStatus
import Amazonka.ManagedBlockChain.Types.MemberSummary
import Amazonka.ManagedBlockChain.Types.Network
import Amazonka.ManagedBlockChain.Types.NetworkEthereumAttributes
import Amazonka.ManagedBlockChain.Types.NetworkFabricAttributes
import Amazonka.ManagedBlockChain.Types.NetworkFabricConfiguration
import Amazonka.ManagedBlockChain.Types.NetworkFrameworkAttributes
import Amazonka.ManagedBlockChain.Types.NetworkFrameworkConfiguration
import Amazonka.ManagedBlockChain.Types.NetworkStatus
import Amazonka.ManagedBlockChain.Types.NetworkSummary
import Amazonka.ManagedBlockChain.Types.Node
import Amazonka.ManagedBlockChain.Types.NodeConfiguration
import Amazonka.ManagedBlockChain.Types.NodeEthereumAttributes
import Amazonka.ManagedBlockChain.Types.NodeFabricAttributes
import Amazonka.ManagedBlockChain.Types.NodeFabricLogPublishingConfiguration
import Amazonka.ManagedBlockChain.Types.NodeFrameworkAttributes
import Amazonka.ManagedBlockChain.Types.NodeLogPublishingConfiguration
import Amazonka.ManagedBlockChain.Types.NodeStatus
import Amazonka.ManagedBlockChain.Types.NodeSummary
import Amazonka.ManagedBlockChain.Types.Proposal
import Amazonka.ManagedBlockChain.Types.ProposalActions
import Amazonka.ManagedBlockChain.Types.ProposalStatus
import Amazonka.ManagedBlockChain.Types.ProposalSummary
import Amazonka.ManagedBlockChain.Types.RemoveAction
import Amazonka.ManagedBlockChain.Types.StateDBType
import Amazonka.ManagedBlockChain.Types.ThresholdComparator
import Amazonka.ManagedBlockChain.Types.VoteSummary
import Amazonka.ManagedBlockChain.Types.VoteValue
import Amazonka.ManagedBlockChain.Types.VotingPolicy
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-09-24@ of the Amazon Managed Blockchain SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "ManagedBlockChain",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "managedblockchain",
      Core.signingName = "managedblockchain",
      Core.version = "2018-09-24",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "ManagedBlockChain",
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

-- | You don\'t have sufficient access to perform this action.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

_IllegalActionException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_IllegalActionException =
  Core._MatchServiceError
    defaultService
    "IllegalActionException"
    Prelude.. Core.hasStatus 400

-- | The request processing has failed because of an unknown error, exception
-- or failure.
_InternalServiceErrorException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServiceErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServiceErrorException"
    Prelude.. Core.hasStatus 500

-- | The action or operation requested is invalid. Verify that the action is
-- typed correctly.
_InvalidRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
    Prelude.. Core.hasStatus 400

-- | A resource request is issued for a resource that already exists.
_ResourceAlreadyExistsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"
    Prelude.. Core.hasStatus 409

-- | The maximum number of resources of that type already exist. Ensure the
-- resources requested are within the boundaries of the service edition and
-- your account limits.
_ResourceLimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ResourceLimitExceededException"
    Prelude.. Core.hasStatus 429

-- | A requested resource doesn\'t exist. It may have been deleted or
-- referenced incorrectly.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The requested resource exists but isn\'t in a status that can complete
-- the operation.
_ResourceNotReadyException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotReadyException =
  Core._MatchServiceError
    defaultService
    "ResourceNotReadyException"
    Prelude.. Core.hasStatus 409

-- | The request or operation couldn\'t be performed because a service is
-- throttling requests. The most common source of throttling errors is
-- creating resources that exceed your service limit for this resource
-- type. Request a limit increase or delete unused resources if possible.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

_TooManyTagsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"
    Prelude.. Core.hasStatus 400

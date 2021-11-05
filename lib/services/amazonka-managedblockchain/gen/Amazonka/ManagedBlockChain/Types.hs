{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ManagedBlockChain.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _InvalidRequestException,
    _ResourceAlreadyExistsException,
    _ResourceLimitExceededException,
    _TooManyTagsException,
    _IllegalActionException,
    _ThrottlingException,
    _InternalServiceErrorException,
    _ResourceNotReadyException,
    _ResourceNotFoundException,

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

    -- * ApprovalThresholdPolicy
    ApprovalThresholdPolicy (..),
    newApprovalThresholdPolicy,
    approvalThresholdPolicy_thresholdPercentage,
    approvalThresholdPolicy_thresholdComparator,
    approvalThresholdPolicy_proposalDurationInHours,

    -- * Invitation
    Invitation (..),
    newInvitation,
    invitation_status,
    invitation_arn,
    invitation_invitationId,
    invitation_expirationDate,
    invitation_networkSummary,
    invitation_creationDate,

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
    member_status,
    member_kmsKeyArn,
    member_logPublishingConfiguration,
    member_arn,
    member_networkId,
    member_name,
    member_id,
    member_creationDate,
    member_frameworkAttributes,
    member_description,
    member_tags,

    -- * MemberConfiguration
    MemberConfiguration (..),
    newMemberConfiguration,
    memberConfiguration_kmsKeyArn,
    memberConfiguration_logPublishingConfiguration,
    memberConfiguration_description,
    memberConfiguration_tags,
    memberConfiguration_name,
    memberConfiguration_frameworkConfiguration,

    -- * MemberFabricAttributes
    MemberFabricAttributes (..),
    newMemberFabricAttributes,
    memberFabricAttributes_caEndpoint,
    memberFabricAttributes_adminUsername,

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
    memberSummary_status,
    memberSummary_arn,
    memberSummary_name,
    memberSummary_id,
    memberSummary_isOwned,
    memberSummary_creationDate,
    memberSummary_description,

    -- * Network
    Network (..),
    newNetwork,
    network_status,
    network_framework,
    network_arn,
    network_frameworkVersion,
    network_vpcEndpointServiceName,
    network_name,
    network_id,
    network_votingPolicy,
    network_creationDate,
    network_frameworkAttributes,
    network_description,
    network_tags,

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
    networkFrameworkAttributes_fabric,
    networkFrameworkAttributes_ethereum,

    -- * NetworkFrameworkConfiguration
    NetworkFrameworkConfiguration (..),
    newNetworkFrameworkConfiguration,
    networkFrameworkConfiguration_fabric,

    -- * NetworkSummary
    NetworkSummary (..),
    newNetworkSummary,
    networkSummary_status,
    networkSummary_framework,
    networkSummary_arn,
    networkSummary_frameworkVersion,
    networkSummary_name,
    networkSummary_id,
    networkSummary_creationDate,
    networkSummary_description,

    -- * Node
    Node (..),
    newNode,
    node_status,
    node_kmsKeyArn,
    node_logPublishingConfiguration,
    node_memberId,
    node_arn,
    node_networkId,
    node_instanceType,
    node_stateDB,
    node_availabilityZone,
    node_id,
    node_creationDate,
    node_frameworkAttributes,
    node_tags,

    -- * NodeConfiguration
    NodeConfiguration (..),
    newNodeConfiguration,
    nodeConfiguration_logPublishingConfiguration,
    nodeConfiguration_stateDB,
    nodeConfiguration_availabilityZone,
    nodeConfiguration_instanceType,

    -- * NodeEthereumAttributes
    NodeEthereumAttributes (..),
    newNodeEthereumAttributes,
    nodeEthereumAttributes_httpEndpoint,
    nodeEthereumAttributes_webSocketEndpoint,

    -- * NodeFabricAttributes
    NodeFabricAttributes (..),
    newNodeFabricAttributes,
    nodeFabricAttributes_peerEventEndpoint,
    nodeFabricAttributes_peerEndpoint,

    -- * NodeFabricLogPublishingConfiguration
    NodeFabricLogPublishingConfiguration (..),
    newNodeFabricLogPublishingConfiguration,
    nodeFabricLogPublishingConfiguration_chaincodeLogs,
    nodeFabricLogPublishingConfiguration_peerLogs,

    -- * NodeFrameworkAttributes
    NodeFrameworkAttributes (..),
    newNodeFrameworkAttributes,
    nodeFrameworkAttributes_fabric,
    nodeFrameworkAttributes_ethereum,

    -- * NodeLogPublishingConfiguration
    NodeLogPublishingConfiguration (..),
    newNodeLogPublishingConfiguration,
    nodeLogPublishingConfiguration_fabric,

    -- * NodeSummary
    NodeSummary (..),
    newNodeSummary,
    nodeSummary_status,
    nodeSummary_arn,
    nodeSummary_instanceType,
    nodeSummary_availabilityZone,
    nodeSummary_id,
    nodeSummary_creationDate,

    -- * Proposal
    Proposal (..),
    newProposal,
    proposal_status,
    proposal_yesVoteCount,
    proposal_noVoteCount,
    proposal_arn,
    proposal_actions,
    proposal_networkId,
    proposal_proposedByMemberId,
    proposal_proposalId,
    proposal_proposedByMemberName,
    proposal_expirationDate,
    proposal_creationDate,
    proposal_description,
    proposal_tags,
    proposal_outstandingVoteCount,

    -- * ProposalActions
    ProposalActions (..),
    newProposalActions,
    proposalActions_invitations,
    proposalActions_removals,

    -- * ProposalSummary
    ProposalSummary (..),
    newProposalSummary,
    proposalSummary_status,
    proposalSummary_arn,
    proposalSummary_proposedByMemberId,
    proposalSummary_proposalId,
    proposalSummary_proposedByMemberName,
    proposalSummary_expirationDate,
    proposalSummary_creationDate,
    proposalSummary_description,

    -- * RemoveAction
    RemoveAction (..),
    newRemoveAction,
    removeAction_memberId,

    -- * VoteSummary
    VoteSummary (..),
    newVoteSummary,
    voteSummary_memberName,
    voteSummary_memberId,
    voteSummary_vote,

    -- * VotingPolicy
    VotingPolicy (..),
    newVotingPolicy,
    votingPolicy_approvalThresholdPolicy,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
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
    { Core._serviceAbbrev =
        "ManagedBlockChain",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "managedblockchain",
      Core._serviceSigningName = "managedblockchain",
      Core._serviceVersion = "2018-09-24",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "ManagedBlockChain",
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

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The action or operation requested is invalid. Verify that the action is
-- typed correctly.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
    Prelude.. Core.hasStatus 400

-- | A resource request is issued for a resource that already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"
    Prelude.. Core.hasStatus 409

-- | The maximum number of resources of that type already exist. Ensure the
-- resources requested are within the boundaries of the service edition and
-- your account limits.
_ResourceLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ResourceLimitExceededException"
    Prelude.. Core.hasStatus 429

-- |
_TooManyTagsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"
    Prelude.. Core.hasStatus 400

-- |
_IllegalActionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IllegalActionException =
  Core._MatchServiceError
    defaultService
    "IllegalActionException"
    Prelude.. Core.hasStatus 400

-- | The request or operation could not be performed because a service is
-- throttling requests. The most common source of throttling errors is
-- launching EC2 instances such that your service limit for EC2 instances
-- is exceeded. Request a limit increase or delete unused resources if
-- possible.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The request processing has failed because of an unknown error, exception
-- or failure.
_InternalServiceErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServiceErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServiceErrorException"
    Prelude.. Core.hasStatus 500

-- | The requested resource exists but is not in a status that can complete
-- the operation.
_ResourceNotReadyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotReadyException =
  Core._MatchServiceError
    defaultService
    "ResourceNotReadyException"
    Prelude.. Core.hasStatus 409

-- | A requested resource does not exist. It may have been deleted or
-- referenced inaccurately.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

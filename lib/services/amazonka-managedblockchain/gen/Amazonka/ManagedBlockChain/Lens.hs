{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ManagedBlockChain.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Lens
  ( -- * Operations

    -- ** CreateAccessor
    createAccessor_tags,
    createAccessor_clientRequestToken,
    createAccessor_accessorType,
    createAccessorResponse_accessorId,
    createAccessorResponse_billingToken,
    createAccessorResponse_httpStatus,

    -- ** CreateMember
    createMember_clientRequestToken,
    createMember_invitationId,
    createMember_networkId,
    createMember_memberConfiguration,
    createMemberResponse_memberId,
    createMemberResponse_httpStatus,

    -- ** CreateNetwork
    createNetwork_description,
    createNetwork_frameworkConfiguration,
    createNetwork_tags,
    createNetwork_clientRequestToken,
    createNetwork_name,
    createNetwork_framework,
    createNetwork_frameworkVersion,
    createNetwork_votingPolicy,
    createNetwork_memberConfiguration,
    createNetworkResponse_memberId,
    createNetworkResponse_networkId,
    createNetworkResponse_httpStatus,

    -- ** CreateNode
    createNode_memberId,
    createNode_tags,
    createNode_clientRequestToken,
    createNode_networkId,
    createNode_nodeConfiguration,
    createNodeResponse_nodeId,
    createNodeResponse_httpStatus,

    -- ** CreateProposal
    createProposal_description,
    createProposal_tags,
    createProposal_clientRequestToken,
    createProposal_networkId,
    createProposal_memberId,
    createProposal_actions,
    createProposalResponse_proposalId,
    createProposalResponse_httpStatus,

    -- ** DeleteAccessor
    deleteAccessor_accessorId,
    deleteAccessorResponse_httpStatus,

    -- ** DeleteMember
    deleteMember_networkId,
    deleteMember_memberId,
    deleteMemberResponse_httpStatus,

    -- ** DeleteNode
    deleteNode_memberId,
    deleteNode_networkId,
    deleteNode_nodeId,
    deleteNodeResponse_httpStatus,

    -- ** GetAccessor
    getAccessor_accessorId,
    getAccessorResponse_accessor,
    getAccessorResponse_httpStatus,

    -- ** GetMember
    getMember_networkId,
    getMember_memberId,
    getMemberResponse_member,
    getMemberResponse_httpStatus,

    -- ** GetNetwork
    getNetwork_networkId,
    getNetworkResponse_network,
    getNetworkResponse_httpStatus,

    -- ** GetNode
    getNode_memberId,
    getNode_networkId,
    getNode_nodeId,
    getNodeResponse_node,
    getNodeResponse_httpStatus,

    -- ** GetProposal
    getProposal_networkId,
    getProposal_proposalId,
    getProposalResponse_proposal,
    getProposalResponse_httpStatus,

    -- ** ListAccessors
    listAccessors_maxResults,
    listAccessors_nextToken,
    listAccessorsResponse_accessors,
    listAccessorsResponse_nextToken,
    listAccessorsResponse_httpStatus,

    -- ** ListInvitations
    listInvitations_maxResults,
    listInvitations_nextToken,
    listInvitationsResponse_invitations,
    listInvitationsResponse_nextToken,
    listInvitationsResponse_httpStatus,

    -- ** ListMembers
    listMembers_isOwned,
    listMembers_maxResults,
    listMembers_name,
    listMembers_nextToken,
    listMembers_status,
    listMembers_networkId,
    listMembersResponse_members,
    listMembersResponse_nextToken,
    listMembersResponse_httpStatus,

    -- ** ListNetworks
    listNetworks_framework,
    listNetworks_maxResults,
    listNetworks_name,
    listNetworks_nextToken,
    listNetworks_status,
    listNetworksResponse_networks,
    listNetworksResponse_nextToken,
    listNetworksResponse_httpStatus,

    -- ** ListNodes
    listNodes_maxResults,
    listNodes_memberId,
    listNodes_nextToken,
    listNodes_status,
    listNodes_networkId,
    listNodesResponse_nextToken,
    listNodesResponse_nodes,
    listNodesResponse_httpStatus,

    -- ** ListProposalVotes
    listProposalVotes_maxResults,
    listProposalVotes_nextToken,
    listProposalVotes_networkId,
    listProposalVotes_proposalId,
    listProposalVotesResponse_nextToken,
    listProposalVotesResponse_proposalVotes,
    listProposalVotesResponse_httpStatus,

    -- ** ListProposals
    listProposals_maxResults,
    listProposals_nextToken,
    listProposals_networkId,
    listProposalsResponse_nextToken,
    listProposalsResponse_proposals,
    listProposalsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** RejectInvitation
    rejectInvitation_invitationId,
    rejectInvitationResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateMember
    updateMember_logPublishingConfiguration,
    updateMember_networkId,
    updateMember_memberId,
    updateMemberResponse_httpStatus,

    -- ** UpdateNode
    updateNode_logPublishingConfiguration,
    updateNode_memberId,
    updateNode_networkId,
    updateNode_nodeId,
    updateNodeResponse_httpStatus,

    -- ** VoteOnProposal
    voteOnProposal_networkId,
    voteOnProposal_proposalId,
    voteOnProposal_voterMemberId,
    voteOnProposal_vote,
    voteOnProposalResponse_httpStatus,

    -- * Types

    -- ** Accessor
    accessor_arn,
    accessor_billingToken,
    accessor_creationDate,
    accessor_id,
    accessor_status,
    accessor_tags,
    accessor_type,

    -- ** AccessorSummary
    accessorSummary_arn,
    accessorSummary_creationDate,
    accessorSummary_id,
    accessorSummary_status,
    accessorSummary_type,

    -- ** ApprovalThresholdPolicy
    approvalThresholdPolicy_proposalDurationInHours,
    approvalThresholdPolicy_thresholdComparator,
    approvalThresholdPolicy_thresholdPercentage,

    -- ** Invitation
    invitation_arn,
    invitation_creationDate,
    invitation_expirationDate,
    invitation_invitationId,
    invitation_networkSummary,
    invitation_status,

    -- ** InviteAction
    inviteAction_principal,

    -- ** LogConfiguration
    logConfiguration_enabled,

    -- ** LogConfigurations
    logConfigurations_cloudwatch,

    -- ** Member
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

    -- ** MemberConfiguration
    memberConfiguration_description,
    memberConfiguration_kmsKeyArn,
    memberConfiguration_logPublishingConfiguration,
    memberConfiguration_tags,
    memberConfiguration_name,
    memberConfiguration_frameworkConfiguration,

    -- ** MemberFabricAttributes
    memberFabricAttributes_adminUsername,
    memberFabricAttributes_caEndpoint,

    -- ** MemberFabricConfiguration
    memberFabricConfiguration_adminUsername,
    memberFabricConfiguration_adminPassword,

    -- ** MemberFabricLogPublishingConfiguration
    memberFabricLogPublishingConfiguration_caLogs,

    -- ** MemberFrameworkAttributes
    memberFrameworkAttributes_fabric,

    -- ** MemberFrameworkConfiguration
    memberFrameworkConfiguration_fabric,

    -- ** MemberLogPublishingConfiguration
    memberLogPublishingConfiguration_fabric,

    -- ** MemberSummary
    memberSummary_arn,
    memberSummary_creationDate,
    memberSummary_description,
    memberSummary_id,
    memberSummary_isOwned,
    memberSummary_name,
    memberSummary_status,

    -- ** Network
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

    -- ** NetworkEthereumAttributes
    networkEthereumAttributes_chainId,

    -- ** NetworkFabricAttributes
    networkFabricAttributes_edition,
    networkFabricAttributes_orderingServiceEndpoint,

    -- ** NetworkFabricConfiguration
    networkFabricConfiguration_edition,

    -- ** NetworkFrameworkAttributes
    networkFrameworkAttributes_ethereum,
    networkFrameworkAttributes_fabric,

    -- ** NetworkFrameworkConfiguration
    networkFrameworkConfiguration_fabric,

    -- ** NetworkSummary
    networkSummary_arn,
    networkSummary_creationDate,
    networkSummary_description,
    networkSummary_framework,
    networkSummary_frameworkVersion,
    networkSummary_id,
    networkSummary_name,
    networkSummary_status,

    -- ** Node
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

    -- ** NodeConfiguration
    nodeConfiguration_availabilityZone,
    nodeConfiguration_logPublishingConfiguration,
    nodeConfiguration_stateDB,
    nodeConfiguration_instanceType,

    -- ** NodeEthereumAttributes
    nodeEthereumAttributes_httpEndpoint,
    nodeEthereumAttributes_webSocketEndpoint,

    -- ** NodeFabricAttributes
    nodeFabricAttributes_peerEndpoint,
    nodeFabricAttributes_peerEventEndpoint,

    -- ** NodeFabricLogPublishingConfiguration
    nodeFabricLogPublishingConfiguration_chaincodeLogs,
    nodeFabricLogPublishingConfiguration_peerLogs,

    -- ** NodeFrameworkAttributes
    nodeFrameworkAttributes_ethereum,
    nodeFrameworkAttributes_fabric,

    -- ** NodeLogPublishingConfiguration
    nodeLogPublishingConfiguration_fabric,

    -- ** NodeSummary
    nodeSummary_arn,
    nodeSummary_availabilityZone,
    nodeSummary_creationDate,
    nodeSummary_id,
    nodeSummary_instanceType,
    nodeSummary_status,

    -- ** Proposal
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

    -- ** ProposalActions
    proposalActions_invitations,
    proposalActions_removals,

    -- ** ProposalSummary
    proposalSummary_arn,
    proposalSummary_creationDate,
    proposalSummary_description,
    proposalSummary_expirationDate,
    proposalSummary_proposalId,
    proposalSummary_proposedByMemberId,
    proposalSummary_proposedByMemberName,
    proposalSummary_status,

    -- ** RemoveAction
    removeAction_memberId,

    -- ** VoteSummary
    voteSummary_memberId,
    voteSummary_memberName,
    voteSummary_vote,

    -- ** VotingPolicy
    votingPolicy_approvalThresholdPolicy,
  )
where

import Amazonka.ManagedBlockChain.CreateAccessor
import Amazonka.ManagedBlockChain.CreateMember
import Amazonka.ManagedBlockChain.CreateNetwork
import Amazonka.ManagedBlockChain.CreateNode
import Amazonka.ManagedBlockChain.CreateProposal
import Amazonka.ManagedBlockChain.DeleteAccessor
import Amazonka.ManagedBlockChain.DeleteMember
import Amazonka.ManagedBlockChain.DeleteNode
import Amazonka.ManagedBlockChain.GetAccessor
import Amazonka.ManagedBlockChain.GetMember
import Amazonka.ManagedBlockChain.GetNetwork
import Amazonka.ManagedBlockChain.GetNode
import Amazonka.ManagedBlockChain.GetProposal
import Amazonka.ManagedBlockChain.ListAccessors
import Amazonka.ManagedBlockChain.ListInvitations
import Amazonka.ManagedBlockChain.ListMembers
import Amazonka.ManagedBlockChain.ListNetworks
import Amazonka.ManagedBlockChain.ListNodes
import Amazonka.ManagedBlockChain.ListProposalVotes
import Amazonka.ManagedBlockChain.ListProposals
import Amazonka.ManagedBlockChain.ListTagsForResource
import Amazonka.ManagedBlockChain.RejectInvitation
import Amazonka.ManagedBlockChain.TagResource
import Amazonka.ManagedBlockChain.Types.Accessor
import Amazonka.ManagedBlockChain.Types.AccessorSummary
import Amazonka.ManagedBlockChain.Types.ApprovalThresholdPolicy
import Amazonka.ManagedBlockChain.Types.Invitation
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
import Amazonka.ManagedBlockChain.Types.MemberSummary
import Amazonka.ManagedBlockChain.Types.Network
import Amazonka.ManagedBlockChain.Types.NetworkEthereumAttributes
import Amazonka.ManagedBlockChain.Types.NetworkFabricAttributes
import Amazonka.ManagedBlockChain.Types.NetworkFabricConfiguration
import Amazonka.ManagedBlockChain.Types.NetworkFrameworkAttributes
import Amazonka.ManagedBlockChain.Types.NetworkFrameworkConfiguration
import Amazonka.ManagedBlockChain.Types.NetworkSummary
import Amazonka.ManagedBlockChain.Types.Node
import Amazonka.ManagedBlockChain.Types.NodeConfiguration
import Amazonka.ManagedBlockChain.Types.NodeEthereumAttributes
import Amazonka.ManagedBlockChain.Types.NodeFabricAttributes
import Amazonka.ManagedBlockChain.Types.NodeFabricLogPublishingConfiguration
import Amazonka.ManagedBlockChain.Types.NodeFrameworkAttributes
import Amazonka.ManagedBlockChain.Types.NodeLogPublishingConfiguration
import Amazonka.ManagedBlockChain.Types.NodeSummary
import Amazonka.ManagedBlockChain.Types.Proposal
import Amazonka.ManagedBlockChain.Types.ProposalActions
import Amazonka.ManagedBlockChain.Types.ProposalSummary
import Amazonka.ManagedBlockChain.Types.RemoveAction
import Amazonka.ManagedBlockChain.Types.VoteSummary
import Amazonka.ManagedBlockChain.Types.VotingPolicy
import Amazonka.ManagedBlockChain.UntagResource
import Amazonka.ManagedBlockChain.UpdateMember
import Amazonka.ManagedBlockChain.UpdateNode
import Amazonka.ManagedBlockChain.VoteOnProposal

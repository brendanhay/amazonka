{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ManagedBlockChain.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Lens
  ( -- * Operations

    -- ** CreateMember
    createMember_clientRequestToken,
    createMember_invitationId,
    createMember_networkId,
    createMember_memberConfiguration,
    createMemberResponse_memberId,
    createMemberResponse_httpStatus,

    -- ** ListNetworks
    listNetworks_status,
    listNetworks_framework,
    listNetworks_nextToken,
    listNetworks_name,
    listNetworks_maxResults,
    listNetworksResponse_networks,
    listNetworksResponse_nextToken,
    listNetworksResponse_httpStatus,

    -- ** GetProposal
    getProposal_networkId,
    getProposal_proposalId,
    getProposalResponse_proposal,
    getProposalResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** CreateNetwork
    createNetwork_frameworkConfiguration,
    createNetwork_description,
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

    -- ** ListProposals
    listProposals_nextToken,
    listProposals_maxResults,
    listProposals_networkId,
    listProposalsResponse_proposals,
    listProposalsResponse_nextToken,
    listProposalsResponse_httpStatus,

    -- ** ListInvitations
    listInvitations_nextToken,
    listInvitations_maxResults,
    listInvitationsResponse_invitations,
    listInvitationsResponse_nextToken,
    listInvitationsResponse_httpStatus,

    -- ** CreateProposal
    createProposal_description,
    createProposal_tags,
    createProposal_clientRequestToken,
    createProposal_networkId,
    createProposal_memberId,
    createProposal_actions,
    createProposalResponse_proposalId,
    createProposalResponse_httpStatus,

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

    -- ** UpdateMember
    updateMember_logPublishingConfiguration,
    updateMember_networkId,
    updateMember_memberId,
    updateMemberResponse_httpStatus,

    -- ** DeleteMember
    deleteMember_networkId,
    deleteMember_memberId,
    deleteMemberResponse_httpStatus,

    -- ** ListMembers
    listMembers_status,
    listMembers_nextToken,
    listMembers_name,
    listMembers_isOwned,
    listMembers_maxResults,
    listMembers_networkId,
    listMembersResponse_members,
    listMembersResponse_nextToken,
    listMembersResponse_httpStatus,

    -- ** CreateNode
    createNode_memberId,
    createNode_tags,
    createNode_clientRequestToken,
    createNode_networkId,
    createNode_nodeConfiguration,
    createNodeResponse_nodeId,
    createNodeResponse_httpStatus,

    -- ** ListProposalVotes
    listProposalVotes_nextToken,
    listProposalVotes_maxResults,
    listProposalVotes_networkId,
    listProposalVotes_proposalId,
    listProposalVotesResponse_nextToken,
    listProposalVotesResponse_proposalVotes,
    listProposalVotesResponse_httpStatus,

    -- ** VoteOnProposal
    voteOnProposal_networkId,
    voteOnProposal_proposalId,
    voteOnProposal_voterMemberId,
    voteOnProposal_vote,
    voteOnProposalResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** GetMember
    getMember_networkId,
    getMember_memberId,
    getMemberResponse_member,
    getMemberResponse_httpStatus,

    -- ** RejectInvitation
    rejectInvitation_invitationId,
    rejectInvitationResponse_httpStatus,

    -- ** UpdateNode
    updateNode_logPublishingConfiguration,
    updateNode_memberId,
    updateNode_networkId,
    updateNode_nodeId,
    updateNodeResponse_httpStatus,

    -- ** DeleteNode
    deleteNode_memberId,
    deleteNode_networkId,
    deleteNode_nodeId,
    deleteNodeResponse_httpStatus,

    -- ** ListNodes
    listNodes_status,
    listNodes_memberId,
    listNodes_nextToken,
    listNodes_maxResults,
    listNodes_networkId,
    listNodesResponse_nextToken,
    listNodesResponse_nodes,
    listNodesResponse_httpStatus,

    -- * Types

    -- ** ApprovalThresholdPolicy
    approvalThresholdPolicy_thresholdPercentage,
    approvalThresholdPolicy_thresholdComparator,
    approvalThresholdPolicy_proposalDurationInHours,

    -- ** Invitation
    invitation_status,
    invitation_arn,
    invitation_invitationId,
    invitation_expirationDate,
    invitation_networkSummary,
    invitation_creationDate,

    -- ** InviteAction
    inviteAction_principal,

    -- ** LogConfiguration
    logConfiguration_enabled,

    -- ** LogConfigurations
    logConfigurations_cloudwatch,

    -- ** Member
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

    -- ** MemberConfiguration
    memberConfiguration_kmsKeyArn,
    memberConfiguration_logPublishingConfiguration,
    memberConfiguration_description,
    memberConfiguration_tags,
    memberConfiguration_name,
    memberConfiguration_frameworkConfiguration,

    -- ** MemberFabricAttributes
    memberFabricAttributes_caEndpoint,
    memberFabricAttributes_adminUsername,

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
    memberSummary_status,
    memberSummary_arn,
    memberSummary_name,
    memberSummary_id,
    memberSummary_isOwned,
    memberSummary_creationDate,
    memberSummary_description,

    -- ** Network
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

    -- ** NetworkEthereumAttributes
    networkEthereumAttributes_chainId,

    -- ** NetworkFabricAttributes
    networkFabricAttributes_edition,
    networkFabricAttributes_orderingServiceEndpoint,

    -- ** NetworkFabricConfiguration
    networkFabricConfiguration_edition,

    -- ** NetworkFrameworkAttributes
    networkFrameworkAttributes_fabric,
    networkFrameworkAttributes_ethereum,

    -- ** NetworkFrameworkConfiguration
    networkFrameworkConfiguration_fabric,

    -- ** NetworkSummary
    networkSummary_status,
    networkSummary_framework,
    networkSummary_arn,
    networkSummary_frameworkVersion,
    networkSummary_name,
    networkSummary_id,
    networkSummary_creationDate,
    networkSummary_description,

    -- ** Node
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

    -- ** NodeConfiguration
    nodeConfiguration_logPublishingConfiguration,
    nodeConfiguration_stateDB,
    nodeConfiguration_availabilityZone,
    nodeConfiguration_instanceType,

    -- ** NodeEthereumAttributes
    nodeEthereumAttributes_httpEndpoint,
    nodeEthereumAttributes_webSocketEndpoint,

    -- ** NodeFabricAttributes
    nodeFabricAttributes_peerEventEndpoint,
    nodeFabricAttributes_peerEndpoint,

    -- ** NodeFabricLogPublishingConfiguration
    nodeFabricLogPublishingConfiguration_chaincodeLogs,
    nodeFabricLogPublishingConfiguration_peerLogs,

    -- ** NodeFrameworkAttributes
    nodeFrameworkAttributes_fabric,
    nodeFrameworkAttributes_ethereum,

    -- ** NodeLogPublishingConfiguration
    nodeLogPublishingConfiguration_fabric,

    -- ** NodeSummary
    nodeSummary_status,
    nodeSummary_arn,
    nodeSummary_instanceType,
    nodeSummary_availabilityZone,
    nodeSummary_id,
    nodeSummary_creationDate,

    -- ** Proposal
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

    -- ** ProposalActions
    proposalActions_invitations,
    proposalActions_removals,

    -- ** ProposalSummary
    proposalSummary_status,
    proposalSummary_arn,
    proposalSummary_proposedByMemberId,
    proposalSummary_proposalId,
    proposalSummary_proposedByMemberName,
    proposalSummary_expirationDate,
    proposalSummary_creationDate,
    proposalSummary_description,

    -- ** RemoveAction
    removeAction_memberId,

    -- ** VoteSummary
    voteSummary_memberName,
    voteSummary_memberId,
    voteSummary_vote,

    -- ** VotingPolicy
    votingPolicy_approvalThresholdPolicy,
  )
where

import Amazonka.ManagedBlockChain.CreateMember
import Amazonka.ManagedBlockChain.CreateNetwork
import Amazonka.ManagedBlockChain.CreateNode
import Amazonka.ManagedBlockChain.CreateProposal
import Amazonka.ManagedBlockChain.DeleteMember
import Amazonka.ManagedBlockChain.DeleteNode
import Amazonka.ManagedBlockChain.GetMember
import Amazonka.ManagedBlockChain.GetNetwork
import Amazonka.ManagedBlockChain.GetNode
import Amazonka.ManagedBlockChain.GetProposal
import Amazonka.ManagedBlockChain.ListInvitations
import Amazonka.ManagedBlockChain.ListMembers
import Amazonka.ManagedBlockChain.ListNetworks
import Amazonka.ManagedBlockChain.ListNodes
import Amazonka.ManagedBlockChain.ListProposalVotes
import Amazonka.ManagedBlockChain.ListProposals
import Amazonka.ManagedBlockChain.ListTagsForResource
import Amazonka.ManagedBlockChain.RejectInvitation
import Amazonka.ManagedBlockChain.TagResource
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

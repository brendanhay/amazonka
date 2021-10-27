{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ManagedBlockChain.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ManagedBlockChain.Lens
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

import Network.AWS.ManagedBlockChain.CreateMember
import Network.AWS.ManagedBlockChain.CreateNetwork
import Network.AWS.ManagedBlockChain.CreateNode
import Network.AWS.ManagedBlockChain.CreateProposal
import Network.AWS.ManagedBlockChain.DeleteMember
import Network.AWS.ManagedBlockChain.DeleteNode
import Network.AWS.ManagedBlockChain.GetMember
import Network.AWS.ManagedBlockChain.GetNetwork
import Network.AWS.ManagedBlockChain.GetNode
import Network.AWS.ManagedBlockChain.GetProposal
import Network.AWS.ManagedBlockChain.ListInvitations
import Network.AWS.ManagedBlockChain.ListMembers
import Network.AWS.ManagedBlockChain.ListNetworks
import Network.AWS.ManagedBlockChain.ListNodes
import Network.AWS.ManagedBlockChain.ListProposalVotes
import Network.AWS.ManagedBlockChain.ListProposals
import Network.AWS.ManagedBlockChain.ListTagsForResource
import Network.AWS.ManagedBlockChain.RejectInvitation
import Network.AWS.ManagedBlockChain.TagResource
import Network.AWS.ManagedBlockChain.Types.ApprovalThresholdPolicy
import Network.AWS.ManagedBlockChain.Types.Invitation
import Network.AWS.ManagedBlockChain.Types.InviteAction
import Network.AWS.ManagedBlockChain.Types.LogConfiguration
import Network.AWS.ManagedBlockChain.Types.LogConfigurations
import Network.AWS.ManagedBlockChain.Types.Member
import Network.AWS.ManagedBlockChain.Types.MemberConfiguration
import Network.AWS.ManagedBlockChain.Types.MemberFabricAttributes
import Network.AWS.ManagedBlockChain.Types.MemberFabricConfiguration
import Network.AWS.ManagedBlockChain.Types.MemberFabricLogPublishingConfiguration
import Network.AWS.ManagedBlockChain.Types.MemberFrameworkAttributes
import Network.AWS.ManagedBlockChain.Types.MemberFrameworkConfiguration
import Network.AWS.ManagedBlockChain.Types.MemberLogPublishingConfiguration
import Network.AWS.ManagedBlockChain.Types.MemberSummary
import Network.AWS.ManagedBlockChain.Types.Network
import Network.AWS.ManagedBlockChain.Types.NetworkEthereumAttributes
import Network.AWS.ManagedBlockChain.Types.NetworkFabricAttributes
import Network.AWS.ManagedBlockChain.Types.NetworkFabricConfiguration
import Network.AWS.ManagedBlockChain.Types.NetworkFrameworkAttributes
import Network.AWS.ManagedBlockChain.Types.NetworkFrameworkConfiguration
import Network.AWS.ManagedBlockChain.Types.NetworkSummary
import Network.AWS.ManagedBlockChain.Types.Node
import Network.AWS.ManagedBlockChain.Types.NodeConfiguration
import Network.AWS.ManagedBlockChain.Types.NodeEthereumAttributes
import Network.AWS.ManagedBlockChain.Types.NodeFabricAttributes
import Network.AWS.ManagedBlockChain.Types.NodeFabricLogPublishingConfiguration
import Network.AWS.ManagedBlockChain.Types.NodeFrameworkAttributes
import Network.AWS.ManagedBlockChain.Types.NodeLogPublishingConfiguration
import Network.AWS.ManagedBlockChain.Types.NodeSummary
import Network.AWS.ManagedBlockChain.Types.Proposal
import Network.AWS.ManagedBlockChain.Types.ProposalActions
import Network.AWS.ManagedBlockChain.Types.ProposalSummary
import Network.AWS.ManagedBlockChain.Types.RemoveAction
import Network.AWS.ManagedBlockChain.Types.VoteSummary
import Network.AWS.ManagedBlockChain.Types.VotingPolicy
import Network.AWS.ManagedBlockChain.UntagResource
import Network.AWS.ManagedBlockChain.UpdateMember
import Network.AWS.ManagedBlockChain.UpdateNode
import Network.AWS.ManagedBlockChain.VoteOnProposal

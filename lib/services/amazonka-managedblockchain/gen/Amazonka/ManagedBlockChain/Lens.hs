{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ManagedBlockChain.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
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

    -- ** CreateNetwork
    createNetwork_tags,
    createNetwork_frameworkConfiguration,
    createNetwork_description,
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
    createNode_tags,
    createNode_memberId,
    createNode_clientRequestToken,
    createNode_networkId,
    createNode_nodeConfiguration,
    createNodeResponse_nodeId,
    createNodeResponse_httpStatus,

    -- ** CreateProposal
    createProposal_tags,
    createProposal_description,
    createProposal_clientRequestToken,
    createProposal_networkId,
    createProposal_memberId,
    createProposal_actions,
    createProposalResponse_proposalId,
    createProposalResponse_httpStatus,

    -- ** DeleteMember
    deleteMember_networkId,
    deleteMember_memberId,
    deleteMemberResponse_httpStatus,

    -- ** DeleteNode
    deleteNode_memberId,
    deleteNode_networkId,
    deleteNode_nodeId,
    deleteNodeResponse_httpStatus,

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

    -- ** ListInvitations
    listInvitations_nextToken,
    listInvitations_maxResults,
    listInvitationsResponse_invitations,
    listInvitationsResponse_nextToken,
    listInvitationsResponse_httpStatus,

    -- ** ListMembers
    listMembers_name,
    listMembers_nextToken,
    listMembers_status,
    listMembers_maxResults,
    listMembers_isOwned,
    listMembers_networkId,
    listMembersResponse_nextToken,
    listMembersResponse_members,
    listMembersResponse_httpStatus,

    -- ** ListNetworks
    listNetworks_name,
    listNetworks_nextToken,
    listNetworks_status,
    listNetworks_maxResults,
    listNetworks_framework,
    listNetworksResponse_networks,
    listNetworksResponse_nextToken,
    listNetworksResponse_httpStatus,

    -- ** ListNodes
    listNodes_memberId,
    listNodes_nextToken,
    listNodes_status,
    listNodes_maxResults,
    listNodes_networkId,
    listNodesResponse_nextToken,
    listNodesResponse_nodes,
    listNodesResponse_httpStatus,

    -- ** ListProposalVotes
    listProposalVotes_nextToken,
    listProposalVotes_maxResults,
    listProposalVotes_networkId,
    listProposalVotes_proposalId,
    listProposalVotesResponse_nextToken,
    listProposalVotesResponse_proposalVotes,
    listProposalVotesResponse_httpStatus,

    -- ** ListProposals
    listProposals_nextToken,
    listProposals_maxResults,
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
    updateNode_memberId,
    updateNode_logPublishingConfiguration,
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

    -- ** ApprovalThresholdPolicy
    approvalThresholdPolicy_proposalDurationInHours,
    approvalThresholdPolicy_thresholdPercentage,
    approvalThresholdPolicy_thresholdComparator,

    -- ** Invitation
    invitation_arn,
    invitation_creationDate,
    invitation_status,
    invitation_networkSummary,
    invitation_expirationDate,
    invitation_invitationId,

    -- ** InviteAction
    inviteAction_principal,

    -- ** LogConfiguration
    logConfiguration_enabled,

    -- ** LogConfigurations
    logConfigurations_cloudwatch,

    -- ** Member
    member_tags,
    member_name,
    member_frameworkAttributes,
    member_arn,
    member_creationDate,
    member_status,
    member_id,
    member_description,
    member_kmsKeyArn,
    member_networkId,
    member_logPublishingConfiguration,

    -- ** MemberConfiguration
    memberConfiguration_tags,
    memberConfiguration_description,
    memberConfiguration_kmsKeyArn,
    memberConfiguration_logPublishingConfiguration,
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
    memberSummary_name,
    memberSummary_arn,
    memberSummary_creationDate,
    memberSummary_status,
    memberSummary_id,
    memberSummary_description,
    memberSummary_isOwned,

    -- ** Network
    network_tags,
    network_name,
    network_frameworkAttributes,
    network_vpcEndpointServiceName,
    network_arn,
    network_creationDate,
    network_votingPolicy,
    network_status,
    network_id,
    network_description,
    network_frameworkVersion,
    network_framework,

    -- ** NetworkEthereumAttributes
    networkEthereumAttributes_chainId,

    -- ** NetworkFabricAttributes
    networkFabricAttributes_orderingServiceEndpoint,
    networkFabricAttributes_edition,

    -- ** NetworkFabricConfiguration
    networkFabricConfiguration_edition,

    -- ** NetworkFrameworkAttributes
    networkFrameworkAttributes_fabric,
    networkFrameworkAttributes_ethereum,

    -- ** NetworkFrameworkConfiguration
    networkFrameworkConfiguration_fabric,

    -- ** NetworkSummary
    networkSummary_name,
    networkSummary_arn,
    networkSummary_creationDate,
    networkSummary_status,
    networkSummary_id,
    networkSummary_description,
    networkSummary_frameworkVersion,
    networkSummary_framework,

    -- ** Node
    node_tags,
    node_memberId,
    node_frameworkAttributes,
    node_arn,
    node_creationDate,
    node_status,
    node_availabilityZone,
    node_id,
    node_kmsKeyArn,
    node_instanceType,
    node_networkId,
    node_stateDB,
    node_logPublishingConfiguration,

    -- ** NodeConfiguration
    nodeConfiguration_availabilityZone,
    nodeConfiguration_stateDB,
    nodeConfiguration_logPublishingConfiguration,
    nodeConfiguration_instanceType,

    -- ** NodeEthereumAttributes
    nodeEthereumAttributes_httpEndpoint,
    nodeEthereumAttributes_webSocketEndpoint,

    -- ** NodeFabricAttributes
    nodeFabricAttributes_peerEndpoint,
    nodeFabricAttributes_peerEventEndpoint,

    -- ** NodeFabricLogPublishingConfiguration
    nodeFabricLogPublishingConfiguration_peerLogs,
    nodeFabricLogPublishingConfiguration_chaincodeLogs,

    -- ** NodeFrameworkAttributes
    nodeFrameworkAttributes_fabric,
    nodeFrameworkAttributes_ethereum,

    -- ** NodeLogPublishingConfiguration
    nodeLogPublishingConfiguration_fabric,

    -- ** NodeSummary
    nodeSummary_arn,
    nodeSummary_creationDate,
    nodeSummary_status,
    nodeSummary_availabilityZone,
    nodeSummary_id,
    nodeSummary_instanceType,

    -- ** Proposal
    proposal_tags,
    proposal_proposalId,
    proposal_yesVoteCount,
    proposal_arn,
    proposal_creationDate,
    proposal_status,
    proposal_description,
    proposal_outstandingVoteCount,
    proposal_noVoteCount,
    proposal_proposedByMemberId,
    proposal_proposedByMemberName,
    proposal_networkId,
    proposal_expirationDate,
    proposal_actions,

    -- ** ProposalActions
    proposalActions_invitations,
    proposalActions_removals,

    -- ** ProposalSummary
    proposalSummary_proposalId,
    proposalSummary_arn,
    proposalSummary_creationDate,
    proposalSummary_status,
    proposalSummary_description,
    proposalSummary_proposedByMemberId,
    proposalSummary_proposedByMemberName,
    proposalSummary_expirationDate,

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

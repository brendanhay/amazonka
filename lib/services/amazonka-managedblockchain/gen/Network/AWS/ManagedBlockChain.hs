{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.ManagedBlockChain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-09-24@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Managed Blockchain is a fully managed service for creating and
-- managing blockchain networks using open-source frameworks. Blockchain
-- allows you to build applications where multiple parties can securely and
-- transparently run transactions and share data without the need for a
-- trusted, central authority.
--
-- Managed Blockchain supports the Hyperledger Fabric and Ethereum
-- open-source frameworks. Because of fundamental differences between the
-- frameworks, some API actions or data types may only apply in the context
-- of one framework and not the other. For example, actions related to
-- Hyperledger Fabric network members such as @CreateMember@ and
-- @DeleteMember@ do not apply to Ethereum.
--
-- The description for each action indicates the framework or frameworks to
-- which it applies. Data types and properties that apply only in the
-- context of a particular framework are similarly indicated.
module Amazonka.ManagedBlockChain
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** ResourceLimitExceededException
    _ResourceLimitExceededException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** IllegalActionException
    _IllegalActionException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InternalServiceErrorException
    _InternalServiceErrorException,

    -- ** ResourceNotReadyException
    _ResourceNotReadyException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateMember
    CreateMember (CreateMember'),
    newCreateMember,
    CreateMemberResponse (CreateMemberResponse'),
    newCreateMemberResponse,

    -- ** ListNetworks
    ListNetworks (ListNetworks'),
    newListNetworks,
    ListNetworksResponse (ListNetworksResponse'),
    newListNetworksResponse,

    -- ** GetProposal
    GetProposal (GetProposal'),
    newGetProposal,
    GetProposalResponse (GetProposalResponse'),
    newGetProposalResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** CreateNetwork
    CreateNetwork (CreateNetwork'),
    newCreateNetwork,
    CreateNetworkResponse (CreateNetworkResponse'),
    newCreateNetworkResponse,

    -- ** ListProposals
    ListProposals (ListProposals'),
    newListProposals,
    ListProposalsResponse (ListProposalsResponse'),
    newListProposalsResponse,

    -- ** ListInvitations
    ListInvitations (ListInvitations'),
    newListInvitations,
    ListInvitationsResponse (ListInvitationsResponse'),
    newListInvitationsResponse,

    -- ** CreateProposal
    CreateProposal (CreateProposal'),
    newCreateProposal,
    CreateProposalResponse (CreateProposalResponse'),
    newCreateProposalResponse,

    -- ** GetNetwork
    GetNetwork (GetNetwork'),
    newGetNetwork,
    GetNetworkResponse (GetNetworkResponse'),
    newGetNetworkResponse,

    -- ** GetNode
    GetNode (GetNode'),
    newGetNode,
    GetNodeResponse (GetNodeResponse'),
    newGetNodeResponse,

    -- ** UpdateMember
    UpdateMember (UpdateMember'),
    newUpdateMember,
    UpdateMemberResponse (UpdateMemberResponse'),
    newUpdateMemberResponse,

    -- ** DeleteMember
    DeleteMember (DeleteMember'),
    newDeleteMember,
    DeleteMemberResponse (DeleteMemberResponse'),
    newDeleteMemberResponse,

    -- ** ListMembers
    ListMembers (ListMembers'),
    newListMembers,
    ListMembersResponse (ListMembersResponse'),
    newListMembersResponse,

    -- ** CreateNode
    CreateNode (CreateNode'),
    newCreateNode,
    CreateNodeResponse (CreateNodeResponse'),
    newCreateNodeResponse,

    -- ** ListProposalVotes
    ListProposalVotes (ListProposalVotes'),
    newListProposalVotes,
    ListProposalVotesResponse (ListProposalVotesResponse'),
    newListProposalVotesResponse,

    -- ** VoteOnProposal
    VoteOnProposal (VoteOnProposal'),
    newVoteOnProposal,
    VoteOnProposalResponse (VoteOnProposalResponse'),
    newVoteOnProposalResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** GetMember
    GetMember (GetMember'),
    newGetMember,
    GetMemberResponse (GetMemberResponse'),
    newGetMemberResponse,

    -- ** RejectInvitation
    RejectInvitation (RejectInvitation'),
    newRejectInvitation,
    RejectInvitationResponse (RejectInvitationResponse'),
    newRejectInvitationResponse,

    -- ** UpdateNode
    UpdateNode (UpdateNode'),
    newUpdateNode,
    UpdateNodeResponse (UpdateNodeResponse'),
    newUpdateNodeResponse,

    -- ** DeleteNode
    DeleteNode (DeleteNode'),
    newDeleteNode,
    DeleteNodeResponse (DeleteNodeResponse'),
    newDeleteNodeResponse,

    -- ** ListNodes
    ListNodes (ListNodes'),
    newListNodes,
    ListNodesResponse (ListNodesResponse'),
    newListNodesResponse,

    -- * Types

    -- ** Edition
    Edition (..),

    -- ** Framework
    Framework (..),

    -- ** InvitationStatus
    InvitationStatus (..),

    -- ** MemberStatus
    MemberStatus (..),

    -- ** NetworkStatus
    NetworkStatus (..),

    -- ** NodeStatus
    NodeStatus (..),

    -- ** ProposalStatus
    ProposalStatus (..),

    -- ** StateDBType
    StateDBType (..),

    -- ** ThresholdComparator
    ThresholdComparator (..),

    -- ** VoteValue
    VoteValue (..),

    -- ** ApprovalThresholdPolicy
    ApprovalThresholdPolicy (ApprovalThresholdPolicy'),
    newApprovalThresholdPolicy,

    -- ** Invitation
    Invitation (Invitation'),
    newInvitation,

    -- ** InviteAction
    InviteAction (InviteAction'),
    newInviteAction,

    -- ** LogConfiguration
    LogConfiguration (LogConfiguration'),
    newLogConfiguration,

    -- ** LogConfigurations
    LogConfigurations (LogConfigurations'),
    newLogConfigurations,

    -- ** Member
    Member (Member'),
    newMember,

    -- ** MemberConfiguration
    MemberConfiguration (MemberConfiguration'),
    newMemberConfiguration,

    -- ** MemberFabricAttributes
    MemberFabricAttributes (MemberFabricAttributes'),
    newMemberFabricAttributes,

    -- ** MemberFabricConfiguration
    MemberFabricConfiguration (MemberFabricConfiguration'),
    newMemberFabricConfiguration,

    -- ** MemberFabricLogPublishingConfiguration
    MemberFabricLogPublishingConfiguration (MemberFabricLogPublishingConfiguration'),
    newMemberFabricLogPublishingConfiguration,

    -- ** MemberFrameworkAttributes
    MemberFrameworkAttributes (MemberFrameworkAttributes'),
    newMemberFrameworkAttributes,

    -- ** MemberFrameworkConfiguration
    MemberFrameworkConfiguration (MemberFrameworkConfiguration'),
    newMemberFrameworkConfiguration,

    -- ** MemberLogPublishingConfiguration
    MemberLogPublishingConfiguration (MemberLogPublishingConfiguration'),
    newMemberLogPublishingConfiguration,

    -- ** MemberSummary
    MemberSummary (MemberSummary'),
    newMemberSummary,

    -- ** Network
    Network (Network'),
    newNetwork,

    -- ** NetworkEthereumAttributes
    NetworkEthereumAttributes (NetworkEthereumAttributes'),
    newNetworkEthereumAttributes,

    -- ** NetworkFabricAttributes
    NetworkFabricAttributes (NetworkFabricAttributes'),
    newNetworkFabricAttributes,

    -- ** NetworkFabricConfiguration
    NetworkFabricConfiguration (NetworkFabricConfiguration'),
    newNetworkFabricConfiguration,

    -- ** NetworkFrameworkAttributes
    NetworkFrameworkAttributes (NetworkFrameworkAttributes'),
    newNetworkFrameworkAttributes,

    -- ** NetworkFrameworkConfiguration
    NetworkFrameworkConfiguration (NetworkFrameworkConfiguration'),
    newNetworkFrameworkConfiguration,

    -- ** NetworkSummary
    NetworkSummary (NetworkSummary'),
    newNetworkSummary,

    -- ** Node
    Node (Node'),
    newNode,

    -- ** NodeConfiguration
    NodeConfiguration (NodeConfiguration'),
    newNodeConfiguration,

    -- ** NodeEthereumAttributes
    NodeEthereumAttributes (NodeEthereumAttributes'),
    newNodeEthereumAttributes,

    -- ** NodeFabricAttributes
    NodeFabricAttributes (NodeFabricAttributes'),
    newNodeFabricAttributes,

    -- ** NodeFabricLogPublishingConfiguration
    NodeFabricLogPublishingConfiguration (NodeFabricLogPublishingConfiguration'),
    newNodeFabricLogPublishingConfiguration,

    -- ** NodeFrameworkAttributes
    NodeFrameworkAttributes (NodeFrameworkAttributes'),
    newNodeFrameworkAttributes,

    -- ** NodeLogPublishingConfiguration
    NodeLogPublishingConfiguration (NodeLogPublishingConfiguration'),
    newNodeLogPublishingConfiguration,

    -- ** NodeSummary
    NodeSummary (NodeSummary'),
    newNodeSummary,

    -- ** Proposal
    Proposal (Proposal'),
    newProposal,

    -- ** ProposalActions
    ProposalActions (ProposalActions'),
    newProposalActions,

    -- ** ProposalSummary
    ProposalSummary (ProposalSummary'),
    newProposalSummary,

    -- ** RemoveAction
    RemoveAction (RemoveAction'),
    newRemoveAction,

    -- ** VoteSummary
    VoteSummary (VoteSummary'),
    newVoteSummary,

    -- ** VotingPolicy
    VotingPolicy (VotingPolicy'),
    newVotingPolicy,
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
import Amazonka.ManagedBlockChain.Lens
import Amazonka.ManagedBlockChain.ListInvitations
import Amazonka.ManagedBlockChain.ListMembers
import Amazonka.ManagedBlockChain.ListNetworks
import Amazonka.ManagedBlockChain.ListNodes
import Amazonka.ManagedBlockChain.ListProposalVotes
import Amazonka.ManagedBlockChain.ListProposals
import Amazonka.ManagedBlockChain.ListTagsForResource
import Amazonka.ManagedBlockChain.RejectInvitation
import Amazonka.ManagedBlockChain.TagResource
import Amazonka.ManagedBlockChain.Types
import Amazonka.ManagedBlockChain.UntagResource
import Amazonka.ManagedBlockChain.UpdateMember
import Amazonka.ManagedBlockChain.UpdateNode
import Amazonka.ManagedBlockChain.VoteOnProposal
import Amazonka.ManagedBlockChain.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ManagedBlockChain'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.

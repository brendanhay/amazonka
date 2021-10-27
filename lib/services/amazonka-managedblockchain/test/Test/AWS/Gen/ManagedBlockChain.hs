{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ManagedBlockChain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.ManagedBlockChain where

import Data.Proxy
import Network.AWS.ManagedBlockChain
import Test.AWS.Fixture
import Test.AWS.ManagedBlockChain.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateMember $
--             newCreateMember
--
--         , requestListNetworks $
--             newListNetworks
--
--         , requestGetProposal $
--             newGetProposal
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestCreateNetwork $
--             newCreateNetwork
--
--         , requestListProposals $
--             newListProposals
--
--         , requestListInvitations $
--             newListInvitations
--
--         , requestCreateProposal $
--             newCreateProposal
--
--         , requestGetNetwork $
--             newGetNetwork
--
--         , requestGetNode $
--             newGetNode
--
--         , requestUpdateMember $
--             newUpdateMember
--
--         , requestDeleteMember $
--             newDeleteMember
--
--         , requestListMembers $
--             newListMembers
--
--         , requestCreateNode $
--             newCreateNode
--
--         , requestListProposalVotes $
--             newListProposalVotes
--
--         , requestVoteOnProposal $
--             newVoteOnProposal
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestGetMember $
--             newGetMember
--
--         , requestRejectInvitation $
--             newRejectInvitation
--
--         , requestUpdateNode $
--             newUpdateNode
--
--         , requestDeleteNode $
--             newDeleteNode
--
--         , requestListNodes $
--             newListNodes
--
--           ]

--     , testGroup "response"
--         [ responseCreateMember $
--             newCreateMemberResponse
--
--         , responseListNetworks $
--             newListNetworksResponse
--
--         , responseGetProposal $
--             newGetProposalResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseCreateNetwork $
--             newCreateNetworkResponse
--
--         , responseListProposals $
--             newListProposalsResponse
--
--         , responseListInvitations $
--             newListInvitationsResponse
--
--         , responseCreateProposal $
--             newCreateProposalResponse
--
--         , responseGetNetwork $
--             newGetNetworkResponse
--
--         , responseGetNode $
--             newGetNodeResponse
--
--         , responseUpdateMember $
--             newUpdateMemberResponse
--
--         , responseDeleteMember $
--             newDeleteMemberResponse
--
--         , responseListMembers $
--             newListMembersResponse
--
--         , responseCreateNode $
--             newCreateNodeResponse
--
--         , responseListProposalVotes $
--             newListProposalVotesResponse
--
--         , responseVoteOnProposal $
--             newVoteOnProposalResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseGetMember $
--             newGetMemberResponse
--
--         , responseRejectInvitation $
--             newRejectInvitationResponse
--
--         , responseUpdateNode $
--             newUpdateNodeResponse
--
--         , responseDeleteNode $
--             newDeleteNodeResponse
--
--         , responseListNodes $
--             newListNodesResponse
--
--           ]
--     ]

-- Requests

requestCreateMember :: CreateMember -> TestTree
requestCreateMember =
  req
    "CreateMember"
    "fixture/CreateMember.yaml"

requestListNetworks :: ListNetworks -> TestTree
requestListNetworks =
  req
    "ListNetworks"
    "fixture/ListNetworks.yaml"

requestGetProposal :: GetProposal -> TestTree
requestGetProposal =
  req
    "GetProposal"
    "fixture/GetProposal.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestCreateNetwork :: CreateNetwork -> TestTree
requestCreateNetwork =
  req
    "CreateNetwork"
    "fixture/CreateNetwork.yaml"

requestListProposals :: ListProposals -> TestTree
requestListProposals =
  req
    "ListProposals"
    "fixture/ListProposals.yaml"

requestListInvitations :: ListInvitations -> TestTree
requestListInvitations =
  req
    "ListInvitations"
    "fixture/ListInvitations.yaml"

requestCreateProposal :: CreateProposal -> TestTree
requestCreateProposal =
  req
    "CreateProposal"
    "fixture/CreateProposal.yaml"

requestGetNetwork :: GetNetwork -> TestTree
requestGetNetwork =
  req
    "GetNetwork"
    "fixture/GetNetwork.yaml"

requestGetNode :: GetNode -> TestTree
requestGetNode =
  req
    "GetNode"
    "fixture/GetNode.yaml"

requestUpdateMember :: UpdateMember -> TestTree
requestUpdateMember =
  req
    "UpdateMember"
    "fixture/UpdateMember.yaml"

requestDeleteMember :: DeleteMember -> TestTree
requestDeleteMember =
  req
    "DeleteMember"
    "fixture/DeleteMember.yaml"

requestListMembers :: ListMembers -> TestTree
requestListMembers =
  req
    "ListMembers"
    "fixture/ListMembers.yaml"

requestCreateNode :: CreateNode -> TestTree
requestCreateNode =
  req
    "CreateNode"
    "fixture/CreateNode.yaml"

requestListProposalVotes :: ListProposalVotes -> TestTree
requestListProposalVotes =
  req
    "ListProposalVotes"
    "fixture/ListProposalVotes.yaml"

requestVoteOnProposal :: VoteOnProposal -> TestTree
requestVoteOnProposal =
  req
    "VoteOnProposal"
    "fixture/VoteOnProposal.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestGetMember :: GetMember -> TestTree
requestGetMember =
  req
    "GetMember"
    "fixture/GetMember.yaml"

requestRejectInvitation :: RejectInvitation -> TestTree
requestRejectInvitation =
  req
    "RejectInvitation"
    "fixture/RejectInvitation.yaml"

requestUpdateNode :: UpdateNode -> TestTree
requestUpdateNode =
  req
    "UpdateNode"
    "fixture/UpdateNode.yaml"

requestDeleteNode :: DeleteNode -> TestTree
requestDeleteNode =
  req
    "DeleteNode"
    "fixture/DeleteNode.yaml"

requestListNodes :: ListNodes -> TestTree
requestListNodes =
  req
    "ListNodes"
    "fixture/ListNodes.yaml"

-- Responses

responseCreateMember :: CreateMemberResponse -> TestTree
responseCreateMember =
  res
    "CreateMemberResponse"
    "fixture/CreateMemberResponse.proto"
    defaultService
    (Proxy :: Proxy CreateMember)

responseListNetworks :: ListNetworksResponse -> TestTree
responseListNetworks =
  res
    "ListNetworksResponse"
    "fixture/ListNetworksResponse.proto"
    defaultService
    (Proxy :: Proxy ListNetworks)

responseGetProposal :: GetProposalResponse -> TestTree
responseGetProposal =
  res
    "GetProposalResponse"
    "fixture/GetProposalResponse.proto"
    defaultService
    (Proxy :: Proxy GetProposal)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseCreateNetwork :: CreateNetworkResponse -> TestTree
responseCreateNetwork =
  res
    "CreateNetworkResponse"
    "fixture/CreateNetworkResponse.proto"
    defaultService
    (Proxy :: Proxy CreateNetwork)

responseListProposals :: ListProposalsResponse -> TestTree
responseListProposals =
  res
    "ListProposalsResponse"
    "fixture/ListProposalsResponse.proto"
    defaultService
    (Proxy :: Proxy ListProposals)

responseListInvitations :: ListInvitationsResponse -> TestTree
responseListInvitations =
  res
    "ListInvitationsResponse"
    "fixture/ListInvitationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListInvitations)

responseCreateProposal :: CreateProposalResponse -> TestTree
responseCreateProposal =
  res
    "CreateProposalResponse"
    "fixture/CreateProposalResponse.proto"
    defaultService
    (Proxy :: Proxy CreateProposal)

responseGetNetwork :: GetNetworkResponse -> TestTree
responseGetNetwork =
  res
    "GetNetworkResponse"
    "fixture/GetNetworkResponse.proto"
    defaultService
    (Proxy :: Proxy GetNetwork)

responseGetNode :: GetNodeResponse -> TestTree
responseGetNode =
  res
    "GetNodeResponse"
    "fixture/GetNodeResponse.proto"
    defaultService
    (Proxy :: Proxy GetNode)

responseUpdateMember :: UpdateMemberResponse -> TestTree
responseUpdateMember =
  res
    "UpdateMemberResponse"
    "fixture/UpdateMemberResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateMember)

responseDeleteMember :: DeleteMemberResponse -> TestTree
responseDeleteMember =
  res
    "DeleteMemberResponse"
    "fixture/DeleteMemberResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteMember)

responseListMembers :: ListMembersResponse -> TestTree
responseListMembers =
  res
    "ListMembersResponse"
    "fixture/ListMembersResponse.proto"
    defaultService
    (Proxy :: Proxy ListMembers)

responseCreateNode :: CreateNodeResponse -> TestTree
responseCreateNode =
  res
    "CreateNodeResponse"
    "fixture/CreateNodeResponse.proto"
    defaultService
    (Proxy :: Proxy CreateNode)

responseListProposalVotes :: ListProposalVotesResponse -> TestTree
responseListProposalVotes =
  res
    "ListProposalVotesResponse"
    "fixture/ListProposalVotesResponse.proto"
    defaultService
    (Proxy :: Proxy ListProposalVotes)

responseVoteOnProposal :: VoteOnProposalResponse -> TestTree
responseVoteOnProposal =
  res
    "VoteOnProposalResponse"
    "fixture/VoteOnProposalResponse.proto"
    defaultService
    (Proxy :: Proxy VoteOnProposal)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseGetMember :: GetMemberResponse -> TestTree
responseGetMember =
  res
    "GetMemberResponse"
    "fixture/GetMemberResponse.proto"
    defaultService
    (Proxy :: Proxy GetMember)

responseRejectInvitation :: RejectInvitationResponse -> TestTree
responseRejectInvitation =
  res
    "RejectInvitationResponse"
    "fixture/RejectInvitationResponse.proto"
    defaultService
    (Proxy :: Proxy RejectInvitation)

responseUpdateNode :: UpdateNodeResponse -> TestTree
responseUpdateNode =
  res
    "UpdateNodeResponse"
    "fixture/UpdateNodeResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateNode)

responseDeleteNode :: DeleteNodeResponse -> TestTree
responseDeleteNode =
  res
    "DeleteNodeResponse"
    "fixture/DeleteNodeResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteNode)

responseListNodes :: ListNodesResponse -> TestTree
responseListNodes =
  res
    "ListNodesResponse"
    "fixture/ListNodesResponse.proto"
    defaultService
    (Proxy :: Proxy ListNodes)

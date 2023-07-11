{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ManagedBlockChain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ManagedBlockChain where

import Amazonka.ManagedBlockChain
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.ManagedBlockChain.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateAccessor $
--             newCreateAccessor
--
--         , requestCreateMember $
--             newCreateMember
--
--         , requestCreateNetwork $
--             newCreateNetwork
--
--         , requestCreateNode $
--             newCreateNode
--
--         , requestCreateProposal $
--             newCreateProposal
--
--         , requestDeleteAccessor $
--             newDeleteAccessor
--
--         , requestDeleteMember $
--             newDeleteMember
--
--         , requestDeleteNode $
--             newDeleteNode
--
--         , requestGetAccessor $
--             newGetAccessor
--
--         , requestGetMember $
--             newGetMember
--
--         , requestGetNetwork $
--             newGetNetwork
--
--         , requestGetNode $
--             newGetNode
--
--         , requestGetProposal $
--             newGetProposal
--
--         , requestListAccessors $
--             newListAccessors
--
--         , requestListInvitations $
--             newListInvitations
--
--         , requestListMembers $
--             newListMembers
--
--         , requestListNetworks $
--             newListNetworks
--
--         , requestListNodes $
--             newListNodes
--
--         , requestListProposalVotes $
--             newListProposalVotes
--
--         , requestListProposals $
--             newListProposals
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestRejectInvitation $
--             newRejectInvitation
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateMember $
--             newUpdateMember
--
--         , requestUpdateNode $
--             newUpdateNode
--
--         , requestVoteOnProposal $
--             newVoteOnProposal
--
--           ]

--     , testGroup "response"
--         [ responseCreateAccessor $
--             newCreateAccessorResponse
--
--         , responseCreateMember $
--             newCreateMemberResponse
--
--         , responseCreateNetwork $
--             newCreateNetworkResponse
--
--         , responseCreateNode $
--             newCreateNodeResponse
--
--         , responseCreateProposal $
--             newCreateProposalResponse
--
--         , responseDeleteAccessor $
--             newDeleteAccessorResponse
--
--         , responseDeleteMember $
--             newDeleteMemberResponse
--
--         , responseDeleteNode $
--             newDeleteNodeResponse
--
--         , responseGetAccessor $
--             newGetAccessorResponse
--
--         , responseGetMember $
--             newGetMemberResponse
--
--         , responseGetNetwork $
--             newGetNetworkResponse
--
--         , responseGetNode $
--             newGetNodeResponse
--
--         , responseGetProposal $
--             newGetProposalResponse
--
--         , responseListAccessors $
--             newListAccessorsResponse
--
--         , responseListInvitations $
--             newListInvitationsResponse
--
--         , responseListMembers $
--             newListMembersResponse
--
--         , responseListNetworks $
--             newListNetworksResponse
--
--         , responseListNodes $
--             newListNodesResponse
--
--         , responseListProposalVotes $
--             newListProposalVotesResponse
--
--         , responseListProposals $
--             newListProposalsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseRejectInvitation $
--             newRejectInvitationResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateMember $
--             newUpdateMemberResponse
--
--         , responseUpdateNode $
--             newUpdateNodeResponse
--
--         , responseVoteOnProposal $
--             newVoteOnProposalResponse
--
--           ]
--     ]

-- Requests

requestCreateAccessor :: CreateAccessor -> TestTree
requestCreateAccessor =
  req
    "CreateAccessor"
    "fixture/CreateAccessor.yaml"

requestCreateMember :: CreateMember -> TestTree
requestCreateMember =
  req
    "CreateMember"
    "fixture/CreateMember.yaml"

requestCreateNetwork :: CreateNetwork -> TestTree
requestCreateNetwork =
  req
    "CreateNetwork"
    "fixture/CreateNetwork.yaml"

requestCreateNode :: CreateNode -> TestTree
requestCreateNode =
  req
    "CreateNode"
    "fixture/CreateNode.yaml"

requestCreateProposal :: CreateProposal -> TestTree
requestCreateProposal =
  req
    "CreateProposal"
    "fixture/CreateProposal.yaml"

requestDeleteAccessor :: DeleteAccessor -> TestTree
requestDeleteAccessor =
  req
    "DeleteAccessor"
    "fixture/DeleteAccessor.yaml"

requestDeleteMember :: DeleteMember -> TestTree
requestDeleteMember =
  req
    "DeleteMember"
    "fixture/DeleteMember.yaml"

requestDeleteNode :: DeleteNode -> TestTree
requestDeleteNode =
  req
    "DeleteNode"
    "fixture/DeleteNode.yaml"

requestGetAccessor :: GetAccessor -> TestTree
requestGetAccessor =
  req
    "GetAccessor"
    "fixture/GetAccessor.yaml"

requestGetMember :: GetMember -> TestTree
requestGetMember =
  req
    "GetMember"
    "fixture/GetMember.yaml"

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

requestGetProposal :: GetProposal -> TestTree
requestGetProposal =
  req
    "GetProposal"
    "fixture/GetProposal.yaml"

requestListAccessors :: ListAccessors -> TestTree
requestListAccessors =
  req
    "ListAccessors"
    "fixture/ListAccessors.yaml"

requestListInvitations :: ListInvitations -> TestTree
requestListInvitations =
  req
    "ListInvitations"
    "fixture/ListInvitations.yaml"

requestListMembers :: ListMembers -> TestTree
requestListMembers =
  req
    "ListMembers"
    "fixture/ListMembers.yaml"

requestListNetworks :: ListNetworks -> TestTree
requestListNetworks =
  req
    "ListNetworks"
    "fixture/ListNetworks.yaml"

requestListNodes :: ListNodes -> TestTree
requestListNodes =
  req
    "ListNodes"
    "fixture/ListNodes.yaml"

requestListProposalVotes :: ListProposalVotes -> TestTree
requestListProposalVotes =
  req
    "ListProposalVotes"
    "fixture/ListProposalVotes.yaml"

requestListProposals :: ListProposals -> TestTree
requestListProposals =
  req
    "ListProposals"
    "fixture/ListProposals.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestRejectInvitation :: RejectInvitation -> TestTree
requestRejectInvitation =
  req
    "RejectInvitation"
    "fixture/RejectInvitation.yaml"

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

requestUpdateMember :: UpdateMember -> TestTree
requestUpdateMember =
  req
    "UpdateMember"
    "fixture/UpdateMember.yaml"

requestUpdateNode :: UpdateNode -> TestTree
requestUpdateNode =
  req
    "UpdateNode"
    "fixture/UpdateNode.yaml"

requestVoteOnProposal :: VoteOnProposal -> TestTree
requestVoteOnProposal =
  req
    "VoteOnProposal"
    "fixture/VoteOnProposal.yaml"

-- Responses

responseCreateAccessor :: CreateAccessorResponse -> TestTree
responseCreateAccessor =
  res
    "CreateAccessorResponse"
    "fixture/CreateAccessorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAccessor)

responseCreateMember :: CreateMemberResponse -> TestTree
responseCreateMember =
  res
    "CreateMemberResponse"
    "fixture/CreateMemberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMember)

responseCreateNetwork :: CreateNetworkResponse -> TestTree
responseCreateNetwork =
  res
    "CreateNetworkResponse"
    "fixture/CreateNetworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNetwork)

responseCreateNode :: CreateNodeResponse -> TestTree
responseCreateNode =
  res
    "CreateNodeResponse"
    "fixture/CreateNodeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNode)

responseCreateProposal :: CreateProposalResponse -> TestTree
responseCreateProposal =
  res
    "CreateProposalResponse"
    "fixture/CreateProposalResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProposal)

responseDeleteAccessor :: DeleteAccessorResponse -> TestTree
responseDeleteAccessor =
  res
    "DeleteAccessorResponse"
    "fixture/DeleteAccessorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAccessor)

responseDeleteMember :: DeleteMemberResponse -> TestTree
responseDeleteMember =
  res
    "DeleteMemberResponse"
    "fixture/DeleteMemberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMember)

responseDeleteNode :: DeleteNodeResponse -> TestTree
responseDeleteNode =
  res
    "DeleteNodeResponse"
    "fixture/DeleteNodeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNode)

responseGetAccessor :: GetAccessorResponse -> TestTree
responseGetAccessor =
  res
    "GetAccessorResponse"
    "fixture/GetAccessorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccessor)

responseGetMember :: GetMemberResponse -> TestTree
responseGetMember =
  res
    "GetMemberResponse"
    "fixture/GetMemberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMember)

responseGetNetwork :: GetNetworkResponse -> TestTree
responseGetNetwork =
  res
    "GetNetworkResponse"
    "fixture/GetNetworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetNetwork)

responseGetNode :: GetNodeResponse -> TestTree
responseGetNode =
  res
    "GetNodeResponse"
    "fixture/GetNodeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetNode)

responseGetProposal :: GetProposalResponse -> TestTree
responseGetProposal =
  res
    "GetProposalResponse"
    "fixture/GetProposalResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetProposal)

responseListAccessors :: ListAccessorsResponse -> TestTree
responseListAccessors =
  res
    "ListAccessorsResponse"
    "fixture/ListAccessorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccessors)

responseListInvitations :: ListInvitationsResponse -> TestTree
responseListInvitations =
  res
    "ListInvitationsResponse"
    "fixture/ListInvitationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInvitations)

responseListMembers :: ListMembersResponse -> TestTree
responseListMembers =
  res
    "ListMembersResponse"
    "fixture/ListMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMembers)

responseListNetworks :: ListNetworksResponse -> TestTree
responseListNetworks =
  res
    "ListNetworksResponse"
    "fixture/ListNetworksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNetworks)

responseListNodes :: ListNodesResponse -> TestTree
responseListNodes =
  res
    "ListNodesResponse"
    "fixture/ListNodesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNodes)

responseListProposalVotes :: ListProposalVotesResponse -> TestTree
responseListProposalVotes =
  res
    "ListProposalVotesResponse"
    "fixture/ListProposalVotesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProposalVotes)

responseListProposals :: ListProposalsResponse -> TestTree
responseListProposals =
  res
    "ListProposalsResponse"
    "fixture/ListProposalsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProposals)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseRejectInvitation :: RejectInvitationResponse -> TestTree
responseRejectInvitation =
  res
    "RejectInvitationResponse"
    "fixture/RejectInvitationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectInvitation)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateMember :: UpdateMemberResponse -> TestTree
responseUpdateMember =
  res
    "UpdateMemberResponse"
    "fixture/UpdateMemberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMember)

responseUpdateNode :: UpdateNodeResponse -> TestTree
responseUpdateNode =
  res
    "UpdateNodeResponse"
    "fixture/UpdateNodeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateNode)

responseVoteOnProposal :: VoteOnProposalResponse -> TestTree
responseVoteOnProposal =
  res
    "VoteOnProposalResponse"
    "fixture/VoteOnProposalResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy VoteOnProposal)

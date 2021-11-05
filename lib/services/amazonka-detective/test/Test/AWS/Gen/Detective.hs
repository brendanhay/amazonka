{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Detective
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Detective where

import Amazonka.Detective
import qualified Data.Proxy as Proxy
import Test.AWS.Detective.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestStartMonitoringMember $
--             newStartMonitoringMember
--
--         , requestDeleteMembers $
--             newDeleteMembers
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDeleteGraph $
--             newDeleteGraph
--
--         , requestListInvitations $
--             newListInvitations
--
--         , requestDisassociateMembership $
--             newDisassociateMembership
--
--         , requestAcceptInvitation $
--             newAcceptInvitation
--
--         , requestListMembers $
--             newListMembers
--
--         , requestCreateMembers $
--             newCreateMembers
--
--         , requestGetMembers $
--             newGetMembers
--
--         , requestListGraphs $
--             newListGraphs
--
--         , requestTagResource $
--             newTagResource
--
--         , requestCreateGraph $
--             newCreateGraph
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestRejectInvitation $
--             newRejectInvitation
--
--           ]

--     , testGroup "response"
--         [ responseStartMonitoringMember $
--             newStartMonitoringMemberResponse
--
--         , responseDeleteMembers $
--             newDeleteMembersResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDeleteGraph $
--             newDeleteGraphResponse
--
--         , responseListInvitations $
--             newListInvitationsResponse
--
--         , responseDisassociateMembership $
--             newDisassociateMembershipResponse
--
--         , responseAcceptInvitation $
--             newAcceptInvitationResponse
--
--         , responseListMembers $
--             newListMembersResponse
--
--         , responseCreateMembers $
--             newCreateMembersResponse
--
--         , responseGetMembers $
--             newGetMembersResponse
--
--         , responseListGraphs $
--             newListGraphsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseCreateGraph $
--             newCreateGraphResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseRejectInvitation $
--             newRejectInvitationResponse
--
--           ]
--     ]

-- Requests

requestStartMonitoringMember :: StartMonitoringMember -> TestTree
requestStartMonitoringMember =
  req
    "StartMonitoringMember"
    "fixture/StartMonitoringMember.yaml"

requestDeleteMembers :: DeleteMembers -> TestTree
requestDeleteMembers =
  req
    "DeleteMembers"
    "fixture/DeleteMembers.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDeleteGraph :: DeleteGraph -> TestTree
requestDeleteGraph =
  req
    "DeleteGraph"
    "fixture/DeleteGraph.yaml"

requestListInvitations :: ListInvitations -> TestTree
requestListInvitations =
  req
    "ListInvitations"
    "fixture/ListInvitations.yaml"

requestDisassociateMembership :: DisassociateMembership -> TestTree
requestDisassociateMembership =
  req
    "DisassociateMembership"
    "fixture/DisassociateMembership.yaml"

requestAcceptInvitation :: AcceptInvitation -> TestTree
requestAcceptInvitation =
  req
    "AcceptInvitation"
    "fixture/AcceptInvitation.yaml"

requestListMembers :: ListMembers -> TestTree
requestListMembers =
  req
    "ListMembers"
    "fixture/ListMembers.yaml"

requestCreateMembers :: CreateMembers -> TestTree
requestCreateMembers =
  req
    "CreateMembers"
    "fixture/CreateMembers.yaml"

requestGetMembers :: GetMembers -> TestTree
requestGetMembers =
  req
    "GetMembers"
    "fixture/GetMembers.yaml"

requestListGraphs :: ListGraphs -> TestTree
requestListGraphs =
  req
    "ListGraphs"
    "fixture/ListGraphs.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestCreateGraph :: CreateGraph -> TestTree
requestCreateGraph =
  req
    "CreateGraph"
    "fixture/CreateGraph.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestRejectInvitation :: RejectInvitation -> TestTree
requestRejectInvitation =
  req
    "RejectInvitation"
    "fixture/RejectInvitation.yaml"

-- Responses

responseStartMonitoringMember :: StartMonitoringMemberResponse -> TestTree
responseStartMonitoringMember =
  res
    "StartMonitoringMemberResponse"
    "fixture/StartMonitoringMemberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartMonitoringMember)

responseDeleteMembers :: DeleteMembersResponse -> TestTree
responseDeleteMembers =
  res
    "DeleteMembersResponse"
    "fixture/DeleteMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMembers)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseDeleteGraph :: DeleteGraphResponse -> TestTree
responseDeleteGraph =
  res
    "DeleteGraphResponse"
    "fixture/DeleteGraphResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGraph)

responseListInvitations :: ListInvitationsResponse -> TestTree
responseListInvitations =
  res
    "ListInvitationsResponse"
    "fixture/ListInvitationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInvitations)

responseDisassociateMembership :: DisassociateMembershipResponse -> TestTree
responseDisassociateMembership =
  res
    "DisassociateMembershipResponse"
    "fixture/DisassociateMembershipResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateMembership)

responseAcceptInvitation :: AcceptInvitationResponse -> TestTree
responseAcceptInvitation =
  res
    "AcceptInvitationResponse"
    "fixture/AcceptInvitationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptInvitation)

responseListMembers :: ListMembersResponse -> TestTree
responseListMembers =
  res
    "ListMembersResponse"
    "fixture/ListMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMembers)

responseCreateMembers :: CreateMembersResponse -> TestTree
responseCreateMembers =
  res
    "CreateMembersResponse"
    "fixture/CreateMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMembers)

responseGetMembers :: GetMembersResponse -> TestTree
responseGetMembers =
  res
    "GetMembersResponse"
    "fixture/GetMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMembers)

responseListGraphs :: ListGraphsResponse -> TestTree
responseListGraphs =
  res
    "ListGraphsResponse"
    "fixture/ListGraphsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGraphs)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseCreateGraph :: CreateGraphResponse -> TestTree
responseCreateGraph =
  res
    "CreateGraphResponse"
    "fixture/CreateGraphResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGraph)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseRejectInvitation :: RejectInvitationResponse -> TestTree
responseRejectInvitation =
  res
    "RejectInvitationResponse"
    "fixture/RejectInvitationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectInvitation)

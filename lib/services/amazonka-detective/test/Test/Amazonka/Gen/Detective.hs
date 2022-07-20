{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Detective
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Detective where

import Amazonka.Detective
import qualified Data.Proxy as Proxy
import Test.Amazonka.Detective.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAcceptInvitation $
--             newAcceptInvitation
--
--         , requestCreateGraph $
--             newCreateGraph
--
--         , requestCreateMembers $
--             newCreateMembers
--
--         , requestDeleteGraph $
--             newDeleteGraph
--
--         , requestDeleteMembers $
--             newDeleteMembers
--
--         , requestDisassociateMembership $
--             newDisassociateMembership
--
--         , requestGetMembers $
--             newGetMembers
--
--         , requestListGraphs $
--             newListGraphs
--
--         , requestListInvitations $
--             newListInvitations
--
--         , requestListMembers $
--             newListMembers
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestRejectInvitation $
--             newRejectInvitation
--
--         , requestStartMonitoringMember $
--             newStartMonitoringMember
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--           ]

--     , testGroup "response"
--         [ responseAcceptInvitation $
--             newAcceptInvitationResponse
--
--         , responseCreateGraph $
--             newCreateGraphResponse
--
--         , responseCreateMembers $
--             newCreateMembersResponse
--
--         , responseDeleteGraph $
--             newDeleteGraphResponse
--
--         , responseDeleteMembers $
--             newDeleteMembersResponse
--
--         , responseDisassociateMembership $
--             newDisassociateMembershipResponse
--
--         , responseGetMembers $
--             newGetMembersResponse
--
--         , responseListGraphs $
--             newListGraphsResponse
--
--         , responseListInvitations $
--             newListInvitationsResponse
--
--         , responseListMembers $
--             newListMembersResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseRejectInvitation $
--             newRejectInvitationResponse
--
--         , responseStartMonitoringMember $
--             newStartMonitoringMemberResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--           ]
--     ]

-- Requests

requestAcceptInvitation :: AcceptInvitation -> TestTree
requestAcceptInvitation =
  req
    "AcceptInvitation"
    "fixture/AcceptInvitation.yaml"

requestCreateGraph :: CreateGraph -> TestTree
requestCreateGraph =
  req
    "CreateGraph"
    "fixture/CreateGraph.yaml"

requestCreateMembers :: CreateMembers -> TestTree
requestCreateMembers =
  req
    "CreateMembers"
    "fixture/CreateMembers.yaml"

requestDeleteGraph :: DeleteGraph -> TestTree
requestDeleteGraph =
  req
    "DeleteGraph"
    "fixture/DeleteGraph.yaml"

requestDeleteMembers :: DeleteMembers -> TestTree
requestDeleteMembers =
  req
    "DeleteMembers"
    "fixture/DeleteMembers.yaml"

requestDisassociateMembership :: DisassociateMembership -> TestTree
requestDisassociateMembership =
  req
    "DisassociateMembership"
    "fixture/DisassociateMembership.yaml"

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

requestStartMonitoringMember :: StartMonitoringMember -> TestTree
requestStartMonitoringMember =
  req
    "StartMonitoringMember"
    "fixture/StartMonitoringMember.yaml"

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

-- Responses

responseAcceptInvitation :: AcceptInvitationResponse -> TestTree
responseAcceptInvitation =
  res
    "AcceptInvitationResponse"
    "fixture/AcceptInvitationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptInvitation)

responseCreateGraph :: CreateGraphResponse -> TestTree
responseCreateGraph =
  res
    "CreateGraphResponse"
    "fixture/CreateGraphResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGraph)

responseCreateMembers :: CreateMembersResponse -> TestTree
responseCreateMembers =
  res
    "CreateMembersResponse"
    "fixture/CreateMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMembers)

responseDeleteGraph :: DeleteGraphResponse -> TestTree
responseDeleteGraph =
  res
    "DeleteGraphResponse"
    "fixture/DeleteGraphResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGraph)

responseDeleteMembers :: DeleteMembersResponse -> TestTree
responseDeleteMembers =
  res
    "DeleteMembersResponse"
    "fixture/DeleteMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMembers)

responseDisassociateMembership :: DisassociateMembershipResponse -> TestTree
responseDisassociateMembership =
  res
    "DisassociateMembershipResponse"
    "fixture/DisassociateMembershipResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateMembership)

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

responseStartMonitoringMember :: StartMonitoringMemberResponse -> TestTree
responseStartMonitoringMember =
  res
    "StartMonitoringMemberResponse"
    "fixture/StartMonitoringMemberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartMonitoringMember)

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

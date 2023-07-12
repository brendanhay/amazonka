{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Detective
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
--         , requestBatchGetGraphMemberDatasources $
--             newBatchGetGraphMemberDatasources
--
--         , requestBatchGetMembershipDatasources $
--             newBatchGetMembershipDatasources
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
--         , requestDescribeOrganizationConfiguration $
--             newDescribeOrganizationConfiguration
--
--         , requestDisableOrganizationAdminAccount $
--             newDisableOrganizationAdminAccount
--
--         , requestDisassociateMembership $
--             newDisassociateMembership
--
--         , requestEnableOrganizationAdminAccount $
--             newEnableOrganizationAdminAccount
--
--         , requestGetMembers $
--             newGetMembers
--
--         , requestListDatasourcePackages $
--             newListDatasourcePackages
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
--         , requestListOrganizationAdminAccounts $
--             newListOrganizationAdminAccounts
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
--         , requestUpdateDatasourcePackages $
--             newUpdateDatasourcePackages
--
--         , requestUpdateOrganizationConfiguration $
--             newUpdateOrganizationConfiguration
--
--           ]

--     , testGroup "response"
--         [ responseAcceptInvitation $
--             newAcceptInvitationResponse
--
--         , responseBatchGetGraphMemberDatasources $
--             newBatchGetGraphMemberDatasourcesResponse
--
--         , responseBatchGetMembershipDatasources $
--             newBatchGetMembershipDatasourcesResponse
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
--         , responseDescribeOrganizationConfiguration $
--             newDescribeOrganizationConfigurationResponse
--
--         , responseDisableOrganizationAdminAccount $
--             newDisableOrganizationAdminAccountResponse
--
--         , responseDisassociateMembership $
--             newDisassociateMembershipResponse
--
--         , responseEnableOrganizationAdminAccount $
--             newEnableOrganizationAdminAccountResponse
--
--         , responseGetMembers $
--             newGetMembersResponse
--
--         , responseListDatasourcePackages $
--             newListDatasourcePackagesResponse
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
--         , responseListOrganizationAdminAccounts $
--             newListOrganizationAdminAccountsResponse
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
--         , responseUpdateDatasourcePackages $
--             newUpdateDatasourcePackagesResponse
--
--         , responseUpdateOrganizationConfiguration $
--             newUpdateOrganizationConfigurationResponse
--
--           ]
--     ]

-- Requests

requestAcceptInvitation :: AcceptInvitation -> TestTree
requestAcceptInvitation =
  req
    "AcceptInvitation"
    "fixture/AcceptInvitation.yaml"

requestBatchGetGraphMemberDatasources :: BatchGetGraphMemberDatasources -> TestTree
requestBatchGetGraphMemberDatasources =
  req
    "BatchGetGraphMemberDatasources"
    "fixture/BatchGetGraphMemberDatasources.yaml"

requestBatchGetMembershipDatasources :: BatchGetMembershipDatasources -> TestTree
requestBatchGetMembershipDatasources =
  req
    "BatchGetMembershipDatasources"
    "fixture/BatchGetMembershipDatasources.yaml"

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

requestDescribeOrganizationConfiguration :: DescribeOrganizationConfiguration -> TestTree
requestDescribeOrganizationConfiguration =
  req
    "DescribeOrganizationConfiguration"
    "fixture/DescribeOrganizationConfiguration.yaml"

requestDisableOrganizationAdminAccount :: DisableOrganizationAdminAccount -> TestTree
requestDisableOrganizationAdminAccount =
  req
    "DisableOrganizationAdminAccount"
    "fixture/DisableOrganizationAdminAccount.yaml"

requestDisassociateMembership :: DisassociateMembership -> TestTree
requestDisassociateMembership =
  req
    "DisassociateMembership"
    "fixture/DisassociateMembership.yaml"

requestEnableOrganizationAdminAccount :: EnableOrganizationAdminAccount -> TestTree
requestEnableOrganizationAdminAccount =
  req
    "EnableOrganizationAdminAccount"
    "fixture/EnableOrganizationAdminAccount.yaml"

requestGetMembers :: GetMembers -> TestTree
requestGetMembers =
  req
    "GetMembers"
    "fixture/GetMembers.yaml"

requestListDatasourcePackages :: ListDatasourcePackages -> TestTree
requestListDatasourcePackages =
  req
    "ListDatasourcePackages"
    "fixture/ListDatasourcePackages.yaml"

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

requestListOrganizationAdminAccounts :: ListOrganizationAdminAccounts -> TestTree
requestListOrganizationAdminAccounts =
  req
    "ListOrganizationAdminAccounts"
    "fixture/ListOrganizationAdminAccounts.yaml"

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

requestUpdateDatasourcePackages :: UpdateDatasourcePackages -> TestTree
requestUpdateDatasourcePackages =
  req
    "UpdateDatasourcePackages"
    "fixture/UpdateDatasourcePackages.yaml"

requestUpdateOrganizationConfiguration :: UpdateOrganizationConfiguration -> TestTree
requestUpdateOrganizationConfiguration =
  req
    "UpdateOrganizationConfiguration"
    "fixture/UpdateOrganizationConfiguration.yaml"

-- Responses

responseAcceptInvitation :: AcceptInvitationResponse -> TestTree
responseAcceptInvitation =
  res
    "AcceptInvitationResponse"
    "fixture/AcceptInvitationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptInvitation)

responseBatchGetGraphMemberDatasources :: BatchGetGraphMemberDatasourcesResponse -> TestTree
responseBatchGetGraphMemberDatasources =
  res
    "BatchGetGraphMemberDatasourcesResponse"
    "fixture/BatchGetGraphMemberDatasourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetGraphMemberDatasources)

responseBatchGetMembershipDatasources :: BatchGetMembershipDatasourcesResponse -> TestTree
responseBatchGetMembershipDatasources =
  res
    "BatchGetMembershipDatasourcesResponse"
    "fixture/BatchGetMembershipDatasourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetMembershipDatasources)

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

responseDescribeOrganizationConfiguration :: DescribeOrganizationConfigurationResponse -> TestTree
responseDescribeOrganizationConfiguration =
  res
    "DescribeOrganizationConfigurationResponse"
    "fixture/DescribeOrganizationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOrganizationConfiguration)

responseDisableOrganizationAdminAccount :: DisableOrganizationAdminAccountResponse -> TestTree
responseDisableOrganizationAdminAccount =
  res
    "DisableOrganizationAdminAccountResponse"
    "fixture/DisableOrganizationAdminAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableOrganizationAdminAccount)

responseDisassociateMembership :: DisassociateMembershipResponse -> TestTree
responseDisassociateMembership =
  res
    "DisassociateMembershipResponse"
    "fixture/DisassociateMembershipResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateMembership)

responseEnableOrganizationAdminAccount :: EnableOrganizationAdminAccountResponse -> TestTree
responseEnableOrganizationAdminAccount =
  res
    "EnableOrganizationAdminAccountResponse"
    "fixture/EnableOrganizationAdminAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableOrganizationAdminAccount)

responseGetMembers :: GetMembersResponse -> TestTree
responseGetMembers =
  res
    "GetMembersResponse"
    "fixture/GetMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMembers)

responseListDatasourcePackages :: ListDatasourcePackagesResponse -> TestTree
responseListDatasourcePackages =
  res
    "ListDatasourcePackagesResponse"
    "fixture/ListDatasourcePackagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDatasourcePackages)

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

responseListOrganizationAdminAccounts :: ListOrganizationAdminAccountsResponse -> TestTree
responseListOrganizationAdminAccounts =
  res
    "ListOrganizationAdminAccountsResponse"
    "fixture/ListOrganizationAdminAccountsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOrganizationAdminAccounts)

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

responseUpdateDatasourcePackages :: UpdateDatasourcePackagesResponse -> TestTree
responseUpdateDatasourcePackages =
  res
    "UpdateDatasourcePackagesResponse"
    "fixture/UpdateDatasourcePackagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDatasourcePackages)

responseUpdateOrganizationConfiguration :: UpdateOrganizationConfigurationResponse -> TestTree
responseUpdateOrganizationConfiguration =
  res
    "UpdateOrganizationConfigurationResponse"
    "fixture/UpdateOrganizationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateOrganizationConfiguration)

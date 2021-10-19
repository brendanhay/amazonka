{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.FMS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.FMS where

import Data.Proxy
import Network.AWS.FMS
import Test.AWS.FMS.Internal
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
--         [ requestListPolicies $
--             newListPolicies
--
--         , requestGetComplianceDetail $
--             newGetComplianceDetail
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestGetNotificationChannel $
--             newGetNotificationChannel
--
--         , requestGetAdminAccount $
--             newGetAdminAccount
--
--         , requestListComplianceStatus $
--             newListComplianceStatus
--
--         , requestGetAppsList $
--             newGetAppsList
--
--         , requestPutPolicy $
--             newPutPolicy
--
--         , requestDeletePolicy $
--             newDeletePolicy
--
--         , requestDisassociateAdminAccount $
--             newDisassociateAdminAccount
--
--         , requestPutNotificationChannel $
--             newPutNotificationChannel
--
--         , requestDeleteNotificationChannel $
--             newDeleteNotificationChannel
--
--         , requestAssociateAdminAccount $
--             newAssociateAdminAccount
--
--         , requestGetViolationDetails $
--             newGetViolationDetails
--
--         , requestListMemberAccounts $
--             newListMemberAccounts
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDeleteProtocolsList $
--             newDeleteProtocolsList
--
--         , requestGetPolicy $
--             newGetPolicy
--
--         , requestListProtocolsLists $
--             newListProtocolsLists
--
--         , requestPutProtocolsList $
--             newPutProtocolsList
--
--         , requestPutAppsList $
--             newPutAppsList
--
--         , requestDeleteAppsList $
--             newDeleteAppsList
--
--         , requestListAppsLists $
--             newListAppsLists
--
--         , requestGetProtocolsList $
--             newGetProtocolsList
--
--         , requestGetProtectionStatus $
--             newGetProtectionStatus
--
--           ]

--     , testGroup "response"
--         [ responseListPolicies $
--             newListPoliciesResponse
--
--         , responseGetComplianceDetail $
--             newGetComplianceDetailResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseGetNotificationChannel $
--             newGetNotificationChannelResponse
--
--         , responseGetAdminAccount $
--             newGetAdminAccountResponse
--
--         , responseListComplianceStatus $
--             newListComplianceStatusResponse
--
--         , responseGetAppsList $
--             newGetAppsListResponse
--
--         , responsePutPolicy $
--             newPutPolicyResponse
--
--         , responseDeletePolicy $
--             newDeletePolicyResponse
--
--         , responseDisassociateAdminAccount $
--             newDisassociateAdminAccountResponse
--
--         , responsePutNotificationChannel $
--             newPutNotificationChannelResponse
--
--         , responseDeleteNotificationChannel $
--             newDeleteNotificationChannelResponse
--
--         , responseAssociateAdminAccount $
--             newAssociateAdminAccountResponse
--
--         , responseGetViolationDetails $
--             newGetViolationDetailsResponse
--
--         , responseListMemberAccounts $
--             newListMemberAccountsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDeleteProtocolsList $
--             newDeleteProtocolsListResponse
--
--         , responseGetPolicy $
--             newGetPolicyResponse
--
--         , responseListProtocolsLists $
--             newListProtocolsListsResponse
--
--         , responsePutProtocolsList $
--             newPutProtocolsListResponse
--
--         , responsePutAppsList $
--             newPutAppsListResponse
--
--         , responseDeleteAppsList $
--             newDeleteAppsListResponse
--
--         , responseListAppsLists $
--             newListAppsListsResponse
--
--         , responseGetProtocolsList $
--             newGetProtocolsListResponse
--
--         , responseGetProtectionStatus $
--             newGetProtectionStatusResponse
--
--           ]
--     ]

-- Requests

requestListPolicies :: ListPolicies -> TestTree
requestListPolicies =
  req
    "ListPolicies"
    "fixture/ListPolicies.yaml"

requestGetComplianceDetail :: GetComplianceDetail -> TestTree
requestGetComplianceDetail =
  req
    "GetComplianceDetail"
    "fixture/GetComplianceDetail.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestGetNotificationChannel :: GetNotificationChannel -> TestTree
requestGetNotificationChannel =
  req
    "GetNotificationChannel"
    "fixture/GetNotificationChannel.yaml"

requestGetAdminAccount :: GetAdminAccount -> TestTree
requestGetAdminAccount =
  req
    "GetAdminAccount"
    "fixture/GetAdminAccount.yaml"

requestListComplianceStatus :: ListComplianceStatus -> TestTree
requestListComplianceStatus =
  req
    "ListComplianceStatus"
    "fixture/ListComplianceStatus.yaml"

requestGetAppsList :: GetAppsList -> TestTree
requestGetAppsList =
  req
    "GetAppsList"
    "fixture/GetAppsList.yaml"

requestPutPolicy :: PutPolicy -> TestTree
requestPutPolicy =
  req
    "PutPolicy"
    "fixture/PutPolicy.yaml"

requestDeletePolicy :: DeletePolicy -> TestTree
requestDeletePolicy =
  req
    "DeletePolicy"
    "fixture/DeletePolicy.yaml"

requestDisassociateAdminAccount :: DisassociateAdminAccount -> TestTree
requestDisassociateAdminAccount =
  req
    "DisassociateAdminAccount"
    "fixture/DisassociateAdminAccount.yaml"

requestPutNotificationChannel :: PutNotificationChannel -> TestTree
requestPutNotificationChannel =
  req
    "PutNotificationChannel"
    "fixture/PutNotificationChannel.yaml"

requestDeleteNotificationChannel :: DeleteNotificationChannel -> TestTree
requestDeleteNotificationChannel =
  req
    "DeleteNotificationChannel"
    "fixture/DeleteNotificationChannel.yaml"

requestAssociateAdminAccount :: AssociateAdminAccount -> TestTree
requestAssociateAdminAccount =
  req
    "AssociateAdminAccount"
    "fixture/AssociateAdminAccount.yaml"

requestGetViolationDetails :: GetViolationDetails -> TestTree
requestGetViolationDetails =
  req
    "GetViolationDetails"
    "fixture/GetViolationDetails.yaml"

requestListMemberAccounts :: ListMemberAccounts -> TestTree
requestListMemberAccounts =
  req
    "ListMemberAccounts"
    "fixture/ListMemberAccounts.yaml"

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

requestDeleteProtocolsList :: DeleteProtocolsList -> TestTree
requestDeleteProtocolsList =
  req
    "DeleteProtocolsList"
    "fixture/DeleteProtocolsList.yaml"

requestGetPolicy :: GetPolicy -> TestTree
requestGetPolicy =
  req
    "GetPolicy"
    "fixture/GetPolicy.yaml"

requestListProtocolsLists :: ListProtocolsLists -> TestTree
requestListProtocolsLists =
  req
    "ListProtocolsLists"
    "fixture/ListProtocolsLists.yaml"

requestPutProtocolsList :: PutProtocolsList -> TestTree
requestPutProtocolsList =
  req
    "PutProtocolsList"
    "fixture/PutProtocolsList.yaml"

requestPutAppsList :: PutAppsList -> TestTree
requestPutAppsList =
  req
    "PutAppsList"
    "fixture/PutAppsList.yaml"

requestDeleteAppsList :: DeleteAppsList -> TestTree
requestDeleteAppsList =
  req
    "DeleteAppsList"
    "fixture/DeleteAppsList.yaml"

requestListAppsLists :: ListAppsLists -> TestTree
requestListAppsLists =
  req
    "ListAppsLists"
    "fixture/ListAppsLists.yaml"

requestGetProtocolsList :: GetProtocolsList -> TestTree
requestGetProtocolsList =
  req
    "GetProtocolsList"
    "fixture/GetProtocolsList.yaml"

requestGetProtectionStatus :: GetProtectionStatus -> TestTree
requestGetProtectionStatus =
  req
    "GetProtectionStatus"
    "fixture/GetProtectionStatus.yaml"

-- Responses

responseListPolicies :: ListPoliciesResponse -> TestTree
responseListPolicies =
  res
    "ListPoliciesResponse"
    "fixture/ListPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy ListPolicies)

responseGetComplianceDetail :: GetComplianceDetailResponse -> TestTree
responseGetComplianceDetail =
  res
    "GetComplianceDetailResponse"
    "fixture/GetComplianceDetailResponse.proto"
    defaultService
    (Proxy :: Proxy GetComplianceDetail)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseGetNotificationChannel :: GetNotificationChannelResponse -> TestTree
responseGetNotificationChannel =
  res
    "GetNotificationChannelResponse"
    "fixture/GetNotificationChannelResponse.proto"
    defaultService
    (Proxy :: Proxy GetNotificationChannel)

responseGetAdminAccount :: GetAdminAccountResponse -> TestTree
responseGetAdminAccount =
  res
    "GetAdminAccountResponse"
    "fixture/GetAdminAccountResponse.proto"
    defaultService
    (Proxy :: Proxy GetAdminAccount)

responseListComplianceStatus :: ListComplianceStatusResponse -> TestTree
responseListComplianceStatus =
  res
    "ListComplianceStatusResponse"
    "fixture/ListComplianceStatusResponse.proto"
    defaultService
    (Proxy :: Proxy ListComplianceStatus)

responseGetAppsList :: GetAppsListResponse -> TestTree
responseGetAppsList =
  res
    "GetAppsListResponse"
    "fixture/GetAppsListResponse.proto"
    defaultService
    (Proxy :: Proxy GetAppsList)

responsePutPolicy :: PutPolicyResponse -> TestTree
responsePutPolicy =
  res
    "PutPolicyResponse"
    "fixture/PutPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutPolicy)

responseDeletePolicy :: DeletePolicyResponse -> TestTree
responseDeletePolicy =
  res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePolicy)

responseDisassociateAdminAccount :: DisassociateAdminAccountResponse -> TestTree
responseDisassociateAdminAccount =
  res
    "DisassociateAdminAccountResponse"
    "fixture/DisassociateAdminAccountResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateAdminAccount)

responsePutNotificationChannel :: PutNotificationChannelResponse -> TestTree
responsePutNotificationChannel =
  res
    "PutNotificationChannelResponse"
    "fixture/PutNotificationChannelResponse.proto"
    defaultService
    (Proxy :: Proxy PutNotificationChannel)

responseDeleteNotificationChannel :: DeleteNotificationChannelResponse -> TestTree
responseDeleteNotificationChannel =
  res
    "DeleteNotificationChannelResponse"
    "fixture/DeleteNotificationChannelResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteNotificationChannel)

responseAssociateAdminAccount :: AssociateAdminAccountResponse -> TestTree
responseAssociateAdminAccount =
  res
    "AssociateAdminAccountResponse"
    "fixture/AssociateAdminAccountResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateAdminAccount)

responseGetViolationDetails :: GetViolationDetailsResponse -> TestTree
responseGetViolationDetails =
  res
    "GetViolationDetailsResponse"
    "fixture/GetViolationDetailsResponse.proto"
    defaultService
    (Proxy :: Proxy GetViolationDetails)

responseListMemberAccounts :: ListMemberAccountsResponse -> TestTree
responseListMemberAccounts =
  res
    "ListMemberAccountsResponse"
    "fixture/ListMemberAccountsResponse.proto"
    defaultService
    (Proxy :: Proxy ListMemberAccounts)

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

responseDeleteProtocolsList :: DeleteProtocolsListResponse -> TestTree
responseDeleteProtocolsList =
  res
    "DeleteProtocolsListResponse"
    "fixture/DeleteProtocolsListResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteProtocolsList)

responseGetPolicy :: GetPolicyResponse -> TestTree
responseGetPolicy =
  res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetPolicy)

responseListProtocolsLists :: ListProtocolsListsResponse -> TestTree
responseListProtocolsLists =
  res
    "ListProtocolsListsResponse"
    "fixture/ListProtocolsListsResponse.proto"
    defaultService
    (Proxy :: Proxy ListProtocolsLists)

responsePutProtocolsList :: PutProtocolsListResponse -> TestTree
responsePutProtocolsList =
  res
    "PutProtocolsListResponse"
    "fixture/PutProtocolsListResponse.proto"
    defaultService
    (Proxy :: Proxy PutProtocolsList)

responsePutAppsList :: PutAppsListResponse -> TestTree
responsePutAppsList =
  res
    "PutAppsListResponse"
    "fixture/PutAppsListResponse.proto"
    defaultService
    (Proxy :: Proxy PutAppsList)

responseDeleteAppsList :: DeleteAppsListResponse -> TestTree
responseDeleteAppsList =
  res
    "DeleteAppsListResponse"
    "fixture/DeleteAppsListResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAppsList)

responseListAppsLists :: ListAppsListsResponse -> TestTree
responseListAppsLists =
  res
    "ListAppsListsResponse"
    "fixture/ListAppsListsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAppsLists)

responseGetProtocolsList :: GetProtocolsListResponse -> TestTree
responseGetProtocolsList =
  res
    "GetProtocolsListResponse"
    "fixture/GetProtocolsListResponse.proto"
    defaultService
    (Proxy :: Proxy GetProtocolsList)

responseGetProtectionStatus :: GetProtectionStatusResponse -> TestTree
responseGetProtectionStatus =
  res
    "GetProtectionStatusResponse"
    "fixture/GetProtectionStatusResponse.proto"
    defaultService
    (Proxy :: Proxy GetProtectionStatus)

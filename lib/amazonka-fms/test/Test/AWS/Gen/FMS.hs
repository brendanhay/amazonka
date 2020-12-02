{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.FMS
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--             listPolicies
--
--         , requestGetComplianceDetail $
--             getComplianceDetail
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestGetNotificationChannel $
--             getNotificationChannel
--
--         , requestGetAdminAccount $
--             getAdminAccount
--
--         , requestListComplianceStatus $
--             listComplianceStatus
--
--         , requestGetAppsList $
--             getAppsList
--
--         , requestPutPolicy $
--             putPolicy
--
--         , requestDeletePolicy $
--             deletePolicy
--
--         , requestDisassociateAdminAccount $
--             disassociateAdminAccount
--
--         , requestPutNotificationChannel $
--             putNotificationChannel
--
--         , requestDeleteNotificationChannel $
--             deleteNotificationChannel
--
--         , requestAssociateAdminAccount $
--             associateAdminAccount
--
--         , requestGetViolationDetails $
--             getViolationDetails
--
--         , requestListMemberAccounts $
--             listMemberAccounts
--
--         , requestTagResource $
--             tagResource
--
--         , requestUntagResource $
--             untagResource
--
--         , requestDeleteProtocolsList $
--             deleteProtocolsList
--
--         , requestGetPolicy $
--             getPolicy
--
--         , requestListProtocolsLists $
--             listProtocolsLists
--
--         , requestPutProtocolsList $
--             putProtocolsList
--
--         , requestPutAppsList $
--             putAppsList
--
--         , requestDeleteAppsList $
--             deleteAppsList
--
--         , requestListAppsLists $
--             listAppsLists
--
--         , requestGetProtocolsList $
--             getProtocolsList
--
--         , requestGetProtectionStatus $
--             getProtectionStatus
--
--           ]

--     , testGroup "response"
--         [ responseListPolicies $
--             listPoliciesResponse
--
--         , responseGetComplianceDetail $
--             getComplianceDetailResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responseGetNotificationChannel $
--             getNotificationChannelResponse
--
--         , responseGetAdminAccount $
--             getAdminAccountResponse
--
--         , responseListComplianceStatus $
--             listComplianceStatusResponse
--
--         , responseGetAppsList $
--             getAppsListResponse
--
--         , responsePutPolicy $
--             putPolicyResponse
--
--         , responseDeletePolicy $
--             deletePolicyResponse
--
--         , responseDisassociateAdminAccount $
--             disassociateAdminAccountResponse
--
--         , responsePutNotificationChannel $
--             putNotificationChannelResponse
--
--         , responseDeleteNotificationChannel $
--             deleteNotificationChannelResponse
--
--         , responseAssociateAdminAccount $
--             associateAdminAccountResponse
--
--         , responseGetViolationDetails $
--             getViolationDetailsResponse
--
--         , responseListMemberAccounts $
--             listMemberAccountsResponse
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responseDeleteProtocolsList $
--             deleteProtocolsListResponse
--
--         , responseGetPolicy $
--             getPolicyResponse
--
--         , responseListProtocolsLists $
--             listProtocolsListsResponse
--
--         , responsePutProtocolsList $
--             putProtocolsListResponse
--
--         , responsePutAppsList $
--             putAppsListResponse
--
--         , responseDeleteAppsList $
--             deleteAppsListResponse
--
--         , responseListAppsLists $
--             listAppsListsResponse
--
--         , responseGetProtocolsList $
--             getProtocolsListResponse
--
--         , responseGetProtectionStatus $
--             getProtectionStatusResponse
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
    fms
    (Proxy :: Proxy ListPolicies)

responseGetComplianceDetail :: GetComplianceDetailResponse -> TestTree
responseGetComplianceDetail =
  res
    "GetComplianceDetailResponse"
    "fixture/GetComplianceDetailResponse.proto"
    fms
    (Proxy :: Proxy GetComplianceDetail)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    fms
    (Proxy :: Proxy ListTagsForResource)

responseGetNotificationChannel :: GetNotificationChannelResponse -> TestTree
responseGetNotificationChannel =
  res
    "GetNotificationChannelResponse"
    "fixture/GetNotificationChannelResponse.proto"
    fms
    (Proxy :: Proxy GetNotificationChannel)

responseGetAdminAccount :: GetAdminAccountResponse -> TestTree
responseGetAdminAccount =
  res
    "GetAdminAccountResponse"
    "fixture/GetAdminAccountResponse.proto"
    fms
    (Proxy :: Proxy GetAdminAccount)

responseListComplianceStatus :: ListComplianceStatusResponse -> TestTree
responseListComplianceStatus =
  res
    "ListComplianceStatusResponse"
    "fixture/ListComplianceStatusResponse.proto"
    fms
    (Proxy :: Proxy ListComplianceStatus)

responseGetAppsList :: GetAppsListResponse -> TestTree
responseGetAppsList =
  res
    "GetAppsListResponse"
    "fixture/GetAppsListResponse.proto"
    fms
    (Proxy :: Proxy GetAppsList)

responsePutPolicy :: PutPolicyResponse -> TestTree
responsePutPolicy =
  res
    "PutPolicyResponse"
    "fixture/PutPolicyResponse.proto"
    fms
    (Proxy :: Proxy PutPolicy)

responseDeletePolicy :: DeletePolicyResponse -> TestTree
responseDeletePolicy =
  res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    fms
    (Proxy :: Proxy DeletePolicy)

responseDisassociateAdminAccount :: DisassociateAdminAccountResponse -> TestTree
responseDisassociateAdminAccount =
  res
    "DisassociateAdminAccountResponse"
    "fixture/DisassociateAdminAccountResponse.proto"
    fms
    (Proxy :: Proxy DisassociateAdminAccount)

responsePutNotificationChannel :: PutNotificationChannelResponse -> TestTree
responsePutNotificationChannel =
  res
    "PutNotificationChannelResponse"
    "fixture/PutNotificationChannelResponse.proto"
    fms
    (Proxy :: Proxy PutNotificationChannel)

responseDeleteNotificationChannel :: DeleteNotificationChannelResponse -> TestTree
responseDeleteNotificationChannel =
  res
    "DeleteNotificationChannelResponse"
    "fixture/DeleteNotificationChannelResponse.proto"
    fms
    (Proxy :: Proxy DeleteNotificationChannel)

responseAssociateAdminAccount :: AssociateAdminAccountResponse -> TestTree
responseAssociateAdminAccount =
  res
    "AssociateAdminAccountResponse"
    "fixture/AssociateAdminAccountResponse.proto"
    fms
    (Proxy :: Proxy AssociateAdminAccount)

responseGetViolationDetails :: GetViolationDetailsResponse -> TestTree
responseGetViolationDetails =
  res
    "GetViolationDetailsResponse"
    "fixture/GetViolationDetailsResponse.proto"
    fms
    (Proxy :: Proxy GetViolationDetails)

responseListMemberAccounts :: ListMemberAccountsResponse -> TestTree
responseListMemberAccounts =
  res
    "ListMemberAccountsResponse"
    "fixture/ListMemberAccountsResponse.proto"
    fms
    (Proxy :: Proxy ListMemberAccounts)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    fms
    (Proxy :: Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    fms
    (Proxy :: Proxy UntagResource)

responseDeleteProtocolsList :: DeleteProtocolsListResponse -> TestTree
responseDeleteProtocolsList =
  res
    "DeleteProtocolsListResponse"
    "fixture/DeleteProtocolsListResponse.proto"
    fms
    (Proxy :: Proxy DeleteProtocolsList)

responseGetPolicy :: GetPolicyResponse -> TestTree
responseGetPolicy =
  res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    fms
    (Proxy :: Proxy GetPolicy)

responseListProtocolsLists :: ListProtocolsListsResponse -> TestTree
responseListProtocolsLists =
  res
    "ListProtocolsListsResponse"
    "fixture/ListProtocolsListsResponse.proto"
    fms
    (Proxy :: Proxy ListProtocolsLists)

responsePutProtocolsList :: PutProtocolsListResponse -> TestTree
responsePutProtocolsList =
  res
    "PutProtocolsListResponse"
    "fixture/PutProtocolsListResponse.proto"
    fms
    (Proxy :: Proxy PutProtocolsList)

responsePutAppsList :: PutAppsListResponse -> TestTree
responsePutAppsList =
  res
    "PutAppsListResponse"
    "fixture/PutAppsListResponse.proto"
    fms
    (Proxy :: Proxy PutAppsList)

responseDeleteAppsList :: DeleteAppsListResponse -> TestTree
responseDeleteAppsList =
  res
    "DeleteAppsListResponse"
    "fixture/DeleteAppsListResponse.proto"
    fms
    (Proxy :: Proxy DeleteAppsList)

responseListAppsLists :: ListAppsListsResponse -> TestTree
responseListAppsLists =
  res
    "ListAppsListsResponse"
    "fixture/ListAppsListsResponse.proto"
    fms
    (Proxy :: Proxy ListAppsLists)

responseGetProtocolsList :: GetProtocolsListResponse -> TestTree
responseGetProtocolsList =
  res
    "GetProtocolsListResponse"
    "fixture/GetProtocolsListResponse.proto"
    fms
    (Proxy :: Proxy GetProtocolsList)

responseGetProtectionStatus :: GetProtectionStatusResponse -> TestTree
responseGetProtectionStatus =
  res
    "GetProtectionStatusResponse"
    "fixture/GetProtectionStatusResponse.proto"
    fms
    (Proxy :: Proxy GetProtectionStatus)

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.FMS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.FMS where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.FMS
import Test.AWS.FMS.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListPolicies $
--             mkListPolicies
--
--         , requestGetComplianceDetail $
--             mkGetComplianceDetail
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestGetNotificationChannel $
--             mkGetNotificationChannel
--
--         , requestGetAdminAccount $
--             mkGetAdminAccount
--
--         , requestListComplianceStatus $
--             mkListComplianceStatus
--
--         , requestGetAppsList $
--             mkGetAppsList
--
--         , requestPutPolicy $
--             mkPutPolicy
--
--         , requestDeletePolicy $
--             mkDeletePolicy
--
--         , requestDisassociateAdminAccount $
--             mkDisassociateAdminAccount
--
--         , requestPutNotificationChannel $
--             mkPutNotificationChannel
--
--         , requestDeleteNotificationChannel $
--             mkDeleteNotificationChannel
--
--         , requestAssociateAdminAccount $
--             mkAssociateAdminAccount
--
--         , requestGetViolationDetails $
--             mkGetViolationDetails
--
--         , requestListMemberAccounts $
--             mkListMemberAccounts
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestDeleteProtocolsList $
--             mkDeleteProtocolsList
--
--         , requestGetPolicy $
--             mkGetPolicy
--
--         , requestListProtocolsLists $
--             mkListProtocolsLists
--
--         , requestPutProtocolsList $
--             mkPutProtocolsList
--
--         , requestPutAppsList $
--             mkPutAppsList
--
--         , requestDeleteAppsList $
--             mkDeleteAppsList
--
--         , requestListAppsLists $
--             mkListAppsLists
--
--         , requestGetProtocolsList $
--             mkGetProtocolsList
--
--         , requestGetProtectionStatus $
--             mkGetProtectionStatus
--
--           ]

--     , testGroup "response"
--         [ responseListPolicies $
--             mkListPoliciesResponse
--
--         , responseGetComplianceDetail $
--             mkGetComplianceDetailResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseGetNotificationChannel $
--             mkGetNotificationChannelResponse
--
--         , responseGetAdminAccount $
--             mkGetAdminAccountResponse
--
--         , responseListComplianceStatus $
--             mkListComplianceStatusResponse
--
--         , responseGetAppsList $
--             mkGetAppsListResponse
--
--         , responsePutPolicy $
--             mkPutPolicyResponse
--
--         , responseDeletePolicy $
--             mkDeletePolicyResponse
--
--         , responseDisassociateAdminAccount $
--             mkDisassociateAdminAccountResponse
--
--         , responsePutNotificationChannel $
--             mkPutNotificationChannelResponse
--
--         , responseDeleteNotificationChannel $
--             mkDeleteNotificationChannelResponse
--
--         , responseAssociateAdminAccount $
--             mkAssociateAdminAccountResponse
--
--         , responseGetViolationDetails $
--             mkGetViolationDetailsResponse
--
--         , responseListMemberAccounts $
--             mkListMemberAccountsResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseDeleteProtocolsList $
--             mkDeleteProtocolsListResponse
--
--         , responseGetPolicy $
--             mkGetPolicyResponse
--
--         , responseListProtocolsLists $
--             mkListProtocolsListsResponse
--
--         , responsePutProtocolsList $
--             mkPutProtocolsListResponse
--
--         , responsePutAppsList $
--             mkPutAppsListResponse
--
--         , responseDeleteAppsList $
--             mkDeleteAppsListResponse
--
--         , responseListAppsLists $
--             mkListAppsListsResponse
--
--         , responseGetProtocolsList $
--             mkGetProtocolsListResponse
--
--         , responseGetProtectionStatus $
--             mkGetProtectionStatusResponse
--
--           ]
--     ]

-- Requests

requestListPolicies :: ListPolicies -> TestTree
requestListPolicies = req
    "ListPolicies"
    "fixture/ListPolicies.yaml"

requestGetComplianceDetail :: GetComplianceDetail -> TestTree
requestGetComplianceDetail = req
    "GetComplianceDetail"
    "fixture/GetComplianceDetail.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestGetNotificationChannel :: GetNotificationChannel -> TestTree
requestGetNotificationChannel = req
    "GetNotificationChannel"
    "fixture/GetNotificationChannel.yaml"

requestGetAdminAccount :: GetAdminAccount -> TestTree
requestGetAdminAccount = req
    "GetAdminAccount"
    "fixture/GetAdminAccount.yaml"

requestListComplianceStatus :: ListComplianceStatus -> TestTree
requestListComplianceStatus = req
    "ListComplianceStatus"
    "fixture/ListComplianceStatus.yaml"

requestGetAppsList :: GetAppsList -> TestTree
requestGetAppsList = req
    "GetAppsList"
    "fixture/GetAppsList.yaml"

requestPutPolicy :: PutPolicy -> TestTree
requestPutPolicy = req
    "PutPolicy"
    "fixture/PutPolicy.yaml"

requestDeletePolicy :: DeletePolicy -> TestTree
requestDeletePolicy = req
    "DeletePolicy"
    "fixture/DeletePolicy.yaml"

requestDisassociateAdminAccount :: DisassociateAdminAccount -> TestTree
requestDisassociateAdminAccount = req
    "DisassociateAdminAccount"
    "fixture/DisassociateAdminAccount.yaml"

requestPutNotificationChannel :: PutNotificationChannel -> TestTree
requestPutNotificationChannel = req
    "PutNotificationChannel"
    "fixture/PutNotificationChannel.yaml"

requestDeleteNotificationChannel :: DeleteNotificationChannel -> TestTree
requestDeleteNotificationChannel = req
    "DeleteNotificationChannel"
    "fixture/DeleteNotificationChannel.yaml"

requestAssociateAdminAccount :: AssociateAdminAccount -> TestTree
requestAssociateAdminAccount = req
    "AssociateAdminAccount"
    "fixture/AssociateAdminAccount.yaml"

requestGetViolationDetails :: GetViolationDetails -> TestTree
requestGetViolationDetails = req
    "GetViolationDetails"
    "fixture/GetViolationDetails.yaml"

requestListMemberAccounts :: ListMemberAccounts -> TestTree
requestListMemberAccounts = req
    "ListMemberAccounts"
    "fixture/ListMemberAccounts.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource = req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource = req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDeleteProtocolsList :: DeleteProtocolsList -> TestTree
requestDeleteProtocolsList = req
    "DeleteProtocolsList"
    "fixture/DeleteProtocolsList.yaml"

requestGetPolicy :: GetPolicy -> TestTree
requestGetPolicy = req
    "GetPolicy"
    "fixture/GetPolicy.yaml"

requestListProtocolsLists :: ListProtocolsLists -> TestTree
requestListProtocolsLists = req
    "ListProtocolsLists"
    "fixture/ListProtocolsLists.yaml"

requestPutProtocolsList :: PutProtocolsList -> TestTree
requestPutProtocolsList = req
    "PutProtocolsList"
    "fixture/PutProtocolsList.yaml"

requestPutAppsList :: PutAppsList -> TestTree
requestPutAppsList = req
    "PutAppsList"
    "fixture/PutAppsList.yaml"

requestDeleteAppsList :: DeleteAppsList -> TestTree
requestDeleteAppsList = req
    "DeleteAppsList"
    "fixture/DeleteAppsList.yaml"

requestListAppsLists :: ListAppsLists -> TestTree
requestListAppsLists = req
    "ListAppsLists"
    "fixture/ListAppsLists.yaml"

requestGetProtocolsList :: GetProtocolsList -> TestTree
requestGetProtocolsList = req
    "GetProtocolsList"
    "fixture/GetProtocolsList.yaml"

requestGetProtectionStatus :: GetProtectionStatus -> TestTree
requestGetProtectionStatus = req
    "GetProtectionStatus"
    "fixture/GetProtectionStatus.yaml"

-- Responses

responseListPolicies :: ListPoliciesResponse -> TestTree
responseListPolicies = res
    "ListPoliciesResponse"
    "fixture/ListPoliciesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListPolicies)

responseGetComplianceDetail :: GetComplianceDetailResponse -> TestTree
responseGetComplianceDetail = res
    "GetComplianceDetailResponse"
    "fixture/GetComplianceDetailResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetComplianceDetail)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTagsForResource)

responseGetNotificationChannel :: GetNotificationChannelResponse -> TestTree
responseGetNotificationChannel = res
    "GetNotificationChannelResponse"
    "fixture/GetNotificationChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetNotificationChannel)

responseGetAdminAccount :: GetAdminAccountResponse -> TestTree
responseGetAdminAccount = res
    "GetAdminAccountResponse"
    "fixture/GetAdminAccountResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetAdminAccount)

responseListComplianceStatus :: ListComplianceStatusResponse -> TestTree
responseListComplianceStatus = res
    "ListComplianceStatusResponse"
    "fixture/ListComplianceStatusResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListComplianceStatus)

responseGetAppsList :: GetAppsListResponse -> TestTree
responseGetAppsList = res
    "GetAppsListResponse"
    "fixture/GetAppsListResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetAppsList)

responsePutPolicy :: PutPolicyResponse -> TestTree
responsePutPolicy = res
    "PutPolicyResponse"
    "fixture/PutPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutPolicy)

responseDeletePolicy :: DeletePolicyResponse -> TestTree
responseDeletePolicy = res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeletePolicy)

responseDisassociateAdminAccount :: DisassociateAdminAccountResponse -> TestTree
responseDisassociateAdminAccount = res
    "DisassociateAdminAccountResponse"
    "fixture/DisassociateAdminAccountResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisassociateAdminAccount)

responsePutNotificationChannel :: PutNotificationChannelResponse -> TestTree
responsePutNotificationChannel = res
    "PutNotificationChannelResponse"
    "fixture/PutNotificationChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutNotificationChannel)

responseDeleteNotificationChannel :: DeleteNotificationChannelResponse -> TestTree
responseDeleteNotificationChannel = res
    "DeleteNotificationChannelResponse"
    "fixture/DeleteNotificationChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteNotificationChannel)

responseAssociateAdminAccount :: AssociateAdminAccountResponse -> TestTree
responseAssociateAdminAccount = res
    "AssociateAdminAccountResponse"
    "fixture/AssociateAdminAccountResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssociateAdminAccount)

responseGetViolationDetails :: GetViolationDetailsResponse -> TestTree
responseGetViolationDetails = res
    "GetViolationDetailsResponse"
    "fixture/GetViolationDetailsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetViolationDetails)

responseListMemberAccounts :: ListMemberAccountsResponse -> TestTree
responseListMemberAccounts = res
    "ListMemberAccountsResponse"
    "fixture/ListMemberAccountsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListMemberAccounts)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource = res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource = res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UntagResource)

responseDeleteProtocolsList :: DeleteProtocolsListResponse -> TestTree
responseDeleteProtocolsList = res
    "DeleteProtocolsListResponse"
    "fixture/DeleteProtocolsListResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteProtocolsList)

responseGetPolicy :: GetPolicyResponse -> TestTree
responseGetPolicy = res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetPolicy)

responseListProtocolsLists :: ListProtocolsListsResponse -> TestTree
responseListProtocolsLists = res
    "ListProtocolsListsResponse"
    "fixture/ListProtocolsListsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListProtocolsLists)

responsePutProtocolsList :: PutProtocolsListResponse -> TestTree
responsePutProtocolsList = res
    "PutProtocolsListResponse"
    "fixture/PutProtocolsListResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutProtocolsList)

responsePutAppsList :: PutAppsListResponse -> TestTree
responsePutAppsList = res
    "PutAppsListResponse"
    "fixture/PutAppsListResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutAppsList)

responseDeleteAppsList :: DeleteAppsListResponse -> TestTree
responseDeleteAppsList = res
    "DeleteAppsListResponse"
    "fixture/DeleteAppsListResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteAppsList)

responseListAppsLists :: ListAppsListsResponse -> TestTree
responseListAppsLists = res
    "ListAppsListsResponse"
    "fixture/ListAppsListsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListAppsLists)

responseGetProtocolsList :: GetProtocolsListResponse -> TestTree
responseGetProtocolsList = res
    "GetProtocolsListResponse"
    "fixture/GetProtocolsListResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetProtocolsList)

responseGetProtectionStatus :: GetProtectionStatusResponse -> TestTree
responseGetProtectionStatus = res
    "GetProtectionStatusResponse"
    "fixture/GetProtectionStatusResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetProtectionStatus)

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.FMS
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.FMS where

import Amazonka.FMS
import qualified Data.Proxy as Proxy
import Test.Amazonka.FMS.Internal
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
--         [ requestAssociateAdminAccount $
--             newAssociateAdminAccount
--
--         , requestAssociateThirdPartyFirewall $
--             newAssociateThirdPartyFirewall
--
--         , requestBatchAssociateResource $
--             newBatchAssociateResource
--
--         , requestBatchDisassociateResource $
--             newBatchDisassociateResource
--
--         , requestDeleteAppsList $
--             newDeleteAppsList
--
--         , requestDeleteNotificationChannel $
--             newDeleteNotificationChannel
--
--         , requestDeletePolicy $
--             newDeletePolicy
--
--         , requestDeleteProtocolsList $
--             newDeleteProtocolsList
--
--         , requestDeleteResourceSet $
--             newDeleteResourceSet
--
--         , requestDisassociateAdminAccount $
--             newDisassociateAdminAccount
--
--         , requestDisassociateThirdPartyFirewall $
--             newDisassociateThirdPartyFirewall
--
--         , requestGetAdminAccount $
--             newGetAdminAccount
--
--         , requestGetAppsList $
--             newGetAppsList
--
--         , requestGetComplianceDetail $
--             newGetComplianceDetail
--
--         , requestGetNotificationChannel $
--             newGetNotificationChannel
--
--         , requestGetPolicy $
--             newGetPolicy
--
--         , requestGetProtectionStatus $
--             newGetProtectionStatus
--
--         , requestGetProtocolsList $
--             newGetProtocolsList
--
--         , requestGetResourceSet $
--             newGetResourceSet
--
--         , requestGetThirdPartyFirewallAssociationStatus $
--             newGetThirdPartyFirewallAssociationStatus
--
--         , requestGetViolationDetails $
--             newGetViolationDetails
--
--         , requestListAppsLists $
--             newListAppsLists
--
--         , requestListComplianceStatus $
--             newListComplianceStatus
--
--         , requestListDiscoveredResources $
--             newListDiscoveredResources
--
--         , requestListMemberAccounts $
--             newListMemberAccounts
--
--         , requestListPolicies $
--             newListPolicies
--
--         , requestListProtocolsLists $
--             newListProtocolsLists
--
--         , requestListResourceSetResources $
--             newListResourceSetResources
--
--         , requestListResourceSets $
--             newListResourceSets
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListThirdPartyFirewallFirewallPolicies $
--             newListThirdPartyFirewallFirewallPolicies
--
--         , requestPutAppsList $
--             newPutAppsList
--
--         , requestPutNotificationChannel $
--             newPutNotificationChannel
--
--         , requestPutPolicy $
--             newPutPolicy
--
--         , requestPutProtocolsList $
--             newPutProtocolsList
--
--         , requestPutResourceSet $
--             newPutResourceSet
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--           ]

--     , testGroup "response"
--         [ responseAssociateAdminAccount $
--             newAssociateAdminAccountResponse
--
--         , responseAssociateThirdPartyFirewall $
--             newAssociateThirdPartyFirewallResponse
--
--         , responseBatchAssociateResource $
--             newBatchAssociateResourceResponse
--
--         , responseBatchDisassociateResource $
--             newBatchDisassociateResourceResponse
--
--         , responseDeleteAppsList $
--             newDeleteAppsListResponse
--
--         , responseDeleteNotificationChannel $
--             newDeleteNotificationChannelResponse
--
--         , responseDeletePolicy $
--             newDeletePolicyResponse
--
--         , responseDeleteProtocolsList $
--             newDeleteProtocolsListResponse
--
--         , responseDeleteResourceSet $
--             newDeleteResourceSetResponse
--
--         , responseDisassociateAdminAccount $
--             newDisassociateAdminAccountResponse
--
--         , responseDisassociateThirdPartyFirewall $
--             newDisassociateThirdPartyFirewallResponse
--
--         , responseGetAdminAccount $
--             newGetAdminAccountResponse
--
--         , responseGetAppsList $
--             newGetAppsListResponse
--
--         , responseGetComplianceDetail $
--             newGetComplianceDetailResponse
--
--         , responseGetNotificationChannel $
--             newGetNotificationChannelResponse
--
--         , responseGetPolicy $
--             newGetPolicyResponse
--
--         , responseGetProtectionStatus $
--             newGetProtectionStatusResponse
--
--         , responseGetProtocolsList $
--             newGetProtocolsListResponse
--
--         , responseGetResourceSet $
--             newGetResourceSetResponse
--
--         , responseGetThirdPartyFirewallAssociationStatus $
--             newGetThirdPartyFirewallAssociationStatusResponse
--
--         , responseGetViolationDetails $
--             newGetViolationDetailsResponse
--
--         , responseListAppsLists $
--             newListAppsListsResponse
--
--         , responseListComplianceStatus $
--             newListComplianceStatusResponse
--
--         , responseListDiscoveredResources $
--             newListDiscoveredResourcesResponse
--
--         , responseListMemberAccounts $
--             newListMemberAccountsResponse
--
--         , responseListPolicies $
--             newListPoliciesResponse
--
--         , responseListProtocolsLists $
--             newListProtocolsListsResponse
--
--         , responseListResourceSetResources $
--             newListResourceSetResourcesResponse
--
--         , responseListResourceSets $
--             newListResourceSetsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListThirdPartyFirewallFirewallPolicies $
--             newListThirdPartyFirewallFirewallPoliciesResponse
--
--         , responsePutAppsList $
--             newPutAppsListResponse
--
--         , responsePutNotificationChannel $
--             newPutNotificationChannelResponse
--
--         , responsePutPolicy $
--             newPutPolicyResponse
--
--         , responsePutProtocolsList $
--             newPutProtocolsListResponse
--
--         , responsePutResourceSet $
--             newPutResourceSetResponse
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

requestAssociateAdminAccount :: AssociateAdminAccount -> TestTree
requestAssociateAdminAccount =
  req
    "AssociateAdminAccount"
    "fixture/AssociateAdminAccount.yaml"

requestAssociateThirdPartyFirewall :: AssociateThirdPartyFirewall -> TestTree
requestAssociateThirdPartyFirewall =
  req
    "AssociateThirdPartyFirewall"
    "fixture/AssociateThirdPartyFirewall.yaml"

requestBatchAssociateResource :: BatchAssociateResource -> TestTree
requestBatchAssociateResource =
  req
    "BatchAssociateResource"
    "fixture/BatchAssociateResource.yaml"

requestBatchDisassociateResource :: BatchDisassociateResource -> TestTree
requestBatchDisassociateResource =
  req
    "BatchDisassociateResource"
    "fixture/BatchDisassociateResource.yaml"

requestDeleteAppsList :: DeleteAppsList -> TestTree
requestDeleteAppsList =
  req
    "DeleteAppsList"
    "fixture/DeleteAppsList.yaml"

requestDeleteNotificationChannel :: DeleteNotificationChannel -> TestTree
requestDeleteNotificationChannel =
  req
    "DeleteNotificationChannel"
    "fixture/DeleteNotificationChannel.yaml"

requestDeletePolicy :: DeletePolicy -> TestTree
requestDeletePolicy =
  req
    "DeletePolicy"
    "fixture/DeletePolicy.yaml"

requestDeleteProtocolsList :: DeleteProtocolsList -> TestTree
requestDeleteProtocolsList =
  req
    "DeleteProtocolsList"
    "fixture/DeleteProtocolsList.yaml"

requestDeleteResourceSet :: DeleteResourceSet -> TestTree
requestDeleteResourceSet =
  req
    "DeleteResourceSet"
    "fixture/DeleteResourceSet.yaml"

requestDisassociateAdminAccount :: DisassociateAdminAccount -> TestTree
requestDisassociateAdminAccount =
  req
    "DisassociateAdminAccount"
    "fixture/DisassociateAdminAccount.yaml"

requestDisassociateThirdPartyFirewall :: DisassociateThirdPartyFirewall -> TestTree
requestDisassociateThirdPartyFirewall =
  req
    "DisassociateThirdPartyFirewall"
    "fixture/DisassociateThirdPartyFirewall.yaml"

requestGetAdminAccount :: GetAdminAccount -> TestTree
requestGetAdminAccount =
  req
    "GetAdminAccount"
    "fixture/GetAdminAccount.yaml"

requestGetAppsList :: GetAppsList -> TestTree
requestGetAppsList =
  req
    "GetAppsList"
    "fixture/GetAppsList.yaml"

requestGetComplianceDetail :: GetComplianceDetail -> TestTree
requestGetComplianceDetail =
  req
    "GetComplianceDetail"
    "fixture/GetComplianceDetail.yaml"

requestGetNotificationChannel :: GetNotificationChannel -> TestTree
requestGetNotificationChannel =
  req
    "GetNotificationChannel"
    "fixture/GetNotificationChannel.yaml"

requestGetPolicy :: GetPolicy -> TestTree
requestGetPolicy =
  req
    "GetPolicy"
    "fixture/GetPolicy.yaml"

requestGetProtectionStatus :: GetProtectionStatus -> TestTree
requestGetProtectionStatus =
  req
    "GetProtectionStatus"
    "fixture/GetProtectionStatus.yaml"

requestGetProtocolsList :: GetProtocolsList -> TestTree
requestGetProtocolsList =
  req
    "GetProtocolsList"
    "fixture/GetProtocolsList.yaml"

requestGetResourceSet :: GetResourceSet -> TestTree
requestGetResourceSet =
  req
    "GetResourceSet"
    "fixture/GetResourceSet.yaml"

requestGetThirdPartyFirewallAssociationStatus :: GetThirdPartyFirewallAssociationStatus -> TestTree
requestGetThirdPartyFirewallAssociationStatus =
  req
    "GetThirdPartyFirewallAssociationStatus"
    "fixture/GetThirdPartyFirewallAssociationStatus.yaml"

requestGetViolationDetails :: GetViolationDetails -> TestTree
requestGetViolationDetails =
  req
    "GetViolationDetails"
    "fixture/GetViolationDetails.yaml"

requestListAppsLists :: ListAppsLists -> TestTree
requestListAppsLists =
  req
    "ListAppsLists"
    "fixture/ListAppsLists.yaml"

requestListComplianceStatus :: ListComplianceStatus -> TestTree
requestListComplianceStatus =
  req
    "ListComplianceStatus"
    "fixture/ListComplianceStatus.yaml"

requestListDiscoveredResources :: ListDiscoveredResources -> TestTree
requestListDiscoveredResources =
  req
    "ListDiscoveredResources"
    "fixture/ListDiscoveredResources.yaml"

requestListMemberAccounts :: ListMemberAccounts -> TestTree
requestListMemberAccounts =
  req
    "ListMemberAccounts"
    "fixture/ListMemberAccounts.yaml"

requestListPolicies :: ListPolicies -> TestTree
requestListPolicies =
  req
    "ListPolicies"
    "fixture/ListPolicies.yaml"

requestListProtocolsLists :: ListProtocolsLists -> TestTree
requestListProtocolsLists =
  req
    "ListProtocolsLists"
    "fixture/ListProtocolsLists.yaml"

requestListResourceSetResources :: ListResourceSetResources -> TestTree
requestListResourceSetResources =
  req
    "ListResourceSetResources"
    "fixture/ListResourceSetResources.yaml"

requestListResourceSets :: ListResourceSets -> TestTree
requestListResourceSets =
  req
    "ListResourceSets"
    "fixture/ListResourceSets.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListThirdPartyFirewallFirewallPolicies :: ListThirdPartyFirewallFirewallPolicies -> TestTree
requestListThirdPartyFirewallFirewallPolicies =
  req
    "ListThirdPartyFirewallFirewallPolicies"
    "fixture/ListThirdPartyFirewallFirewallPolicies.yaml"

requestPutAppsList :: PutAppsList -> TestTree
requestPutAppsList =
  req
    "PutAppsList"
    "fixture/PutAppsList.yaml"

requestPutNotificationChannel :: PutNotificationChannel -> TestTree
requestPutNotificationChannel =
  req
    "PutNotificationChannel"
    "fixture/PutNotificationChannel.yaml"

requestPutPolicy :: PutPolicy -> TestTree
requestPutPolicy =
  req
    "PutPolicy"
    "fixture/PutPolicy.yaml"

requestPutProtocolsList :: PutProtocolsList -> TestTree
requestPutProtocolsList =
  req
    "PutProtocolsList"
    "fixture/PutProtocolsList.yaml"

requestPutResourceSet :: PutResourceSet -> TestTree
requestPutResourceSet =
  req
    "PutResourceSet"
    "fixture/PutResourceSet.yaml"

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

responseAssociateAdminAccount :: AssociateAdminAccountResponse -> TestTree
responseAssociateAdminAccount =
  res
    "AssociateAdminAccountResponse"
    "fixture/AssociateAdminAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateAdminAccount)

responseAssociateThirdPartyFirewall :: AssociateThirdPartyFirewallResponse -> TestTree
responseAssociateThirdPartyFirewall =
  res
    "AssociateThirdPartyFirewallResponse"
    "fixture/AssociateThirdPartyFirewallResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateThirdPartyFirewall)

responseBatchAssociateResource :: BatchAssociateResourceResponse -> TestTree
responseBatchAssociateResource =
  res
    "BatchAssociateResourceResponse"
    "fixture/BatchAssociateResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchAssociateResource)

responseBatchDisassociateResource :: BatchDisassociateResourceResponse -> TestTree
responseBatchDisassociateResource =
  res
    "BatchDisassociateResourceResponse"
    "fixture/BatchDisassociateResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDisassociateResource)

responseDeleteAppsList :: DeleteAppsListResponse -> TestTree
responseDeleteAppsList =
  res
    "DeleteAppsListResponse"
    "fixture/DeleteAppsListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAppsList)

responseDeleteNotificationChannel :: DeleteNotificationChannelResponse -> TestTree
responseDeleteNotificationChannel =
  res
    "DeleteNotificationChannelResponse"
    "fixture/DeleteNotificationChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNotificationChannel)

responseDeletePolicy :: DeletePolicyResponse -> TestTree
responseDeletePolicy =
  res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePolicy)

responseDeleteProtocolsList :: DeleteProtocolsListResponse -> TestTree
responseDeleteProtocolsList =
  res
    "DeleteProtocolsListResponse"
    "fixture/DeleteProtocolsListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProtocolsList)

responseDeleteResourceSet :: DeleteResourceSetResponse -> TestTree
responseDeleteResourceSet =
  res
    "DeleteResourceSetResponse"
    "fixture/DeleteResourceSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourceSet)

responseDisassociateAdminAccount :: DisassociateAdminAccountResponse -> TestTree
responseDisassociateAdminAccount =
  res
    "DisassociateAdminAccountResponse"
    "fixture/DisassociateAdminAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateAdminAccount)

responseDisassociateThirdPartyFirewall :: DisassociateThirdPartyFirewallResponse -> TestTree
responseDisassociateThirdPartyFirewall =
  res
    "DisassociateThirdPartyFirewallResponse"
    "fixture/DisassociateThirdPartyFirewallResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateThirdPartyFirewall)

responseGetAdminAccount :: GetAdminAccountResponse -> TestTree
responseGetAdminAccount =
  res
    "GetAdminAccountResponse"
    "fixture/GetAdminAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAdminAccount)

responseGetAppsList :: GetAppsListResponse -> TestTree
responseGetAppsList =
  res
    "GetAppsListResponse"
    "fixture/GetAppsListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAppsList)

responseGetComplianceDetail :: GetComplianceDetailResponse -> TestTree
responseGetComplianceDetail =
  res
    "GetComplianceDetailResponse"
    "fixture/GetComplianceDetailResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetComplianceDetail)

responseGetNotificationChannel :: GetNotificationChannelResponse -> TestTree
responseGetNotificationChannel =
  res
    "GetNotificationChannelResponse"
    "fixture/GetNotificationChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetNotificationChannel)

responseGetPolicy :: GetPolicyResponse -> TestTree
responseGetPolicy =
  res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPolicy)

responseGetProtectionStatus :: GetProtectionStatusResponse -> TestTree
responseGetProtectionStatus =
  res
    "GetProtectionStatusResponse"
    "fixture/GetProtectionStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetProtectionStatus)

responseGetProtocolsList :: GetProtocolsListResponse -> TestTree
responseGetProtocolsList =
  res
    "GetProtocolsListResponse"
    "fixture/GetProtocolsListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetProtocolsList)

responseGetResourceSet :: GetResourceSetResponse -> TestTree
responseGetResourceSet =
  res
    "GetResourceSetResponse"
    "fixture/GetResourceSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourceSet)

responseGetThirdPartyFirewallAssociationStatus :: GetThirdPartyFirewallAssociationStatusResponse -> TestTree
responseGetThirdPartyFirewallAssociationStatus =
  res
    "GetThirdPartyFirewallAssociationStatusResponse"
    "fixture/GetThirdPartyFirewallAssociationStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetThirdPartyFirewallAssociationStatus)

responseGetViolationDetails :: GetViolationDetailsResponse -> TestTree
responseGetViolationDetails =
  res
    "GetViolationDetailsResponse"
    "fixture/GetViolationDetailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetViolationDetails)

responseListAppsLists :: ListAppsListsResponse -> TestTree
responseListAppsLists =
  res
    "ListAppsListsResponse"
    "fixture/ListAppsListsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAppsLists)

responseListComplianceStatus :: ListComplianceStatusResponse -> TestTree
responseListComplianceStatus =
  res
    "ListComplianceStatusResponse"
    "fixture/ListComplianceStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListComplianceStatus)

responseListDiscoveredResources :: ListDiscoveredResourcesResponse -> TestTree
responseListDiscoveredResources =
  res
    "ListDiscoveredResourcesResponse"
    "fixture/ListDiscoveredResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDiscoveredResources)

responseListMemberAccounts :: ListMemberAccountsResponse -> TestTree
responseListMemberAccounts =
  res
    "ListMemberAccountsResponse"
    "fixture/ListMemberAccountsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMemberAccounts)

responseListPolicies :: ListPoliciesResponse -> TestTree
responseListPolicies =
  res
    "ListPoliciesResponse"
    "fixture/ListPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPolicies)

responseListProtocolsLists :: ListProtocolsListsResponse -> TestTree
responseListProtocolsLists =
  res
    "ListProtocolsListsResponse"
    "fixture/ListProtocolsListsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProtocolsLists)

responseListResourceSetResources :: ListResourceSetResourcesResponse -> TestTree
responseListResourceSetResources =
  res
    "ListResourceSetResourcesResponse"
    "fixture/ListResourceSetResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResourceSetResources)

responseListResourceSets :: ListResourceSetsResponse -> TestTree
responseListResourceSets =
  res
    "ListResourceSetsResponse"
    "fixture/ListResourceSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResourceSets)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListThirdPartyFirewallFirewallPolicies :: ListThirdPartyFirewallFirewallPoliciesResponse -> TestTree
responseListThirdPartyFirewallFirewallPolicies =
  res
    "ListThirdPartyFirewallFirewallPoliciesResponse"
    "fixture/ListThirdPartyFirewallFirewallPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListThirdPartyFirewallFirewallPolicies)

responsePutAppsList :: PutAppsListResponse -> TestTree
responsePutAppsList =
  res
    "PutAppsListResponse"
    "fixture/PutAppsListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAppsList)

responsePutNotificationChannel :: PutNotificationChannelResponse -> TestTree
responsePutNotificationChannel =
  res
    "PutNotificationChannelResponse"
    "fixture/PutNotificationChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutNotificationChannel)

responsePutPolicy :: PutPolicyResponse -> TestTree
responsePutPolicy =
  res
    "PutPolicyResponse"
    "fixture/PutPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutPolicy)

responsePutProtocolsList :: PutProtocolsListResponse -> TestTree
responsePutProtocolsList =
  res
    "PutProtocolsListResponse"
    "fixture/PutProtocolsListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutProtocolsList)

responsePutResourceSet :: PutResourceSetResponse -> TestTree
responsePutResourceSet =
  res
    "PutResourceSetResponse"
    "fixture/PutResourceSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutResourceSet)

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

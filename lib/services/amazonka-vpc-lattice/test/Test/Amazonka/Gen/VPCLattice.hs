{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.VPCLattice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.VPCLattice where

import Amazonka.VPCLattice
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.VPCLattice.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestBatchUpdateRule $
--             newBatchUpdateRule
--
--         , requestCreateAccessLogSubscription $
--             newCreateAccessLogSubscription
--
--         , requestCreateListener $
--             newCreateListener
--
--         , requestCreateRule $
--             newCreateRule
--
--         , requestCreateService $
--             newCreateService
--
--         , requestCreateServiceNetwork $
--             newCreateServiceNetwork
--
--         , requestCreateServiceNetworkServiceAssociation $
--             newCreateServiceNetworkServiceAssociation
--
--         , requestCreateServiceNetworkVpcAssociation $
--             newCreateServiceNetworkVpcAssociation
--
--         , requestCreateTargetGroup $
--             newCreateTargetGroup
--
--         , requestDeleteAccessLogSubscription $
--             newDeleteAccessLogSubscription
--
--         , requestDeleteAuthPolicy $
--             newDeleteAuthPolicy
--
--         , requestDeleteListener $
--             newDeleteListener
--
--         , requestDeleteResourcePolicy $
--             newDeleteResourcePolicy
--
--         , requestDeleteRule $
--             newDeleteRule
--
--         , requestDeleteService $
--             newDeleteService
--
--         , requestDeleteServiceNetwork $
--             newDeleteServiceNetwork
--
--         , requestDeleteServiceNetworkServiceAssociation $
--             newDeleteServiceNetworkServiceAssociation
--
--         , requestDeleteServiceNetworkVpcAssociation $
--             newDeleteServiceNetworkVpcAssociation
--
--         , requestDeleteTargetGroup $
--             newDeleteTargetGroup
--
--         , requestDeregisterTargets $
--             newDeregisterTargets
--
--         , requestGetAccessLogSubscription $
--             newGetAccessLogSubscription
--
--         , requestGetAuthPolicy $
--             newGetAuthPolicy
--
--         , requestGetListener $
--             newGetListener
--
--         , requestGetResourcePolicy $
--             newGetResourcePolicy
--
--         , requestGetRule $
--             newGetRule
--
--         , requestGetService $
--             newGetService
--
--         , requestGetServiceNetwork $
--             newGetServiceNetwork
--
--         , requestGetServiceNetworkServiceAssociation $
--             newGetServiceNetworkServiceAssociation
--
--         , requestGetServiceNetworkVpcAssociation $
--             newGetServiceNetworkVpcAssociation
--
--         , requestGetTargetGroup $
--             newGetTargetGroup
--
--         , requestListAccessLogSubscriptions $
--             newListAccessLogSubscriptions
--
--         , requestListListeners $
--             newListListeners
--
--         , requestListRules $
--             newListRules
--
--         , requestListServiceNetworkServiceAssociations $
--             newListServiceNetworkServiceAssociations
--
--         , requestListServiceNetworkVpcAssociations $
--             newListServiceNetworkVpcAssociations
--
--         , requestListServiceNetworks $
--             newListServiceNetworks
--
--         , requestListServices $
--             newListServices
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListTargetGroups $
--             newListTargetGroups
--
--         , requestListTargets $
--             newListTargets
--
--         , requestPutAuthPolicy $
--             newPutAuthPolicy
--
--         , requestPutResourcePolicy $
--             newPutResourcePolicy
--
--         , requestRegisterTargets $
--             newRegisterTargets
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateAccessLogSubscription $
--             newUpdateAccessLogSubscription
--
--         , requestUpdateListener $
--             newUpdateListener
--
--         , requestUpdateRule $
--             newUpdateRule
--
--         , requestUpdateService $
--             newUpdateService
--
--         , requestUpdateServiceNetwork $
--             newUpdateServiceNetwork
--
--         , requestUpdateServiceNetworkVpcAssociation $
--             newUpdateServiceNetworkVpcAssociation
--
--         , requestUpdateTargetGroup $
--             newUpdateTargetGroup
--
--           ]

--     , testGroup "response"
--         [ responseBatchUpdateRule $
--             newBatchUpdateRuleResponse
--
--         , responseCreateAccessLogSubscription $
--             newCreateAccessLogSubscriptionResponse
--
--         , responseCreateListener $
--             newCreateListenerResponse
--
--         , responseCreateRule $
--             newCreateRuleResponse
--
--         , responseCreateService $
--             newCreateServiceResponse
--
--         , responseCreateServiceNetwork $
--             newCreateServiceNetworkResponse
--
--         , responseCreateServiceNetworkServiceAssociation $
--             newCreateServiceNetworkServiceAssociationResponse
--
--         , responseCreateServiceNetworkVpcAssociation $
--             newCreateServiceNetworkVpcAssociationResponse
--
--         , responseCreateTargetGroup $
--             newCreateTargetGroupResponse
--
--         , responseDeleteAccessLogSubscription $
--             newDeleteAccessLogSubscriptionResponse
--
--         , responseDeleteAuthPolicy $
--             newDeleteAuthPolicyResponse
--
--         , responseDeleteListener $
--             newDeleteListenerResponse
--
--         , responseDeleteResourcePolicy $
--             newDeleteResourcePolicyResponse
--
--         , responseDeleteRule $
--             newDeleteRuleResponse
--
--         , responseDeleteService $
--             newDeleteServiceResponse
--
--         , responseDeleteServiceNetwork $
--             newDeleteServiceNetworkResponse
--
--         , responseDeleteServiceNetworkServiceAssociation $
--             newDeleteServiceNetworkServiceAssociationResponse
--
--         , responseDeleteServiceNetworkVpcAssociation $
--             newDeleteServiceNetworkVpcAssociationResponse
--
--         , responseDeleteTargetGroup $
--             newDeleteTargetGroupResponse
--
--         , responseDeregisterTargets $
--             newDeregisterTargetsResponse
--
--         , responseGetAccessLogSubscription $
--             newGetAccessLogSubscriptionResponse
--
--         , responseGetAuthPolicy $
--             newGetAuthPolicyResponse
--
--         , responseGetListener $
--             newGetListenerResponse
--
--         , responseGetResourcePolicy $
--             newGetResourcePolicyResponse
--
--         , responseGetRule $
--             newGetRuleResponse
--
--         , responseGetService $
--             newGetServiceResponse
--
--         , responseGetServiceNetwork $
--             newGetServiceNetworkResponse
--
--         , responseGetServiceNetworkServiceAssociation $
--             newGetServiceNetworkServiceAssociationResponse
--
--         , responseGetServiceNetworkVpcAssociation $
--             newGetServiceNetworkVpcAssociationResponse
--
--         , responseGetTargetGroup $
--             newGetTargetGroupResponse
--
--         , responseListAccessLogSubscriptions $
--             newListAccessLogSubscriptionsResponse
--
--         , responseListListeners $
--             newListListenersResponse
--
--         , responseListRules $
--             newListRulesResponse
--
--         , responseListServiceNetworkServiceAssociations $
--             newListServiceNetworkServiceAssociationsResponse
--
--         , responseListServiceNetworkVpcAssociations $
--             newListServiceNetworkVpcAssociationsResponse
--
--         , responseListServiceNetworks $
--             newListServiceNetworksResponse
--
--         , responseListServices $
--             newListServicesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListTargetGroups $
--             newListTargetGroupsResponse
--
--         , responseListTargets $
--             newListTargetsResponse
--
--         , responsePutAuthPolicy $
--             newPutAuthPolicyResponse
--
--         , responsePutResourcePolicy $
--             newPutResourcePolicyResponse
--
--         , responseRegisterTargets $
--             newRegisterTargetsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateAccessLogSubscription $
--             newUpdateAccessLogSubscriptionResponse
--
--         , responseUpdateListener $
--             newUpdateListenerResponse
--
--         , responseUpdateRule $
--             newUpdateRuleResponse
--
--         , responseUpdateService $
--             newUpdateServiceResponse
--
--         , responseUpdateServiceNetwork $
--             newUpdateServiceNetworkResponse
--
--         , responseUpdateServiceNetworkVpcAssociation $
--             newUpdateServiceNetworkVpcAssociationResponse
--
--         , responseUpdateTargetGroup $
--             newUpdateTargetGroupResponse
--
--           ]
--     ]

-- Requests

requestBatchUpdateRule :: BatchUpdateRule -> TestTree
requestBatchUpdateRule =
  req
    "BatchUpdateRule"
    "fixture/BatchUpdateRule.yaml"

requestCreateAccessLogSubscription :: CreateAccessLogSubscription -> TestTree
requestCreateAccessLogSubscription =
  req
    "CreateAccessLogSubscription"
    "fixture/CreateAccessLogSubscription.yaml"

requestCreateListener :: CreateListener -> TestTree
requestCreateListener =
  req
    "CreateListener"
    "fixture/CreateListener.yaml"

requestCreateRule :: CreateRule -> TestTree
requestCreateRule =
  req
    "CreateRule"
    "fixture/CreateRule.yaml"

requestCreateService :: CreateService -> TestTree
requestCreateService =
  req
    "CreateService"
    "fixture/CreateService.yaml"

requestCreateServiceNetwork :: CreateServiceNetwork -> TestTree
requestCreateServiceNetwork =
  req
    "CreateServiceNetwork"
    "fixture/CreateServiceNetwork.yaml"

requestCreateServiceNetworkServiceAssociation :: CreateServiceNetworkServiceAssociation -> TestTree
requestCreateServiceNetworkServiceAssociation =
  req
    "CreateServiceNetworkServiceAssociation"
    "fixture/CreateServiceNetworkServiceAssociation.yaml"

requestCreateServiceNetworkVpcAssociation :: CreateServiceNetworkVpcAssociation -> TestTree
requestCreateServiceNetworkVpcAssociation =
  req
    "CreateServiceNetworkVpcAssociation"
    "fixture/CreateServiceNetworkVpcAssociation.yaml"

requestCreateTargetGroup :: CreateTargetGroup -> TestTree
requestCreateTargetGroup =
  req
    "CreateTargetGroup"
    "fixture/CreateTargetGroup.yaml"

requestDeleteAccessLogSubscription :: DeleteAccessLogSubscription -> TestTree
requestDeleteAccessLogSubscription =
  req
    "DeleteAccessLogSubscription"
    "fixture/DeleteAccessLogSubscription.yaml"

requestDeleteAuthPolicy :: DeleteAuthPolicy -> TestTree
requestDeleteAuthPolicy =
  req
    "DeleteAuthPolicy"
    "fixture/DeleteAuthPolicy.yaml"

requestDeleteListener :: DeleteListener -> TestTree
requestDeleteListener =
  req
    "DeleteListener"
    "fixture/DeleteListener.yaml"

requestDeleteResourcePolicy :: DeleteResourcePolicy -> TestTree
requestDeleteResourcePolicy =
  req
    "DeleteResourcePolicy"
    "fixture/DeleteResourcePolicy.yaml"

requestDeleteRule :: DeleteRule -> TestTree
requestDeleteRule =
  req
    "DeleteRule"
    "fixture/DeleteRule.yaml"

requestDeleteService :: DeleteService -> TestTree
requestDeleteService =
  req
    "DeleteService"
    "fixture/DeleteService.yaml"

requestDeleteServiceNetwork :: DeleteServiceNetwork -> TestTree
requestDeleteServiceNetwork =
  req
    "DeleteServiceNetwork"
    "fixture/DeleteServiceNetwork.yaml"

requestDeleteServiceNetworkServiceAssociation :: DeleteServiceNetworkServiceAssociation -> TestTree
requestDeleteServiceNetworkServiceAssociation =
  req
    "DeleteServiceNetworkServiceAssociation"
    "fixture/DeleteServiceNetworkServiceAssociation.yaml"

requestDeleteServiceNetworkVpcAssociation :: DeleteServiceNetworkVpcAssociation -> TestTree
requestDeleteServiceNetworkVpcAssociation =
  req
    "DeleteServiceNetworkVpcAssociation"
    "fixture/DeleteServiceNetworkVpcAssociation.yaml"

requestDeleteTargetGroup :: DeleteTargetGroup -> TestTree
requestDeleteTargetGroup =
  req
    "DeleteTargetGroup"
    "fixture/DeleteTargetGroup.yaml"

requestDeregisterTargets :: DeregisterTargets -> TestTree
requestDeregisterTargets =
  req
    "DeregisterTargets"
    "fixture/DeregisterTargets.yaml"

requestGetAccessLogSubscription :: GetAccessLogSubscription -> TestTree
requestGetAccessLogSubscription =
  req
    "GetAccessLogSubscription"
    "fixture/GetAccessLogSubscription.yaml"

requestGetAuthPolicy :: GetAuthPolicy -> TestTree
requestGetAuthPolicy =
  req
    "GetAuthPolicy"
    "fixture/GetAuthPolicy.yaml"

requestGetListener :: GetListener -> TestTree
requestGetListener =
  req
    "GetListener"
    "fixture/GetListener.yaml"

requestGetResourcePolicy :: GetResourcePolicy -> TestTree
requestGetResourcePolicy =
  req
    "GetResourcePolicy"
    "fixture/GetResourcePolicy.yaml"

requestGetRule :: GetRule -> TestTree
requestGetRule =
  req
    "GetRule"
    "fixture/GetRule.yaml"

requestGetService :: GetService -> TestTree
requestGetService =
  req
    "GetService"
    "fixture/GetService.yaml"

requestGetServiceNetwork :: GetServiceNetwork -> TestTree
requestGetServiceNetwork =
  req
    "GetServiceNetwork"
    "fixture/GetServiceNetwork.yaml"

requestGetServiceNetworkServiceAssociation :: GetServiceNetworkServiceAssociation -> TestTree
requestGetServiceNetworkServiceAssociation =
  req
    "GetServiceNetworkServiceAssociation"
    "fixture/GetServiceNetworkServiceAssociation.yaml"

requestGetServiceNetworkVpcAssociation :: GetServiceNetworkVpcAssociation -> TestTree
requestGetServiceNetworkVpcAssociation =
  req
    "GetServiceNetworkVpcAssociation"
    "fixture/GetServiceNetworkVpcAssociation.yaml"

requestGetTargetGroup :: GetTargetGroup -> TestTree
requestGetTargetGroup =
  req
    "GetTargetGroup"
    "fixture/GetTargetGroup.yaml"

requestListAccessLogSubscriptions :: ListAccessLogSubscriptions -> TestTree
requestListAccessLogSubscriptions =
  req
    "ListAccessLogSubscriptions"
    "fixture/ListAccessLogSubscriptions.yaml"

requestListListeners :: ListListeners -> TestTree
requestListListeners =
  req
    "ListListeners"
    "fixture/ListListeners.yaml"

requestListRules :: ListRules -> TestTree
requestListRules =
  req
    "ListRules"
    "fixture/ListRules.yaml"

requestListServiceNetworkServiceAssociations :: ListServiceNetworkServiceAssociations -> TestTree
requestListServiceNetworkServiceAssociations =
  req
    "ListServiceNetworkServiceAssociations"
    "fixture/ListServiceNetworkServiceAssociations.yaml"

requestListServiceNetworkVpcAssociations :: ListServiceNetworkVpcAssociations -> TestTree
requestListServiceNetworkVpcAssociations =
  req
    "ListServiceNetworkVpcAssociations"
    "fixture/ListServiceNetworkVpcAssociations.yaml"

requestListServiceNetworks :: ListServiceNetworks -> TestTree
requestListServiceNetworks =
  req
    "ListServiceNetworks"
    "fixture/ListServiceNetworks.yaml"

requestListServices :: ListServices -> TestTree
requestListServices =
  req
    "ListServices"
    "fixture/ListServices.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListTargetGroups :: ListTargetGroups -> TestTree
requestListTargetGroups =
  req
    "ListTargetGroups"
    "fixture/ListTargetGroups.yaml"

requestListTargets :: ListTargets -> TestTree
requestListTargets =
  req
    "ListTargets"
    "fixture/ListTargets.yaml"

requestPutAuthPolicy :: PutAuthPolicy -> TestTree
requestPutAuthPolicy =
  req
    "PutAuthPolicy"
    "fixture/PutAuthPolicy.yaml"

requestPutResourcePolicy :: PutResourcePolicy -> TestTree
requestPutResourcePolicy =
  req
    "PutResourcePolicy"
    "fixture/PutResourcePolicy.yaml"

requestRegisterTargets :: RegisterTargets -> TestTree
requestRegisterTargets =
  req
    "RegisterTargets"
    "fixture/RegisterTargets.yaml"

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

requestUpdateAccessLogSubscription :: UpdateAccessLogSubscription -> TestTree
requestUpdateAccessLogSubscription =
  req
    "UpdateAccessLogSubscription"
    "fixture/UpdateAccessLogSubscription.yaml"

requestUpdateListener :: UpdateListener -> TestTree
requestUpdateListener =
  req
    "UpdateListener"
    "fixture/UpdateListener.yaml"

requestUpdateRule :: UpdateRule -> TestTree
requestUpdateRule =
  req
    "UpdateRule"
    "fixture/UpdateRule.yaml"

requestUpdateService :: UpdateService -> TestTree
requestUpdateService =
  req
    "UpdateService"
    "fixture/UpdateService.yaml"

requestUpdateServiceNetwork :: UpdateServiceNetwork -> TestTree
requestUpdateServiceNetwork =
  req
    "UpdateServiceNetwork"
    "fixture/UpdateServiceNetwork.yaml"

requestUpdateServiceNetworkVpcAssociation :: UpdateServiceNetworkVpcAssociation -> TestTree
requestUpdateServiceNetworkVpcAssociation =
  req
    "UpdateServiceNetworkVpcAssociation"
    "fixture/UpdateServiceNetworkVpcAssociation.yaml"

requestUpdateTargetGroup :: UpdateTargetGroup -> TestTree
requestUpdateTargetGroup =
  req
    "UpdateTargetGroup"
    "fixture/UpdateTargetGroup.yaml"

-- Responses

responseBatchUpdateRule :: BatchUpdateRuleResponse -> TestTree
responseBatchUpdateRule =
  res
    "BatchUpdateRuleResponse"
    "fixture/BatchUpdateRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchUpdateRule)

responseCreateAccessLogSubscription :: CreateAccessLogSubscriptionResponse -> TestTree
responseCreateAccessLogSubscription =
  res
    "CreateAccessLogSubscriptionResponse"
    "fixture/CreateAccessLogSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAccessLogSubscription)

responseCreateListener :: CreateListenerResponse -> TestTree
responseCreateListener =
  res
    "CreateListenerResponse"
    "fixture/CreateListenerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateListener)

responseCreateRule :: CreateRuleResponse -> TestTree
responseCreateRule =
  res
    "CreateRuleResponse"
    "fixture/CreateRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRule)

responseCreateService :: CreateServiceResponse -> TestTree
responseCreateService =
  res
    "CreateServiceResponse"
    "fixture/CreateServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateService)

responseCreateServiceNetwork :: CreateServiceNetworkResponse -> TestTree
responseCreateServiceNetwork =
  res
    "CreateServiceNetworkResponse"
    "fixture/CreateServiceNetworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateServiceNetwork)

responseCreateServiceNetworkServiceAssociation :: CreateServiceNetworkServiceAssociationResponse -> TestTree
responseCreateServiceNetworkServiceAssociation =
  res
    "CreateServiceNetworkServiceAssociationResponse"
    "fixture/CreateServiceNetworkServiceAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateServiceNetworkServiceAssociation)

responseCreateServiceNetworkVpcAssociation :: CreateServiceNetworkVpcAssociationResponse -> TestTree
responseCreateServiceNetworkVpcAssociation =
  res
    "CreateServiceNetworkVpcAssociationResponse"
    "fixture/CreateServiceNetworkVpcAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateServiceNetworkVpcAssociation)

responseCreateTargetGroup :: CreateTargetGroupResponse -> TestTree
responseCreateTargetGroup =
  res
    "CreateTargetGroupResponse"
    "fixture/CreateTargetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTargetGroup)

responseDeleteAccessLogSubscription :: DeleteAccessLogSubscriptionResponse -> TestTree
responseDeleteAccessLogSubscription =
  res
    "DeleteAccessLogSubscriptionResponse"
    "fixture/DeleteAccessLogSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAccessLogSubscription)

responseDeleteAuthPolicy :: DeleteAuthPolicyResponse -> TestTree
responseDeleteAuthPolicy =
  res
    "DeleteAuthPolicyResponse"
    "fixture/DeleteAuthPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAuthPolicy)

responseDeleteListener :: DeleteListenerResponse -> TestTree
responseDeleteListener =
  res
    "DeleteListenerResponse"
    "fixture/DeleteListenerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteListener)

responseDeleteResourcePolicy :: DeleteResourcePolicyResponse -> TestTree
responseDeleteResourcePolicy =
  res
    "DeleteResourcePolicyResponse"
    "fixture/DeleteResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourcePolicy)

responseDeleteRule :: DeleteRuleResponse -> TestTree
responseDeleteRule =
  res
    "DeleteRuleResponse"
    "fixture/DeleteRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRule)

responseDeleteService :: DeleteServiceResponse -> TestTree
responseDeleteService =
  res
    "DeleteServiceResponse"
    "fixture/DeleteServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteService)

responseDeleteServiceNetwork :: DeleteServiceNetworkResponse -> TestTree
responseDeleteServiceNetwork =
  res
    "DeleteServiceNetworkResponse"
    "fixture/DeleteServiceNetworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteServiceNetwork)

responseDeleteServiceNetworkServiceAssociation :: DeleteServiceNetworkServiceAssociationResponse -> TestTree
responseDeleteServiceNetworkServiceAssociation =
  res
    "DeleteServiceNetworkServiceAssociationResponse"
    "fixture/DeleteServiceNetworkServiceAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteServiceNetworkServiceAssociation)

responseDeleteServiceNetworkVpcAssociation :: DeleteServiceNetworkVpcAssociationResponse -> TestTree
responseDeleteServiceNetworkVpcAssociation =
  res
    "DeleteServiceNetworkVpcAssociationResponse"
    "fixture/DeleteServiceNetworkVpcAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteServiceNetworkVpcAssociation)

responseDeleteTargetGroup :: DeleteTargetGroupResponse -> TestTree
responseDeleteTargetGroup =
  res
    "DeleteTargetGroupResponse"
    "fixture/DeleteTargetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTargetGroup)

responseDeregisterTargets :: DeregisterTargetsResponse -> TestTree
responseDeregisterTargets =
  res
    "DeregisterTargetsResponse"
    "fixture/DeregisterTargetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterTargets)

responseGetAccessLogSubscription :: GetAccessLogSubscriptionResponse -> TestTree
responseGetAccessLogSubscription =
  res
    "GetAccessLogSubscriptionResponse"
    "fixture/GetAccessLogSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccessLogSubscription)

responseGetAuthPolicy :: GetAuthPolicyResponse -> TestTree
responseGetAuthPolicy =
  res
    "GetAuthPolicyResponse"
    "fixture/GetAuthPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAuthPolicy)

responseGetListener :: GetListenerResponse -> TestTree
responseGetListener =
  res
    "GetListenerResponse"
    "fixture/GetListenerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetListener)

responseGetResourcePolicy :: GetResourcePolicyResponse -> TestTree
responseGetResourcePolicy =
  res
    "GetResourcePolicyResponse"
    "fixture/GetResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourcePolicy)

responseGetRule :: GetRuleResponse -> TestTree
responseGetRule =
  res
    "GetRuleResponse"
    "fixture/GetRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRule)

responseGetService :: GetServiceResponse -> TestTree
responseGetService =
  res
    "GetServiceResponse"
    "fixture/GetServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetService)

responseGetServiceNetwork :: GetServiceNetworkResponse -> TestTree
responseGetServiceNetwork =
  res
    "GetServiceNetworkResponse"
    "fixture/GetServiceNetworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetServiceNetwork)

responseGetServiceNetworkServiceAssociation :: GetServiceNetworkServiceAssociationResponse -> TestTree
responseGetServiceNetworkServiceAssociation =
  res
    "GetServiceNetworkServiceAssociationResponse"
    "fixture/GetServiceNetworkServiceAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetServiceNetworkServiceAssociation)

responseGetServiceNetworkVpcAssociation :: GetServiceNetworkVpcAssociationResponse -> TestTree
responseGetServiceNetworkVpcAssociation =
  res
    "GetServiceNetworkVpcAssociationResponse"
    "fixture/GetServiceNetworkVpcAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetServiceNetworkVpcAssociation)

responseGetTargetGroup :: GetTargetGroupResponse -> TestTree
responseGetTargetGroup =
  res
    "GetTargetGroupResponse"
    "fixture/GetTargetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTargetGroup)

responseListAccessLogSubscriptions :: ListAccessLogSubscriptionsResponse -> TestTree
responseListAccessLogSubscriptions =
  res
    "ListAccessLogSubscriptionsResponse"
    "fixture/ListAccessLogSubscriptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccessLogSubscriptions)

responseListListeners :: ListListenersResponse -> TestTree
responseListListeners =
  res
    "ListListenersResponse"
    "fixture/ListListenersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListListeners)

responseListRules :: ListRulesResponse -> TestTree
responseListRules =
  res
    "ListRulesResponse"
    "fixture/ListRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRules)

responseListServiceNetworkServiceAssociations :: ListServiceNetworkServiceAssociationsResponse -> TestTree
responseListServiceNetworkServiceAssociations =
  res
    "ListServiceNetworkServiceAssociationsResponse"
    "fixture/ListServiceNetworkServiceAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServiceNetworkServiceAssociations)

responseListServiceNetworkVpcAssociations :: ListServiceNetworkVpcAssociationsResponse -> TestTree
responseListServiceNetworkVpcAssociations =
  res
    "ListServiceNetworkVpcAssociationsResponse"
    "fixture/ListServiceNetworkVpcAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServiceNetworkVpcAssociations)

responseListServiceNetworks :: ListServiceNetworksResponse -> TestTree
responseListServiceNetworks =
  res
    "ListServiceNetworksResponse"
    "fixture/ListServiceNetworksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServiceNetworks)

responseListServices :: ListServicesResponse -> TestTree
responseListServices =
  res
    "ListServicesResponse"
    "fixture/ListServicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServices)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListTargetGroups :: ListTargetGroupsResponse -> TestTree
responseListTargetGroups =
  res
    "ListTargetGroupsResponse"
    "fixture/ListTargetGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTargetGroups)

responseListTargets :: ListTargetsResponse -> TestTree
responseListTargets =
  res
    "ListTargetsResponse"
    "fixture/ListTargetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTargets)

responsePutAuthPolicy :: PutAuthPolicyResponse -> TestTree
responsePutAuthPolicy =
  res
    "PutAuthPolicyResponse"
    "fixture/PutAuthPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAuthPolicy)

responsePutResourcePolicy :: PutResourcePolicyResponse -> TestTree
responsePutResourcePolicy =
  res
    "PutResourcePolicyResponse"
    "fixture/PutResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutResourcePolicy)

responseRegisterTargets :: RegisterTargetsResponse -> TestTree
responseRegisterTargets =
  res
    "RegisterTargetsResponse"
    "fixture/RegisterTargetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterTargets)

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

responseUpdateAccessLogSubscription :: UpdateAccessLogSubscriptionResponse -> TestTree
responseUpdateAccessLogSubscription =
  res
    "UpdateAccessLogSubscriptionResponse"
    "fixture/UpdateAccessLogSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAccessLogSubscription)

responseUpdateListener :: UpdateListenerResponse -> TestTree
responseUpdateListener =
  res
    "UpdateListenerResponse"
    "fixture/UpdateListenerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateListener)

responseUpdateRule :: UpdateRuleResponse -> TestTree
responseUpdateRule =
  res
    "UpdateRuleResponse"
    "fixture/UpdateRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRule)

responseUpdateService :: UpdateServiceResponse -> TestTree
responseUpdateService =
  res
    "UpdateServiceResponse"
    "fixture/UpdateServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateService)

responseUpdateServiceNetwork :: UpdateServiceNetworkResponse -> TestTree
responseUpdateServiceNetwork =
  res
    "UpdateServiceNetworkResponse"
    "fixture/UpdateServiceNetworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateServiceNetwork)

responseUpdateServiceNetworkVpcAssociation :: UpdateServiceNetworkVpcAssociationResponse -> TestTree
responseUpdateServiceNetworkVpcAssociation =
  res
    "UpdateServiceNetworkVpcAssociationResponse"
    "fixture/UpdateServiceNetworkVpcAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateServiceNetworkVpcAssociation)

responseUpdateTargetGroup :: UpdateTargetGroupResponse -> TestTree
responseUpdateTargetGroup =
  res
    "UpdateTargetGroupResponse"
    "fixture/UpdateTargetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTargetGroup)

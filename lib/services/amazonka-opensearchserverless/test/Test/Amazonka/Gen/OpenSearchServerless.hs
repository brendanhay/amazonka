{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.OpenSearchServerless
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.OpenSearchServerless where

import Amazonka.OpenSearchServerless
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.OpenSearchServerless.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestBatchGetCollection $
--             newBatchGetCollection
--
--         , requestBatchGetVpcEndpoint $
--             newBatchGetVpcEndpoint
--
--         , requestCreateAccessPolicy $
--             newCreateAccessPolicy
--
--         , requestCreateCollection $
--             newCreateCollection
--
--         , requestCreateSecurityConfig $
--             newCreateSecurityConfig
--
--         , requestCreateSecurityPolicy $
--             newCreateSecurityPolicy
--
--         , requestCreateVpcEndpoint $
--             newCreateVpcEndpoint
--
--         , requestDeleteAccessPolicy $
--             newDeleteAccessPolicy
--
--         , requestDeleteCollection $
--             newDeleteCollection
--
--         , requestDeleteSecurityConfig $
--             newDeleteSecurityConfig
--
--         , requestDeleteSecurityPolicy $
--             newDeleteSecurityPolicy
--
--         , requestDeleteVpcEndpoint $
--             newDeleteVpcEndpoint
--
--         , requestGetAccessPolicy $
--             newGetAccessPolicy
--
--         , requestGetAccountSettings $
--             newGetAccountSettings
--
--         , requestGetPoliciesStats $
--             newGetPoliciesStats
--
--         , requestGetSecurityConfig $
--             newGetSecurityConfig
--
--         , requestGetSecurityPolicy $
--             newGetSecurityPolicy
--
--         , requestListAccessPolicies $
--             newListAccessPolicies
--
--         , requestListCollections $
--             newListCollections
--
--         , requestListSecurityConfigs $
--             newListSecurityConfigs
--
--         , requestListSecurityPolicies $
--             newListSecurityPolicies
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListVpcEndpoints $
--             newListVpcEndpoints
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateAccessPolicy $
--             newUpdateAccessPolicy
--
--         , requestUpdateAccountSettings $
--             newUpdateAccountSettings
--
--         , requestUpdateCollection $
--             newUpdateCollection
--
--         , requestUpdateSecurityConfig $
--             newUpdateSecurityConfig
--
--         , requestUpdateSecurityPolicy $
--             newUpdateSecurityPolicy
--
--         , requestUpdateVpcEndpoint $
--             newUpdateVpcEndpoint
--
--           ]

--     , testGroup "response"
--         [ responseBatchGetCollection $
--             newBatchGetCollectionResponse
--
--         , responseBatchGetVpcEndpoint $
--             newBatchGetVpcEndpointResponse
--
--         , responseCreateAccessPolicy $
--             newCreateAccessPolicyResponse
--
--         , responseCreateCollection $
--             newCreateCollectionResponse
--
--         , responseCreateSecurityConfig $
--             newCreateSecurityConfigResponse
--
--         , responseCreateSecurityPolicy $
--             newCreateSecurityPolicyResponse
--
--         , responseCreateVpcEndpoint $
--             newCreateVpcEndpointResponse
--
--         , responseDeleteAccessPolicy $
--             newDeleteAccessPolicyResponse
--
--         , responseDeleteCollection $
--             newDeleteCollectionResponse
--
--         , responseDeleteSecurityConfig $
--             newDeleteSecurityConfigResponse
--
--         , responseDeleteSecurityPolicy $
--             newDeleteSecurityPolicyResponse
--
--         , responseDeleteVpcEndpoint $
--             newDeleteVpcEndpointResponse
--
--         , responseGetAccessPolicy $
--             newGetAccessPolicyResponse
--
--         , responseGetAccountSettings $
--             newGetAccountSettingsResponse
--
--         , responseGetPoliciesStats $
--             newGetPoliciesStatsResponse
--
--         , responseGetSecurityConfig $
--             newGetSecurityConfigResponse
--
--         , responseGetSecurityPolicy $
--             newGetSecurityPolicyResponse
--
--         , responseListAccessPolicies $
--             newListAccessPoliciesResponse
--
--         , responseListCollections $
--             newListCollectionsResponse
--
--         , responseListSecurityConfigs $
--             newListSecurityConfigsResponse
--
--         , responseListSecurityPolicies $
--             newListSecurityPoliciesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListVpcEndpoints $
--             newListVpcEndpointsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateAccessPolicy $
--             newUpdateAccessPolicyResponse
--
--         , responseUpdateAccountSettings $
--             newUpdateAccountSettingsResponse
--
--         , responseUpdateCollection $
--             newUpdateCollectionResponse
--
--         , responseUpdateSecurityConfig $
--             newUpdateSecurityConfigResponse
--
--         , responseUpdateSecurityPolicy $
--             newUpdateSecurityPolicyResponse
--
--         , responseUpdateVpcEndpoint $
--             newUpdateVpcEndpointResponse
--
--           ]
--     ]

-- Requests

requestBatchGetCollection :: BatchGetCollection -> TestTree
requestBatchGetCollection =
  req
    "BatchGetCollection"
    "fixture/BatchGetCollection.yaml"

requestBatchGetVpcEndpoint :: BatchGetVpcEndpoint -> TestTree
requestBatchGetVpcEndpoint =
  req
    "BatchGetVpcEndpoint"
    "fixture/BatchGetVpcEndpoint.yaml"

requestCreateAccessPolicy :: CreateAccessPolicy -> TestTree
requestCreateAccessPolicy =
  req
    "CreateAccessPolicy"
    "fixture/CreateAccessPolicy.yaml"

requestCreateCollection :: CreateCollection -> TestTree
requestCreateCollection =
  req
    "CreateCollection"
    "fixture/CreateCollection.yaml"

requestCreateSecurityConfig :: CreateSecurityConfig -> TestTree
requestCreateSecurityConfig =
  req
    "CreateSecurityConfig"
    "fixture/CreateSecurityConfig.yaml"

requestCreateSecurityPolicy :: CreateSecurityPolicy -> TestTree
requestCreateSecurityPolicy =
  req
    "CreateSecurityPolicy"
    "fixture/CreateSecurityPolicy.yaml"

requestCreateVpcEndpoint :: CreateVpcEndpoint -> TestTree
requestCreateVpcEndpoint =
  req
    "CreateVpcEndpoint"
    "fixture/CreateVpcEndpoint.yaml"

requestDeleteAccessPolicy :: DeleteAccessPolicy -> TestTree
requestDeleteAccessPolicy =
  req
    "DeleteAccessPolicy"
    "fixture/DeleteAccessPolicy.yaml"

requestDeleteCollection :: DeleteCollection -> TestTree
requestDeleteCollection =
  req
    "DeleteCollection"
    "fixture/DeleteCollection.yaml"

requestDeleteSecurityConfig :: DeleteSecurityConfig -> TestTree
requestDeleteSecurityConfig =
  req
    "DeleteSecurityConfig"
    "fixture/DeleteSecurityConfig.yaml"

requestDeleteSecurityPolicy :: DeleteSecurityPolicy -> TestTree
requestDeleteSecurityPolicy =
  req
    "DeleteSecurityPolicy"
    "fixture/DeleteSecurityPolicy.yaml"

requestDeleteVpcEndpoint :: DeleteVpcEndpoint -> TestTree
requestDeleteVpcEndpoint =
  req
    "DeleteVpcEndpoint"
    "fixture/DeleteVpcEndpoint.yaml"

requestGetAccessPolicy :: GetAccessPolicy -> TestTree
requestGetAccessPolicy =
  req
    "GetAccessPolicy"
    "fixture/GetAccessPolicy.yaml"

requestGetAccountSettings :: GetAccountSettings -> TestTree
requestGetAccountSettings =
  req
    "GetAccountSettings"
    "fixture/GetAccountSettings.yaml"

requestGetPoliciesStats :: GetPoliciesStats -> TestTree
requestGetPoliciesStats =
  req
    "GetPoliciesStats"
    "fixture/GetPoliciesStats.yaml"

requestGetSecurityConfig :: GetSecurityConfig -> TestTree
requestGetSecurityConfig =
  req
    "GetSecurityConfig"
    "fixture/GetSecurityConfig.yaml"

requestGetSecurityPolicy :: GetSecurityPolicy -> TestTree
requestGetSecurityPolicy =
  req
    "GetSecurityPolicy"
    "fixture/GetSecurityPolicy.yaml"

requestListAccessPolicies :: ListAccessPolicies -> TestTree
requestListAccessPolicies =
  req
    "ListAccessPolicies"
    "fixture/ListAccessPolicies.yaml"

requestListCollections :: ListCollections -> TestTree
requestListCollections =
  req
    "ListCollections"
    "fixture/ListCollections.yaml"

requestListSecurityConfigs :: ListSecurityConfigs -> TestTree
requestListSecurityConfigs =
  req
    "ListSecurityConfigs"
    "fixture/ListSecurityConfigs.yaml"

requestListSecurityPolicies :: ListSecurityPolicies -> TestTree
requestListSecurityPolicies =
  req
    "ListSecurityPolicies"
    "fixture/ListSecurityPolicies.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListVpcEndpoints :: ListVpcEndpoints -> TestTree
requestListVpcEndpoints =
  req
    "ListVpcEndpoints"
    "fixture/ListVpcEndpoints.yaml"

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

requestUpdateAccessPolicy :: UpdateAccessPolicy -> TestTree
requestUpdateAccessPolicy =
  req
    "UpdateAccessPolicy"
    "fixture/UpdateAccessPolicy.yaml"

requestUpdateAccountSettings :: UpdateAccountSettings -> TestTree
requestUpdateAccountSettings =
  req
    "UpdateAccountSettings"
    "fixture/UpdateAccountSettings.yaml"

requestUpdateCollection :: UpdateCollection -> TestTree
requestUpdateCollection =
  req
    "UpdateCollection"
    "fixture/UpdateCollection.yaml"

requestUpdateSecurityConfig :: UpdateSecurityConfig -> TestTree
requestUpdateSecurityConfig =
  req
    "UpdateSecurityConfig"
    "fixture/UpdateSecurityConfig.yaml"

requestUpdateSecurityPolicy :: UpdateSecurityPolicy -> TestTree
requestUpdateSecurityPolicy =
  req
    "UpdateSecurityPolicy"
    "fixture/UpdateSecurityPolicy.yaml"

requestUpdateVpcEndpoint :: UpdateVpcEndpoint -> TestTree
requestUpdateVpcEndpoint =
  req
    "UpdateVpcEndpoint"
    "fixture/UpdateVpcEndpoint.yaml"

-- Responses

responseBatchGetCollection :: BatchGetCollectionResponse -> TestTree
responseBatchGetCollection =
  res
    "BatchGetCollectionResponse"
    "fixture/BatchGetCollectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetCollection)

responseBatchGetVpcEndpoint :: BatchGetVpcEndpointResponse -> TestTree
responseBatchGetVpcEndpoint =
  res
    "BatchGetVpcEndpointResponse"
    "fixture/BatchGetVpcEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetVpcEndpoint)

responseCreateAccessPolicy :: CreateAccessPolicyResponse -> TestTree
responseCreateAccessPolicy =
  res
    "CreateAccessPolicyResponse"
    "fixture/CreateAccessPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAccessPolicy)

responseCreateCollection :: CreateCollectionResponse -> TestTree
responseCreateCollection =
  res
    "CreateCollectionResponse"
    "fixture/CreateCollectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCollection)

responseCreateSecurityConfig :: CreateSecurityConfigResponse -> TestTree
responseCreateSecurityConfig =
  res
    "CreateSecurityConfigResponse"
    "fixture/CreateSecurityConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSecurityConfig)

responseCreateSecurityPolicy :: CreateSecurityPolicyResponse -> TestTree
responseCreateSecurityPolicy =
  res
    "CreateSecurityPolicyResponse"
    "fixture/CreateSecurityPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSecurityPolicy)

responseCreateVpcEndpoint :: CreateVpcEndpointResponse -> TestTree
responseCreateVpcEndpoint =
  res
    "CreateVpcEndpointResponse"
    "fixture/CreateVpcEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVpcEndpoint)

responseDeleteAccessPolicy :: DeleteAccessPolicyResponse -> TestTree
responseDeleteAccessPolicy =
  res
    "DeleteAccessPolicyResponse"
    "fixture/DeleteAccessPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAccessPolicy)

responseDeleteCollection :: DeleteCollectionResponse -> TestTree
responseDeleteCollection =
  res
    "DeleteCollectionResponse"
    "fixture/DeleteCollectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCollection)

responseDeleteSecurityConfig :: DeleteSecurityConfigResponse -> TestTree
responseDeleteSecurityConfig =
  res
    "DeleteSecurityConfigResponse"
    "fixture/DeleteSecurityConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSecurityConfig)

responseDeleteSecurityPolicy :: DeleteSecurityPolicyResponse -> TestTree
responseDeleteSecurityPolicy =
  res
    "DeleteSecurityPolicyResponse"
    "fixture/DeleteSecurityPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSecurityPolicy)

responseDeleteVpcEndpoint :: DeleteVpcEndpointResponse -> TestTree
responseDeleteVpcEndpoint =
  res
    "DeleteVpcEndpointResponse"
    "fixture/DeleteVpcEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVpcEndpoint)

responseGetAccessPolicy :: GetAccessPolicyResponse -> TestTree
responseGetAccessPolicy =
  res
    "GetAccessPolicyResponse"
    "fixture/GetAccessPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccessPolicy)

responseGetAccountSettings :: GetAccountSettingsResponse -> TestTree
responseGetAccountSettings =
  res
    "GetAccountSettingsResponse"
    "fixture/GetAccountSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccountSettings)

responseGetPoliciesStats :: GetPoliciesStatsResponse -> TestTree
responseGetPoliciesStats =
  res
    "GetPoliciesStatsResponse"
    "fixture/GetPoliciesStatsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPoliciesStats)

responseGetSecurityConfig :: GetSecurityConfigResponse -> TestTree
responseGetSecurityConfig =
  res
    "GetSecurityConfigResponse"
    "fixture/GetSecurityConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSecurityConfig)

responseGetSecurityPolicy :: GetSecurityPolicyResponse -> TestTree
responseGetSecurityPolicy =
  res
    "GetSecurityPolicyResponse"
    "fixture/GetSecurityPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSecurityPolicy)

responseListAccessPolicies :: ListAccessPoliciesResponse -> TestTree
responseListAccessPolicies =
  res
    "ListAccessPoliciesResponse"
    "fixture/ListAccessPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccessPolicies)

responseListCollections :: ListCollectionsResponse -> TestTree
responseListCollections =
  res
    "ListCollectionsResponse"
    "fixture/ListCollectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCollections)

responseListSecurityConfigs :: ListSecurityConfigsResponse -> TestTree
responseListSecurityConfigs =
  res
    "ListSecurityConfigsResponse"
    "fixture/ListSecurityConfigsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSecurityConfigs)

responseListSecurityPolicies :: ListSecurityPoliciesResponse -> TestTree
responseListSecurityPolicies =
  res
    "ListSecurityPoliciesResponse"
    "fixture/ListSecurityPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSecurityPolicies)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListVpcEndpoints :: ListVpcEndpointsResponse -> TestTree
responseListVpcEndpoints =
  res
    "ListVpcEndpointsResponse"
    "fixture/ListVpcEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVpcEndpoints)

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

responseUpdateAccessPolicy :: UpdateAccessPolicyResponse -> TestTree
responseUpdateAccessPolicy =
  res
    "UpdateAccessPolicyResponse"
    "fixture/UpdateAccessPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAccessPolicy)

responseUpdateAccountSettings :: UpdateAccountSettingsResponse -> TestTree
responseUpdateAccountSettings =
  res
    "UpdateAccountSettingsResponse"
    "fixture/UpdateAccountSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAccountSettings)

responseUpdateCollection :: UpdateCollectionResponse -> TestTree
responseUpdateCollection =
  res
    "UpdateCollectionResponse"
    "fixture/UpdateCollectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCollection)

responseUpdateSecurityConfig :: UpdateSecurityConfigResponse -> TestTree
responseUpdateSecurityConfig =
  res
    "UpdateSecurityConfigResponse"
    "fixture/UpdateSecurityConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSecurityConfig)

responseUpdateSecurityPolicy :: UpdateSecurityPolicyResponse -> TestTree
responseUpdateSecurityPolicy =
  res
    "UpdateSecurityPolicyResponse"
    "fixture/UpdateSecurityPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSecurityPolicy)

responseUpdateVpcEndpoint :: UpdateVpcEndpointResponse -> TestTree
responseUpdateVpcEndpoint =
  res
    "UpdateVpcEndpointResponse"
    "fixture/UpdateVpcEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVpcEndpoint)

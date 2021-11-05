{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.IoTSiteWise
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.IoTSiteWise where

import Amazonka.IoTSiteWise
import qualified Data.Proxy as Proxy
import Test.AWS.Fixture
import Test.AWS.IoTSiteWise.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListProjects $
--             newListProjects
--
--         , requestDeleteProject $
--             newDeleteProject
--
--         , requestUpdateProject $
--             newUpdateProject
--
--         , requestPutLoggingOptions $
--             newPutLoggingOptions
--
--         , requestDescribeAssetModel $
--             newDescribeAssetModel
--
--         , requestDescribeAssetProperty $
--             newDescribeAssetProperty
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestGetAssetPropertyValue $
--             newGetAssetPropertyValue
--
--         , requestDeleteAccessPolicy $
--             newDeleteAccessPolicy
--
--         , requestUpdateAccessPolicy $
--             newUpdateAccessPolicy
--
--         , requestDescribeGateway $
--             newDescribeGateway
--
--         , requestDescribeAsset $
--             newDescribeAsset
--
--         , requestListDashboards $
--             newListDashboards
--
--         , requestListAccessPolicies $
--             newListAccessPolicies
--
--         , requestDescribeProject $
--             newDescribeProject
--
--         , requestGetAssetPropertyValueHistory $
--             newGetAssetPropertyValueHistory
--
--         , requestCreateDashboard $
--             newCreateDashboard
--
--         , requestCreateAccessPolicy $
--             newCreateAccessPolicy
--
--         , requestCreateAssetModel $
--             newCreateAssetModel
--
--         , requestBatchAssociateProjectAssets $
--             newBatchAssociateProjectAssets
--
--         , requestListAssetModels $
--             newListAssetModels
--
--         , requestListAssociatedAssets $
--             newListAssociatedAssets
--
--         , requestBatchPutAssetPropertyValue $
--             newBatchPutAssetPropertyValue
--
--         , requestDeleteAsset $
--             newDeleteAsset
--
--         , requestUpdateAsset $
--             newUpdateAsset
--
--         , requestDeleteGateway $
--             newDeleteGateway
--
--         , requestDescribeAccessPolicy $
--             newDescribeAccessPolicy
--
--         , requestUpdateGateway $
--             newUpdateGateway
--
--         , requestListProjectAssets $
--             newListProjectAssets
--
--         , requestCreateGateway $
--             newCreateGateway
--
--         , requestDescribeStorageConfiguration $
--             newDescribeStorageConfiguration
--
--         , requestCreateAsset $
--             newCreateAsset
--
--         , requestAssociateAssets $
--             newAssociateAssets
--
--         , requestGetInterpolatedAssetPropertyValues $
--             newGetInterpolatedAssetPropertyValues
--
--         , requestDescribeGatewayCapabilityConfiguration $
--             newDescribeGatewayCapabilityConfiguration
--
--         , requestPutDefaultEncryptionConfiguration $
--             newPutDefaultEncryptionConfiguration
--
--         , requestDeletePortal $
--             newDeletePortal
--
--         , requestListAssetRelationships $
--             newListAssetRelationships
--
--         , requestUpdatePortal $
--             newUpdatePortal
--
--         , requestListPortals $
--             newListPortals
--
--         , requestDeleteDashboard $
--             newDeleteDashboard
--
--         , requestUpdateDashboard $
--             newUpdateDashboard
--
--         , requestPutStorageConfiguration $
--             newPutStorageConfiguration
--
--         , requestCreatePortal $
--             newCreatePortal
--
--         , requestTagResource $
--             newTagResource
--
--         , requestBatchDisassociateProjectAssets $
--             newBatchDisassociateProjectAssets
--
--         , requestGetAssetPropertyAggregates $
--             newGetAssetPropertyAggregates
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDeleteAssetModel $
--             newDeleteAssetModel
--
--         , requestUpdateAssetModel $
--             newUpdateAssetModel
--
--         , requestUpdateAssetProperty $
--             newUpdateAssetProperty
--
--         , requestDescribeLoggingOptions $
--             newDescribeLoggingOptions
--
--         , requestListGateways $
--             newListGateways
--
--         , requestUpdateGatewayCapabilityConfiguration $
--             newUpdateGatewayCapabilityConfiguration
--
--         , requestDescribeDashboard $
--             newDescribeDashboard
--
--         , requestDescribePortal $
--             newDescribePortal
--
--         , requestCreateProject $
--             newCreateProject
--
--         , requestDescribeDefaultEncryptionConfiguration $
--             newDescribeDefaultEncryptionConfiguration
--
--         , requestListAssets $
--             newListAssets
--
--         , requestDisassociateAssets $
--             newDisassociateAssets
--
--           ]

--     , testGroup "response"
--         [ responseListProjects $
--             newListProjectsResponse
--
--         , responseDeleteProject $
--             newDeleteProjectResponse
--
--         , responseUpdateProject $
--             newUpdateProjectResponse
--
--         , responsePutLoggingOptions $
--             newPutLoggingOptionsResponse
--
--         , responseDescribeAssetModel $
--             newDescribeAssetModelResponse
--
--         , responseDescribeAssetProperty $
--             newDescribeAssetPropertyResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseGetAssetPropertyValue $
--             newGetAssetPropertyValueResponse
--
--         , responseDeleteAccessPolicy $
--             newDeleteAccessPolicyResponse
--
--         , responseUpdateAccessPolicy $
--             newUpdateAccessPolicyResponse
--
--         , responseDescribeGateway $
--             newDescribeGatewayResponse
--
--         , responseDescribeAsset $
--             newDescribeAssetResponse
--
--         , responseListDashboards $
--             newListDashboardsResponse
--
--         , responseListAccessPolicies $
--             newListAccessPoliciesResponse
--
--         , responseDescribeProject $
--             newDescribeProjectResponse
--
--         , responseGetAssetPropertyValueHistory $
--             newGetAssetPropertyValueHistoryResponse
--
--         , responseCreateDashboard $
--             newCreateDashboardResponse
--
--         , responseCreateAccessPolicy $
--             newCreateAccessPolicyResponse
--
--         , responseCreateAssetModel $
--             newCreateAssetModelResponse
--
--         , responseBatchAssociateProjectAssets $
--             newBatchAssociateProjectAssetsResponse
--
--         , responseListAssetModels $
--             newListAssetModelsResponse
--
--         , responseListAssociatedAssets $
--             newListAssociatedAssetsResponse
--
--         , responseBatchPutAssetPropertyValue $
--             newBatchPutAssetPropertyValueResponse
--
--         , responseDeleteAsset $
--             newDeleteAssetResponse
--
--         , responseUpdateAsset $
--             newUpdateAssetResponse
--
--         , responseDeleteGateway $
--             newDeleteGatewayResponse
--
--         , responseDescribeAccessPolicy $
--             newDescribeAccessPolicyResponse
--
--         , responseUpdateGateway $
--             newUpdateGatewayResponse
--
--         , responseListProjectAssets $
--             newListProjectAssetsResponse
--
--         , responseCreateGateway $
--             newCreateGatewayResponse
--
--         , responseDescribeStorageConfiguration $
--             newDescribeStorageConfigurationResponse
--
--         , responseCreateAsset $
--             newCreateAssetResponse
--
--         , responseAssociateAssets $
--             newAssociateAssetsResponse
--
--         , responseGetInterpolatedAssetPropertyValues $
--             newGetInterpolatedAssetPropertyValuesResponse
--
--         , responseDescribeGatewayCapabilityConfiguration $
--             newDescribeGatewayCapabilityConfigurationResponse
--
--         , responsePutDefaultEncryptionConfiguration $
--             newPutDefaultEncryptionConfigurationResponse
--
--         , responseDeletePortal $
--             newDeletePortalResponse
--
--         , responseListAssetRelationships $
--             newListAssetRelationshipsResponse
--
--         , responseUpdatePortal $
--             newUpdatePortalResponse
--
--         , responseListPortals $
--             newListPortalsResponse
--
--         , responseDeleteDashboard $
--             newDeleteDashboardResponse
--
--         , responseUpdateDashboard $
--             newUpdateDashboardResponse
--
--         , responsePutStorageConfiguration $
--             newPutStorageConfigurationResponse
--
--         , responseCreatePortal $
--             newCreatePortalResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseBatchDisassociateProjectAssets $
--             newBatchDisassociateProjectAssetsResponse
--
--         , responseGetAssetPropertyAggregates $
--             newGetAssetPropertyAggregatesResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDeleteAssetModel $
--             newDeleteAssetModelResponse
--
--         , responseUpdateAssetModel $
--             newUpdateAssetModelResponse
--
--         , responseUpdateAssetProperty $
--             newUpdateAssetPropertyResponse
--
--         , responseDescribeLoggingOptions $
--             newDescribeLoggingOptionsResponse
--
--         , responseListGateways $
--             newListGatewaysResponse
--
--         , responseUpdateGatewayCapabilityConfiguration $
--             newUpdateGatewayCapabilityConfigurationResponse
--
--         , responseDescribeDashboard $
--             newDescribeDashboardResponse
--
--         , responseDescribePortal $
--             newDescribePortalResponse
--
--         , responseCreateProject $
--             newCreateProjectResponse
--
--         , responseDescribeDefaultEncryptionConfiguration $
--             newDescribeDefaultEncryptionConfigurationResponse
--
--         , responseListAssets $
--             newListAssetsResponse
--
--         , responseDisassociateAssets $
--             newDisassociateAssetsResponse
--
--           ]
--     ]

-- Requests

requestListProjects :: ListProjects -> TestTree
requestListProjects =
  req
    "ListProjects"
    "fixture/ListProjects.yaml"

requestDeleteProject :: DeleteProject -> TestTree
requestDeleteProject =
  req
    "DeleteProject"
    "fixture/DeleteProject.yaml"

requestUpdateProject :: UpdateProject -> TestTree
requestUpdateProject =
  req
    "UpdateProject"
    "fixture/UpdateProject.yaml"

requestPutLoggingOptions :: PutLoggingOptions -> TestTree
requestPutLoggingOptions =
  req
    "PutLoggingOptions"
    "fixture/PutLoggingOptions.yaml"

requestDescribeAssetModel :: DescribeAssetModel -> TestTree
requestDescribeAssetModel =
  req
    "DescribeAssetModel"
    "fixture/DescribeAssetModel.yaml"

requestDescribeAssetProperty :: DescribeAssetProperty -> TestTree
requestDescribeAssetProperty =
  req
    "DescribeAssetProperty"
    "fixture/DescribeAssetProperty.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestGetAssetPropertyValue :: GetAssetPropertyValue -> TestTree
requestGetAssetPropertyValue =
  req
    "GetAssetPropertyValue"
    "fixture/GetAssetPropertyValue.yaml"

requestDeleteAccessPolicy :: DeleteAccessPolicy -> TestTree
requestDeleteAccessPolicy =
  req
    "DeleteAccessPolicy"
    "fixture/DeleteAccessPolicy.yaml"

requestUpdateAccessPolicy :: UpdateAccessPolicy -> TestTree
requestUpdateAccessPolicy =
  req
    "UpdateAccessPolicy"
    "fixture/UpdateAccessPolicy.yaml"

requestDescribeGateway :: DescribeGateway -> TestTree
requestDescribeGateway =
  req
    "DescribeGateway"
    "fixture/DescribeGateway.yaml"

requestDescribeAsset :: DescribeAsset -> TestTree
requestDescribeAsset =
  req
    "DescribeAsset"
    "fixture/DescribeAsset.yaml"

requestListDashboards :: ListDashboards -> TestTree
requestListDashboards =
  req
    "ListDashboards"
    "fixture/ListDashboards.yaml"

requestListAccessPolicies :: ListAccessPolicies -> TestTree
requestListAccessPolicies =
  req
    "ListAccessPolicies"
    "fixture/ListAccessPolicies.yaml"

requestDescribeProject :: DescribeProject -> TestTree
requestDescribeProject =
  req
    "DescribeProject"
    "fixture/DescribeProject.yaml"

requestGetAssetPropertyValueHistory :: GetAssetPropertyValueHistory -> TestTree
requestGetAssetPropertyValueHistory =
  req
    "GetAssetPropertyValueHistory"
    "fixture/GetAssetPropertyValueHistory.yaml"

requestCreateDashboard :: CreateDashboard -> TestTree
requestCreateDashboard =
  req
    "CreateDashboard"
    "fixture/CreateDashboard.yaml"

requestCreateAccessPolicy :: CreateAccessPolicy -> TestTree
requestCreateAccessPolicy =
  req
    "CreateAccessPolicy"
    "fixture/CreateAccessPolicy.yaml"

requestCreateAssetModel :: CreateAssetModel -> TestTree
requestCreateAssetModel =
  req
    "CreateAssetModel"
    "fixture/CreateAssetModel.yaml"

requestBatchAssociateProjectAssets :: BatchAssociateProjectAssets -> TestTree
requestBatchAssociateProjectAssets =
  req
    "BatchAssociateProjectAssets"
    "fixture/BatchAssociateProjectAssets.yaml"

requestListAssetModels :: ListAssetModels -> TestTree
requestListAssetModels =
  req
    "ListAssetModels"
    "fixture/ListAssetModels.yaml"

requestListAssociatedAssets :: ListAssociatedAssets -> TestTree
requestListAssociatedAssets =
  req
    "ListAssociatedAssets"
    "fixture/ListAssociatedAssets.yaml"

requestBatchPutAssetPropertyValue :: BatchPutAssetPropertyValue -> TestTree
requestBatchPutAssetPropertyValue =
  req
    "BatchPutAssetPropertyValue"
    "fixture/BatchPutAssetPropertyValue.yaml"

requestDeleteAsset :: DeleteAsset -> TestTree
requestDeleteAsset =
  req
    "DeleteAsset"
    "fixture/DeleteAsset.yaml"

requestUpdateAsset :: UpdateAsset -> TestTree
requestUpdateAsset =
  req
    "UpdateAsset"
    "fixture/UpdateAsset.yaml"

requestDeleteGateway :: DeleteGateway -> TestTree
requestDeleteGateway =
  req
    "DeleteGateway"
    "fixture/DeleteGateway.yaml"

requestDescribeAccessPolicy :: DescribeAccessPolicy -> TestTree
requestDescribeAccessPolicy =
  req
    "DescribeAccessPolicy"
    "fixture/DescribeAccessPolicy.yaml"

requestUpdateGateway :: UpdateGateway -> TestTree
requestUpdateGateway =
  req
    "UpdateGateway"
    "fixture/UpdateGateway.yaml"

requestListProjectAssets :: ListProjectAssets -> TestTree
requestListProjectAssets =
  req
    "ListProjectAssets"
    "fixture/ListProjectAssets.yaml"

requestCreateGateway :: CreateGateway -> TestTree
requestCreateGateway =
  req
    "CreateGateway"
    "fixture/CreateGateway.yaml"

requestDescribeStorageConfiguration :: DescribeStorageConfiguration -> TestTree
requestDescribeStorageConfiguration =
  req
    "DescribeStorageConfiguration"
    "fixture/DescribeStorageConfiguration.yaml"

requestCreateAsset :: CreateAsset -> TestTree
requestCreateAsset =
  req
    "CreateAsset"
    "fixture/CreateAsset.yaml"

requestAssociateAssets :: AssociateAssets -> TestTree
requestAssociateAssets =
  req
    "AssociateAssets"
    "fixture/AssociateAssets.yaml"

requestGetInterpolatedAssetPropertyValues :: GetInterpolatedAssetPropertyValues -> TestTree
requestGetInterpolatedAssetPropertyValues =
  req
    "GetInterpolatedAssetPropertyValues"
    "fixture/GetInterpolatedAssetPropertyValues.yaml"

requestDescribeGatewayCapabilityConfiguration :: DescribeGatewayCapabilityConfiguration -> TestTree
requestDescribeGatewayCapabilityConfiguration =
  req
    "DescribeGatewayCapabilityConfiguration"
    "fixture/DescribeGatewayCapabilityConfiguration.yaml"

requestPutDefaultEncryptionConfiguration :: PutDefaultEncryptionConfiguration -> TestTree
requestPutDefaultEncryptionConfiguration =
  req
    "PutDefaultEncryptionConfiguration"
    "fixture/PutDefaultEncryptionConfiguration.yaml"

requestDeletePortal :: DeletePortal -> TestTree
requestDeletePortal =
  req
    "DeletePortal"
    "fixture/DeletePortal.yaml"

requestListAssetRelationships :: ListAssetRelationships -> TestTree
requestListAssetRelationships =
  req
    "ListAssetRelationships"
    "fixture/ListAssetRelationships.yaml"

requestUpdatePortal :: UpdatePortal -> TestTree
requestUpdatePortal =
  req
    "UpdatePortal"
    "fixture/UpdatePortal.yaml"

requestListPortals :: ListPortals -> TestTree
requestListPortals =
  req
    "ListPortals"
    "fixture/ListPortals.yaml"

requestDeleteDashboard :: DeleteDashboard -> TestTree
requestDeleteDashboard =
  req
    "DeleteDashboard"
    "fixture/DeleteDashboard.yaml"

requestUpdateDashboard :: UpdateDashboard -> TestTree
requestUpdateDashboard =
  req
    "UpdateDashboard"
    "fixture/UpdateDashboard.yaml"

requestPutStorageConfiguration :: PutStorageConfiguration -> TestTree
requestPutStorageConfiguration =
  req
    "PutStorageConfiguration"
    "fixture/PutStorageConfiguration.yaml"

requestCreatePortal :: CreatePortal -> TestTree
requestCreatePortal =
  req
    "CreatePortal"
    "fixture/CreatePortal.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestBatchDisassociateProjectAssets :: BatchDisassociateProjectAssets -> TestTree
requestBatchDisassociateProjectAssets =
  req
    "BatchDisassociateProjectAssets"
    "fixture/BatchDisassociateProjectAssets.yaml"

requestGetAssetPropertyAggregates :: GetAssetPropertyAggregates -> TestTree
requestGetAssetPropertyAggregates =
  req
    "GetAssetPropertyAggregates"
    "fixture/GetAssetPropertyAggregates.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDeleteAssetModel :: DeleteAssetModel -> TestTree
requestDeleteAssetModel =
  req
    "DeleteAssetModel"
    "fixture/DeleteAssetModel.yaml"

requestUpdateAssetModel :: UpdateAssetModel -> TestTree
requestUpdateAssetModel =
  req
    "UpdateAssetModel"
    "fixture/UpdateAssetModel.yaml"

requestUpdateAssetProperty :: UpdateAssetProperty -> TestTree
requestUpdateAssetProperty =
  req
    "UpdateAssetProperty"
    "fixture/UpdateAssetProperty.yaml"

requestDescribeLoggingOptions :: DescribeLoggingOptions -> TestTree
requestDescribeLoggingOptions =
  req
    "DescribeLoggingOptions"
    "fixture/DescribeLoggingOptions.yaml"

requestListGateways :: ListGateways -> TestTree
requestListGateways =
  req
    "ListGateways"
    "fixture/ListGateways.yaml"

requestUpdateGatewayCapabilityConfiguration :: UpdateGatewayCapabilityConfiguration -> TestTree
requestUpdateGatewayCapabilityConfiguration =
  req
    "UpdateGatewayCapabilityConfiguration"
    "fixture/UpdateGatewayCapabilityConfiguration.yaml"

requestDescribeDashboard :: DescribeDashboard -> TestTree
requestDescribeDashboard =
  req
    "DescribeDashboard"
    "fixture/DescribeDashboard.yaml"

requestDescribePortal :: DescribePortal -> TestTree
requestDescribePortal =
  req
    "DescribePortal"
    "fixture/DescribePortal.yaml"

requestCreateProject :: CreateProject -> TestTree
requestCreateProject =
  req
    "CreateProject"
    "fixture/CreateProject.yaml"

requestDescribeDefaultEncryptionConfiguration :: DescribeDefaultEncryptionConfiguration -> TestTree
requestDescribeDefaultEncryptionConfiguration =
  req
    "DescribeDefaultEncryptionConfiguration"
    "fixture/DescribeDefaultEncryptionConfiguration.yaml"

requestListAssets :: ListAssets -> TestTree
requestListAssets =
  req
    "ListAssets"
    "fixture/ListAssets.yaml"

requestDisassociateAssets :: DisassociateAssets -> TestTree
requestDisassociateAssets =
  req
    "DisassociateAssets"
    "fixture/DisassociateAssets.yaml"

-- Responses

responseListProjects :: ListProjectsResponse -> TestTree
responseListProjects =
  res
    "ListProjectsResponse"
    "fixture/ListProjectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProjects)

responseDeleteProject :: DeleteProjectResponse -> TestTree
responseDeleteProject =
  res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProject)

responseUpdateProject :: UpdateProjectResponse -> TestTree
responseUpdateProject =
  res
    "UpdateProjectResponse"
    "fixture/UpdateProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProject)

responsePutLoggingOptions :: PutLoggingOptionsResponse -> TestTree
responsePutLoggingOptions =
  res
    "PutLoggingOptionsResponse"
    "fixture/PutLoggingOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutLoggingOptions)

responseDescribeAssetModel :: DescribeAssetModelResponse -> TestTree
responseDescribeAssetModel =
  res
    "DescribeAssetModelResponse"
    "fixture/DescribeAssetModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAssetModel)

responseDescribeAssetProperty :: DescribeAssetPropertyResponse -> TestTree
responseDescribeAssetProperty =
  res
    "DescribeAssetPropertyResponse"
    "fixture/DescribeAssetPropertyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAssetProperty)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseGetAssetPropertyValue :: GetAssetPropertyValueResponse -> TestTree
responseGetAssetPropertyValue =
  res
    "GetAssetPropertyValueResponse"
    "fixture/GetAssetPropertyValueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAssetPropertyValue)

responseDeleteAccessPolicy :: DeleteAccessPolicyResponse -> TestTree
responseDeleteAccessPolicy =
  res
    "DeleteAccessPolicyResponse"
    "fixture/DeleteAccessPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAccessPolicy)

responseUpdateAccessPolicy :: UpdateAccessPolicyResponse -> TestTree
responseUpdateAccessPolicy =
  res
    "UpdateAccessPolicyResponse"
    "fixture/UpdateAccessPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAccessPolicy)

responseDescribeGateway :: DescribeGatewayResponse -> TestTree
responseDescribeGateway =
  res
    "DescribeGatewayResponse"
    "fixture/DescribeGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGateway)

responseDescribeAsset :: DescribeAssetResponse -> TestTree
responseDescribeAsset =
  res
    "DescribeAssetResponse"
    "fixture/DescribeAssetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAsset)

responseListDashboards :: ListDashboardsResponse -> TestTree
responseListDashboards =
  res
    "ListDashboardsResponse"
    "fixture/ListDashboardsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDashboards)

responseListAccessPolicies :: ListAccessPoliciesResponse -> TestTree
responseListAccessPolicies =
  res
    "ListAccessPoliciesResponse"
    "fixture/ListAccessPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccessPolicies)

responseDescribeProject :: DescribeProjectResponse -> TestTree
responseDescribeProject =
  res
    "DescribeProjectResponse"
    "fixture/DescribeProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProject)

responseGetAssetPropertyValueHistory :: GetAssetPropertyValueHistoryResponse -> TestTree
responseGetAssetPropertyValueHistory =
  res
    "GetAssetPropertyValueHistoryResponse"
    "fixture/GetAssetPropertyValueHistoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAssetPropertyValueHistory)

responseCreateDashboard :: CreateDashboardResponse -> TestTree
responseCreateDashboard =
  res
    "CreateDashboardResponse"
    "fixture/CreateDashboardResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDashboard)

responseCreateAccessPolicy :: CreateAccessPolicyResponse -> TestTree
responseCreateAccessPolicy =
  res
    "CreateAccessPolicyResponse"
    "fixture/CreateAccessPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAccessPolicy)

responseCreateAssetModel :: CreateAssetModelResponse -> TestTree
responseCreateAssetModel =
  res
    "CreateAssetModelResponse"
    "fixture/CreateAssetModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAssetModel)

responseBatchAssociateProjectAssets :: BatchAssociateProjectAssetsResponse -> TestTree
responseBatchAssociateProjectAssets =
  res
    "BatchAssociateProjectAssetsResponse"
    "fixture/BatchAssociateProjectAssetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchAssociateProjectAssets)

responseListAssetModels :: ListAssetModelsResponse -> TestTree
responseListAssetModels =
  res
    "ListAssetModelsResponse"
    "fixture/ListAssetModelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssetModels)

responseListAssociatedAssets :: ListAssociatedAssetsResponse -> TestTree
responseListAssociatedAssets =
  res
    "ListAssociatedAssetsResponse"
    "fixture/ListAssociatedAssetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssociatedAssets)

responseBatchPutAssetPropertyValue :: BatchPutAssetPropertyValueResponse -> TestTree
responseBatchPutAssetPropertyValue =
  res
    "BatchPutAssetPropertyValueResponse"
    "fixture/BatchPutAssetPropertyValueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchPutAssetPropertyValue)

responseDeleteAsset :: DeleteAssetResponse -> TestTree
responseDeleteAsset =
  res
    "DeleteAssetResponse"
    "fixture/DeleteAssetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAsset)

responseUpdateAsset :: UpdateAssetResponse -> TestTree
responseUpdateAsset =
  res
    "UpdateAssetResponse"
    "fixture/UpdateAssetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAsset)

responseDeleteGateway :: DeleteGatewayResponse -> TestTree
responseDeleteGateway =
  res
    "DeleteGatewayResponse"
    "fixture/DeleteGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGateway)

responseDescribeAccessPolicy :: DescribeAccessPolicyResponse -> TestTree
responseDescribeAccessPolicy =
  res
    "DescribeAccessPolicyResponse"
    "fixture/DescribeAccessPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccessPolicy)

responseUpdateGateway :: UpdateGatewayResponse -> TestTree
responseUpdateGateway =
  res
    "UpdateGatewayResponse"
    "fixture/UpdateGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGateway)

responseListProjectAssets :: ListProjectAssetsResponse -> TestTree
responseListProjectAssets =
  res
    "ListProjectAssetsResponse"
    "fixture/ListProjectAssetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProjectAssets)

responseCreateGateway :: CreateGatewayResponse -> TestTree
responseCreateGateway =
  res
    "CreateGatewayResponse"
    "fixture/CreateGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGateway)

responseDescribeStorageConfiguration :: DescribeStorageConfigurationResponse -> TestTree
responseDescribeStorageConfiguration =
  res
    "DescribeStorageConfigurationResponse"
    "fixture/DescribeStorageConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStorageConfiguration)

responseCreateAsset :: CreateAssetResponse -> TestTree
responseCreateAsset =
  res
    "CreateAssetResponse"
    "fixture/CreateAssetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAsset)

responseAssociateAssets :: AssociateAssetsResponse -> TestTree
responseAssociateAssets =
  res
    "AssociateAssetsResponse"
    "fixture/AssociateAssetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateAssets)

responseGetInterpolatedAssetPropertyValues :: GetInterpolatedAssetPropertyValuesResponse -> TestTree
responseGetInterpolatedAssetPropertyValues =
  res
    "GetInterpolatedAssetPropertyValuesResponse"
    "fixture/GetInterpolatedAssetPropertyValuesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInterpolatedAssetPropertyValues)

responseDescribeGatewayCapabilityConfiguration :: DescribeGatewayCapabilityConfigurationResponse -> TestTree
responseDescribeGatewayCapabilityConfiguration =
  res
    "DescribeGatewayCapabilityConfigurationResponse"
    "fixture/DescribeGatewayCapabilityConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGatewayCapabilityConfiguration)

responsePutDefaultEncryptionConfiguration :: PutDefaultEncryptionConfigurationResponse -> TestTree
responsePutDefaultEncryptionConfiguration =
  res
    "PutDefaultEncryptionConfigurationResponse"
    "fixture/PutDefaultEncryptionConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutDefaultEncryptionConfiguration)

responseDeletePortal :: DeletePortalResponse -> TestTree
responseDeletePortal =
  res
    "DeletePortalResponse"
    "fixture/DeletePortalResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePortal)

responseListAssetRelationships :: ListAssetRelationshipsResponse -> TestTree
responseListAssetRelationships =
  res
    "ListAssetRelationshipsResponse"
    "fixture/ListAssetRelationshipsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssetRelationships)

responseUpdatePortal :: UpdatePortalResponse -> TestTree
responseUpdatePortal =
  res
    "UpdatePortalResponse"
    "fixture/UpdatePortalResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePortal)

responseListPortals :: ListPortalsResponse -> TestTree
responseListPortals =
  res
    "ListPortalsResponse"
    "fixture/ListPortalsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPortals)

responseDeleteDashboard :: DeleteDashboardResponse -> TestTree
responseDeleteDashboard =
  res
    "DeleteDashboardResponse"
    "fixture/DeleteDashboardResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDashboard)

responseUpdateDashboard :: UpdateDashboardResponse -> TestTree
responseUpdateDashboard =
  res
    "UpdateDashboardResponse"
    "fixture/UpdateDashboardResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDashboard)

responsePutStorageConfiguration :: PutStorageConfigurationResponse -> TestTree
responsePutStorageConfiguration =
  res
    "PutStorageConfigurationResponse"
    "fixture/PutStorageConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutStorageConfiguration)

responseCreatePortal :: CreatePortalResponse -> TestTree
responseCreatePortal =
  res
    "CreatePortalResponse"
    "fixture/CreatePortalResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePortal)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseBatchDisassociateProjectAssets :: BatchDisassociateProjectAssetsResponse -> TestTree
responseBatchDisassociateProjectAssets =
  res
    "BatchDisassociateProjectAssetsResponse"
    "fixture/BatchDisassociateProjectAssetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDisassociateProjectAssets)

responseGetAssetPropertyAggregates :: GetAssetPropertyAggregatesResponse -> TestTree
responseGetAssetPropertyAggregates =
  res
    "GetAssetPropertyAggregatesResponse"
    "fixture/GetAssetPropertyAggregatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAssetPropertyAggregates)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseDeleteAssetModel :: DeleteAssetModelResponse -> TestTree
responseDeleteAssetModel =
  res
    "DeleteAssetModelResponse"
    "fixture/DeleteAssetModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAssetModel)

responseUpdateAssetModel :: UpdateAssetModelResponse -> TestTree
responseUpdateAssetModel =
  res
    "UpdateAssetModelResponse"
    "fixture/UpdateAssetModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAssetModel)

responseUpdateAssetProperty :: UpdateAssetPropertyResponse -> TestTree
responseUpdateAssetProperty =
  res
    "UpdateAssetPropertyResponse"
    "fixture/UpdateAssetPropertyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAssetProperty)

responseDescribeLoggingOptions :: DescribeLoggingOptionsResponse -> TestTree
responseDescribeLoggingOptions =
  res
    "DescribeLoggingOptionsResponse"
    "fixture/DescribeLoggingOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLoggingOptions)

responseListGateways :: ListGatewaysResponse -> TestTree
responseListGateways =
  res
    "ListGatewaysResponse"
    "fixture/ListGatewaysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGateways)

responseUpdateGatewayCapabilityConfiguration :: UpdateGatewayCapabilityConfigurationResponse -> TestTree
responseUpdateGatewayCapabilityConfiguration =
  res
    "UpdateGatewayCapabilityConfigurationResponse"
    "fixture/UpdateGatewayCapabilityConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGatewayCapabilityConfiguration)

responseDescribeDashboard :: DescribeDashboardResponse -> TestTree
responseDescribeDashboard =
  res
    "DescribeDashboardResponse"
    "fixture/DescribeDashboardResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDashboard)

responseDescribePortal :: DescribePortalResponse -> TestTree
responseDescribePortal =
  res
    "DescribePortalResponse"
    "fixture/DescribePortalResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePortal)

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject =
  res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProject)

responseDescribeDefaultEncryptionConfiguration :: DescribeDefaultEncryptionConfigurationResponse -> TestTree
responseDescribeDefaultEncryptionConfiguration =
  res
    "DescribeDefaultEncryptionConfigurationResponse"
    "fixture/DescribeDefaultEncryptionConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDefaultEncryptionConfiguration)

responseListAssets :: ListAssetsResponse -> TestTree
responseListAssets =
  res
    "ListAssetsResponse"
    "fixture/ListAssetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssets)

responseDisassociateAssets :: DisassociateAssetsResponse -> TestTree
responseDisassociateAssets =
  res
    "DisassociateAssetsResponse"
    "fixture/DisassociateAssetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateAssets)

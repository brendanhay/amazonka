{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.IoTSiteWise
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.IoTSiteWise where

import Amazonka.IoTSiteWise
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.IoTSiteWise.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateAssets $
--             newAssociateAssets
--
--         , requestAssociateTimeSeriesToAssetProperty $
--             newAssociateTimeSeriesToAssetProperty
--
--         , requestBatchAssociateProjectAssets $
--             newBatchAssociateProjectAssets
--
--         , requestBatchDisassociateProjectAssets $
--             newBatchDisassociateProjectAssets
--
--         , requestBatchGetAssetPropertyAggregates $
--             newBatchGetAssetPropertyAggregates
--
--         , requestBatchGetAssetPropertyValue $
--             newBatchGetAssetPropertyValue
--
--         , requestBatchGetAssetPropertyValueHistory $
--             newBatchGetAssetPropertyValueHistory
--
--         , requestBatchPutAssetPropertyValue $
--             newBatchPutAssetPropertyValue
--
--         , requestCreateAccessPolicy $
--             newCreateAccessPolicy
--
--         , requestCreateAsset $
--             newCreateAsset
--
--         , requestCreateAssetModel $
--             newCreateAssetModel
--
--         , requestCreateBulkImportJob $
--             newCreateBulkImportJob
--
--         , requestCreateDashboard $
--             newCreateDashboard
--
--         , requestCreateGateway $
--             newCreateGateway
--
--         , requestCreatePortal $
--             newCreatePortal
--
--         , requestCreateProject $
--             newCreateProject
--
--         , requestDeleteAccessPolicy $
--             newDeleteAccessPolicy
--
--         , requestDeleteAsset $
--             newDeleteAsset
--
--         , requestDeleteAssetModel $
--             newDeleteAssetModel
--
--         , requestDeleteDashboard $
--             newDeleteDashboard
--
--         , requestDeleteGateway $
--             newDeleteGateway
--
--         , requestDeletePortal $
--             newDeletePortal
--
--         , requestDeleteProject $
--             newDeleteProject
--
--         , requestDeleteTimeSeries $
--             newDeleteTimeSeries
--
--         , requestDescribeAccessPolicy $
--             newDescribeAccessPolicy
--
--         , requestDescribeAsset $
--             newDescribeAsset
--
--         , requestDescribeAssetModel $
--             newDescribeAssetModel
--
--         , requestDescribeAssetProperty $
--             newDescribeAssetProperty
--
--         , requestDescribeBulkImportJob $
--             newDescribeBulkImportJob
--
--         , requestDescribeDashboard $
--             newDescribeDashboard
--
--         , requestDescribeDefaultEncryptionConfiguration $
--             newDescribeDefaultEncryptionConfiguration
--
--         , requestDescribeGateway $
--             newDescribeGateway
--
--         , requestDescribeGatewayCapabilityConfiguration $
--             newDescribeGatewayCapabilityConfiguration
--
--         , requestDescribeLoggingOptions $
--             newDescribeLoggingOptions
--
--         , requestDescribePortal $
--             newDescribePortal
--
--         , requestDescribeProject $
--             newDescribeProject
--
--         , requestDescribeStorageConfiguration $
--             newDescribeStorageConfiguration
--
--         , requestDescribeTimeSeries $
--             newDescribeTimeSeries
--
--         , requestDisassociateAssets $
--             newDisassociateAssets
--
--         , requestDisassociateTimeSeriesFromAssetProperty $
--             newDisassociateTimeSeriesFromAssetProperty
--
--         , requestGetAssetPropertyAggregates $
--             newGetAssetPropertyAggregates
--
--         , requestGetAssetPropertyValue $
--             newGetAssetPropertyValue
--
--         , requestGetAssetPropertyValueHistory $
--             newGetAssetPropertyValueHistory
--
--         , requestGetInterpolatedAssetPropertyValues $
--             newGetInterpolatedAssetPropertyValues
--
--         , requestListAccessPolicies $
--             newListAccessPolicies
--
--         , requestListAssetModelProperties $
--             newListAssetModelProperties
--
--         , requestListAssetModels $
--             newListAssetModels
--
--         , requestListAssetProperties $
--             newListAssetProperties
--
--         , requestListAssetRelationships $
--             newListAssetRelationships
--
--         , requestListAssets $
--             newListAssets
--
--         , requestListAssociatedAssets $
--             newListAssociatedAssets
--
--         , requestListBulkImportJobs $
--             newListBulkImportJobs
--
--         , requestListDashboards $
--             newListDashboards
--
--         , requestListGateways $
--             newListGateways
--
--         , requestListPortals $
--             newListPortals
--
--         , requestListProjectAssets $
--             newListProjectAssets
--
--         , requestListProjects $
--             newListProjects
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListTimeSeries $
--             newListTimeSeries
--
--         , requestPutDefaultEncryptionConfiguration $
--             newPutDefaultEncryptionConfiguration
--
--         , requestPutLoggingOptions $
--             newPutLoggingOptions
--
--         , requestPutStorageConfiguration $
--             newPutStorageConfiguration
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
--         , requestUpdateAsset $
--             newUpdateAsset
--
--         , requestUpdateAssetModel $
--             newUpdateAssetModel
--
--         , requestUpdateAssetProperty $
--             newUpdateAssetProperty
--
--         , requestUpdateDashboard $
--             newUpdateDashboard
--
--         , requestUpdateGateway $
--             newUpdateGateway
--
--         , requestUpdateGatewayCapabilityConfiguration $
--             newUpdateGatewayCapabilityConfiguration
--
--         , requestUpdatePortal $
--             newUpdatePortal
--
--         , requestUpdateProject $
--             newUpdateProject
--
--           ]

--     , testGroup "response"
--         [ responseAssociateAssets $
--             newAssociateAssetsResponse
--
--         , responseAssociateTimeSeriesToAssetProperty $
--             newAssociateTimeSeriesToAssetPropertyResponse
--
--         , responseBatchAssociateProjectAssets $
--             newBatchAssociateProjectAssetsResponse
--
--         , responseBatchDisassociateProjectAssets $
--             newBatchDisassociateProjectAssetsResponse
--
--         , responseBatchGetAssetPropertyAggregates $
--             newBatchGetAssetPropertyAggregatesResponse
--
--         , responseBatchGetAssetPropertyValue $
--             newBatchGetAssetPropertyValueResponse
--
--         , responseBatchGetAssetPropertyValueHistory $
--             newBatchGetAssetPropertyValueHistoryResponse
--
--         , responseBatchPutAssetPropertyValue $
--             newBatchPutAssetPropertyValueResponse
--
--         , responseCreateAccessPolicy $
--             newCreateAccessPolicyResponse
--
--         , responseCreateAsset $
--             newCreateAssetResponse
--
--         , responseCreateAssetModel $
--             newCreateAssetModelResponse
--
--         , responseCreateBulkImportJob $
--             newCreateBulkImportJobResponse
--
--         , responseCreateDashboard $
--             newCreateDashboardResponse
--
--         , responseCreateGateway $
--             newCreateGatewayResponse
--
--         , responseCreatePortal $
--             newCreatePortalResponse
--
--         , responseCreateProject $
--             newCreateProjectResponse
--
--         , responseDeleteAccessPolicy $
--             newDeleteAccessPolicyResponse
--
--         , responseDeleteAsset $
--             newDeleteAssetResponse
--
--         , responseDeleteAssetModel $
--             newDeleteAssetModelResponse
--
--         , responseDeleteDashboard $
--             newDeleteDashboardResponse
--
--         , responseDeleteGateway $
--             newDeleteGatewayResponse
--
--         , responseDeletePortal $
--             newDeletePortalResponse
--
--         , responseDeleteProject $
--             newDeleteProjectResponse
--
--         , responseDeleteTimeSeries $
--             newDeleteTimeSeriesResponse
--
--         , responseDescribeAccessPolicy $
--             newDescribeAccessPolicyResponse
--
--         , responseDescribeAsset $
--             newDescribeAssetResponse
--
--         , responseDescribeAssetModel $
--             newDescribeAssetModelResponse
--
--         , responseDescribeAssetProperty $
--             newDescribeAssetPropertyResponse
--
--         , responseDescribeBulkImportJob $
--             newDescribeBulkImportJobResponse
--
--         , responseDescribeDashboard $
--             newDescribeDashboardResponse
--
--         , responseDescribeDefaultEncryptionConfiguration $
--             newDescribeDefaultEncryptionConfigurationResponse
--
--         , responseDescribeGateway $
--             newDescribeGatewayResponse
--
--         , responseDescribeGatewayCapabilityConfiguration $
--             newDescribeGatewayCapabilityConfigurationResponse
--
--         , responseDescribeLoggingOptions $
--             newDescribeLoggingOptionsResponse
--
--         , responseDescribePortal $
--             newDescribePortalResponse
--
--         , responseDescribeProject $
--             newDescribeProjectResponse
--
--         , responseDescribeStorageConfiguration $
--             newDescribeStorageConfigurationResponse
--
--         , responseDescribeTimeSeries $
--             newDescribeTimeSeriesResponse
--
--         , responseDisassociateAssets $
--             newDisassociateAssetsResponse
--
--         , responseDisassociateTimeSeriesFromAssetProperty $
--             newDisassociateTimeSeriesFromAssetPropertyResponse
--
--         , responseGetAssetPropertyAggregates $
--             newGetAssetPropertyAggregatesResponse
--
--         , responseGetAssetPropertyValue $
--             newGetAssetPropertyValueResponse
--
--         , responseGetAssetPropertyValueHistory $
--             newGetAssetPropertyValueHistoryResponse
--
--         , responseGetInterpolatedAssetPropertyValues $
--             newGetInterpolatedAssetPropertyValuesResponse
--
--         , responseListAccessPolicies $
--             newListAccessPoliciesResponse
--
--         , responseListAssetModelProperties $
--             newListAssetModelPropertiesResponse
--
--         , responseListAssetModels $
--             newListAssetModelsResponse
--
--         , responseListAssetProperties $
--             newListAssetPropertiesResponse
--
--         , responseListAssetRelationships $
--             newListAssetRelationshipsResponse
--
--         , responseListAssets $
--             newListAssetsResponse
--
--         , responseListAssociatedAssets $
--             newListAssociatedAssetsResponse
--
--         , responseListBulkImportJobs $
--             newListBulkImportJobsResponse
--
--         , responseListDashboards $
--             newListDashboardsResponse
--
--         , responseListGateways $
--             newListGatewaysResponse
--
--         , responseListPortals $
--             newListPortalsResponse
--
--         , responseListProjectAssets $
--             newListProjectAssetsResponse
--
--         , responseListProjects $
--             newListProjectsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListTimeSeries $
--             newListTimeSeriesResponse
--
--         , responsePutDefaultEncryptionConfiguration $
--             newPutDefaultEncryptionConfigurationResponse
--
--         , responsePutLoggingOptions $
--             newPutLoggingOptionsResponse
--
--         , responsePutStorageConfiguration $
--             newPutStorageConfigurationResponse
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
--         , responseUpdateAsset $
--             newUpdateAssetResponse
--
--         , responseUpdateAssetModel $
--             newUpdateAssetModelResponse
--
--         , responseUpdateAssetProperty $
--             newUpdateAssetPropertyResponse
--
--         , responseUpdateDashboard $
--             newUpdateDashboardResponse
--
--         , responseUpdateGateway $
--             newUpdateGatewayResponse
--
--         , responseUpdateGatewayCapabilityConfiguration $
--             newUpdateGatewayCapabilityConfigurationResponse
--
--         , responseUpdatePortal $
--             newUpdatePortalResponse
--
--         , responseUpdateProject $
--             newUpdateProjectResponse
--
--           ]
--     ]

-- Requests

requestAssociateAssets :: AssociateAssets -> TestTree
requestAssociateAssets =
  req
    "AssociateAssets"
    "fixture/AssociateAssets.yaml"

requestAssociateTimeSeriesToAssetProperty :: AssociateTimeSeriesToAssetProperty -> TestTree
requestAssociateTimeSeriesToAssetProperty =
  req
    "AssociateTimeSeriesToAssetProperty"
    "fixture/AssociateTimeSeriesToAssetProperty.yaml"

requestBatchAssociateProjectAssets :: BatchAssociateProjectAssets -> TestTree
requestBatchAssociateProjectAssets =
  req
    "BatchAssociateProjectAssets"
    "fixture/BatchAssociateProjectAssets.yaml"

requestBatchDisassociateProjectAssets :: BatchDisassociateProjectAssets -> TestTree
requestBatchDisassociateProjectAssets =
  req
    "BatchDisassociateProjectAssets"
    "fixture/BatchDisassociateProjectAssets.yaml"

requestBatchGetAssetPropertyAggregates :: BatchGetAssetPropertyAggregates -> TestTree
requestBatchGetAssetPropertyAggregates =
  req
    "BatchGetAssetPropertyAggregates"
    "fixture/BatchGetAssetPropertyAggregates.yaml"

requestBatchGetAssetPropertyValue :: BatchGetAssetPropertyValue -> TestTree
requestBatchGetAssetPropertyValue =
  req
    "BatchGetAssetPropertyValue"
    "fixture/BatchGetAssetPropertyValue.yaml"

requestBatchGetAssetPropertyValueHistory :: BatchGetAssetPropertyValueHistory -> TestTree
requestBatchGetAssetPropertyValueHistory =
  req
    "BatchGetAssetPropertyValueHistory"
    "fixture/BatchGetAssetPropertyValueHistory.yaml"

requestBatchPutAssetPropertyValue :: BatchPutAssetPropertyValue -> TestTree
requestBatchPutAssetPropertyValue =
  req
    "BatchPutAssetPropertyValue"
    "fixture/BatchPutAssetPropertyValue.yaml"

requestCreateAccessPolicy :: CreateAccessPolicy -> TestTree
requestCreateAccessPolicy =
  req
    "CreateAccessPolicy"
    "fixture/CreateAccessPolicy.yaml"

requestCreateAsset :: CreateAsset -> TestTree
requestCreateAsset =
  req
    "CreateAsset"
    "fixture/CreateAsset.yaml"

requestCreateAssetModel :: CreateAssetModel -> TestTree
requestCreateAssetModel =
  req
    "CreateAssetModel"
    "fixture/CreateAssetModel.yaml"

requestCreateBulkImportJob :: CreateBulkImportJob -> TestTree
requestCreateBulkImportJob =
  req
    "CreateBulkImportJob"
    "fixture/CreateBulkImportJob.yaml"

requestCreateDashboard :: CreateDashboard -> TestTree
requestCreateDashboard =
  req
    "CreateDashboard"
    "fixture/CreateDashboard.yaml"

requestCreateGateway :: CreateGateway -> TestTree
requestCreateGateway =
  req
    "CreateGateway"
    "fixture/CreateGateway.yaml"

requestCreatePortal :: CreatePortal -> TestTree
requestCreatePortal =
  req
    "CreatePortal"
    "fixture/CreatePortal.yaml"

requestCreateProject :: CreateProject -> TestTree
requestCreateProject =
  req
    "CreateProject"
    "fixture/CreateProject.yaml"

requestDeleteAccessPolicy :: DeleteAccessPolicy -> TestTree
requestDeleteAccessPolicy =
  req
    "DeleteAccessPolicy"
    "fixture/DeleteAccessPolicy.yaml"

requestDeleteAsset :: DeleteAsset -> TestTree
requestDeleteAsset =
  req
    "DeleteAsset"
    "fixture/DeleteAsset.yaml"

requestDeleteAssetModel :: DeleteAssetModel -> TestTree
requestDeleteAssetModel =
  req
    "DeleteAssetModel"
    "fixture/DeleteAssetModel.yaml"

requestDeleteDashboard :: DeleteDashboard -> TestTree
requestDeleteDashboard =
  req
    "DeleteDashboard"
    "fixture/DeleteDashboard.yaml"

requestDeleteGateway :: DeleteGateway -> TestTree
requestDeleteGateway =
  req
    "DeleteGateway"
    "fixture/DeleteGateway.yaml"

requestDeletePortal :: DeletePortal -> TestTree
requestDeletePortal =
  req
    "DeletePortal"
    "fixture/DeletePortal.yaml"

requestDeleteProject :: DeleteProject -> TestTree
requestDeleteProject =
  req
    "DeleteProject"
    "fixture/DeleteProject.yaml"

requestDeleteTimeSeries :: DeleteTimeSeries -> TestTree
requestDeleteTimeSeries =
  req
    "DeleteTimeSeries"
    "fixture/DeleteTimeSeries.yaml"

requestDescribeAccessPolicy :: DescribeAccessPolicy -> TestTree
requestDescribeAccessPolicy =
  req
    "DescribeAccessPolicy"
    "fixture/DescribeAccessPolicy.yaml"

requestDescribeAsset :: DescribeAsset -> TestTree
requestDescribeAsset =
  req
    "DescribeAsset"
    "fixture/DescribeAsset.yaml"

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

requestDescribeBulkImportJob :: DescribeBulkImportJob -> TestTree
requestDescribeBulkImportJob =
  req
    "DescribeBulkImportJob"
    "fixture/DescribeBulkImportJob.yaml"

requestDescribeDashboard :: DescribeDashboard -> TestTree
requestDescribeDashboard =
  req
    "DescribeDashboard"
    "fixture/DescribeDashboard.yaml"

requestDescribeDefaultEncryptionConfiguration :: DescribeDefaultEncryptionConfiguration -> TestTree
requestDescribeDefaultEncryptionConfiguration =
  req
    "DescribeDefaultEncryptionConfiguration"
    "fixture/DescribeDefaultEncryptionConfiguration.yaml"

requestDescribeGateway :: DescribeGateway -> TestTree
requestDescribeGateway =
  req
    "DescribeGateway"
    "fixture/DescribeGateway.yaml"

requestDescribeGatewayCapabilityConfiguration :: DescribeGatewayCapabilityConfiguration -> TestTree
requestDescribeGatewayCapabilityConfiguration =
  req
    "DescribeGatewayCapabilityConfiguration"
    "fixture/DescribeGatewayCapabilityConfiguration.yaml"

requestDescribeLoggingOptions :: DescribeLoggingOptions -> TestTree
requestDescribeLoggingOptions =
  req
    "DescribeLoggingOptions"
    "fixture/DescribeLoggingOptions.yaml"

requestDescribePortal :: DescribePortal -> TestTree
requestDescribePortal =
  req
    "DescribePortal"
    "fixture/DescribePortal.yaml"

requestDescribeProject :: DescribeProject -> TestTree
requestDescribeProject =
  req
    "DescribeProject"
    "fixture/DescribeProject.yaml"

requestDescribeStorageConfiguration :: DescribeStorageConfiguration -> TestTree
requestDescribeStorageConfiguration =
  req
    "DescribeStorageConfiguration"
    "fixture/DescribeStorageConfiguration.yaml"

requestDescribeTimeSeries :: DescribeTimeSeries -> TestTree
requestDescribeTimeSeries =
  req
    "DescribeTimeSeries"
    "fixture/DescribeTimeSeries.yaml"

requestDisassociateAssets :: DisassociateAssets -> TestTree
requestDisassociateAssets =
  req
    "DisassociateAssets"
    "fixture/DisassociateAssets.yaml"

requestDisassociateTimeSeriesFromAssetProperty :: DisassociateTimeSeriesFromAssetProperty -> TestTree
requestDisassociateTimeSeriesFromAssetProperty =
  req
    "DisassociateTimeSeriesFromAssetProperty"
    "fixture/DisassociateTimeSeriesFromAssetProperty.yaml"

requestGetAssetPropertyAggregates :: GetAssetPropertyAggregates -> TestTree
requestGetAssetPropertyAggregates =
  req
    "GetAssetPropertyAggregates"
    "fixture/GetAssetPropertyAggregates.yaml"

requestGetAssetPropertyValue :: GetAssetPropertyValue -> TestTree
requestGetAssetPropertyValue =
  req
    "GetAssetPropertyValue"
    "fixture/GetAssetPropertyValue.yaml"

requestGetAssetPropertyValueHistory :: GetAssetPropertyValueHistory -> TestTree
requestGetAssetPropertyValueHistory =
  req
    "GetAssetPropertyValueHistory"
    "fixture/GetAssetPropertyValueHistory.yaml"

requestGetInterpolatedAssetPropertyValues :: GetInterpolatedAssetPropertyValues -> TestTree
requestGetInterpolatedAssetPropertyValues =
  req
    "GetInterpolatedAssetPropertyValues"
    "fixture/GetInterpolatedAssetPropertyValues.yaml"

requestListAccessPolicies :: ListAccessPolicies -> TestTree
requestListAccessPolicies =
  req
    "ListAccessPolicies"
    "fixture/ListAccessPolicies.yaml"

requestListAssetModelProperties :: ListAssetModelProperties -> TestTree
requestListAssetModelProperties =
  req
    "ListAssetModelProperties"
    "fixture/ListAssetModelProperties.yaml"

requestListAssetModels :: ListAssetModels -> TestTree
requestListAssetModels =
  req
    "ListAssetModels"
    "fixture/ListAssetModels.yaml"

requestListAssetProperties :: ListAssetProperties -> TestTree
requestListAssetProperties =
  req
    "ListAssetProperties"
    "fixture/ListAssetProperties.yaml"

requestListAssetRelationships :: ListAssetRelationships -> TestTree
requestListAssetRelationships =
  req
    "ListAssetRelationships"
    "fixture/ListAssetRelationships.yaml"

requestListAssets :: ListAssets -> TestTree
requestListAssets =
  req
    "ListAssets"
    "fixture/ListAssets.yaml"

requestListAssociatedAssets :: ListAssociatedAssets -> TestTree
requestListAssociatedAssets =
  req
    "ListAssociatedAssets"
    "fixture/ListAssociatedAssets.yaml"

requestListBulkImportJobs :: ListBulkImportJobs -> TestTree
requestListBulkImportJobs =
  req
    "ListBulkImportJobs"
    "fixture/ListBulkImportJobs.yaml"

requestListDashboards :: ListDashboards -> TestTree
requestListDashboards =
  req
    "ListDashboards"
    "fixture/ListDashboards.yaml"

requestListGateways :: ListGateways -> TestTree
requestListGateways =
  req
    "ListGateways"
    "fixture/ListGateways.yaml"

requestListPortals :: ListPortals -> TestTree
requestListPortals =
  req
    "ListPortals"
    "fixture/ListPortals.yaml"

requestListProjectAssets :: ListProjectAssets -> TestTree
requestListProjectAssets =
  req
    "ListProjectAssets"
    "fixture/ListProjectAssets.yaml"

requestListProjects :: ListProjects -> TestTree
requestListProjects =
  req
    "ListProjects"
    "fixture/ListProjects.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListTimeSeries :: ListTimeSeries -> TestTree
requestListTimeSeries =
  req
    "ListTimeSeries"
    "fixture/ListTimeSeries.yaml"

requestPutDefaultEncryptionConfiguration :: PutDefaultEncryptionConfiguration -> TestTree
requestPutDefaultEncryptionConfiguration =
  req
    "PutDefaultEncryptionConfiguration"
    "fixture/PutDefaultEncryptionConfiguration.yaml"

requestPutLoggingOptions :: PutLoggingOptions -> TestTree
requestPutLoggingOptions =
  req
    "PutLoggingOptions"
    "fixture/PutLoggingOptions.yaml"

requestPutStorageConfiguration :: PutStorageConfiguration -> TestTree
requestPutStorageConfiguration =
  req
    "PutStorageConfiguration"
    "fixture/PutStorageConfiguration.yaml"

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

requestUpdateAsset :: UpdateAsset -> TestTree
requestUpdateAsset =
  req
    "UpdateAsset"
    "fixture/UpdateAsset.yaml"

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

requestUpdateDashboard :: UpdateDashboard -> TestTree
requestUpdateDashboard =
  req
    "UpdateDashboard"
    "fixture/UpdateDashboard.yaml"

requestUpdateGateway :: UpdateGateway -> TestTree
requestUpdateGateway =
  req
    "UpdateGateway"
    "fixture/UpdateGateway.yaml"

requestUpdateGatewayCapabilityConfiguration :: UpdateGatewayCapabilityConfiguration -> TestTree
requestUpdateGatewayCapabilityConfiguration =
  req
    "UpdateGatewayCapabilityConfiguration"
    "fixture/UpdateGatewayCapabilityConfiguration.yaml"

requestUpdatePortal :: UpdatePortal -> TestTree
requestUpdatePortal =
  req
    "UpdatePortal"
    "fixture/UpdatePortal.yaml"

requestUpdateProject :: UpdateProject -> TestTree
requestUpdateProject =
  req
    "UpdateProject"
    "fixture/UpdateProject.yaml"

-- Responses

responseAssociateAssets :: AssociateAssetsResponse -> TestTree
responseAssociateAssets =
  res
    "AssociateAssetsResponse"
    "fixture/AssociateAssetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateAssets)

responseAssociateTimeSeriesToAssetProperty :: AssociateTimeSeriesToAssetPropertyResponse -> TestTree
responseAssociateTimeSeriesToAssetProperty =
  res
    "AssociateTimeSeriesToAssetPropertyResponse"
    "fixture/AssociateTimeSeriesToAssetPropertyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateTimeSeriesToAssetProperty)

responseBatchAssociateProjectAssets :: BatchAssociateProjectAssetsResponse -> TestTree
responseBatchAssociateProjectAssets =
  res
    "BatchAssociateProjectAssetsResponse"
    "fixture/BatchAssociateProjectAssetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchAssociateProjectAssets)

responseBatchDisassociateProjectAssets :: BatchDisassociateProjectAssetsResponse -> TestTree
responseBatchDisassociateProjectAssets =
  res
    "BatchDisassociateProjectAssetsResponse"
    "fixture/BatchDisassociateProjectAssetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDisassociateProjectAssets)

responseBatchGetAssetPropertyAggregates :: BatchGetAssetPropertyAggregatesResponse -> TestTree
responseBatchGetAssetPropertyAggregates =
  res
    "BatchGetAssetPropertyAggregatesResponse"
    "fixture/BatchGetAssetPropertyAggregatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetAssetPropertyAggregates)

responseBatchGetAssetPropertyValue :: BatchGetAssetPropertyValueResponse -> TestTree
responseBatchGetAssetPropertyValue =
  res
    "BatchGetAssetPropertyValueResponse"
    "fixture/BatchGetAssetPropertyValueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetAssetPropertyValue)

responseBatchGetAssetPropertyValueHistory :: BatchGetAssetPropertyValueHistoryResponse -> TestTree
responseBatchGetAssetPropertyValueHistory =
  res
    "BatchGetAssetPropertyValueHistoryResponse"
    "fixture/BatchGetAssetPropertyValueHistoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetAssetPropertyValueHistory)

responseBatchPutAssetPropertyValue :: BatchPutAssetPropertyValueResponse -> TestTree
responseBatchPutAssetPropertyValue =
  res
    "BatchPutAssetPropertyValueResponse"
    "fixture/BatchPutAssetPropertyValueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchPutAssetPropertyValue)

responseCreateAccessPolicy :: CreateAccessPolicyResponse -> TestTree
responseCreateAccessPolicy =
  res
    "CreateAccessPolicyResponse"
    "fixture/CreateAccessPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAccessPolicy)

responseCreateAsset :: CreateAssetResponse -> TestTree
responseCreateAsset =
  res
    "CreateAssetResponse"
    "fixture/CreateAssetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAsset)

responseCreateAssetModel :: CreateAssetModelResponse -> TestTree
responseCreateAssetModel =
  res
    "CreateAssetModelResponse"
    "fixture/CreateAssetModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAssetModel)

responseCreateBulkImportJob :: CreateBulkImportJobResponse -> TestTree
responseCreateBulkImportJob =
  res
    "CreateBulkImportJobResponse"
    "fixture/CreateBulkImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBulkImportJob)

responseCreateDashboard :: CreateDashboardResponse -> TestTree
responseCreateDashboard =
  res
    "CreateDashboardResponse"
    "fixture/CreateDashboardResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDashboard)

responseCreateGateway :: CreateGatewayResponse -> TestTree
responseCreateGateway =
  res
    "CreateGatewayResponse"
    "fixture/CreateGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGateway)

responseCreatePortal :: CreatePortalResponse -> TestTree
responseCreatePortal =
  res
    "CreatePortalResponse"
    "fixture/CreatePortalResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePortal)

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject =
  res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProject)

responseDeleteAccessPolicy :: DeleteAccessPolicyResponse -> TestTree
responseDeleteAccessPolicy =
  res
    "DeleteAccessPolicyResponse"
    "fixture/DeleteAccessPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAccessPolicy)

responseDeleteAsset :: DeleteAssetResponse -> TestTree
responseDeleteAsset =
  res
    "DeleteAssetResponse"
    "fixture/DeleteAssetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAsset)

responseDeleteAssetModel :: DeleteAssetModelResponse -> TestTree
responseDeleteAssetModel =
  res
    "DeleteAssetModelResponse"
    "fixture/DeleteAssetModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAssetModel)

responseDeleteDashboard :: DeleteDashboardResponse -> TestTree
responseDeleteDashboard =
  res
    "DeleteDashboardResponse"
    "fixture/DeleteDashboardResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDashboard)

responseDeleteGateway :: DeleteGatewayResponse -> TestTree
responseDeleteGateway =
  res
    "DeleteGatewayResponse"
    "fixture/DeleteGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGateway)

responseDeletePortal :: DeletePortalResponse -> TestTree
responseDeletePortal =
  res
    "DeletePortalResponse"
    "fixture/DeletePortalResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePortal)

responseDeleteProject :: DeleteProjectResponse -> TestTree
responseDeleteProject =
  res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProject)

responseDeleteTimeSeries :: DeleteTimeSeriesResponse -> TestTree
responseDeleteTimeSeries =
  res
    "DeleteTimeSeriesResponse"
    "fixture/DeleteTimeSeriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTimeSeries)

responseDescribeAccessPolicy :: DescribeAccessPolicyResponse -> TestTree
responseDescribeAccessPolicy =
  res
    "DescribeAccessPolicyResponse"
    "fixture/DescribeAccessPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccessPolicy)

responseDescribeAsset :: DescribeAssetResponse -> TestTree
responseDescribeAsset =
  res
    "DescribeAssetResponse"
    "fixture/DescribeAssetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAsset)

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

responseDescribeBulkImportJob :: DescribeBulkImportJobResponse -> TestTree
responseDescribeBulkImportJob =
  res
    "DescribeBulkImportJobResponse"
    "fixture/DescribeBulkImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBulkImportJob)

responseDescribeDashboard :: DescribeDashboardResponse -> TestTree
responseDescribeDashboard =
  res
    "DescribeDashboardResponse"
    "fixture/DescribeDashboardResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDashboard)

responseDescribeDefaultEncryptionConfiguration :: DescribeDefaultEncryptionConfigurationResponse -> TestTree
responseDescribeDefaultEncryptionConfiguration =
  res
    "DescribeDefaultEncryptionConfigurationResponse"
    "fixture/DescribeDefaultEncryptionConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDefaultEncryptionConfiguration)

responseDescribeGateway :: DescribeGatewayResponse -> TestTree
responseDescribeGateway =
  res
    "DescribeGatewayResponse"
    "fixture/DescribeGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGateway)

responseDescribeGatewayCapabilityConfiguration :: DescribeGatewayCapabilityConfigurationResponse -> TestTree
responseDescribeGatewayCapabilityConfiguration =
  res
    "DescribeGatewayCapabilityConfigurationResponse"
    "fixture/DescribeGatewayCapabilityConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGatewayCapabilityConfiguration)

responseDescribeLoggingOptions :: DescribeLoggingOptionsResponse -> TestTree
responseDescribeLoggingOptions =
  res
    "DescribeLoggingOptionsResponse"
    "fixture/DescribeLoggingOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLoggingOptions)

responseDescribePortal :: DescribePortalResponse -> TestTree
responseDescribePortal =
  res
    "DescribePortalResponse"
    "fixture/DescribePortalResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePortal)

responseDescribeProject :: DescribeProjectResponse -> TestTree
responseDescribeProject =
  res
    "DescribeProjectResponse"
    "fixture/DescribeProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProject)

responseDescribeStorageConfiguration :: DescribeStorageConfigurationResponse -> TestTree
responseDescribeStorageConfiguration =
  res
    "DescribeStorageConfigurationResponse"
    "fixture/DescribeStorageConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStorageConfiguration)

responseDescribeTimeSeries :: DescribeTimeSeriesResponse -> TestTree
responseDescribeTimeSeries =
  res
    "DescribeTimeSeriesResponse"
    "fixture/DescribeTimeSeriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTimeSeries)

responseDisassociateAssets :: DisassociateAssetsResponse -> TestTree
responseDisassociateAssets =
  res
    "DisassociateAssetsResponse"
    "fixture/DisassociateAssetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateAssets)

responseDisassociateTimeSeriesFromAssetProperty :: DisassociateTimeSeriesFromAssetPropertyResponse -> TestTree
responseDisassociateTimeSeriesFromAssetProperty =
  res
    "DisassociateTimeSeriesFromAssetPropertyResponse"
    "fixture/DisassociateTimeSeriesFromAssetPropertyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateTimeSeriesFromAssetProperty)

responseGetAssetPropertyAggregates :: GetAssetPropertyAggregatesResponse -> TestTree
responseGetAssetPropertyAggregates =
  res
    "GetAssetPropertyAggregatesResponse"
    "fixture/GetAssetPropertyAggregatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAssetPropertyAggregates)

responseGetAssetPropertyValue :: GetAssetPropertyValueResponse -> TestTree
responseGetAssetPropertyValue =
  res
    "GetAssetPropertyValueResponse"
    "fixture/GetAssetPropertyValueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAssetPropertyValue)

responseGetAssetPropertyValueHistory :: GetAssetPropertyValueHistoryResponse -> TestTree
responseGetAssetPropertyValueHistory =
  res
    "GetAssetPropertyValueHistoryResponse"
    "fixture/GetAssetPropertyValueHistoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAssetPropertyValueHistory)

responseGetInterpolatedAssetPropertyValues :: GetInterpolatedAssetPropertyValuesResponse -> TestTree
responseGetInterpolatedAssetPropertyValues =
  res
    "GetInterpolatedAssetPropertyValuesResponse"
    "fixture/GetInterpolatedAssetPropertyValuesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInterpolatedAssetPropertyValues)

responseListAccessPolicies :: ListAccessPoliciesResponse -> TestTree
responseListAccessPolicies =
  res
    "ListAccessPoliciesResponse"
    "fixture/ListAccessPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccessPolicies)

responseListAssetModelProperties :: ListAssetModelPropertiesResponse -> TestTree
responseListAssetModelProperties =
  res
    "ListAssetModelPropertiesResponse"
    "fixture/ListAssetModelPropertiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssetModelProperties)

responseListAssetModels :: ListAssetModelsResponse -> TestTree
responseListAssetModels =
  res
    "ListAssetModelsResponse"
    "fixture/ListAssetModelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssetModels)

responseListAssetProperties :: ListAssetPropertiesResponse -> TestTree
responseListAssetProperties =
  res
    "ListAssetPropertiesResponse"
    "fixture/ListAssetPropertiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssetProperties)

responseListAssetRelationships :: ListAssetRelationshipsResponse -> TestTree
responseListAssetRelationships =
  res
    "ListAssetRelationshipsResponse"
    "fixture/ListAssetRelationshipsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssetRelationships)

responseListAssets :: ListAssetsResponse -> TestTree
responseListAssets =
  res
    "ListAssetsResponse"
    "fixture/ListAssetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssets)

responseListAssociatedAssets :: ListAssociatedAssetsResponse -> TestTree
responseListAssociatedAssets =
  res
    "ListAssociatedAssetsResponse"
    "fixture/ListAssociatedAssetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssociatedAssets)

responseListBulkImportJobs :: ListBulkImportJobsResponse -> TestTree
responseListBulkImportJobs =
  res
    "ListBulkImportJobsResponse"
    "fixture/ListBulkImportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBulkImportJobs)

responseListDashboards :: ListDashboardsResponse -> TestTree
responseListDashboards =
  res
    "ListDashboardsResponse"
    "fixture/ListDashboardsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDashboards)

responseListGateways :: ListGatewaysResponse -> TestTree
responseListGateways =
  res
    "ListGatewaysResponse"
    "fixture/ListGatewaysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGateways)

responseListPortals :: ListPortalsResponse -> TestTree
responseListPortals =
  res
    "ListPortalsResponse"
    "fixture/ListPortalsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPortals)

responseListProjectAssets :: ListProjectAssetsResponse -> TestTree
responseListProjectAssets =
  res
    "ListProjectAssetsResponse"
    "fixture/ListProjectAssetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProjectAssets)

responseListProjects :: ListProjectsResponse -> TestTree
responseListProjects =
  res
    "ListProjectsResponse"
    "fixture/ListProjectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProjects)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListTimeSeries :: ListTimeSeriesResponse -> TestTree
responseListTimeSeries =
  res
    "ListTimeSeriesResponse"
    "fixture/ListTimeSeriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTimeSeries)

responsePutDefaultEncryptionConfiguration :: PutDefaultEncryptionConfigurationResponse -> TestTree
responsePutDefaultEncryptionConfiguration =
  res
    "PutDefaultEncryptionConfigurationResponse"
    "fixture/PutDefaultEncryptionConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutDefaultEncryptionConfiguration)

responsePutLoggingOptions :: PutLoggingOptionsResponse -> TestTree
responsePutLoggingOptions =
  res
    "PutLoggingOptionsResponse"
    "fixture/PutLoggingOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutLoggingOptions)

responsePutStorageConfiguration :: PutStorageConfigurationResponse -> TestTree
responsePutStorageConfiguration =
  res
    "PutStorageConfigurationResponse"
    "fixture/PutStorageConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutStorageConfiguration)

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

responseUpdateAsset :: UpdateAssetResponse -> TestTree
responseUpdateAsset =
  res
    "UpdateAssetResponse"
    "fixture/UpdateAssetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAsset)

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

responseUpdateDashboard :: UpdateDashboardResponse -> TestTree
responseUpdateDashboard =
  res
    "UpdateDashboardResponse"
    "fixture/UpdateDashboardResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDashboard)

responseUpdateGateway :: UpdateGatewayResponse -> TestTree
responseUpdateGateway =
  res
    "UpdateGatewayResponse"
    "fixture/UpdateGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGateway)

responseUpdateGatewayCapabilityConfiguration :: UpdateGatewayCapabilityConfigurationResponse -> TestTree
responseUpdateGatewayCapabilityConfiguration =
  res
    "UpdateGatewayCapabilityConfigurationResponse"
    "fixture/UpdateGatewayCapabilityConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGatewayCapabilityConfiguration)

responseUpdatePortal :: UpdatePortalResponse -> TestTree
responseUpdatePortal =
  res
    "UpdatePortalResponse"
    "fixture/UpdatePortalResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePortal)

responseUpdateProject :: UpdateProjectResponse -> TestTree
responseUpdateProject =
  res
    "UpdateProjectResponse"
    "fixture/UpdateProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProject)

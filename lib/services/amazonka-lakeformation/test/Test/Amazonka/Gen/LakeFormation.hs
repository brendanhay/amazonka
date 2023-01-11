{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.LakeFormation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.LakeFormation where

import Amazonka.LakeFormation
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.LakeFormation.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAddLFTagsToResource $
--             newAddLFTagsToResource
--
--         , requestAssumeDecoratedRoleWithSAML $
--             newAssumeDecoratedRoleWithSAML
--
--         , requestBatchGrantPermissions $
--             newBatchGrantPermissions
--
--         , requestBatchRevokePermissions $
--             newBatchRevokePermissions
--
--         , requestCancelTransaction $
--             newCancelTransaction
--
--         , requestCommitTransaction $
--             newCommitTransaction
--
--         , requestCreateDataCellsFilter $
--             newCreateDataCellsFilter
--
--         , requestCreateLFTag $
--             newCreateLFTag
--
--         , requestDeleteDataCellsFilter $
--             newDeleteDataCellsFilter
--
--         , requestDeleteLFTag $
--             newDeleteLFTag
--
--         , requestDeleteObjectsOnCancel $
--             newDeleteObjectsOnCancel
--
--         , requestDeregisterResource $
--             newDeregisterResource
--
--         , requestDescribeResource $
--             newDescribeResource
--
--         , requestDescribeTransaction $
--             newDescribeTransaction
--
--         , requestExtendTransaction $
--             newExtendTransaction
--
--         , requestGetDataLakeSettings $
--             newGetDataLakeSettings
--
--         , requestGetEffectivePermissionsForPath $
--             newGetEffectivePermissionsForPath
--
--         , requestGetLFTag $
--             newGetLFTag
--
--         , requestGetQueryState $
--             newGetQueryState
--
--         , requestGetQueryStatistics $
--             newGetQueryStatistics
--
--         , requestGetResourceLFTags $
--             newGetResourceLFTags
--
--         , requestGetTableObjects $
--             newGetTableObjects
--
--         , requestGetTemporaryGluePartitionCredentials $
--             newGetTemporaryGluePartitionCredentials
--
--         , requestGetTemporaryGlueTableCredentials $
--             newGetTemporaryGlueTableCredentials
--
--         , requestGetWorkUnitResults $
--             newGetWorkUnitResults
--
--         , requestGetWorkUnits $
--             newGetWorkUnits
--
--         , requestGrantPermissions $
--             newGrantPermissions
--
--         , requestListDataCellsFilter $
--             newListDataCellsFilter
--
--         , requestListLFTags $
--             newListLFTags
--
--         , requestListPermissions $
--             newListPermissions
--
--         , requestListResources $
--             newListResources
--
--         , requestListTableStorageOptimizers $
--             newListTableStorageOptimizers
--
--         , requestListTransactions $
--             newListTransactions
--
--         , requestPutDataLakeSettings $
--             newPutDataLakeSettings
--
--         , requestRegisterResource $
--             newRegisterResource
--
--         , requestRemoveLFTagsFromResource $
--             newRemoveLFTagsFromResource
--
--         , requestRevokePermissions $
--             newRevokePermissions
--
--         , requestSearchDatabasesByLFTags $
--             newSearchDatabasesByLFTags
--
--         , requestSearchTablesByLFTags $
--             newSearchTablesByLFTags
--
--         , requestStartQueryPlanning $
--             newStartQueryPlanning
--
--         , requestStartTransaction $
--             newStartTransaction
--
--         , requestUpdateLFTag $
--             newUpdateLFTag
--
--         , requestUpdateResource $
--             newUpdateResource
--
--         , requestUpdateTableObjects $
--             newUpdateTableObjects
--
--         , requestUpdateTableStorageOptimizer $
--             newUpdateTableStorageOptimizer
--
--           ]

--     , testGroup "response"
--         [ responseAddLFTagsToResource $
--             newAddLFTagsToResourceResponse
--
--         , responseAssumeDecoratedRoleWithSAML $
--             newAssumeDecoratedRoleWithSAMLResponse
--
--         , responseBatchGrantPermissions $
--             newBatchGrantPermissionsResponse
--
--         , responseBatchRevokePermissions $
--             newBatchRevokePermissionsResponse
--
--         , responseCancelTransaction $
--             newCancelTransactionResponse
--
--         , responseCommitTransaction $
--             newCommitTransactionResponse
--
--         , responseCreateDataCellsFilter $
--             newCreateDataCellsFilterResponse
--
--         , responseCreateLFTag $
--             newCreateLFTagResponse
--
--         , responseDeleteDataCellsFilter $
--             newDeleteDataCellsFilterResponse
--
--         , responseDeleteLFTag $
--             newDeleteLFTagResponse
--
--         , responseDeleteObjectsOnCancel $
--             newDeleteObjectsOnCancelResponse
--
--         , responseDeregisterResource $
--             newDeregisterResourceResponse
--
--         , responseDescribeResource $
--             newDescribeResourceResponse
--
--         , responseDescribeTransaction $
--             newDescribeTransactionResponse
--
--         , responseExtendTransaction $
--             newExtendTransactionResponse
--
--         , responseGetDataLakeSettings $
--             newGetDataLakeSettingsResponse
--
--         , responseGetEffectivePermissionsForPath $
--             newGetEffectivePermissionsForPathResponse
--
--         , responseGetLFTag $
--             newGetLFTagResponse
--
--         , responseGetQueryState $
--             newGetQueryStateResponse
--
--         , responseGetQueryStatistics $
--             newGetQueryStatisticsResponse
--
--         , responseGetResourceLFTags $
--             newGetResourceLFTagsResponse
--
--         , responseGetTableObjects $
--             newGetTableObjectsResponse
--
--         , responseGetTemporaryGluePartitionCredentials $
--             newGetTemporaryGluePartitionCredentialsResponse
--
--         , responseGetTemporaryGlueTableCredentials $
--             newGetTemporaryGlueTableCredentialsResponse
--
--         , responseGetWorkUnitResults $
--             newGetWorkUnitResultsResponse
--
--         , responseGetWorkUnits $
--             newGetWorkUnitsResponse
--
--         , responseGrantPermissions $
--             newGrantPermissionsResponse
--
--         , responseListDataCellsFilter $
--             newListDataCellsFilterResponse
--
--         , responseListLFTags $
--             newListLFTagsResponse
--
--         , responseListPermissions $
--             newListPermissionsResponse
--
--         , responseListResources $
--             newListResourcesResponse
--
--         , responseListTableStorageOptimizers $
--             newListTableStorageOptimizersResponse
--
--         , responseListTransactions $
--             newListTransactionsResponse
--
--         , responsePutDataLakeSettings $
--             newPutDataLakeSettingsResponse
--
--         , responseRegisterResource $
--             newRegisterResourceResponse
--
--         , responseRemoveLFTagsFromResource $
--             newRemoveLFTagsFromResourceResponse
--
--         , responseRevokePermissions $
--             newRevokePermissionsResponse
--
--         , responseSearchDatabasesByLFTags $
--             newSearchDatabasesByLFTagsResponse
--
--         , responseSearchTablesByLFTags $
--             newSearchTablesByLFTagsResponse
--
--         , responseStartQueryPlanning $
--             newStartQueryPlanningResponse
--
--         , responseStartTransaction $
--             newStartTransactionResponse
--
--         , responseUpdateLFTag $
--             newUpdateLFTagResponse
--
--         , responseUpdateResource $
--             newUpdateResourceResponse
--
--         , responseUpdateTableObjects $
--             newUpdateTableObjectsResponse
--
--         , responseUpdateTableStorageOptimizer $
--             newUpdateTableStorageOptimizerResponse
--
--           ]
--     ]

-- Requests

requestAddLFTagsToResource :: AddLFTagsToResource -> TestTree
requestAddLFTagsToResource =
  req
    "AddLFTagsToResource"
    "fixture/AddLFTagsToResource.yaml"

requestAssumeDecoratedRoleWithSAML :: AssumeDecoratedRoleWithSAML -> TestTree
requestAssumeDecoratedRoleWithSAML =
  req
    "AssumeDecoratedRoleWithSAML"
    "fixture/AssumeDecoratedRoleWithSAML.yaml"

requestBatchGrantPermissions :: BatchGrantPermissions -> TestTree
requestBatchGrantPermissions =
  req
    "BatchGrantPermissions"
    "fixture/BatchGrantPermissions.yaml"

requestBatchRevokePermissions :: BatchRevokePermissions -> TestTree
requestBatchRevokePermissions =
  req
    "BatchRevokePermissions"
    "fixture/BatchRevokePermissions.yaml"

requestCancelTransaction :: CancelTransaction -> TestTree
requestCancelTransaction =
  req
    "CancelTransaction"
    "fixture/CancelTransaction.yaml"

requestCommitTransaction :: CommitTransaction -> TestTree
requestCommitTransaction =
  req
    "CommitTransaction"
    "fixture/CommitTransaction.yaml"

requestCreateDataCellsFilter :: CreateDataCellsFilter -> TestTree
requestCreateDataCellsFilter =
  req
    "CreateDataCellsFilter"
    "fixture/CreateDataCellsFilter.yaml"

requestCreateLFTag :: CreateLFTag -> TestTree
requestCreateLFTag =
  req
    "CreateLFTag"
    "fixture/CreateLFTag.yaml"

requestDeleteDataCellsFilter :: DeleteDataCellsFilter -> TestTree
requestDeleteDataCellsFilter =
  req
    "DeleteDataCellsFilter"
    "fixture/DeleteDataCellsFilter.yaml"

requestDeleteLFTag :: DeleteLFTag -> TestTree
requestDeleteLFTag =
  req
    "DeleteLFTag"
    "fixture/DeleteLFTag.yaml"

requestDeleteObjectsOnCancel :: DeleteObjectsOnCancel -> TestTree
requestDeleteObjectsOnCancel =
  req
    "DeleteObjectsOnCancel"
    "fixture/DeleteObjectsOnCancel.yaml"

requestDeregisterResource :: DeregisterResource -> TestTree
requestDeregisterResource =
  req
    "DeregisterResource"
    "fixture/DeregisterResource.yaml"

requestDescribeResource :: DescribeResource -> TestTree
requestDescribeResource =
  req
    "DescribeResource"
    "fixture/DescribeResource.yaml"

requestDescribeTransaction :: DescribeTransaction -> TestTree
requestDescribeTransaction =
  req
    "DescribeTransaction"
    "fixture/DescribeTransaction.yaml"

requestExtendTransaction :: ExtendTransaction -> TestTree
requestExtendTransaction =
  req
    "ExtendTransaction"
    "fixture/ExtendTransaction.yaml"

requestGetDataLakeSettings :: GetDataLakeSettings -> TestTree
requestGetDataLakeSettings =
  req
    "GetDataLakeSettings"
    "fixture/GetDataLakeSettings.yaml"

requestGetEffectivePermissionsForPath :: GetEffectivePermissionsForPath -> TestTree
requestGetEffectivePermissionsForPath =
  req
    "GetEffectivePermissionsForPath"
    "fixture/GetEffectivePermissionsForPath.yaml"

requestGetLFTag :: GetLFTag -> TestTree
requestGetLFTag =
  req
    "GetLFTag"
    "fixture/GetLFTag.yaml"

requestGetQueryState :: GetQueryState -> TestTree
requestGetQueryState =
  req
    "GetQueryState"
    "fixture/GetQueryState.yaml"

requestGetQueryStatistics :: GetQueryStatistics -> TestTree
requestGetQueryStatistics =
  req
    "GetQueryStatistics"
    "fixture/GetQueryStatistics.yaml"

requestGetResourceLFTags :: GetResourceLFTags -> TestTree
requestGetResourceLFTags =
  req
    "GetResourceLFTags"
    "fixture/GetResourceLFTags.yaml"

requestGetTableObjects :: GetTableObjects -> TestTree
requestGetTableObjects =
  req
    "GetTableObjects"
    "fixture/GetTableObjects.yaml"

requestGetTemporaryGluePartitionCredentials :: GetTemporaryGluePartitionCredentials -> TestTree
requestGetTemporaryGluePartitionCredentials =
  req
    "GetTemporaryGluePartitionCredentials"
    "fixture/GetTemporaryGluePartitionCredentials.yaml"

requestGetTemporaryGlueTableCredentials :: GetTemporaryGlueTableCredentials -> TestTree
requestGetTemporaryGlueTableCredentials =
  req
    "GetTemporaryGlueTableCredentials"
    "fixture/GetTemporaryGlueTableCredentials.yaml"

requestGetWorkUnitResults :: GetWorkUnitResults -> TestTree
requestGetWorkUnitResults =
  req
    "GetWorkUnitResults"
    "fixture/GetWorkUnitResults.yaml"

requestGetWorkUnits :: GetWorkUnits -> TestTree
requestGetWorkUnits =
  req
    "GetWorkUnits"
    "fixture/GetWorkUnits.yaml"

requestGrantPermissions :: GrantPermissions -> TestTree
requestGrantPermissions =
  req
    "GrantPermissions"
    "fixture/GrantPermissions.yaml"

requestListDataCellsFilter :: ListDataCellsFilter -> TestTree
requestListDataCellsFilter =
  req
    "ListDataCellsFilter"
    "fixture/ListDataCellsFilter.yaml"

requestListLFTags :: ListLFTags -> TestTree
requestListLFTags =
  req
    "ListLFTags"
    "fixture/ListLFTags.yaml"

requestListPermissions :: ListPermissions -> TestTree
requestListPermissions =
  req
    "ListPermissions"
    "fixture/ListPermissions.yaml"

requestListResources :: ListResources -> TestTree
requestListResources =
  req
    "ListResources"
    "fixture/ListResources.yaml"

requestListTableStorageOptimizers :: ListTableStorageOptimizers -> TestTree
requestListTableStorageOptimizers =
  req
    "ListTableStorageOptimizers"
    "fixture/ListTableStorageOptimizers.yaml"

requestListTransactions :: ListTransactions -> TestTree
requestListTransactions =
  req
    "ListTransactions"
    "fixture/ListTransactions.yaml"

requestPutDataLakeSettings :: PutDataLakeSettings -> TestTree
requestPutDataLakeSettings =
  req
    "PutDataLakeSettings"
    "fixture/PutDataLakeSettings.yaml"

requestRegisterResource :: RegisterResource -> TestTree
requestRegisterResource =
  req
    "RegisterResource"
    "fixture/RegisterResource.yaml"

requestRemoveLFTagsFromResource :: RemoveLFTagsFromResource -> TestTree
requestRemoveLFTagsFromResource =
  req
    "RemoveLFTagsFromResource"
    "fixture/RemoveLFTagsFromResource.yaml"

requestRevokePermissions :: RevokePermissions -> TestTree
requestRevokePermissions =
  req
    "RevokePermissions"
    "fixture/RevokePermissions.yaml"

requestSearchDatabasesByLFTags :: SearchDatabasesByLFTags -> TestTree
requestSearchDatabasesByLFTags =
  req
    "SearchDatabasesByLFTags"
    "fixture/SearchDatabasesByLFTags.yaml"

requestSearchTablesByLFTags :: SearchTablesByLFTags -> TestTree
requestSearchTablesByLFTags =
  req
    "SearchTablesByLFTags"
    "fixture/SearchTablesByLFTags.yaml"

requestStartQueryPlanning :: StartQueryPlanning -> TestTree
requestStartQueryPlanning =
  req
    "StartQueryPlanning"
    "fixture/StartQueryPlanning.yaml"

requestStartTransaction :: StartTransaction -> TestTree
requestStartTransaction =
  req
    "StartTransaction"
    "fixture/StartTransaction.yaml"

requestUpdateLFTag :: UpdateLFTag -> TestTree
requestUpdateLFTag =
  req
    "UpdateLFTag"
    "fixture/UpdateLFTag.yaml"

requestUpdateResource :: UpdateResource -> TestTree
requestUpdateResource =
  req
    "UpdateResource"
    "fixture/UpdateResource.yaml"

requestUpdateTableObjects :: UpdateTableObjects -> TestTree
requestUpdateTableObjects =
  req
    "UpdateTableObjects"
    "fixture/UpdateTableObjects.yaml"

requestUpdateTableStorageOptimizer :: UpdateTableStorageOptimizer -> TestTree
requestUpdateTableStorageOptimizer =
  req
    "UpdateTableStorageOptimizer"
    "fixture/UpdateTableStorageOptimizer.yaml"

-- Responses

responseAddLFTagsToResource :: AddLFTagsToResourceResponse -> TestTree
responseAddLFTagsToResource =
  res
    "AddLFTagsToResourceResponse"
    "fixture/AddLFTagsToResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddLFTagsToResource)

responseAssumeDecoratedRoleWithSAML :: AssumeDecoratedRoleWithSAMLResponse -> TestTree
responseAssumeDecoratedRoleWithSAML =
  res
    "AssumeDecoratedRoleWithSAMLResponse"
    "fixture/AssumeDecoratedRoleWithSAMLResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssumeDecoratedRoleWithSAML)

responseBatchGrantPermissions :: BatchGrantPermissionsResponse -> TestTree
responseBatchGrantPermissions =
  res
    "BatchGrantPermissionsResponse"
    "fixture/BatchGrantPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGrantPermissions)

responseBatchRevokePermissions :: BatchRevokePermissionsResponse -> TestTree
responseBatchRevokePermissions =
  res
    "BatchRevokePermissionsResponse"
    "fixture/BatchRevokePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchRevokePermissions)

responseCancelTransaction :: CancelTransactionResponse -> TestTree
responseCancelTransaction =
  res
    "CancelTransactionResponse"
    "fixture/CancelTransactionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelTransaction)

responseCommitTransaction :: CommitTransactionResponse -> TestTree
responseCommitTransaction =
  res
    "CommitTransactionResponse"
    "fixture/CommitTransactionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CommitTransaction)

responseCreateDataCellsFilter :: CreateDataCellsFilterResponse -> TestTree
responseCreateDataCellsFilter =
  res
    "CreateDataCellsFilterResponse"
    "fixture/CreateDataCellsFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataCellsFilter)

responseCreateLFTag :: CreateLFTagResponse -> TestTree
responseCreateLFTag =
  res
    "CreateLFTagResponse"
    "fixture/CreateLFTagResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLFTag)

responseDeleteDataCellsFilter :: DeleteDataCellsFilterResponse -> TestTree
responseDeleteDataCellsFilter =
  res
    "DeleteDataCellsFilterResponse"
    "fixture/DeleteDataCellsFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataCellsFilter)

responseDeleteLFTag :: DeleteLFTagResponse -> TestTree
responseDeleteLFTag =
  res
    "DeleteLFTagResponse"
    "fixture/DeleteLFTagResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLFTag)

responseDeleteObjectsOnCancel :: DeleteObjectsOnCancelResponse -> TestTree
responseDeleteObjectsOnCancel =
  res
    "DeleteObjectsOnCancelResponse"
    "fixture/DeleteObjectsOnCancelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteObjectsOnCancel)

responseDeregisterResource :: DeregisterResourceResponse -> TestTree
responseDeregisterResource =
  res
    "DeregisterResourceResponse"
    "fixture/DeregisterResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterResource)

responseDescribeResource :: DescribeResourceResponse -> TestTree
responseDescribeResource =
  res
    "DescribeResourceResponse"
    "fixture/DescribeResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeResource)

responseDescribeTransaction :: DescribeTransactionResponse -> TestTree
responseDescribeTransaction =
  res
    "DescribeTransactionResponse"
    "fixture/DescribeTransactionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTransaction)

responseExtendTransaction :: ExtendTransactionResponse -> TestTree
responseExtendTransaction =
  res
    "ExtendTransactionResponse"
    "fixture/ExtendTransactionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExtendTransaction)

responseGetDataLakeSettings :: GetDataLakeSettingsResponse -> TestTree
responseGetDataLakeSettings =
  res
    "GetDataLakeSettingsResponse"
    "fixture/GetDataLakeSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDataLakeSettings)

responseGetEffectivePermissionsForPath :: GetEffectivePermissionsForPathResponse -> TestTree
responseGetEffectivePermissionsForPath =
  res
    "GetEffectivePermissionsForPathResponse"
    "fixture/GetEffectivePermissionsForPathResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEffectivePermissionsForPath)

responseGetLFTag :: GetLFTagResponse -> TestTree
responseGetLFTag =
  res
    "GetLFTagResponse"
    "fixture/GetLFTagResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLFTag)

responseGetQueryState :: GetQueryStateResponse -> TestTree
responseGetQueryState =
  res
    "GetQueryStateResponse"
    "fixture/GetQueryStateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetQueryState)

responseGetQueryStatistics :: GetQueryStatisticsResponse -> TestTree
responseGetQueryStatistics =
  res
    "GetQueryStatisticsResponse"
    "fixture/GetQueryStatisticsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetQueryStatistics)

responseGetResourceLFTags :: GetResourceLFTagsResponse -> TestTree
responseGetResourceLFTags =
  res
    "GetResourceLFTagsResponse"
    "fixture/GetResourceLFTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourceLFTags)

responseGetTableObjects :: GetTableObjectsResponse -> TestTree
responseGetTableObjects =
  res
    "GetTableObjectsResponse"
    "fixture/GetTableObjectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTableObjects)

responseGetTemporaryGluePartitionCredentials :: GetTemporaryGluePartitionCredentialsResponse -> TestTree
responseGetTemporaryGluePartitionCredentials =
  res
    "GetTemporaryGluePartitionCredentialsResponse"
    "fixture/GetTemporaryGluePartitionCredentialsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTemporaryGluePartitionCredentials)

responseGetTemporaryGlueTableCredentials :: GetTemporaryGlueTableCredentialsResponse -> TestTree
responseGetTemporaryGlueTableCredentials =
  res
    "GetTemporaryGlueTableCredentialsResponse"
    "fixture/GetTemporaryGlueTableCredentialsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTemporaryGlueTableCredentials)

responseGetWorkUnits :: GetWorkUnitsResponse -> TestTree
responseGetWorkUnits =
  res
    "GetWorkUnitsResponse"
    "fixture/GetWorkUnitsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWorkUnits)

responseGrantPermissions :: GrantPermissionsResponse -> TestTree
responseGrantPermissions =
  res
    "GrantPermissionsResponse"
    "fixture/GrantPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GrantPermissions)

responseListDataCellsFilter :: ListDataCellsFilterResponse -> TestTree
responseListDataCellsFilter =
  res
    "ListDataCellsFilterResponse"
    "fixture/ListDataCellsFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDataCellsFilter)

responseListLFTags :: ListLFTagsResponse -> TestTree
responseListLFTags =
  res
    "ListLFTagsResponse"
    "fixture/ListLFTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLFTags)

responseListPermissions :: ListPermissionsResponse -> TestTree
responseListPermissions =
  res
    "ListPermissionsResponse"
    "fixture/ListPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPermissions)

responseListResources :: ListResourcesResponse -> TestTree
responseListResources =
  res
    "ListResourcesResponse"
    "fixture/ListResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResources)

responseListTableStorageOptimizers :: ListTableStorageOptimizersResponse -> TestTree
responseListTableStorageOptimizers =
  res
    "ListTableStorageOptimizersResponse"
    "fixture/ListTableStorageOptimizersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTableStorageOptimizers)

responseListTransactions :: ListTransactionsResponse -> TestTree
responseListTransactions =
  res
    "ListTransactionsResponse"
    "fixture/ListTransactionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTransactions)

responsePutDataLakeSettings :: PutDataLakeSettingsResponse -> TestTree
responsePutDataLakeSettings =
  res
    "PutDataLakeSettingsResponse"
    "fixture/PutDataLakeSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutDataLakeSettings)

responseRegisterResource :: RegisterResourceResponse -> TestTree
responseRegisterResource =
  res
    "RegisterResourceResponse"
    "fixture/RegisterResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterResource)

responseRemoveLFTagsFromResource :: RemoveLFTagsFromResourceResponse -> TestTree
responseRemoveLFTagsFromResource =
  res
    "RemoveLFTagsFromResourceResponse"
    "fixture/RemoveLFTagsFromResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveLFTagsFromResource)

responseRevokePermissions :: RevokePermissionsResponse -> TestTree
responseRevokePermissions =
  res
    "RevokePermissionsResponse"
    "fixture/RevokePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RevokePermissions)

responseSearchDatabasesByLFTags :: SearchDatabasesByLFTagsResponse -> TestTree
responseSearchDatabasesByLFTags =
  res
    "SearchDatabasesByLFTagsResponse"
    "fixture/SearchDatabasesByLFTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchDatabasesByLFTags)

responseSearchTablesByLFTags :: SearchTablesByLFTagsResponse -> TestTree
responseSearchTablesByLFTags =
  res
    "SearchTablesByLFTagsResponse"
    "fixture/SearchTablesByLFTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchTablesByLFTags)

responseStartQueryPlanning :: StartQueryPlanningResponse -> TestTree
responseStartQueryPlanning =
  res
    "StartQueryPlanningResponse"
    "fixture/StartQueryPlanningResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartQueryPlanning)

responseStartTransaction :: StartTransactionResponse -> TestTree
responseStartTransaction =
  res
    "StartTransactionResponse"
    "fixture/StartTransactionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartTransaction)

responseUpdateLFTag :: UpdateLFTagResponse -> TestTree
responseUpdateLFTag =
  res
    "UpdateLFTagResponse"
    "fixture/UpdateLFTagResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLFTag)

responseUpdateResource :: UpdateResourceResponse -> TestTree
responseUpdateResource =
  res
    "UpdateResourceResponse"
    "fixture/UpdateResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateResource)

responseUpdateTableObjects :: UpdateTableObjectsResponse -> TestTree
responseUpdateTableObjects =
  res
    "UpdateTableObjectsResponse"
    "fixture/UpdateTableObjectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTableObjects)

responseUpdateTableStorageOptimizer :: UpdateTableStorageOptimizerResponse -> TestTree
responseUpdateTableStorageOptimizer =
  res
    "UpdateTableStorageOptimizerResponse"
    "fixture/UpdateTableStorageOptimizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTableStorageOptimizer)

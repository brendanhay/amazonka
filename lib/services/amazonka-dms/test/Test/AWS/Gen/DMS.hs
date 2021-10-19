{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DMS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.DMS where

import Data.Proxy
import Network.AWS.DMS
import Test.AWS.DMS.Internal
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
--         [ requestDeleteReplicationInstance $
--             newDeleteReplicationInstance
--
--         , requestRebootReplicationInstance $
--             newRebootReplicationInstance
--
--         , requestReloadTables $
--             newReloadTables
--
--         , requestStartReplicationTaskAssessment $
--             newStartReplicationTaskAssessment
--
--         , requestDeleteReplicationTaskAssessmentRun $
--             newDeleteReplicationTaskAssessmentRun
--
--         , requestCreateEndpoint $
--             newCreateEndpoint
--
--         , requestDescribeSchemas $
--             newDescribeSchemas
--
--         , requestDeleteConnection $
--             newDeleteConnection
--
--         , requestModifyEventSubscription $
--             newModifyEventSubscription
--
--         , requestDescribeReplicationInstanceTaskLogs $
--             newDescribeReplicationInstanceTaskLogs
--
--         , requestDescribeEvents $
--             newDescribeEvents
--
--         , requestDeleteEndpoint $
--             newDeleteEndpoint
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDescribeEndpointTypes $
--             newDescribeEndpointTypes
--
--         , requestDeleteReplicationTask $
--             newDeleteReplicationTask
--
--         , requestDescribeReplicationTaskAssessmentRuns $
--             newDescribeReplicationTaskAssessmentRuns
--
--         , requestDescribeReplicationTaskAssessmentResults $
--             newDescribeReplicationTaskAssessmentResults
--
--         , requestTestConnection $
--             newTestConnection
--
--         , requestDescribeConnections $
--             newDescribeConnections
--
--         , requestMoveReplicationTask $
--             newMoveReplicationTask
--
--         , requestRemoveTagsFromResource $
--             newRemoveTagsFromResource
--
--         , requestModifyEndpoint $
--             newModifyEndpoint
--
--         , requestCreateEventSubscription $
--             newCreateEventSubscription
--
--         , requestDescribeEndpointSettings $
--             newDescribeEndpointSettings
--
--         , requestDescribeCertificates $
--             newDescribeCertificates
--
--         , requestStartReplicationTaskAssessmentRun $
--             newStartReplicationTaskAssessmentRun
--
--         , requestDeleteEventSubscription $
--             newDeleteEventSubscription
--
--         , requestDescribeTableStatistics $
--             newDescribeTableStatistics
--
--         , requestDescribeReplicationSubnetGroups $
--             newDescribeReplicationSubnetGroups
--
--         , requestStartReplicationTask $
--             newStartReplicationTask
--
--         , requestDescribeEventSubscriptions $
--             newDescribeEventSubscriptions
--
--         , requestAddTagsToResource $
--             newAddTagsToResource
--
--         , requestCreateReplicationSubnetGroup $
--             newCreateReplicationSubnetGroup
--
--         , requestDescribeApplicableIndividualAssessments $
--             newDescribeApplicableIndividualAssessments
--
--         , requestDeleteCertificate $
--             newDeleteCertificate
--
--         , requestRefreshSchemas $
--             newRefreshSchemas
--
--         , requestDescribeReplicationTasks $
--             newDescribeReplicationTasks
--
--         , requestDescribeEventCategories $
--             newDescribeEventCategories
--
--         , requestDescribeOrderableReplicationInstances $
--             newDescribeOrderableReplicationInstances
--
--         , requestDescribePendingMaintenanceActions $
--             newDescribePendingMaintenanceActions
--
--         , requestCreateReplicationTask $
--             newCreateReplicationTask
--
--         , requestDescribeEndpoints $
--             newDescribeEndpoints
--
--         , requestModifyReplicationInstance $
--             newModifyReplicationInstance
--
--         , requestImportCertificate $
--             newImportCertificate
--
--         , requestCancelReplicationTaskAssessmentRun $
--             newCancelReplicationTaskAssessmentRun
--
--         , requestModifyReplicationSubnetGroup $
--             newModifyReplicationSubnetGroup
--
--         , requestDescribeReplicationTaskIndividualAssessments $
--             newDescribeReplicationTaskIndividualAssessments
--
--         , requestApplyPendingMaintenanceAction $
--             newApplyPendingMaintenanceAction
--
--         , requestDescribeAccountAttributes $
--             newDescribeAccountAttributes
--
--         , requestDescribeReplicationInstances $
--             newDescribeReplicationInstances
--
--         , requestDescribeRefreshSchemasStatus $
--             newDescribeRefreshSchemasStatus
--
--         , requestStopReplicationTask $
--             newStopReplicationTask
--
--         , requestModifyReplicationTask $
--             newModifyReplicationTask
--
--         , requestCreateReplicationInstance $
--             newCreateReplicationInstance
--
--         , requestDeleteReplicationSubnetGroup $
--             newDeleteReplicationSubnetGroup
--
--           ]

--     , testGroup "response"
--         [ responseDeleteReplicationInstance $
--             newDeleteReplicationInstanceResponse
--
--         , responseRebootReplicationInstance $
--             newRebootReplicationInstanceResponse
--
--         , responseReloadTables $
--             newReloadTablesResponse
--
--         , responseStartReplicationTaskAssessment $
--             newStartReplicationTaskAssessmentResponse
--
--         , responseDeleteReplicationTaskAssessmentRun $
--             newDeleteReplicationTaskAssessmentRunResponse
--
--         , responseCreateEndpoint $
--             newCreateEndpointResponse
--
--         , responseDescribeSchemas $
--             newDescribeSchemasResponse
--
--         , responseDeleteConnection $
--             newDeleteConnectionResponse
--
--         , responseModifyEventSubscription $
--             newModifyEventSubscriptionResponse
--
--         , responseDescribeReplicationInstanceTaskLogs $
--             newDescribeReplicationInstanceTaskLogsResponse
--
--         , responseDescribeEvents $
--             newDescribeEventsResponse
--
--         , responseDeleteEndpoint $
--             newDeleteEndpointResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDescribeEndpointTypes $
--             newDescribeEndpointTypesResponse
--
--         , responseDeleteReplicationTask $
--             newDeleteReplicationTaskResponse
--
--         , responseDescribeReplicationTaskAssessmentRuns $
--             newDescribeReplicationTaskAssessmentRunsResponse
--
--         , responseDescribeReplicationTaskAssessmentResults $
--             newDescribeReplicationTaskAssessmentResultsResponse
--
--         , responseTestConnection $
--             newTestConnectionResponse
--
--         , responseDescribeConnections $
--             newDescribeConnectionsResponse
--
--         , responseMoveReplicationTask $
--             newMoveReplicationTaskResponse
--
--         , responseRemoveTagsFromResource $
--             newRemoveTagsFromResourceResponse
--
--         , responseModifyEndpoint $
--             newModifyEndpointResponse
--
--         , responseCreateEventSubscription $
--             newCreateEventSubscriptionResponse
--
--         , responseDescribeEndpointSettings $
--             newDescribeEndpointSettingsResponse
--
--         , responseDescribeCertificates $
--             newDescribeCertificatesResponse
--
--         , responseStartReplicationTaskAssessmentRun $
--             newStartReplicationTaskAssessmentRunResponse
--
--         , responseDeleteEventSubscription $
--             newDeleteEventSubscriptionResponse
--
--         , responseDescribeTableStatistics $
--             newDescribeTableStatisticsResponse
--
--         , responseDescribeReplicationSubnetGroups $
--             newDescribeReplicationSubnetGroupsResponse
--
--         , responseStartReplicationTask $
--             newStartReplicationTaskResponse
--
--         , responseDescribeEventSubscriptions $
--             newDescribeEventSubscriptionsResponse
--
--         , responseAddTagsToResource $
--             newAddTagsToResourceResponse
--
--         , responseCreateReplicationSubnetGroup $
--             newCreateReplicationSubnetGroupResponse
--
--         , responseDescribeApplicableIndividualAssessments $
--             newDescribeApplicableIndividualAssessmentsResponse
--
--         , responseDeleteCertificate $
--             newDeleteCertificateResponse
--
--         , responseRefreshSchemas $
--             newRefreshSchemasResponse
--
--         , responseDescribeReplicationTasks $
--             newDescribeReplicationTasksResponse
--
--         , responseDescribeEventCategories $
--             newDescribeEventCategoriesResponse
--
--         , responseDescribeOrderableReplicationInstances $
--             newDescribeOrderableReplicationInstancesResponse
--
--         , responseDescribePendingMaintenanceActions $
--             newDescribePendingMaintenanceActionsResponse
--
--         , responseCreateReplicationTask $
--             newCreateReplicationTaskResponse
--
--         , responseDescribeEndpoints $
--             newDescribeEndpointsResponse
--
--         , responseModifyReplicationInstance $
--             newModifyReplicationInstanceResponse
--
--         , responseImportCertificate $
--             newImportCertificateResponse
--
--         , responseCancelReplicationTaskAssessmentRun $
--             newCancelReplicationTaskAssessmentRunResponse
--
--         , responseModifyReplicationSubnetGroup $
--             newModifyReplicationSubnetGroupResponse
--
--         , responseDescribeReplicationTaskIndividualAssessments $
--             newDescribeReplicationTaskIndividualAssessmentsResponse
--
--         , responseApplyPendingMaintenanceAction $
--             newApplyPendingMaintenanceActionResponse
--
--         , responseDescribeAccountAttributes $
--             newDescribeAccountAttributesResponse
--
--         , responseDescribeReplicationInstances $
--             newDescribeReplicationInstancesResponse
--
--         , responseDescribeRefreshSchemasStatus $
--             newDescribeRefreshSchemasStatusResponse
--
--         , responseStopReplicationTask $
--             newStopReplicationTaskResponse
--
--         , responseModifyReplicationTask $
--             newModifyReplicationTaskResponse
--
--         , responseCreateReplicationInstance $
--             newCreateReplicationInstanceResponse
--
--         , responseDeleteReplicationSubnetGroup $
--             newDeleteReplicationSubnetGroupResponse
--
--           ]
--     ]

-- Requests

requestDeleteReplicationInstance :: DeleteReplicationInstance -> TestTree
requestDeleteReplicationInstance =
  req
    "DeleteReplicationInstance"
    "fixture/DeleteReplicationInstance.yaml"

requestRebootReplicationInstance :: RebootReplicationInstance -> TestTree
requestRebootReplicationInstance =
  req
    "RebootReplicationInstance"
    "fixture/RebootReplicationInstance.yaml"

requestReloadTables :: ReloadTables -> TestTree
requestReloadTables =
  req
    "ReloadTables"
    "fixture/ReloadTables.yaml"

requestStartReplicationTaskAssessment :: StartReplicationTaskAssessment -> TestTree
requestStartReplicationTaskAssessment =
  req
    "StartReplicationTaskAssessment"
    "fixture/StartReplicationTaskAssessment.yaml"

requestDeleteReplicationTaskAssessmentRun :: DeleteReplicationTaskAssessmentRun -> TestTree
requestDeleteReplicationTaskAssessmentRun =
  req
    "DeleteReplicationTaskAssessmentRun"
    "fixture/DeleteReplicationTaskAssessmentRun.yaml"

requestCreateEndpoint :: CreateEndpoint -> TestTree
requestCreateEndpoint =
  req
    "CreateEndpoint"
    "fixture/CreateEndpoint.yaml"

requestDescribeSchemas :: DescribeSchemas -> TestTree
requestDescribeSchemas =
  req
    "DescribeSchemas"
    "fixture/DescribeSchemas.yaml"

requestDeleteConnection :: DeleteConnection -> TestTree
requestDeleteConnection =
  req
    "DeleteConnection"
    "fixture/DeleteConnection.yaml"

requestModifyEventSubscription :: ModifyEventSubscription -> TestTree
requestModifyEventSubscription =
  req
    "ModifyEventSubscription"
    "fixture/ModifyEventSubscription.yaml"

requestDescribeReplicationInstanceTaskLogs :: DescribeReplicationInstanceTaskLogs -> TestTree
requestDescribeReplicationInstanceTaskLogs =
  req
    "DescribeReplicationInstanceTaskLogs"
    "fixture/DescribeReplicationInstanceTaskLogs.yaml"

requestDescribeEvents :: DescribeEvents -> TestTree
requestDescribeEvents =
  req
    "DescribeEvents"
    "fixture/DescribeEvents.yaml"

requestDeleteEndpoint :: DeleteEndpoint -> TestTree
requestDeleteEndpoint =
  req
    "DeleteEndpoint"
    "fixture/DeleteEndpoint.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDescribeEndpointTypes :: DescribeEndpointTypes -> TestTree
requestDescribeEndpointTypes =
  req
    "DescribeEndpointTypes"
    "fixture/DescribeEndpointTypes.yaml"

requestDeleteReplicationTask :: DeleteReplicationTask -> TestTree
requestDeleteReplicationTask =
  req
    "DeleteReplicationTask"
    "fixture/DeleteReplicationTask.yaml"

requestDescribeReplicationTaskAssessmentRuns :: DescribeReplicationTaskAssessmentRuns -> TestTree
requestDescribeReplicationTaskAssessmentRuns =
  req
    "DescribeReplicationTaskAssessmentRuns"
    "fixture/DescribeReplicationTaskAssessmentRuns.yaml"

requestDescribeReplicationTaskAssessmentResults :: DescribeReplicationTaskAssessmentResults -> TestTree
requestDescribeReplicationTaskAssessmentResults =
  req
    "DescribeReplicationTaskAssessmentResults"
    "fixture/DescribeReplicationTaskAssessmentResults.yaml"

requestTestConnection :: TestConnection -> TestTree
requestTestConnection =
  req
    "TestConnection"
    "fixture/TestConnection.yaml"

requestDescribeConnections :: DescribeConnections -> TestTree
requestDescribeConnections =
  req
    "DescribeConnections"
    "fixture/DescribeConnections.yaml"

requestMoveReplicationTask :: MoveReplicationTask -> TestTree
requestMoveReplicationTask =
  req
    "MoveReplicationTask"
    "fixture/MoveReplicationTask.yaml"

requestRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
requestRemoveTagsFromResource =
  req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource.yaml"

requestModifyEndpoint :: ModifyEndpoint -> TestTree
requestModifyEndpoint =
  req
    "ModifyEndpoint"
    "fixture/ModifyEndpoint.yaml"

requestCreateEventSubscription :: CreateEventSubscription -> TestTree
requestCreateEventSubscription =
  req
    "CreateEventSubscription"
    "fixture/CreateEventSubscription.yaml"

requestDescribeEndpointSettings :: DescribeEndpointSettings -> TestTree
requestDescribeEndpointSettings =
  req
    "DescribeEndpointSettings"
    "fixture/DescribeEndpointSettings.yaml"

requestDescribeCertificates :: DescribeCertificates -> TestTree
requestDescribeCertificates =
  req
    "DescribeCertificates"
    "fixture/DescribeCertificates.yaml"

requestStartReplicationTaskAssessmentRun :: StartReplicationTaskAssessmentRun -> TestTree
requestStartReplicationTaskAssessmentRun =
  req
    "StartReplicationTaskAssessmentRun"
    "fixture/StartReplicationTaskAssessmentRun.yaml"

requestDeleteEventSubscription :: DeleteEventSubscription -> TestTree
requestDeleteEventSubscription =
  req
    "DeleteEventSubscription"
    "fixture/DeleteEventSubscription.yaml"

requestDescribeTableStatistics :: DescribeTableStatistics -> TestTree
requestDescribeTableStatistics =
  req
    "DescribeTableStatistics"
    "fixture/DescribeTableStatistics.yaml"

requestDescribeReplicationSubnetGroups :: DescribeReplicationSubnetGroups -> TestTree
requestDescribeReplicationSubnetGroups =
  req
    "DescribeReplicationSubnetGroups"
    "fixture/DescribeReplicationSubnetGroups.yaml"

requestStartReplicationTask :: StartReplicationTask -> TestTree
requestStartReplicationTask =
  req
    "StartReplicationTask"
    "fixture/StartReplicationTask.yaml"

requestDescribeEventSubscriptions :: DescribeEventSubscriptions -> TestTree
requestDescribeEventSubscriptions =
  req
    "DescribeEventSubscriptions"
    "fixture/DescribeEventSubscriptions.yaml"

requestAddTagsToResource :: AddTagsToResource -> TestTree
requestAddTagsToResource =
  req
    "AddTagsToResource"
    "fixture/AddTagsToResource.yaml"

requestCreateReplicationSubnetGroup :: CreateReplicationSubnetGroup -> TestTree
requestCreateReplicationSubnetGroup =
  req
    "CreateReplicationSubnetGroup"
    "fixture/CreateReplicationSubnetGroup.yaml"

requestDescribeApplicableIndividualAssessments :: DescribeApplicableIndividualAssessments -> TestTree
requestDescribeApplicableIndividualAssessments =
  req
    "DescribeApplicableIndividualAssessments"
    "fixture/DescribeApplicableIndividualAssessments.yaml"

requestDeleteCertificate :: DeleteCertificate -> TestTree
requestDeleteCertificate =
  req
    "DeleteCertificate"
    "fixture/DeleteCertificate.yaml"

requestRefreshSchemas :: RefreshSchemas -> TestTree
requestRefreshSchemas =
  req
    "RefreshSchemas"
    "fixture/RefreshSchemas.yaml"

requestDescribeReplicationTasks :: DescribeReplicationTasks -> TestTree
requestDescribeReplicationTasks =
  req
    "DescribeReplicationTasks"
    "fixture/DescribeReplicationTasks.yaml"

requestDescribeEventCategories :: DescribeEventCategories -> TestTree
requestDescribeEventCategories =
  req
    "DescribeEventCategories"
    "fixture/DescribeEventCategories.yaml"

requestDescribeOrderableReplicationInstances :: DescribeOrderableReplicationInstances -> TestTree
requestDescribeOrderableReplicationInstances =
  req
    "DescribeOrderableReplicationInstances"
    "fixture/DescribeOrderableReplicationInstances.yaml"

requestDescribePendingMaintenanceActions :: DescribePendingMaintenanceActions -> TestTree
requestDescribePendingMaintenanceActions =
  req
    "DescribePendingMaintenanceActions"
    "fixture/DescribePendingMaintenanceActions.yaml"

requestCreateReplicationTask :: CreateReplicationTask -> TestTree
requestCreateReplicationTask =
  req
    "CreateReplicationTask"
    "fixture/CreateReplicationTask.yaml"

requestDescribeEndpoints :: DescribeEndpoints -> TestTree
requestDescribeEndpoints =
  req
    "DescribeEndpoints"
    "fixture/DescribeEndpoints.yaml"

requestModifyReplicationInstance :: ModifyReplicationInstance -> TestTree
requestModifyReplicationInstance =
  req
    "ModifyReplicationInstance"
    "fixture/ModifyReplicationInstance.yaml"

requestImportCertificate :: ImportCertificate -> TestTree
requestImportCertificate =
  req
    "ImportCertificate"
    "fixture/ImportCertificate.yaml"

requestCancelReplicationTaskAssessmentRun :: CancelReplicationTaskAssessmentRun -> TestTree
requestCancelReplicationTaskAssessmentRun =
  req
    "CancelReplicationTaskAssessmentRun"
    "fixture/CancelReplicationTaskAssessmentRun.yaml"

requestModifyReplicationSubnetGroup :: ModifyReplicationSubnetGroup -> TestTree
requestModifyReplicationSubnetGroup =
  req
    "ModifyReplicationSubnetGroup"
    "fixture/ModifyReplicationSubnetGroup.yaml"

requestDescribeReplicationTaskIndividualAssessments :: DescribeReplicationTaskIndividualAssessments -> TestTree
requestDescribeReplicationTaskIndividualAssessments =
  req
    "DescribeReplicationTaskIndividualAssessments"
    "fixture/DescribeReplicationTaskIndividualAssessments.yaml"

requestApplyPendingMaintenanceAction :: ApplyPendingMaintenanceAction -> TestTree
requestApplyPendingMaintenanceAction =
  req
    "ApplyPendingMaintenanceAction"
    "fixture/ApplyPendingMaintenanceAction.yaml"

requestDescribeAccountAttributes :: DescribeAccountAttributes -> TestTree
requestDescribeAccountAttributes =
  req
    "DescribeAccountAttributes"
    "fixture/DescribeAccountAttributes.yaml"

requestDescribeReplicationInstances :: DescribeReplicationInstances -> TestTree
requestDescribeReplicationInstances =
  req
    "DescribeReplicationInstances"
    "fixture/DescribeReplicationInstances.yaml"

requestDescribeRefreshSchemasStatus :: DescribeRefreshSchemasStatus -> TestTree
requestDescribeRefreshSchemasStatus =
  req
    "DescribeRefreshSchemasStatus"
    "fixture/DescribeRefreshSchemasStatus.yaml"

requestStopReplicationTask :: StopReplicationTask -> TestTree
requestStopReplicationTask =
  req
    "StopReplicationTask"
    "fixture/StopReplicationTask.yaml"

requestModifyReplicationTask :: ModifyReplicationTask -> TestTree
requestModifyReplicationTask =
  req
    "ModifyReplicationTask"
    "fixture/ModifyReplicationTask.yaml"

requestCreateReplicationInstance :: CreateReplicationInstance -> TestTree
requestCreateReplicationInstance =
  req
    "CreateReplicationInstance"
    "fixture/CreateReplicationInstance.yaml"

requestDeleteReplicationSubnetGroup :: DeleteReplicationSubnetGroup -> TestTree
requestDeleteReplicationSubnetGroup =
  req
    "DeleteReplicationSubnetGroup"
    "fixture/DeleteReplicationSubnetGroup.yaml"

-- Responses

responseDeleteReplicationInstance :: DeleteReplicationInstanceResponse -> TestTree
responseDeleteReplicationInstance =
  res
    "DeleteReplicationInstanceResponse"
    "fixture/DeleteReplicationInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteReplicationInstance)

responseRebootReplicationInstance :: RebootReplicationInstanceResponse -> TestTree
responseRebootReplicationInstance =
  res
    "RebootReplicationInstanceResponse"
    "fixture/RebootReplicationInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy RebootReplicationInstance)

responseReloadTables :: ReloadTablesResponse -> TestTree
responseReloadTables =
  res
    "ReloadTablesResponse"
    "fixture/ReloadTablesResponse.proto"
    defaultService
    (Proxy :: Proxy ReloadTables)

responseStartReplicationTaskAssessment :: StartReplicationTaskAssessmentResponse -> TestTree
responseStartReplicationTaskAssessment =
  res
    "StartReplicationTaskAssessmentResponse"
    "fixture/StartReplicationTaskAssessmentResponse.proto"
    defaultService
    (Proxy :: Proxy StartReplicationTaskAssessment)

responseDeleteReplicationTaskAssessmentRun :: DeleteReplicationTaskAssessmentRunResponse -> TestTree
responseDeleteReplicationTaskAssessmentRun =
  res
    "DeleteReplicationTaskAssessmentRunResponse"
    "fixture/DeleteReplicationTaskAssessmentRunResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteReplicationTaskAssessmentRun)

responseCreateEndpoint :: CreateEndpointResponse -> TestTree
responseCreateEndpoint =
  res
    "CreateEndpointResponse"
    "fixture/CreateEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEndpoint)

responseDescribeSchemas :: DescribeSchemasResponse -> TestTree
responseDescribeSchemas =
  res
    "DescribeSchemasResponse"
    "fixture/DescribeSchemasResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSchemas)

responseDeleteConnection :: DeleteConnectionResponse -> TestTree
responseDeleteConnection =
  res
    "DeleteConnectionResponse"
    "fixture/DeleteConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConnection)

responseModifyEventSubscription :: ModifyEventSubscriptionResponse -> TestTree
responseModifyEventSubscription =
  res
    "ModifyEventSubscriptionResponse"
    "fixture/ModifyEventSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyEventSubscription)

responseDescribeReplicationInstanceTaskLogs :: DescribeReplicationInstanceTaskLogsResponse -> TestTree
responseDescribeReplicationInstanceTaskLogs =
  res
    "DescribeReplicationInstanceTaskLogsResponse"
    "fixture/DescribeReplicationInstanceTaskLogsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReplicationInstanceTaskLogs)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents =
  res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEvents)

responseDeleteEndpoint :: DeleteEndpointResponse -> TestTree
responseDeleteEndpoint =
  res
    "DeleteEndpointResponse"
    "fixture/DeleteEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEndpoint)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseDescribeEndpointTypes :: DescribeEndpointTypesResponse -> TestTree
responseDescribeEndpointTypes =
  res
    "DescribeEndpointTypesResponse"
    "fixture/DescribeEndpointTypesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEndpointTypes)

responseDeleteReplicationTask :: DeleteReplicationTaskResponse -> TestTree
responseDeleteReplicationTask =
  res
    "DeleteReplicationTaskResponse"
    "fixture/DeleteReplicationTaskResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteReplicationTask)

responseDescribeReplicationTaskAssessmentRuns :: DescribeReplicationTaskAssessmentRunsResponse -> TestTree
responseDescribeReplicationTaskAssessmentRuns =
  res
    "DescribeReplicationTaskAssessmentRunsResponse"
    "fixture/DescribeReplicationTaskAssessmentRunsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReplicationTaskAssessmentRuns)

responseDescribeReplicationTaskAssessmentResults :: DescribeReplicationTaskAssessmentResultsResponse -> TestTree
responseDescribeReplicationTaskAssessmentResults =
  res
    "DescribeReplicationTaskAssessmentResultsResponse"
    "fixture/DescribeReplicationTaskAssessmentResultsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReplicationTaskAssessmentResults)

responseTestConnection :: TestConnectionResponse -> TestTree
responseTestConnection =
  res
    "TestConnectionResponse"
    "fixture/TestConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy TestConnection)

responseDescribeConnections :: DescribeConnectionsResponse -> TestTree
responseDescribeConnections =
  res
    "DescribeConnectionsResponse"
    "fixture/DescribeConnectionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConnections)

responseMoveReplicationTask :: MoveReplicationTaskResponse -> TestTree
responseMoveReplicationTask =
  res
    "MoveReplicationTaskResponse"
    "fixture/MoveReplicationTaskResponse.proto"
    defaultService
    (Proxy :: Proxy MoveReplicationTask)

responseRemoveTagsFromResource :: RemoveTagsFromResourceResponse -> TestTree
responseRemoveTagsFromResource =
  res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveTagsFromResource)

responseModifyEndpoint :: ModifyEndpointResponse -> TestTree
responseModifyEndpoint =
  res
    "ModifyEndpointResponse"
    "fixture/ModifyEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyEndpoint)

responseCreateEventSubscription :: CreateEventSubscriptionResponse -> TestTree
responseCreateEventSubscription =
  res
    "CreateEventSubscriptionResponse"
    "fixture/CreateEventSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEventSubscription)

responseDescribeEndpointSettings :: DescribeEndpointSettingsResponse -> TestTree
responseDescribeEndpointSettings =
  res
    "DescribeEndpointSettingsResponse"
    "fixture/DescribeEndpointSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEndpointSettings)

responseDescribeCertificates :: DescribeCertificatesResponse -> TestTree
responseDescribeCertificates =
  res
    "DescribeCertificatesResponse"
    "fixture/DescribeCertificatesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCertificates)

responseStartReplicationTaskAssessmentRun :: StartReplicationTaskAssessmentRunResponse -> TestTree
responseStartReplicationTaskAssessmentRun =
  res
    "StartReplicationTaskAssessmentRunResponse"
    "fixture/StartReplicationTaskAssessmentRunResponse.proto"
    defaultService
    (Proxy :: Proxy StartReplicationTaskAssessmentRun)

responseDeleteEventSubscription :: DeleteEventSubscriptionResponse -> TestTree
responseDeleteEventSubscription =
  res
    "DeleteEventSubscriptionResponse"
    "fixture/DeleteEventSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEventSubscription)

responseDescribeTableStatistics :: DescribeTableStatisticsResponse -> TestTree
responseDescribeTableStatistics =
  res
    "DescribeTableStatisticsResponse"
    "fixture/DescribeTableStatisticsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTableStatistics)

responseDescribeReplicationSubnetGroups :: DescribeReplicationSubnetGroupsResponse -> TestTree
responseDescribeReplicationSubnetGroups =
  res
    "DescribeReplicationSubnetGroupsResponse"
    "fixture/DescribeReplicationSubnetGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReplicationSubnetGroups)

responseStartReplicationTask :: StartReplicationTaskResponse -> TestTree
responseStartReplicationTask =
  res
    "StartReplicationTaskResponse"
    "fixture/StartReplicationTaskResponse.proto"
    defaultService
    (Proxy :: Proxy StartReplicationTask)

responseDescribeEventSubscriptions :: DescribeEventSubscriptionsResponse -> TestTree
responseDescribeEventSubscriptions =
  res
    "DescribeEventSubscriptionsResponse"
    "fixture/DescribeEventSubscriptionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEventSubscriptions)

responseAddTagsToResource :: AddTagsToResourceResponse -> TestTree
responseAddTagsToResource =
  res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    defaultService
    (Proxy :: Proxy AddTagsToResource)

responseCreateReplicationSubnetGroup :: CreateReplicationSubnetGroupResponse -> TestTree
responseCreateReplicationSubnetGroup =
  res
    "CreateReplicationSubnetGroupResponse"
    "fixture/CreateReplicationSubnetGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateReplicationSubnetGroup)

responseDescribeApplicableIndividualAssessments :: DescribeApplicableIndividualAssessmentsResponse -> TestTree
responseDescribeApplicableIndividualAssessments =
  res
    "DescribeApplicableIndividualAssessmentsResponse"
    "fixture/DescribeApplicableIndividualAssessmentsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeApplicableIndividualAssessments)

responseDeleteCertificate :: DeleteCertificateResponse -> TestTree
responseDeleteCertificate =
  res
    "DeleteCertificateResponse"
    "fixture/DeleteCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCertificate)

responseRefreshSchemas :: RefreshSchemasResponse -> TestTree
responseRefreshSchemas =
  res
    "RefreshSchemasResponse"
    "fixture/RefreshSchemasResponse.proto"
    defaultService
    (Proxy :: Proxy RefreshSchemas)

responseDescribeReplicationTasks :: DescribeReplicationTasksResponse -> TestTree
responseDescribeReplicationTasks =
  res
    "DescribeReplicationTasksResponse"
    "fixture/DescribeReplicationTasksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReplicationTasks)

responseDescribeEventCategories :: DescribeEventCategoriesResponse -> TestTree
responseDescribeEventCategories =
  res
    "DescribeEventCategoriesResponse"
    "fixture/DescribeEventCategoriesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEventCategories)

responseDescribeOrderableReplicationInstances :: DescribeOrderableReplicationInstancesResponse -> TestTree
responseDescribeOrderableReplicationInstances =
  res
    "DescribeOrderableReplicationInstancesResponse"
    "fixture/DescribeOrderableReplicationInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeOrderableReplicationInstances)

responseDescribePendingMaintenanceActions :: DescribePendingMaintenanceActionsResponse -> TestTree
responseDescribePendingMaintenanceActions =
  res
    "DescribePendingMaintenanceActionsResponse"
    "fixture/DescribePendingMaintenanceActionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePendingMaintenanceActions)

responseCreateReplicationTask :: CreateReplicationTaskResponse -> TestTree
responseCreateReplicationTask =
  res
    "CreateReplicationTaskResponse"
    "fixture/CreateReplicationTaskResponse.proto"
    defaultService
    (Proxy :: Proxy CreateReplicationTask)

responseDescribeEndpoints :: DescribeEndpointsResponse -> TestTree
responseDescribeEndpoints =
  res
    "DescribeEndpointsResponse"
    "fixture/DescribeEndpointsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEndpoints)

responseModifyReplicationInstance :: ModifyReplicationInstanceResponse -> TestTree
responseModifyReplicationInstance =
  res
    "ModifyReplicationInstanceResponse"
    "fixture/ModifyReplicationInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyReplicationInstance)

responseImportCertificate :: ImportCertificateResponse -> TestTree
responseImportCertificate =
  res
    "ImportCertificateResponse"
    "fixture/ImportCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy ImportCertificate)

responseCancelReplicationTaskAssessmentRun :: CancelReplicationTaskAssessmentRunResponse -> TestTree
responseCancelReplicationTaskAssessmentRun =
  res
    "CancelReplicationTaskAssessmentRunResponse"
    "fixture/CancelReplicationTaskAssessmentRunResponse.proto"
    defaultService
    (Proxy :: Proxy CancelReplicationTaskAssessmentRun)

responseModifyReplicationSubnetGroup :: ModifyReplicationSubnetGroupResponse -> TestTree
responseModifyReplicationSubnetGroup =
  res
    "ModifyReplicationSubnetGroupResponse"
    "fixture/ModifyReplicationSubnetGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyReplicationSubnetGroup)

responseDescribeReplicationTaskIndividualAssessments :: DescribeReplicationTaskIndividualAssessmentsResponse -> TestTree
responseDescribeReplicationTaskIndividualAssessments =
  res
    "DescribeReplicationTaskIndividualAssessmentsResponse"
    "fixture/DescribeReplicationTaskIndividualAssessmentsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReplicationTaskIndividualAssessments)

responseApplyPendingMaintenanceAction :: ApplyPendingMaintenanceActionResponse -> TestTree
responseApplyPendingMaintenanceAction =
  res
    "ApplyPendingMaintenanceActionResponse"
    "fixture/ApplyPendingMaintenanceActionResponse.proto"
    defaultService
    (Proxy :: Proxy ApplyPendingMaintenanceAction)

responseDescribeAccountAttributes :: DescribeAccountAttributesResponse -> TestTree
responseDescribeAccountAttributes =
  res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAccountAttributes)

responseDescribeReplicationInstances :: DescribeReplicationInstancesResponse -> TestTree
responseDescribeReplicationInstances =
  res
    "DescribeReplicationInstancesResponse"
    "fixture/DescribeReplicationInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReplicationInstances)

responseDescribeRefreshSchemasStatus :: DescribeRefreshSchemasStatusResponse -> TestTree
responseDescribeRefreshSchemasStatus =
  res
    "DescribeRefreshSchemasStatusResponse"
    "fixture/DescribeRefreshSchemasStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeRefreshSchemasStatus)

responseStopReplicationTask :: StopReplicationTaskResponse -> TestTree
responseStopReplicationTask =
  res
    "StopReplicationTaskResponse"
    "fixture/StopReplicationTaskResponse.proto"
    defaultService
    (Proxy :: Proxy StopReplicationTask)

responseModifyReplicationTask :: ModifyReplicationTaskResponse -> TestTree
responseModifyReplicationTask =
  res
    "ModifyReplicationTaskResponse"
    "fixture/ModifyReplicationTaskResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyReplicationTask)

responseCreateReplicationInstance :: CreateReplicationInstanceResponse -> TestTree
responseCreateReplicationInstance =
  res
    "CreateReplicationInstanceResponse"
    "fixture/CreateReplicationInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy CreateReplicationInstance)

responseDeleteReplicationSubnetGroup :: DeleteReplicationSubnetGroupResponse -> TestTree
responseDeleteReplicationSubnetGroup =
  res
    "DeleteReplicationSubnetGroupResponse"
    "fixture/DeleteReplicationSubnetGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteReplicationSubnetGroup)

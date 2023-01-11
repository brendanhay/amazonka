{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.DMS
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.DMS where

import Amazonka.DMS
import qualified Data.Proxy as Proxy
import Test.Amazonka.DMS.Internal
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
--         [ requestAddTagsToResource $
--             newAddTagsToResource
--
--         , requestApplyPendingMaintenanceAction $
--             newApplyPendingMaintenanceAction
--
--         , requestCancelReplicationTaskAssessmentRun $
--             newCancelReplicationTaskAssessmentRun
--
--         , requestCreateEndpoint $
--             newCreateEndpoint
--
--         , requestCreateEventSubscription $
--             newCreateEventSubscription
--
--         , requestCreateFleetAdvisorCollector $
--             newCreateFleetAdvisorCollector
--
--         , requestCreateReplicationInstance $
--             newCreateReplicationInstance
--
--         , requestCreateReplicationSubnetGroup $
--             newCreateReplicationSubnetGroup
--
--         , requestCreateReplicationTask $
--             newCreateReplicationTask
--
--         , requestDeleteCertificate $
--             newDeleteCertificate
--
--         , requestDeleteConnection $
--             newDeleteConnection
--
--         , requestDeleteEndpoint $
--             newDeleteEndpoint
--
--         , requestDeleteEventSubscription $
--             newDeleteEventSubscription
--
--         , requestDeleteFleetAdvisorCollector $
--             newDeleteFleetAdvisorCollector
--
--         , requestDeleteFleetAdvisorDatabases $
--             newDeleteFleetAdvisorDatabases
--
--         , requestDeleteReplicationInstance $
--             newDeleteReplicationInstance
--
--         , requestDeleteReplicationSubnetGroup $
--             newDeleteReplicationSubnetGroup
--
--         , requestDeleteReplicationTask $
--             newDeleteReplicationTask
--
--         , requestDeleteReplicationTaskAssessmentRun $
--             newDeleteReplicationTaskAssessmentRun
--
--         , requestDescribeAccountAttributes $
--             newDescribeAccountAttributes
--
--         , requestDescribeApplicableIndividualAssessments $
--             newDescribeApplicableIndividualAssessments
--
--         , requestDescribeCertificates $
--             newDescribeCertificates
--
--         , requestDescribeConnections $
--             newDescribeConnections
--
--         , requestDescribeEndpointSettings $
--             newDescribeEndpointSettings
--
--         , requestDescribeEndpointTypes $
--             newDescribeEndpointTypes
--
--         , requestDescribeEndpoints $
--             newDescribeEndpoints
--
--         , requestDescribeEventCategories $
--             newDescribeEventCategories
--
--         , requestDescribeEventSubscriptions $
--             newDescribeEventSubscriptions
--
--         , requestDescribeEvents $
--             newDescribeEvents
--
--         , requestDescribeFleetAdvisorCollectors $
--             newDescribeFleetAdvisorCollectors
--
--         , requestDescribeFleetAdvisorDatabases $
--             newDescribeFleetAdvisorDatabases
--
--         , requestDescribeFleetAdvisorLsaAnalysis $
--             newDescribeFleetAdvisorLsaAnalysis
--
--         , requestDescribeFleetAdvisorSchemaObjectSummary $
--             newDescribeFleetAdvisorSchemaObjectSummary
--
--         , requestDescribeFleetAdvisorSchemas $
--             newDescribeFleetAdvisorSchemas
--
--         , requestDescribeOrderableReplicationInstances $
--             newDescribeOrderableReplicationInstances
--
--         , requestDescribePendingMaintenanceActions $
--             newDescribePendingMaintenanceActions
--
--         , requestDescribeRefreshSchemasStatus $
--             newDescribeRefreshSchemasStatus
--
--         , requestDescribeReplicationInstanceTaskLogs $
--             newDescribeReplicationInstanceTaskLogs
--
--         , requestDescribeReplicationInstances $
--             newDescribeReplicationInstances
--
--         , requestDescribeReplicationSubnetGroups $
--             newDescribeReplicationSubnetGroups
--
--         , requestDescribeReplicationTaskAssessmentResults $
--             newDescribeReplicationTaskAssessmentResults
--
--         , requestDescribeReplicationTaskAssessmentRuns $
--             newDescribeReplicationTaskAssessmentRuns
--
--         , requestDescribeReplicationTaskIndividualAssessments $
--             newDescribeReplicationTaskIndividualAssessments
--
--         , requestDescribeReplicationTasks $
--             newDescribeReplicationTasks
--
--         , requestDescribeSchemas $
--             newDescribeSchemas
--
--         , requestDescribeTableStatistics $
--             newDescribeTableStatistics
--
--         , requestImportCertificate $
--             newImportCertificate
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestModifyEndpoint $
--             newModifyEndpoint
--
--         , requestModifyEventSubscription $
--             newModifyEventSubscription
--
--         , requestModifyReplicationInstance $
--             newModifyReplicationInstance
--
--         , requestModifyReplicationSubnetGroup $
--             newModifyReplicationSubnetGroup
--
--         , requestModifyReplicationTask $
--             newModifyReplicationTask
--
--         , requestMoveReplicationTask $
--             newMoveReplicationTask
--
--         , requestRebootReplicationInstance $
--             newRebootReplicationInstance
--
--         , requestRefreshSchemas $
--             newRefreshSchemas
--
--         , requestReloadTables $
--             newReloadTables
--
--         , requestRemoveTagsFromResource $
--             newRemoveTagsFromResource
--
--         , requestRunFleetAdvisorLsaAnalysis $
--             newRunFleetAdvisorLsaAnalysis
--
--         , requestStartReplicationTask $
--             newStartReplicationTask
--
--         , requestStartReplicationTaskAssessment $
--             newStartReplicationTaskAssessment
--
--         , requestStartReplicationTaskAssessmentRun $
--             newStartReplicationTaskAssessmentRun
--
--         , requestStopReplicationTask $
--             newStopReplicationTask
--
--         , requestTestConnection $
--             newTestConnection
--
--         , requestUpdateSubscriptionsToEventBridge $
--             newUpdateSubscriptionsToEventBridge
--
--           ]

--     , testGroup "response"
--         [ responseAddTagsToResource $
--             newAddTagsToResourceResponse
--
--         , responseApplyPendingMaintenanceAction $
--             newApplyPendingMaintenanceActionResponse
--
--         , responseCancelReplicationTaskAssessmentRun $
--             newCancelReplicationTaskAssessmentRunResponse
--
--         , responseCreateEndpoint $
--             newCreateEndpointResponse
--
--         , responseCreateEventSubscription $
--             newCreateEventSubscriptionResponse
--
--         , responseCreateFleetAdvisorCollector $
--             newCreateFleetAdvisorCollectorResponse
--
--         , responseCreateReplicationInstance $
--             newCreateReplicationInstanceResponse
--
--         , responseCreateReplicationSubnetGroup $
--             newCreateReplicationSubnetGroupResponse
--
--         , responseCreateReplicationTask $
--             newCreateReplicationTaskResponse
--
--         , responseDeleteCertificate $
--             newDeleteCertificateResponse
--
--         , responseDeleteConnection $
--             newDeleteConnectionResponse
--
--         , responseDeleteEndpoint $
--             newDeleteEndpointResponse
--
--         , responseDeleteEventSubscription $
--             newDeleteEventSubscriptionResponse
--
--         , responseDeleteFleetAdvisorCollector $
--             newDeleteFleetAdvisorCollectorResponse
--
--         , responseDeleteFleetAdvisorDatabases $
--             newDeleteFleetAdvisorDatabasesResponse
--
--         , responseDeleteReplicationInstance $
--             newDeleteReplicationInstanceResponse
--
--         , responseDeleteReplicationSubnetGroup $
--             newDeleteReplicationSubnetGroupResponse
--
--         , responseDeleteReplicationTask $
--             newDeleteReplicationTaskResponse
--
--         , responseDeleteReplicationTaskAssessmentRun $
--             newDeleteReplicationTaskAssessmentRunResponse
--
--         , responseDescribeAccountAttributes $
--             newDescribeAccountAttributesResponse
--
--         , responseDescribeApplicableIndividualAssessments $
--             newDescribeApplicableIndividualAssessmentsResponse
--
--         , responseDescribeCertificates $
--             newDescribeCertificatesResponse
--
--         , responseDescribeConnections $
--             newDescribeConnectionsResponse
--
--         , responseDescribeEndpointSettings $
--             newDescribeEndpointSettingsResponse
--
--         , responseDescribeEndpointTypes $
--             newDescribeEndpointTypesResponse
--
--         , responseDescribeEndpoints $
--             newDescribeEndpointsResponse
--
--         , responseDescribeEventCategories $
--             newDescribeEventCategoriesResponse
--
--         , responseDescribeEventSubscriptions $
--             newDescribeEventSubscriptionsResponse
--
--         , responseDescribeEvents $
--             newDescribeEventsResponse
--
--         , responseDescribeFleetAdvisorCollectors $
--             newDescribeFleetAdvisorCollectorsResponse
--
--         , responseDescribeFleetAdvisorDatabases $
--             newDescribeFleetAdvisorDatabasesResponse
--
--         , responseDescribeFleetAdvisorLsaAnalysis $
--             newDescribeFleetAdvisorLsaAnalysisResponse
--
--         , responseDescribeFleetAdvisorSchemaObjectSummary $
--             newDescribeFleetAdvisorSchemaObjectSummaryResponse
--
--         , responseDescribeFleetAdvisorSchemas $
--             newDescribeFleetAdvisorSchemasResponse
--
--         , responseDescribeOrderableReplicationInstances $
--             newDescribeOrderableReplicationInstancesResponse
--
--         , responseDescribePendingMaintenanceActions $
--             newDescribePendingMaintenanceActionsResponse
--
--         , responseDescribeRefreshSchemasStatus $
--             newDescribeRefreshSchemasStatusResponse
--
--         , responseDescribeReplicationInstanceTaskLogs $
--             newDescribeReplicationInstanceTaskLogsResponse
--
--         , responseDescribeReplicationInstances $
--             newDescribeReplicationInstancesResponse
--
--         , responseDescribeReplicationSubnetGroups $
--             newDescribeReplicationSubnetGroupsResponse
--
--         , responseDescribeReplicationTaskAssessmentResults $
--             newDescribeReplicationTaskAssessmentResultsResponse
--
--         , responseDescribeReplicationTaskAssessmentRuns $
--             newDescribeReplicationTaskAssessmentRunsResponse
--
--         , responseDescribeReplicationTaskIndividualAssessments $
--             newDescribeReplicationTaskIndividualAssessmentsResponse
--
--         , responseDescribeReplicationTasks $
--             newDescribeReplicationTasksResponse
--
--         , responseDescribeSchemas $
--             newDescribeSchemasResponse
--
--         , responseDescribeTableStatistics $
--             newDescribeTableStatisticsResponse
--
--         , responseImportCertificate $
--             newImportCertificateResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseModifyEndpoint $
--             newModifyEndpointResponse
--
--         , responseModifyEventSubscription $
--             newModifyEventSubscriptionResponse
--
--         , responseModifyReplicationInstance $
--             newModifyReplicationInstanceResponse
--
--         , responseModifyReplicationSubnetGroup $
--             newModifyReplicationSubnetGroupResponse
--
--         , responseModifyReplicationTask $
--             newModifyReplicationTaskResponse
--
--         , responseMoveReplicationTask $
--             newMoveReplicationTaskResponse
--
--         , responseRebootReplicationInstance $
--             newRebootReplicationInstanceResponse
--
--         , responseRefreshSchemas $
--             newRefreshSchemasResponse
--
--         , responseReloadTables $
--             newReloadTablesResponse
--
--         , responseRemoveTagsFromResource $
--             newRemoveTagsFromResourceResponse
--
--         , responseRunFleetAdvisorLsaAnalysis $
--             newRunFleetAdvisorLsaAnalysisResponse
--
--         , responseStartReplicationTask $
--             newStartReplicationTaskResponse
--
--         , responseStartReplicationTaskAssessment $
--             newStartReplicationTaskAssessmentResponse
--
--         , responseStartReplicationTaskAssessmentRun $
--             newStartReplicationTaskAssessmentRunResponse
--
--         , responseStopReplicationTask $
--             newStopReplicationTaskResponse
--
--         , responseTestConnection $
--             newTestConnectionResponse
--
--         , responseUpdateSubscriptionsToEventBridge $
--             newUpdateSubscriptionsToEventBridgeResponse
--
--           ]
--     ]

-- Requests

requestAddTagsToResource :: AddTagsToResource -> TestTree
requestAddTagsToResource =
  req
    "AddTagsToResource"
    "fixture/AddTagsToResource.yaml"

requestApplyPendingMaintenanceAction :: ApplyPendingMaintenanceAction -> TestTree
requestApplyPendingMaintenanceAction =
  req
    "ApplyPendingMaintenanceAction"
    "fixture/ApplyPendingMaintenanceAction.yaml"

requestCancelReplicationTaskAssessmentRun :: CancelReplicationTaskAssessmentRun -> TestTree
requestCancelReplicationTaskAssessmentRun =
  req
    "CancelReplicationTaskAssessmentRun"
    "fixture/CancelReplicationTaskAssessmentRun.yaml"

requestCreateEndpoint :: CreateEndpoint -> TestTree
requestCreateEndpoint =
  req
    "CreateEndpoint"
    "fixture/CreateEndpoint.yaml"

requestCreateEventSubscription :: CreateEventSubscription -> TestTree
requestCreateEventSubscription =
  req
    "CreateEventSubscription"
    "fixture/CreateEventSubscription.yaml"

requestCreateFleetAdvisorCollector :: CreateFleetAdvisorCollector -> TestTree
requestCreateFleetAdvisorCollector =
  req
    "CreateFleetAdvisorCollector"
    "fixture/CreateFleetAdvisorCollector.yaml"

requestCreateReplicationInstance :: CreateReplicationInstance -> TestTree
requestCreateReplicationInstance =
  req
    "CreateReplicationInstance"
    "fixture/CreateReplicationInstance.yaml"

requestCreateReplicationSubnetGroup :: CreateReplicationSubnetGroup -> TestTree
requestCreateReplicationSubnetGroup =
  req
    "CreateReplicationSubnetGroup"
    "fixture/CreateReplicationSubnetGroup.yaml"

requestCreateReplicationTask :: CreateReplicationTask -> TestTree
requestCreateReplicationTask =
  req
    "CreateReplicationTask"
    "fixture/CreateReplicationTask.yaml"

requestDeleteCertificate :: DeleteCertificate -> TestTree
requestDeleteCertificate =
  req
    "DeleteCertificate"
    "fixture/DeleteCertificate.yaml"

requestDeleteConnection :: DeleteConnection -> TestTree
requestDeleteConnection =
  req
    "DeleteConnection"
    "fixture/DeleteConnection.yaml"

requestDeleteEndpoint :: DeleteEndpoint -> TestTree
requestDeleteEndpoint =
  req
    "DeleteEndpoint"
    "fixture/DeleteEndpoint.yaml"

requestDeleteEventSubscription :: DeleteEventSubscription -> TestTree
requestDeleteEventSubscription =
  req
    "DeleteEventSubscription"
    "fixture/DeleteEventSubscription.yaml"

requestDeleteFleetAdvisorCollector :: DeleteFleetAdvisorCollector -> TestTree
requestDeleteFleetAdvisorCollector =
  req
    "DeleteFleetAdvisorCollector"
    "fixture/DeleteFleetAdvisorCollector.yaml"

requestDeleteFleetAdvisorDatabases :: DeleteFleetAdvisorDatabases -> TestTree
requestDeleteFleetAdvisorDatabases =
  req
    "DeleteFleetAdvisorDatabases"
    "fixture/DeleteFleetAdvisorDatabases.yaml"

requestDeleteReplicationInstance :: DeleteReplicationInstance -> TestTree
requestDeleteReplicationInstance =
  req
    "DeleteReplicationInstance"
    "fixture/DeleteReplicationInstance.yaml"

requestDeleteReplicationSubnetGroup :: DeleteReplicationSubnetGroup -> TestTree
requestDeleteReplicationSubnetGroup =
  req
    "DeleteReplicationSubnetGroup"
    "fixture/DeleteReplicationSubnetGroup.yaml"

requestDeleteReplicationTask :: DeleteReplicationTask -> TestTree
requestDeleteReplicationTask =
  req
    "DeleteReplicationTask"
    "fixture/DeleteReplicationTask.yaml"

requestDeleteReplicationTaskAssessmentRun :: DeleteReplicationTaskAssessmentRun -> TestTree
requestDeleteReplicationTaskAssessmentRun =
  req
    "DeleteReplicationTaskAssessmentRun"
    "fixture/DeleteReplicationTaskAssessmentRun.yaml"

requestDescribeAccountAttributes :: DescribeAccountAttributes -> TestTree
requestDescribeAccountAttributes =
  req
    "DescribeAccountAttributes"
    "fixture/DescribeAccountAttributes.yaml"

requestDescribeApplicableIndividualAssessments :: DescribeApplicableIndividualAssessments -> TestTree
requestDescribeApplicableIndividualAssessments =
  req
    "DescribeApplicableIndividualAssessments"
    "fixture/DescribeApplicableIndividualAssessments.yaml"

requestDescribeCertificates :: DescribeCertificates -> TestTree
requestDescribeCertificates =
  req
    "DescribeCertificates"
    "fixture/DescribeCertificates.yaml"

requestDescribeConnections :: DescribeConnections -> TestTree
requestDescribeConnections =
  req
    "DescribeConnections"
    "fixture/DescribeConnections.yaml"

requestDescribeEndpointSettings :: DescribeEndpointSettings -> TestTree
requestDescribeEndpointSettings =
  req
    "DescribeEndpointSettings"
    "fixture/DescribeEndpointSettings.yaml"

requestDescribeEndpointTypes :: DescribeEndpointTypes -> TestTree
requestDescribeEndpointTypes =
  req
    "DescribeEndpointTypes"
    "fixture/DescribeEndpointTypes.yaml"

requestDescribeEndpoints :: DescribeEndpoints -> TestTree
requestDescribeEndpoints =
  req
    "DescribeEndpoints"
    "fixture/DescribeEndpoints.yaml"

requestDescribeEventCategories :: DescribeEventCategories -> TestTree
requestDescribeEventCategories =
  req
    "DescribeEventCategories"
    "fixture/DescribeEventCategories.yaml"

requestDescribeEventSubscriptions :: DescribeEventSubscriptions -> TestTree
requestDescribeEventSubscriptions =
  req
    "DescribeEventSubscriptions"
    "fixture/DescribeEventSubscriptions.yaml"

requestDescribeEvents :: DescribeEvents -> TestTree
requestDescribeEvents =
  req
    "DescribeEvents"
    "fixture/DescribeEvents.yaml"

requestDescribeFleetAdvisorCollectors :: DescribeFleetAdvisorCollectors -> TestTree
requestDescribeFleetAdvisorCollectors =
  req
    "DescribeFleetAdvisorCollectors"
    "fixture/DescribeFleetAdvisorCollectors.yaml"

requestDescribeFleetAdvisorDatabases :: DescribeFleetAdvisorDatabases -> TestTree
requestDescribeFleetAdvisorDatabases =
  req
    "DescribeFleetAdvisorDatabases"
    "fixture/DescribeFleetAdvisorDatabases.yaml"

requestDescribeFleetAdvisorLsaAnalysis :: DescribeFleetAdvisorLsaAnalysis -> TestTree
requestDescribeFleetAdvisorLsaAnalysis =
  req
    "DescribeFleetAdvisorLsaAnalysis"
    "fixture/DescribeFleetAdvisorLsaAnalysis.yaml"

requestDescribeFleetAdvisorSchemaObjectSummary :: DescribeFleetAdvisorSchemaObjectSummary -> TestTree
requestDescribeFleetAdvisorSchemaObjectSummary =
  req
    "DescribeFleetAdvisorSchemaObjectSummary"
    "fixture/DescribeFleetAdvisorSchemaObjectSummary.yaml"

requestDescribeFleetAdvisorSchemas :: DescribeFleetAdvisorSchemas -> TestTree
requestDescribeFleetAdvisorSchemas =
  req
    "DescribeFleetAdvisorSchemas"
    "fixture/DescribeFleetAdvisorSchemas.yaml"

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

requestDescribeRefreshSchemasStatus :: DescribeRefreshSchemasStatus -> TestTree
requestDescribeRefreshSchemasStatus =
  req
    "DescribeRefreshSchemasStatus"
    "fixture/DescribeRefreshSchemasStatus.yaml"

requestDescribeReplicationInstanceTaskLogs :: DescribeReplicationInstanceTaskLogs -> TestTree
requestDescribeReplicationInstanceTaskLogs =
  req
    "DescribeReplicationInstanceTaskLogs"
    "fixture/DescribeReplicationInstanceTaskLogs.yaml"

requestDescribeReplicationInstances :: DescribeReplicationInstances -> TestTree
requestDescribeReplicationInstances =
  req
    "DescribeReplicationInstances"
    "fixture/DescribeReplicationInstances.yaml"

requestDescribeReplicationSubnetGroups :: DescribeReplicationSubnetGroups -> TestTree
requestDescribeReplicationSubnetGroups =
  req
    "DescribeReplicationSubnetGroups"
    "fixture/DescribeReplicationSubnetGroups.yaml"

requestDescribeReplicationTaskAssessmentResults :: DescribeReplicationTaskAssessmentResults -> TestTree
requestDescribeReplicationTaskAssessmentResults =
  req
    "DescribeReplicationTaskAssessmentResults"
    "fixture/DescribeReplicationTaskAssessmentResults.yaml"

requestDescribeReplicationTaskAssessmentRuns :: DescribeReplicationTaskAssessmentRuns -> TestTree
requestDescribeReplicationTaskAssessmentRuns =
  req
    "DescribeReplicationTaskAssessmentRuns"
    "fixture/DescribeReplicationTaskAssessmentRuns.yaml"

requestDescribeReplicationTaskIndividualAssessments :: DescribeReplicationTaskIndividualAssessments -> TestTree
requestDescribeReplicationTaskIndividualAssessments =
  req
    "DescribeReplicationTaskIndividualAssessments"
    "fixture/DescribeReplicationTaskIndividualAssessments.yaml"

requestDescribeReplicationTasks :: DescribeReplicationTasks -> TestTree
requestDescribeReplicationTasks =
  req
    "DescribeReplicationTasks"
    "fixture/DescribeReplicationTasks.yaml"

requestDescribeSchemas :: DescribeSchemas -> TestTree
requestDescribeSchemas =
  req
    "DescribeSchemas"
    "fixture/DescribeSchemas.yaml"

requestDescribeTableStatistics :: DescribeTableStatistics -> TestTree
requestDescribeTableStatistics =
  req
    "DescribeTableStatistics"
    "fixture/DescribeTableStatistics.yaml"

requestImportCertificate :: ImportCertificate -> TestTree
requestImportCertificate =
  req
    "ImportCertificate"
    "fixture/ImportCertificate.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestModifyEndpoint :: ModifyEndpoint -> TestTree
requestModifyEndpoint =
  req
    "ModifyEndpoint"
    "fixture/ModifyEndpoint.yaml"

requestModifyEventSubscription :: ModifyEventSubscription -> TestTree
requestModifyEventSubscription =
  req
    "ModifyEventSubscription"
    "fixture/ModifyEventSubscription.yaml"

requestModifyReplicationInstance :: ModifyReplicationInstance -> TestTree
requestModifyReplicationInstance =
  req
    "ModifyReplicationInstance"
    "fixture/ModifyReplicationInstance.yaml"

requestModifyReplicationSubnetGroup :: ModifyReplicationSubnetGroup -> TestTree
requestModifyReplicationSubnetGroup =
  req
    "ModifyReplicationSubnetGroup"
    "fixture/ModifyReplicationSubnetGroup.yaml"

requestModifyReplicationTask :: ModifyReplicationTask -> TestTree
requestModifyReplicationTask =
  req
    "ModifyReplicationTask"
    "fixture/ModifyReplicationTask.yaml"

requestMoveReplicationTask :: MoveReplicationTask -> TestTree
requestMoveReplicationTask =
  req
    "MoveReplicationTask"
    "fixture/MoveReplicationTask.yaml"

requestRebootReplicationInstance :: RebootReplicationInstance -> TestTree
requestRebootReplicationInstance =
  req
    "RebootReplicationInstance"
    "fixture/RebootReplicationInstance.yaml"

requestRefreshSchemas :: RefreshSchemas -> TestTree
requestRefreshSchemas =
  req
    "RefreshSchemas"
    "fixture/RefreshSchemas.yaml"

requestReloadTables :: ReloadTables -> TestTree
requestReloadTables =
  req
    "ReloadTables"
    "fixture/ReloadTables.yaml"

requestRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
requestRemoveTagsFromResource =
  req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource.yaml"

requestRunFleetAdvisorLsaAnalysis :: RunFleetAdvisorLsaAnalysis -> TestTree
requestRunFleetAdvisorLsaAnalysis =
  req
    "RunFleetAdvisorLsaAnalysis"
    "fixture/RunFleetAdvisorLsaAnalysis.yaml"

requestStartReplicationTask :: StartReplicationTask -> TestTree
requestStartReplicationTask =
  req
    "StartReplicationTask"
    "fixture/StartReplicationTask.yaml"

requestStartReplicationTaskAssessment :: StartReplicationTaskAssessment -> TestTree
requestStartReplicationTaskAssessment =
  req
    "StartReplicationTaskAssessment"
    "fixture/StartReplicationTaskAssessment.yaml"

requestStartReplicationTaskAssessmentRun :: StartReplicationTaskAssessmentRun -> TestTree
requestStartReplicationTaskAssessmentRun =
  req
    "StartReplicationTaskAssessmentRun"
    "fixture/StartReplicationTaskAssessmentRun.yaml"

requestStopReplicationTask :: StopReplicationTask -> TestTree
requestStopReplicationTask =
  req
    "StopReplicationTask"
    "fixture/StopReplicationTask.yaml"

requestTestConnection :: TestConnection -> TestTree
requestTestConnection =
  req
    "TestConnection"
    "fixture/TestConnection.yaml"

requestUpdateSubscriptionsToEventBridge :: UpdateSubscriptionsToEventBridge -> TestTree
requestUpdateSubscriptionsToEventBridge =
  req
    "UpdateSubscriptionsToEventBridge"
    "fixture/UpdateSubscriptionsToEventBridge.yaml"

-- Responses

responseAddTagsToResource :: AddTagsToResourceResponse -> TestTree
responseAddTagsToResource =
  res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddTagsToResource)

responseApplyPendingMaintenanceAction :: ApplyPendingMaintenanceActionResponse -> TestTree
responseApplyPendingMaintenanceAction =
  res
    "ApplyPendingMaintenanceActionResponse"
    "fixture/ApplyPendingMaintenanceActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ApplyPendingMaintenanceAction)

responseCancelReplicationTaskAssessmentRun :: CancelReplicationTaskAssessmentRunResponse -> TestTree
responseCancelReplicationTaskAssessmentRun =
  res
    "CancelReplicationTaskAssessmentRunResponse"
    "fixture/CancelReplicationTaskAssessmentRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelReplicationTaskAssessmentRun)

responseCreateEndpoint :: CreateEndpointResponse -> TestTree
responseCreateEndpoint =
  res
    "CreateEndpointResponse"
    "fixture/CreateEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEndpoint)

responseCreateEventSubscription :: CreateEventSubscriptionResponse -> TestTree
responseCreateEventSubscription =
  res
    "CreateEventSubscriptionResponse"
    "fixture/CreateEventSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEventSubscription)

responseCreateFleetAdvisorCollector :: CreateFleetAdvisorCollectorResponse -> TestTree
responseCreateFleetAdvisorCollector =
  res
    "CreateFleetAdvisorCollectorResponse"
    "fixture/CreateFleetAdvisorCollectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFleetAdvisorCollector)

responseCreateReplicationInstance :: CreateReplicationInstanceResponse -> TestTree
responseCreateReplicationInstance =
  res
    "CreateReplicationInstanceResponse"
    "fixture/CreateReplicationInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateReplicationInstance)

responseCreateReplicationSubnetGroup :: CreateReplicationSubnetGroupResponse -> TestTree
responseCreateReplicationSubnetGroup =
  res
    "CreateReplicationSubnetGroupResponse"
    "fixture/CreateReplicationSubnetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateReplicationSubnetGroup)

responseCreateReplicationTask :: CreateReplicationTaskResponse -> TestTree
responseCreateReplicationTask =
  res
    "CreateReplicationTaskResponse"
    "fixture/CreateReplicationTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateReplicationTask)

responseDeleteCertificate :: DeleteCertificateResponse -> TestTree
responseDeleteCertificate =
  res
    "DeleteCertificateResponse"
    "fixture/DeleteCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCertificate)

responseDeleteConnection :: DeleteConnectionResponse -> TestTree
responseDeleteConnection =
  res
    "DeleteConnectionResponse"
    "fixture/DeleteConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConnection)

responseDeleteEndpoint :: DeleteEndpointResponse -> TestTree
responseDeleteEndpoint =
  res
    "DeleteEndpointResponse"
    "fixture/DeleteEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEndpoint)

responseDeleteEventSubscription :: DeleteEventSubscriptionResponse -> TestTree
responseDeleteEventSubscription =
  res
    "DeleteEventSubscriptionResponse"
    "fixture/DeleteEventSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEventSubscription)

responseDeleteFleetAdvisorCollector :: DeleteFleetAdvisorCollectorResponse -> TestTree
responseDeleteFleetAdvisorCollector =
  res
    "DeleteFleetAdvisorCollectorResponse"
    "fixture/DeleteFleetAdvisorCollectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFleetAdvisorCollector)

responseDeleteFleetAdvisorDatabases :: DeleteFleetAdvisorDatabasesResponse -> TestTree
responseDeleteFleetAdvisorDatabases =
  res
    "DeleteFleetAdvisorDatabasesResponse"
    "fixture/DeleteFleetAdvisorDatabasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFleetAdvisorDatabases)

responseDeleteReplicationInstance :: DeleteReplicationInstanceResponse -> TestTree
responseDeleteReplicationInstance =
  res
    "DeleteReplicationInstanceResponse"
    "fixture/DeleteReplicationInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteReplicationInstance)

responseDeleteReplicationSubnetGroup :: DeleteReplicationSubnetGroupResponse -> TestTree
responseDeleteReplicationSubnetGroup =
  res
    "DeleteReplicationSubnetGroupResponse"
    "fixture/DeleteReplicationSubnetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteReplicationSubnetGroup)

responseDeleteReplicationTask :: DeleteReplicationTaskResponse -> TestTree
responseDeleteReplicationTask =
  res
    "DeleteReplicationTaskResponse"
    "fixture/DeleteReplicationTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteReplicationTask)

responseDeleteReplicationTaskAssessmentRun :: DeleteReplicationTaskAssessmentRunResponse -> TestTree
responseDeleteReplicationTaskAssessmentRun =
  res
    "DeleteReplicationTaskAssessmentRunResponse"
    "fixture/DeleteReplicationTaskAssessmentRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteReplicationTaskAssessmentRun)

responseDescribeAccountAttributes :: DescribeAccountAttributesResponse -> TestTree
responseDescribeAccountAttributes =
  res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccountAttributes)

responseDescribeApplicableIndividualAssessments :: DescribeApplicableIndividualAssessmentsResponse -> TestTree
responseDescribeApplicableIndividualAssessments =
  res
    "DescribeApplicableIndividualAssessmentsResponse"
    "fixture/DescribeApplicableIndividualAssessmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeApplicableIndividualAssessments)

responseDescribeCertificates :: DescribeCertificatesResponse -> TestTree
responseDescribeCertificates =
  res
    "DescribeCertificatesResponse"
    "fixture/DescribeCertificatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCertificates)

responseDescribeConnections :: DescribeConnectionsResponse -> TestTree
responseDescribeConnections =
  res
    "DescribeConnectionsResponse"
    "fixture/DescribeConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConnections)

responseDescribeEndpointSettings :: DescribeEndpointSettingsResponse -> TestTree
responseDescribeEndpointSettings =
  res
    "DescribeEndpointSettingsResponse"
    "fixture/DescribeEndpointSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEndpointSettings)

responseDescribeEndpointTypes :: DescribeEndpointTypesResponse -> TestTree
responseDescribeEndpointTypes =
  res
    "DescribeEndpointTypesResponse"
    "fixture/DescribeEndpointTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEndpointTypes)

responseDescribeEndpoints :: DescribeEndpointsResponse -> TestTree
responseDescribeEndpoints =
  res
    "DescribeEndpointsResponse"
    "fixture/DescribeEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEndpoints)

responseDescribeEventCategories :: DescribeEventCategoriesResponse -> TestTree
responseDescribeEventCategories =
  res
    "DescribeEventCategoriesResponse"
    "fixture/DescribeEventCategoriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEventCategories)

responseDescribeEventSubscriptions :: DescribeEventSubscriptionsResponse -> TestTree
responseDescribeEventSubscriptions =
  res
    "DescribeEventSubscriptionsResponse"
    "fixture/DescribeEventSubscriptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEventSubscriptions)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents =
  res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEvents)

responseDescribeFleetAdvisorCollectors :: DescribeFleetAdvisorCollectorsResponse -> TestTree
responseDescribeFleetAdvisorCollectors =
  res
    "DescribeFleetAdvisorCollectorsResponse"
    "fixture/DescribeFleetAdvisorCollectorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFleetAdvisorCollectors)

responseDescribeFleetAdvisorDatabases :: DescribeFleetAdvisorDatabasesResponse -> TestTree
responseDescribeFleetAdvisorDatabases =
  res
    "DescribeFleetAdvisorDatabasesResponse"
    "fixture/DescribeFleetAdvisorDatabasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFleetAdvisorDatabases)

responseDescribeFleetAdvisorLsaAnalysis :: DescribeFleetAdvisorLsaAnalysisResponse -> TestTree
responseDescribeFleetAdvisorLsaAnalysis =
  res
    "DescribeFleetAdvisorLsaAnalysisResponse"
    "fixture/DescribeFleetAdvisorLsaAnalysisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFleetAdvisorLsaAnalysis)

responseDescribeFleetAdvisorSchemaObjectSummary :: DescribeFleetAdvisorSchemaObjectSummaryResponse -> TestTree
responseDescribeFleetAdvisorSchemaObjectSummary =
  res
    "DescribeFleetAdvisorSchemaObjectSummaryResponse"
    "fixture/DescribeFleetAdvisorSchemaObjectSummaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFleetAdvisorSchemaObjectSummary)

responseDescribeFleetAdvisorSchemas :: DescribeFleetAdvisorSchemasResponse -> TestTree
responseDescribeFleetAdvisorSchemas =
  res
    "DescribeFleetAdvisorSchemasResponse"
    "fixture/DescribeFleetAdvisorSchemasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFleetAdvisorSchemas)

responseDescribeOrderableReplicationInstances :: DescribeOrderableReplicationInstancesResponse -> TestTree
responseDescribeOrderableReplicationInstances =
  res
    "DescribeOrderableReplicationInstancesResponse"
    "fixture/DescribeOrderableReplicationInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOrderableReplicationInstances)

responseDescribePendingMaintenanceActions :: DescribePendingMaintenanceActionsResponse -> TestTree
responseDescribePendingMaintenanceActions =
  res
    "DescribePendingMaintenanceActionsResponse"
    "fixture/DescribePendingMaintenanceActionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePendingMaintenanceActions)

responseDescribeRefreshSchemasStatus :: DescribeRefreshSchemasStatusResponse -> TestTree
responseDescribeRefreshSchemasStatus =
  res
    "DescribeRefreshSchemasStatusResponse"
    "fixture/DescribeRefreshSchemasStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRefreshSchemasStatus)

responseDescribeReplicationInstanceTaskLogs :: DescribeReplicationInstanceTaskLogsResponse -> TestTree
responseDescribeReplicationInstanceTaskLogs =
  res
    "DescribeReplicationInstanceTaskLogsResponse"
    "fixture/DescribeReplicationInstanceTaskLogsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReplicationInstanceTaskLogs)

responseDescribeReplicationInstances :: DescribeReplicationInstancesResponse -> TestTree
responseDescribeReplicationInstances =
  res
    "DescribeReplicationInstancesResponse"
    "fixture/DescribeReplicationInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReplicationInstances)

responseDescribeReplicationSubnetGroups :: DescribeReplicationSubnetGroupsResponse -> TestTree
responseDescribeReplicationSubnetGroups =
  res
    "DescribeReplicationSubnetGroupsResponse"
    "fixture/DescribeReplicationSubnetGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReplicationSubnetGroups)

responseDescribeReplicationTaskAssessmentResults :: DescribeReplicationTaskAssessmentResultsResponse -> TestTree
responseDescribeReplicationTaskAssessmentResults =
  res
    "DescribeReplicationTaskAssessmentResultsResponse"
    "fixture/DescribeReplicationTaskAssessmentResultsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReplicationTaskAssessmentResults)

responseDescribeReplicationTaskAssessmentRuns :: DescribeReplicationTaskAssessmentRunsResponse -> TestTree
responseDescribeReplicationTaskAssessmentRuns =
  res
    "DescribeReplicationTaskAssessmentRunsResponse"
    "fixture/DescribeReplicationTaskAssessmentRunsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReplicationTaskAssessmentRuns)

responseDescribeReplicationTaskIndividualAssessments :: DescribeReplicationTaskIndividualAssessmentsResponse -> TestTree
responseDescribeReplicationTaskIndividualAssessments =
  res
    "DescribeReplicationTaskIndividualAssessmentsResponse"
    "fixture/DescribeReplicationTaskIndividualAssessmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReplicationTaskIndividualAssessments)

responseDescribeReplicationTasks :: DescribeReplicationTasksResponse -> TestTree
responseDescribeReplicationTasks =
  res
    "DescribeReplicationTasksResponse"
    "fixture/DescribeReplicationTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReplicationTasks)

responseDescribeSchemas :: DescribeSchemasResponse -> TestTree
responseDescribeSchemas =
  res
    "DescribeSchemasResponse"
    "fixture/DescribeSchemasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSchemas)

responseDescribeTableStatistics :: DescribeTableStatisticsResponse -> TestTree
responseDescribeTableStatistics =
  res
    "DescribeTableStatisticsResponse"
    "fixture/DescribeTableStatisticsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTableStatistics)

responseImportCertificate :: ImportCertificateResponse -> TestTree
responseImportCertificate =
  res
    "ImportCertificateResponse"
    "fixture/ImportCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportCertificate)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseModifyEndpoint :: ModifyEndpointResponse -> TestTree
responseModifyEndpoint =
  res
    "ModifyEndpointResponse"
    "fixture/ModifyEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyEndpoint)

responseModifyEventSubscription :: ModifyEventSubscriptionResponse -> TestTree
responseModifyEventSubscription =
  res
    "ModifyEventSubscriptionResponse"
    "fixture/ModifyEventSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyEventSubscription)

responseModifyReplicationInstance :: ModifyReplicationInstanceResponse -> TestTree
responseModifyReplicationInstance =
  res
    "ModifyReplicationInstanceResponse"
    "fixture/ModifyReplicationInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyReplicationInstance)

responseModifyReplicationSubnetGroup :: ModifyReplicationSubnetGroupResponse -> TestTree
responseModifyReplicationSubnetGroup =
  res
    "ModifyReplicationSubnetGroupResponse"
    "fixture/ModifyReplicationSubnetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyReplicationSubnetGroup)

responseModifyReplicationTask :: ModifyReplicationTaskResponse -> TestTree
responseModifyReplicationTask =
  res
    "ModifyReplicationTaskResponse"
    "fixture/ModifyReplicationTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyReplicationTask)

responseMoveReplicationTask :: MoveReplicationTaskResponse -> TestTree
responseMoveReplicationTask =
  res
    "MoveReplicationTaskResponse"
    "fixture/MoveReplicationTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy MoveReplicationTask)

responseRebootReplicationInstance :: RebootReplicationInstanceResponse -> TestTree
responseRebootReplicationInstance =
  res
    "RebootReplicationInstanceResponse"
    "fixture/RebootReplicationInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RebootReplicationInstance)

responseRefreshSchemas :: RefreshSchemasResponse -> TestTree
responseRefreshSchemas =
  res
    "RefreshSchemasResponse"
    "fixture/RefreshSchemasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RefreshSchemas)

responseReloadTables :: ReloadTablesResponse -> TestTree
responseReloadTables =
  res
    "ReloadTablesResponse"
    "fixture/ReloadTablesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReloadTables)

responseRemoveTagsFromResource :: RemoveTagsFromResourceResponse -> TestTree
responseRemoveTagsFromResource =
  res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveTagsFromResource)

responseRunFleetAdvisorLsaAnalysis :: RunFleetAdvisorLsaAnalysisResponse -> TestTree
responseRunFleetAdvisorLsaAnalysis =
  res
    "RunFleetAdvisorLsaAnalysisResponse"
    "fixture/RunFleetAdvisorLsaAnalysisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RunFleetAdvisorLsaAnalysis)

responseStartReplicationTask :: StartReplicationTaskResponse -> TestTree
responseStartReplicationTask =
  res
    "StartReplicationTaskResponse"
    "fixture/StartReplicationTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartReplicationTask)

responseStartReplicationTaskAssessment :: StartReplicationTaskAssessmentResponse -> TestTree
responseStartReplicationTaskAssessment =
  res
    "StartReplicationTaskAssessmentResponse"
    "fixture/StartReplicationTaskAssessmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartReplicationTaskAssessment)

responseStartReplicationTaskAssessmentRun :: StartReplicationTaskAssessmentRunResponse -> TestTree
responseStartReplicationTaskAssessmentRun =
  res
    "StartReplicationTaskAssessmentRunResponse"
    "fixture/StartReplicationTaskAssessmentRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartReplicationTaskAssessmentRun)

responseStopReplicationTask :: StopReplicationTaskResponse -> TestTree
responseStopReplicationTask =
  res
    "StopReplicationTaskResponse"
    "fixture/StopReplicationTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopReplicationTask)

responseTestConnection :: TestConnectionResponse -> TestTree
responseTestConnection =
  res
    "TestConnectionResponse"
    "fixture/TestConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TestConnection)

responseUpdateSubscriptionsToEventBridge :: UpdateSubscriptionsToEventBridgeResponse -> TestTree
responseUpdateSubscriptionsToEventBridge =
  res
    "UpdateSubscriptionsToEventBridgeResponse"
    "fixture/UpdateSubscriptionsToEventBridgeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSubscriptionsToEventBridge)

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.MGN
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.MGN where

import Amazonka.MGN
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.MGN.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestArchiveApplication $
--             newArchiveApplication
--
--         , requestArchiveWave $
--             newArchiveWave
--
--         , requestAssociateApplications $
--             newAssociateApplications
--
--         , requestAssociateSourceServers $
--             newAssociateSourceServers
--
--         , requestChangeServerLifeCycleState $
--             newChangeServerLifeCycleState
--
--         , requestCreateApplication $
--             newCreateApplication
--
--         , requestCreateLaunchConfigurationTemplate $
--             newCreateLaunchConfigurationTemplate
--
--         , requestCreateReplicationConfigurationTemplate $
--             newCreateReplicationConfigurationTemplate
--
--         , requestCreateWave $
--             newCreateWave
--
--         , requestDeleteApplication $
--             newDeleteApplication
--
--         , requestDeleteJob $
--             newDeleteJob
--
--         , requestDeleteLaunchConfigurationTemplate $
--             newDeleteLaunchConfigurationTemplate
--
--         , requestDeleteReplicationConfigurationTemplate $
--             newDeleteReplicationConfigurationTemplate
--
--         , requestDeleteSourceServer $
--             newDeleteSourceServer
--
--         , requestDeleteVcenterClient $
--             newDeleteVcenterClient
--
--         , requestDeleteWave $
--             newDeleteWave
--
--         , requestDescribeJobLogItems $
--             newDescribeJobLogItems
--
--         , requestDescribeJobs $
--             newDescribeJobs
--
--         , requestDescribeLaunchConfigurationTemplates $
--             newDescribeLaunchConfigurationTemplates
--
--         , requestDescribeReplicationConfigurationTemplates $
--             newDescribeReplicationConfigurationTemplates
--
--         , requestDescribeSourceServers $
--             newDescribeSourceServers
--
--         , requestDescribeVcenterClients $
--             newDescribeVcenterClients
--
--         , requestDisassociateApplications $
--             newDisassociateApplications
--
--         , requestDisassociateSourceServers $
--             newDisassociateSourceServers
--
--         , requestDisconnectFromService $
--             newDisconnectFromService
--
--         , requestFinalizeCutover $
--             newFinalizeCutover
--
--         , requestGetLaunchConfiguration $
--             newGetLaunchConfiguration
--
--         , requestGetReplicationConfiguration $
--             newGetReplicationConfiguration
--
--         , requestInitializeService $
--             newInitializeService
--
--         , requestListApplications $
--             newListApplications
--
--         , requestListSourceServerActions $
--             newListSourceServerActions
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListTemplateActions $
--             newListTemplateActions
--
--         , requestListWaves $
--             newListWaves
--
--         , requestMarkAsArchived $
--             newMarkAsArchived
--
--         , requestPutSourceServerAction $
--             newPutSourceServerAction
--
--         , requestPutTemplateAction $
--             newPutTemplateAction
--
--         , requestRemoveSourceServerAction $
--             newRemoveSourceServerAction
--
--         , requestRemoveTemplateAction $
--             newRemoveTemplateAction
--
--         , requestRetryDataReplication $
--             newRetryDataReplication
--
--         , requestStartCutover $
--             newStartCutover
--
--         , requestStartReplication $
--             newStartReplication
--
--         , requestStartTest $
--             newStartTest
--
--         , requestTagResource $
--             newTagResource
--
--         , requestTerminateTargetInstances $
--             newTerminateTargetInstances
--
--         , requestUnarchiveApplication $
--             newUnarchiveApplication
--
--         , requestUnarchiveWave $
--             newUnarchiveWave
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateApplication $
--             newUpdateApplication
--
--         , requestUpdateLaunchConfiguration $
--             newUpdateLaunchConfiguration
--
--         , requestUpdateLaunchConfigurationTemplate $
--             newUpdateLaunchConfigurationTemplate
--
--         , requestUpdateReplicationConfiguration $
--             newUpdateReplicationConfiguration
--
--         , requestUpdateReplicationConfigurationTemplate $
--             newUpdateReplicationConfigurationTemplate
--
--         , requestUpdateSourceServerReplicationType $
--             newUpdateSourceServerReplicationType
--
--         , requestUpdateWave $
--             newUpdateWave
--
--           ]

--     , testGroup "response"
--         [ responseArchiveApplication $
--             newApplication
--
--         , responseArchiveWave $
--             newWave
--
--         , responseAssociateApplications $
--             newAssociateApplicationsResponse
--
--         , responseAssociateSourceServers $
--             newAssociateSourceServersResponse
--
--         , responseChangeServerLifeCycleState $
--             newSourceServer
--
--         , responseCreateApplication $
--             newApplication
--
--         , responseCreateLaunchConfigurationTemplate $
--             newLaunchConfigurationTemplate
--
--         , responseCreateReplicationConfigurationTemplate $
--             newReplicationConfigurationTemplate
--
--         , responseCreateWave $
--             newWave
--
--         , responseDeleteApplication $
--             newDeleteApplicationResponse
--
--         , responseDeleteJob $
--             newDeleteJobResponse
--
--         , responseDeleteLaunchConfigurationTemplate $
--             newDeleteLaunchConfigurationTemplateResponse
--
--         , responseDeleteReplicationConfigurationTemplate $
--             newDeleteReplicationConfigurationTemplateResponse
--
--         , responseDeleteSourceServer $
--             newDeleteSourceServerResponse
--
--         , responseDeleteVcenterClient $
--             newDeleteVcenterClientResponse
--
--         , responseDeleteWave $
--             newDeleteWaveResponse
--
--         , responseDescribeJobLogItems $
--             newDescribeJobLogItemsResponse
--
--         , responseDescribeJobs $
--             newDescribeJobsResponse
--
--         , responseDescribeLaunchConfigurationTemplates $
--             newDescribeLaunchConfigurationTemplatesResponse
--
--         , responseDescribeReplicationConfigurationTemplates $
--             newDescribeReplicationConfigurationTemplatesResponse
--
--         , responseDescribeSourceServers $
--             newDescribeSourceServersResponse
--
--         , responseDescribeVcenterClients $
--             newDescribeVcenterClientsResponse
--
--         , responseDisassociateApplications $
--             newDisassociateApplicationsResponse
--
--         , responseDisassociateSourceServers $
--             newDisassociateSourceServersResponse
--
--         , responseDisconnectFromService $
--             newSourceServer
--
--         , responseFinalizeCutover $
--             newSourceServer
--
--         , responseGetLaunchConfiguration $
--             newLaunchConfiguration
--
--         , responseGetReplicationConfiguration $
--             newReplicationConfiguration
--
--         , responseInitializeService $
--             newInitializeServiceResponse
--
--         , responseListApplications $
--             newListApplicationsResponse
--
--         , responseListSourceServerActions $
--             newListSourceServerActionsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListTemplateActions $
--             newListTemplateActionsResponse
--
--         , responseListWaves $
--             newListWavesResponse
--
--         , responseMarkAsArchived $
--             newSourceServer
--
--         , responsePutSourceServerAction $
--             newSourceServerActionDocument
--
--         , responsePutTemplateAction $
--             newTemplateActionDocument
--
--         , responseRemoveSourceServerAction $
--             newRemoveSourceServerActionResponse
--
--         , responseRemoveTemplateAction $
--             newRemoveTemplateActionResponse
--
--         , responseRetryDataReplication $
--             newSourceServer
--
--         , responseStartCutover $
--             newStartCutoverResponse
--
--         , responseStartReplication $
--             newSourceServer
--
--         , responseStartTest $
--             newStartTestResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseTerminateTargetInstances $
--             newTerminateTargetInstancesResponse
--
--         , responseUnarchiveApplication $
--             newApplication
--
--         , responseUnarchiveWave $
--             newWave
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateApplication $
--             newApplication
--
--         , responseUpdateLaunchConfiguration $
--             newLaunchConfiguration
--
--         , responseUpdateLaunchConfigurationTemplate $
--             newLaunchConfigurationTemplate
--
--         , responseUpdateReplicationConfiguration $
--             newReplicationConfiguration
--
--         , responseUpdateReplicationConfigurationTemplate $
--             newReplicationConfigurationTemplate
--
--         , responseUpdateSourceServerReplicationType $
--             newSourceServer
--
--         , responseUpdateWave $
--             newWave
--
--           ]
--     ]

-- Requests

requestArchiveApplication :: ArchiveApplication -> TestTree
requestArchiveApplication =
  req
    "ArchiveApplication"
    "fixture/ArchiveApplication.yaml"

requestArchiveWave :: ArchiveWave -> TestTree
requestArchiveWave =
  req
    "ArchiveWave"
    "fixture/ArchiveWave.yaml"

requestAssociateApplications :: AssociateApplications -> TestTree
requestAssociateApplications =
  req
    "AssociateApplications"
    "fixture/AssociateApplications.yaml"

requestAssociateSourceServers :: AssociateSourceServers -> TestTree
requestAssociateSourceServers =
  req
    "AssociateSourceServers"
    "fixture/AssociateSourceServers.yaml"

requestChangeServerLifeCycleState :: ChangeServerLifeCycleState -> TestTree
requestChangeServerLifeCycleState =
  req
    "ChangeServerLifeCycleState"
    "fixture/ChangeServerLifeCycleState.yaml"

requestCreateApplication :: CreateApplication -> TestTree
requestCreateApplication =
  req
    "CreateApplication"
    "fixture/CreateApplication.yaml"

requestCreateLaunchConfigurationTemplate :: CreateLaunchConfigurationTemplate -> TestTree
requestCreateLaunchConfigurationTemplate =
  req
    "CreateLaunchConfigurationTemplate"
    "fixture/CreateLaunchConfigurationTemplate.yaml"

requestCreateReplicationConfigurationTemplate :: CreateReplicationConfigurationTemplate -> TestTree
requestCreateReplicationConfigurationTemplate =
  req
    "CreateReplicationConfigurationTemplate"
    "fixture/CreateReplicationConfigurationTemplate.yaml"

requestCreateWave :: CreateWave -> TestTree
requestCreateWave =
  req
    "CreateWave"
    "fixture/CreateWave.yaml"

requestDeleteApplication :: DeleteApplication -> TestTree
requestDeleteApplication =
  req
    "DeleteApplication"
    "fixture/DeleteApplication.yaml"

requestDeleteJob :: DeleteJob -> TestTree
requestDeleteJob =
  req
    "DeleteJob"
    "fixture/DeleteJob.yaml"

requestDeleteLaunchConfigurationTemplate :: DeleteLaunchConfigurationTemplate -> TestTree
requestDeleteLaunchConfigurationTemplate =
  req
    "DeleteLaunchConfigurationTemplate"
    "fixture/DeleteLaunchConfigurationTemplate.yaml"

requestDeleteReplicationConfigurationTemplate :: DeleteReplicationConfigurationTemplate -> TestTree
requestDeleteReplicationConfigurationTemplate =
  req
    "DeleteReplicationConfigurationTemplate"
    "fixture/DeleteReplicationConfigurationTemplate.yaml"

requestDeleteSourceServer :: DeleteSourceServer -> TestTree
requestDeleteSourceServer =
  req
    "DeleteSourceServer"
    "fixture/DeleteSourceServer.yaml"

requestDeleteVcenterClient :: DeleteVcenterClient -> TestTree
requestDeleteVcenterClient =
  req
    "DeleteVcenterClient"
    "fixture/DeleteVcenterClient.yaml"

requestDeleteWave :: DeleteWave -> TestTree
requestDeleteWave =
  req
    "DeleteWave"
    "fixture/DeleteWave.yaml"

requestDescribeJobLogItems :: DescribeJobLogItems -> TestTree
requestDescribeJobLogItems =
  req
    "DescribeJobLogItems"
    "fixture/DescribeJobLogItems.yaml"

requestDescribeJobs :: DescribeJobs -> TestTree
requestDescribeJobs =
  req
    "DescribeJobs"
    "fixture/DescribeJobs.yaml"

requestDescribeLaunchConfigurationTemplates :: DescribeLaunchConfigurationTemplates -> TestTree
requestDescribeLaunchConfigurationTemplates =
  req
    "DescribeLaunchConfigurationTemplates"
    "fixture/DescribeLaunchConfigurationTemplates.yaml"

requestDescribeReplicationConfigurationTemplates :: DescribeReplicationConfigurationTemplates -> TestTree
requestDescribeReplicationConfigurationTemplates =
  req
    "DescribeReplicationConfigurationTemplates"
    "fixture/DescribeReplicationConfigurationTemplates.yaml"

requestDescribeSourceServers :: DescribeSourceServers -> TestTree
requestDescribeSourceServers =
  req
    "DescribeSourceServers"
    "fixture/DescribeSourceServers.yaml"

requestDescribeVcenterClients :: DescribeVcenterClients -> TestTree
requestDescribeVcenterClients =
  req
    "DescribeVcenterClients"
    "fixture/DescribeVcenterClients.yaml"

requestDisassociateApplications :: DisassociateApplications -> TestTree
requestDisassociateApplications =
  req
    "DisassociateApplications"
    "fixture/DisassociateApplications.yaml"

requestDisassociateSourceServers :: DisassociateSourceServers -> TestTree
requestDisassociateSourceServers =
  req
    "DisassociateSourceServers"
    "fixture/DisassociateSourceServers.yaml"

requestDisconnectFromService :: DisconnectFromService -> TestTree
requestDisconnectFromService =
  req
    "DisconnectFromService"
    "fixture/DisconnectFromService.yaml"

requestFinalizeCutover :: FinalizeCutover -> TestTree
requestFinalizeCutover =
  req
    "FinalizeCutover"
    "fixture/FinalizeCutover.yaml"

requestGetLaunchConfiguration :: GetLaunchConfiguration -> TestTree
requestGetLaunchConfiguration =
  req
    "GetLaunchConfiguration"
    "fixture/GetLaunchConfiguration.yaml"

requestGetReplicationConfiguration :: GetReplicationConfiguration -> TestTree
requestGetReplicationConfiguration =
  req
    "GetReplicationConfiguration"
    "fixture/GetReplicationConfiguration.yaml"

requestInitializeService :: InitializeService -> TestTree
requestInitializeService =
  req
    "InitializeService"
    "fixture/InitializeService.yaml"

requestListApplications :: ListApplications -> TestTree
requestListApplications =
  req
    "ListApplications"
    "fixture/ListApplications.yaml"

requestListSourceServerActions :: ListSourceServerActions -> TestTree
requestListSourceServerActions =
  req
    "ListSourceServerActions"
    "fixture/ListSourceServerActions.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListTemplateActions :: ListTemplateActions -> TestTree
requestListTemplateActions =
  req
    "ListTemplateActions"
    "fixture/ListTemplateActions.yaml"

requestListWaves :: ListWaves -> TestTree
requestListWaves =
  req
    "ListWaves"
    "fixture/ListWaves.yaml"

requestMarkAsArchived :: MarkAsArchived -> TestTree
requestMarkAsArchived =
  req
    "MarkAsArchived"
    "fixture/MarkAsArchived.yaml"

requestPutSourceServerAction :: PutSourceServerAction -> TestTree
requestPutSourceServerAction =
  req
    "PutSourceServerAction"
    "fixture/PutSourceServerAction.yaml"

requestPutTemplateAction :: PutTemplateAction -> TestTree
requestPutTemplateAction =
  req
    "PutTemplateAction"
    "fixture/PutTemplateAction.yaml"

requestRemoveSourceServerAction :: RemoveSourceServerAction -> TestTree
requestRemoveSourceServerAction =
  req
    "RemoveSourceServerAction"
    "fixture/RemoveSourceServerAction.yaml"

requestRemoveTemplateAction :: RemoveTemplateAction -> TestTree
requestRemoveTemplateAction =
  req
    "RemoveTemplateAction"
    "fixture/RemoveTemplateAction.yaml"

requestRetryDataReplication :: RetryDataReplication -> TestTree
requestRetryDataReplication =
  req
    "RetryDataReplication"
    "fixture/RetryDataReplication.yaml"

requestStartCutover :: StartCutover -> TestTree
requestStartCutover =
  req
    "StartCutover"
    "fixture/StartCutover.yaml"

requestStartReplication :: StartReplication -> TestTree
requestStartReplication =
  req
    "StartReplication"
    "fixture/StartReplication.yaml"

requestStartTest :: StartTest -> TestTree
requestStartTest =
  req
    "StartTest"
    "fixture/StartTest.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestTerminateTargetInstances :: TerminateTargetInstances -> TestTree
requestTerminateTargetInstances =
  req
    "TerminateTargetInstances"
    "fixture/TerminateTargetInstances.yaml"

requestUnarchiveApplication :: UnarchiveApplication -> TestTree
requestUnarchiveApplication =
  req
    "UnarchiveApplication"
    "fixture/UnarchiveApplication.yaml"

requestUnarchiveWave :: UnarchiveWave -> TestTree
requestUnarchiveWave =
  req
    "UnarchiveWave"
    "fixture/UnarchiveWave.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateApplication :: UpdateApplication -> TestTree
requestUpdateApplication =
  req
    "UpdateApplication"
    "fixture/UpdateApplication.yaml"

requestUpdateLaunchConfiguration :: UpdateLaunchConfiguration -> TestTree
requestUpdateLaunchConfiguration =
  req
    "UpdateLaunchConfiguration"
    "fixture/UpdateLaunchConfiguration.yaml"

requestUpdateLaunchConfigurationTemplate :: UpdateLaunchConfigurationTemplate -> TestTree
requestUpdateLaunchConfigurationTemplate =
  req
    "UpdateLaunchConfigurationTemplate"
    "fixture/UpdateLaunchConfigurationTemplate.yaml"

requestUpdateReplicationConfiguration :: UpdateReplicationConfiguration -> TestTree
requestUpdateReplicationConfiguration =
  req
    "UpdateReplicationConfiguration"
    "fixture/UpdateReplicationConfiguration.yaml"

requestUpdateReplicationConfigurationTemplate :: UpdateReplicationConfigurationTemplate -> TestTree
requestUpdateReplicationConfigurationTemplate =
  req
    "UpdateReplicationConfigurationTemplate"
    "fixture/UpdateReplicationConfigurationTemplate.yaml"

requestUpdateSourceServerReplicationType :: UpdateSourceServerReplicationType -> TestTree
requestUpdateSourceServerReplicationType =
  req
    "UpdateSourceServerReplicationType"
    "fixture/UpdateSourceServerReplicationType.yaml"

requestUpdateWave :: UpdateWave -> TestTree
requestUpdateWave =
  req
    "UpdateWave"
    "fixture/UpdateWave.yaml"

-- Responses

responseArchiveApplication :: Application -> TestTree
responseArchiveApplication =
  res
    "ArchiveApplicationResponse"
    "fixture/ArchiveApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ArchiveApplication)

responseArchiveWave :: Wave -> TestTree
responseArchiveWave =
  res
    "ArchiveWaveResponse"
    "fixture/ArchiveWaveResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ArchiveWave)

responseAssociateApplications :: AssociateApplicationsResponse -> TestTree
responseAssociateApplications =
  res
    "AssociateApplicationsResponse"
    "fixture/AssociateApplicationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateApplications)

responseAssociateSourceServers :: AssociateSourceServersResponse -> TestTree
responseAssociateSourceServers =
  res
    "AssociateSourceServersResponse"
    "fixture/AssociateSourceServersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateSourceServers)

responseChangeServerLifeCycleState :: SourceServer -> TestTree
responseChangeServerLifeCycleState =
  res
    "ChangeServerLifeCycleStateResponse"
    "fixture/ChangeServerLifeCycleStateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ChangeServerLifeCycleState)

responseCreateApplication :: Application -> TestTree
responseCreateApplication =
  res
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApplication)

responseCreateLaunchConfigurationTemplate :: LaunchConfigurationTemplate -> TestTree
responseCreateLaunchConfigurationTemplate =
  res
    "CreateLaunchConfigurationTemplateResponse"
    "fixture/CreateLaunchConfigurationTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLaunchConfigurationTemplate)

responseCreateReplicationConfigurationTemplate :: ReplicationConfigurationTemplate -> TestTree
responseCreateReplicationConfigurationTemplate =
  res
    "CreateReplicationConfigurationTemplateResponse"
    "fixture/CreateReplicationConfigurationTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateReplicationConfigurationTemplate)

responseCreateWave :: Wave -> TestTree
responseCreateWave =
  res
    "CreateWaveResponse"
    "fixture/CreateWaveResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWave)

responseDeleteApplication :: DeleteApplicationResponse -> TestTree
responseDeleteApplication =
  res
    "DeleteApplicationResponse"
    "fixture/DeleteApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApplication)

responseDeleteJob :: DeleteJobResponse -> TestTree
responseDeleteJob =
  res
    "DeleteJobResponse"
    "fixture/DeleteJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteJob)

responseDeleteLaunchConfigurationTemplate :: DeleteLaunchConfigurationTemplateResponse -> TestTree
responseDeleteLaunchConfigurationTemplate =
  res
    "DeleteLaunchConfigurationTemplateResponse"
    "fixture/DeleteLaunchConfigurationTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLaunchConfigurationTemplate)

responseDeleteReplicationConfigurationTemplate :: DeleteReplicationConfigurationTemplateResponse -> TestTree
responseDeleteReplicationConfigurationTemplate =
  res
    "DeleteReplicationConfigurationTemplateResponse"
    "fixture/DeleteReplicationConfigurationTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteReplicationConfigurationTemplate)

responseDeleteSourceServer :: DeleteSourceServerResponse -> TestTree
responseDeleteSourceServer =
  res
    "DeleteSourceServerResponse"
    "fixture/DeleteSourceServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSourceServer)

responseDeleteVcenterClient :: DeleteVcenterClientResponse -> TestTree
responseDeleteVcenterClient =
  res
    "DeleteVcenterClientResponse"
    "fixture/DeleteVcenterClientResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVcenterClient)

responseDeleteWave :: DeleteWaveResponse -> TestTree
responseDeleteWave =
  res
    "DeleteWaveResponse"
    "fixture/DeleteWaveResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWave)

responseDescribeJobLogItems :: DescribeJobLogItemsResponse -> TestTree
responseDescribeJobLogItems =
  res
    "DescribeJobLogItemsResponse"
    "fixture/DescribeJobLogItemsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeJobLogItems)

responseDescribeJobs :: DescribeJobsResponse -> TestTree
responseDescribeJobs =
  res
    "DescribeJobsResponse"
    "fixture/DescribeJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeJobs)

responseDescribeLaunchConfigurationTemplates :: DescribeLaunchConfigurationTemplatesResponse -> TestTree
responseDescribeLaunchConfigurationTemplates =
  res
    "DescribeLaunchConfigurationTemplatesResponse"
    "fixture/DescribeLaunchConfigurationTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLaunchConfigurationTemplates)

responseDescribeReplicationConfigurationTemplates :: DescribeReplicationConfigurationTemplatesResponse -> TestTree
responseDescribeReplicationConfigurationTemplates =
  res
    "DescribeReplicationConfigurationTemplatesResponse"
    "fixture/DescribeReplicationConfigurationTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReplicationConfigurationTemplates)

responseDescribeSourceServers :: DescribeSourceServersResponse -> TestTree
responseDescribeSourceServers =
  res
    "DescribeSourceServersResponse"
    "fixture/DescribeSourceServersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSourceServers)

responseDescribeVcenterClients :: DescribeVcenterClientsResponse -> TestTree
responseDescribeVcenterClients =
  res
    "DescribeVcenterClientsResponse"
    "fixture/DescribeVcenterClientsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVcenterClients)

responseDisassociateApplications :: DisassociateApplicationsResponse -> TestTree
responseDisassociateApplications =
  res
    "DisassociateApplicationsResponse"
    "fixture/DisassociateApplicationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateApplications)

responseDisassociateSourceServers :: DisassociateSourceServersResponse -> TestTree
responseDisassociateSourceServers =
  res
    "DisassociateSourceServersResponse"
    "fixture/DisassociateSourceServersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateSourceServers)

responseDisconnectFromService :: SourceServer -> TestTree
responseDisconnectFromService =
  res
    "DisconnectFromServiceResponse"
    "fixture/DisconnectFromServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisconnectFromService)

responseFinalizeCutover :: SourceServer -> TestTree
responseFinalizeCutover =
  res
    "FinalizeCutoverResponse"
    "fixture/FinalizeCutoverResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy FinalizeCutover)

responseGetLaunchConfiguration :: LaunchConfiguration -> TestTree
responseGetLaunchConfiguration =
  res
    "GetLaunchConfigurationResponse"
    "fixture/GetLaunchConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLaunchConfiguration)

responseGetReplicationConfiguration :: ReplicationConfiguration -> TestTree
responseGetReplicationConfiguration =
  res
    "GetReplicationConfigurationResponse"
    "fixture/GetReplicationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetReplicationConfiguration)

responseInitializeService :: InitializeServiceResponse -> TestTree
responseInitializeService =
  res
    "InitializeServiceResponse"
    "fixture/InitializeServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy InitializeService)

responseListApplications :: ListApplicationsResponse -> TestTree
responseListApplications =
  res
    "ListApplicationsResponse"
    "fixture/ListApplicationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApplications)

responseListSourceServerActions :: ListSourceServerActionsResponse -> TestTree
responseListSourceServerActions =
  res
    "ListSourceServerActionsResponse"
    "fixture/ListSourceServerActionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSourceServerActions)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListTemplateActions :: ListTemplateActionsResponse -> TestTree
responseListTemplateActions =
  res
    "ListTemplateActionsResponse"
    "fixture/ListTemplateActionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTemplateActions)

responseListWaves :: ListWavesResponse -> TestTree
responseListWaves =
  res
    "ListWavesResponse"
    "fixture/ListWavesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWaves)

responseMarkAsArchived :: SourceServer -> TestTree
responseMarkAsArchived =
  res
    "MarkAsArchivedResponse"
    "fixture/MarkAsArchivedResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy MarkAsArchived)

responsePutSourceServerAction :: SourceServerActionDocument -> TestTree
responsePutSourceServerAction =
  res
    "PutSourceServerActionResponse"
    "fixture/PutSourceServerActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutSourceServerAction)

responsePutTemplateAction :: TemplateActionDocument -> TestTree
responsePutTemplateAction =
  res
    "PutTemplateActionResponse"
    "fixture/PutTemplateActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutTemplateAction)

responseRemoveSourceServerAction :: RemoveSourceServerActionResponse -> TestTree
responseRemoveSourceServerAction =
  res
    "RemoveSourceServerActionResponse"
    "fixture/RemoveSourceServerActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveSourceServerAction)

responseRemoveTemplateAction :: RemoveTemplateActionResponse -> TestTree
responseRemoveTemplateAction =
  res
    "RemoveTemplateActionResponse"
    "fixture/RemoveTemplateActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveTemplateAction)

responseRetryDataReplication :: SourceServer -> TestTree
responseRetryDataReplication =
  res
    "RetryDataReplicationResponse"
    "fixture/RetryDataReplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RetryDataReplication)

responseStartCutover :: StartCutoverResponse -> TestTree
responseStartCutover =
  res
    "StartCutoverResponse"
    "fixture/StartCutoverResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartCutover)

responseStartReplication :: SourceServer -> TestTree
responseStartReplication =
  res
    "StartReplicationResponse"
    "fixture/StartReplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartReplication)

responseStartTest :: StartTestResponse -> TestTree
responseStartTest =
  res
    "StartTestResponse"
    "fixture/StartTestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartTest)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseTerminateTargetInstances :: TerminateTargetInstancesResponse -> TestTree
responseTerminateTargetInstances =
  res
    "TerminateTargetInstancesResponse"
    "fixture/TerminateTargetInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TerminateTargetInstances)

responseUnarchiveApplication :: Application -> TestTree
responseUnarchiveApplication =
  res
    "UnarchiveApplicationResponse"
    "fixture/UnarchiveApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UnarchiveApplication)

responseUnarchiveWave :: Wave -> TestTree
responseUnarchiveWave =
  res
    "UnarchiveWaveResponse"
    "fixture/UnarchiveWaveResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UnarchiveWave)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateApplication :: Application -> TestTree
responseUpdateApplication =
  res
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApplication)

responseUpdateLaunchConfiguration :: LaunchConfiguration -> TestTree
responseUpdateLaunchConfiguration =
  res
    "UpdateLaunchConfigurationResponse"
    "fixture/UpdateLaunchConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLaunchConfiguration)

responseUpdateLaunchConfigurationTemplate :: LaunchConfigurationTemplate -> TestTree
responseUpdateLaunchConfigurationTemplate =
  res
    "UpdateLaunchConfigurationTemplateResponse"
    "fixture/UpdateLaunchConfigurationTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLaunchConfigurationTemplate)

responseUpdateReplicationConfiguration :: ReplicationConfiguration -> TestTree
responseUpdateReplicationConfiguration =
  res
    "UpdateReplicationConfigurationResponse"
    "fixture/UpdateReplicationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateReplicationConfiguration)

responseUpdateReplicationConfigurationTemplate :: ReplicationConfigurationTemplate -> TestTree
responseUpdateReplicationConfigurationTemplate =
  res
    "UpdateReplicationConfigurationTemplateResponse"
    "fixture/UpdateReplicationConfigurationTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateReplicationConfigurationTemplate)

responseUpdateSourceServerReplicationType :: SourceServer -> TestTree
responseUpdateSourceServerReplicationType =
  res
    "UpdateSourceServerReplicationTypeResponse"
    "fixture/UpdateSourceServerReplicationTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSourceServerReplicationType)

responseUpdateWave :: Wave -> TestTree
responseUpdateWave =
  res
    "UpdateWaveResponse"
    "fixture/UpdateWaveResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWave)

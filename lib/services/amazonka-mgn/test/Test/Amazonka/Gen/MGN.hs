{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.MGN
-- Copyright   : (c) 2013-2022 Brendan Hay
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
--         [ requestChangeServerLifeCycleState $
--             newChangeServerLifeCycleState
--
--         , requestCreateLaunchConfigurationTemplate $
--             newCreateLaunchConfigurationTemplate
--
--         , requestCreateReplicationConfigurationTemplate $
--             newCreateReplicationConfigurationTemplate
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
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestMarkAsArchived $
--             newMarkAsArchived
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
--         , requestUntagResource $
--             newUntagResource
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
--           ]

--     , testGroup "response"
--         [ responseChangeServerLifeCycleState $
--             newSourceServer
--
--         , responseCreateLaunchConfigurationTemplate $
--             newLaunchConfigurationTemplate
--
--         , responseCreateReplicationConfigurationTemplate $
--             newReplicationConfigurationTemplate
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
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseMarkAsArchived $
--             newSourceServer
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
--         , responseUntagResource $
--             newUntagResourceResponse
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
--           ]
--     ]

-- Requests

requestChangeServerLifeCycleState :: ChangeServerLifeCycleState -> TestTree
requestChangeServerLifeCycleState =
  req
    "ChangeServerLifeCycleState"
    "fixture/ChangeServerLifeCycleState.yaml"

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

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestMarkAsArchived :: MarkAsArchived -> TestTree
requestMarkAsArchived =
  req
    "MarkAsArchived"
    "fixture/MarkAsArchived.yaml"

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

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

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

-- Responses

responseChangeServerLifeCycleState :: SourceServer -> TestTree
responseChangeServerLifeCycleState =
  res
    "ChangeServerLifeCycleStateResponse"
    "fixture/ChangeServerLifeCycleStateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ChangeServerLifeCycleState)

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

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseMarkAsArchived :: SourceServer -> TestTree
responseMarkAsArchived =
  res
    "MarkAsArchivedResponse"
    "fixture/MarkAsArchivedResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy MarkAsArchived)

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

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

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

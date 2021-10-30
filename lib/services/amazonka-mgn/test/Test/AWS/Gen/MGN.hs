{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.MGN
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.MGN where

import qualified Data.Proxy as Proxy
import Network.AWS.MGN
import Test.AWS.Fixture
import Test.AWS.MGN.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestUpdateLaunchConfiguration $
--             newUpdateLaunchConfiguration
--
--         , requestDescribeReplicationConfigurationTemplates $
--             newDescribeReplicationConfigurationTemplates
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestInitializeService $
--             newInitializeService
--
--         , requestUpdateReplicationConfigurationTemplate $
--             newUpdateReplicationConfigurationTemplate
--
--         , requestDeleteReplicationConfigurationTemplate $
--             newDeleteReplicationConfigurationTemplate
--
--         , requestCreateReplicationConfigurationTemplate $
--             newCreateReplicationConfigurationTemplate
--
--         , requestDescribeJobLogItems $
--             newDescribeJobLogItems
--
--         , requestDisconnectFromService $
--             newDisconnectFromService
--
--         , requestStartTest $
--             newStartTest
--
--         , requestDescribeSourceServers $
--             newDescribeSourceServers
--
--         , requestDeleteJob $
--             newDeleteJob
--
--         , requestFinalizeCutover $
--             newFinalizeCutover
--
--         , requestDescribeJobs $
--             newDescribeJobs
--
--         , requestMarkAsArchived $
--             newMarkAsArchived
--
--         , requestStartCutover $
--             newStartCutover
--
--         , requestRetryDataReplication $
--             newRetryDataReplication
--
--         , requestGetReplicationConfiguration $
--             newGetReplicationConfiguration
--
--         , requestChangeServerLifeCycleState $
--             newChangeServerLifeCycleState
--
--         , requestTerminateTargetInstances $
--             newTerminateTargetInstances
--
--         , requestUpdateReplicationConfiguration $
--             newUpdateReplicationConfiguration
--
--         , requestTagResource $
--             newTagResource
--
--         , requestGetLaunchConfiguration $
--             newGetLaunchConfiguration
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDeleteSourceServer $
--             newDeleteSourceServer
--
--           ]

--     , testGroup "response"
--         [ responseUpdateLaunchConfiguration $
--             newLaunchConfiguration
--
--         , responseDescribeReplicationConfigurationTemplates $
--             newDescribeReplicationConfigurationTemplatesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseInitializeService $
--             newInitializeServiceResponse
--
--         , responseUpdateReplicationConfigurationTemplate $
--             newReplicationConfigurationTemplate
--
--         , responseDeleteReplicationConfigurationTemplate $
--             newDeleteReplicationConfigurationTemplateResponse
--
--         , responseCreateReplicationConfigurationTemplate $
--             newReplicationConfigurationTemplate
--
--         , responseDescribeJobLogItems $
--             newDescribeJobLogItemsResponse
--
--         , responseDisconnectFromService $
--             newSourceServer
--
--         , responseStartTest $
--             newStartTestResponse
--
--         , responseDescribeSourceServers $
--             newDescribeSourceServersResponse
--
--         , responseDeleteJob $
--             newDeleteJobResponse
--
--         , responseFinalizeCutover $
--             newSourceServer
--
--         , responseDescribeJobs $
--             newDescribeJobsResponse
--
--         , responseMarkAsArchived $
--             newSourceServer
--
--         , responseStartCutover $
--             newStartCutoverResponse
--
--         , responseRetryDataReplication $
--             newSourceServer
--
--         , responseGetReplicationConfiguration $
--             newReplicationConfiguration
--
--         , responseChangeServerLifeCycleState $
--             newSourceServer
--
--         , responseTerminateTargetInstances $
--             newTerminateTargetInstancesResponse
--
--         , responseUpdateReplicationConfiguration $
--             newReplicationConfiguration
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseGetLaunchConfiguration $
--             newLaunchConfiguration
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDeleteSourceServer $
--             newDeleteSourceServerResponse
--
--           ]
--     ]

-- Requests

requestUpdateLaunchConfiguration :: UpdateLaunchConfiguration -> TestTree
requestUpdateLaunchConfiguration =
  req
    "UpdateLaunchConfiguration"
    "fixture/UpdateLaunchConfiguration.yaml"

requestDescribeReplicationConfigurationTemplates :: DescribeReplicationConfigurationTemplates -> TestTree
requestDescribeReplicationConfigurationTemplates =
  req
    "DescribeReplicationConfigurationTemplates"
    "fixture/DescribeReplicationConfigurationTemplates.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestInitializeService :: InitializeService -> TestTree
requestInitializeService =
  req
    "InitializeService"
    "fixture/InitializeService.yaml"

requestUpdateReplicationConfigurationTemplate :: UpdateReplicationConfigurationTemplate -> TestTree
requestUpdateReplicationConfigurationTemplate =
  req
    "UpdateReplicationConfigurationTemplate"
    "fixture/UpdateReplicationConfigurationTemplate.yaml"

requestDeleteReplicationConfigurationTemplate :: DeleteReplicationConfigurationTemplate -> TestTree
requestDeleteReplicationConfigurationTemplate =
  req
    "DeleteReplicationConfigurationTemplate"
    "fixture/DeleteReplicationConfigurationTemplate.yaml"

requestCreateReplicationConfigurationTemplate :: CreateReplicationConfigurationTemplate -> TestTree
requestCreateReplicationConfigurationTemplate =
  req
    "CreateReplicationConfigurationTemplate"
    "fixture/CreateReplicationConfigurationTemplate.yaml"

requestDescribeJobLogItems :: DescribeJobLogItems -> TestTree
requestDescribeJobLogItems =
  req
    "DescribeJobLogItems"
    "fixture/DescribeJobLogItems.yaml"

requestDisconnectFromService :: DisconnectFromService -> TestTree
requestDisconnectFromService =
  req
    "DisconnectFromService"
    "fixture/DisconnectFromService.yaml"

requestStartTest :: StartTest -> TestTree
requestStartTest =
  req
    "StartTest"
    "fixture/StartTest.yaml"

requestDescribeSourceServers :: DescribeSourceServers -> TestTree
requestDescribeSourceServers =
  req
    "DescribeSourceServers"
    "fixture/DescribeSourceServers.yaml"

requestDeleteJob :: DeleteJob -> TestTree
requestDeleteJob =
  req
    "DeleteJob"
    "fixture/DeleteJob.yaml"

requestFinalizeCutover :: FinalizeCutover -> TestTree
requestFinalizeCutover =
  req
    "FinalizeCutover"
    "fixture/FinalizeCutover.yaml"

requestDescribeJobs :: DescribeJobs -> TestTree
requestDescribeJobs =
  req
    "DescribeJobs"
    "fixture/DescribeJobs.yaml"

requestMarkAsArchived :: MarkAsArchived -> TestTree
requestMarkAsArchived =
  req
    "MarkAsArchived"
    "fixture/MarkAsArchived.yaml"

requestStartCutover :: StartCutover -> TestTree
requestStartCutover =
  req
    "StartCutover"
    "fixture/StartCutover.yaml"

requestRetryDataReplication :: RetryDataReplication -> TestTree
requestRetryDataReplication =
  req
    "RetryDataReplication"
    "fixture/RetryDataReplication.yaml"

requestGetReplicationConfiguration :: GetReplicationConfiguration -> TestTree
requestGetReplicationConfiguration =
  req
    "GetReplicationConfiguration"
    "fixture/GetReplicationConfiguration.yaml"

requestChangeServerLifeCycleState :: ChangeServerLifeCycleState -> TestTree
requestChangeServerLifeCycleState =
  req
    "ChangeServerLifeCycleState"
    "fixture/ChangeServerLifeCycleState.yaml"

requestTerminateTargetInstances :: TerminateTargetInstances -> TestTree
requestTerminateTargetInstances =
  req
    "TerminateTargetInstances"
    "fixture/TerminateTargetInstances.yaml"

requestUpdateReplicationConfiguration :: UpdateReplicationConfiguration -> TestTree
requestUpdateReplicationConfiguration =
  req
    "UpdateReplicationConfiguration"
    "fixture/UpdateReplicationConfiguration.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestGetLaunchConfiguration :: GetLaunchConfiguration -> TestTree
requestGetLaunchConfiguration =
  req
    "GetLaunchConfiguration"
    "fixture/GetLaunchConfiguration.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDeleteSourceServer :: DeleteSourceServer -> TestTree
requestDeleteSourceServer =
  req
    "DeleteSourceServer"
    "fixture/DeleteSourceServer.yaml"

-- Responses

responseUpdateLaunchConfiguration :: LaunchConfiguration -> TestTree
responseUpdateLaunchConfiguration =
  res
    "UpdateLaunchConfigurationResponse"
    "fixture/UpdateLaunchConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLaunchConfiguration)

responseDescribeReplicationConfigurationTemplates :: DescribeReplicationConfigurationTemplatesResponse -> TestTree
responseDescribeReplicationConfigurationTemplates =
  res
    "DescribeReplicationConfigurationTemplatesResponse"
    "fixture/DescribeReplicationConfigurationTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReplicationConfigurationTemplates)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseInitializeService :: InitializeServiceResponse -> TestTree
responseInitializeService =
  res
    "InitializeServiceResponse"
    "fixture/InitializeServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy InitializeService)

responseUpdateReplicationConfigurationTemplate :: ReplicationConfigurationTemplate -> TestTree
responseUpdateReplicationConfigurationTemplate =
  res
    "UpdateReplicationConfigurationTemplateResponse"
    "fixture/UpdateReplicationConfigurationTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateReplicationConfigurationTemplate)

responseDeleteReplicationConfigurationTemplate :: DeleteReplicationConfigurationTemplateResponse -> TestTree
responseDeleteReplicationConfigurationTemplate =
  res
    "DeleteReplicationConfigurationTemplateResponse"
    "fixture/DeleteReplicationConfigurationTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteReplicationConfigurationTemplate)

responseCreateReplicationConfigurationTemplate :: ReplicationConfigurationTemplate -> TestTree
responseCreateReplicationConfigurationTemplate =
  res
    "CreateReplicationConfigurationTemplateResponse"
    "fixture/CreateReplicationConfigurationTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateReplicationConfigurationTemplate)

responseDescribeJobLogItems :: DescribeJobLogItemsResponse -> TestTree
responseDescribeJobLogItems =
  res
    "DescribeJobLogItemsResponse"
    "fixture/DescribeJobLogItemsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeJobLogItems)

responseDisconnectFromService :: SourceServer -> TestTree
responseDisconnectFromService =
  res
    "DisconnectFromServiceResponse"
    "fixture/DisconnectFromServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisconnectFromService)

responseStartTest :: StartTestResponse -> TestTree
responseStartTest =
  res
    "StartTestResponse"
    "fixture/StartTestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartTest)

responseDescribeSourceServers :: DescribeSourceServersResponse -> TestTree
responseDescribeSourceServers =
  res
    "DescribeSourceServersResponse"
    "fixture/DescribeSourceServersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSourceServers)

responseDeleteJob :: DeleteJobResponse -> TestTree
responseDeleteJob =
  res
    "DeleteJobResponse"
    "fixture/DeleteJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteJob)

responseFinalizeCutover :: SourceServer -> TestTree
responseFinalizeCutover =
  res
    "FinalizeCutoverResponse"
    "fixture/FinalizeCutoverResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy FinalizeCutover)

responseDescribeJobs :: DescribeJobsResponse -> TestTree
responseDescribeJobs =
  res
    "DescribeJobsResponse"
    "fixture/DescribeJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeJobs)

responseMarkAsArchived :: SourceServer -> TestTree
responseMarkAsArchived =
  res
    "MarkAsArchivedResponse"
    "fixture/MarkAsArchivedResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy MarkAsArchived)

responseStartCutover :: StartCutoverResponse -> TestTree
responseStartCutover =
  res
    "StartCutoverResponse"
    "fixture/StartCutoverResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartCutover)

responseRetryDataReplication :: SourceServer -> TestTree
responseRetryDataReplication =
  res
    "RetryDataReplicationResponse"
    "fixture/RetryDataReplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RetryDataReplication)

responseGetReplicationConfiguration :: ReplicationConfiguration -> TestTree
responseGetReplicationConfiguration =
  res
    "GetReplicationConfigurationResponse"
    "fixture/GetReplicationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetReplicationConfiguration)

responseChangeServerLifeCycleState :: SourceServer -> TestTree
responseChangeServerLifeCycleState =
  res
    "ChangeServerLifeCycleStateResponse"
    "fixture/ChangeServerLifeCycleStateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ChangeServerLifeCycleState)

responseTerminateTargetInstances :: TerminateTargetInstancesResponse -> TestTree
responseTerminateTargetInstances =
  res
    "TerminateTargetInstancesResponse"
    "fixture/TerminateTargetInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TerminateTargetInstances)

responseUpdateReplicationConfiguration :: ReplicationConfiguration -> TestTree
responseUpdateReplicationConfiguration =
  res
    "UpdateReplicationConfigurationResponse"
    "fixture/UpdateReplicationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateReplicationConfiguration)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseGetLaunchConfiguration :: LaunchConfiguration -> TestTree
responseGetLaunchConfiguration =
  res
    "GetLaunchConfigurationResponse"
    "fixture/GetLaunchConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLaunchConfiguration)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseDeleteSourceServer :: DeleteSourceServerResponse -> TestTree
responseDeleteSourceServer =
  res
    "DeleteSourceServerResponse"
    "fixture/DeleteSourceServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSourceServer)

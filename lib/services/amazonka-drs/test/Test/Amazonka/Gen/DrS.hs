{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.DrS
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.DrS where

import Amazonka.DrS
import qualified Data.Proxy as Proxy
import Test.Amazonka.DrS.Internal
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
--         [ requestCreateExtendedSourceServer $
--             newCreateExtendedSourceServer
--
--         , requestCreateReplicationConfigurationTemplate $
--             newCreateReplicationConfigurationTemplate
--
--         , requestDeleteJob $
--             newDeleteJob
--
--         , requestDeleteRecoveryInstance $
--             newDeleteRecoveryInstance
--
--         , requestDeleteReplicationConfigurationTemplate $
--             newDeleteReplicationConfigurationTemplate
--
--         , requestDeleteSourceServer $
--             newDeleteSourceServer
--
--         , requestDescribeJobLogItems $
--             newDescribeJobLogItems
--
--         , requestDescribeJobs $
--             newDescribeJobs
--
--         , requestDescribeRecoveryInstances $
--             newDescribeRecoveryInstances
--
--         , requestDescribeRecoverySnapshots $
--             newDescribeRecoverySnapshots
--
--         , requestDescribeReplicationConfigurationTemplates $
--             newDescribeReplicationConfigurationTemplates
--
--         , requestDescribeSourceServers $
--             newDescribeSourceServers
--
--         , requestDisconnectRecoveryInstance $
--             newDisconnectRecoveryInstance
--
--         , requestDisconnectSourceServer $
--             newDisconnectSourceServer
--
--         , requestGetFailbackReplicationConfiguration $
--             newGetFailbackReplicationConfiguration
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
--         , requestListExtensibleSourceServers $
--             newListExtensibleSourceServers
--
--         , requestListStagingAccounts $
--             newListStagingAccounts
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestRetryDataReplication $
--             newRetryDataReplication
--
--         , requestReverseReplication $
--             newReverseReplication
--
--         , requestStartFailbackLaunch $
--             newStartFailbackLaunch
--
--         , requestStartRecovery $
--             newStartRecovery
--
--         , requestStartReplication $
--             newStartReplication
--
--         , requestStopFailback $
--             newStopFailback
--
--         , requestStopReplication $
--             newStopReplication
--
--         , requestTagResource $
--             newTagResource
--
--         , requestTerminateRecoveryInstances $
--             newTerminateRecoveryInstances
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateFailbackReplicationConfiguration $
--             newUpdateFailbackReplicationConfiguration
--
--         , requestUpdateLaunchConfiguration $
--             newUpdateLaunchConfiguration
--
--         , requestUpdateReplicationConfiguration $
--             newUpdateReplicationConfiguration
--
--         , requestUpdateReplicationConfigurationTemplate $
--             newUpdateReplicationConfigurationTemplate
--
--           ]

--     , testGroup "response"
--         [ responseCreateExtendedSourceServer $
--             newCreateExtendedSourceServerResponse
--
--         , responseCreateReplicationConfigurationTemplate $
--             newReplicationConfigurationTemplate
--
--         , responseDeleteJob $
--             newDeleteJobResponse
--
--         , responseDeleteRecoveryInstance $
--             newDeleteRecoveryInstanceResponse
--
--         , responseDeleteReplicationConfigurationTemplate $
--             newDeleteReplicationConfigurationTemplateResponse
--
--         , responseDeleteSourceServer $
--             newDeleteSourceServerResponse
--
--         , responseDescribeJobLogItems $
--             newDescribeJobLogItemsResponse
--
--         , responseDescribeJobs $
--             newDescribeJobsResponse
--
--         , responseDescribeRecoveryInstances $
--             newDescribeRecoveryInstancesResponse
--
--         , responseDescribeRecoverySnapshots $
--             newDescribeRecoverySnapshotsResponse
--
--         , responseDescribeReplicationConfigurationTemplates $
--             newDescribeReplicationConfigurationTemplatesResponse
--
--         , responseDescribeSourceServers $
--             newDescribeSourceServersResponse
--
--         , responseDisconnectRecoveryInstance $
--             newDisconnectRecoveryInstanceResponse
--
--         , responseDisconnectSourceServer $
--             newSourceServer
--
--         , responseGetFailbackReplicationConfiguration $
--             newGetFailbackReplicationConfigurationResponse
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
--         , responseListExtensibleSourceServers $
--             newListExtensibleSourceServersResponse
--
--         , responseListStagingAccounts $
--             newListStagingAccountsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseRetryDataReplication $
--             newSourceServer
--
--         , responseReverseReplication $
--             newReverseReplicationResponse
--
--         , responseStartFailbackLaunch $
--             newStartFailbackLaunchResponse
--
--         , responseStartRecovery $
--             newStartRecoveryResponse
--
--         , responseStartReplication $
--             newStartReplicationResponse
--
--         , responseStopFailback $
--             newStopFailbackResponse
--
--         , responseStopReplication $
--             newStopReplicationResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseTerminateRecoveryInstances $
--             newTerminateRecoveryInstancesResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateFailbackReplicationConfiguration $
--             newUpdateFailbackReplicationConfigurationResponse
--
--         , responseUpdateLaunchConfiguration $
--             newLaunchConfiguration
--
--         , responseUpdateReplicationConfiguration $
--             newReplicationConfiguration
--
--         , responseUpdateReplicationConfigurationTemplate $
--             newReplicationConfigurationTemplate
--
--           ]
--     ]

-- Requests

requestCreateExtendedSourceServer :: CreateExtendedSourceServer -> TestTree
requestCreateExtendedSourceServer =
  req
    "CreateExtendedSourceServer"
    "fixture/CreateExtendedSourceServer.yaml"

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

requestDeleteRecoveryInstance :: DeleteRecoveryInstance -> TestTree
requestDeleteRecoveryInstance =
  req
    "DeleteRecoveryInstance"
    "fixture/DeleteRecoveryInstance.yaml"

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

requestDescribeRecoveryInstances :: DescribeRecoveryInstances -> TestTree
requestDescribeRecoveryInstances =
  req
    "DescribeRecoveryInstances"
    "fixture/DescribeRecoveryInstances.yaml"

requestDescribeRecoverySnapshots :: DescribeRecoverySnapshots -> TestTree
requestDescribeRecoverySnapshots =
  req
    "DescribeRecoverySnapshots"
    "fixture/DescribeRecoverySnapshots.yaml"

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

requestDisconnectRecoveryInstance :: DisconnectRecoveryInstance -> TestTree
requestDisconnectRecoveryInstance =
  req
    "DisconnectRecoveryInstance"
    "fixture/DisconnectRecoveryInstance.yaml"

requestDisconnectSourceServer :: DisconnectSourceServer -> TestTree
requestDisconnectSourceServer =
  req
    "DisconnectSourceServer"
    "fixture/DisconnectSourceServer.yaml"

requestGetFailbackReplicationConfiguration :: GetFailbackReplicationConfiguration -> TestTree
requestGetFailbackReplicationConfiguration =
  req
    "GetFailbackReplicationConfiguration"
    "fixture/GetFailbackReplicationConfiguration.yaml"

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

requestListExtensibleSourceServers :: ListExtensibleSourceServers -> TestTree
requestListExtensibleSourceServers =
  req
    "ListExtensibleSourceServers"
    "fixture/ListExtensibleSourceServers.yaml"

requestListStagingAccounts :: ListStagingAccounts -> TestTree
requestListStagingAccounts =
  req
    "ListStagingAccounts"
    "fixture/ListStagingAccounts.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestRetryDataReplication :: RetryDataReplication -> TestTree
requestRetryDataReplication =
  req
    "RetryDataReplication"
    "fixture/RetryDataReplication.yaml"

requestReverseReplication :: ReverseReplication -> TestTree
requestReverseReplication =
  req
    "ReverseReplication"
    "fixture/ReverseReplication.yaml"

requestStartFailbackLaunch :: StartFailbackLaunch -> TestTree
requestStartFailbackLaunch =
  req
    "StartFailbackLaunch"
    "fixture/StartFailbackLaunch.yaml"

requestStartRecovery :: StartRecovery -> TestTree
requestStartRecovery =
  req
    "StartRecovery"
    "fixture/StartRecovery.yaml"

requestStartReplication :: StartReplication -> TestTree
requestStartReplication =
  req
    "StartReplication"
    "fixture/StartReplication.yaml"

requestStopFailback :: StopFailback -> TestTree
requestStopFailback =
  req
    "StopFailback"
    "fixture/StopFailback.yaml"

requestStopReplication :: StopReplication -> TestTree
requestStopReplication =
  req
    "StopReplication"
    "fixture/StopReplication.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestTerminateRecoveryInstances :: TerminateRecoveryInstances -> TestTree
requestTerminateRecoveryInstances =
  req
    "TerminateRecoveryInstances"
    "fixture/TerminateRecoveryInstances.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateFailbackReplicationConfiguration :: UpdateFailbackReplicationConfiguration -> TestTree
requestUpdateFailbackReplicationConfiguration =
  req
    "UpdateFailbackReplicationConfiguration"
    "fixture/UpdateFailbackReplicationConfiguration.yaml"

requestUpdateLaunchConfiguration :: UpdateLaunchConfiguration -> TestTree
requestUpdateLaunchConfiguration =
  req
    "UpdateLaunchConfiguration"
    "fixture/UpdateLaunchConfiguration.yaml"

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

-- Responses

responseCreateExtendedSourceServer :: CreateExtendedSourceServerResponse -> TestTree
responseCreateExtendedSourceServer =
  res
    "CreateExtendedSourceServerResponse"
    "fixture/CreateExtendedSourceServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateExtendedSourceServer)

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

responseDeleteRecoveryInstance :: DeleteRecoveryInstanceResponse -> TestTree
responseDeleteRecoveryInstance =
  res
    "DeleteRecoveryInstanceResponse"
    "fixture/DeleteRecoveryInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRecoveryInstance)

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

responseDescribeRecoveryInstances :: DescribeRecoveryInstancesResponse -> TestTree
responseDescribeRecoveryInstances =
  res
    "DescribeRecoveryInstancesResponse"
    "fixture/DescribeRecoveryInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRecoveryInstances)

responseDescribeRecoverySnapshots :: DescribeRecoverySnapshotsResponse -> TestTree
responseDescribeRecoverySnapshots =
  res
    "DescribeRecoverySnapshotsResponse"
    "fixture/DescribeRecoverySnapshotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRecoverySnapshots)

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

responseDisconnectRecoveryInstance :: DisconnectRecoveryInstanceResponse -> TestTree
responseDisconnectRecoveryInstance =
  res
    "DisconnectRecoveryInstanceResponse"
    "fixture/DisconnectRecoveryInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisconnectRecoveryInstance)

responseDisconnectSourceServer :: SourceServer -> TestTree
responseDisconnectSourceServer =
  res
    "DisconnectSourceServerResponse"
    "fixture/DisconnectSourceServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisconnectSourceServer)

responseGetFailbackReplicationConfiguration :: GetFailbackReplicationConfigurationResponse -> TestTree
responseGetFailbackReplicationConfiguration =
  res
    "GetFailbackReplicationConfigurationResponse"
    "fixture/GetFailbackReplicationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFailbackReplicationConfiguration)

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

responseListExtensibleSourceServers :: ListExtensibleSourceServersResponse -> TestTree
responseListExtensibleSourceServers =
  res
    "ListExtensibleSourceServersResponse"
    "fixture/ListExtensibleSourceServersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListExtensibleSourceServers)

responseListStagingAccounts :: ListStagingAccountsResponse -> TestTree
responseListStagingAccounts =
  res
    "ListStagingAccountsResponse"
    "fixture/ListStagingAccountsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStagingAccounts)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseRetryDataReplication :: SourceServer -> TestTree
responseRetryDataReplication =
  res
    "RetryDataReplicationResponse"
    "fixture/RetryDataReplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RetryDataReplication)

responseReverseReplication :: ReverseReplicationResponse -> TestTree
responseReverseReplication =
  res
    "ReverseReplicationResponse"
    "fixture/ReverseReplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReverseReplication)

responseStartFailbackLaunch :: StartFailbackLaunchResponse -> TestTree
responseStartFailbackLaunch =
  res
    "StartFailbackLaunchResponse"
    "fixture/StartFailbackLaunchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartFailbackLaunch)

responseStartRecovery :: StartRecoveryResponse -> TestTree
responseStartRecovery =
  res
    "StartRecoveryResponse"
    "fixture/StartRecoveryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartRecovery)

responseStartReplication :: StartReplicationResponse -> TestTree
responseStartReplication =
  res
    "StartReplicationResponse"
    "fixture/StartReplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartReplication)

responseStopFailback :: StopFailbackResponse -> TestTree
responseStopFailback =
  res
    "StopFailbackResponse"
    "fixture/StopFailbackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopFailback)

responseStopReplication :: StopReplicationResponse -> TestTree
responseStopReplication =
  res
    "StopReplicationResponse"
    "fixture/StopReplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopReplication)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseTerminateRecoveryInstances :: TerminateRecoveryInstancesResponse -> TestTree
responseTerminateRecoveryInstances =
  res
    "TerminateRecoveryInstancesResponse"
    "fixture/TerminateRecoveryInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TerminateRecoveryInstances)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateFailbackReplicationConfiguration :: UpdateFailbackReplicationConfigurationResponse -> TestTree
responseUpdateFailbackReplicationConfiguration =
  res
    "UpdateFailbackReplicationConfigurationResponse"
    "fixture/UpdateFailbackReplicationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFailbackReplicationConfiguration)

responseUpdateLaunchConfiguration :: LaunchConfiguration -> TestTree
responseUpdateLaunchConfiguration =
  res
    "UpdateLaunchConfigurationResponse"
    "fixture/UpdateLaunchConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLaunchConfiguration)

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

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SMS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.SMS where

import Data.Proxy
import Network.AWS.SMS
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.SMS.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGenerateChangeSet $
--             newGenerateChangeSet
--
--         , requestImportAppCatalog $
--             newImportAppCatalog
--
--         , requestGetAppValidationConfiguration $
--             newGetAppValidationConfiguration
--
--         , requestLaunchApp $
--             newLaunchApp
--
--         , requestPutAppReplicationConfiguration $
--             newPutAppReplicationConfiguration
--
--         , requestGetConnectors $
--             newGetConnectors
--
--         , requestStartOnDemandReplicationRun $
--             newStartOnDemandReplicationRun
--
--         , requestGenerateTemplate $
--             newGenerateTemplate
--
--         , requestPutAppValidationConfiguration $
--             newPutAppValidationConfiguration
--
--         , requestListApps $
--             newListApps
--
--         , requestGetServers $
--             newGetServers
--
--         , requestGetReplicationRuns $
--             newGetReplicationRuns
--
--         , requestTerminateApp $
--             newTerminateApp
--
--         , requestStartAppReplication $
--             newStartAppReplication
--
--         , requestPutAppLaunchConfiguration $
--             newPutAppLaunchConfiguration
--
--         , requestStopAppReplication $
--             newStopAppReplication
--
--         , requestCreateReplicationJob $
--             newCreateReplicationJob
--
--         , requestGetApp $
--             newGetApp
--
--         , requestDeleteServerCatalog $
--             newDeleteServerCatalog
--
--         , requestDeleteAppReplicationConfiguration $
--             newDeleteAppReplicationConfiguration
--
--         , requestDisassociateConnector $
--             newDisassociateConnector
--
--         , requestGetReplicationJobs $
--             newGetReplicationJobs
--
--         , requestNotifyAppValidationOutput $
--             newNotifyAppValidationOutput
--
--         , requestStartOnDemandAppReplication $
--             newStartOnDemandAppReplication
--
--         , requestDeleteAppValidationConfiguration $
--             newDeleteAppValidationConfiguration
--
--         , requestGetAppReplicationConfiguration $
--             newGetAppReplicationConfiguration
--
--         , requestImportServerCatalog $
--             newImportServerCatalog
--
--         , requestGetAppValidationOutput $
--             newGetAppValidationOutput
--
--         , requestUpdateApp $
--             newUpdateApp
--
--         , requestDeleteApp $
--             newDeleteApp
--
--         , requestDeleteAppLaunchConfiguration $
--             newDeleteAppLaunchConfiguration
--
--         , requestCreateApp $
--             newCreateApp
--
--         , requestGetAppLaunchConfiguration $
--             newGetAppLaunchConfiguration
--
--         , requestUpdateReplicationJob $
--             newUpdateReplicationJob
--
--         , requestDeleteReplicationJob $
--             newDeleteReplicationJob
--
--           ]

--     , testGroup "response"
--         [ responseGenerateChangeSet $
--             newGenerateChangeSetResponse
--
--         , responseImportAppCatalog $
--             newImportAppCatalogResponse
--
--         , responseGetAppValidationConfiguration $
--             newGetAppValidationConfigurationResponse
--
--         , responseLaunchApp $
--             newLaunchAppResponse
--
--         , responsePutAppReplicationConfiguration $
--             newPutAppReplicationConfigurationResponse
--
--         , responseGetConnectors $
--             newGetConnectorsResponse
--
--         , responseStartOnDemandReplicationRun $
--             newStartOnDemandReplicationRunResponse
--
--         , responseGenerateTemplate $
--             newGenerateTemplateResponse
--
--         , responsePutAppValidationConfiguration $
--             newPutAppValidationConfigurationResponse
--
--         , responseListApps $
--             newListAppsResponse
--
--         , responseGetServers $
--             newGetServersResponse
--
--         , responseGetReplicationRuns $
--             newGetReplicationRunsResponse
--
--         , responseTerminateApp $
--             newTerminateAppResponse
--
--         , responseStartAppReplication $
--             newStartAppReplicationResponse
--
--         , responsePutAppLaunchConfiguration $
--             newPutAppLaunchConfigurationResponse
--
--         , responseStopAppReplication $
--             newStopAppReplicationResponse
--
--         , responseCreateReplicationJob $
--             newCreateReplicationJobResponse
--
--         , responseGetApp $
--             newGetAppResponse
--
--         , responseDeleteServerCatalog $
--             newDeleteServerCatalogResponse
--
--         , responseDeleteAppReplicationConfiguration $
--             newDeleteAppReplicationConfigurationResponse
--
--         , responseDisassociateConnector $
--             newDisassociateConnectorResponse
--
--         , responseGetReplicationJobs $
--             newGetReplicationJobsResponse
--
--         , responseNotifyAppValidationOutput $
--             newNotifyAppValidationOutputResponse
--
--         , responseStartOnDemandAppReplication $
--             newStartOnDemandAppReplicationResponse
--
--         , responseDeleteAppValidationConfiguration $
--             newDeleteAppValidationConfigurationResponse
--
--         , responseGetAppReplicationConfiguration $
--             newGetAppReplicationConfigurationResponse
--
--         , responseImportServerCatalog $
--             newImportServerCatalogResponse
--
--         , responseGetAppValidationOutput $
--             newGetAppValidationOutputResponse
--
--         , responseUpdateApp $
--             newUpdateAppResponse
--
--         , responseDeleteApp $
--             newDeleteAppResponse
--
--         , responseDeleteAppLaunchConfiguration $
--             newDeleteAppLaunchConfigurationResponse
--
--         , responseCreateApp $
--             newCreateAppResponse
--
--         , responseGetAppLaunchConfiguration $
--             newGetAppLaunchConfigurationResponse
--
--         , responseUpdateReplicationJob $
--             newUpdateReplicationJobResponse
--
--         , responseDeleteReplicationJob $
--             newDeleteReplicationJobResponse
--
--           ]
--     ]

-- Requests

requestGenerateChangeSet :: GenerateChangeSet -> TestTree
requestGenerateChangeSet =
  req
    "GenerateChangeSet"
    "fixture/GenerateChangeSet.yaml"

requestImportAppCatalog :: ImportAppCatalog -> TestTree
requestImportAppCatalog =
  req
    "ImportAppCatalog"
    "fixture/ImportAppCatalog.yaml"

requestGetAppValidationConfiguration :: GetAppValidationConfiguration -> TestTree
requestGetAppValidationConfiguration =
  req
    "GetAppValidationConfiguration"
    "fixture/GetAppValidationConfiguration.yaml"

requestLaunchApp :: LaunchApp -> TestTree
requestLaunchApp =
  req
    "LaunchApp"
    "fixture/LaunchApp.yaml"

requestPutAppReplicationConfiguration :: PutAppReplicationConfiguration -> TestTree
requestPutAppReplicationConfiguration =
  req
    "PutAppReplicationConfiguration"
    "fixture/PutAppReplicationConfiguration.yaml"

requestGetConnectors :: GetConnectors -> TestTree
requestGetConnectors =
  req
    "GetConnectors"
    "fixture/GetConnectors.yaml"

requestStartOnDemandReplicationRun :: StartOnDemandReplicationRun -> TestTree
requestStartOnDemandReplicationRun =
  req
    "StartOnDemandReplicationRun"
    "fixture/StartOnDemandReplicationRun.yaml"

requestGenerateTemplate :: GenerateTemplate -> TestTree
requestGenerateTemplate =
  req
    "GenerateTemplate"
    "fixture/GenerateTemplate.yaml"

requestPutAppValidationConfiguration :: PutAppValidationConfiguration -> TestTree
requestPutAppValidationConfiguration =
  req
    "PutAppValidationConfiguration"
    "fixture/PutAppValidationConfiguration.yaml"

requestListApps :: ListApps -> TestTree
requestListApps =
  req
    "ListApps"
    "fixture/ListApps.yaml"

requestGetServers :: GetServers -> TestTree
requestGetServers =
  req
    "GetServers"
    "fixture/GetServers.yaml"

requestGetReplicationRuns :: GetReplicationRuns -> TestTree
requestGetReplicationRuns =
  req
    "GetReplicationRuns"
    "fixture/GetReplicationRuns.yaml"

requestTerminateApp :: TerminateApp -> TestTree
requestTerminateApp =
  req
    "TerminateApp"
    "fixture/TerminateApp.yaml"

requestStartAppReplication :: StartAppReplication -> TestTree
requestStartAppReplication =
  req
    "StartAppReplication"
    "fixture/StartAppReplication.yaml"

requestPutAppLaunchConfiguration :: PutAppLaunchConfiguration -> TestTree
requestPutAppLaunchConfiguration =
  req
    "PutAppLaunchConfiguration"
    "fixture/PutAppLaunchConfiguration.yaml"

requestStopAppReplication :: StopAppReplication -> TestTree
requestStopAppReplication =
  req
    "StopAppReplication"
    "fixture/StopAppReplication.yaml"

requestCreateReplicationJob :: CreateReplicationJob -> TestTree
requestCreateReplicationJob =
  req
    "CreateReplicationJob"
    "fixture/CreateReplicationJob.yaml"

requestGetApp :: GetApp -> TestTree
requestGetApp =
  req
    "GetApp"
    "fixture/GetApp.yaml"

requestDeleteServerCatalog :: DeleteServerCatalog -> TestTree
requestDeleteServerCatalog =
  req
    "DeleteServerCatalog"
    "fixture/DeleteServerCatalog.yaml"

requestDeleteAppReplicationConfiguration :: DeleteAppReplicationConfiguration -> TestTree
requestDeleteAppReplicationConfiguration =
  req
    "DeleteAppReplicationConfiguration"
    "fixture/DeleteAppReplicationConfiguration.yaml"

requestDisassociateConnector :: DisassociateConnector -> TestTree
requestDisassociateConnector =
  req
    "DisassociateConnector"
    "fixture/DisassociateConnector.yaml"

requestGetReplicationJobs :: GetReplicationJobs -> TestTree
requestGetReplicationJobs =
  req
    "GetReplicationJobs"
    "fixture/GetReplicationJobs.yaml"

requestNotifyAppValidationOutput :: NotifyAppValidationOutput -> TestTree
requestNotifyAppValidationOutput =
  req
    "NotifyAppValidationOutput"
    "fixture/NotifyAppValidationOutput.yaml"

requestStartOnDemandAppReplication :: StartOnDemandAppReplication -> TestTree
requestStartOnDemandAppReplication =
  req
    "StartOnDemandAppReplication"
    "fixture/StartOnDemandAppReplication.yaml"

requestDeleteAppValidationConfiguration :: DeleteAppValidationConfiguration -> TestTree
requestDeleteAppValidationConfiguration =
  req
    "DeleteAppValidationConfiguration"
    "fixture/DeleteAppValidationConfiguration.yaml"

requestGetAppReplicationConfiguration :: GetAppReplicationConfiguration -> TestTree
requestGetAppReplicationConfiguration =
  req
    "GetAppReplicationConfiguration"
    "fixture/GetAppReplicationConfiguration.yaml"

requestImportServerCatalog :: ImportServerCatalog -> TestTree
requestImportServerCatalog =
  req
    "ImportServerCatalog"
    "fixture/ImportServerCatalog.yaml"

requestGetAppValidationOutput :: GetAppValidationOutput -> TestTree
requestGetAppValidationOutput =
  req
    "GetAppValidationOutput"
    "fixture/GetAppValidationOutput.yaml"

requestUpdateApp :: UpdateApp -> TestTree
requestUpdateApp =
  req
    "UpdateApp"
    "fixture/UpdateApp.yaml"

requestDeleteApp :: DeleteApp -> TestTree
requestDeleteApp =
  req
    "DeleteApp"
    "fixture/DeleteApp.yaml"

requestDeleteAppLaunchConfiguration :: DeleteAppLaunchConfiguration -> TestTree
requestDeleteAppLaunchConfiguration =
  req
    "DeleteAppLaunchConfiguration"
    "fixture/DeleteAppLaunchConfiguration.yaml"

requestCreateApp :: CreateApp -> TestTree
requestCreateApp =
  req
    "CreateApp"
    "fixture/CreateApp.yaml"

requestGetAppLaunchConfiguration :: GetAppLaunchConfiguration -> TestTree
requestGetAppLaunchConfiguration =
  req
    "GetAppLaunchConfiguration"
    "fixture/GetAppLaunchConfiguration.yaml"

requestUpdateReplicationJob :: UpdateReplicationJob -> TestTree
requestUpdateReplicationJob =
  req
    "UpdateReplicationJob"
    "fixture/UpdateReplicationJob.yaml"

requestDeleteReplicationJob :: DeleteReplicationJob -> TestTree
requestDeleteReplicationJob =
  req
    "DeleteReplicationJob"
    "fixture/DeleteReplicationJob.yaml"

-- Responses

responseGenerateChangeSet :: GenerateChangeSetResponse -> TestTree
responseGenerateChangeSet =
  res
    "GenerateChangeSetResponse"
    "fixture/GenerateChangeSetResponse.proto"
    defaultService
    (Proxy :: Proxy GenerateChangeSet)

responseImportAppCatalog :: ImportAppCatalogResponse -> TestTree
responseImportAppCatalog =
  res
    "ImportAppCatalogResponse"
    "fixture/ImportAppCatalogResponse.proto"
    defaultService
    (Proxy :: Proxy ImportAppCatalog)

responseGetAppValidationConfiguration :: GetAppValidationConfigurationResponse -> TestTree
responseGetAppValidationConfiguration =
  res
    "GetAppValidationConfigurationResponse"
    "fixture/GetAppValidationConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetAppValidationConfiguration)

responseLaunchApp :: LaunchAppResponse -> TestTree
responseLaunchApp =
  res
    "LaunchAppResponse"
    "fixture/LaunchAppResponse.proto"
    defaultService
    (Proxy :: Proxy LaunchApp)

responsePutAppReplicationConfiguration :: PutAppReplicationConfigurationResponse -> TestTree
responsePutAppReplicationConfiguration =
  res
    "PutAppReplicationConfigurationResponse"
    "fixture/PutAppReplicationConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy PutAppReplicationConfiguration)

responseGetConnectors :: GetConnectorsResponse -> TestTree
responseGetConnectors =
  res
    "GetConnectorsResponse"
    "fixture/GetConnectorsResponse.proto"
    defaultService
    (Proxy :: Proxy GetConnectors)

responseStartOnDemandReplicationRun :: StartOnDemandReplicationRunResponse -> TestTree
responseStartOnDemandReplicationRun =
  res
    "StartOnDemandReplicationRunResponse"
    "fixture/StartOnDemandReplicationRunResponse.proto"
    defaultService
    (Proxy :: Proxy StartOnDemandReplicationRun)

responseGenerateTemplate :: GenerateTemplateResponse -> TestTree
responseGenerateTemplate =
  res
    "GenerateTemplateResponse"
    "fixture/GenerateTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy GenerateTemplate)

responsePutAppValidationConfiguration :: PutAppValidationConfigurationResponse -> TestTree
responsePutAppValidationConfiguration =
  res
    "PutAppValidationConfigurationResponse"
    "fixture/PutAppValidationConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy PutAppValidationConfiguration)

responseListApps :: ListAppsResponse -> TestTree
responseListApps =
  res
    "ListAppsResponse"
    "fixture/ListAppsResponse.proto"
    defaultService
    (Proxy :: Proxy ListApps)

responseGetServers :: GetServersResponse -> TestTree
responseGetServers =
  res
    "GetServersResponse"
    "fixture/GetServersResponse.proto"
    defaultService
    (Proxy :: Proxy GetServers)

responseGetReplicationRuns :: GetReplicationRunsResponse -> TestTree
responseGetReplicationRuns =
  res
    "GetReplicationRunsResponse"
    "fixture/GetReplicationRunsResponse.proto"
    defaultService
    (Proxy :: Proxy GetReplicationRuns)

responseTerminateApp :: TerminateAppResponse -> TestTree
responseTerminateApp =
  res
    "TerminateAppResponse"
    "fixture/TerminateAppResponse.proto"
    defaultService
    (Proxy :: Proxy TerminateApp)

responseStartAppReplication :: StartAppReplicationResponse -> TestTree
responseStartAppReplication =
  res
    "StartAppReplicationResponse"
    "fixture/StartAppReplicationResponse.proto"
    defaultService
    (Proxy :: Proxy StartAppReplication)

responsePutAppLaunchConfiguration :: PutAppLaunchConfigurationResponse -> TestTree
responsePutAppLaunchConfiguration =
  res
    "PutAppLaunchConfigurationResponse"
    "fixture/PutAppLaunchConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy PutAppLaunchConfiguration)

responseStopAppReplication :: StopAppReplicationResponse -> TestTree
responseStopAppReplication =
  res
    "StopAppReplicationResponse"
    "fixture/StopAppReplicationResponse.proto"
    defaultService
    (Proxy :: Proxy StopAppReplication)

responseCreateReplicationJob :: CreateReplicationJobResponse -> TestTree
responseCreateReplicationJob =
  res
    "CreateReplicationJobResponse"
    "fixture/CreateReplicationJobResponse.proto"
    defaultService
    (Proxy :: Proxy CreateReplicationJob)

responseGetApp :: GetAppResponse -> TestTree
responseGetApp =
  res
    "GetAppResponse"
    "fixture/GetAppResponse.proto"
    defaultService
    (Proxy :: Proxy GetApp)

responseDeleteServerCatalog :: DeleteServerCatalogResponse -> TestTree
responseDeleteServerCatalog =
  res
    "DeleteServerCatalogResponse"
    "fixture/DeleteServerCatalogResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteServerCatalog)

responseDeleteAppReplicationConfiguration :: DeleteAppReplicationConfigurationResponse -> TestTree
responseDeleteAppReplicationConfiguration =
  res
    "DeleteAppReplicationConfigurationResponse"
    "fixture/DeleteAppReplicationConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAppReplicationConfiguration)

responseDisassociateConnector :: DisassociateConnectorResponse -> TestTree
responseDisassociateConnector =
  res
    "DisassociateConnectorResponse"
    "fixture/DisassociateConnectorResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateConnector)

responseGetReplicationJobs :: GetReplicationJobsResponse -> TestTree
responseGetReplicationJobs =
  res
    "GetReplicationJobsResponse"
    "fixture/GetReplicationJobsResponse.proto"
    defaultService
    (Proxy :: Proxy GetReplicationJobs)

responseNotifyAppValidationOutput :: NotifyAppValidationOutputResponse -> TestTree
responseNotifyAppValidationOutput =
  res
    "NotifyAppValidationOutputResponse"
    "fixture/NotifyAppValidationOutputResponse.proto"
    defaultService
    (Proxy :: Proxy NotifyAppValidationOutput)

responseStartOnDemandAppReplication :: StartOnDemandAppReplicationResponse -> TestTree
responseStartOnDemandAppReplication =
  res
    "StartOnDemandAppReplicationResponse"
    "fixture/StartOnDemandAppReplicationResponse.proto"
    defaultService
    (Proxy :: Proxy StartOnDemandAppReplication)

responseDeleteAppValidationConfiguration :: DeleteAppValidationConfigurationResponse -> TestTree
responseDeleteAppValidationConfiguration =
  res
    "DeleteAppValidationConfigurationResponse"
    "fixture/DeleteAppValidationConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAppValidationConfiguration)

responseGetAppReplicationConfiguration :: GetAppReplicationConfigurationResponse -> TestTree
responseGetAppReplicationConfiguration =
  res
    "GetAppReplicationConfigurationResponse"
    "fixture/GetAppReplicationConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetAppReplicationConfiguration)

responseImportServerCatalog :: ImportServerCatalogResponse -> TestTree
responseImportServerCatalog =
  res
    "ImportServerCatalogResponse"
    "fixture/ImportServerCatalogResponse.proto"
    defaultService
    (Proxy :: Proxy ImportServerCatalog)

responseGetAppValidationOutput :: GetAppValidationOutputResponse -> TestTree
responseGetAppValidationOutput =
  res
    "GetAppValidationOutputResponse"
    "fixture/GetAppValidationOutputResponse.proto"
    defaultService
    (Proxy :: Proxy GetAppValidationOutput)

responseUpdateApp :: UpdateAppResponse -> TestTree
responseUpdateApp =
  res
    "UpdateAppResponse"
    "fixture/UpdateAppResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateApp)

responseDeleteApp :: DeleteAppResponse -> TestTree
responseDeleteApp =
  res
    "DeleteAppResponse"
    "fixture/DeleteAppResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApp)

responseDeleteAppLaunchConfiguration :: DeleteAppLaunchConfigurationResponse -> TestTree
responseDeleteAppLaunchConfiguration =
  res
    "DeleteAppLaunchConfigurationResponse"
    "fixture/DeleteAppLaunchConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAppLaunchConfiguration)

responseCreateApp :: CreateAppResponse -> TestTree
responseCreateApp =
  res
    "CreateAppResponse"
    "fixture/CreateAppResponse.proto"
    defaultService
    (Proxy :: Proxy CreateApp)

responseGetAppLaunchConfiguration :: GetAppLaunchConfigurationResponse -> TestTree
responseGetAppLaunchConfiguration =
  res
    "GetAppLaunchConfigurationResponse"
    "fixture/GetAppLaunchConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetAppLaunchConfiguration)

responseUpdateReplicationJob :: UpdateReplicationJobResponse -> TestTree
responseUpdateReplicationJob =
  res
    "UpdateReplicationJobResponse"
    "fixture/UpdateReplicationJobResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateReplicationJob)

responseDeleteReplicationJob :: DeleteReplicationJobResponse -> TestTree
responseDeleteReplicationJob =
  res
    "DeleteReplicationJobResponse"
    "fixture/DeleteReplicationJobResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteReplicationJob)

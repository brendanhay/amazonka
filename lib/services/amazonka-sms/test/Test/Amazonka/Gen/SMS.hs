{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.SMS
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.SMS where

import Amazonka.SMS
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.SMS.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateApp $
--             newCreateApp
--
--         , requestCreateReplicationJob $
--             newCreateReplicationJob
--
--         , requestDeleteApp $
--             newDeleteApp
--
--         , requestDeleteAppLaunchConfiguration $
--             newDeleteAppLaunchConfiguration
--
--         , requestDeleteAppReplicationConfiguration $
--             newDeleteAppReplicationConfiguration
--
--         , requestDeleteAppValidationConfiguration $
--             newDeleteAppValidationConfiguration
--
--         , requestDeleteReplicationJob $
--             newDeleteReplicationJob
--
--         , requestDeleteServerCatalog $
--             newDeleteServerCatalog
--
--         , requestDisassociateConnector $
--             newDisassociateConnector
--
--         , requestGenerateChangeSet $
--             newGenerateChangeSet
--
--         , requestGenerateTemplate $
--             newGenerateTemplate
--
--         , requestGetApp $
--             newGetApp
--
--         , requestGetAppLaunchConfiguration $
--             newGetAppLaunchConfiguration
--
--         , requestGetAppReplicationConfiguration $
--             newGetAppReplicationConfiguration
--
--         , requestGetAppValidationConfiguration $
--             newGetAppValidationConfiguration
--
--         , requestGetAppValidationOutput $
--             newGetAppValidationOutput
--
--         , requestGetConnectors $
--             newGetConnectors
--
--         , requestGetReplicationJobs $
--             newGetReplicationJobs
--
--         , requestGetReplicationRuns $
--             newGetReplicationRuns
--
--         , requestGetServers $
--             newGetServers
--
--         , requestImportAppCatalog $
--             newImportAppCatalog
--
--         , requestImportServerCatalog $
--             newImportServerCatalog
--
--         , requestLaunchApp $
--             newLaunchApp
--
--         , requestListApps $
--             newListApps
--
--         , requestNotifyAppValidationOutput $
--             newNotifyAppValidationOutput
--
--         , requestPutAppLaunchConfiguration $
--             newPutAppLaunchConfiguration
--
--         , requestPutAppReplicationConfiguration $
--             newPutAppReplicationConfiguration
--
--         , requestPutAppValidationConfiguration $
--             newPutAppValidationConfiguration
--
--         , requestStartAppReplication $
--             newStartAppReplication
--
--         , requestStartOnDemandAppReplication $
--             newStartOnDemandAppReplication
--
--         , requestStartOnDemandReplicationRun $
--             newStartOnDemandReplicationRun
--
--         , requestStopAppReplication $
--             newStopAppReplication
--
--         , requestTerminateApp $
--             newTerminateApp
--
--         , requestUpdateApp $
--             newUpdateApp
--
--         , requestUpdateReplicationJob $
--             newUpdateReplicationJob
--
--           ]

--     , testGroup "response"
--         [ responseCreateApp $
--             newCreateAppResponse
--
--         , responseCreateReplicationJob $
--             newCreateReplicationJobResponse
--
--         , responseDeleteApp $
--             newDeleteAppResponse
--
--         , responseDeleteAppLaunchConfiguration $
--             newDeleteAppLaunchConfigurationResponse
--
--         , responseDeleteAppReplicationConfiguration $
--             newDeleteAppReplicationConfigurationResponse
--
--         , responseDeleteAppValidationConfiguration $
--             newDeleteAppValidationConfigurationResponse
--
--         , responseDeleteReplicationJob $
--             newDeleteReplicationJobResponse
--
--         , responseDeleteServerCatalog $
--             newDeleteServerCatalogResponse
--
--         , responseDisassociateConnector $
--             newDisassociateConnectorResponse
--
--         , responseGenerateChangeSet $
--             newGenerateChangeSetResponse
--
--         , responseGenerateTemplate $
--             newGenerateTemplateResponse
--
--         , responseGetApp $
--             newGetAppResponse
--
--         , responseGetAppLaunchConfiguration $
--             newGetAppLaunchConfigurationResponse
--
--         , responseGetAppReplicationConfiguration $
--             newGetAppReplicationConfigurationResponse
--
--         , responseGetAppValidationConfiguration $
--             newGetAppValidationConfigurationResponse
--
--         , responseGetAppValidationOutput $
--             newGetAppValidationOutputResponse
--
--         , responseGetConnectors $
--             newGetConnectorsResponse
--
--         , responseGetReplicationJobs $
--             newGetReplicationJobsResponse
--
--         , responseGetReplicationRuns $
--             newGetReplicationRunsResponse
--
--         , responseGetServers $
--             newGetServersResponse
--
--         , responseImportAppCatalog $
--             newImportAppCatalogResponse
--
--         , responseImportServerCatalog $
--             newImportServerCatalogResponse
--
--         , responseLaunchApp $
--             newLaunchAppResponse
--
--         , responseListApps $
--             newListAppsResponse
--
--         , responseNotifyAppValidationOutput $
--             newNotifyAppValidationOutputResponse
--
--         , responsePutAppLaunchConfiguration $
--             newPutAppLaunchConfigurationResponse
--
--         , responsePutAppReplicationConfiguration $
--             newPutAppReplicationConfigurationResponse
--
--         , responsePutAppValidationConfiguration $
--             newPutAppValidationConfigurationResponse
--
--         , responseStartAppReplication $
--             newStartAppReplicationResponse
--
--         , responseStartOnDemandAppReplication $
--             newStartOnDemandAppReplicationResponse
--
--         , responseStartOnDemandReplicationRun $
--             newStartOnDemandReplicationRunResponse
--
--         , responseStopAppReplication $
--             newStopAppReplicationResponse
--
--         , responseTerminateApp $
--             newTerminateAppResponse
--
--         , responseUpdateApp $
--             newUpdateAppResponse
--
--         , responseUpdateReplicationJob $
--             newUpdateReplicationJobResponse
--
--           ]
--     ]

-- Requests

requestCreateApp :: CreateApp -> TestTree
requestCreateApp =
  req
    "CreateApp"
    "fixture/CreateApp.yaml"

requestCreateReplicationJob :: CreateReplicationJob -> TestTree
requestCreateReplicationJob =
  req
    "CreateReplicationJob"
    "fixture/CreateReplicationJob.yaml"

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

requestDeleteAppReplicationConfiguration :: DeleteAppReplicationConfiguration -> TestTree
requestDeleteAppReplicationConfiguration =
  req
    "DeleteAppReplicationConfiguration"
    "fixture/DeleteAppReplicationConfiguration.yaml"

requestDeleteAppValidationConfiguration :: DeleteAppValidationConfiguration -> TestTree
requestDeleteAppValidationConfiguration =
  req
    "DeleteAppValidationConfiguration"
    "fixture/DeleteAppValidationConfiguration.yaml"

requestDeleteReplicationJob :: DeleteReplicationJob -> TestTree
requestDeleteReplicationJob =
  req
    "DeleteReplicationJob"
    "fixture/DeleteReplicationJob.yaml"

requestDeleteServerCatalog :: DeleteServerCatalog -> TestTree
requestDeleteServerCatalog =
  req
    "DeleteServerCatalog"
    "fixture/DeleteServerCatalog.yaml"

requestDisassociateConnector :: DisassociateConnector -> TestTree
requestDisassociateConnector =
  req
    "DisassociateConnector"
    "fixture/DisassociateConnector.yaml"

requestGenerateChangeSet :: GenerateChangeSet -> TestTree
requestGenerateChangeSet =
  req
    "GenerateChangeSet"
    "fixture/GenerateChangeSet.yaml"

requestGenerateTemplate :: GenerateTemplate -> TestTree
requestGenerateTemplate =
  req
    "GenerateTemplate"
    "fixture/GenerateTemplate.yaml"

requestGetApp :: GetApp -> TestTree
requestGetApp =
  req
    "GetApp"
    "fixture/GetApp.yaml"

requestGetAppLaunchConfiguration :: GetAppLaunchConfiguration -> TestTree
requestGetAppLaunchConfiguration =
  req
    "GetAppLaunchConfiguration"
    "fixture/GetAppLaunchConfiguration.yaml"

requestGetAppReplicationConfiguration :: GetAppReplicationConfiguration -> TestTree
requestGetAppReplicationConfiguration =
  req
    "GetAppReplicationConfiguration"
    "fixture/GetAppReplicationConfiguration.yaml"

requestGetAppValidationConfiguration :: GetAppValidationConfiguration -> TestTree
requestGetAppValidationConfiguration =
  req
    "GetAppValidationConfiguration"
    "fixture/GetAppValidationConfiguration.yaml"

requestGetAppValidationOutput :: GetAppValidationOutput -> TestTree
requestGetAppValidationOutput =
  req
    "GetAppValidationOutput"
    "fixture/GetAppValidationOutput.yaml"

requestGetConnectors :: GetConnectors -> TestTree
requestGetConnectors =
  req
    "GetConnectors"
    "fixture/GetConnectors.yaml"

requestGetReplicationJobs :: GetReplicationJobs -> TestTree
requestGetReplicationJobs =
  req
    "GetReplicationJobs"
    "fixture/GetReplicationJobs.yaml"

requestGetReplicationRuns :: GetReplicationRuns -> TestTree
requestGetReplicationRuns =
  req
    "GetReplicationRuns"
    "fixture/GetReplicationRuns.yaml"

requestGetServers :: GetServers -> TestTree
requestGetServers =
  req
    "GetServers"
    "fixture/GetServers.yaml"

requestImportAppCatalog :: ImportAppCatalog -> TestTree
requestImportAppCatalog =
  req
    "ImportAppCatalog"
    "fixture/ImportAppCatalog.yaml"

requestImportServerCatalog :: ImportServerCatalog -> TestTree
requestImportServerCatalog =
  req
    "ImportServerCatalog"
    "fixture/ImportServerCatalog.yaml"

requestLaunchApp :: LaunchApp -> TestTree
requestLaunchApp =
  req
    "LaunchApp"
    "fixture/LaunchApp.yaml"

requestListApps :: ListApps -> TestTree
requestListApps =
  req
    "ListApps"
    "fixture/ListApps.yaml"

requestNotifyAppValidationOutput :: NotifyAppValidationOutput -> TestTree
requestNotifyAppValidationOutput =
  req
    "NotifyAppValidationOutput"
    "fixture/NotifyAppValidationOutput.yaml"

requestPutAppLaunchConfiguration :: PutAppLaunchConfiguration -> TestTree
requestPutAppLaunchConfiguration =
  req
    "PutAppLaunchConfiguration"
    "fixture/PutAppLaunchConfiguration.yaml"

requestPutAppReplicationConfiguration :: PutAppReplicationConfiguration -> TestTree
requestPutAppReplicationConfiguration =
  req
    "PutAppReplicationConfiguration"
    "fixture/PutAppReplicationConfiguration.yaml"

requestPutAppValidationConfiguration :: PutAppValidationConfiguration -> TestTree
requestPutAppValidationConfiguration =
  req
    "PutAppValidationConfiguration"
    "fixture/PutAppValidationConfiguration.yaml"

requestStartAppReplication :: StartAppReplication -> TestTree
requestStartAppReplication =
  req
    "StartAppReplication"
    "fixture/StartAppReplication.yaml"

requestStartOnDemandAppReplication :: StartOnDemandAppReplication -> TestTree
requestStartOnDemandAppReplication =
  req
    "StartOnDemandAppReplication"
    "fixture/StartOnDemandAppReplication.yaml"

requestStartOnDemandReplicationRun :: StartOnDemandReplicationRun -> TestTree
requestStartOnDemandReplicationRun =
  req
    "StartOnDemandReplicationRun"
    "fixture/StartOnDemandReplicationRun.yaml"

requestStopAppReplication :: StopAppReplication -> TestTree
requestStopAppReplication =
  req
    "StopAppReplication"
    "fixture/StopAppReplication.yaml"

requestTerminateApp :: TerminateApp -> TestTree
requestTerminateApp =
  req
    "TerminateApp"
    "fixture/TerminateApp.yaml"

requestUpdateApp :: UpdateApp -> TestTree
requestUpdateApp =
  req
    "UpdateApp"
    "fixture/UpdateApp.yaml"

requestUpdateReplicationJob :: UpdateReplicationJob -> TestTree
requestUpdateReplicationJob =
  req
    "UpdateReplicationJob"
    "fixture/UpdateReplicationJob.yaml"

-- Responses

responseCreateApp :: CreateAppResponse -> TestTree
responseCreateApp =
  res
    "CreateAppResponse"
    "fixture/CreateAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApp)

responseCreateReplicationJob :: CreateReplicationJobResponse -> TestTree
responseCreateReplicationJob =
  res
    "CreateReplicationJobResponse"
    "fixture/CreateReplicationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateReplicationJob)

responseDeleteApp :: DeleteAppResponse -> TestTree
responseDeleteApp =
  res
    "DeleteAppResponse"
    "fixture/DeleteAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApp)

responseDeleteAppLaunchConfiguration :: DeleteAppLaunchConfigurationResponse -> TestTree
responseDeleteAppLaunchConfiguration =
  res
    "DeleteAppLaunchConfigurationResponse"
    "fixture/DeleteAppLaunchConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAppLaunchConfiguration)

responseDeleteAppReplicationConfiguration :: DeleteAppReplicationConfigurationResponse -> TestTree
responseDeleteAppReplicationConfiguration =
  res
    "DeleteAppReplicationConfigurationResponse"
    "fixture/DeleteAppReplicationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAppReplicationConfiguration)

responseDeleteAppValidationConfiguration :: DeleteAppValidationConfigurationResponse -> TestTree
responseDeleteAppValidationConfiguration =
  res
    "DeleteAppValidationConfigurationResponse"
    "fixture/DeleteAppValidationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAppValidationConfiguration)

responseDeleteReplicationJob :: DeleteReplicationJobResponse -> TestTree
responseDeleteReplicationJob =
  res
    "DeleteReplicationJobResponse"
    "fixture/DeleteReplicationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteReplicationJob)

responseDeleteServerCatalog :: DeleteServerCatalogResponse -> TestTree
responseDeleteServerCatalog =
  res
    "DeleteServerCatalogResponse"
    "fixture/DeleteServerCatalogResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteServerCatalog)

responseDisassociateConnector :: DisassociateConnectorResponse -> TestTree
responseDisassociateConnector =
  res
    "DisassociateConnectorResponse"
    "fixture/DisassociateConnectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateConnector)

responseGenerateChangeSet :: GenerateChangeSetResponse -> TestTree
responseGenerateChangeSet =
  res
    "GenerateChangeSetResponse"
    "fixture/GenerateChangeSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GenerateChangeSet)

responseGenerateTemplate :: GenerateTemplateResponse -> TestTree
responseGenerateTemplate =
  res
    "GenerateTemplateResponse"
    "fixture/GenerateTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GenerateTemplate)

responseGetApp :: GetAppResponse -> TestTree
responseGetApp =
  res
    "GetAppResponse"
    "fixture/GetAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApp)

responseGetAppLaunchConfiguration :: GetAppLaunchConfigurationResponse -> TestTree
responseGetAppLaunchConfiguration =
  res
    "GetAppLaunchConfigurationResponse"
    "fixture/GetAppLaunchConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAppLaunchConfiguration)

responseGetAppReplicationConfiguration :: GetAppReplicationConfigurationResponse -> TestTree
responseGetAppReplicationConfiguration =
  res
    "GetAppReplicationConfigurationResponse"
    "fixture/GetAppReplicationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAppReplicationConfiguration)

responseGetAppValidationConfiguration :: GetAppValidationConfigurationResponse -> TestTree
responseGetAppValidationConfiguration =
  res
    "GetAppValidationConfigurationResponse"
    "fixture/GetAppValidationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAppValidationConfiguration)

responseGetAppValidationOutput :: GetAppValidationOutputResponse -> TestTree
responseGetAppValidationOutput =
  res
    "GetAppValidationOutputResponse"
    "fixture/GetAppValidationOutputResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAppValidationOutput)

responseGetConnectors :: GetConnectorsResponse -> TestTree
responseGetConnectors =
  res
    "GetConnectorsResponse"
    "fixture/GetConnectorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConnectors)

responseGetReplicationJobs :: GetReplicationJobsResponse -> TestTree
responseGetReplicationJobs =
  res
    "GetReplicationJobsResponse"
    "fixture/GetReplicationJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetReplicationJobs)

responseGetReplicationRuns :: GetReplicationRunsResponse -> TestTree
responseGetReplicationRuns =
  res
    "GetReplicationRunsResponse"
    "fixture/GetReplicationRunsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetReplicationRuns)

responseGetServers :: GetServersResponse -> TestTree
responseGetServers =
  res
    "GetServersResponse"
    "fixture/GetServersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetServers)

responseImportAppCatalog :: ImportAppCatalogResponse -> TestTree
responseImportAppCatalog =
  res
    "ImportAppCatalogResponse"
    "fixture/ImportAppCatalogResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportAppCatalog)

responseImportServerCatalog :: ImportServerCatalogResponse -> TestTree
responseImportServerCatalog =
  res
    "ImportServerCatalogResponse"
    "fixture/ImportServerCatalogResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportServerCatalog)

responseLaunchApp :: LaunchAppResponse -> TestTree
responseLaunchApp =
  res
    "LaunchAppResponse"
    "fixture/LaunchAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy LaunchApp)

responseListApps :: ListAppsResponse -> TestTree
responseListApps =
  res
    "ListAppsResponse"
    "fixture/ListAppsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApps)

responseNotifyAppValidationOutput :: NotifyAppValidationOutputResponse -> TestTree
responseNotifyAppValidationOutput =
  res
    "NotifyAppValidationOutputResponse"
    "fixture/NotifyAppValidationOutputResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy NotifyAppValidationOutput)

responsePutAppLaunchConfiguration :: PutAppLaunchConfigurationResponse -> TestTree
responsePutAppLaunchConfiguration =
  res
    "PutAppLaunchConfigurationResponse"
    "fixture/PutAppLaunchConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAppLaunchConfiguration)

responsePutAppReplicationConfiguration :: PutAppReplicationConfigurationResponse -> TestTree
responsePutAppReplicationConfiguration =
  res
    "PutAppReplicationConfigurationResponse"
    "fixture/PutAppReplicationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAppReplicationConfiguration)

responsePutAppValidationConfiguration :: PutAppValidationConfigurationResponse -> TestTree
responsePutAppValidationConfiguration =
  res
    "PutAppValidationConfigurationResponse"
    "fixture/PutAppValidationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAppValidationConfiguration)

responseStartAppReplication :: StartAppReplicationResponse -> TestTree
responseStartAppReplication =
  res
    "StartAppReplicationResponse"
    "fixture/StartAppReplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartAppReplication)

responseStartOnDemandAppReplication :: StartOnDemandAppReplicationResponse -> TestTree
responseStartOnDemandAppReplication =
  res
    "StartOnDemandAppReplicationResponse"
    "fixture/StartOnDemandAppReplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartOnDemandAppReplication)

responseStartOnDemandReplicationRun :: StartOnDemandReplicationRunResponse -> TestTree
responseStartOnDemandReplicationRun =
  res
    "StartOnDemandReplicationRunResponse"
    "fixture/StartOnDemandReplicationRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartOnDemandReplicationRun)

responseStopAppReplication :: StopAppReplicationResponse -> TestTree
responseStopAppReplication =
  res
    "StopAppReplicationResponse"
    "fixture/StopAppReplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopAppReplication)

responseTerminateApp :: TerminateAppResponse -> TestTree
responseTerminateApp =
  res
    "TerminateAppResponse"
    "fixture/TerminateAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TerminateApp)

responseUpdateApp :: UpdateAppResponse -> TestTree
responseUpdateApp =
  res
    "UpdateAppResponse"
    "fixture/UpdateAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApp)

responseUpdateReplicationJob :: UpdateReplicationJobResponse -> TestTree
responseUpdateReplicationJob =
  res
    "UpdateReplicationJobResponse"
    "fixture/UpdateReplicationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateReplicationJob)

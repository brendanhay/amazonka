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

import Amazonka.SMS
import qualified Data.Proxy as Proxy
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
--         [ requestDeleteAppReplicationConfiguration $
--             newDeleteAppReplicationConfiguration
--
--         , requestPutAppReplicationConfiguration $
--             newPutAppReplicationConfiguration
--
--         , requestDeleteServerCatalog $
--             newDeleteServerCatalog
--
--         , requestImportAppCatalog $
--             newImportAppCatalog
--
--         , requestGetAppLaunchConfiguration $
--             newGetAppLaunchConfiguration
--
--         , requestDeleteAppLaunchConfiguration $
--             newDeleteAppLaunchConfiguration
--
--         , requestStartAppReplication $
--             newStartAppReplication
--
--         , requestPutAppLaunchConfiguration $
--             newPutAppLaunchConfiguration
--
--         , requestGetReplicationRuns $
--             newGetReplicationRuns
--
--         , requestTerminateApp $
--             newTerminateApp
--
--         , requestListApps $
--             newListApps
--
--         , requestGetServers $
--             newGetServers
--
--         , requestDeleteApp $
--             newDeleteApp
--
--         , requestUpdateApp $
--             newUpdateApp
--
--         , requestStartOnDemandAppReplication $
--             newStartOnDemandAppReplication
--
--         , requestImportServerCatalog $
--             newImportServerCatalog
--
--         , requestGenerateTemplate $
--             newGenerateTemplate
--
--         , requestGetConnectors $
--             newGetConnectors
--
--         , requestGetReplicationJobs $
--             newGetReplicationJobs
--
--         , requestDisassociateConnector $
--             newDisassociateConnector
--
--         , requestLaunchApp $
--             newLaunchApp
--
--         , requestGetAppValidationConfiguration $
--             newGetAppValidationConfiguration
--
--         , requestCreateReplicationJob $
--             newCreateReplicationJob
--
--         , requestGenerateChangeSet $
--             newGenerateChangeSet
--
--         , requestGetApp $
--             newGetApp
--
--         , requestUpdateReplicationJob $
--             newUpdateReplicationJob
--
--         , requestDeleteReplicationJob $
--             newDeleteReplicationJob
--
--         , requestCreateApp $
--             newCreateApp
--
--         , requestStopAppReplication $
--             newStopAppReplication
--
--         , requestDeleteAppValidationConfiguration $
--             newDeleteAppValidationConfiguration
--
--         , requestPutAppValidationConfiguration $
--             newPutAppValidationConfiguration
--
--         , requestGetAppValidationOutput $
--             newGetAppValidationOutput
--
--         , requestGetAppReplicationConfiguration $
--             newGetAppReplicationConfiguration
--
--         , requestStartOnDemandReplicationRun $
--             newStartOnDemandReplicationRun
--
--         , requestNotifyAppValidationOutput $
--             newNotifyAppValidationOutput
--
--           ]

--     , testGroup "response"
--         [ responseDeleteAppReplicationConfiguration $
--             newDeleteAppReplicationConfigurationResponse
--
--         , responsePutAppReplicationConfiguration $
--             newPutAppReplicationConfigurationResponse
--
--         , responseDeleteServerCatalog $
--             newDeleteServerCatalogResponse
--
--         , responseImportAppCatalog $
--             newImportAppCatalogResponse
--
--         , responseGetAppLaunchConfiguration $
--             newGetAppLaunchConfigurationResponse
--
--         , responseDeleteAppLaunchConfiguration $
--             newDeleteAppLaunchConfigurationResponse
--
--         , responseStartAppReplication $
--             newStartAppReplicationResponse
--
--         , responsePutAppLaunchConfiguration $
--             newPutAppLaunchConfigurationResponse
--
--         , responseGetReplicationRuns $
--             newGetReplicationRunsResponse
--
--         , responseTerminateApp $
--             newTerminateAppResponse
--
--         , responseListApps $
--             newListAppsResponse
--
--         , responseGetServers $
--             newGetServersResponse
--
--         , responseDeleteApp $
--             newDeleteAppResponse
--
--         , responseUpdateApp $
--             newUpdateAppResponse
--
--         , responseStartOnDemandAppReplication $
--             newStartOnDemandAppReplicationResponse
--
--         , responseImportServerCatalog $
--             newImportServerCatalogResponse
--
--         , responseGenerateTemplate $
--             newGenerateTemplateResponse
--
--         , responseGetConnectors $
--             newGetConnectorsResponse
--
--         , responseGetReplicationJobs $
--             newGetReplicationJobsResponse
--
--         , responseDisassociateConnector $
--             newDisassociateConnectorResponse
--
--         , responseLaunchApp $
--             newLaunchAppResponse
--
--         , responseGetAppValidationConfiguration $
--             newGetAppValidationConfigurationResponse
--
--         , responseCreateReplicationJob $
--             newCreateReplicationJobResponse
--
--         , responseGenerateChangeSet $
--             newGenerateChangeSetResponse
--
--         , responseGetApp $
--             newGetAppResponse
--
--         , responseUpdateReplicationJob $
--             newUpdateReplicationJobResponse
--
--         , responseDeleteReplicationJob $
--             newDeleteReplicationJobResponse
--
--         , responseCreateApp $
--             newCreateAppResponse
--
--         , responseStopAppReplication $
--             newStopAppReplicationResponse
--
--         , responseDeleteAppValidationConfiguration $
--             newDeleteAppValidationConfigurationResponse
--
--         , responsePutAppValidationConfiguration $
--             newPutAppValidationConfigurationResponse
--
--         , responseGetAppValidationOutput $
--             newGetAppValidationOutputResponse
--
--         , responseGetAppReplicationConfiguration $
--             newGetAppReplicationConfigurationResponse
--
--         , responseStartOnDemandReplicationRun $
--             newStartOnDemandReplicationRunResponse
--
--         , responseNotifyAppValidationOutput $
--             newNotifyAppValidationOutputResponse
--
--           ]
--     ]

-- Requests

requestDeleteAppReplicationConfiguration :: DeleteAppReplicationConfiguration -> TestTree
requestDeleteAppReplicationConfiguration =
  req
    "DeleteAppReplicationConfiguration"
    "fixture/DeleteAppReplicationConfiguration.yaml"

requestPutAppReplicationConfiguration :: PutAppReplicationConfiguration -> TestTree
requestPutAppReplicationConfiguration =
  req
    "PutAppReplicationConfiguration"
    "fixture/PutAppReplicationConfiguration.yaml"

requestDeleteServerCatalog :: DeleteServerCatalog -> TestTree
requestDeleteServerCatalog =
  req
    "DeleteServerCatalog"
    "fixture/DeleteServerCatalog.yaml"

requestImportAppCatalog :: ImportAppCatalog -> TestTree
requestImportAppCatalog =
  req
    "ImportAppCatalog"
    "fixture/ImportAppCatalog.yaml"

requestGetAppLaunchConfiguration :: GetAppLaunchConfiguration -> TestTree
requestGetAppLaunchConfiguration =
  req
    "GetAppLaunchConfiguration"
    "fixture/GetAppLaunchConfiguration.yaml"

requestDeleteAppLaunchConfiguration :: DeleteAppLaunchConfiguration -> TestTree
requestDeleteAppLaunchConfiguration =
  req
    "DeleteAppLaunchConfiguration"
    "fixture/DeleteAppLaunchConfiguration.yaml"

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

requestDeleteApp :: DeleteApp -> TestTree
requestDeleteApp =
  req
    "DeleteApp"
    "fixture/DeleteApp.yaml"

requestUpdateApp :: UpdateApp -> TestTree
requestUpdateApp =
  req
    "UpdateApp"
    "fixture/UpdateApp.yaml"

requestStartOnDemandAppReplication :: StartOnDemandAppReplication -> TestTree
requestStartOnDemandAppReplication =
  req
    "StartOnDemandAppReplication"
    "fixture/StartOnDemandAppReplication.yaml"

requestImportServerCatalog :: ImportServerCatalog -> TestTree
requestImportServerCatalog =
  req
    "ImportServerCatalog"
    "fixture/ImportServerCatalog.yaml"

requestGenerateTemplate :: GenerateTemplate -> TestTree
requestGenerateTemplate =
  req
    "GenerateTemplate"
    "fixture/GenerateTemplate.yaml"

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

requestDisassociateConnector :: DisassociateConnector -> TestTree
requestDisassociateConnector =
  req
    "DisassociateConnector"
    "fixture/DisassociateConnector.yaml"

requestLaunchApp :: LaunchApp -> TestTree
requestLaunchApp =
  req
    "LaunchApp"
    "fixture/LaunchApp.yaml"

requestGetAppValidationConfiguration :: GetAppValidationConfiguration -> TestTree
requestGetAppValidationConfiguration =
  req
    "GetAppValidationConfiguration"
    "fixture/GetAppValidationConfiguration.yaml"

requestCreateReplicationJob :: CreateReplicationJob -> TestTree
requestCreateReplicationJob =
  req
    "CreateReplicationJob"
    "fixture/CreateReplicationJob.yaml"

requestGenerateChangeSet :: GenerateChangeSet -> TestTree
requestGenerateChangeSet =
  req
    "GenerateChangeSet"
    "fixture/GenerateChangeSet.yaml"

requestGetApp :: GetApp -> TestTree
requestGetApp =
  req
    "GetApp"
    "fixture/GetApp.yaml"

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

requestCreateApp :: CreateApp -> TestTree
requestCreateApp =
  req
    "CreateApp"
    "fixture/CreateApp.yaml"

requestStopAppReplication :: StopAppReplication -> TestTree
requestStopAppReplication =
  req
    "StopAppReplication"
    "fixture/StopAppReplication.yaml"

requestDeleteAppValidationConfiguration :: DeleteAppValidationConfiguration -> TestTree
requestDeleteAppValidationConfiguration =
  req
    "DeleteAppValidationConfiguration"
    "fixture/DeleteAppValidationConfiguration.yaml"

requestPutAppValidationConfiguration :: PutAppValidationConfiguration -> TestTree
requestPutAppValidationConfiguration =
  req
    "PutAppValidationConfiguration"
    "fixture/PutAppValidationConfiguration.yaml"

requestGetAppValidationOutput :: GetAppValidationOutput -> TestTree
requestGetAppValidationOutput =
  req
    "GetAppValidationOutput"
    "fixture/GetAppValidationOutput.yaml"

requestGetAppReplicationConfiguration :: GetAppReplicationConfiguration -> TestTree
requestGetAppReplicationConfiguration =
  req
    "GetAppReplicationConfiguration"
    "fixture/GetAppReplicationConfiguration.yaml"

requestStartOnDemandReplicationRun :: StartOnDemandReplicationRun -> TestTree
requestStartOnDemandReplicationRun =
  req
    "StartOnDemandReplicationRun"
    "fixture/StartOnDemandReplicationRun.yaml"

requestNotifyAppValidationOutput :: NotifyAppValidationOutput -> TestTree
requestNotifyAppValidationOutput =
  req
    "NotifyAppValidationOutput"
    "fixture/NotifyAppValidationOutput.yaml"

-- Responses

responseDeleteAppReplicationConfiguration :: DeleteAppReplicationConfigurationResponse -> TestTree
responseDeleteAppReplicationConfiguration =
  res
    "DeleteAppReplicationConfigurationResponse"
    "fixture/DeleteAppReplicationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAppReplicationConfiguration)

responsePutAppReplicationConfiguration :: PutAppReplicationConfigurationResponse -> TestTree
responsePutAppReplicationConfiguration =
  res
    "PutAppReplicationConfigurationResponse"
    "fixture/PutAppReplicationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAppReplicationConfiguration)

responseDeleteServerCatalog :: DeleteServerCatalogResponse -> TestTree
responseDeleteServerCatalog =
  res
    "DeleteServerCatalogResponse"
    "fixture/DeleteServerCatalogResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteServerCatalog)

responseImportAppCatalog :: ImportAppCatalogResponse -> TestTree
responseImportAppCatalog =
  res
    "ImportAppCatalogResponse"
    "fixture/ImportAppCatalogResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportAppCatalog)

responseGetAppLaunchConfiguration :: GetAppLaunchConfigurationResponse -> TestTree
responseGetAppLaunchConfiguration =
  res
    "GetAppLaunchConfigurationResponse"
    "fixture/GetAppLaunchConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAppLaunchConfiguration)

responseDeleteAppLaunchConfiguration :: DeleteAppLaunchConfigurationResponse -> TestTree
responseDeleteAppLaunchConfiguration =
  res
    "DeleteAppLaunchConfigurationResponse"
    "fixture/DeleteAppLaunchConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAppLaunchConfiguration)

responseStartAppReplication :: StartAppReplicationResponse -> TestTree
responseStartAppReplication =
  res
    "StartAppReplicationResponse"
    "fixture/StartAppReplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartAppReplication)

responsePutAppLaunchConfiguration :: PutAppLaunchConfigurationResponse -> TestTree
responsePutAppLaunchConfiguration =
  res
    "PutAppLaunchConfigurationResponse"
    "fixture/PutAppLaunchConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAppLaunchConfiguration)

responseGetReplicationRuns :: GetReplicationRunsResponse -> TestTree
responseGetReplicationRuns =
  res
    "GetReplicationRunsResponse"
    "fixture/GetReplicationRunsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetReplicationRuns)

responseTerminateApp :: TerminateAppResponse -> TestTree
responseTerminateApp =
  res
    "TerminateAppResponse"
    "fixture/TerminateAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TerminateApp)

responseListApps :: ListAppsResponse -> TestTree
responseListApps =
  res
    "ListAppsResponse"
    "fixture/ListAppsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApps)

responseGetServers :: GetServersResponse -> TestTree
responseGetServers =
  res
    "GetServersResponse"
    "fixture/GetServersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetServers)

responseDeleteApp :: DeleteAppResponse -> TestTree
responseDeleteApp =
  res
    "DeleteAppResponse"
    "fixture/DeleteAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApp)

responseUpdateApp :: UpdateAppResponse -> TestTree
responseUpdateApp =
  res
    "UpdateAppResponse"
    "fixture/UpdateAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApp)

responseStartOnDemandAppReplication :: StartOnDemandAppReplicationResponse -> TestTree
responseStartOnDemandAppReplication =
  res
    "StartOnDemandAppReplicationResponse"
    "fixture/StartOnDemandAppReplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartOnDemandAppReplication)

responseImportServerCatalog :: ImportServerCatalogResponse -> TestTree
responseImportServerCatalog =
  res
    "ImportServerCatalogResponse"
    "fixture/ImportServerCatalogResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportServerCatalog)

responseGenerateTemplate :: GenerateTemplateResponse -> TestTree
responseGenerateTemplate =
  res
    "GenerateTemplateResponse"
    "fixture/GenerateTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GenerateTemplate)

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

responseDisassociateConnector :: DisassociateConnectorResponse -> TestTree
responseDisassociateConnector =
  res
    "DisassociateConnectorResponse"
    "fixture/DisassociateConnectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateConnector)

responseLaunchApp :: LaunchAppResponse -> TestTree
responseLaunchApp =
  res
    "LaunchAppResponse"
    "fixture/LaunchAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy LaunchApp)

responseGetAppValidationConfiguration :: GetAppValidationConfigurationResponse -> TestTree
responseGetAppValidationConfiguration =
  res
    "GetAppValidationConfigurationResponse"
    "fixture/GetAppValidationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAppValidationConfiguration)

responseCreateReplicationJob :: CreateReplicationJobResponse -> TestTree
responseCreateReplicationJob =
  res
    "CreateReplicationJobResponse"
    "fixture/CreateReplicationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateReplicationJob)

responseGenerateChangeSet :: GenerateChangeSetResponse -> TestTree
responseGenerateChangeSet =
  res
    "GenerateChangeSetResponse"
    "fixture/GenerateChangeSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GenerateChangeSet)

responseGetApp :: GetAppResponse -> TestTree
responseGetApp =
  res
    "GetAppResponse"
    "fixture/GetAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApp)

responseUpdateReplicationJob :: UpdateReplicationJobResponse -> TestTree
responseUpdateReplicationJob =
  res
    "UpdateReplicationJobResponse"
    "fixture/UpdateReplicationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateReplicationJob)

responseDeleteReplicationJob :: DeleteReplicationJobResponse -> TestTree
responseDeleteReplicationJob =
  res
    "DeleteReplicationJobResponse"
    "fixture/DeleteReplicationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteReplicationJob)

responseCreateApp :: CreateAppResponse -> TestTree
responseCreateApp =
  res
    "CreateAppResponse"
    "fixture/CreateAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApp)

responseStopAppReplication :: StopAppReplicationResponse -> TestTree
responseStopAppReplication =
  res
    "StopAppReplicationResponse"
    "fixture/StopAppReplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopAppReplication)

responseDeleteAppValidationConfiguration :: DeleteAppValidationConfigurationResponse -> TestTree
responseDeleteAppValidationConfiguration =
  res
    "DeleteAppValidationConfigurationResponse"
    "fixture/DeleteAppValidationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAppValidationConfiguration)

responsePutAppValidationConfiguration :: PutAppValidationConfigurationResponse -> TestTree
responsePutAppValidationConfiguration =
  res
    "PutAppValidationConfigurationResponse"
    "fixture/PutAppValidationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAppValidationConfiguration)

responseGetAppValidationOutput :: GetAppValidationOutputResponse -> TestTree
responseGetAppValidationOutput =
  res
    "GetAppValidationOutputResponse"
    "fixture/GetAppValidationOutputResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAppValidationOutput)

responseGetAppReplicationConfiguration :: GetAppReplicationConfigurationResponse -> TestTree
responseGetAppReplicationConfiguration =
  res
    "GetAppReplicationConfigurationResponse"
    "fixture/GetAppReplicationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAppReplicationConfiguration)

responseStartOnDemandReplicationRun :: StartOnDemandReplicationRunResponse -> TestTree
responseStartOnDemandReplicationRun =
  res
    "StartOnDemandReplicationRunResponse"
    "fixture/StartOnDemandReplicationRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartOnDemandReplicationRun)

responseNotifyAppValidationOutput :: NotifyAppValidationOutputResponse -> TestTree
responseNotifyAppValidationOutput =
  res
    "NotifyAppValidationOutputResponse"
    "fixture/NotifyAppValidationOutputResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy NotifyAppValidationOutput)

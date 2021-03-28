{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SMS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.SMS where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.SMS
import Test.AWS.SMS.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDeleteAppReplicationConfiguration $
--             mkDeleteAppReplicationConfiguration
--
--         , requestPutAppReplicationConfiguration $
--             mkPutAppReplicationConfiguration
--
--         , requestDeleteServerCatalog $
--             mkDeleteServerCatalog
--
--         , requestImportAppCatalog $
--             mkImportAppCatalog
--
--         , requestGetAppLaunchConfiguration $
--             mkGetAppLaunchConfiguration
--
--         , requestDeleteAppLaunchConfiguration $
--             mkDeleteAppLaunchConfiguration
--
--         , requestStartAppReplication $
--             mkStartAppReplication
--
--         , requestPutAppLaunchConfiguration $
--             mkPutAppLaunchConfiguration
--
--         , requestGetReplicationRuns $
--             mkGetReplicationRuns
--
--         , requestTerminateApp $
--             mkTerminateApp
--
--         , requestListApps $
--             mkListApps
--
--         , requestGetServers $
--             mkGetServers
--
--         , requestDeleteApp $
--             mkDeleteApp
--
--         , requestUpdateApp $
--             mkUpdateApp
--
--         , requestStartOnDemandAppReplication $
--             mkStartOnDemandAppReplication
--
--         , requestImportServerCatalog $
--             mkImportServerCatalog
--
--         , requestGenerateTemplate $
--             mkGenerateTemplate
--
--         , requestGetConnectors $
--             mkGetConnectors
--
--         , requestGetReplicationJobs $
--             mkGetReplicationJobs
--
--         , requestDisassociateConnector $
--             mkDisassociateConnector
--
--         , requestLaunchApp $
--             mkLaunchApp
--
--         , requestGetAppValidationConfiguration $
--             mkGetAppValidationConfiguration
--
--         , requestCreateReplicationJob $
--             mkCreateReplicationJob
--
--         , requestGenerateChangeSet $
--             mkGenerateChangeSet
--
--         , requestGetApp $
--             mkGetApp
--
--         , requestUpdateReplicationJob $
--             mkUpdateReplicationJob
--
--         , requestDeleteReplicationJob $
--             mkDeleteReplicationJob
--
--         , requestCreateApp $
--             mkCreateApp
--
--         , requestStopAppReplication $
--             mkStopAppReplication
--
--         , requestDeleteAppValidationConfiguration $
--             mkDeleteAppValidationConfiguration
--
--         , requestPutAppValidationConfiguration $
--             mkPutAppValidationConfiguration
--
--         , requestGetAppValidationOutput $
--             mkGetAppValidationOutput
--
--         , requestGetAppReplicationConfiguration $
--             mkGetAppReplicationConfiguration
--
--         , requestStartOnDemandReplicationRun $
--             mkStartOnDemandReplicationRun
--
--         , requestNotifyAppValidationOutput $
--             mkNotifyAppValidationOutput
--
--           ]

--     , testGroup "response"
--         [ responseDeleteAppReplicationConfiguration $
--             mkDeleteAppReplicationConfigurationResponse
--
--         , responsePutAppReplicationConfiguration $
--             mkPutAppReplicationConfigurationResponse
--
--         , responseDeleteServerCatalog $
--             mkDeleteServerCatalogResponse
--
--         , responseImportAppCatalog $
--             mkImportAppCatalogResponse
--
--         , responseGetAppLaunchConfiguration $
--             mkGetAppLaunchConfigurationResponse
--
--         , responseDeleteAppLaunchConfiguration $
--             mkDeleteAppLaunchConfigurationResponse
--
--         , responseStartAppReplication $
--             mkStartAppReplicationResponse
--
--         , responsePutAppLaunchConfiguration $
--             mkPutAppLaunchConfigurationResponse
--
--         , responseGetReplicationRuns $
--             mkGetReplicationRunsResponse
--
--         , responseTerminateApp $
--             mkTerminateAppResponse
--
--         , responseListApps $
--             mkListAppsResponse
--
--         , responseGetServers $
--             mkGetServersResponse
--
--         , responseDeleteApp $
--             mkDeleteAppResponse
--
--         , responseUpdateApp $
--             mkUpdateAppResponse
--
--         , responseStartOnDemandAppReplication $
--             mkStartOnDemandAppReplicationResponse
--
--         , responseImportServerCatalog $
--             mkImportServerCatalogResponse
--
--         , responseGenerateTemplate $
--             mkGenerateTemplateResponse
--
--         , responseGetConnectors $
--             mkGetConnectorsResponse
--
--         , responseGetReplicationJobs $
--             mkGetReplicationJobsResponse
--
--         , responseDisassociateConnector $
--             mkDisassociateConnectorResponse
--
--         , responseLaunchApp $
--             mkLaunchAppResponse
--
--         , responseGetAppValidationConfiguration $
--             mkGetAppValidationConfigurationResponse
--
--         , responseCreateReplicationJob $
--             mkCreateReplicationJobResponse
--
--         , responseGenerateChangeSet $
--             mkGenerateChangeSetResponse
--
--         , responseGetApp $
--             mkGetAppResponse
--
--         , responseUpdateReplicationJob $
--             mkUpdateReplicationJobResponse
--
--         , responseDeleteReplicationJob $
--             mkDeleteReplicationJobResponse
--
--         , responseCreateApp $
--             mkCreateAppResponse
--
--         , responseStopAppReplication $
--             mkStopAppReplicationResponse
--
--         , responseDeleteAppValidationConfiguration $
--             mkDeleteAppValidationConfigurationResponse
--
--         , responsePutAppValidationConfiguration $
--             mkPutAppValidationConfigurationResponse
--
--         , responseGetAppValidationOutput $
--             mkGetAppValidationOutputResponse
--
--         , responseGetAppReplicationConfiguration $
--             mkGetAppReplicationConfigurationResponse
--
--         , responseStartOnDemandReplicationRun $
--             mkStartOnDemandReplicationRunResponse
--
--         , responseNotifyAppValidationOutput $
--             mkNotifyAppValidationOutputResponse
--
--           ]
--     ]

-- Requests

requestDeleteAppReplicationConfiguration :: DeleteAppReplicationConfiguration -> TestTree
requestDeleteAppReplicationConfiguration = req
    "DeleteAppReplicationConfiguration"
    "fixture/DeleteAppReplicationConfiguration.yaml"

requestPutAppReplicationConfiguration :: PutAppReplicationConfiguration -> TestTree
requestPutAppReplicationConfiguration = req
    "PutAppReplicationConfiguration"
    "fixture/PutAppReplicationConfiguration.yaml"

requestDeleteServerCatalog :: DeleteServerCatalog -> TestTree
requestDeleteServerCatalog = req
    "DeleteServerCatalog"
    "fixture/DeleteServerCatalog.yaml"

requestImportAppCatalog :: ImportAppCatalog -> TestTree
requestImportAppCatalog = req
    "ImportAppCatalog"
    "fixture/ImportAppCatalog.yaml"

requestGetAppLaunchConfiguration :: GetAppLaunchConfiguration -> TestTree
requestGetAppLaunchConfiguration = req
    "GetAppLaunchConfiguration"
    "fixture/GetAppLaunchConfiguration.yaml"

requestDeleteAppLaunchConfiguration :: DeleteAppLaunchConfiguration -> TestTree
requestDeleteAppLaunchConfiguration = req
    "DeleteAppLaunchConfiguration"
    "fixture/DeleteAppLaunchConfiguration.yaml"

requestStartAppReplication :: StartAppReplication -> TestTree
requestStartAppReplication = req
    "StartAppReplication"
    "fixture/StartAppReplication.yaml"

requestPutAppLaunchConfiguration :: PutAppLaunchConfiguration -> TestTree
requestPutAppLaunchConfiguration = req
    "PutAppLaunchConfiguration"
    "fixture/PutAppLaunchConfiguration.yaml"

requestGetReplicationRuns :: GetReplicationRuns -> TestTree
requestGetReplicationRuns = req
    "GetReplicationRuns"
    "fixture/GetReplicationRuns.yaml"

requestTerminateApp :: TerminateApp -> TestTree
requestTerminateApp = req
    "TerminateApp"
    "fixture/TerminateApp.yaml"

requestListApps :: ListApps -> TestTree
requestListApps = req
    "ListApps"
    "fixture/ListApps.yaml"

requestGetServers :: GetServers -> TestTree
requestGetServers = req
    "GetServers"
    "fixture/GetServers.yaml"

requestDeleteApp :: DeleteApp -> TestTree
requestDeleteApp = req
    "DeleteApp"
    "fixture/DeleteApp.yaml"

requestUpdateApp :: UpdateApp -> TestTree
requestUpdateApp = req
    "UpdateApp"
    "fixture/UpdateApp.yaml"

requestStartOnDemandAppReplication :: StartOnDemandAppReplication -> TestTree
requestStartOnDemandAppReplication = req
    "StartOnDemandAppReplication"
    "fixture/StartOnDemandAppReplication.yaml"

requestImportServerCatalog :: ImportServerCatalog -> TestTree
requestImportServerCatalog = req
    "ImportServerCatalog"
    "fixture/ImportServerCatalog.yaml"

requestGenerateTemplate :: GenerateTemplate -> TestTree
requestGenerateTemplate = req
    "GenerateTemplate"
    "fixture/GenerateTemplate.yaml"

requestGetConnectors :: GetConnectors -> TestTree
requestGetConnectors = req
    "GetConnectors"
    "fixture/GetConnectors.yaml"

requestGetReplicationJobs :: GetReplicationJobs -> TestTree
requestGetReplicationJobs = req
    "GetReplicationJobs"
    "fixture/GetReplicationJobs.yaml"

requestDisassociateConnector :: DisassociateConnector -> TestTree
requestDisassociateConnector = req
    "DisassociateConnector"
    "fixture/DisassociateConnector.yaml"

requestLaunchApp :: LaunchApp -> TestTree
requestLaunchApp = req
    "LaunchApp"
    "fixture/LaunchApp.yaml"

requestGetAppValidationConfiguration :: GetAppValidationConfiguration -> TestTree
requestGetAppValidationConfiguration = req
    "GetAppValidationConfiguration"
    "fixture/GetAppValidationConfiguration.yaml"

requestCreateReplicationJob :: CreateReplicationJob -> TestTree
requestCreateReplicationJob = req
    "CreateReplicationJob"
    "fixture/CreateReplicationJob.yaml"

requestGenerateChangeSet :: GenerateChangeSet -> TestTree
requestGenerateChangeSet = req
    "GenerateChangeSet"
    "fixture/GenerateChangeSet.yaml"

requestGetApp :: GetApp -> TestTree
requestGetApp = req
    "GetApp"
    "fixture/GetApp.yaml"

requestUpdateReplicationJob :: UpdateReplicationJob -> TestTree
requestUpdateReplicationJob = req
    "UpdateReplicationJob"
    "fixture/UpdateReplicationJob.yaml"

requestDeleteReplicationJob :: DeleteReplicationJob -> TestTree
requestDeleteReplicationJob = req
    "DeleteReplicationJob"
    "fixture/DeleteReplicationJob.yaml"

requestCreateApp :: CreateApp -> TestTree
requestCreateApp = req
    "CreateApp"
    "fixture/CreateApp.yaml"

requestStopAppReplication :: StopAppReplication -> TestTree
requestStopAppReplication = req
    "StopAppReplication"
    "fixture/StopAppReplication.yaml"

requestDeleteAppValidationConfiguration :: DeleteAppValidationConfiguration -> TestTree
requestDeleteAppValidationConfiguration = req
    "DeleteAppValidationConfiguration"
    "fixture/DeleteAppValidationConfiguration.yaml"

requestPutAppValidationConfiguration :: PutAppValidationConfiguration -> TestTree
requestPutAppValidationConfiguration = req
    "PutAppValidationConfiguration"
    "fixture/PutAppValidationConfiguration.yaml"

requestGetAppValidationOutput :: GetAppValidationOutput -> TestTree
requestGetAppValidationOutput = req
    "GetAppValidationOutput"
    "fixture/GetAppValidationOutput.yaml"

requestGetAppReplicationConfiguration :: GetAppReplicationConfiguration -> TestTree
requestGetAppReplicationConfiguration = req
    "GetAppReplicationConfiguration"
    "fixture/GetAppReplicationConfiguration.yaml"

requestStartOnDemandReplicationRun :: StartOnDemandReplicationRun -> TestTree
requestStartOnDemandReplicationRun = req
    "StartOnDemandReplicationRun"
    "fixture/StartOnDemandReplicationRun.yaml"

requestNotifyAppValidationOutput :: NotifyAppValidationOutput -> TestTree
requestNotifyAppValidationOutput = req
    "NotifyAppValidationOutput"
    "fixture/NotifyAppValidationOutput.yaml"

-- Responses

responseDeleteAppReplicationConfiguration :: DeleteAppReplicationConfigurationResponse -> TestTree
responseDeleteAppReplicationConfiguration = res
    "DeleteAppReplicationConfigurationResponse"
    "fixture/DeleteAppReplicationConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteAppReplicationConfiguration)

responsePutAppReplicationConfiguration :: PutAppReplicationConfigurationResponse -> TestTree
responsePutAppReplicationConfiguration = res
    "PutAppReplicationConfigurationResponse"
    "fixture/PutAppReplicationConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutAppReplicationConfiguration)

responseDeleteServerCatalog :: DeleteServerCatalogResponse -> TestTree
responseDeleteServerCatalog = res
    "DeleteServerCatalogResponse"
    "fixture/DeleteServerCatalogResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteServerCatalog)

responseImportAppCatalog :: ImportAppCatalogResponse -> TestTree
responseImportAppCatalog = res
    "ImportAppCatalogResponse"
    "fixture/ImportAppCatalogResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ImportAppCatalog)

responseGetAppLaunchConfiguration :: GetAppLaunchConfigurationResponse -> TestTree
responseGetAppLaunchConfiguration = res
    "GetAppLaunchConfigurationResponse"
    "fixture/GetAppLaunchConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetAppLaunchConfiguration)

responseDeleteAppLaunchConfiguration :: DeleteAppLaunchConfigurationResponse -> TestTree
responseDeleteAppLaunchConfiguration = res
    "DeleteAppLaunchConfigurationResponse"
    "fixture/DeleteAppLaunchConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteAppLaunchConfiguration)

responseStartAppReplication :: StartAppReplicationResponse -> TestTree
responseStartAppReplication = res
    "StartAppReplicationResponse"
    "fixture/StartAppReplicationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartAppReplication)

responsePutAppLaunchConfiguration :: PutAppLaunchConfigurationResponse -> TestTree
responsePutAppLaunchConfiguration = res
    "PutAppLaunchConfigurationResponse"
    "fixture/PutAppLaunchConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutAppLaunchConfiguration)

responseGetReplicationRuns :: GetReplicationRunsResponse -> TestTree
responseGetReplicationRuns = res
    "GetReplicationRunsResponse"
    "fixture/GetReplicationRunsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetReplicationRuns)

responseTerminateApp :: TerminateAppResponse -> TestTree
responseTerminateApp = res
    "TerminateAppResponse"
    "fixture/TerminateAppResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TerminateApp)

responseListApps :: ListAppsResponse -> TestTree
responseListApps = res
    "ListAppsResponse"
    "fixture/ListAppsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListApps)

responseGetServers :: GetServersResponse -> TestTree
responseGetServers = res
    "GetServersResponse"
    "fixture/GetServersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetServers)

responseDeleteApp :: DeleteAppResponse -> TestTree
responseDeleteApp = res
    "DeleteAppResponse"
    "fixture/DeleteAppResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteApp)

responseUpdateApp :: UpdateAppResponse -> TestTree
responseUpdateApp = res
    "UpdateAppResponse"
    "fixture/UpdateAppResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateApp)

responseStartOnDemandAppReplication :: StartOnDemandAppReplicationResponse -> TestTree
responseStartOnDemandAppReplication = res
    "StartOnDemandAppReplicationResponse"
    "fixture/StartOnDemandAppReplicationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartOnDemandAppReplication)

responseImportServerCatalog :: ImportServerCatalogResponse -> TestTree
responseImportServerCatalog = res
    "ImportServerCatalogResponse"
    "fixture/ImportServerCatalogResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ImportServerCatalog)

responseGenerateTemplate :: GenerateTemplateResponse -> TestTree
responseGenerateTemplate = res
    "GenerateTemplateResponse"
    "fixture/GenerateTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GenerateTemplate)

responseGetConnectors :: GetConnectorsResponse -> TestTree
responseGetConnectors = res
    "GetConnectorsResponse"
    "fixture/GetConnectorsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetConnectors)

responseGetReplicationJobs :: GetReplicationJobsResponse -> TestTree
responseGetReplicationJobs = res
    "GetReplicationJobsResponse"
    "fixture/GetReplicationJobsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetReplicationJobs)

responseDisassociateConnector :: DisassociateConnectorResponse -> TestTree
responseDisassociateConnector = res
    "DisassociateConnectorResponse"
    "fixture/DisassociateConnectorResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisassociateConnector)

responseLaunchApp :: LaunchAppResponse -> TestTree
responseLaunchApp = res
    "LaunchAppResponse"
    "fixture/LaunchAppResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy LaunchApp)

responseGetAppValidationConfiguration :: GetAppValidationConfigurationResponse -> TestTree
responseGetAppValidationConfiguration = res
    "GetAppValidationConfigurationResponse"
    "fixture/GetAppValidationConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetAppValidationConfiguration)

responseCreateReplicationJob :: CreateReplicationJobResponse -> TestTree
responseCreateReplicationJob = res
    "CreateReplicationJobResponse"
    "fixture/CreateReplicationJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateReplicationJob)

responseGenerateChangeSet :: GenerateChangeSetResponse -> TestTree
responseGenerateChangeSet = res
    "GenerateChangeSetResponse"
    "fixture/GenerateChangeSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GenerateChangeSet)

responseGetApp :: GetAppResponse -> TestTree
responseGetApp = res
    "GetAppResponse"
    "fixture/GetAppResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetApp)

responseUpdateReplicationJob :: UpdateReplicationJobResponse -> TestTree
responseUpdateReplicationJob = res
    "UpdateReplicationJobResponse"
    "fixture/UpdateReplicationJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateReplicationJob)

responseDeleteReplicationJob :: DeleteReplicationJobResponse -> TestTree
responseDeleteReplicationJob = res
    "DeleteReplicationJobResponse"
    "fixture/DeleteReplicationJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteReplicationJob)

responseCreateApp :: CreateAppResponse -> TestTree
responseCreateApp = res
    "CreateAppResponse"
    "fixture/CreateAppResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateApp)

responseStopAppReplication :: StopAppReplicationResponse -> TestTree
responseStopAppReplication = res
    "StopAppReplicationResponse"
    "fixture/StopAppReplicationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StopAppReplication)

responseDeleteAppValidationConfiguration :: DeleteAppValidationConfigurationResponse -> TestTree
responseDeleteAppValidationConfiguration = res
    "DeleteAppValidationConfigurationResponse"
    "fixture/DeleteAppValidationConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteAppValidationConfiguration)

responsePutAppValidationConfiguration :: PutAppValidationConfigurationResponse -> TestTree
responsePutAppValidationConfiguration = res
    "PutAppValidationConfigurationResponse"
    "fixture/PutAppValidationConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutAppValidationConfiguration)

responseGetAppValidationOutput :: GetAppValidationOutputResponse -> TestTree
responseGetAppValidationOutput = res
    "GetAppValidationOutputResponse"
    "fixture/GetAppValidationOutputResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetAppValidationOutput)

responseGetAppReplicationConfiguration :: GetAppReplicationConfigurationResponse -> TestTree
responseGetAppReplicationConfiguration = res
    "GetAppReplicationConfigurationResponse"
    "fixture/GetAppReplicationConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetAppReplicationConfiguration)

responseStartOnDemandReplicationRun :: StartOnDemandReplicationRunResponse -> TestTree
responseStartOnDemandReplicationRun = res
    "StartOnDemandReplicationRunResponse"
    "fixture/StartOnDemandReplicationRunResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartOnDemandReplicationRun)

responseNotifyAppValidationOutput :: NotifyAppValidationOutputResponse -> TestTree
responseNotifyAppValidationOutput = res
    "NotifyAppValidationOutputResponse"
    "fixture/NotifyAppValidationOutputResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy NotifyAppValidationOutput)

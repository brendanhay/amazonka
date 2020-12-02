{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SMS
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--         [ requestDeleteServerCatalog $
--             deleteServerCatalog
--
--         , requestGetReplicationRuns $
--             getReplicationRuns
--
--         , requestGetServers $
--             getServers
--
--         , requestImportServerCatalog $
--             importServerCatalog
--
--         , requestGetConnectors $
--             getConnectors
--
--         , requestGetReplicationJobs $
--             getReplicationJobs
--
--         , requestDisassociateConnector $
--             disassociateConnector
--
--         , requestCreateReplicationJob $
--             createReplicationJob
--
--         , requestUpdateReplicationJob $
--             updateReplicationJob
--
--         , requestDeleteReplicationJob $
--             deleteReplicationJob
--
--         , requestStartOnDemandReplicationRun $
--             startOnDemandReplicationRun
--
--           ]

--     , testGroup "response"
--         [ responseDeleteServerCatalog $
--             deleteServerCatalogResponse
--
--         , responseGetReplicationRuns $
--             getReplicationRunsResponse
--
--         , responseGetServers $
--             getServersResponse
--
--         , responseImportServerCatalog $
--             importServerCatalogResponse
--
--         , responseGetConnectors $
--             getConnectorsResponse
--
--         , responseGetReplicationJobs $
--             getReplicationJobsResponse
--
--         , responseDisassociateConnector $
--             disassociateConnectorResponse
--
--         , responseCreateReplicationJob $
--             createReplicationJobResponse
--
--         , responseUpdateReplicationJob $
--             updateReplicationJobResponse
--
--         , responseDeleteReplicationJob $
--             deleteReplicationJobResponse
--
--         , responseStartOnDemandReplicationRun $
--             startOnDemandReplicationRunResponse
--
--           ]
--     ]

-- Requests

requestDeleteServerCatalog :: DeleteServerCatalog -> TestTree
requestDeleteServerCatalog = req
    "DeleteServerCatalog"
    "fixture/DeleteServerCatalog.yaml"

requestGetReplicationRuns :: GetReplicationRuns -> TestTree
requestGetReplicationRuns = req
    "GetReplicationRuns"
    "fixture/GetReplicationRuns.yaml"

requestGetServers :: GetServers -> TestTree
requestGetServers = req
    "GetServers"
    "fixture/GetServers.yaml"

requestImportServerCatalog :: ImportServerCatalog -> TestTree
requestImportServerCatalog = req
    "ImportServerCatalog"
    "fixture/ImportServerCatalog.yaml"

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

requestCreateReplicationJob :: CreateReplicationJob -> TestTree
requestCreateReplicationJob = req
    "CreateReplicationJob"
    "fixture/CreateReplicationJob.yaml"

requestUpdateReplicationJob :: UpdateReplicationJob -> TestTree
requestUpdateReplicationJob = req
    "UpdateReplicationJob"
    "fixture/UpdateReplicationJob.yaml"

requestDeleteReplicationJob :: DeleteReplicationJob -> TestTree
requestDeleteReplicationJob = req
    "DeleteReplicationJob"
    "fixture/DeleteReplicationJob.yaml"

requestStartOnDemandReplicationRun :: StartOnDemandReplicationRun -> TestTree
requestStartOnDemandReplicationRun = req
    "StartOnDemandReplicationRun"
    "fixture/StartOnDemandReplicationRun.yaml"

-- Responses

responseDeleteServerCatalog :: DeleteServerCatalogResponse -> TestTree
responseDeleteServerCatalog = res
    "DeleteServerCatalogResponse"
    "fixture/DeleteServerCatalogResponse.proto"
    sms
    (Proxy :: Proxy DeleteServerCatalog)

responseGetReplicationRuns :: GetReplicationRunsResponse -> TestTree
responseGetReplicationRuns = res
    "GetReplicationRunsResponse"
    "fixture/GetReplicationRunsResponse.proto"
    sms
    (Proxy :: Proxy GetReplicationRuns)

responseGetServers :: GetServersResponse -> TestTree
responseGetServers = res
    "GetServersResponse"
    "fixture/GetServersResponse.proto"
    sms
    (Proxy :: Proxy GetServers)

responseImportServerCatalog :: ImportServerCatalogResponse -> TestTree
responseImportServerCatalog = res
    "ImportServerCatalogResponse"
    "fixture/ImportServerCatalogResponse.proto"
    sms
    (Proxy :: Proxy ImportServerCatalog)

responseGetConnectors :: GetConnectorsResponse -> TestTree
responseGetConnectors = res
    "GetConnectorsResponse"
    "fixture/GetConnectorsResponse.proto"
    sms
    (Proxy :: Proxy GetConnectors)

responseGetReplicationJobs :: GetReplicationJobsResponse -> TestTree
responseGetReplicationJobs = res
    "GetReplicationJobsResponse"
    "fixture/GetReplicationJobsResponse.proto"
    sms
    (Proxy :: Proxy GetReplicationJobs)

responseDisassociateConnector :: DisassociateConnectorResponse -> TestTree
responseDisassociateConnector = res
    "DisassociateConnectorResponse"
    "fixture/DisassociateConnectorResponse.proto"
    sms
    (Proxy :: Proxy DisassociateConnector)

responseCreateReplicationJob :: CreateReplicationJobResponse -> TestTree
responseCreateReplicationJob = res
    "CreateReplicationJobResponse"
    "fixture/CreateReplicationJobResponse.proto"
    sms
    (Proxy :: Proxy CreateReplicationJob)

responseUpdateReplicationJob :: UpdateReplicationJobResponse -> TestTree
responseUpdateReplicationJob = res
    "UpdateReplicationJobResponse"
    "fixture/UpdateReplicationJobResponse.proto"
    sms
    (Proxy :: Proxy UpdateReplicationJob)

responseDeleteReplicationJob :: DeleteReplicationJobResponse -> TestTree
responseDeleteReplicationJob = res
    "DeleteReplicationJobResponse"
    "fixture/DeleteReplicationJobResponse.proto"
    sms
    (Proxy :: Proxy DeleteReplicationJob)

responseStartOnDemandReplicationRun :: StartOnDemandReplicationRunResponse -> TestTree
responseStartOnDemandReplicationRun = res
    "StartOnDemandReplicationRunResponse"
    "fixture/StartOnDemandReplicationRunResponse.proto"
    sms
    (Proxy :: Proxy StartOnDemandReplicationRun)

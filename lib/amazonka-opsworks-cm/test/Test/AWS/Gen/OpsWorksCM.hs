{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.OpsWorksCM
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.OpsWorksCM where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.OpsWorksCM
import Test.AWS.OpsWorksCM.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateNode $
--             mkAssociateNode
--
--         , requestUpdateServer $
--             mkUpdateServer
--
--         , requestDeleteServer $
--             mkDeleteServer
--
--         , requestDeleteBackup $
--             mkDeleteBackup
--
--         , requestDescribeEvents $
--             mkDescribeEvents
--
--         , requestDisassociateNode $
--             mkDisassociateNode
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestCreateBackup $
--             mkCreateBackup
--
--         , requestUpdateServerEngineAttributes $
--             mkUpdateServerEngineAttributes
--
--         , requestStartMaintenance $
--             mkStartMaintenance
--
--         , requestDescribeBackups $
--             mkDescribeBackups
--
--         , requestCreateServer $
--             mkCreateServer
--
--         , requestRestoreServer $
--             mkRestoreServer
--
--         , requestDescribeNodeAssociationStatus $
--             mkDescribeNodeAssociationStatus
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestDescribeAccountAttributes $
--             mkDescribeAccountAttributes
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestDescribeServers $
--             mkDescribeServers
--
--         , requestExportServerEngineAttribute $
--             mkExportServerEngineAttribute
--
--           ]

--     , testGroup "response"
--         [ responseAssociateNode $
--             mkAssociateNodeResponse
--
--         , responseUpdateServer $
--             mkUpdateServerResponse
--
--         , responseDeleteServer $
--             mkDeleteServerResponse
--
--         , responseDeleteBackup $
--             mkDeleteBackupResponse
--
--         , responseDescribeEvents $
--             mkDescribeEventsResponse
--
--         , responseDisassociateNode $
--             mkDisassociateNodeResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseCreateBackup $
--             mkCreateBackupResponse
--
--         , responseUpdateServerEngineAttributes $
--             mkUpdateServerEngineAttributesResponse
--
--         , responseStartMaintenance $
--             mkStartMaintenanceResponse
--
--         , responseDescribeBackups $
--             mkDescribeBackupsResponse
--
--         , responseCreateServer $
--             mkCreateServerResponse
--
--         , responseRestoreServer $
--             mkRestoreServerResponse
--
--         , responseDescribeNodeAssociationStatus $
--             mkDescribeNodeAssociationStatusResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseDescribeAccountAttributes $
--             mkDescribeAccountAttributesResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseDescribeServers $
--             mkDescribeServersResponse
--
--         , responseExportServerEngineAttribute $
--             mkExportServerEngineAttributeResponse
--
--           ]
--     ]

-- Requests

requestAssociateNode :: AssociateNode -> TestTree
requestAssociateNode = req
    "AssociateNode"
    "fixture/AssociateNode.yaml"

requestUpdateServer :: UpdateServer -> TestTree
requestUpdateServer = req
    "UpdateServer"
    "fixture/UpdateServer.yaml"

requestDeleteServer :: DeleteServer -> TestTree
requestDeleteServer = req
    "DeleteServer"
    "fixture/DeleteServer.yaml"

requestDeleteBackup :: DeleteBackup -> TestTree
requestDeleteBackup = req
    "DeleteBackup"
    "fixture/DeleteBackup.yaml"

requestDescribeEvents :: DescribeEvents -> TestTree
requestDescribeEvents = req
    "DescribeEvents"
    "fixture/DescribeEvents.yaml"

requestDisassociateNode :: DisassociateNode -> TestTree
requestDisassociateNode = req
    "DisassociateNode"
    "fixture/DisassociateNode.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestCreateBackup :: CreateBackup -> TestTree
requestCreateBackup = req
    "CreateBackup"
    "fixture/CreateBackup.yaml"

requestUpdateServerEngineAttributes :: UpdateServerEngineAttributes -> TestTree
requestUpdateServerEngineAttributes = req
    "UpdateServerEngineAttributes"
    "fixture/UpdateServerEngineAttributes.yaml"

requestStartMaintenance :: StartMaintenance -> TestTree
requestStartMaintenance = req
    "StartMaintenance"
    "fixture/StartMaintenance.yaml"

requestDescribeBackups :: DescribeBackups -> TestTree
requestDescribeBackups = req
    "DescribeBackups"
    "fixture/DescribeBackups.yaml"

requestCreateServer :: CreateServer -> TestTree
requestCreateServer = req
    "CreateServer"
    "fixture/CreateServer.yaml"

requestRestoreServer :: RestoreServer -> TestTree
requestRestoreServer = req
    "RestoreServer"
    "fixture/RestoreServer.yaml"

requestDescribeNodeAssociationStatus :: DescribeNodeAssociationStatus -> TestTree
requestDescribeNodeAssociationStatus = req
    "DescribeNodeAssociationStatus"
    "fixture/DescribeNodeAssociationStatus.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource = req
    "TagResource"
    "fixture/TagResource.yaml"

requestDescribeAccountAttributes :: DescribeAccountAttributes -> TestTree
requestDescribeAccountAttributes = req
    "DescribeAccountAttributes"
    "fixture/DescribeAccountAttributes.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource = req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDescribeServers :: DescribeServers -> TestTree
requestDescribeServers = req
    "DescribeServers"
    "fixture/DescribeServers.yaml"

requestExportServerEngineAttribute :: ExportServerEngineAttribute -> TestTree
requestExportServerEngineAttribute = req
    "ExportServerEngineAttribute"
    "fixture/ExportServerEngineAttribute.yaml"

-- Responses

responseAssociateNode :: AssociateNodeResponse -> TestTree
responseAssociateNode = res
    "AssociateNodeResponse"
    "fixture/AssociateNodeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssociateNode)

responseUpdateServer :: UpdateServerResponse -> TestTree
responseUpdateServer = res
    "UpdateServerResponse"
    "fixture/UpdateServerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateServer)

responseDeleteServer :: DeleteServerResponse -> TestTree
responseDeleteServer = res
    "DeleteServerResponse"
    "fixture/DeleteServerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteServer)

responseDeleteBackup :: DeleteBackupResponse -> TestTree
responseDeleteBackup = res
    "DeleteBackupResponse"
    "fixture/DeleteBackupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteBackup)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents = res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeEvents)

responseDisassociateNode :: DisassociateNodeResponse -> TestTree
responseDisassociateNode = res
    "DisassociateNodeResponse"
    "fixture/DisassociateNodeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisassociateNode)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTagsForResource)

responseCreateBackup :: CreateBackupResponse -> TestTree
responseCreateBackup = res
    "CreateBackupResponse"
    "fixture/CreateBackupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateBackup)

responseUpdateServerEngineAttributes :: UpdateServerEngineAttributesResponse -> TestTree
responseUpdateServerEngineAttributes = res
    "UpdateServerEngineAttributesResponse"
    "fixture/UpdateServerEngineAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateServerEngineAttributes)

responseStartMaintenance :: StartMaintenanceResponse -> TestTree
responseStartMaintenance = res
    "StartMaintenanceResponse"
    "fixture/StartMaintenanceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartMaintenance)

responseDescribeBackups :: DescribeBackupsResponse -> TestTree
responseDescribeBackups = res
    "DescribeBackupsResponse"
    "fixture/DescribeBackupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeBackups)

responseCreateServer :: CreateServerResponse -> TestTree
responseCreateServer = res
    "CreateServerResponse"
    "fixture/CreateServerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateServer)

responseRestoreServer :: RestoreServerResponse -> TestTree
responseRestoreServer = res
    "RestoreServerResponse"
    "fixture/RestoreServerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RestoreServer)

responseDescribeNodeAssociationStatus :: DescribeNodeAssociationStatusResponse -> TestTree
responseDescribeNodeAssociationStatus = res
    "DescribeNodeAssociationStatusResponse"
    "fixture/DescribeNodeAssociationStatusResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeNodeAssociationStatus)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource = res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TagResource)

responseDescribeAccountAttributes :: DescribeAccountAttributesResponse -> TestTree
responseDescribeAccountAttributes = res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeAccountAttributes)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource = res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UntagResource)

responseDescribeServers :: DescribeServersResponse -> TestTree
responseDescribeServers = res
    "DescribeServersResponse"
    "fixture/DescribeServersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeServers)

responseExportServerEngineAttribute :: ExportServerEngineAttributeResponse -> TestTree
responseExportServerEngineAttribute = res
    "ExportServerEngineAttributeResponse"
    "fixture/ExportServerEngineAttributeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ExportServerEngineAttribute)

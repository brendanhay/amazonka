{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.OpsWorksCM
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.OpsWorksCM where

import Data.Proxy
import Network.AWS.OpsWorksCM
import Test.AWS.Fixture
import Test.AWS.OpsWorksCM.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDeleteBackup $
--             newDeleteBackup
--
--         , requestUpdateServer $
--             newUpdateServer
--
--         , requestDeleteServer $
--             newDeleteServer
--
--         , requestCreateServer $
--             newCreateServer
--
--         , requestDescribeAccountAttributes $
--             newDescribeAccountAttributes
--
--         , requestExportServerEngineAttribute $
--             newExportServerEngineAttribute
--
--         , requestDescribeServers $
--             newDescribeServers
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDescribeNodeAssociationStatus $
--             newDescribeNodeAssociationStatus
--
--         , requestTagResource $
--             newTagResource
--
--         , requestDisassociateNode $
--             newDisassociateNode
--
--         , requestDescribeEvents $
--             newDescribeEvents
--
--         , requestCreateBackup $
--             newCreateBackup
--
--         , requestAssociateNode $
--             newAssociateNode
--
--         , requestDescribeBackups $
--             newDescribeBackups
--
--         , requestUpdateServerEngineAttributes $
--             newUpdateServerEngineAttributes
--
--         , requestStartMaintenance $
--             newStartMaintenance
--
--         , requestRestoreServer $
--             newRestoreServer
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--           ]

--     , testGroup "response"
--         [ responseDeleteBackup $
--             newDeleteBackupResponse
--
--         , responseUpdateServer $
--             newUpdateServerResponse
--
--         , responseDeleteServer $
--             newDeleteServerResponse
--
--         , responseCreateServer $
--             newCreateServerResponse
--
--         , responseDescribeAccountAttributes $
--             newDescribeAccountAttributesResponse
--
--         , responseExportServerEngineAttribute $
--             newExportServerEngineAttributeResponse
--
--         , responseDescribeServers $
--             newDescribeServersResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDescribeNodeAssociationStatus $
--             newDescribeNodeAssociationStatusResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseDisassociateNode $
--             newDisassociateNodeResponse
--
--         , responseDescribeEvents $
--             newDescribeEventsResponse
--
--         , responseCreateBackup $
--             newCreateBackupResponse
--
--         , responseAssociateNode $
--             newAssociateNodeResponse
--
--         , responseDescribeBackups $
--             newDescribeBackupsResponse
--
--         , responseUpdateServerEngineAttributes $
--             newUpdateServerEngineAttributesResponse
--
--         , responseStartMaintenance $
--             newStartMaintenanceResponse
--
--         , responseRestoreServer $
--             newRestoreServerResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--           ]
--     ]

-- Requests

requestDeleteBackup :: DeleteBackup -> TestTree
requestDeleteBackup =
  req
    "DeleteBackup"
    "fixture/DeleteBackup.yaml"

requestUpdateServer :: UpdateServer -> TestTree
requestUpdateServer =
  req
    "UpdateServer"
    "fixture/UpdateServer.yaml"

requestDeleteServer :: DeleteServer -> TestTree
requestDeleteServer =
  req
    "DeleteServer"
    "fixture/DeleteServer.yaml"

requestCreateServer :: CreateServer -> TestTree
requestCreateServer =
  req
    "CreateServer"
    "fixture/CreateServer.yaml"

requestDescribeAccountAttributes :: DescribeAccountAttributes -> TestTree
requestDescribeAccountAttributes =
  req
    "DescribeAccountAttributes"
    "fixture/DescribeAccountAttributes.yaml"

requestExportServerEngineAttribute :: ExportServerEngineAttribute -> TestTree
requestExportServerEngineAttribute =
  req
    "ExportServerEngineAttribute"
    "fixture/ExportServerEngineAttribute.yaml"

requestDescribeServers :: DescribeServers -> TestTree
requestDescribeServers =
  req
    "DescribeServers"
    "fixture/DescribeServers.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDescribeNodeAssociationStatus :: DescribeNodeAssociationStatus -> TestTree
requestDescribeNodeAssociationStatus =
  req
    "DescribeNodeAssociationStatus"
    "fixture/DescribeNodeAssociationStatus.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestDisassociateNode :: DisassociateNode -> TestTree
requestDisassociateNode =
  req
    "DisassociateNode"
    "fixture/DisassociateNode.yaml"

requestDescribeEvents :: DescribeEvents -> TestTree
requestDescribeEvents =
  req
    "DescribeEvents"
    "fixture/DescribeEvents.yaml"

requestCreateBackup :: CreateBackup -> TestTree
requestCreateBackup =
  req
    "CreateBackup"
    "fixture/CreateBackup.yaml"

requestAssociateNode :: AssociateNode -> TestTree
requestAssociateNode =
  req
    "AssociateNode"
    "fixture/AssociateNode.yaml"

requestDescribeBackups :: DescribeBackups -> TestTree
requestDescribeBackups =
  req
    "DescribeBackups"
    "fixture/DescribeBackups.yaml"

requestUpdateServerEngineAttributes :: UpdateServerEngineAttributes -> TestTree
requestUpdateServerEngineAttributes =
  req
    "UpdateServerEngineAttributes"
    "fixture/UpdateServerEngineAttributes.yaml"

requestStartMaintenance :: StartMaintenance -> TestTree
requestStartMaintenance =
  req
    "StartMaintenance"
    "fixture/StartMaintenance.yaml"

requestRestoreServer :: RestoreServer -> TestTree
requestRestoreServer =
  req
    "RestoreServer"
    "fixture/RestoreServer.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

-- Responses

responseDeleteBackup :: DeleteBackupResponse -> TestTree
responseDeleteBackup =
  res
    "DeleteBackupResponse"
    "fixture/DeleteBackupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBackup)

responseUpdateServer :: UpdateServerResponse -> TestTree
responseUpdateServer =
  res
    "UpdateServerResponse"
    "fixture/UpdateServerResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateServer)

responseDeleteServer :: DeleteServerResponse -> TestTree
responseDeleteServer =
  res
    "DeleteServerResponse"
    "fixture/DeleteServerResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteServer)

responseCreateServer :: CreateServerResponse -> TestTree
responseCreateServer =
  res
    "CreateServerResponse"
    "fixture/CreateServerResponse.proto"
    defaultService
    (Proxy :: Proxy CreateServer)

responseDescribeAccountAttributes :: DescribeAccountAttributesResponse -> TestTree
responseDescribeAccountAttributes =
  res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAccountAttributes)

responseExportServerEngineAttribute :: ExportServerEngineAttributeResponse -> TestTree
responseExportServerEngineAttribute =
  res
    "ExportServerEngineAttributeResponse"
    "fixture/ExportServerEngineAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy ExportServerEngineAttribute)

responseDescribeServers :: DescribeServersResponse -> TestTree
responseDescribeServers =
  res
    "DescribeServersResponse"
    "fixture/DescribeServersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeServers)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseDescribeNodeAssociationStatus :: DescribeNodeAssociationStatusResponse -> TestTree
responseDescribeNodeAssociationStatus =
  res
    "DescribeNodeAssociationStatusResponse"
    "fixture/DescribeNodeAssociationStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeNodeAssociationStatus)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseDisassociateNode :: DisassociateNodeResponse -> TestTree
responseDisassociateNode =
  res
    "DisassociateNodeResponse"
    "fixture/DisassociateNodeResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateNode)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents =
  res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEvents)

responseCreateBackup :: CreateBackupResponse -> TestTree
responseCreateBackup =
  res
    "CreateBackupResponse"
    "fixture/CreateBackupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateBackup)

responseAssociateNode :: AssociateNodeResponse -> TestTree
responseAssociateNode =
  res
    "AssociateNodeResponse"
    "fixture/AssociateNodeResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateNode)

responseDescribeBackups :: DescribeBackupsResponse -> TestTree
responseDescribeBackups =
  res
    "DescribeBackupsResponse"
    "fixture/DescribeBackupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeBackups)

responseUpdateServerEngineAttributes :: UpdateServerEngineAttributesResponse -> TestTree
responseUpdateServerEngineAttributes =
  res
    "UpdateServerEngineAttributesResponse"
    "fixture/UpdateServerEngineAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateServerEngineAttributes)

responseStartMaintenance :: StartMaintenanceResponse -> TestTree
responseStartMaintenance =
  res
    "StartMaintenanceResponse"
    "fixture/StartMaintenanceResponse.proto"
    defaultService
    (Proxy :: Proxy StartMaintenance)

responseRestoreServer :: RestoreServerResponse -> TestTree
responseRestoreServer =
  res
    "RestoreServerResponse"
    "fixture/RestoreServerResponse.proto"
    defaultService
    (Proxy :: Proxy RestoreServer)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

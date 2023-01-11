{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.OpsWorksCM
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.OpsWorksCM where

import Amazonka.OpsWorksCM
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.OpsWorksCM.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateNode $
--             newAssociateNode
--
--         , requestCreateBackup $
--             newCreateBackup
--
--         , requestCreateServer $
--             newCreateServer
--
--         , requestDeleteBackup $
--             newDeleteBackup
--
--         , requestDeleteServer $
--             newDeleteServer
--
--         , requestDescribeAccountAttributes $
--             newDescribeAccountAttributes
--
--         , requestDescribeBackups $
--             newDescribeBackups
--
--         , requestDescribeEvents $
--             newDescribeEvents
--
--         , requestDescribeNodeAssociationStatus $
--             newDescribeNodeAssociationStatus
--
--         , requestDescribeServers $
--             newDescribeServers
--
--         , requestDisassociateNode $
--             newDisassociateNode
--
--         , requestExportServerEngineAttribute $
--             newExportServerEngineAttribute
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestRestoreServer $
--             newRestoreServer
--
--         , requestStartMaintenance $
--             newStartMaintenance
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateServer $
--             newUpdateServer
--
--         , requestUpdateServerEngineAttributes $
--             newUpdateServerEngineAttributes
--
--           ]

--     , testGroup "response"
--         [ responseAssociateNode $
--             newAssociateNodeResponse
--
--         , responseCreateBackup $
--             newCreateBackupResponse
--
--         , responseCreateServer $
--             newCreateServerResponse
--
--         , responseDeleteBackup $
--             newDeleteBackupResponse
--
--         , responseDeleteServer $
--             newDeleteServerResponse
--
--         , responseDescribeAccountAttributes $
--             newDescribeAccountAttributesResponse
--
--         , responseDescribeBackups $
--             newDescribeBackupsResponse
--
--         , responseDescribeEvents $
--             newDescribeEventsResponse
--
--         , responseDescribeNodeAssociationStatus $
--             newDescribeNodeAssociationStatusResponse
--
--         , responseDescribeServers $
--             newDescribeServersResponse
--
--         , responseDisassociateNode $
--             newDisassociateNodeResponse
--
--         , responseExportServerEngineAttribute $
--             newExportServerEngineAttributeResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseRestoreServer $
--             newRestoreServerResponse
--
--         , responseStartMaintenance $
--             newStartMaintenanceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateServer $
--             newUpdateServerResponse
--
--         , responseUpdateServerEngineAttributes $
--             newUpdateServerEngineAttributesResponse
--
--           ]
--     ]

-- Requests

requestAssociateNode :: AssociateNode -> TestTree
requestAssociateNode =
  req
    "AssociateNode"
    "fixture/AssociateNode.yaml"

requestCreateBackup :: CreateBackup -> TestTree
requestCreateBackup =
  req
    "CreateBackup"
    "fixture/CreateBackup.yaml"

requestCreateServer :: CreateServer -> TestTree
requestCreateServer =
  req
    "CreateServer"
    "fixture/CreateServer.yaml"

requestDeleteBackup :: DeleteBackup -> TestTree
requestDeleteBackup =
  req
    "DeleteBackup"
    "fixture/DeleteBackup.yaml"

requestDeleteServer :: DeleteServer -> TestTree
requestDeleteServer =
  req
    "DeleteServer"
    "fixture/DeleteServer.yaml"

requestDescribeAccountAttributes :: DescribeAccountAttributes -> TestTree
requestDescribeAccountAttributes =
  req
    "DescribeAccountAttributes"
    "fixture/DescribeAccountAttributes.yaml"

requestDescribeBackups :: DescribeBackups -> TestTree
requestDescribeBackups =
  req
    "DescribeBackups"
    "fixture/DescribeBackups.yaml"

requestDescribeEvents :: DescribeEvents -> TestTree
requestDescribeEvents =
  req
    "DescribeEvents"
    "fixture/DescribeEvents.yaml"

requestDescribeNodeAssociationStatus :: DescribeNodeAssociationStatus -> TestTree
requestDescribeNodeAssociationStatus =
  req
    "DescribeNodeAssociationStatus"
    "fixture/DescribeNodeAssociationStatus.yaml"

requestDescribeServers :: DescribeServers -> TestTree
requestDescribeServers =
  req
    "DescribeServers"
    "fixture/DescribeServers.yaml"

requestDisassociateNode :: DisassociateNode -> TestTree
requestDisassociateNode =
  req
    "DisassociateNode"
    "fixture/DisassociateNode.yaml"

requestExportServerEngineAttribute :: ExportServerEngineAttribute -> TestTree
requestExportServerEngineAttribute =
  req
    "ExportServerEngineAttribute"
    "fixture/ExportServerEngineAttribute.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestRestoreServer :: RestoreServer -> TestTree
requestRestoreServer =
  req
    "RestoreServer"
    "fixture/RestoreServer.yaml"

requestStartMaintenance :: StartMaintenance -> TestTree
requestStartMaintenance =
  req
    "StartMaintenance"
    "fixture/StartMaintenance.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateServer :: UpdateServer -> TestTree
requestUpdateServer =
  req
    "UpdateServer"
    "fixture/UpdateServer.yaml"

requestUpdateServerEngineAttributes :: UpdateServerEngineAttributes -> TestTree
requestUpdateServerEngineAttributes =
  req
    "UpdateServerEngineAttributes"
    "fixture/UpdateServerEngineAttributes.yaml"

-- Responses

responseAssociateNode :: AssociateNodeResponse -> TestTree
responseAssociateNode =
  res
    "AssociateNodeResponse"
    "fixture/AssociateNodeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateNode)

responseCreateBackup :: CreateBackupResponse -> TestTree
responseCreateBackup =
  res
    "CreateBackupResponse"
    "fixture/CreateBackupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBackup)

responseCreateServer :: CreateServerResponse -> TestTree
responseCreateServer =
  res
    "CreateServerResponse"
    "fixture/CreateServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateServer)

responseDeleteBackup :: DeleteBackupResponse -> TestTree
responseDeleteBackup =
  res
    "DeleteBackupResponse"
    "fixture/DeleteBackupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBackup)

responseDeleteServer :: DeleteServerResponse -> TestTree
responseDeleteServer =
  res
    "DeleteServerResponse"
    "fixture/DeleteServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteServer)

responseDescribeAccountAttributes :: DescribeAccountAttributesResponse -> TestTree
responseDescribeAccountAttributes =
  res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccountAttributes)

responseDescribeBackups :: DescribeBackupsResponse -> TestTree
responseDescribeBackups =
  res
    "DescribeBackupsResponse"
    "fixture/DescribeBackupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBackups)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents =
  res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEvents)

responseDescribeNodeAssociationStatus :: DescribeNodeAssociationStatusResponse -> TestTree
responseDescribeNodeAssociationStatus =
  res
    "DescribeNodeAssociationStatusResponse"
    "fixture/DescribeNodeAssociationStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNodeAssociationStatus)

responseDescribeServers :: DescribeServersResponse -> TestTree
responseDescribeServers =
  res
    "DescribeServersResponse"
    "fixture/DescribeServersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeServers)

responseDisassociateNode :: DisassociateNodeResponse -> TestTree
responseDisassociateNode =
  res
    "DisassociateNodeResponse"
    "fixture/DisassociateNodeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateNode)

responseExportServerEngineAttribute :: ExportServerEngineAttributeResponse -> TestTree
responseExportServerEngineAttribute =
  res
    "ExportServerEngineAttributeResponse"
    "fixture/ExportServerEngineAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExportServerEngineAttribute)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseRestoreServer :: RestoreServerResponse -> TestTree
responseRestoreServer =
  res
    "RestoreServerResponse"
    "fixture/RestoreServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreServer)

responseStartMaintenance :: StartMaintenanceResponse -> TestTree
responseStartMaintenance =
  res
    "StartMaintenanceResponse"
    "fixture/StartMaintenanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartMaintenance)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateServer :: UpdateServerResponse -> TestTree
responseUpdateServer =
  res
    "UpdateServerResponse"
    "fixture/UpdateServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateServer)

responseUpdateServerEngineAttributes :: UpdateServerEngineAttributesResponse -> TestTree
responseUpdateServerEngineAttributes =
  res
    "UpdateServerEngineAttributesResponse"
    "fixture/UpdateServerEngineAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateServerEngineAttributes)

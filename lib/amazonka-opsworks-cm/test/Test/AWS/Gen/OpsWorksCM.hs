{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.OpsWorksCM
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--         [ requestAssociateNode $
--             associateNode
--
--         , requestUpdateServer $
--             updateServer
--
--         , requestDeleteServer $
--             deleteServer
--
--         , requestDeleteBackup $
--             deleteBackup
--
--         , requestDescribeEvents $
--             describeEvents
--
--         , requestDisassociateNode $
--             disassociateNode
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestCreateBackup $
--             createBackup
--
--         , requestUpdateServerEngineAttributes $
--             updateServerEngineAttributes
--
--         , requestStartMaintenance $
--             startMaintenance
--
--         , requestDescribeBackups $
--             describeBackups
--
--         , requestCreateServer $
--             createServer
--
--         , requestRestoreServer $
--             restoreServer
--
--         , requestDescribeNodeAssociationStatus $
--             describeNodeAssociationStatus
--
--         , requestTagResource $
--             tagResource
--
--         , requestDescribeAccountAttributes $
--             describeAccountAttributes
--
--         , requestUntagResource $
--             untagResource
--
--         , requestDescribeServers $
--             describeServers
--
--         , requestExportServerEngineAttribute $
--             exportServerEngineAttribute
--
--           ]

--     , testGroup "response"
--         [ responseAssociateNode $
--             associateNodeResponse
--
--         , responseUpdateServer $
--             updateServerResponse
--
--         , responseDeleteServer $
--             deleteServerResponse
--
--         , responseDeleteBackup $
--             deleteBackupResponse
--
--         , responseDescribeEvents $
--             describeEventsResponse
--
--         , responseDisassociateNode $
--             disassociateNodeResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responseCreateBackup $
--             createBackupResponse
--
--         , responseUpdateServerEngineAttributes $
--             updateServerEngineAttributesResponse
--
--         , responseStartMaintenance $
--             startMaintenanceResponse
--
--         , responseDescribeBackups $
--             describeBackupsResponse
--
--         , responseCreateServer $
--             createServerResponse
--
--         , responseRestoreServer $
--             restoreServerResponse
--
--         , responseDescribeNodeAssociationStatus $
--             describeNodeAssociationStatusResponse
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responseDescribeAccountAttributes $
--             describeAccountAttributesResponse
--
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responseDescribeServers $
--             describeServersResponse
--
--         , responseExportServerEngineAttribute $
--             exportServerEngineAttributeResponse
--
--           ]
--     ]

-- Requests

requestAssociateNode :: AssociateNode -> TestTree
requestAssociateNode =
  req
    "AssociateNode"
    "fixture/AssociateNode.yaml"

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

requestDeleteBackup :: DeleteBackup -> TestTree
requestDeleteBackup =
  req
    "DeleteBackup"
    "fixture/DeleteBackup.yaml"

requestDescribeEvents :: DescribeEvents -> TestTree
requestDescribeEvents =
  req
    "DescribeEvents"
    "fixture/DescribeEvents.yaml"

requestDisassociateNode :: DisassociateNode -> TestTree
requestDisassociateNode =
  req
    "DisassociateNode"
    "fixture/DisassociateNode.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestCreateBackup :: CreateBackup -> TestTree
requestCreateBackup =
  req
    "CreateBackup"
    "fixture/CreateBackup.yaml"

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

requestDescribeBackups :: DescribeBackups -> TestTree
requestDescribeBackups =
  req
    "DescribeBackups"
    "fixture/DescribeBackups.yaml"

requestCreateServer :: CreateServer -> TestTree
requestCreateServer =
  req
    "CreateServer"
    "fixture/CreateServer.yaml"

requestRestoreServer :: RestoreServer -> TestTree
requestRestoreServer =
  req
    "RestoreServer"
    "fixture/RestoreServer.yaml"

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

requestDescribeAccountAttributes :: DescribeAccountAttributes -> TestTree
requestDescribeAccountAttributes =
  req
    "DescribeAccountAttributes"
    "fixture/DescribeAccountAttributes.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDescribeServers :: DescribeServers -> TestTree
requestDescribeServers =
  req
    "DescribeServers"
    "fixture/DescribeServers.yaml"

requestExportServerEngineAttribute :: ExportServerEngineAttribute -> TestTree
requestExportServerEngineAttribute =
  req
    "ExportServerEngineAttribute"
    "fixture/ExportServerEngineAttribute.yaml"

-- Responses

responseAssociateNode :: AssociateNodeResponse -> TestTree
responseAssociateNode =
  res
    "AssociateNodeResponse"
    "fixture/AssociateNodeResponse.proto"
    opsWorksCM
    (Proxy :: Proxy AssociateNode)

responseUpdateServer :: UpdateServerResponse -> TestTree
responseUpdateServer =
  res
    "UpdateServerResponse"
    "fixture/UpdateServerResponse.proto"
    opsWorksCM
    (Proxy :: Proxy UpdateServer)

responseDeleteServer :: DeleteServerResponse -> TestTree
responseDeleteServer =
  res
    "DeleteServerResponse"
    "fixture/DeleteServerResponse.proto"
    opsWorksCM
    (Proxy :: Proxy DeleteServer)

responseDeleteBackup :: DeleteBackupResponse -> TestTree
responseDeleteBackup =
  res
    "DeleteBackupResponse"
    "fixture/DeleteBackupResponse.proto"
    opsWorksCM
    (Proxy :: Proxy DeleteBackup)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents =
  res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    opsWorksCM
    (Proxy :: Proxy DescribeEvents)

responseDisassociateNode :: DisassociateNodeResponse -> TestTree
responseDisassociateNode =
  res
    "DisassociateNodeResponse"
    "fixture/DisassociateNodeResponse.proto"
    opsWorksCM
    (Proxy :: Proxy DisassociateNode)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    opsWorksCM
    (Proxy :: Proxy ListTagsForResource)

responseCreateBackup :: CreateBackupResponse -> TestTree
responseCreateBackup =
  res
    "CreateBackupResponse"
    "fixture/CreateBackupResponse.proto"
    opsWorksCM
    (Proxy :: Proxy CreateBackup)

responseUpdateServerEngineAttributes :: UpdateServerEngineAttributesResponse -> TestTree
responseUpdateServerEngineAttributes =
  res
    "UpdateServerEngineAttributesResponse"
    "fixture/UpdateServerEngineAttributesResponse.proto"
    opsWorksCM
    (Proxy :: Proxy UpdateServerEngineAttributes)

responseStartMaintenance :: StartMaintenanceResponse -> TestTree
responseStartMaintenance =
  res
    "StartMaintenanceResponse"
    "fixture/StartMaintenanceResponse.proto"
    opsWorksCM
    (Proxy :: Proxy StartMaintenance)

responseDescribeBackups :: DescribeBackupsResponse -> TestTree
responseDescribeBackups =
  res
    "DescribeBackupsResponse"
    "fixture/DescribeBackupsResponse.proto"
    opsWorksCM
    (Proxy :: Proxy DescribeBackups)

responseCreateServer :: CreateServerResponse -> TestTree
responseCreateServer =
  res
    "CreateServerResponse"
    "fixture/CreateServerResponse.proto"
    opsWorksCM
    (Proxy :: Proxy CreateServer)

responseRestoreServer :: RestoreServerResponse -> TestTree
responseRestoreServer =
  res
    "RestoreServerResponse"
    "fixture/RestoreServerResponse.proto"
    opsWorksCM
    (Proxy :: Proxy RestoreServer)

responseDescribeNodeAssociationStatus :: DescribeNodeAssociationStatusResponse -> TestTree
responseDescribeNodeAssociationStatus =
  res
    "DescribeNodeAssociationStatusResponse"
    "fixture/DescribeNodeAssociationStatusResponse.proto"
    opsWorksCM
    (Proxy :: Proxy DescribeNodeAssociationStatus)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    opsWorksCM
    (Proxy :: Proxy TagResource)

responseDescribeAccountAttributes :: DescribeAccountAttributesResponse -> TestTree
responseDescribeAccountAttributes =
  res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse.proto"
    opsWorksCM
    (Proxy :: Proxy DescribeAccountAttributes)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    opsWorksCM
    (Proxy :: Proxy UntagResource)

responseDescribeServers :: DescribeServersResponse -> TestTree
responseDescribeServers =
  res
    "DescribeServersResponse"
    "fixture/DescribeServersResponse.proto"
    opsWorksCM
    (Proxy :: Proxy DescribeServers)

responseExportServerEngineAttribute :: ExportServerEngineAttributeResponse -> TestTree
responseExportServerEngineAttribute =
  res
    "ExportServerEngineAttributeResponse"
    "fixture/ExportServerEngineAttributeResponse.proto"
    opsWorksCM
    (Proxy :: Proxy ExportServerEngineAttribute)

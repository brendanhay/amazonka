{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Lightsail
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Lightsail where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.Lightsail
import Test.AWS.Lightsail.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCloseInstancePublicPorts $
--             closeInstancePublicPorts
--
--         , requestAllocateStaticIP $
--             allocateStaticIP
--
--         , requestDeleteKeyPair $
--             deleteKeyPair
--
--         , requestDeleteInstanceSnapshot $
--             deleteInstanceSnapshot
--
--         , requestGetInstances $
--             getInstances
--
--         , requestGetInstance $
--             getInstance
--
--         , requestAttachStaticIP $
--             attachStaticIP
--
--         , requestDownloadDefaultKeyPair $
--             downloadDefaultKeyPair
--
--         , requestGetDomains $
--             getDomains
--
--         , requestCreateDomainEntry $
--             createDomainEntry
--
--         , requestImportKeyPair $
--             importKeyPair
--
--         , requestGetInstanceSnapshots $
--             getInstanceSnapshots
--
--         , requestReleaseStaticIP $
--             releaseStaticIP
--
--         , requestDeleteInstance $
--             deleteInstance
--
--         , requestRebootInstance $
--             rebootInstance
--
--         , requestGetInstanceSnapshot $
--             getInstanceSnapshot
--
--         , requestGetDomain $
--             getDomain
--
--         , requestGetActiveNames $
--             getActiveNames
--
--         , requestGetInstanceAccessDetails $
--             getInstanceAccessDetails
--
--         , requestStopInstance $
--             stopInstance
--
--         , requestCreateInstanceSnapshot $
--             createInstanceSnapshot
--
--         , requestIsVPCPeered $
--             isVPCPeered
--
--         , requestGetStaticIPs $
--             getStaticIPs
--
--         , requestUnpeerVPC $
--             unpeerVPC
--
--         , requestCreateInstancesFromSnapshot $
--             createInstancesFromSnapshot
--
--         , requestCreateDomain $
--             createDomain
--
--         , requestPeerVPC $
--             peerVPC
--
--         , requestGetStaticIP $
--             getStaticIP
--
--         , requestGetBlueprints $
--             getBlueprints
--
--         , requestGetInstancePortStates $
--             getInstancePortStates
--
--         , requestDeleteDomainEntry $
--             deleteDomainEntry
--
--         , requestUpdateDomainEntry $
--             updateDomainEntry
--
--         , requestGetRegions $
--             getRegions
--
--         , requestGetInstanceState $
--             getInstanceState
--
--         , requestGetKeyPairs $
--             getKeyPairs
--
--         , requestGetOperations $
--             getOperations
--
--         , requestGetOperation $
--             getOperation
--
--         , requestGetInstanceMetricData $
--             getInstanceMetricData
--
--         , requestGetKeyPair $
--             getKeyPair
--
--         , requestPutInstancePublicPorts $
--             putInstancePublicPorts
--
--         , requestDetachStaticIP $
--             detachStaticIP
--
--         , requestCreateInstances $
--             createInstances
--
--         , requestOpenInstancePublicPorts $
--             openInstancePublicPorts
--
--         , requestGetBundles $
--             getBundles
--
--         , requestDeleteDomain $
--             deleteDomain
--
--         , requestGetOperationsForResource $
--             getOperationsForResource
--
--         , requestCreateKeyPair $
--             createKeyPair
--
--         , requestStartInstance $
--             startInstance
--
--           ]

--     , testGroup "response"
--         [ responseCloseInstancePublicPorts $
--             closeInstancePublicPortsResponse
--
--         , responseAllocateStaticIP $
--             allocateStaticIPResponse
--
--         , responseDeleteKeyPair $
--             deleteKeyPairResponse
--
--         , responseDeleteInstanceSnapshot $
--             deleteInstanceSnapshotResponse
--
--         , responseGetInstances $
--             getInstancesResponse
--
--         , responseGetInstance $
--             getInstanceResponse
--
--         , responseAttachStaticIP $
--             attachStaticIPResponse
--
--         , responseDownloadDefaultKeyPair $
--             downloadDefaultKeyPairResponse
--
--         , responseGetDomains $
--             getDomainsResponse
--
--         , responseCreateDomainEntry $
--             createDomainEntryResponse
--
--         , responseImportKeyPair $
--             importKeyPairResponse
--
--         , responseGetInstanceSnapshots $
--             getInstanceSnapshotsResponse
--
--         , responseReleaseStaticIP $
--             releaseStaticIPResponse
--
--         , responseDeleteInstance $
--             deleteInstanceResponse
--
--         , responseRebootInstance $
--             rebootInstanceResponse
--
--         , responseGetInstanceSnapshot $
--             getInstanceSnapshotResponse
--
--         , responseGetDomain $
--             getDomainResponse
--
--         , responseGetActiveNames $
--             getActiveNamesResponse
--
--         , responseGetInstanceAccessDetails $
--             getInstanceAccessDetailsResponse
--
--         , responseStopInstance $
--             stopInstanceResponse
--
--         , responseCreateInstanceSnapshot $
--             createInstanceSnapshotResponse
--
--         , responseIsVPCPeered $
--             isVPCPeeredResponse
--
--         , responseGetStaticIPs $
--             getStaticIPsResponse
--
--         , responseUnpeerVPC $
--             unpeerVPCResponse
--
--         , responseCreateInstancesFromSnapshot $
--             createInstancesFromSnapshotResponse
--
--         , responseCreateDomain $
--             createDomainResponse
--
--         , responsePeerVPC $
--             peerVPCResponse
--
--         , responseGetStaticIP $
--             getStaticIPResponse
--
--         , responseGetBlueprints $
--             getBlueprintsResponse
--
--         , responseGetInstancePortStates $
--             getInstancePortStatesResponse
--
--         , responseDeleteDomainEntry $
--             deleteDomainEntryResponse
--
--         , responseUpdateDomainEntry $
--             updateDomainEntryResponse
--
--         , responseGetRegions $
--             getRegionsResponse
--
--         , responseGetInstanceState $
--             getInstanceStateResponse
--
--         , responseGetKeyPairs $
--             getKeyPairsResponse
--
--         , responseGetOperations $
--             getOperationsResponse
--
--         , responseGetOperation $
--             getOperationResponse
--
--         , responseGetInstanceMetricData $
--             getInstanceMetricDataResponse
--
--         , responseGetKeyPair $
--             getKeyPairResponse
--
--         , responsePutInstancePublicPorts $
--             putInstancePublicPortsResponse
--
--         , responseDetachStaticIP $
--             detachStaticIPResponse
--
--         , responseCreateInstances $
--             createInstancesResponse
--
--         , responseOpenInstancePublicPorts $
--             openInstancePublicPortsResponse
--
--         , responseGetBundles $
--             getBundlesResponse
--
--         , responseDeleteDomain $
--             deleteDomainResponse
--
--         , responseGetOperationsForResource $
--             getOperationsForResourceResponse
--
--         , responseCreateKeyPair $
--             createKeyPairResponse
--
--         , responseStartInstance $
--             startInstanceResponse
--
--           ]
--     ]

-- Requests

requestCloseInstancePublicPorts :: CloseInstancePublicPorts -> TestTree
requestCloseInstancePublicPorts = req
    "CloseInstancePublicPorts"
    "fixture/CloseInstancePublicPorts.yaml"

requestAllocateStaticIP :: AllocateStaticIP -> TestTree
requestAllocateStaticIP = req
    "AllocateStaticIP"
    "fixture/AllocateStaticIP.yaml"

requestDeleteKeyPair :: DeleteKeyPair -> TestTree
requestDeleteKeyPair = req
    "DeleteKeyPair"
    "fixture/DeleteKeyPair.yaml"

requestDeleteInstanceSnapshot :: DeleteInstanceSnapshot -> TestTree
requestDeleteInstanceSnapshot = req
    "DeleteInstanceSnapshot"
    "fixture/DeleteInstanceSnapshot.yaml"

requestGetInstances :: GetInstances -> TestTree
requestGetInstances = req
    "GetInstances"
    "fixture/GetInstances.yaml"

requestGetInstance :: GetInstance -> TestTree
requestGetInstance = req
    "GetInstance"
    "fixture/GetInstance.yaml"

requestAttachStaticIP :: AttachStaticIP -> TestTree
requestAttachStaticIP = req
    "AttachStaticIP"
    "fixture/AttachStaticIP.yaml"

requestDownloadDefaultKeyPair :: DownloadDefaultKeyPair -> TestTree
requestDownloadDefaultKeyPair = req
    "DownloadDefaultKeyPair"
    "fixture/DownloadDefaultKeyPair.yaml"

requestGetDomains :: GetDomains -> TestTree
requestGetDomains = req
    "GetDomains"
    "fixture/GetDomains.yaml"

requestCreateDomainEntry :: CreateDomainEntry -> TestTree
requestCreateDomainEntry = req
    "CreateDomainEntry"
    "fixture/CreateDomainEntry.yaml"

requestImportKeyPair :: ImportKeyPair -> TestTree
requestImportKeyPair = req
    "ImportKeyPair"
    "fixture/ImportKeyPair.yaml"

requestGetInstanceSnapshots :: GetInstanceSnapshots -> TestTree
requestGetInstanceSnapshots = req
    "GetInstanceSnapshots"
    "fixture/GetInstanceSnapshots.yaml"

requestReleaseStaticIP :: ReleaseStaticIP -> TestTree
requestReleaseStaticIP = req
    "ReleaseStaticIP"
    "fixture/ReleaseStaticIP.yaml"

requestDeleteInstance :: DeleteInstance -> TestTree
requestDeleteInstance = req
    "DeleteInstance"
    "fixture/DeleteInstance.yaml"

requestRebootInstance :: RebootInstance -> TestTree
requestRebootInstance = req
    "RebootInstance"
    "fixture/RebootInstance.yaml"

requestGetInstanceSnapshot :: GetInstanceSnapshot -> TestTree
requestGetInstanceSnapshot = req
    "GetInstanceSnapshot"
    "fixture/GetInstanceSnapshot.yaml"

requestGetDomain :: GetDomain -> TestTree
requestGetDomain = req
    "GetDomain"
    "fixture/GetDomain.yaml"

requestGetActiveNames :: GetActiveNames -> TestTree
requestGetActiveNames = req
    "GetActiveNames"
    "fixture/GetActiveNames.yaml"

requestGetInstanceAccessDetails :: GetInstanceAccessDetails -> TestTree
requestGetInstanceAccessDetails = req
    "GetInstanceAccessDetails"
    "fixture/GetInstanceAccessDetails.yaml"

requestStopInstance :: StopInstance -> TestTree
requestStopInstance = req
    "StopInstance"
    "fixture/StopInstance.yaml"

requestCreateInstanceSnapshot :: CreateInstanceSnapshot -> TestTree
requestCreateInstanceSnapshot = req
    "CreateInstanceSnapshot"
    "fixture/CreateInstanceSnapshot.yaml"

requestIsVPCPeered :: IsVPCPeered -> TestTree
requestIsVPCPeered = req
    "IsVPCPeered"
    "fixture/IsVPCPeered.yaml"

requestGetStaticIPs :: GetStaticIPs -> TestTree
requestGetStaticIPs = req
    "GetStaticIPs"
    "fixture/GetStaticIPs.yaml"

requestUnpeerVPC :: UnpeerVPC -> TestTree
requestUnpeerVPC = req
    "UnpeerVPC"
    "fixture/UnpeerVPC.yaml"

requestCreateInstancesFromSnapshot :: CreateInstancesFromSnapshot -> TestTree
requestCreateInstancesFromSnapshot = req
    "CreateInstancesFromSnapshot"
    "fixture/CreateInstancesFromSnapshot.yaml"

requestCreateDomain :: CreateDomain -> TestTree
requestCreateDomain = req
    "CreateDomain"
    "fixture/CreateDomain.yaml"

requestPeerVPC :: PeerVPC -> TestTree
requestPeerVPC = req
    "PeerVPC"
    "fixture/PeerVPC.yaml"

requestGetStaticIP :: GetStaticIP -> TestTree
requestGetStaticIP = req
    "GetStaticIP"
    "fixture/GetStaticIP.yaml"

requestGetBlueprints :: GetBlueprints -> TestTree
requestGetBlueprints = req
    "GetBlueprints"
    "fixture/GetBlueprints.yaml"

requestGetInstancePortStates :: GetInstancePortStates -> TestTree
requestGetInstancePortStates = req
    "GetInstancePortStates"
    "fixture/GetInstancePortStates.yaml"

requestDeleteDomainEntry :: DeleteDomainEntry -> TestTree
requestDeleteDomainEntry = req
    "DeleteDomainEntry"
    "fixture/DeleteDomainEntry.yaml"

requestUpdateDomainEntry :: UpdateDomainEntry -> TestTree
requestUpdateDomainEntry = req
    "UpdateDomainEntry"
    "fixture/UpdateDomainEntry.yaml"

requestGetRegions :: GetRegions -> TestTree
requestGetRegions = req
    "GetRegions"
    "fixture/GetRegions.yaml"

requestGetInstanceState :: GetInstanceState -> TestTree
requestGetInstanceState = req
    "GetInstanceState"
    "fixture/GetInstanceState.yaml"

requestGetKeyPairs :: GetKeyPairs -> TestTree
requestGetKeyPairs = req
    "GetKeyPairs"
    "fixture/GetKeyPairs.yaml"

requestGetOperations :: GetOperations -> TestTree
requestGetOperations = req
    "GetOperations"
    "fixture/GetOperations.yaml"

requestGetOperation :: GetOperation -> TestTree
requestGetOperation = req
    "GetOperation"
    "fixture/GetOperation.yaml"

requestGetInstanceMetricData :: GetInstanceMetricData -> TestTree
requestGetInstanceMetricData = req
    "GetInstanceMetricData"
    "fixture/GetInstanceMetricData.yaml"

requestGetKeyPair :: GetKeyPair -> TestTree
requestGetKeyPair = req
    "GetKeyPair"
    "fixture/GetKeyPair.yaml"

requestPutInstancePublicPorts :: PutInstancePublicPorts -> TestTree
requestPutInstancePublicPorts = req
    "PutInstancePublicPorts"
    "fixture/PutInstancePublicPorts.yaml"

requestDetachStaticIP :: DetachStaticIP -> TestTree
requestDetachStaticIP = req
    "DetachStaticIP"
    "fixture/DetachStaticIP.yaml"

requestCreateInstances :: CreateInstances -> TestTree
requestCreateInstances = req
    "CreateInstances"
    "fixture/CreateInstances.yaml"

requestOpenInstancePublicPorts :: OpenInstancePublicPorts -> TestTree
requestOpenInstancePublicPorts = req
    "OpenInstancePublicPorts"
    "fixture/OpenInstancePublicPorts.yaml"

requestGetBundles :: GetBundles -> TestTree
requestGetBundles = req
    "GetBundles"
    "fixture/GetBundles.yaml"

requestDeleteDomain :: DeleteDomain -> TestTree
requestDeleteDomain = req
    "DeleteDomain"
    "fixture/DeleteDomain.yaml"

requestGetOperationsForResource :: GetOperationsForResource -> TestTree
requestGetOperationsForResource = req
    "GetOperationsForResource"
    "fixture/GetOperationsForResource.yaml"

requestCreateKeyPair :: CreateKeyPair -> TestTree
requestCreateKeyPair = req
    "CreateKeyPair"
    "fixture/CreateKeyPair.yaml"

requestStartInstance :: StartInstance -> TestTree
requestStartInstance = req
    "StartInstance"
    "fixture/StartInstance.yaml"

-- Responses

responseCloseInstancePublicPorts :: CloseInstancePublicPortsResponse -> TestTree
responseCloseInstancePublicPorts = res
    "CloseInstancePublicPortsResponse"
    "fixture/CloseInstancePublicPortsResponse.proto"
    lightsail
    (Proxy :: Proxy CloseInstancePublicPorts)

responseAllocateStaticIP :: AllocateStaticIPResponse -> TestTree
responseAllocateStaticIP = res
    "AllocateStaticIPResponse"
    "fixture/AllocateStaticIPResponse.proto"
    lightsail
    (Proxy :: Proxy AllocateStaticIP)

responseDeleteKeyPair :: DeleteKeyPairResponse -> TestTree
responseDeleteKeyPair = res
    "DeleteKeyPairResponse"
    "fixture/DeleteKeyPairResponse.proto"
    lightsail
    (Proxy :: Proxy DeleteKeyPair)

responseDeleteInstanceSnapshot :: DeleteInstanceSnapshotResponse -> TestTree
responseDeleteInstanceSnapshot = res
    "DeleteInstanceSnapshotResponse"
    "fixture/DeleteInstanceSnapshotResponse.proto"
    lightsail
    (Proxy :: Proxy DeleteInstanceSnapshot)

responseGetInstances :: GetInstancesResponse -> TestTree
responseGetInstances = res
    "GetInstancesResponse"
    "fixture/GetInstancesResponse.proto"
    lightsail
    (Proxy :: Proxy GetInstances)

responseGetInstance :: GetInstanceResponse -> TestTree
responseGetInstance = res
    "GetInstanceResponse"
    "fixture/GetInstanceResponse.proto"
    lightsail
    (Proxy :: Proxy GetInstance)

responseAttachStaticIP :: AttachStaticIPResponse -> TestTree
responseAttachStaticIP = res
    "AttachStaticIPResponse"
    "fixture/AttachStaticIPResponse.proto"
    lightsail
    (Proxy :: Proxy AttachStaticIP)

responseDownloadDefaultKeyPair :: DownloadDefaultKeyPairResponse -> TestTree
responseDownloadDefaultKeyPair = res
    "DownloadDefaultKeyPairResponse"
    "fixture/DownloadDefaultKeyPairResponse.proto"
    lightsail
    (Proxy :: Proxy DownloadDefaultKeyPair)

responseGetDomains :: GetDomainsResponse -> TestTree
responseGetDomains = res
    "GetDomainsResponse"
    "fixture/GetDomainsResponse.proto"
    lightsail
    (Proxy :: Proxy GetDomains)

responseCreateDomainEntry :: CreateDomainEntryResponse -> TestTree
responseCreateDomainEntry = res
    "CreateDomainEntryResponse"
    "fixture/CreateDomainEntryResponse.proto"
    lightsail
    (Proxy :: Proxy CreateDomainEntry)

responseImportKeyPair :: ImportKeyPairResponse -> TestTree
responseImportKeyPair = res
    "ImportKeyPairResponse"
    "fixture/ImportKeyPairResponse.proto"
    lightsail
    (Proxy :: Proxy ImportKeyPair)

responseGetInstanceSnapshots :: GetInstanceSnapshotsResponse -> TestTree
responseGetInstanceSnapshots = res
    "GetInstanceSnapshotsResponse"
    "fixture/GetInstanceSnapshotsResponse.proto"
    lightsail
    (Proxy :: Proxy GetInstanceSnapshots)

responseReleaseStaticIP :: ReleaseStaticIPResponse -> TestTree
responseReleaseStaticIP = res
    "ReleaseStaticIPResponse"
    "fixture/ReleaseStaticIPResponse.proto"
    lightsail
    (Proxy :: Proxy ReleaseStaticIP)

responseDeleteInstance :: DeleteInstanceResponse -> TestTree
responseDeleteInstance = res
    "DeleteInstanceResponse"
    "fixture/DeleteInstanceResponse.proto"
    lightsail
    (Proxy :: Proxy DeleteInstance)

responseRebootInstance :: RebootInstanceResponse -> TestTree
responseRebootInstance = res
    "RebootInstanceResponse"
    "fixture/RebootInstanceResponse.proto"
    lightsail
    (Proxy :: Proxy RebootInstance)

responseGetInstanceSnapshot :: GetInstanceSnapshotResponse -> TestTree
responseGetInstanceSnapshot = res
    "GetInstanceSnapshotResponse"
    "fixture/GetInstanceSnapshotResponse.proto"
    lightsail
    (Proxy :: Proxy GetInstanceSnapshot)

responseGetDomain :: GetDomainResponse -> TestTree
responseGetDomain = res
    "GetDomainResponse"
    "fixture/GetDomainResponse.proto"
    lightsail
    (Proxy :: Proxy GetDomain)

responseGetActiveNames :: GetActiveNamesResponse -> TestTree
responseGetActiveNames = res
    "GetActiveNamesResponse"
    "fixture/GetActiveNamesResponse.proto"
    lightsail
    (Proxy :: Proxy GetActiveNames)

responseGetInstanceAccessDetails :: GetInstanceAccessDetailsResponse -> TestTree
responseGetInstanceAccessDetails = res
    "GetInstanceAccessDetailsResponse"
    "fixture/GetInstanceAccessDetailsResponse.proto"
    lightsail
    (Proxy :: Proxy GetInstanceAccessDetails)

responseStopInstance :: StopInstanceResponse -> TestTree
responseStopInstance = res
    "StopInstanceResponse"
    "fixture/StopInstanceResponse.proto"
    lightsail
    (Proxy :: Proxy StopInstance)

responseCreateInstanceSnapshot :: CreateInstanceSnapshotResponse -> TestTree
responseCreateInstanceSnapshot = res
    "CreateInstanceSnapshotResponse"
    "fixture/CreateInstanceSnapshotResponse.proto"
    lightsail
    (Proxy :: Proxy CreateInstanceSnapshot)

responseIsVPCPeered :: IsVPCPeeredResponse -> TestTree
responseIsVPCPeered = res
    "IsVPCPeeredResponse"
    "fixture/IsVPCPeeredResponse.proto"
    lightsail
    (Proxy :: Proxy IsVPCPeered)

responseGetStaticIPs :: GetStaticIPsResponse -> TestTree
responseGetStaticIPs = res
    "GetStaticIPsResponse"
    "fixture/GetStaticIPsResponse.proto"
    lightsail
    (Proxy :: Proxy GetStaticIPs)

responseUnpeerVPC :: UnpeerVPCResponse -> TestTree
responseUnpeerVPC = res
    "UnpeerVPCResponse"
    "fixture/UnpeerVPCResponse.proto"
    lightsail
    (Proxy :: Proxy UnpeerVPC)

responseCreateInstancesFromSnapshot :: CreateInstancesFromSnapshotResponse -> TestTree
responseCreateInstancesFromSnapshot = res
    "CreateInstancesFromSnapshotResponse"
    "fixture/CreateInstancesFromSnapshotResponse.proto"
    lightsail
    (Proxy :: Proxy CreateInstancesFromSnapshot)

responseCreateDomain :: CreateDomainResponse -> TestTree
responseCreateDomain = res
    "CreateDomainResponse"
    "fixture/CreateDomainResponse.proto"
    lightsail
    (Proxy :: Proxy CreateDomain)

responsePeerVPC :: PeerVPCResponse -> TestTree
responsePeerVPC = res
    "PeerVPCResponse"
    "fixture/PeerVPCResponse.proto"
    lightsail
    (Proxy :: Proxy PeerVPC)

responseGetStaticIP :: GetStaticIPResponse -> TestTree
responseGetStaticIP = res
    "GetStaticIPResponse"
    "fixture/GetStaticIPResponse.proto"
    lightsail
    (Proxy :: Proxy GetStaticIP)

responseGetBlueprints :: GetBlueprintsResponse -> TestTree
responseGetBlueprints = res
    "GetBlueprintsResponse"
    "fixture/GetBlueprintsResponse.proto"
    lightsail
    (Proxy :: Proxy GetBlueprints)

responseGetInstancePortStates :: GetInstancePortStatesResponse -> TestTree
responseGetInstancePortStates = res
    "GetInstancePortStatesResponse"
    "fixture/GetInstancePortStatesResponse.proto"
    lightsail
    (Proxy :: Proxy GetInstancePortStates)

responseDeleteDomainEntry :: DeleteDomainEntryResponse -> TestTree
responseDeleteDomainEntry = res
    "DeleteDomainEntryResponse"
    "fixture/DeleteDomainEntryResponse.proto"
    lightsail
    (Proxy :: Proxy DeleteDomainEntry)

responseUpdateDomainEntry :: UpdateDomainEntryResponse -> TestTree
responseUpdateDomainEntry = res
    "UpdateDomainEntryResponse"
    "fixture/UpdateDomainEntryResponse.proto"
    lightsail
    (Proxy :: Proxy UpdateDomainEntry)

responseGetRegions :: GetRegionsResponse -> TestTree
responseGetRegions = res
    "GetRegionsResponse"
    "fixture/GetRegionsResponse.proto"
    lightsail
    (Proxy :: Proxy GetRegions)

responseGetInstanceState :: GetInstanceStateResponse -> TestTree
responseGetInstanceState = res
    "GetInstanceStateResponse"
    "fixture/GetInstanceStateResponse.proto"
    lightsail
    (Proxy :: Proxy GetInstanceState)

responseGetKeyPairs :: GetKeyPairsResponse -> TestTree
responseGetKeyPairs = res
    "GetKeyPairsResponse"
    "fixture/GetKeyPairsResponse.proto"
    lightsail
    (Proxy :: Proxy GetKeyPairs)

responseGetOperations :: GetOperationsResponse -> TestTree
responseGetOperations = res
    "GetOperationsResponse"
    "fixture/GetOperationsResponse.proto"
    lightsail
    (Proxy :: Proxy GetOperations)

responseGetOperation :: GetOperationResponse -> TestTree
responseGetOperation = res
    "GetOperationResponse"
    "fixture/GetOperationResponse.proto"
    lightsail
    (Proxy :: Proxy GetOperation)

responseGetInstanceMetricData :: GetInstanceMetricDataResponse -> TestTree
responseGetInstanceMetricData = res
    "GetInstanceMetricDataResponse"
    "fixture/GetInstanceMetricDataResponse.proto"
    lightsail
    (Proxy :: Proxy GetInstanceMetricData)

responseGetKeyPair :: GetKeyPairResponse -> TestTree
responseGetKeyPair = res
    "GetKeyPairResponse"
    "fixture/GetKeyPairResponse.proto"
    lightsail
    (Proxy :: Proxy GetKeyPair)

responsePutInstancePublicPorts :: PutInstancePublicPortsResponse -> TestTree
responsePutInstancePublicPorts = res
    "PutInstancePublicPortsResponse"
    "fixture/PutInstancePublicPortsResponse.proto"
    lightsail
    (Proxy :: Proxy PutInstancePublicPorts)

responseDetachStaticIP :: DetachStaticIPResponse -> TestTree
responseDetachStaticIP = res
    "DetachStaticIPResponse"
    "fixture/DetachStaticIPResponse.proto"
    lightsail
    (Proxy :: Proxy DetachStaticIP)

responseCreateInstances :: CreateInstancesResponse -> TestTree
responseCreateInstances = res
    "CreateInstancesResponse"
    "fixture/CreateInstancesResponse.proto"
    lightsail
    (Proxy :: Proxy CreateInstances)

responseOpenInstancePublicPorts :: OpenInstancePublicPortsResponse -> TestTree
responseOpenInstancePublicPorts = res
    "OpenInstancePublicPortsResponse"
    "fixture/OpenInstancePublicPortsResponse.proto"
    lightsail
    (Proxy :: Proxy OpenInstancePublicPorts)

responseGetBundles :: GetBundlesResponse -> TestTree
responseGetBundles = res
    "GetBundlesResponse"
    "fixture/GetBundlesResponse.proto"
    lightsail
    (Proxy :: Proxy GetBundles)

responseDeleteDomain :: DeleteDomainResponse -> TestTree
responseDeleteDomain = res
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse.proto"
    lightsail
    (Proxy :: Proxy DeleteDomain)

responseGetOperationsForResource :: GetOperationsForResourceResponse -> TestTree
responseGetOperationsForResource = res
    "GetOperationsForResourceResponse"
    "fixture/GetOperationsForResourceResponse.proto"
    lightsail
    (Proxy :: Proxy GetOperationsForResource)

responseCreateKeyPair :: CreateKeyPairResponse -> TestTree
responseCreateKeyPair = res
    "CreateKeyPairResponse"
    "fixture/CreateKeyPairResponse.proto"
    lightsail
    (Proxy :: Proxy CreateKeyPair)

responseStartInstance :: StartInstanceResponse -> TestTree
responseStartInstance = res
    "StartInstanceResponse"
    "fixture/StartInstanceResponse.proto"
    lightsail
    (Proxy :: Proxy StartInstance)

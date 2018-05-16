{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DirectoryService
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.DirectoryService where

import Data.Proxy
import Network.AWS.DirectoryService
import Test.AWS.DirectoryService.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestUpdateNumberOfDomainControllers $
--             updateNumberOfDomainControllers
--
--         , requestDescribeConditionalForwarders $
--             describeConditionalForwarders
--
--         , requestGetSnapshotLimits $
--             getSnapshotLimits
--
--         , requestRegisterEventTopic $
--             registerEventTopic
--
--         , requestConnectDirectory $
--             connectDirectory
--
--         , requestCreateAlias $
--             createAlias
--
--         , requestDescribeDirectories $
--             describeDirectories
--
--         , requestAddIPRoutes $
--             addIPRoutes
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestDescribeTrusts $
--             describeTrusts
--
--         , requestDeleteTrust $
--             deleteTrust
--
--         , requestCreateMicrosoftAD $
--             createMicrosoftAD
--
--         , requestDeregisterEventTopic $
--             deregisterEventTopic
--
--         , requestCreateDirectory $
--             createDirectory
--
--         , requestRemoveTagsFromResource $
--             removeTagsFromResource
--
--         , requestDescribeEventTopics $
--             describeEventTopics
--
--         , requestUpdateConditionalForwarder $
--             updateConditionalForwarder
--
--         , requestDeleteConditionalForwarder $
--             deleteConditionalForwarder
--
--         , requestEnableSSO $
--             enableSSO
--
--         , requestCancelSchemaExtension $
--             cancelSchemaExtension
--
--         , requestEnableRadius $
--             enableRadius
--
--         , requestListIPRoutes $
--             listIPRoutes
--
--         , requestAddTagsToResource $
--             addTagsToResource
--
--         , requestListSchemaExtensions $
--             listSchemaExtensions
--
--         , requestDisableRadius $
--             disableRadius
--
--         , requestRestoreFromSnapshot $
--             restoreFromSnapshot
--
--         , requestDescribeDomainControllers $
--             describeDomainControllers
--
--         , requestDescribeSnapshots $
--             describeSnapshots
--
--         , requestRemoveIPRoutes $
--             removeIPRoutes
--
--         , requestDeleteSnapshot $
--             deleteSnapshot
--
--         , requestStartSchemaExtension $
--             startSchemaExtension
--
--         , requestCreateTrust $
--             createTrust
--
--         , requestDeleteDirectory $
--             deleteDirectory
--
--         , requestCreateSnapshot $
--             createSnapshot
--
--         , requestCreateComputer $
--             createComputer
--
--         , requestDisableSSO $
--             disableSSO
--
--         , requestVerifyTrust $
--             verifyTrust
--
--         , requestCreateConditionalForwarder $
--             createConditionalForwarder
--
--         , requestGetDirectoryLimits $
--             getDirectoryLimits
--
--         , requestUpdateRadius $
--             updateRadius
--
--           ]

--     , testGroup "response"
--         [ responseUpdateNumberOfDomainControllers $
--             updateNumberOfDomainControllersResponse
--
--         , responseDescribeConditionalForwarders $
--             describeConditionalForwardersResponse
--
--         , responseGetSnapshotLimits $
--             getSnapshotLimitsResponse
--
--         , responseRegisterEventTopic $
--             registerEventTopicResponse
--
--         , responseConnectDirectory $
--             connectDirectoryResponse
--
--         , responseCreateAlias $
--             createAliasResponse
--
--         , responseDescribeDirectories $
--             describeDirectoriesResponse
--
--         , responseAddIPRoutes $
--             addIPRoutesResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responseDescribeTrusts $
--             describeTrustsResponse
--
--         , responseDeleteTrust $
--             deleteTrustResponse
--
--         , responseCreateMicrosoftAD $
--             createMicrosoftADResponse
--
--         , responseDeregisterEventTopic $
--             deregisterEventTopicResponse
--
--         , responseCreateDirectory $
--             createDirectoryResponse
--
--         , responseRemoveTagsFromResource $
--             removeTagsFromResourceResponse
--
--         , responseDescribeEventTopics $
--             describeEventTopicsResponse
--
--         , responseUpdateConditionalForwarder $
--             updateConditionalForwarderResponse
--
--         , responseDeleteConditionalForwarder $
--             deleteConditionalForwarderResponse
--
--         , responseEnableSSO $
--             enableSSOResponse
--
--         , responseCancelSchemaExtension $
--             cancelSchemaExtensionResponse
--
--         , responseEnableRadius $
--             enableRadiusResponse
--
--         , responseListIPRoutes $
--             listIPRoutesResponse
--
--         , responseAddTagsToResource $
--             addTagsToResourceResponse
--
--         , responseListSchemaExtensions $
--             listSchemaExtensionsResponse
--
--         , responseDisableRadius $
--             disableRadiusResponse
--
--         , responseRestoreFromSnapshot $
--             restoreFromSnapshotResponse
--
--         , responseDescribeDomainControllers $
--             describeDomainControllersResponse
--
--         , responseDescribeSnapshots $
--             describeSnapshotsResponse
--
--         , responseRemoveIPRoutes $
--             removeIPRoutesResponse
--
--         , responseDeleteSnapshot $
--             deleteSnapshotResponse
--
--         , responseStartSchemaExtension $
--             startSchemaExtensionResponse
--
--         , responseCreateTrust $
--             createTrustResponse
--
--         , responseDeleteDirectory $
--             deleteDirectoryResponse
--
--         , responseCreateSnapshot $
--             createSnapshotResponse
--
--         , responseCreateComputer $
--             createComputerResponse
--
--         , responseDisableSSO $
--             disableSSOResponse
--
--         , responseVerifyTrust $
--             verifyTrustResponse
--
--         , responseCreateConditionalForwarder $
--             createConditionalForwarderResponse
--
--         , responseGetDirectoryLimits $
--             getDirectoryLimitsResponse
--
--         , responseUpdateRadius $
--             updateRadiusResponse
--
--           ]
--     ]

-- Requests

requestUpdateNumberOfDomainControllers :: UpdateNumberOfDomainControllers -> TestTree
requestUpdateNumberOfDomainControllers = req
    "UpdateNumberOfDomainControllers"
    "fixture/UpdateNumberOfDomainControllers.yaml"

requestDescribeConditionalForwarders :: DescribeConditionalForwarders -> TestTree
requestDescribeConditionalForwarders = req
    "DescribeConditionalForwarders"
    "fixture/DescribeConditionalForwarders.yaml"

requestGetSnapshotLimits :: GetSnapshotLimits -> TestTree
requestGetSnapshotLimits = req
    "GetSnapshotLimits"
    "fixture/GetSnapshotLimits.yaml"

requestRegisterEventTopic :: RegisterEventTopic -> TestTree
requestRegisterEventTopic = req
    "RegisterEventTopic"
    "fixture/RegisterEventTopic.yaml"

requestConnectDirectory :: ConnectDirectory -> TestTree
requestConnectDirectory = req
    "ConnectDirectory"
    "fixture/ConnectDirectory.yaml"

requestCreateAlias :: CreateAlias -> TestTree
requestCreateAlias = req
    "CreateAlias"
    "fixture/CreateAlias.yaml"

requestDescribeDirectories :: DescribeDirectories -> TestTree
requestDescribeDirectories = req
    "DescribeDirectories"
    "fixture/DescribeDirectories.yaml"

requestAddIPRoutes :: AddIPRoutes -> TestTree
requestAddIPRoutes = req
    "AddIPRoutes"
    "fixture/AddIPRoutes.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDescribeTrusts :: DescribeTrusts -> TestTree
requestDescribeTrusts = req
    "DescribeTrusts"
    "fixture/DescribeTrusts.yaml"

requestDeleteTrust :: DeleteTrust -> TestTree
requestDeleteTrust = req
    "DeleteTrust"
    "fixture/DeleteTrust.yaml"

requestCreateMicrosoftAD :: CreateMicrosoftAD -> TestTree
requestCreateMicrosoftAD = req
    "CreateMicrosoftAD"
    "fixture/CreateMicrosoftAD.yaml"

requestDeregisterEventTopic :: DeregisterEventTopic -> TestTree
requestDeregisterEventTopic = req
    "DeregisterEventTopic"
    "fixture/DeregisterEventTopic.yaml"

requestCreateDirectory :: CreateDirectory -> TestTree
requestCreateDirectory = req
    "CreateDirectory"
    "fixture/CreateDirectory.yaml"

requestRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
requestRemoveTagsFromResource = req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource.yaml"

requestDescribeEventTopics :: DescribeEventTopics -> TestTree
requestDescribeEventTopics = req
    "DescribeEventTopics"
    "fixture/DescribeEventTopics.yaml"

requestUpdateConditionalForwarder :: UpdateConditionalForwarder -> TestTree
requestUpdateConditionalForwarder = req
    "UpdateConditionalForwarder"
    "fixture/UpdateConditionalForwarder.yaml"

requestDeleteConditionalForwarder :: DeleteConditionalForwarder -> TestTree
requestDeleteConditionalForwarder = req
    "DeleteConditionalForwarder"
    "fixture/DeleteConditionalForwarder.yaml"

requestEnableSSO :: EnableSSO -> TestTree
requestEnableSSO = req
    "EnableSSO"
    "fixture/EnableSSO.yaml"

requestCancelSchemaExtension :: CancelSchemaExtension -> TestTree
requestCancelSchemaExtension = req
    "CancelSchemaExtension"
    "fixture/CancelSchemaExtension.yaml"

requestEnableRadius :: EnableRadius -> TestTree
requestEnableRadius = req
    "EnableRadius"
    "fixture/EnableRadius.yaml"

requestListIPRoutes :: ListIPRoutes -> TestTree
requestListIPRoutes = req
    "ListIPRoutes"
    "fixture/ListIPRoutes.yaml"

requestAddTagsToResource :: AddTagsToResource -> TestTree
requestAddTagsToResource = req
    "AddTagsToResource"
    "fixture/AddTagsToResource.yaml"

requestListSchemaExtensions :: ListSchemaExtensions -> TestTree
requestListSchemaExtensions = req
    "ListSchemaExtensions"
    "fixture/ListSchemaExtensions.yaml"

requestDisableRadius :: DisableRadius -> TestTree
requestDisableRadius = req
    "DisableRadius"
    "fixture/DisableRadius.yaml"

requestRestoreFromSnapshot :: RestoreFromSnapshot -> TestTree
requestRestoreFromSnapshot = req
    "RestoreFromSnapshot"
    "fixture/RestoreFromSnapshot.yaml"

requestDescribeDomainControllers :: DescribeDomainControllers -> TestTree
requestDescribeDomainControllers = req
    "DescribeDomainControllers"
    "fixture/DescribeDomainControllers.yaml"

requestDescribeSnapshots :: DescribeSnapshots -> TestTree
requestDescribeSnapshots = req
    "DescribeSnapshots"
    "fixture/DescribeSnapshots.yaml"

requestRemoveIPRoutes :: RemoveIPRoutes -> TestTree
requestRemoveIPRoutes = req
    "RemoveIPRoutes"
    "fixture/RemoveIPRoutes.yaml"

requestDeleteSnapshot :: DeleteSnapshot -> TestTree
requestDeleteSnapshot = req
    "DeleteSnapshot"
    "fixture/DeleteSnapshot.yaml"

requestStartSchemaExtension :: StartSchemaExtension -> TestTree
requestStartSchemaExtension = req
    "StartSchemaExtension"
    "fixture/StartSchemaExtension.yaml"

requestCreateTrust :: CreateTrust -> TestTree
requestCreateTrust = req
    "CreateTrust"
    "fixture/CreateTrust.yaml"

requestDeleteDirectory :: DeleteDirectory -> TestTree
requestDeleteDirectory = req
    "DeleteDirectory"
    "fixture/DeleteDirectory.yaml"

requestCreateSnapshot :: CreateSnapshot -> TestTree
requestCreateSnapshot = req
    "CreateSnapshot"
    "fixture/CreateSnapshot.yaml"

requestCreateComputer :: CreateComputer -> TestTree
requestCreateComputer = req
    "CreateComputer"
    "fixture/CreateComputer.yaml"

requestDisableSSO :: DisableSSO -> TestTree
requestDisableSSO = req
    "DisableSSO"
    "fixture/DisableSSO.yaml"

requestVerifyTrust :: VerifyTrust -> TestTree
requestVerifyTrust = req
    "VerifyTrust"
    "fixture/VerifyTrust.yaml"

requestCreateConditionalForwarder :: CreateConditionalForwarder -> TestTree
requestCreateConditionalForwarder = req
    "CreateConditionalForwarder"
    "fixture/CreateConditionalForwarder.yaml"

requestGetDirectoryLimits :: GetDirectoryLimits -> TestTree
requestGetDirectoryLimits = req
    "GetDirectoryLimits"
    "fixture/GetDirectoryLimits.yaml"

requestUpdateRadius :: UpdateRadius -> TestTree
requestUpdateRadius = req
    "UpdateRadius"
    "fixture/UpdateRadius.yaml"

-- Responses

responseUpdateNumberOfDomainControllers :: UpdateNumberOfDomainControllersResponse -> TestTree
responseUpdateNumberOfDomainControllers = res
    "UpdateNumberOfDomainControllersResponse"
    "fixture/UpdateNumberOfDomainControllersResponse.proto"
    directoryService
    (Proxy :: Proxy UpdateNumberOfDomainControllers)

responseDescribeConditionalForwarders :: DescribeConditionalForwardersResponse -> TestTree
responseDescribeConditionalForwarders = res
    "DescribeConditionalForwardersResponse"
    "fixture/DescribeConditionalForwardersResponse.proto"
    directoryService
    (Proxy :: Proxy DescribeConditionalForwarders)

responseGetSnapshotLimits :: GetSnapshotLimitsResponse -> TestTree
responseGetSnapshotLimits = res
    "GetSnapshotLimitsResponse"
    "fixture/GetSnapshotLimitsResponse.proto"
    directoryService
    (Proxy :: Proxy GetSnapshotLimits)

responseRegisterEventTopic :: RegisterEventTopicResponse -> TestTree
responseRegisterEventTopic = res
    "RegisterEventTopicResponse"
    "fixture/RegisterEventTopicResponse.proto"
    directoryService
    (Proxy :: Proxy RegisterEventTopic)

responseConnectDirectory :: ConnectDirectoryResponse -> TestTree
responseConnectDirectory = res
    "ConnectDirectoryResponse"
    "fixture/ConnectDirectoryResponse.proto"
    directoryService
    (Proxy :: Proxy ConnectDirectory)

responseCreateAlias :: CreateAliasResponse -> TestTree
responseCreateAlias = res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse.proto"
    directoryService
    (Proxy :: Proxy CreateAlias)

responseDescribeDirectories :: DescribeDirectoriesResponse -> TestTree
responseDescribeDirectories = res
    "DescribeDirectoriesResponse"
    "fixture/DescribeDirectoriesResponse.proto"
    directoryService
    (Proxy :: Proxy DescribeDirectories)

responseAddIPRoutes :: AddIPRoutesResponse -> TestTree
responseAddIPRoutes = res
    "AddIPRoutesResponse"
    "fixture/AddIPRoutesResponse.proto"
    directoryService
    (Proxy :: Proxy AddIPRoutes)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    directoryService
    (Proxy :: Proxy ListTagsForResource)

responseDescribeTrusts :: DescribeTrustsResponse -> TestTree
responseDescribeTrusts = res
    "DescribeTrustsResponse"
    "fixture/DescribeTrustsResponse.proto"
    directoryService
    (Proxy :: Proxy DescribeTrusts)

responseDeleteTrust :: DeleteTrustResponse -> TestTree
responseDeleteTrust = res
    "DeleteTrustResponse"
    "fixture/DeleteTrustResponse.proto"
    directoryService
    (Proxy :: Proxy DeleteTrust)

responseCreateMicrosoftAD :: CreateMicrosoftADResponse -> TestTree
responseCreateMicrosoftAD = res
    "CreateMicrosoftADResponse"
    "fixture/CreateMicrosoftADResponse.proto"
    directoryService
    (Proxy :: Proxy CreateMicrosoftAD)

responseDeregisterEventTopic :: DeregisterEventTopicResponse -> TestTree
responseDeregisterEventTopic = res
    "DeregisterEventTopicResponse"
    "fixture/DeregisterEventTopicResponse.proto"
    directoryService
    (Proxy :: Proxy DeregisterEventTopic)

responseCreateDirectory :: CreateDirectoryResponse -> TestTree
responseCreateDirectory = res
    "CreateDirectoryResponse"
    "fixture/CreateDirectoryResponse.proto"
    directoryService
    (Proxy :: Proxy CreateDirectory)

responseRemoveTagsFromResource :: RemoveTagsFromResourceResponse -> TestTree
responseRemoveTagsFromResource = res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    directoryService
    (Proxy :: Proxy RemoveTagsFromResource)

responseDescribeEventTopics :: DescribeEventTopicsResponse -> TestTree
responseDescribeEventTopics = res
    "DescribeEventTopicsResponse"
    "fixture/DescribeEventTopicsResponse.proto"
    directoryService
    (Proxy :: Proxy DescribeEventTopics)

responseUpdateConditionalForwarder :: UpdateConditionalForwarderResponse -> TestTree
responseUpdateConditionalForwarder = res
    "UpdateConditionalForwarderResponse"
    "fixture/UpdateConditionalForwarderResponse.proto"
    directoryService
    (Proxy :: Proxy UpdateConditionalForwarder)

responseDeleteConditionalForwarder :: DeleteConditionalForwarderResponse -> TestTree
responseDeleteConditionalForwarder = res
    "DeleteConditionalForwarderResponse"
    "fixture/DeleteConditionalForwarderResponse.proto"
    directoryService
    (Proxy :: Proxy DeleteConditionalForwarder)

responseEnableSSO :: EnableSSOResponse -> TestTree
responseEnableSSO = res
    "EnableSSOResponse"
    "fixture/EnableSSOResponse.proto"
    directoryService
    (Proxy :: Proxy EnableSSO)

responseCancelSchemaExtension :: CancelSchemaExtensionResponse -> TestTree
responseCancelSchemaExtension = res
    "CancelSchemaExtensionResponse"
    "fixture/CancelSchemaExtensionResponse.proto"
    directoryService
    (Proxy :: Proxy CancelSchemaExtension)

responseEnableRadius :: EnableRadiusResponse -> TestTree
responseEnableRadius = res
    "EnableRadiusResponse"
    "fixture/EnableRadiusResponse.proto"
    directoryService
    (Proxy :: Proxy EnableRadius)

responseListIPRoutes :: ListIPRoutesResponse -> TestTree
responseListIPRoutes = res
    "ListIPRoutesResponse"
    "fixture/ListIPRoutesResponse.proto"
    directoryService
    (Proxy :: Proxy ListIPRoutes)

responseAddTagsToResource :: AddTagsToResourceResponse -> TestTree
responseAddTagsToResource = res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    directoryService
    (Proxy :: Proxy AddTagsToResource)

responseListSchemaExtensions :: ListSchemaExtensionsResponse -> TestTree
responseListSchemaExtensions = res
    "ListSchemaExtensionsResponse"
    "fixture/ListSchemaExtensionsResponse.proto"
    directoryService
    (Proxy :: Proxy ListSchemaExtensions)

responseDisableRadius :: DisableRadiusResponse -> TestTree
responseDisableRadius = res
    "DisableRadiusResponse"
    "fixture/DisableRadiusResponse.proto"
    directoryService
    (Proxy :: Proxy DisableRadius)

responseRestoreFromSnapshot :: RestoreFromSnapshotResponse -> TestTree
responseRestoreFromSnapshot = res
    "RestoreFromSnapshotResponse"
    "fixture/RestoreFromSnapshotResponse.proto"
    directoryService
    (Proxy :: Proxy RestoreFromSnapshot)

responseDescribeDomainControllers :: DescribeDomainControllersResponse -> TestTree
responseDescribeDomainControllers = res
    "DescribeDomainControllersResponse"
    "fixture/DescribeDomainControllersResponse.proto"
    directoryService
    (Proxy :: Proxy DescribeDomainControllers)

responseDescribeSnapshots :: DescribeSnapshotsResponse -> TestTree
responseDescribeSnapshots = res
    "DescribeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse.proto"
    directoryService
    (Proxy :: Proxy DescribeSnapshots)

responseRemoveIPRoutes :: RemoveIPRoutesResponse -> TestTree
responseRemoveIPRoutes = res
    "RemoveIPRoutesResponse"
    "fixture/RemoveIPRoutesResponse.proto"
    directoryService
    (Proxy :: Proxy RemoveIPRoutes)

responseDeleteSnapshot :: DeleteSnapshotResponse -> TestTree
responseDeleteSnapshot = res
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse.proto"
    directoryService
    (Proxy :: Proxy DeleteSnapshot)

responseStartSchemaExtension :: StartSchemaExtensionResponse -> TestTree
responseStartSchemaExtension = res
    "StartSchemaExtensionResponse"
    "fixture/StartSchemaExtensionResponse.proto"
    directoryService
    (Proxy :: Proxy StartSchemaExtension)

responseCreateTrust :: CreateTrustResponse -> TestTree
responseCreateTrust = res
    "CreateTrustResponse"
    "fixture/CreateTrustResponse.proto"
    directoryService
    (Proxy :: Proxy CreateTrust)

responseDeleteDirectory :: DeleteDirectoryResponse -> TestTree
responseDeleteDirectory = res
    "DeleteDirectoryResponse"
    "fixture/DeleteDirectoryResponse.proto"
    directoryService
    (Proxy :: Proxy DeleteDirectory)

responseCreateSnapshot :: CreateSnapshotResponse -> TestTree
responseCreateSnapshot = res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    directoryService
    (Proxy :: Proxy CreateSnapshot)

responseCreateComputer :: CreateComputerResponse -> TestTree
responseCreateComputer = res
    "CreateComputerResponse"
    "fixture/CreateComputerResponse.proto"
    directoryService
    (Proxy :: Proxy CreateComputer)

responseDisableSSO :: DisableSSOResponse -> TestTree
responseDisableSSO = res
    "DisableSSOResponse"
    "fixture/DisableSSOResponse.proto"
    directoryService
    (Proxy :: Proxy DisableSSO)

responseVerifyTrust :: VerifyTrustResponse -> TestTree
responseVerifyTrust = res
    "VerifyTrustResponse"
    "fixture/VerifyTrustResponse.proto"
    directoryService
    (Proxy :: Proxy VerifyTrust)

responseCreateConditionalForwarder :: CreateConditionalForwarderResponse -> TestTree
responseCreateConditionalForwarder = res
    "CreateConditionalForwarderResponse"
    "fixture/CreateConditionalForwarderResponse.proto"
    directoryService
    (Proxy :: Proxy CreateConditionalForwarder)

responseGetDirectoryLimits :: GetDirectoryLimitsResponse -> TestTree
responseGetDirectoryLimits = res
    "GetDirectoryLimitsResponse"
    "fixture/GetDirectoryLimitsResponse.proto"
    directoryService
    (Proxy :: Proxy GetDirectoryLimits)

responseUpdateRadius :: UpdateRadiusResponse -> TestTree
responseUpdateRadius = res
    "UpdateRadiusResponse"
    "fixture/UpdateRadiusResponse.proto"
    directoryService
    (Proxy :: Proxy UpdateRadius)

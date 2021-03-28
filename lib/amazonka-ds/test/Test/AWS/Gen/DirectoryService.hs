{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DirectoryService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.DirectoryService where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.DirectoryService
import Test.AWS.DirectoryService.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestShareDirectory $
--             mkShareDirectory
--
--         , requestUpdateNumberOfDomainControllers $
--             mkUpdateNumberOfDomainControllers
--
--         , requestDescribeConditionalForwarders $
--             mkDescribeConditionalForwarders
--
--         , requestGetSnapshotLimits $
--             mkGetSnapshotLimits
--
--         , requestRegisterEventTopic $
--             mkRegisterEventTopic
--
--         , requestRegisterCertificate $
--             mkRegisterCertificate
--
--         , requestConnectDirectory $
--             mkConnectDirectory
--
--         , requestDescribeLDAPSSettings $
--             mkDescribeLDAPSSettings
--
--         , requestCreateAlias $
--             mkCreateAlias
--
--         , requestDescribeDirectories $
--             mkDescribeDirectories
--
--         , requestAddIpRoutes $
--             mkAddIpRoutes
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestDescribeTrusts $
--             mkDescribeTrusts
--
--         , requestDeleteTrust $
--             mkDeleteTrust
--
--         , requestUpdateTrust $
--             mkUpdateTrust
--
--         , requestCreateMicrosoftAD $
--             mkCreateMicrosoftAD
--
--         , requestDeregisterEventTopic $
--             mkDeregisterEventTopic
--
--         , requestCreateDirectory $
--             mkCreateDirectory
--
--         , requestAcceptSharedDirectory $
--             mkAcceptSharedDirectory
--
--         , requestCreateLogSubscription $
--             mkCreateLogSubscription
--
--         , requestRemoveTagsFromResource $
--             mkRemoveTagsFromResource
--
--         , requestDescribeEventTopics $
--             mkDescribeEventTopics
--
--         , requestResetUserPassword $
--             mkResetUserPassword
--
--         , requestUpdateConditionalForwarder $
--             mkUpdateConditionalForwarder
--
--         , requestDeleteConditionalForwarder $
--             mkDeleteConditionalForwarder
--
--         , requestDisableLDAPS $
--             mkDisableLDAPS
--
--         , requestDeleteLogSubscription $
--             mkDeleteLogSubscription
--
--         , requestEnableSso $
--             mkEnableSso
--
--         , requestCancelSchemaExtension $
--             mkCancelSchemaExtension
--
--         , requestListLogSubscriptions $
--             mkListLogSubscriptions
--
--         , requestEnableRadius $
--             mkEnableRadius
--
--         , requestListIpRoutes $
--             mkListIpRoutes
--
--         , requestAddTagsToResource $
--             mkAddTagsToResource
--
--         , requestListSchemaExtensions $
--             mkListSchemaExtensions
--
--         , requestDisableRadius $
--             mkDisableRadius
--
--         , requestListCertificates $
--             mkListCertificates
--
--         , requestRejectSharedDirectory $
--             mkRejectSharedDirectory
--
--         , requestUnshareDirectory $
--             mkUnshareDirectory
--
--         , requestRestoreFromSnapshot $
--             mkRestoreFromSnapshot
--
--         , requestDescribeDomainControllers $
--             mkDescribeDomainControllers
--
--         , requestDescribeSnapshots $
--             mkDescribeSnapshots
--
--         , requestRemoveIpRoutes $
--             mkRemoveIpRoutes
--
--         , requestDeleteSnapshot $
--             mkDeleteSnapshot
--
--         , requestDeregisterCertificate $
--             mkDeregisterCertificate
--
--         , requestStartSchemaExtension $
--             mkStartSchemaExtension
--
--         , requestCreateTrust $
--             mkCreateTrust
--
--         , requestDeleteDirectory $
--             mkDeleteDirectory
--
--         , requestCreateSnapshot $
--             mkCreateSnapshot
--
--         , requestDescribeCertificate $
--             mkDescribeCertificate
--
--         , requestCreateComputer $
--             mkCreateComputer
--
--         , requestDescribeSharedDirectories $
--             mkDescribeSharedDirectories
--
--         , requestEnableLDAPS $
--             mkEnableLDAPS
--
--         , requestDisableSso $
--             mkDisableSso
--
--         , requestVerifyTrust $
--             mkVerifyTrust
--
--         , requestRemoveRegion $
--             mkRemoveRegion
--
--         , requestCreateConditionalForwarder $
--             mkCreateConditionalForwarder
--
--         , requestDescribeRegions $
--             mkDescribeRegions
--
--         , requestAddRegion $
--             mkAddRegion
--
--         , requestGetDirectoryLimits $
--             mkGetDirectoryLimits
--
--         , requestUpdateRadius $
--             mkUpdateRadius
--
--           ]

--     , testGroup "response"
--         [ responseShareDirectory $
--             mkShareDirectoryResponse
--
--         , responseUpdateNumberOfDomainControllers $
--             mkUpdateNumberOfDomainControllersResponse
--
--         , responseDescribeConditionalForwarders $
--             mkDescribeConditionalForwardersResponse
--
--         , responseGetSnapshotLimits $
--             mkGetSnapshotLimitsResponse
--
--         , responseRegisterEventTopic $
--             mkRegisterEventTopicResponse
--
--         , responseRegisterCertificate $
--             mkRegisterCertificateResponse
--
--         , responseConnectDirectory $
--             mkConnectDirectoryResponse
--
--         , responseDescribeLDAPSSettings $
--             mkDescribeLDAPSSettingsResponse
--
--         , responseCreateAlias $
--             mkCreateAliasResponse
--
--         , responseDescribeDirectories $
--             mkDescribeDirectoriesResponse
--
--         , responseAddIpRoutes $
--             mkAddIpRoutesResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseDescribeTrusts $
--             mkDescribeTrustsResponse
--
--         , responseDeleteTrust $
--             mkDeleteTrustResponse
--
--         , responseUpdateTrust $
--             mkUpdateTrustResponse
--
--         , responseCreateMicrosoftAD $
--             mkCreateMicrosoftADResponse
--
--         , responseDeregisterEventTopic $
--             mkDeregisterEventTopicResponse
--
--         , responseCreateDirectory $
--             mkCreateDirectoryResponse
--
--         , responseAcceptSharedDirectory $
--             mkAcceptSharedDirectoryResponse
--
--         , responseCreateLogSubscription $
--             mkCreateLogSubscriptionResponse
--
--         , responseRemoveTagsFromResource $
--             mkRemoveTagsFromResourceResponse
--
--         , responseDescribeEventTopics $
--             mkDescribeEventTopicsResponse
--
--         , responseResetUserPassword $
--             mkResetUserPasswordResponse
--
--         , responseUpdateConditionalForwarder $
--             mkUpdateConditionalForwarderResponse
--
--         , responseDeleteConditionalForwarder $
--             mkDeleteConditionalForwarderResponse
--
--         , responseDisableLDAPS $
--             mkDisableLDAPSResponse
--
--         , responseDeleteLogSubscription $
--             mkDeleteLogSubscriptionResponse
--
--         , responseEnableSso $
--             mkEnableSsoResponse
--
--         , responseCancelSchemaExtension $
--             mkCancelSchemaExtensionResponse
--
--         , responseListLogSubscriptions $
--             mkListLogSubscriptionsResponse
--
--         , responseEnableRadius $
--             mkEnableRadiusResponse
--
--         , responseListIpRoutes $
--             mkListIpRoutesResponse
--
--         , responseAddTagsToResource $
--             mkAddTagsToResourceResponse
--
--         , responseListSchemaExtensions $
--             mkListSchemaExtensionsResponse
--
--         , responseDisableRadius $
--             mkDisableRadiusResponse
--
--         , responseListCertificates $
--             mkListCertificatesResponse
--
--         , responseRejectSharedDirectory $
--             mkRejectSharedDirectoryResponse
--
--         , responseUnshareDirectory $
--             mkUnshareDirectoryResponse
--
--         , responseRestoreFromSnapshot $
--             mkRestoreFromSnapshotResponse
--
--         , responseDescribeDomainControllers $
--             mkDescribeDomainControllersResponse
--
--         , responseDescribeSnapshots $
--             mkDescribeSnapshotsResponse
--
--         , responseRemoveIpRoutes $
--             mkRemoveIpRoutesResponse
--
--         , responseDeleteSnapshot $
--             mkDeleteSnapshotResponse
--
--         , responseDeregisterCertificate $
--             mkDeregisterCertificateResponse
--
--         , responseStartSchemaExtension $
--             mkStartSchemaExtensionResponse
--
--         , responseCreateTrust $
--             mkCreateTrustResponse
--
--         , responseDeleteDirectory $
--             mkDeleteDirectoryResponse
--
--         , responseCreateSnapshot $
--             mkCreateSnapshotResponse
--
--         , responseDescribeCertificate $
--             mkDescribeCertificateResponse
--
--         , responseCreateComputer $
--             mkCreateComputerResponse
--
--         , responseDescribeSharedDirectories $
--             mkDescribeSharedDirectoriesResponse
--
--         , responseEnableLDAPS $
--             mkEnableLDAPSResponse
--
--         , responseDisableSso $
--             mkDisableSsoResponse
--
--         , responseVerifyTrust $
--             mkVerifyTrustResponse
--
--         , responseRemoveRegion $
--             mkRemoveRegionResponse
--
--         , responseCreateConditionalForwarder $
--             mkCreateConditionalForwarderResponse
--
--         , responseDescribeRegions $
--             mkDescribeRegionsResponse
--
--         , responseAddRegion $
--             mkAddRegionResponse
--
--         , responseGetDirectoryLimits $
--             mkGetDirectoryLimitsResponse
--
--         , responseUpdateRadius $
--             mkUpdateRadiusResponse
--
--           ]
--     ]

-- Requests

requestShareDirectory :: ShareDirectory -> TestTree
requestShareDirectory = req
    "ShareDirectory"
    "fixture/ShareDirectory.yaml"

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

requestRegisterCertificate :: RegisterCertificate -> TestTree
requestRegisterCertificate = req
    "RegisterCertificate"
    "fixture/RegisterCertificate.yaml"

requestConnectDirectory :: ConnectDirectory -> TestTree
requestConnectDirectory = req
    "ConnectDirectory"
    "fixture/ConnectDirectory.yaml"

requestDescribeLDAPSSettings :: DescribeLDAPSSettings -> TestTree
requestDescribeLDAPSSettings = req
    "DescribeLDAPSSettings"
    "fixture/DescribeLDAPSSettings.yaml"

requestCreateAlias :: CreateAlias -> TestTree
requestCreateAlias = req
    "CreateAlias"
    "fixture/CreateAlias.yaml"

requestDescribeDirectories :: DescribeDirectories -> TestTree
requestDescribeDirectories = req
    "DescribeDirectories"
    "fixture/DescribeDirectories.yaml"

requestAddIpRoutes :: AddIpRoutes -> TestTree
requestAddIpRoutes = req
    "AddIpRoutes"
    "fixture/AddIpRoutes.yaml"

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

requestUpdateTrust :: UpdateTrust -> TestTree
requestUpdateTrust = req
    "UpdateTrust"
    "fixture/UpdateTrust.yaml"

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

requestAcceptSharedDirectory :: AcceptSharedDirectory -> TestTree
requestAcceptSharedDirectory = req
    "AcceptSharedDirectory"
    "fixture/AcceptSharedDirectory.yaml"

requestCreateLogSubscription :: CreateLogSubscription -> TestTree
requestCreateLogSubscription = req
    "CreateLogSubscription"
    "fixture/CreateLogSubscription.yaml"

requestRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
requestRemoveTagsFromResource = req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource.yaml"

requestDescribeEventTopics :: DescribeEventTopics -> TestTree
requestDescribeEventTopics = req
    "DescribeEventTopics"
    "fixture/DescribeEventTopics.yaml"

requestResetUserPassword :: ResetUserPassword -> TestTree
requestResetUserPassword = req
    "ResetUserPassword"
    "fixture/ResetUserPassword.yaml"

requestUpdateConditionalForwarder :: UpdateConditionalForwarder -> TestTree
requestUpdateConditionalForwarder = req
    "UpdateConditionalForwarder"
    "fixture/UpdateConditionalForwarder.yaml"

requestDeleteConditionalForwarder :: DeleteConditionalForwarder -> TestTree
requestDeleteConditionalForwarder = req
    "DeleteConditionalForwarder"
    "fixture/DeleteConditionalForwarder.yaml"

requestDisableLDAPS :: DisableLDAPS -> TestTree
requestDisableLDAPS = req
    "DisableLDAPS"
    "fixture/DisableLDAPS.yaml"

requestDeleteLogSubscription :: DeleteLogSubscription -> TestTree
requestDeleteLogSubscription = req
    "DeleteLogSubscription"
    "fixture/DeleteLogSubscription.yaml"

requestEnableSso :: EnableSso -> TestTree
requestEnableSso = req
    "EnableSso"
    "fixture/EnableSso.yaml"

requestCancelSchemaExtension :: CancelSchemaExtension -> TestTree
requestCancelSchemaExtension = req
    "CancelSchemaExtension"
    "fixture/CancelSchemaExtension.yaml"

requestListLogSubscriptions :: ListLogSubscriptions -> TestTree
requestListLogSubscriptions = req
    "ListLogSubscriptions"
    "fixture/ListLogSubscriptions.yaml"

requestEnableRadius :: EnableRadius -> TestTree
requestEnableRadius = req
    "EnableRadius"
    "fixture/EnableRadius.yaml"

requestListIpRoutes :: ListIpRoutes -> TestTree
requestListIpRoutes = req
    "ListIpRoutes"
    "fixture/ListIpRoutes.yaml"

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

requestListCertificates :: ListCertificates -> TestTree
requestListCertificates = req
    "ListCertificates"
    "fixture/ListCertificates.yaml"

requestRejectSharedDirectory :: RejectSharedDirectory -> TestTree
requestRejectSharedDirectory = req
    "RejectSharedDirectory"
    "fixture/RejectSharedDirectory.yaml"

requestUnshareDirectory :: UnshareDirectory -> TestTree
requestUnshareDirectory = req
    "UnshareDirectory"
    "fixture/UnshareDirectory.yaml"

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

requestRemoveIpRoutes :: RemoveIpRoutes -> TestTree
requestRemoveIpRoutes = req
    "RemoveIpRoutes"
    "fixture/RemoveIpRoutes.yaml"

requestDeleteSnapshot :: DeleteSnapshot -> TestTree
requestDeleteSnapshot = req
    "DeleteSnapshot"
    "fixture/DeleteSnapshot.yaml"

requestDeregisterCertificate :: DeregisterCertificate -> TestTree
requestDeregisterCertificate = req
    "DeregisterCertificate"
    "fixture/DeregisterCertificate.yaml"

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

requestDescribeCertificate :: DescribeCertificate -> TestTree
requestDescribeCertificate = req
    "DescribeCertificate"
    "fixture/DescribeCertificate.yaml"

requestCreateComputer :: CreateComputer -> TestTree
requestCreateComputer = req
    "CreateComputer"
    "fixture/CreateComputer.yaml"

requestDescribeSharedDirectories :: DescribeSharedDirectories -> TestTree
requestDescribeSharedDirectories = req
    "DescribeSharedDirectories"
    "fixture/DescribeSharedDirectories.yaml"

requestEnableLDAPS :: EnableLDAPS -> TestTree
requestEnableLDAPS = req
    "EnableLDAPS"
    "fixture/EnableLDAPS.yaml"

requestDisableSso :: DisableSso -> TestTree
requestDisableSso = req
    "DisableSso"
    "fixture/DisableSso.yaml"

requestVerifyTrust :: VerifyTrust -> TestTree
requestVerifyTrust = req
    "VerifyTrust"
    "fixture/VerifyTrust.yaml"

requestRemoveRegion :: RemoveRegion -> TestTree
requestRemoveRegion = req
    "RemoveRegion"
    "fixture/RemoveRegion.yaml"

requestCreateConditionalForwarder :: CreateConditionalForwarder -> TestTree
requestCreateConditionalForwarder = req
    "CreateConditionalForwarder"
    "fixture/CreateConditionalForwarder.yaml"

requestDescribeRegions :: DescribeRegions -> TestTree
requestDescribeRegions = req
    "DescribeRegions"
    "fixture/DescribeRegions.yaml"

requestAddRegion :: AddRegion -> TestTree
requestAddRegion = req
    "AddRegion"
    "fixture/AddRegion.yaml"

requestGetDirectoryLimits :: GetDirectoryLimits -> TestTree
requestGetDirectoryLimits = req
    "GetDirectoryLimits"
    "fixture/GetDirectoryLimits.yaml"

requestUpdateRadius :: UpdateRadius -> TestTree
requestUpdateRadius = req
    "UpdateRadius"
    "fixture/UpdateRadius.yaml"

-- Responses

responseShareDirectory :: ShareDirectoryResponse -> TestTree
responseShareDirectory = res
    "ShareDirectoryResponse"
    "fixture/ShareDirectoryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ShareDirectory)

responseUpdateNumberOfDomainControllers :: UpdateNumberOfDomainControllersResponse -> TestTree
responseUpdateNumberOfDomainControllers = res
    "UpdateNumberOfDomainControllersResponse"
    "fixture/UpdateNumberOfDomainControllersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateNumberOfDomainControllers)

responseDescribeConditionalForwarders :: DescribeConditionalForwardersResponse -> TestTree
responseDescribeConditionalForwarders = res
    "DescribeConditionalForwardersResponse"
    "fixture/DescribeConditionalForwardersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeConditionalForwarders)

responseGetSnapshotLimits :: GetSnapshotLimitsResponse -> TestTree
responseGetSnapshotLimits = res
    "GetSnapshotLimitsResponse"
    "fixture/GetSnapshotLimitsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSnapshotLimits)

responseRegisterEventTopic :: RegisterEventTopicResponse -> TestTree
responseRegisterEventTopic = res
    "RegisterEventTopicResponse"
    "fixture/RegisterEventTopicResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RegisterEventTopic)

responseRegisterCertificate :: RegisterCertificateResponse -> TestTree
responseRegisterCertificate = res
    "RegisterCertificateResponse"
    "fixture/RegisterCertificateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RegisterCertificate)

responseConnectDirectory :: ConnectDirectoryResponse -> TestTree
responseConnectDirectory = res
    "ConnectDirectoryResponse"
    "fixture/ConnectDirectoryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ConnectDirectory)

responseDescribeLDAPSSettings :: DescribeLDAPSSettingsResponse -> TestTree
responseDescribeLDAPSSettings = res
    "DescribeLDAPSSettingsResponse"
    "fixture/DescribeLDAPSSettingsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeLDAPSSettings)

responseCreateAlias :: CreateAliasResponse -> TestTree
responseCreateAlias = res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateAlias)

responseDescribeDirectories :: DescribeDirectoriesResponse -> TestTree
responseDescribeDirectories = res
    "DescribeDirectoriesResponse"
    "fixture/DescribeDirectoriesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeDirectories)

responseAddIpRoutes :: AddIpRoutesResponse -> TestTree
responseAddIpRoutes = res
    "AddIpRoutesResponse"
    "fixture/AddIpRoutesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AddIpRoutes)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTagsForResource)

responseDescribeTrusts :: DescribeTrustsResponse -> TestTree
responseDescribeTrusts = res
    "DescribeTrustsResponse"
    "fixture/DescribeTrustsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeTrusts)

responseDeleteTrust :: DeleteTrustResponse -> TestTree
responseDeleteTrust = res
    "DeleteTrustResponse"
    "fixture/DeleteTrustResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteTrust)

responseUpdateTrust :: UpdateTrustResponse -> TestTree
responseUpdateTrust = res
    "UpdateTrustResponse"
    "fixture/UpdateTrustResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateTrust)

responseCreateMicrosoftAD :: CreateMicrosoftADResponse -> TestTree
responseCreateMicrosoftAD = res
    "CreateMicrosoftADResponse"
    "fixture/CreateMicrosoftADResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateMicrosoftAD)

responseDeregisterEventTopic :: DeregisterEventTopicResponse -> TestTree
responseDeregisterEventTopic = res
    "DeregisterEventTopicResponse"
    "fixture/DeregisterEventTopicResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeregisterEventTopic)

responseCreateDirectory :: CreateDirectoryResponse -> TestTree
responseCreateDirectory = res
    "CreateDirectoryResponse"
    "fixture/CreateDirectoryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateDirectory)

responseAcceptSharedDirectory :: AcceptSharedDirectoryResponse -> TestTree
responseAcceptSharedDirectory = res
    "AcceptSharedDirectoryResponse"
    "fixture/AcceptSharedDirectoryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AcceptSharedDirectory)

responseCreateLogSubscription :: CreateLogSubscriptionResponse -> TestTree
responseCreateLogSubscription = res
    "CreateLogSubscriptionResponse"
    "fixture/CreateLogSubscriptionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateLogSubscription)

responseRemoveTagsFromResource :: RemoveTagsFromResourceResponse -> TestTree
responseRemoveTagsFromResource = res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RemoveTagsFromResource)

responseDescribeEventTopics :: DescribeEventTopicsResponse -> TestTree
responseDescribeEventTopics = res
    "DescribeEventTopicsResponse"
    "fixture/DescribeEventTopicsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeEventTopics)

responseResetUserPassword :: ResetUserPasswordResponse -> TestTree
responseResetUserPassword = res
    "ResetUserPasswordResponse"
    "fixture/ResetUserPasswordResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ResetUserPassword)

responseUpdateConditionalForwarder :: UpdateConditionalForwarderResponse -> TestTree
responseUpdateConditionalForwarder = res
    "UpdateConditionalForwarderResponse"
    "fixture/UpdateConditionalForwarderResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateConditionalForwarder)

responseDeleteConditionalForwarder :: DeleteConditionalForwarderResponse -> TestTree
responseDeleteConditionalForwarder = res
    "DeleteConditionalForwarderResponse"
    "fixture/DeleteConditionalForwarderResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteConditionalForwarder)

responseDisableLDAPS :: DisableLDAPSResponse -> TestTree
responseDisableLDAPS = res
    "DisableLDAPSResponse"
    "fixture/DisableLDAPSResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisableLDAPS)

responseDeleteLogSubscription :: DeleteLogSubscriptionResponse -> TestTree
responseDeleteLogSubscription = res
    "DeleteLogSubscriptionResponse"
    "fixture/DeleteLogSubscriptionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteLogSubscription)

responseEnableSso :: EnableSsoResponse -> TestTree
responseEnableSso = res
    "EnableSsoResponse"
    "fixture/EnableSsoResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy EnableSso)

responseCancelSchemaExtension :: CancelSchemaExtensionResponse -> TestTree
responseCancelSchemaExtension = res
    "CancelSchemaExtensionResponse"
    "fixture/CancelSchemaExtensionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CancelSchemaExtension)

responseListLogSubscriptions :: ListLogSubscriptionsResponse -> TestTree
responseListLogSubscriptions = res
    "ListLogSubscriptionsResponse"
    "fixture/ListLogSubscriptionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListLogSubscriptions)

responseEnableRadius :: EnableRadiusResponse -> TestTree
responseEnableRadius = res
    "EnableRadiusResponse"
    "fixture/EnableRadiusResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy EnableRadius)

responseListIpRoutes :: ListIpRoutesResponse -> TestTree
responseListIpRoutes = res
    "ListIpRoutesResponse"
    "fixture/ListIpRoutesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListIpRoutes)

responseAddTagsToResource :: AddTagsToResourceResponse -> TestTree
responseAddTagsToResource = res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AddTagsToResource)

responseListSchemaExtensions :: ListSchemaExtensionsResponse -> TestTree
responseListSchemaExtensions = res
    "ListSchemaExtensionsResponse"
    "fixture/ListSchemaExtensionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListSchemaExtensions)

responseDisableRadius :: DisableRadiusResponse -> TestTree
responseDisableRadius = res
    "DisableRadiusResponse"
    "fixture/DisableRadiusResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisableRadius)

responseListCertificates :: ListCertificatesResponse -> TestTree
responseListCertificates = res
    "ListCertificatesResponse"
    "fixture/ListCertificatesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListCertificates)

responseRejectSharedDirectory :: RejectSharedDirectoryResponse -> TestTree
responseRejectSharedDirectory = res
    "RejectSharedDirectoryResponse"
    "fixture/RejectSharedDirectoryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RejectSharedDirectory)

responseUnshareDirectory :: UnshareDirectoryResponse -> TestTree
responseUnshareDirectory = res
    "UnshareDirectoryResponse"
    "fixture/UnshareDirectoryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UnshareDirectory)

responseRestoreFromSnapshot :: RestoreFromSnapshotResponse -> TestTree
responseRestoreFromSnapshot = res
    "RestoreFromSnapshotResponse"
    "fixture/RestoreFromSnapshotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RestoreFromSnapshot)

responseDescribeDomainControllers :: DescribeDomainControllersResponse -> TestTree
responseDescribeDomainControllers = res
    "DescribeDomainControllersResponse"
    "fixture/DescribeDomainControllersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeDomainControllers)

responseDescribeSnapshots :: DescribeSnapshotsResponse -> TestTree
responseDescribeSnapshots = res
    "DescribeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeSnapshots)

responseRemoveIpRoutes :: RemoveIpRoutesResponse -> TestTree
responseRemoveIpRoutes = res
    "RemoveIpRoutesResponse"
    "fixture/RemoveIpRoutesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RemoveIpRoutes)

responseDeleteSnapshot :: DeleteSnapshotResponse -> TestTree
responseDeleteSnapshot = res
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteSnapshot)

responseDeregisterCertificate :: DeregisterCertificateResponse -> TestTree
responseDeregisterCertificate = res
    "DeregisterCertificateResponse"
    "fixture/DeregisterCertificateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeregisterCertificate)

responseStartSchemaExtension :: StartSchemaExtensionResponse -> TestTree
responseStartSchemaExtension = res
    "StartSchemaExtensionResponse"
    "fixture/StartSchemaExtensionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartSchemaExtension)

responseCreateTrust :: CreateTrustResponse -> TestTree
responseCreateTrust = res
    "CreateTrustResponse"
    "fixture/CreateTrustResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateTrust)

responseDeleteDirectory :: DeleteDirectoryResponse -> TestTree
responseDeleteDirectory = res
    "DeleteDirectoryResponse"
    "fixture/DeleteDirectoryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteDirectory)

responseCreateSnapshot :: CreateSnapshotResponse -> TestTree
responseCreateSnapshot = res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateSnapshot)

responseDescribeCertificate :: DescribeCertificateResponse -> TestTree
responseDescribeCertificate = res
    "DescribeCertificateResponse"
    "fixture/DescribeCertificateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeCertificate)

responseCreateComputer :: CreateComputerResponse -> TestTree
responseCreateComputer = res
    "CreateComputerResponse"
    "fixture/CreateComputerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateComputer)

responseDescribeSharedDirectories :: DescribeSharedDirectoriesResponse -> TestTree
responseDescribeSharedDirectories = res
    "DescribeSharedDirectoriesResponse"
    "fixture/DescribeSharedDirectoriesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeSharedDirectories)

responseEnableLDAPS :: EnableLDAPSResponse -> TestTree
responseEnableLDAPS = res
    "EnableLDAPSResponse"
    "fixture/EnableLDAPSResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy EnableLDAPS)

responseDisableSso :: DisableSsoResponse -> TestTree
responseDisableSso = res
    "DisableSsoResponse"
    "fixture/DisableSsoResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisableSso)

responseVerifyTrust :: VerifyTrustResponse -> TestTree
responseVerifyTrust = res
    "VerifyTrustResponse"
    "fixture/VerifyTrustResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy VerifyTrust)

responseRemoveRegion :: RemoveRegionResponse -> TestTree
responseRemoveRegion = res
    "RemoveRegionResponse"
    "fixture/RemoveRegionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RemoveRegion)

responseCreateConditionalForwarder :: CreateConditionalForwarderResponse -> TestTree
responseCreateConditionalForwarder = res
    "CreateConditionalForwarderResponse"
    "fixture/CreateConditionalForwarderResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateConditionalForwarder)

responseDescribeRegions :: DescribeRegionsResponse -> TestTree
responseDescribeRegions = res
    "DescribeRegionsResponse"
    "fixture/DescribeRegionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeRegions)

responseAddRegion :: AddRegionResponse -> TestTree
responseAddRegion = res
    "AddRegionResponse"
    "fixture/AddRegionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AddRegion)

responseGetDirectoryLimits :: GetDirectoryLimitsResponse -> TestTree
responseGetDirectoryLimits = res
    "GetDirectoryLimitsResponse"
    "fixture/GetDirectoryLimitsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetDirectoryLimits)

responseUpdateRadius :: UpdateRadiusResponse -> TestTree
responseUpdateRadius = res
    "UpdateRadiusResponse"
    "fixture/UpdateRadiusResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateRadius)

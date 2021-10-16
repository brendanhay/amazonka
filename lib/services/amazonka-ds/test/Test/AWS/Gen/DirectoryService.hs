{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DirectoryService
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--         [ requestRejectSharedDirectory $
--             newRejectSharedDirectory
--
--         , requestConnectDirectory $
--             newConnectDirectory
--
--         , requestRegisterEventTopic $
--             newRegisterEventTopic
--
--         , requestDisableRadius $
--             newDisableRadius
--
--         , requestShareDirectory $
--             newShareDirectory
--
--         , requestListIpRoutes $
--             newListIpRoutes
--
--         , requestListSchemaExtensions $
--             newListSchemaExtensions
--
--         , requestDescribeClientAuthenticationSettings $
--             newDescribeClientAuthenticationSettings
--
--         , requestEnableRadius $
--             newEnableRadius
--
--         , requestAddRegion $
--             newAddRegion
--
--         , requestDeleteLogSubscription $
--             newDeleteLogSubscription
--
--         , requestRemoveRegion $
--             newRemoveRegion
--
--         , requestEnableSso $
--             newEnableSso
--
--         , requestCreateConditionalForwarder $
--             newCreateConditionalForwarder
--
--         , requestCancelSchemaExtension $
--             newCancelSchemaExtension
--
--         , requestRemoveTagsFromResource $
--             newRemoveTagsFromResource
--
--         , requestDeleteConditionalForwarder $
--             newDeleteConditionalForwarder
--
--         , requestVerifyTrust $
--             newVerifyTrust
--
--         , requestDescribeSharedDirectories $
--             newDescribeSharedDirectories
--
--         , requestUpdateConditionalForwarder $
--             newUpdateConditionalForwarder
--
--         , requestEnableLDAPS $
--             newEnableLDAPS
--
--         , requestCreateTrust $
--             newCreateTrust
--
--         , requestDescribeCertificate $
--             newDescribeCertificate
--
--         , requestDeleteDirectory $
--             newDeleteDirectory
--
--         , requestRemoveIpRoutes $
--             newRemoveIpRoutes
--
--         , requestUpdateTrust $
--             newUpdateTrust
--
--         , requestCreateMicrosoftAD $
--             newCreateMicrosoftAD
--
--         , requestCreateDirectory $
--             newCreateDirectory
--
--         , requestDeleteSnapshot $
--             newDeleteSnapshot
--
--         , requestDisableClientAuthentication $
--             newDisableClientAuthentication
--
--         , requestDeleteTrust $
--             newDeleteTrust
--
--         , requestRestoreFromSnapshot $
--             newRestoreFromSnapshot
--
--         , requestDescribeDomainControllers $
--             newDescribeDomainControllers
--
--         , requestDescribeSnapshots $
--             newDescribeSnapshots
--
--         , requestDescribeTrusts $
--             newDescribeTrusts
--
--         , requestUnshareDirectory $
--             newUnshareDirectory
--
--         , requestRegisterCertificate $
--             newRegisterCertificate
--
--         , requestGetSnapshotLimits $
--             newGetSnapshotLimits
--
--         , requestUpdateNumberOfDomainControllers $
--             newUpdateNumberOfDomainControllers
--
--         , requestListCertificates $
--             newListCertificates
--
--         , requestDescribeConditionalForwarders $
--             newDescribeConditionalForwarders
--
--         , requestAddTagsToResource $
--             newAddTagsToResource
--
--         , requestUpdateRadius $
--             newUpdateRadius
--
--         , requestGetDirectoryLimits $
--             newGetDirectoryLimits
--
--         , requestListLogSubscriptions $
--             newListLogSubscriptions
--
--         , requestDescribeRegions $
--             newDescribeRegions
--
--         , requestDisableLDAPS $
--             newDisableLDAPS
--
--         , requestCreateComputer $
--             newCreateComputer
--
--         , requestDisableSso $
--             newDisableSso
--
--         , requestCreateLogSubscription $
--             newCreateLogSubscription
--
--         , requestDescribeEventTopics $
--             newDescribeEventTopics
--
--         , requestResetUserPassword $
--             newResetUserPassword
--
--         , requestEnableClientAuthentication $
--             newEnableClientAuthentication
--
--         , requestStartSchemaExtension $
--             newStartSchemaExtension
--
--         , requestCreateSnapshot $
--             newCreateSnapshot
--
--         , requestAcceptSharedDirectory $
--             newAcceptSharedDirectory
--
--         , requestDeregisterEventTopic $
--             newDeregisterEventTopic
--
--         , requestDeregisterCertificate $
--             newDeregisterCertificate
--
--         , requestAddIpRoutes $
--             newAddIpRoutes
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDescribeDirectories $
--             newDescribeDirectories
--
--         , requestCreateAlias $
--             newCreateAlias
--
--         , requestDescribeLDAPSSettings $
--             newDescribeLDAPSSettings
--
--           ]

--     , testGroup "response"
--         [ responseRejectSharedDirectory $
--             newRejectSharedDirectoryResponse
--
--         , responseConnectDirectory $
--             newConnectDirectoryResponse
--
--         , responseRegisterEventTopic $
--             newRegisterEventTopicResponse
--
--         , responseDisableRadius $
--             newDisableRadiusResponse
--
--         , responseShareDirectory $
--             newShareDirectoryResponse
--
--         , responseListIpRoutes $
--             newListIpRoutesResponse
--
--         , responseListSchemaExtensions $
--             newListSchemaExtensionsResponse
--
--         , responseDescribeClientAuthenticationSettings $
--             newDescribeClientAuthenticationSettingsResponse
--
--         , responseEnableRadius $
--             newEnableRadiusResponse
--
--         , responseAddRegion $
--             newAddRegionResponse
--
--         , responseDeleteLogSubscription $
--             newDeleteLogSubscriptionResponse
--
--         , responseRemoveRegion $
--             newRemoveRegionResponse
--
--         , responseEnableSso $
--             newEnableSsoResponse
--
--         , responseCreateConditionalForwarder $
--             newCreateConditionalForwarderResponse
--
--         , responseCancelSchemaExtension $
--             newCancelSchemaExtensionResponse
--
--         , responseRemoveTagsFromResource $
--             newRemoveTagsFromResourceResponse
--
--         , responseDeleteConditionalForwarder $
--             newDeleteConditionalForwarderResponse
--
--         , responseVerifyTrust $
--             newVerifyTrustResponse
--
--         , responseDescribeSharedDirectories $
--             newDescribeSharedDirectoriesResponse
--
--         , responseUpdateConditionalForwarder $
--             newUpdateConditionalForwarderResponse
--
--         , responseEnableLDAPS $
--             newEnableLDAPSResponse
--
--         , responseCreateTrust $
--             newCreateTrustResponse
--
--         , responseDescribeCertificate $
--             newDescribeCertificateResponse
--
--         , responseDeleteDirectory $
--             newDeleteDirectoryResponse
--
--         , responseRemoveIpRoutes $
--             newRemoveIpRoutesResponse
--
--         , responseUpdateTrust $
--             newUpdateTrustResponse
--
--         , responseCreateMicrosoftAD $
--             newCreateMicrosoftADResponse
--
--         , responseCreateDirectory $
--             newCreateDirectoryResponse
--
--         , responseDeleteSnapshot $
--             newDeleteSnapshotResponse
--
--         , responseDisableClientAuthentication $
--             newDisableClientAuthenticationResponse
--
--         , responseDeleteTrust $
--             newDeleteTrustResponse
--
--         , responseRestoreFromSnapshot $
--             newRestoreFromSnapshotResponse
--
--         , responseDescribeDomainControllers $
--             newDescribeDomainControllersResponse
--
--         , responseDescribeSnapshots $
--             newDescribeSnapshotsResponse
--
--         , responseDescribeTrusts $
--             newDescribeTrustsResponse
--
--         , responseUnshareDirectory $
--             newUnshareDirectoryResponse
--
--         , responseRegisterCertificate $
--             newRegisterCertificateResponse
--
--         , responseGetSnapshotLimits $
--             newGetSnapshotLimitsResponse
--
--         , responseUpdateNumberOfDomainControllers $
--             newUpdateNumberOfDomainControllersResponse
--
--         , responseListCertificates $
--             newListCertificatesResponse
--
--         , responseDescribeConditionalForwarders $
--             newDescribeConditionalForwardersResponse
--
--         , responseAddTagsToResource $
--             newAddTagsToResourceResponse
--
--         , responseUpdateRadius $
--             newUpdateRadiusResponse
--
--         , responseGetDirectoryLimits $
--             newGetDirectoryLimitsResponse
--
--         , responseListLogSubscriptions $
--             newListLogSubscriptionsResponse
--
--         , responseDescribeRegions $
--             newDescribeRegionsResponse
--
--         , responseDisableLDAPS $
--             newDisableLDAPSResponse
--
--         , responseCreateComputer $
--             newCreateComputerResponse
--
--         , responseDisableSso $
--             newDisableSsoResponse
--
--         , responseCreateLogSubscription $
--             newCreateLogSubscriptionResponse
--
--         , responseDescribeEventTopics $
--             newDescribeEventTopicsResponse
--
--         , responseResetUserPassword $
--             newResetUserPasswordResponse
--
--         , responseEnableClientAuthentication $
--             newEnableClientAuthenticationResponse
--
--         , responseStartSchemaExtension $
--             newStartSchemaExtensionResponse
--
--         , responseCreateSnapshot $
--             newCreateSnapshotResponse
--
--         , responseAcceptSharedDirectory $
--             newAcceptSharedDirectoryResponse
--
--         , responseDeregisterEventTopic $
--             newDeregisterEventTopicResponse
--
--         , responseDeregisterCertificate $
--             newDeregisterCertificateResponse
--
--         , responseAddIpRoutes $
--             newAddIpRoutesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDescribeDirectories $
--             newDescribeDirectoriesResponse
--
--         , responseCreateAlias $
--             newCreateAliasResponse
--
--         , responseDescribeLDAPSSettings $
--             newDescribeLDAPSSettingsResponse
--
--           ]
--     ]

-- Requests

requestRejectSharedDirectory :: RejectSharedDirectory -> TestTree
requestRejectSharedDirectory =
  req
    "RejectSharedDirectory"
    "fixture/RejectSharedDirectory.yaml"

requestConnectDirectory :: ConnectDirectory -> TestTree
requestConnectDirectory =
  req
    "ConnectDirectory"
    "fixture/ConnectDirectory.yaml"

requestRegisterEventTopic :: RegisterEventTopic -> TestTree
requestRegisterEventTopic =
  req
    "RegisterEventTopic"
    "fixture/RegisterEventTopic.yaml"

requestDisableRadius :: DisableRadius -> TestTree
requestDisableRadius =
  req
    "DisableRadius"
    "fixture/DisableRadius.yaml"

requestShareDirectory :: ShareDirectory -> TestTree
requestShareDirectory =
  req
    "ShareDirectory"
    "fixture/ShareDirectory.yaml"

requestListIpRoutes :: ListIpRoutes -> TestTree
requestListIpRoutes =
  req
    "ListIpRoutes"
    "fixture/ListIpRoutes.yaml"

requestListSchemaExtensions :: ListSchemaExtensions -> TestTree
requestListSchemaExtensions =
  req
    "ListSchemaExtensions"
    "fixture/ListSchemaExtensions.yaml"

requestDescribeClientAuthenticationSettings :: DescribeClientAuthenticationSettings -> TestTree
requestDescribeClientAuthenticationSettings =
  req
    "DescribeClientAuthenticationSettings"
    "fixture/DescribeClientAuthenticationSettings.yaml"

requestEnableRadius :: EnableRadius -> TestTree
requestEnableRadius =
  req
    "EnableRadius"
    "fixture/EnableRadius.yaml"

requestAddRegion :: AddRegion -> TestTree
requestAddRegion =
  req
    "AddRegion"
    "fixture/AddRegion.yaml"

requestDeleteLogSubscription :: DeleteLogSubscription -> TestTree
requestDeleteLogSubscription =
  req
    "DeleteLogSubscription"
    "fixture/DeleteLogSubscription.yaml"

requestRemoveRegion :: RemoveRegion -> TestTree
requestRemoveRegion =
  req
    "RemoveRegion"
    "fixture/RemoveRegion.yaml"

requestEnableSso :: EnableSso -> TestTree
requestEnableSso =
  req
    "EnableSso"
    "fixture/EnableSso.yaml"

requestCreateConditionalForwarder :: CreateConditionalForwarder -> TestTree
requestCreateConditionalForwarder =
  req
    "CreateConditionalForwarder"
    "fixture/CreateConditionalForwarder.yaml"

requestCancelSchemaExtension :: CancelSchemaExtension -> TestTree
requestCancelSchemaExtension =
  req
    "CancelSchemaExtension"
    "fixture/CancelSchemaExtension.yaml"

requestRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
requestRemoveTagsFromResource =
  req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource.yaml"

requestDeleteConditionalForwarder :: DeleteConditionalForwarder -> TestTree
requestDeleteConditionalForwarder =
  req
    "DeleteConditionalForwarder"
    "fixture/DeleteConditionalForwarder.yaml"

requestVerifyTrust :: VerifyTrust -> TestTree
requestVerifyTrust =
  req
    "VerifyTrust"
    "fixture/VerifyTrust.yaml"

requestDescribeSharedDirectories :: DescribeSharedDirectories -> TestTree
requestDescribeSharedDirectories =
  req
    "DescribeSharedDirectories"
    "fixture/DescribeSharedDirectories.yaml"

requestUpdateConditionalForwarder :: UpdateConditionalForwarder -> TestTree
requestUpdateConditionalForwarder =
  req
    "UpdateConditionalForwarder"
    "fixture/UpdateConditionalForwarder.yaml"

requestEnableLDAPS :: EnableLDAPS -> TestTree
requestEnableLDAPS =
  req
    "EnableLDAPS"
    "fixture/EnableLDAPS.yaml"

requestCreateTrust :: CreateTrust -> TestTree
requestCreateTrust =
  req
    "CreateTrust"
    "fixture/CreateTrust.yaml"

requestDescribeCertificate :: DescribeCertificate -> TestTree
requestDescribeCertificate =
  req
    "DescribeCertificate"
    "fixture/DescribeCertificate.yaml"

requestDeleteDirectory :: DeleteDirectory -> TestTree
requestDeleteDirectory =
  req
    "DeleteDirectory"
    "fixture/DeleteDirectory.yaml"

requestRemoveIpRoutes :: RemoveIpRoutes -> TestTree
requestRemoveIpRoutes =
  req
    "RemoveIpRoutes"
    "fixture/RemoveIpRoutes.yaml"

requestUpdateTrust :: UpdateTrust -> TestTree
requestUpdateTrust =
  req
    "UpdateTrust"
    "fixture/UpdateTrust.yaml"

requestCreateMicrosoftAD :: CreateMicrosoftAD -> TestTree
requestCreateMicrosoftAD =
  req
    "CreateMicrosoftAD"
    "fixture/CreateMicrosoftAD.yaml"

requestCreateDirectory :: CreateDirectory -> TestTree
requestCreateDirectory =
  req
    "CreateDirectory"
    "fixture/CreateDirectory.yaml"

requestDeleteSnapshot :: DeleteSnapshot -> TestTree
requestDeleteSnapshot =
  req
    "DeleteSnapshot"
    "fixture/DeleteSnapshot.yaml"

requestDisableClientAuthentication :: DisableClientAuthentication -> TestTree
requestDisableClientAuthentication =
  req
    "DisableClientAuthentication"
    "fixture/DisableClientAuthentication.yaml"

requestDeleteTrust :: DeleteTrust -> TestTree
requestDeleteTrust =
  req
    "DeleteTrust"
    "fixture/DeleteTrust.yaml"

requestRestoreFromSnapshot :: RestoreFromSnapshot -> TestTree
requestRestoreFromSnapshot =
  req
    "RestoreFromSnapshot"
    "fixture/RestoreFromSnapshot.yaml"

requestDescribeDomainControllers :: DescribeDomainControllers -> TestTree
requestDescribeDomainControllers =
  req
    "DescribeDomainControllers"
    "fixture/DescribeDomainControllers.yaml"

requestDescribeSnapshots :: DescribeSnapshots -> TestTree
requestDescribeSnapshots =
  req
    "DescribeSnapshots"
    "fixture/DescribeSnapshots.yaml"

requestDescribeTrusts :: DescribeTrusts -> TestTree
requestDescribeTrusts =
  req
    "DescribeTrusts"
    "fixture/DescribeTrusts.yaml"

requestUnshareDirectory :: UnshareDirectory -> TestTree
requestUnshareDirectory =
  req
    "UnshareDirectory"
    "fixture/UnshareDirectory.yaml"

requestRegisterCertificate :: RegisterCertificate -> TestTree
requestRegisterCertificate =
  req
    "RegisterCertificate"
    "fixture/RegisterCertificate.yaml"

requestGetSnapshotLimits :: GetSnapshotLimits -> TestTree
requestGetSnapshotLimits =
  req
    "GetSnapshotLimits"
    "fixture/GetSnapshotLimits.yaml"

requestUpdateNumberOfDomainControllers :: UpdateNumberOfDomainControllers -> TestTree
requestUpdateNumberOfDomainControllers =
  req
    "UpdateNumberOfDomainControllers"
    "fixture/UpdateNumberOfDomainControllers.yaml"

requestListCertificates :: ListCertificates -> TestTree
requestListCertificates =
  req
    "ListCertificates"
    "fixture/ListCertificates.yaml"

requestDescribeConditionalForwarders :: DescribeConditionalForwarders -> TestTree
requestDescribeConditionalForwarders =
  req
    "DescribeConditionalForwarders"
    "fixture/DescribeConditionalForwarders.yaml"

requestAddTagsToResource :: AddTagsToResource -> TestTree
requestAddTagsToResource =
  req
    "AddTagsToResource"
    "fixture/AddTagsToResource.yaml"

requestUpdateRadius :: UpdateRadius -> TestTree
requestUpdateRadius =
  req
    "UpdateRadius"
    "fixture/UpdateRadius.yaml"

requestGetDirectoryLimits :: GetDirectoryLimits -> TestTree
requestGetDirectoryLimits =
  req
    "GetDirectoryLimits"
    "fixture/GetDirectoryLimits.yaml"

requestListLogSubscriptions :: ListLogSubscriptions -> TestTree
requestListLogSubscriptions =
  req
    "ListLogSubscriptions"
    "fixture/ListLogSubscriptions.yaml"

requestDescribeRegions :: DescribeRegions -> TestTree
requestDescribeRegions =
  req
    "DescribeRegions"
    "fixture/DescribeRegions.yaml"

requestDisableLDAPS :: DisableLDAPS -> TestTree
requestDisableLDAPS =
  req
    "DisableLDAPS"
    "fixture/DisableLDAPS.yaml"

requestCreateComputer :: CreateComputer -> TestTree
requestCreateComputer =
  req
    "CreateComputer"
    "fixture/CreateComputer.yaml"

requestDisableSso :: DisableSso -> TestTree
requestDisableSso =
  req
    "DisableSso"
    "fixture/DisableSso.yaml"

requestCreateLogSubscription :: CreateLogSubscription -> TestTree
requestCreateLogSubscription =
  req
    "CreateLogSubscription"
    "fixture/CreateLogSubscription.yaml"

requestDescribeEventTopics :: DescribeEventTopics -> TestTree
requestDescribeEventTopics =
  req
    "DescribeEventTopics"
    "fixture/DescribeEventTopics.yaml"

requestResetUserPassword :: ResetUserPassword -> TestTree
requestResetUserPassword =
  req
    "ResetUserPassword"
    "fixture/ResetUserPassword.yaml"

requestEnableClientAuthentication :: EnableClientAuthentication -> TestTree
requestEnableClientAuthentication =
  req
    "EnableClientAuthentication"
    "fixture/EnableClientAuthentication.yaml"

requestStartSchemaExtension :: StartSchemaExtension -> TestTree
requestStartSchemaExtension =
  req
    "StartSchemaExtension"
    "fixture/StartSchemaExtension.yaml"

requestCreateSnapshot :: CreateSnapshot -> TestTree
requestCreateSnapshot =
  req
    "CreateSnapshot"
    "fixture/CreateSnapshot.yaml"

requestAcceptSharedDirectory :: AcceptSharedDirectory -> TestTree
requestAcceptSharedDirectory =
  req
    "AcceptSharedDirectory"
    "fixture/AcceptSharedDirectory.yaml"

requestDeregisterEventTopic :: DeregisterEventTopic -> TestTree
requestDeregisterEventTopic =
  req
    "DeregisterEventTopic"
    "fixture/DeregisterEventTopic.yaml"

requestDeregisterCertificate :: DeregisterCertificate -> TestTree
requestDeregisterCertificate =
  req
    "DeregisterCertificate"
    "fixture/DeregisterCertificate.yaml"

requestAddIpRoutes :: AddIpRoutes -> TestTree
requestAddIpRoutes =
  req
    "AddIpRoutes"
    "fixture/AddIpRoutes.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDescribeDirectories :: DescribeDirectories -> TestTree
requestDescribeDirectories =
  req
    "DescribeDirectories"
    "fixture/DescribeDirectories.yaml"

requestCreateAlias :: CreateAlias -> TestTree
requestCreateAlias =
  req
    "CreateAlias"
    "fixture/CreateAlias.yaml"

requestDescribeLDAPSSettings :: DescribeLDAPSSettings -> TestTree
requestDescribeLDAPSSettings =
  req
    "DescribeLDAPSSettings"
    "fixture/DescribeLDAPSSettings.yaml"

-- Responses

responseRejectSharedDirectory :: RejectSharedDirectoryResponse -> TestTree
responseRejectSharedDirectory =
  res
    "RejectSharedDirectoryResponse"
    "fixture/RejectSharedDirectoryResponse.proto"
    defaultService
    (Proxy :: Proxy RejectSharedDirectory)

responseConnectDirectory :: ConnectDirectoryResponse -> TestTree
responseConnectDirectory =
  res
    "ConnectDirectoryResponse"
    "fixture/ConnectDirectoryResponse.proto"
    defaultService
    (Proxy :: Proxy ConnectDirectory)

responseRegisterEventTopic :: RegisterEventTopicResponse -> TestTree
responseRegisterEventTopic =
  res
    "RegisterEventTopicResponse"
    "fixture/RegisterEventTopicResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterEventTopic)

responseDisableRadius :: DisableRadiusResponse -> TestTree
responseDisableRadius =
  res
    "DisableRadiusResponse"
    "fixture/DisableRadiusResponse.proto"
    defaultService
    (Proxy :: Proxy DisableRadius)

responseShareDirectory :: ShareDirectoryResponse -> TestTree
responseShareDirectory =
  res
    "ShareDirectoryResponse"
    "fixture/ShareDirectoryResponse.proto"
    defaultService
    (Proxy :: Proxy ShareDirectory)

responseListIpRoutes :: ListIpRoutesResponse -> TestTree
responseListIpRoutes =
  res
    "ListIpRoutesResponse"
    "fixture/ListIpRoutesResponse.proto"
    defaultService
    (Proxy :: Proxy ListIpRoutes)

responseListSchemaExtensions :: ListSchemaExtensionsResponse -> TestTree
responseListSchemaExtensions =
  res
    "ListSchemaExtensionsResponse"
    "fixture/ListSchemaExtensionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSchemaExtensions)

responseDescribeClientAuthenticationSettings :: DescribeClientAuthenticationSettingsResponse -> TestTree
responseDescribeClientAuthenticationSettings =
  res
    "DescribeClientAuthenticationSettingsResponse"
    "fixture/DescribeClientAuthenticationSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeClientAuthenticationSettings)

responseEnableRadius :: EnableRadiusResponse -> TestTree
responseEnableRadius =
  res
    "EnableRadiusResponse"
    "fixture/EnableRadiusResponse.proto"
    defaultService
    (Proxy :: Proxy EnableRadius)

responseAddRegion :: AddRegionResponse -> TestTree
responseAddRegion =
  res
    "AddRegionResponse"
    "fixture/AddRegionResponse.proto"
    defaultService
    (Proxy :: Proxy AddRegion)

responseDeleteLogSubscription :: DeleteLogSubscriptionResponse -> TestTree
responseDeleteLogSubscription =
  res
    "DeleteLogSubscriptionResponse"
    "fixture/DeleteLogSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLogSubscription)

responseRemoveRegion :: RemoveRegionResponse -> TestTree
responseRemoveRegion =
  res
    "RemoveRegionResponse"
    "fixture/RemoveRegionResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveRegion)

responseEnableSso :: EnableSsoResponse -> TestTree
responseEnableSso =
  res
    "EnableSsoResponse"
    "fixture/EnableSsoResponse.proto"
    defaultService
    (Proxy :: Proxy EnableSso)

responseCreateConditionalForwarder :: CreateConditionalForwarderResponse -> TestTree
responseCreateConditionalForwarder =
  res
    "CreateConditionalForwarderResponse"
    "fixture/CreateConditionalForwarderResponse.proto"
    defaultService
    (Proxy :: Proxy CreateConditionalForwarder)

responseCancelSchemaExtension :: CancelSchemaExtensionResponse -> TestTree
responseCancelSchemaExtension =
  res
    "CancelSchemaExtensionResponse"
    "fixture/CancelSchemaExtensionResponse.proto"
    defaultService
    (Proxy :: Proxy CancelSchemaExtension)

responseRemoveTagsFromResource :: RemoveTagsFromResourceResponse -> TestTree
responseRemoveTagsFromResource =
  res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveTagsFromResource)

responseDeleteConditionalForwarder :: DeleteConditionalForwarderResponse -> TestTree
responseDeleteConditionalForwarder =
  res
    "DeleteConditionalForwarderResponse"
    "fixture/DeleteConditionalForwarderResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConditionalForwarder)

responseVerifyTrust :: VerifyTrustResponse -> TestTree
responseVerifyTrust =
  res
    "VerifyTrustResponse"
    "fixture/VerifyTrustResponse.proto"
    defaultService
    (Proxy :: Proxy VerifyTrust)

responseDescribeSharedDirectories :: DescribeSharedDirectoriesResponse -> TestTree
responseDescribeSharedDirectories =
  res
    "DescribeSharedDirectoriesResponse"
    "fixture/DescribeSharedDirectoriesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSharedDirectories)

responseUpdateConditionalForwarder :: UpdateConditionalForwarderResponse -> TestTree
responseUpdateConditionalForwarder =
  res
    "UpdateConditionalForwarderResponse"
    "fixture/UpdateConditionalForwarderResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateConditionalForwarder)

responseEnableLDAPS :: EnableLDAPSResponse -> TestTree
responseEnableLDAPS =
  res
    "EnableLDAPSResponse"
    "fixture/EnableLDAPSResponse.proto"
    defaultService
    (Proxy :: Proxy EnableLDAPS)

responseCreateTrust :: CreateTrustResponse -> TestTree
responseCreateTrust =
  res
    "CreateTrustResponse"
    "fixture/CreateTrustResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTrust)

responseDescribeCertificate :: DescribeCertificateResponse -> TestTree
responseDescribeCertificate =
  res
    "DescribeCertificateResponse"
    "fixture/DescribeCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCertificate)

responseDeleteDirectory :: DeleteDirectoryResponse -> TestTree
responseDeleteDirectory =
  res
    "DeleteDirectoryResponse"
    "fixture/DeleteDirectoryResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDirectory)

responseRemoveIpRoutes :: RemoveIpRoutesResponse -> TestTree
responseRemoveIpRoutes =
  res
    "RemoveIpRoutesResponse"
    "fixture/RemoveIpRoutesResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveIpRoutes)

responseUpdateTrust :: UpdateTrustResponse -> TestTree
responseUpdateTrust =
  res
    "UpdateTrustResponse"
    "fixture/UpdateTrustResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateTrust)

responseCreateMicrosoftAD :: CreateMicrosoftADResponse -> TestTree
responseCreateMicrosoftAD =
  res
    "CreateMicrosoftADResponse"
    "fixture/CreateMicrosoftADResponse.proto"
    defaultService
    (Proxy :: Proxy CreateMicrosoftAD)

responseCreateDirectory :: CreateDirectoryResponse -> TestTree
responseCreateDirectory =
  res
    "CreateDirectoryResponse"
    "fixture/CreateDirectoryResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDirectory)

responseDeleteSnapshot :: DeleteSnapshotResponse -> TestTree
responseDeleteSnapshot =
  res
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSnapshot)

responseDisableClientAuthentication :: DisableClientAuthenticationResponse -> TestTree
responseDisableClientAuthentication =
  res
    "DisableClientAuthenticationResponse"
    "fixture/DisableClientAuthenticationResponse.proto"
    defaultService
    (Proxy :: Proxy DisableClientAuthentication)

responseDeleteTrust :: DeleteTrustResponse -> TestTree
responseDeleteTrust =
  res
    "DeleteTrustResponse"
    "fixture/DeleteTrustResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTrust)

responseRestoreFromSnapshot :: RestoreFromSnapshotResponse -> TestTree
responseRestoreFromSnapshot =
  res
    "RestoreFromSnapshotResponse"
    "fixture/RestoreFromSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy RestoreFromSnapshot)

responseDescribeDomainControllers :: DescribeDomainControllersResponse -> TestTree
responseDescribeDomainControllers =
  res
    "DescribeDomainControllersResponse"
    "fixture/DescribeDomainControllersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDomainControllers)

responseDescribeSnapshots :: DescribeSnapshotsResponse -> TestTree
responseDescribeSnapshots =
  res
    "DescribeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSnapshots)

responseDescribeTrusts :: DescribeTrustsResponse -> TestTree
responseDescribeTrusts =
  res
    "DescribeTrustsResponse"
    "fixture/DescribeTrustsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTrusts)

responseUnshareDirectory :: UnshareDirectoryResponse -> TestTree
responseUnshareDirectory =
  res
    "UnshareDirectoryResponse"
    "fixture/UnshareDirectoryResponse.proto"
    defaultService
    (Proxy :: Proxy UnshareDirectory)

responseRegisterCertificate :: RegisterCertificateResponse -> TestTree
responseRegisterCertificate =
  res
    "RegisterCertificateResponse"
    "fixture/RegisterCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterCertificate)

responseGetSnapshotLimits :: GetSnapshotLimitsResponse -> TestTree
responseGetSnapshotLimits =
  res
    "GetSnapshotLimitsResponse"
    "fixture/GetSnapshotLimitsResponse.proto"
    defaultService
    (Proxy :: Proxy GetSnapshotLimits)

responseUpdateNumberOfDomainControllers :: UpdateNumberOfDomainControllersResponse -> TestTree
responseUpdateNumberOfDomainControllers =
  res
    "UpdateNumberOfDomainControllersResponse"
    "fixture/UpdateNumberOfDomainControllersResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateNumberOfDomainControllers)

responseListCertificates :: ListCertificatesResponse -> TestTree
responseListCertificates =
  res
    "ListCertificatesResponse"
    "fixture/ListCertificatesResponse.proto"
    defaultService
    (Proxy :: Proxy ListCertificates)

responseDescribeConditionalForwarders :: DescribeConditionalForwardersResponse -> TestTree
responseDescribeConditionalForwarders =
  res
    "DescribeConditionalForwardersResponse"
    "fixture/DescribeConditionalForwardersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConditionalForwarders)

responseAddTagsToResource :: AddTagsToResourceResponse -> TestTree
responseAddTagsToResource =
  res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    defaultService
    (Proxy :: Proxy AddTagsToResource)

responseUpdateRadius :: UpdateRadiusResponse -> TestTree
responseUpdateRadius =
  res
    "UpdateRadiusResponse"
    "fixture/UpdateRadiusResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRadius)

responseGetDirectoryLimits :: GetDirectoryLimitsResponse -> TestTree
responseGetDirectoryLimits =
  res
    "GetDirectoryLimitsResponse"
    "fixture/GetDirectoryLimitsResponse.proto"
    defaultService
    (Proxy :: Proxy GetDirectoryLimits)

responseListLogSubscriptions :: ListLogSubscriptionsResponse -> TestTree
responseListLogSubscriptions =
  res
    "ListLogSubscriptionsResponse"
    "fixture/ListLogSubscriptionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListLogSubscriptions)

responseDescribeRegions :: DescribeRegionsResponse -> TestTree
responseDescribeRegions =
  res
    "DescribeRegionsResponse"
    "fixture/DescribeRegionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeRegions)

responseDisableLDAPS :: DisableLDAPSResponse -> TestTree
responseDisableLDAPS =
  res
    "DisableLDAPSResponse"
    "fixture/DisableLDAPSResponse.proto"
    defaultService
    (Proxy :: Proxy DisableLDAPS)

responseCreateComputer :: CreateComputerResponse -> TestTree
responseCreateComputer =
  res
    "CreateComputerResponse"
    "fixture/CreateComputerResponse.proto"
    defaultService
    (Proxy :: Proxy CreateComputer)

responseDisableSso :: DisableSsoResponse -> TestTree
responseDisableSso =
  res
    "DisableSsoResponse"
    "fixture/DisableSsoResponse.proto"
    defaultService
    (Proxy :: Proxy DisableSso)

responseCreateLogSubscription :: CreateLogSubscriptionResponse -> TestTree
responseCreateLogSubscription =
  res
    "CreateLogSubscriptionResponse"
    "fixture/CreateLogSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateLogSubscription)

responseDescribeEventTopics :: DescribeEventTopicsResponse -> TestTree
responseDescribeEventTopics =
  res
    "DescribeEventTopicsResponse"
    "fixture/DescribeEventTopicsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEventTopics)

responseResetUserPassword :: ResetUserPasswordResponse -> TestTree
responseResetUserPassword =
  res
    "ResetUserPasswordResponse"
    "fixture/ResetUserPasswordResponse.proto"
    defaultService
    (Proxy :: Proxy ResetUserPassword)

responseEnableClientAuthentication :: EnableClientAuthenticationResponse -> TestTree
responseEnableClientAuthentication =
  res
    "EnableClientAuthenticationResponse"
    "fixture/EnableClientAuthenticationResponse.proto"
    defaultService
    (Proxy :: Proxy EnableClientAuthentication)

responseStartSchemaExtension :: StartSchemaExtensionResponse -> TestTree
responseStartSchemaExtension =
  res
    "StartSchemaExtensionResponse"
    "fixture/StartSchemaExtensionResponse.proto"
    defaultService
    (Proxy :: Proxy StartSchemaExtension)

responseCreateSnapshot :: CreateSnapshotResponse -> TestTree
responseCreateSnapshot =
  res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSnapshot)

responseAcceptSharedDirectory :: AcceptSharedDirectoryResponse -> TestTree
responseAcceptSharedDirectory =
  res
    "AcceptSharedDirectoryResponse"
    "fixture/AcceptSharedDirectoryResponse.proto"
    defaultService
    (Proxy :: Proxy AcceptSharedDirectory)

responseDeregisterEventTopic :: DeregisterEventTopicResponse -> TestTree
responseDeregisterEventTopic =
  res
    "DeregisterEventTopicResponse"
    "fixture/DeregisterEventTopicResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterEventTopic)

responseDeregisterCertificate :: DeregisterCertificateResponse -> TestTree
responseDeregisterCertificate =
  res
    "DeregisterCertificateResponse"
    "fixture/DeregisterCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterCertificate)

responseAddIpRoutes :: AddIpRoutesResponse -> TestTree
responseAddIpRoutes =
  res
    "AddIpRoutesResponse"
    "fixture/AddIpRoutesResponse.proto"
    defaultService
    (Proxy :: Proxy AddIpRoutes)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseDescribeDirectories :: DescribeDirectoriesResponse -> TestTree
responseDescribeDirectories =
  res
    "DescribeDirectoriesResponse"
    "fixture/DescribeDirectoriesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDirectories)

responseCreateAlias :: CreateAliasResponse -> TestTree
responseCreateAlias =
  res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAlias)

responseDescribeLDAPSSettings :: DescribeLDAPSSettingsResponse -> TestTree
responseDescribeLDAPSSettings =
  res
    "DescribeLDAPSSettingsResponse"
    "fixture/DescribeLDAPSSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLDAPSSettings)

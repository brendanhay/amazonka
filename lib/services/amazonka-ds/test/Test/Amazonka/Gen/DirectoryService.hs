{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.DirectoryService
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.DirectoryService where

import Amazonka.DirectoryService
import qualified Data.Proxy as Proxy
import Test.Amazonka.DirectoryService.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAcceptSharedDirectory $
--             newAcceptSharedDirectory
--
--         , requestAddIpRoutes $
--             newAddIpRoutes
--
--         , requestAddRegion $
--             newAddRegion
--
--         , requestAddTagsToResource $
--             newAddTagsToResource
--
--         , requestCancelSchemaExtension $
--             newCancelSchemaExtension
--
--         , requestConnectDirectory $
--             newConnectDirectory
--
--         , requestCreateAlias $
--             newCreateAlias
--
--         , requestCreateComputer $
--             newCreateComputer
--
--         , requestCreateConditionalForwarder $
--             newCreateConditionalForwarder
--
--         , requestCreateDirectory $
--             newCreateDirectory
--
--         , requestCreateLogSubscription $
--             newCreateLogSubscription
--
--         , requestCreateMicrosoftAD $
--             newCreateMicrosoftAD
--
--         , requestCreateSnapshot $
--             newCreateSnapshot
--
--         , requestCreateTrust $
--             newCreateTrust
--
--         , requestDeleteConditionalForwarder $
--             newDeleteConditionalForwarder
--
--         , requestDeleteDirectory $
--             newDeleteDirectory
--
--         , requestDeleteLogSubscription $
--             newDeleteLogSubscription
--
--         , requestDeleteSnapshot $
--             newDeleteSnapshot
--
--         , requestDeleteTrust $
--             newDeleteTrust
--
--         , requestDeregisterCertificate $
--             newDeregisterCertificate
--
--         , requestDeregisterEventTopic $
--             newDeregisterEventTopic
--
--         , requestDescribeCertificate $
--             newDescribeCertificate
--
--         , requestDescribeClientAuthenticationSettings $
--             newDescribeClientAuthenticationSettings
--
--         , requestDescribeConditionalForwarders $
--             newDescribeConditionalForwarders
--
--         , requestDescribeDirectories $
--             newDescribeDirectories
--
--         , requestDescribeDomainControllers $
--             newDescribeDomainControllers
--
--         , requestDescribeEventTopics $
--             newDescribeEventTopics
--
--         , requestDescribeLDAPSSettings $
--             newDescribeLDAPSSettings
--
--         , requestDescribeRegions $
--             newDescribeRegions
--
--         , requestDescribeSettings $
--             newDescribeSettings
--
--         , requestDescribeSharedDirectories $
--             newDescribeSharedDirectories
--
--         , requestDescribeSnapshots $
--             newDescribeSnapshots
--
--         , requestDescribeTrusts $
--             newDescribeTrusts
--
--         , requestDescribeUpdateDirectory $
--             newDescribeUpdateDirectory
--
--         , requestDisableClientAuthentication $
--             newDisableClientAuthentication
--
--         , requestDisableLDAPS $
--             newDisableLDAPS
--
--         , requestDisableRadius $
--             newDisableRadius
--
--         , requestDisableSso $
--             newDisableSso
--
--         , requestEnableClientAuthentication $
--             newEnableClientAuthentication
--
--         , requestEnableLDAPS $
--             newEnableLDAPS
--
--         , requestEnableRadius $
--             newEnableRadius
--
--         , requestEnableSso $
--             newEnableSso
--
--         , requestGetDirectoryLimits $
--             newGetDirectoryLimits
--
--         , requestGetSnapshotLimits $
--             newGetSnapshotLimits
--
--         , requestListCertificates $
--             newListCertificates
--
--         , requestListIpRoutes $
--             newListIpRoutes
--
--         , requestListLogSubscriptions $
--             newListLogSubscriptions
--
--         , requestListSchemaExtensions $
--             newListSchemaExtensions
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestRegisterCertificate $
--             newRegisterCertificate
--
--         , requestRegisterEventTopic $
--             newRegisterEventTopic
--
--         , requestRejectSharedDirectory $
--             newRejectSharedDirectory
--
--         , requestRemoveIpRoutes $
--             newRemoveIpRoutes
--
--         , requestRemoveRegion $
--             newRemoveRegion
--
--         , requestRemoveTagsFromResource $
--             newRemoveTagsFromResource
--
--         , requestResetUserPassword $
--             newResetUserPassword
--
--         , requestRestoreFromSnapshot $
--             newRestoreFromSnapshot
--
--         , requestShareDirectory $
--             newShareDirectory
--
--         , requestStartSchemaExtension $
--             newStartSchemaExtension
--
--         , requestUnshareDirectory $
--             newUnshareDirectory
--
--         , requestUpdateConditionalForwarder $
--             newUpdateConditionalForwarder
--
--         , requestUpdateDirectorySetup $
--             newUpdateDirectorySetup
--
--         , requestUpdateNumberOfDomainControllers $
--             newUpdateNumberOfDomainControllers
--
--         , requestUpdateRadius $
--             newUpdateRadius
--
--         , requestUpdateSettings $
--             newUpdateSettings
--
--         , requestUpdateTrust $
--             newUpdateTrust
--
--         , requestVerifyTrust $
--             newVerifyTrust
--
--           ]

--     , testGroup "response"
--         [ responseAcceptSharedDirectory $
--             newAcceptSharedDirectoryResponse
--
--         , responseAddIpRoutes $
--             newAddIpRoutesResponse
--
--         , responseAddRegion $
--             newAddRegionResponse
--
--         , responseAddTagsToResource $
--             newAddTagsToResourceResponse
--
--         , responseCancelSchemaExtension $
--             newCancelSchemaExtensionResponse
--
--         , responseConnectDirectory $
--             newConnectDirectoryResponse
--
--         , responseCreateAlias $
--             newCreateAliasResponse
--
--         , responseCreateComputer $
--             newCreateComputerResponse
--
--         , responseCreateConditionalForwarder $
--             newCreateConditionalForwarderResponse
--
--         , responseCreateDirectory $
--             newCreateDirectoryResponse
--
--         , responseCreateLogSubscription $
--             newCreateLogSubscriptionResponse
--
--         , responseCreateMicrosoftAD $
--             newCreateMicrosoftADResponse
--
--         , responseCreateSnapshot $
--             newCreateSnapshotResponse
--
--         , responseCreateTrust $
--             newCreateTrustResponse
--
--         , responseDeleteConditionalForwarder $
--             newDeleteConditionalForwarderResponse
--
--         , responseDeleteDirectory $
--             newDeleteDirectoryResponse
--
--         , responseDeleteLogSubscription $
--             newDeleteLogSubscriptionResponse
--
--         , responseDeleteSnapshot $
--             newDeleteSnapshotResponse
--
--         , responseDeleteTrust $
--             newDeleteTrustResponse
--
--         , responseDeregisterCertificate $
--             newDeregisterCertificateResponse
--
--         , responseDeregisterEventTopic $
--             newDeregisterEventTopicResponse
--
--         , responseDescribeCertificate $
--             newDescribeCertificateResponse
--
--         , responseDescribeClientAuthenticationSettings $
--             newDescribeClientAuthenticationSettingsResponse
--
--         , responseDescribeConditionalForwarders $
--             newDescribeConditionalForwardersResponse
--
--         , responseDescribeDirectories $
--             newDescribeDirectoriesResponse
--
--         , responseDescribeDomainControllers $
--             newDescribeDomainControllersResponse
--
--         , responseDescribeEventTopics $
--             newDescribeEventTopicsResponse
--
--         , responseDescribeLDAPSSettings $
--             newDescribeLDAPSSettingsResponse
--
--         , responseDescribeRegions $
--             newDescribeRegionsResponse
--
--         , responseDescribeSettings $
--             newDescribeSettingsResponse
--
--         , responseDescribeSharedDirectories $
--             newDescribeSharedDirectoriesResponse
--
--         , responseDescribeSnapshots $
--             newDescribeSnapshotsResponse
--
--         , responseDescribeTrusts $
--             newDescribeTrustsResponse
--
--         , responseDescribeUpdateDirectory $
--             newDescribeUpdateDirectoryResponse
--
--         , responseDisableClientAuthentication $
--             newDisableClientAuthenticationResponse
--
--         , responseDisableLDAPS $
--             newDisableLDAPSResponse
--
--         , responseDisableRadius $
--             newDisableRadiusResponse
--
--         , responseDisableSso $
--             newDisableSsoResponse
--
--         , responseEnableClientAuthentication $
--             newEnableClientAuthenticationResponse
--
--         , responseEnableLDAPS $
--             newEnableLDAPSResponse
--
--         , responseEnableRadius $
--             newEnableRadiusResponse
--
--         , responseEnableSso $
--             newEnableSsoResponse
--
--         , responseGetDirectoryLimits $
--             newGetDirectoryLimitsResponse
--
--         , responseGetSnapshotLimits $
--             newGetSnapshotLimitsResponse
--
--         , responseListCertificates $
--             newListCertificatesResponse
--
--         , responseListIpRoutes $
--             newListIpRoutesResponse
--
--         , responseListLogSubscriptions $
--             newListLogSubscriptionsResponse
--
--         , responseListSchemaExtensions $
--             newListSchemaExtensionsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseRegisterCertificate $
--             newRegisterCertificateResponse
--
--         , responseRegisterEventTopic $
--             newRegisterEventTopicResponse
--
--         , responseRejectSharedDirectory $
--             newRejectSharedDirectoryResponse
--
--         , responseRemoveIpRoutes $
--             newRemoveIpRoutesResponse
--
--         , responseRemoveRegion $
--             newRemoveRegionResponse
--
--         , responseRemoveTagsFromResource $
--             newRemoveTagsFromResourceResponse
--
--         , responseResetUserPassword $
--             newResetUserPasswordResponse
--
--         , responseRestoreFromSnapshot $
--             newRestoreFromSnapshotResponse
--
--         , responseShareDirectory $
--             newShareDirectoryResponse
--
--         , responseStartSchemaExtension $
--             newStartSchemaExtensionResponse
--
--         , responseUnshareDirectory $
--             newUnshareDirectoryResponse
--
--         , responseUpdateConditionalForwarder $
--             newUpdateConditionalForwarderResponse
--
--         , responseUpdateDirectorySetup $
--             newUpdateDirectorySetupResponse
--
--         , responseUpdateNumberOfDomainControllers $
--             newUpdateNumberOfDomainControllersResponse
--
--         , responseUpdateRadius $
--             newUpdateRadiusResponse
--
--         , responseUpdateSettings $
--             newUpdateSettingsResponse
--
--         , responseUpdateTrust $
--             newUpdateTrustResponse
--
--         , responseVerifyTrust $
--             newVerifyTrustResponse
--
--           ]
--     ]

-- Requests

requestAcceptSharedDirectory :: AcceptSharedDirectory -> TestTree
requestAcceptSharedDirectory =
  req
    "AcceptSharedDirectory"
    "fixture/AcceptSharedDirectory.yaml"

requestAddIpRoutes :: AddIpRoutes -> TestTree
requestAddIpRoutes =
  req
    "AddIpRoutes"
    "fixture/AddIpRoutes.yaml"

requestAddRegion :: AddRegion -> TestTree
requestAddRegion =
  req
    "AddRegion"
    "fixture/AddRegion.yaml"

requestAddTagsToResource :: AddTagsToResource -> TestTree
requestAddTagsToResource =
  req
    "AddTagsToResource"
    "fixture/AddTagsToResource.yaml"

requestCancelSchemaExtension :: CancelSchemaExtension -> TestTree
requestCancelSchemaExtension =
  req
    "CancelSchemaExtension"
    "fixture/CancelSchemaExtension.yaml"

requestConnectDirectory :: ConnectDirectory -> TestTree
requestConnectDirectory =
  req
    "ConnectDirectory"
    "fixture/ConnectDirectory.yaml"

requestCreateAlias :: CreateAlias -> TestTree
requestCreateAlias =
  req
    "CreateAlias"
    "fixture/CreateAlias.yaml"

requestCreateComputer :: CreateComputer -> TestTree
requestCreateComputer =
  req
    "CreateComputer"
    "fixture/CreateComputer.yaml"

requestCreateConditionalForwarder :: CreateConditionalForwarder -> TestTree
requestCreateConditionalForwarder =
  req
    "CreateConditionalForwarder"
    "fixture/CreateConditionalForwarder.yaml"

requestCreateDirectory :: CreateDirectory -> TestTree
requestCreateDirectory =
  req
    "CreateDirectory"
    "fixture/CreateDirectory.yaml"

requestCreateLogSubscription :: CreateLogSubscription -> TestTree
requestCreateLogSubscription =
  req
    "CreateLogSubscription"
    "fixture/CreateLogSubscription.yaml"

requestCreateMicrosoftAD :: CreateMicrosoftAD -> TestTree
requestCreateMicrosoftAD =
  req
    "CreateMicrosoftAD"
    "fixture/CreateMicrosoftAD.yaml"

requestCreateSnapshot :: CreateSnapshot -> TestTree
requestCreateSnapshot =
  req
    "CreateSnapshot"
    "fixture/CreateSnapshot.yaml"

requestCreateTrust :: CreateTrust -> TestTree
requestCreateTrust =
  req
    "CreateTrust"
    "fixture/CreateTrust.yaml"

requestDeleteConditionalForwarder :: DeleteConditionalForwarder -> TestTree
requestDeleteConditionalForwarder =
  req
    "DeleteConditionalForwarder"
    "fixture/DeleteConditionalForwarder.yaml"

requestDeleteDirectory :: DeleteDirectory -> TestTree
requestDeleteDirectory =
  req
    "DeleteDirectory"
    "fixture/DeleteDirectory.yaml"

requestDeleteLogSubscription :: DeleteLogSubscription -> TestTree
requestDeleteLogSubscription =
  req
    "DeleteLogSubscription"
    "fixture/DeleteLogSubscription.yaml"

requestDeleteSnapshot :: DeleteSnapshot -> TestTree
requestDeleteSnapshot =
  req
    "DeleteSnapshot"
    "fixture/DeleteSnapshot.yaml"

requestDeleteTrust :: DeleteTrust -> TestTree
requestDeleteTrust =
  req
    "DeleteTrust"
    "fixture/DeleteTrust.yaml"

requestDeregisterCertificate :: DeregisterCertificate -> TestTree
requestDeregisterCertificate =
  req
    "DeregisterCertificate"
    "fixture/DeregisterCertificate.yaml"

requestDeregisterEventTopic :: DeregisterEventTopic -> TestTree
requestDeregisterEventTopic =
  req
    "DeregisterEventTopic"
    "fixture/DeregisterEventTopic.yaml"

requestDescribeCertificate :: DescribeCertificate -> TestTree
requestDescribeCertificate =
  req
    "DescribeCertificate"
    "fixture/DescribeCertificate.yaml"

requestDescribeClientAuthenticationSettings :: DescribeClientAuthenticationSettings -> TestTree
requestDescribeClientAuthenticationSettings =
  req
    "DescribeClientAuthenticationSettings"
    "fixture/DescribeClientAuthenticationSettings.yaml"

requestDescribeConditionalForwarders :: DescribeConditionalForwarders -> TestTree
requestDescribeConditionalForwarders =
  req
    "DescribeConditionalForwarders"
    "fixture/DescribeConditionalForwarders.yaml"

requestDescribeDirectories :: DescribeDirectories -> TestTree
requestDescribeDirectories =
  req
    "DescribeDirectories"
    "fixture/DescribeDirectories.yaml"

requestDescribeDomainControllers :: DescribeDomainControllers -> TestTree
requestDescribeDomainControllers =
  req
    "DescribeDomainControllers"
    "fixture/DescribeDomainControllers.yaml"

requestDescribeEventTopics :: DescribeEventTopics -> TestTree
requestDescribeEventTopics =
  req
    "DescribeEventTopics"
    "fixture/DescribeEventTopics.yaml"

requestDescribeLDAPSSettings :: DescribeLDAPSSettings -> TestTree
requestDescribeLDAPSSettings =
  req
    "DescribeLDAPSSettings"
    "fixture/DescribeLDAPSSettings.yaml"

requestDescribeRegions :: DescribeRegions -> TestTree
requestDescribeRegions =
  req
    "DescribeRegions"
    "fixture/DescribeRegions.yaml"

requestDescribeSettings :: DescribeSettings -> TestTree
requestDescribeSettings =
  req
    "DescribeSettings"
    "fixture/DescribeSettings.yaml"

requestDescribeSharedDirectories :: DescribeSharedDirectories -> TestTree
requestDescribeSharedDirectories =
  req
    "DescribeSharedDirectories"
    "fixture/DescribeSharedDirectories.yaml"

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

requestDescribeUpdateDirectory :: DescribeUpdateDirectory -> TestTree
requestDescribeUpdateDirectory =
  req
    "DescribeUpdateDirectory"
    "fixture/DescribeUpdateDirectory.yaml"

requestDisableClientAuthentication :: DisableClientAuthentication -> TestTree
requestDisableClientAuthentication =
  req
    "DisableClientAuthentication"
    "fixture/DisableClientAuthentication.yaml"

requestDisableLDAPS :: DisableLDAPS -> TestTree
requestDisableLDAPS =
  req
    "DisableLDAPS"
    "fixture/DisableLDAPS.yaml"

requestDisableRadius :: DisableRadius -> TestTree
requestDisableRadius =
  req
    "DisableRadius"
    "fixture/DisableRadius.yaml"

requestDisableSso :: DisableSso -> TestTree
requestDisableSso =
  req
    "DisableSso"
    "fixture/DisableSso.yaml"

requestEnableClientAuthentication :: EnableClientAuthentication -> TestTree
requestEnableClientAuthentication =
  req
    "EnableClientAuthentication"
    "fixture/EnableClientAuthentication.yaml"

requestEnableLDAPS :: EnableLDAPS -> TestTree
requestEnableLDAPS =
  req
    "EnableLDAPS"
    "fixture/EnableLDAPS.yaml"

requestEnableRadius :: EnableRadius -> TestTree
requestEnableRadius =
  req
    "EnableRadius"
    "fixture/EnableRadius.yaml"

requestEnableSso :: EnableSso -> TestTree
requestEnableSso =
  req
    "EnableSso"
    "fixture/EnableSso.yaml"

requestGetDirectoryLimits :: GetDirectoryLimits -> TestTree
requestGetDirectoryLimits =
  req
    "GetDirectoryLimits"
    "fixture/GetDirectoryLimits.yaml"

requestGetSnapshotLimits :: GetSnapshotLimits -> TestTree
requestGetSnapshotLimits =
  req
    "GetSnapshotLimits"
    "fixture/GetSnapshotLimits.yaml"

requestListCertificates :: ListCertificates -> TestTree
requestListCertificates =
  req
    "ListCertificates"
    "fixture/ListCertificates.yaml"

requestListIpRoutes :: ListIpRoutes -> TestTree
requestListIpRoutes =
  req
    "ListIpRoutes"
    "fixture/ListIpRoutes.yaml"

requestListLogSubscriptions :: ListLogSubscriptions -> TestTree
requestListLogSubscriptions =
  req
    "ListLogSubscriptions"
    "fixture/ListLogSubscriptions.yaml"

requestListSchemaExtensions :: ListSchemaExtensions -> TestTree
requestListSchemaExtensions =
  req
    "ListSchemaExtensions"
    "fixture/ListSchemaExtensions.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestRegisterCertificate :: RegisterCertificate -> TestTree
requestRegisterCertificate =
  req
    "RegisterCertificate"
    "fixture/RegisterCertificate.yaml"

requestRegisterEventTopic :: RegisterEventTopic -> TestTree
requestRegisterEventTopic =
  req
    "RegisterEventTopic"
    "fixture/RegisterEventTopic.yaml"

requestRejectSharedDirectory :: RejectSharedDirectory -> TestTree
requestRejectSharedDirectory =
  req
    "RejectSharedDirectory"
    "fixture/RejectSharedDirectory.yaml"

requestRemoveIpRoutes :: RemoveIpRoutes -> TestTree
requestRemoveIpRoutes =
  req
    "RemoveIpRoutes"
    "fixture/RemoveIpRoutes.yaml"

requestRemoveRegion :: RemoveRegion -> TestTree
requestRemoveRegion =
  req
    "RemoveRegion"
    "fixture/RemoveRegion.yaml"

requestRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
requestRemoveTagsFromResource =
  req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource.yaml"

requestResetUserPassword :: ResetUserPassword -> TestTree
requestResetUserPassword =
  req
    "ResetUserPassword"
    "fixture/ResetUserPassword.yaml"

requestRestoreFromSnapshot :: RestoreFromSnapshot -> TestTree
requestRestoreFromSnapshot =
  req
    "RestoreFromSnapshot"
    "fixture/RestoreFromSnapshot.yaml"

requestShareDirectory :: ShareDirectory -> TestTree
requestShareDirectory =
  req
    "ShareDirectory"
    "fixture/ShareDirectory.yaml"

requestStartSchemaExtension :: StartSchemaExtension -> TestTree
requestStartSchemaExtension =
  req
    "StartSchemaExtension"
    "fixture/StartSchemaExtension.yaml"

requestUnshareDirectory :: UnshareDirectory -> TestTree
requestUnshareDirectory =
  req
    "UnshareDirectory"
    "fixture/UnshareDirectory.yaml"

requestUpdateConditionalForwarder :: UpdateConditionalForwarder -> TestTree
requestUpdateConditionalForwarder =
  req
    "UpdateConditionalForwarder"
    "fixture/UpdateConditionalForwarder.yaml"

requestUpdateDirectorySetup :: UpdateDirectorySetup -> TestTree
requestUpdateDirectorySetup =
  req
    "UpdateDirectorySetup"
    "fixture/UpdateDirectorySetup.yaml"

requestUpdateNumberOfDomainControllers :: UpdateNumberOfDomainControllers -> TestTree
requestUpdateNumberOfDomainControllers =
  req
    "UpdateNumberOfDomainControllers"
    "fixture/UpdateNumberOfDomainControllers.yaml"

requestUpdateRadius :: UpdateRadius -> TestTree
requestUpdateRadius =
  req
    "UpdateRadius"
    "fixture/UpdateRadius.yaml"

requestUpdateSettings :: UpdateSettings -> TestTree
requestUpdateSettings =
  req
    "UpdateSettings"
    "fixture/UpdateSettings.yaml"

requestUpdateTrust :: UpdateTrust -> TestTree
requestUpdateTrust =
  req
    "UpdateTrust"
    "fixture/UpdateTrust.yaml"

requestVerifyTrust :: VerifyTrust -> TestTree
requestVerifyTrust =
  req
    "VerifyTrust"
    "fixture/VerifyTrust.yaml"

-- Responses

responseAcceptSharedDirectory :: AcceptSharedDirectoryResponse -> TestTree
responseAcceptSharedDirectory =
  res
    "AcceptSharedDirectoryResponse"
    "fixture/AcceptSharedDirectoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptSharedDirectory)

responseAddIpRoutes :: AddIpRoutesResponse -> TestTree
responseAddIpRoutes =
  res
    "AddIpRoutesResponse"
    "fixture/AddIpRoutesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddIpRoutes)

responseAddRegion :: AddRegionResponse -> TestTree
responseAddRegion =
  res
    "AddRegionResponse"
    "fixture/AddRegionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddRegion)

responseAddTagsToResource :: AddTagsToResourceResponse -> TestTree
responseAddTagsToResource =
  res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddTagsToResource)

responseCancelSchemaExtension :: CancelSchemaExtensionResponse -> TestTree
responseCancelSchemaExtension =
  res
    "CancelSchemaExtensionResponse"
    "fixture/CancelSchemaExtensionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelSchemaExtension)

responseConnectDirectory :: ConnectDirectoryResponse -> TestTree
responseConnectDirectory =
  res
    "ConnectDirectoryResponse"
    "fixture/ConnectDirectoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ConnectDirectory)

responseCreateAlias :: CreateAliasResponse -> TestTree
responseCreateAlias =
  res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAlias)

responseCreateComputer :: CreateComputerResponse -> TestTree
responseCreateComputer =
  res
    "CreateComputerResponse"
    "fixture/CreateComputerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateComputer)

responseCreateConditionalForwarder :: CreateConditionalForwarderResponse -> TestTree
responseCreateConditionalForwarder =
  res
    "CreateConditionalForwarderResponse"
    "fixture/CreateConditionalForwarderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConditionalForwarder)

responseCreateDirectory :: CreateDirectoryResponse -> TestTree
responseCreateDirectory =
  res
    "CreateDirectoryResponse"
    "fixture/CreateDirectoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDirectory)

responseCreateLogSubscription :: CreateLogSubscriptionResponse -> TestTree
responseCreateLogSubscription =
  res
    "CreateLogSubscriptionResponse"
    "fixture/CreateLogSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLogSubscription)

responseCreateMicrosoftAD :: CreateMicrosoftADResponse -> TestTree
responseCreateMicrosoftAD =
  res
    "CreateMicrosoftADResponse"
    "fixture/CreateMicrosoftADResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMicrosoftAD)

responseCreateSnapshot :: CreateSnapshotResponse -> TestTree
responseCreateSnapshot =
  res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSnapshot)

responseCreateTrust :: CreateTrustResponse -> TestTree
responseCreateTrust =
  res
    "CreateTrustResponse"
    "fixture/CreateTrustResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTrust)

responseDeleteConditionalForwarder :: DeleteConditionalForwarderResponse -> TestTree
responseDeleteConditionalForwarder =
  res
    "DeleteConditionalForwarderResponse"
    "fixture/DeleteConditionalForwarderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConditionalForwarder)

responseDeleteDirectory :: DeleteDirectoryResponse -> TestTree
responseDeleteDirectory =
  res
    "DeleteDirectoryResponse"
    "fixture/DeleteDirectoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDirectory)

responseDeleteLogSubscription :: DeleteLogSubscriptionResponse -> TestTree
responseDeleteLogSubscription =
  res
    "DeleteLogSubscriptionResponse"
    "fixture/DeleteLogSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLogSubscription)

responseDeleteSnapshot :: DeleteSnapshotResponse -> TestTree
responseDeleteSnapshot =
  res
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSnapshot)

responseDeleteTrust :: DeleteTrustResponse -> TestTree
responseDeleteTrust =
  res
    "DeleteTrustResponse"
    "fixture/DeleteTrustResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTrust)

responseDeregisterCertificate :: DeregisterCertificateResponse -> TestTree
responseDeregisterCertificate =
  res
    "DeregisterCertificateResponse"
    "fixture/DeregisterCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterCertificate)

responseDeregisterEventTopic :: DeregisterEventTopicResponse -> TestTree
responseDeregisterEventTopic =
  res
    "DeregisterEventTopicResponse"
    "fixture/DeregisterEventTopicResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterEventTopic)

responseDescribeCertificate :: DescribeCertificateResponse -> TestTree
responseDescribeCertificate =
  res
    "DescribeCertificateResponse"
    "fixture/DescribeCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCertificate)

responseDescribeClientAuthenticationSettings :: DescribeClientAuthenticationSettingsResponse -> TestTree
responseDescribeClientAuthenticationSettings =
  res
    "DescribeClientAuthenticationSettingsResponse"
    "fixture/DescribeClientAuthenticationSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClientAuthenticationSettings)

responseDescribeConditionalForwarders :: DescribeConditionalForwardersResponse -> TestTree
responseDescribeConditionalForwarders =
  res
    "DescribeConditionalForwardersResponse"
    "fixture/DescribeConditionalForwardersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConditionalForwarders)

responseDescribeDirectories :: DescribeDirectoriesResponse -> TestTree
responseDescribeDirectories =
  res
    "DescribeDirectoriesResponse"
    "fixture/DescribeDirectoriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDirectories)

responseDescribeDomainControllers :: DescribeDomainControllersResponse -> TestTree
responseDescribeDomainControllers =
  res
    "DescribeDomainControllersResponse"
    "fixture/DescribeDomainControllersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDomainControllers)

responseDescribeEventTopics :: DescribeEventTopicsResponse -> TestTree
responseDescribeEventTopics =
  res
    "DescribeEventTopicsResponse"
    "fixture/DescribeEventTopicsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEventTopics)

responseDescribeLDAPSSettings :: DescribeLDAPSSettingsResponse -> TestTree
responseDescribeLDAPSSettings =
  res
    "DescribeLDAPSSettingsResponse"
    "fixture/DescribeLDAPSSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLDAPSSettings)

responseDescribeRegions :: DescribeRegionsResponse -> TestTree
responseDescribeRegions =
  res
    "DescribeRegionsResponse"
    "fixture/DescribeRegionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRegions)

responseDescribeSettings :: DescribeSettingsResponse -> TestTree
responseDescribeSettings =
  res
    "DescribeSettingsResponse"
    "fixture/DescribeSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSettings)

responseDescribeSharedDirectories :: DescribeSharedDirectoriesResponse -> TestTree
responseDescribeSharedDirectories =
  res
    "DescribeSharedDirectoriesResponse"
    "fixture/DescribeSharedDirectoriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSharedDirectories)

responseDescribeSnapshots :: DescribeSnapshotsResponse -> TestTree
responseDescribeSnapshots =
  res
    "DescribeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSnapshots)

responseDescribeTrusts :: DescribeTrustsResponse -> TestTree
responseDescribeTrusts =
  res
    "DescribeTrustsResponse"
    "fixture/DescribeTrustsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTrusts)

responseDescribeUpdateDirectory :: DescribeUpdateDirectoryResponse -> TestTree
responseDescribeUpdateDirectory =
  res
    "DescribeUpdateDirectoryResponse"
    "fixture/DescribeUpdateDirectoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUpdateDirectory)

responseDisableClientAuthentication :: DisableClientAuthenticationResponse -> TestTree
responseDisableClientAuthentication =
  res
    "DisableClientAuthenticationResponse"
    "fixture/DisableClientAuthenticationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableClientAuthentication)

responseDisableLDAPS :: DisableLDAPSResponse -> TestTree
responseDisableLDAPS =
  res
    "DisableLDAPSResponse"
    "fixture/DisableLDAPSResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableLDAPS)

responseDisableRadius :: DisableRadiusResponse -> TestTree
responseDisableRadius =
  res
    "DisableRadiusResponse"
    "fixture/DisableRadiusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableRadius)

responseDisableSso :: DisableSsoResponse -> TestTree
responseDisableSso =
  res
    "DisableSsoResponse"
    "fixture/DisableSsoResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableSso)

responseEnableClientAuthentication :: EnableClientAuthenticationResponse -> TestTree
responseEnableClientAuthentication =
  res
    "EnableClientAuthenticationResponse"
    "fixture/EnableClientAuthenticationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableClientAuthentication)

responseEnableLDAPS :: EnableLDAPSResponse -> TestTree
responseEnableLDAPS =
  res
    "EnableLDAPSResponse"
    "fixture/EnableLDAPSResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableLDAPS)

responseEnableRadius :: EnableRadiusResponse -> TestTree
responseEnableRadius =
  res
    "EnableRadiusResponse"
    "fixture/EnableRadiusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableRadius)

responseEnableSso :: EnableSsoResponse -> TestTree
responseEnableSso =
  res
    "EnableSsoResponse"
    "fixture/EnableSsoResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableSso)

responseGetDirectoryLimits :: GetDirectoryLimitsResponse -> TestTree
responseGetDirectoryLimits =
  res
    "GetDirectoryLimitsResponse"
    "fixture/GetDirectoryLimitsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDirectoryLimits)

responseGetSnapshotLimits :: GetSnapshotLimitsResponse -> TestTree
responseGetSnapshotLimits =
  res
    "GetSnapshotLimitsResponse"
    "fixture/GetSnapshotLimitsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSnapshotLimits)

responseListCertificates :: ListCertificatesResponse -> TestTree
responseListCertificates =
  res
    "ListCertificatesResponse"
    "fixture/ListCertificatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCertificates)

responseListIpRoutes :: ListIpRoutesResponse -> TestTree
responseListIpRoutes =
  res
    "ListIpRoutesResponse"
    "fixture/ListIpRoutesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIpRoutes)

responseListLogSubscriptions :: ListLogSubscriptionsResponse -> TestTree
responseListLogSubscriptions =
  res
    "ListLogSubscriptionsResponse"
    "fixture/ListLogSubscriptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLogSubscriptions)

responseListSchemaExtensions :: ListSchemaExtensionsResponse -> TestTree
responseListSchemaExtensions =
  res
    "ListSchemaExtensionsResponse"
    "fixture/ListSchemaExtensionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSchemaExtensions)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseRegisterCertificate :: RegisterCertificateResponse -> TestTree
responseRegisterCertificate =
  res
    "RegisterCertificateResponse"
    "fixture/RegisterCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterCertificate)

responseRegisterEventTopic :: RegisterEventTopicResponse -> TestTree
responseRegisterEventTopic =
  res
    "RegisterEventTopicResponse"
    "fixture/RegisterEventTopicResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterEventTopic)

responseRejectSharedDirectory :: RejectSharedDirectoryResponse -> TestTree
responseRejectSharedDirectory =
  res
    "RejectSharedDirectoryResponse"
    "fixture/RejectSharedDirectoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectSharedDirectory)

responseRemoveIpRoutes :: RemoveIpRoutesResponse -> TestTree
responseRemoveIpRoutes =
  res
    "RemoveIpRoutesResponse"
    "fixture/RemoveIpRoutesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveIpRoutes)

responseRemoveRegion :: RemoveRegionResponse -> TestTree
responseRemoveRegion =
  res
    "RemoveRegionResponse"
    "fixture/RemoveRegionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveRegion)

responseRemoveTagsFromResource :: RemoveTagsFromResourceResponse -> TestTree
responseRemoveTagsFromResource =
  res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveTagsFromResource)

responseResetUserPassword :: ResetUserPasswordResponse -> TestTree
responseResetUserPassword =
  res
    "ResetUserPasswordResponse"
    "fixture/ResetUserPasswordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetUserPassword)

responseRestoreFromSnapshot :: RestoreFromSnapshotResponse -> TestTree
responseRestoreFromSnapshot =
  res
    "RestoreFromSnapshotResponse"
    "fixture/RestoreFromSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreFromSnapshot)

responseShareDirectory :: ShareDirectoryResponse -> TestTree
responseShareDirectory =
  res
    "ShareDirectoryResponse"
    "fixture/ShareDirectoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ShareDirectory)

responseStartSchemaExtension :: StartSchemaExtensionResponse -> TestTree
responseStartSchemaExtension =
  res
    "StartSchemaExtensionResponse"
    "fixture/StartSchemaExtensionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartSchemaExtension)

responseUnshareDirectory :: UnshareDirectoryResponse -> TestTree
responseUnshareDirectory =
  res
    "UnshareDirectoryResponse"
    "fixture/UnshareDirectoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UnshareDirectory)

responseUpdateConditionalForwarder :: UpdateConditionalForwarderResponse -> TestTree
responseUpdateConditionalForwarder =
  res
    "UpdateConditionalForwarderResponse"
    "fixture/UpdateConditionalForwarderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConditionalForwarder)

responseUpdateDirectorySetup :: UpdateDirectorySetupResponse -> TestTree
responseUpdateDirectorySetup =
  res
    "UpdateDirectorySetupResponse"
    "fixture/UpdateDirectorySetupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDirectorySetup)

responseUpdateNumberOfDomainControllers :: UpdateNumberOfDomainControllersResponse -> TestTree
responseUpdateNumberOfDomainControllers =
  res
    "UpdateNumberOfDomainControllersResponse"
    "fixture/UpdateNumberOfDomainControllersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateNumberOfDomainControllers)

responseUpdateRadius :: UpdateRadiusResponse -> TestTree
responseUpdateRadius =
  res
    "UpdateRadiusResponse"
    "fixture/UpdateRadiusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRadius)

responseUpdateSettings :: UpdateSettingsResponse -> TestTree
responseUpdateSettings =
  res
    "UpdateSettingsResponse"
    "fixture/UpdateSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSettings)

responseUpdateTrust :: UpdateTrustResponse -> TestTree
responseUpdateTrust =
  res
    "UpdateTrustResponse"
    "fixture/UpdateTrustResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTrust)

responseVerifyTrust :: VerifyTrustResponse -> TestTree
responseVerifyTrust =
  res
    "VerifyTrustResponse"
    "fixture/VerifyTrustResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy VerifyTrust)

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

import qualified Data.Proxy as Proxy
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
--         [ requestShareDirectory $
--             newShareDirectory
--
--         , requestUpdateNumberOfDomainControllers $
--             newUpdateNumberOfDomainControllers
--
--         , requestDescribeConditionalForwarders $
--             newDescribeConditionalForwarders
--
--         , requestGetSnapshotLimits $
--             newGetSnapshotLimits
--
--         , requestRegisterEventTopic $
--             newRegisterEventTopic
--
--         , requestRegisterCertificate $
--             newRegisterCertificate
--
--         , requestConnectDirectory $
--             newConnectDirectory
--
--         , requestDescribeLDAPSSettings $
--             newDescribeLDAPSSettings
--
--         , requestCreateAlias $
--             newCreateAlias
--
--         , requestDescribeDirectories $
--             newDescribeDirectories
--
--         , requestAddIpRoutes $
--             newAddIpRoutes
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDescribeTrusts $
--             newDescribeTrusts
--
--         , requestDeleteTrust $
--             newDeleteTrust
--
--         , requestUpdateTrust $
--             newUpdateTrust
--
--         , requestCreateMicrosoftAD $
--             newCreateMicrosoftAD
--
--         , requestDisableClientAuthentication $
--             newDisableClientAuthentication
--
--         , requestDeregisterEventTopic $
--             newDeregisterEventTopic
--
--         , requestCreateDirectory $
--             newCreateDirectory
--
--         , requestAcceptSharedDirectory $
--             newAcceptSharedDirectory
--
--         , requestCreateLogSubscription $
--             newCreateLogSubscription
--
--         , requestRemoveTagsFromResource $
--             newRemoveTagsFromResource
--
--         , requestDescribeEventTopics $
--             newDescribeEventTopics
--
--         , requestResetUserPassword $
--             newResetUserPassword
--
--         , requestUpdateConditionalForwarder $
--             newUpdateConditionalForwarder
--
--         , requestDeleteConditionalForwarder $
--             newDeleteConditionalForwarder
--
--         , requestDisableLDAPS $
--             newDisableLDAPS
--
--         , requestDeleteLogSubscription $
--             newDeleteLogSubscription
--
--         , requestEnableSso $
--             newEnableSso
--
--         , requestCancelSchemaExtension $
--             newCancelSchemaExtension
--
--         , requestListLogSubscriptions $
--             newListLogSubscriptions
--
--         , requestEnableRadius $
--             newEnableRadius
--
--         , requestListIpRoutes $
--             newListIpRoutes
--
--         , requestAddTagsToResource $
--             newAddTagsToResource
--
--         , requestDescribeClientAuthenticationSettings $
--             newDescribeClientAuthenticationSettings
--
--         , requestListSchemaExtensions $
--             newListSchemaExtensions
--
--         , requestDisableRadius $
--             newDisableRadius
--
--         , requestListCertificates $
--             newListCertificates
--
--         , requestRejectSharedDirectory $
--             newRejectSharedDirectory
--
--         , requestUnshareDirectory $
--             newUnshareDirectory
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
--         , requestRemoveIpRoutes $
--             newRemoveIpRoutes
--
--         , requestDeleteSnapshot $
--             newDeleteSnapshot
--
--         , requestDeregisterCertificate $
--             newDeregisterCertificate
--
--         , requestStartSchemaExtension $
--             newStartSchemaExtension
--
--         , requestCreateTrust $
--             newCreateTrust
--
--         , requestDeleteDirectory $
--             newDeleteDirectory
--
--         , requestCreateSnapshot $
--             newCreateSnapshot
--
--         , requestDescribeCertificate $
--             newDescribeCertificate
--
--         , requestEnableClientAuthentication $
--             newEnableClientAuthentication
--
--         , requestCreateComputer $
--             newCreateComputer
--
--         , requestDescribeSharedDirectories $
--             newDescribeSharedDirectories
--
--         , requestEnableLDAPS $
--             newEnableLDAPS
--
--         , requestDisableSso $
--             newDisableSso
--
--         , requestVerifyTrust $
--             newVerifyTrust
--
--         , requestRemoveRegion $
--             newRemoveRegion
--
--         , requestCreateConditionalForwarder $
--             newCreateConditionalForwarder
--
--         , requestDescribeRegions $
--             newDescribeRegions
--
--         , requestAddRegion $
--             newAddRegion
--
--         , requestGetDirectoryLimits $
--             newGetDirectoryLimits
--
--         , requestUpdateRadius $
--             newUpdateRadius
--
--           ]

--     , testGroup "response"
--         [ responseShareDirectory $
--             newShareDirectoryResponse
--
--         , responseUpdateNumberOfDomainControllers $
--             newUpdateNumberOfDomainControllersResponse
--
--         , responseDescribeConditionalForwarders $
--             newDescribeConditionalForwardersResponse
--
--         , responseGetSnapshotLimits $
--             newGetSnapshotLimitsResponse
--
--         , responseRegisterEventTopic $
--             newRegisterEventTopicResponse
--
--         , responseRegisterCertificate $
--             newRegisterCertificateResponse
--
--         , responseConnectDirectory $
--             newConnectDirectoryResponse
--
--         , responseDescribeLDAPSSettings $
--             newDescribeLDAPSSettingsResponse
--
--         , responseCreateAlias $
--             newCreateAliasResponse
--
--         , responseDescribeDirectories $
--             newDescribeDirectoriesResponse
--
--         , responseAddIpRoutes $
--             newAddIpRoutesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDescribeTrusts $
--             newDescribeTrustsResponse
--
--         , responseDeleteTrust $
--             newDeleteTrustResponse
--
--         , responseUpdateTrust $
--             newUpdateTrustResponse
--
--         , responseCreateMicrosoftAD $
--             newCreateMicrosoftADResponse
--
--         , responseDisableClientAuthentication $
--             newDisableClientAuthenticationResponse
--
--         , responseDeregisterEventTopic $
--             newDeregisterEventTopicResponse
--
--         , responseCreateDirectory $
--             newCreateDirectoryResponse
--
--         , responseAcceptSharedDirectory $
--             newAcceptSharedDirectoryResponse
--
--         , responseCreateLogSubscription $
--             newCreateLogSubscriptionResponse
--
--         , responseRemoveTagsFromResource $
--             newRemoveTagsFromResourceResponse
--
--         , responseDescribeEventTopics $
--             newDescribeEventTopicsResponse
--
--         , responseResetUserPassword $
--             newResetUserPasswordResponse
--
--         , responseUpdateConditionalForwarder $
--             newUpdateConditionalForwarderResponse
--
--         , responseDeleteConditionalForwarder $
--             newDeleteConditionalForwarderResponse
--
--         , responseDisableLDAPS $
--             newDisableLDAPSResponse
--
--         , responseDeleteLogSubscription $
--             newDeleteLogSubscriptionResponse
--
--         , responseEnableSso $
--             newEnableSsoResponse
--
--         , responseCancelSchemaExtension $
--             newCancelSchemaExtensionResponse
--
--         , responseListLogSubscriptions $
--             newListLogSubscriptionsResponse
--
--         , responseEnableRadius $
--             newEnableRadiusResponse
--
--         , responseListIpRoutes $
--             newListIpRoutesResponse
--
--         , responseAddTagsToResource $
--             newAddTagsToResourceResponse
--
--         , responseDescribeClientAuthenticationSettings $
--             newDescribeClientAuthenticationSettingsResponse
--
--         , responseListSchemaExtensions $
--             newListSchemaExtensionsResponse
--
--         , responseDisableRadius $
--             newDisableRadiusResponse
--
--         , responseListCertificates $
--             newListCertificatesResponse
--
--         , responseRejectSharedDirectory $
--             newRejectSharedDirectoryResponse
--
--         , responseUnshareDirectory $
--             newUnshareDirectoryResponse
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
--         , responseRemoveIpRoutes $
--             newRemoveIpRoutesResponse
--
--         , responseDeleteSnapshot $
--             newDeleteSnapshotResponse
--
--         , responseDeregisterCertificate $
--             newDeregisterCertificateResponse
--
--         , responseStartSchemaExtension $
--             newStartSchemaExtensionResponse
--
--         , responseCreateTrust $
--             newCreateTrustResponse
--
--         , responseDeleteDirectory $
--             newDeleteDirectoryResponse
--
--         , responseCreateSnapshot $
--             newCreateSnapshotResponse
--
--         , responseDescribeCertificate $
--             newDescribeCertificateResponse
--
--         , responseEnableClientAuthentication $
--             newEnableClientAuthenticationResponse
--
--         , responseCreateComputer $
--             newCreateComputerResponse
--
--         , responseDescribeSharedDirectories $
--             newDescribeSharedDirectoriesResponse
--
--         , responseEnableLDAPS $
--             newEnableLDAPSResponse
--
--         , responseDisableSso $
--             newDisableSsoResponse
--
--         , responseVerifyTrust $
--             newVerifyTrustResponse
--
--         , responseRemoveRegion $
--             newRemoveRegionResponse
--
--         , responseCreateConditionalForwarder $
--             newCreateConditionalForwarderResponse
--
--         , responseDescribeRegions $
--             newDescribeRegionsResponse
--
--         , responseAddRegion $
--             newAddRegionResponse
--
--         , responseGetDirectoryLimits $
--             newGetDirectoryLimitsResponse
--
--         , responseUpdateRadius $
--             newUpdateRadiusResponse
--
--           ]
--     ]

-- Requests

requestShareDirectory :: ShareDirectory -> TestTree
requestShareDirectory =
  req
    "ShareDirectory"
    "fixture/ShareDirectory.yaml"

requestUpdateNumberOfDomainControllers :: UpdateNumberOfDomainControllers -> TestTree
requestUpdateNumberOfDomainControllers =
  req
    "UpdateNumberOfDomainControllers"
    "fixture/UpdateNumberOfDomainControllers.yaml"

requestDescribeConditionalForwarders :: DescribeConditionalForwarders -> TestTree
requestDescribeConditionalForwarders =
  req
    "DescribeConditionalForwarders"
    "fixture/DescribeConditionalForwarders.yaml"

requestGetSnapshotLimits :: GetSnapshotLimits -> TestTree
requestGetSnapshotLimits =
  req
    "GetSnapshotLimits"
    "fixture/GetSnapshotLimits.yaml"

requestRegisterEventTopic :: RegisterEventTopic -> TestTree
requestRegisterEventTopic =
  req
    "RegisterEventTopic"
    "fixture/RegisterEventTopic.yaml"

requestRegisterCertificate :: RegisterCertificate -> TestTree
requestRegisterCertificate =
  req
    "RegisterCertificate"
    "fixture/RegisterCertificate.yaml"

requestConnectDirectory :: ConnectDirectory -> TestTree
requestConnectDirectory =
  req
    "ConnectDirectory"
    "fixture/ConnectDirectory.yaml"

requestDescribeLDAPSSettings :: DescribeLDAPSSettings -> TestTree
requestDescribeLDAPSSettings =
  req
    "DescribeLDAPSSettings"
    "fixture/DescribeLDAPSSettings.yaml"

requestCreateAlias :: CreateAlias -> TestTree
requestCreateAlias =
  req
    "CreateAlias"
    "fixture/CreateAlias.yaml"

requestDescribeDirectories :: DescribeDirectories -> TestTree
requestDescribeDirectories =
  req
    "DescribeDirectories"
    "fixture/DescribeDirectories.yaml"

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

requestDescribeTrusts :: DescribeTrusts -> TestTree
requestDescribeTrusts =
  req
    "DescribeTrusts"
    "fixture/DescribeTrusts.yaml"

requestDeleteTrust :: DeleteTrust -> TestTree
requestDeleteTrust =
  req
    "DeleteTrust"
    "fixture/DeleteTrust.yaml"

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

requestDisableClientAuthentication :: DisableClientAuthentication -> TestTree
requestDisableClientAuthentication =
  req
    "DisableClientAuthentication"
    "fixture/DisableClientAuthentication.yaml"

requestDeregisterEventTopic :: DeregisterEventTopic -> TestTree
requestDeregisterEventTopic =
  req
    "DeregisterEventTopic"
    "fixture/DeregisterEventTopic.yaml"

requestCreateDirectory :: CreateDirectory -> TestTree
requestCreateDirectory =
  req
    "CreateDirectory"
    "fixture/CreateDirectory.yaml"

requestAcceptSharedDirectory :: AcceptSharedDirectory -> TestTree
requestAcceptSharedDirectory =
  req
    "AcceptSharedDirectory"
    "fixture/AcceptSharedDirectory.yaml"

requestCreateLogSubscription :: CreateLogSubscription -> TestTree
requestCreateLogSubscription =
  req
    "CreateLogSubscription"
    "fixture/CreateLogSubscription.yaml"

requestRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
requestRemoveTagsFromResource =
  req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource.yaml"

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

requestUpdateConditionalForwarder :: UpdateConditionalForwarder -> TestTree
requestUpdateConditionalForwarder =
  req
    "UpdateConditionalForwarder"
    "fixture/UpdateConditionalForwarder.yaml"

requestDeleteConditionalForwarder :: DeleteConditionalForwarder -> TestTree
requestDeleteConditionalForwarder =
  req
    "DeleteConditionalForwarder"
    "fixture/DeleteConditionalForwarder.yaml"

requestDisableLDAPS :: DisableLDAPS -> TestTree
requestDisableLDAPS =
  req
    "DisableLDAPS"
    "fixture/DisableLDAPS.yaml"

requestDeleteLogSubscription :: DeleteLogSubscription -> TestTree
requestDeleteLogSubscription =
  req
    "DeleteLogSubscription"
    "fixture/DeleteLogSubscription.yaml"

requestEnableSso :: EnableSso -> TestTree
requestEnableSso =
  req
    "EnableSso"
    "fixture/EnableSso.yaml"

requestCancelSchemaExtension :: CancelSchemaExtension -> TestTree
requestCancelSchemaExtension =
  req
    "CancelSchemaExtension"
    "fixture/CancelSchemaExtension.yaml"

requestListLogSubscriptions :: ListLogSubscriptions -> TestTree
requestListLogSubscriptions =
  req
    "ListLogSubscriptions"
    "fixture/ListLogSubscriptions.yaml"

requestEnableRadius :: EnableRadius -> TestTree
requestEnableRadius =
  req
    "EnableRadius"
    "fixture/EnableRadius.yaml"

requestListIpRoutes :: ListIpRoutes -> TestTree
requestListIpRoutes =
  req
    "ListIpRoutes"
    "fixture/ListIpRoutes.yaml"

requestAddTagsToResource :: AddTagsToResource -> TestTree
requestAddTagsToResource =
  req
    "AddTagsToResource"
    "fixture/AddTagsToResource.yaml"

requestDescribeClientAuthenticationSettings :: DescribeClientAuthenticationSettings -> TestTree
requestDescribeClientAuthenticationSettings =
  req
    "DescribeClientAuthenticationSettings"
    "fixture/DescribeClientAuthenticationSettings.yaml"

requestListSchemaExtensions :: ListSchemaExtensions -> TestTree
requestListSchemaExtensions =
  req
    "ListSchemaExtensions"
    "fixture/ListSchemaExtensions.yaml"

requestDisableRadius :: DisableRadius -> TestTree
requestDisableRadius =
  req
    "DisableRadius"
    "fixture/DisableRadius.yaml"

requestListCertificates :: ListCertificates -> TestTree
requestListCertificates =
  req
    "ListCertificates"
    "fixture/ListCertificates.yaml"

requestRejectSharedDirectory :: RejectSharedDirectory -> TestTree
requestRejectSharedDirectory =
  req
    "RejectSharedDirectory"
    "fixture/RejectSharedDirectory.yaml"

requestUnshareDirectory :: UnshareDirectory -> TestTree
requestUnshareDirectory =
  req
    "UnshareDirectory"
    "fixture/UnshareDirectory.yaml"

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

requestRemoveIpRoutes :: RemoveIpRoutes -> TestTree
requestRemoveIpRoutes =
  req
    "RemoveIpRoutes"
    "fixture/RemoveIpRoutes.yaml"

requestDeleteSnapshot :: DeleteSnapshot -> TestTree
requestDeleteSnapshot =
  req
    "DeleteSnapshot"
    "fixture/DeleteSnapshot.yaml"

requestDeregisterCertificate :: DeregisterCertificate -> TestTree
requestDeregisterCertificate =
  req
    "DeregisterCertificate"
    "fixture/DeregisterCertificate.yaml"

requestStartSchemaExtension :: StartSchemaExtension -> TestTree
requestStartSchemaExtension =
  req
    "StartSchemaExtension"
    "fixture/StartSchemaExtension.yaml"

requestCreateTrust :: CreateTrust -> TestTree
requestCreateTrust =
  req
    "CreateTrust"
    "fixture/CreateTrust.yaml"

requestDeleteDirectory :: DeleteDirectory -> TestTree
requestDeleteDirectory =
  req
    "DeleteDirectory"
    "fixture/DeleteDirectory.yaml"

requestCreateSnapshot :: CreateSnapshot -> TestTree
requestCreateSnapshot =
  req
    "CreateSnapshot"
    "fixture/CreateSnapshot.yaml"

requestDescribeCertificate :: DescribeCertificate -> TestTree
requestDescribeCertificate =
  req
    "DescribeCertificate"
    "fixture/DescribeCertificate.yaml"

requestEnableClientAuthentication :: EnableClientAuthentication -> TestTree
requestEnableClientAuthentication =
  req
    "EnableClientAuthentication"
    "fixture/EnableClientAuthentication.yaml"

requestCreateComputer :: CreateComputer -> TestTree
requestCreateComputer =
  req
    "CreateComputer"
    "fixture/CreateComputer.yaml"

requestDescribeSharedDirectories :: DescribeSharedDirectories -> TestTree
requestDescribeSharedDirectories =
  req
    "DescribeSharedDirectories"
    "fixture/DescribeSharedDirectories.yaml"

requestEnableLDAPS :: EnableLDAPS -> TestTree
requestEnableLDAPS =
  req
    "EnableLDAPS"
    "fixture/EnableLDAPS.yaml"

requestDisableSso :: DisableSso -> TestTree
requestDisableSso =
  req
    "DisableSso"
    "fixture/DisableSso.yaml"

requestVerifyTrust :: VerifyTrust -> TestTree
requestVerifyTrust =
  req
    "VerifyTrust"
    "fixture/VerifyTrust.yaml"

requestRemoveRegion :: RemoveRegion -> TestTree
requestRemoveRegion =
  req
    "RemoveRegion"
    "fixture/RemoveRegion.yaml"

requestCreateConditionalForwarder :: CreateConditionalForwarder -> TestTree
requestCreateConditionalForwarder =
  req
    "CreateConditionalForwarder"
    "fixture/CreateConditionalForwarder.yaml"

requestDescribeRegions :: DescribeRegions -> TestTree
requestDescribeRegions =
  req
    "DescribeRegions"
    "fixture/DescribeRegions.yaml"

requestAddRegion :: AddRegion -> TestTree
requestAddRegion =
  req
    "AddRegion"
    "fixture/AddRegion.yaml"

requestGetDirectoryLimits :: GetDirectoryLimits -> TestTree
requestGetDirectoryLimits =
  req
    "GetDirectoryLimits"
    "fixture/GetDirectoryLimits.yaml"

requestUpdateRadius :: UpdateRadius -> TestTree
requestUpdateRadius =
  req
    "UpdateRadius"
    "fixture/UpdateRadius.yaml"

-- Responses

responseShareDirectory :: ShareDirectoryResponse -> TestTree
responseShareDirectory =
  res
    "ShareDirectoryResponse"
    "fixture/ShareDirectoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ShareDirectory)

responseUpdateNumberOfDomainControllers :: UpdateNumberOfDomainControllersResponse -> TestTree
responseUpdateNumberOfDomainControllers =
  res
    "UpdateNumberOfDomainControllersResponse"
    "fixture/UpdateNumberOfDomainControllersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateNumberOfDomainControllers)

responseDescribeConditionalForwarders :: DescribeConditionalForwardersResponse -> TestTree
responseDescribeConditionalForwarders =
  res
    "DescribeConditionalForwardersResponse"
    "fixture/DescribeConditionalForwardersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConditionalForwarders)

responseGetSnapshotLimits :: GetSnapshotLimitsResponse -> TestTree
responseGetSnapshotLimits =
  res
    "GetSnapshotLimitsResponse"
    "fixture/GetSnapshotLimitsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSnapshotLimits)

responseRegisterEventTopic :: RegisterEventTopicResponse -> TestTree
responseRegisterEventTopic =
  res
    "RegisterEventTopicResponse"
    "fixture/RegisterEventTopicResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterEventTopic)

responseRegisterCertificate :: RegisterCertificateResponse -> TestTree
responseRegisterCertificate =
  res
    "RegisterCertificateResponse"
    "fixture/RegisterCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterCertificate)

responseConnectDirectory :: ConnectDirectoryResponse -> TestTree
responseConnectDirectory =
  res
    "ConnectDirectoryResponse"
    "fixture/ConnectDirectoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ConnectDirectory)

responseDescribeLDAPSSettings :: DescribeLDAPSSettingsResponse -> TestTree
responseDescribeLDAPSSettings =
  res
    "DescribeLDAPSSettingsResponse"
    "fixture/DescribeLDAPSSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLDAPSSettings)

responseCreateAlias :: CreateAliasResponse -> TestTree
responseCreateAlias =
  res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAlias)

responseDescribeDirectories :: DescribeDirectoriesResponse -> TestTree
responseDescribeDirectories =
  res
    "DescribeDirectoriesResponse"
    "fixture/DescribeDirectoriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDirectories)

responseAddIpRoutes :: AddIpRoutesResponse -> TestTree
responseAddIpRoutes =
  res
    "AddIpRoutesResponse"
    "fixture/AddIpRoutesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddIpRoutes)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseDescribeTrusts :: DescribeTrustsResponse -> TestTree
responseDescribeTrusts =
  res
    "DescribeTrustsResponse"
    "fixture/DescribeTrustsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTrusts)

responseDeleteTrust :: DeleteTrustResponse -> TestTree
responseDeleteTrust =
  res
    "DeleteTrustResponse"
    "fixture/DeleteTrustResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTrust)

responseUpdateTrust :: UpdateTrustResponse -> TestTree
responseUpdateTrust =
  res
    "UpdateTrustResponse"
    "fixture/UpdateTrustResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTrust)

responseCreateMicrosoftAD :: CreateMicrosoftADResponse -> TestTree
responseCreateMicrosoftAD =
  res
    "CreateMicrosoftADResponse"
    "fixture/CreateMicrosoftADResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMicrosoftAD)

responseDisableClientAuthentication :: DisableClientAuthenticationResponse -> TestTree
responseDisableClientAuthentication =
  res
    "DisableClientAuthenticationResponse"
    "fixture/DisableClientAuthenticationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableClientAuthentication)

responseDeregisterEventTopic :: DeregisterEventTopicResponse -> TestTree
responseDeregisterEventTopic =
  res
    "DeregisterEventTopicResponse"
    "fixture/DeregisterEventTopicResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterEventTopic)

responseCreateDirectory :: CreateDirectoryResponse -> TestTree
responseCreateDirectory =
  res
    "CreateDirectoryResponse"
    "fixture/CreateDirectoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDirectory)

responseAcceptSharedDirectory :: AcceptSharedDirectoryResponse -> TestTree
responseAcceptSharedDirectory =
  res
    "AcceptSharedDirectoryResponse"
    "fixture/AcceptSharedDirectoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptSharedDirectory)

responseCreateLogSubscription :: CreateLogSubscriptionResponse -> TestTree
responseCreateLogSubscription =
  res
    "CreateLogSubscriptionResponse"
    "fixture/CreateLogSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLogSubscription)

responseRemoveTagsFromResource :: RemoveTagsFromResourceResponse -> TestTree
responseRemoveTagsFromResource =
  res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveTagsFromResource)

responseDescribeEventTopics :: DescribeEventTopicsResponse -> TestTree
responseDescribeEventTopics =
  res
    "DescribeEventTopicsResponse"
    "fixture/DescribeEventTopicsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEventTopics)

responseResetUserPassword :: ResetUserPasswordResponse -> TestTree
responseResetUserPassword =
  res
    "ResetUserPasswordResponse"
    "fixture/ResetUserPasswordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetUserPassword)

responseUpdateConditionalForwarder :: UpdateConditionalForwarderResponse -> TestTree
responseUpdateConditionalForwarder =
  res
    "UpdateConditionalForwarderResponse"
    "fixture/UpdateConditionalForwarderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConditionalForwarder)

responseDeleteConditionalForwarder :: DeleteConditionalForwarderResponse -> TestTree
responseDeleteConditionalForwarder =
  res
    "DeleteConditionalForwarderResponse"
    "fixture/DeleteConditionalForwarderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConditionalForwarder)

responseDisableLDAPS :: DisableLDAPSResponse -> TestTree
responseDisableLDAPS =
  res
    "DisableLDAPSResponse"
    "fixture/DisableLDAPSResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableLDAPS)

responseDeleteLogSubscription :: DeleteLogSubscriptionResponse -> TestTree
responseDeleteLogSubscription =
  res
    "DeleteLogSubscriptionResponse"
    "fixture/DeleteLogSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLogSubscription)

responseEnableSso :: EnableSsoResponse -> TestTree
responseEnableSso =
  res
    "EnableSsoResponse"
    "fixture/EnableSsoResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableSso)

responseCancelSchemaExtension :: CancelSchemaExtensionResponse -> TestTree
responseCancelSchemaExtension =
  res
    "CancelSchemaExtensionResponse"
    "fixture/CancelSchemaExtensionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelSchemaExtension)

responseListLogSubscriptions :: ListLogSubscriptionsResponse -> TestTree
responseListLogSubscriptions =
  res
    "ListLogSubscriptionsResponse"
    "fixture/ListLogSubscriptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLogSubscriptions)

responseEnableRadius :: EnableRadiusResponse -> TestTree
responseEnableRadius =
  res
    "EnableRadiusResponse"
    "fixture/EnableRadiusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableRadius)

responseListIpRoutes :: ListIpRoutesResponse -> TestTree
responseListIpRoutes =
  res
    "ListIpRoutesResponse"
    "fixture/ListIpRoutesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIpRoutes)

responseAddTagsToResource :: AddTagsToResourceResponse -> TestTree
responseAddTagsToResource =
  res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddTagsToResource)

responseDescribeClientAuthenticationSettings :: DescribeClientAuthenticationSettingsResponse -> TestTree
responseDescribeClientAuthenticationSettings =
  res
    "DescribeClientAuthenticationSettingsResponse"
    "fixture/DescribeClientAuthenticationSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClientAuthenticationSettings)

responseListSchemaExtensions :: ListSchemaExtensionsResponse -> TestTree
responseListSchemaExtensions =
  res
    "ListSchemaExtensionsResponse"
    "fixture/ListSchemaExtensionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSchemaExtensions)

responseDisableRadius :: DisableRadiusResponse -> TestTree
responseDisableRadius =
  res
    "DisableRadiusResponse"
    "fixture/DisableRadiusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableRadius)

responseListCertificates :: ListCertificatesResponse -> TestTree
responseListCertificates =
  res
    "ListCertificatesResponse"
    "fixture/ListCertificatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCertificates)

responseRejectSharedDirectory :: RejectSharedDirectoryResponse -> TestTree
responseRejectSharedDirectory =
  res
    "RejectSharedDirectoryResponse"
    "fixture/RejectSharedDirectoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectSharedDirectory)

responseUnshareDirectory :: UnshareDirectoryResponse -> TestTree
responseUnshareDirectory =
  res
    "UnshareDirectoryResponse"
    "fixture/UnshareDirectoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UnshareDirectory)

responseRestoreFromSnapshot :: RestoreFromSnapshotResponse -> TestTree
responseRestoreFromSnapshot =
  res
    "RestoreFromSnapshotResponse"
    "fixture/RestoreFromSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreFromSnapshot)

responseDescribeDomainControllers :: DescribeDomainControllersResponse -> TestTree
responseDescribeDomainControllers =
  res
    "DescribeDomainControllersResponse"
    "fixture/DescribeDomainControllersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDomainControllers)

responseDescribeSnapshots :: DescribeSnapshotsResponse -> TestTree
responseDescribeSnapshots =
  res
    "DescribeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSnapshots)

responseRemoveIpRoutes :: RemoveIpRoutesResponse -> TestTree
responseRemoveIpRoutes =
  res
    "RemoveIpRoutesResponse"
    "fixture/RemoveIpRoutesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveIpRoutes)

responseDeleteSnapshot :: DeleteSnapshotResponse -> TestTree
responseDeleteSnapshot =
  res
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSnapshot)

responseDeregisterCertificate :: DeregisterCertificateResponse -> TestTree
responseDeregisterCertificate =
  res
    "DeregisterCertificateResponse"
    "fixture/DeregisterCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterCertificate)

responseStartSchemaExtension :: StartSchemaExtensionResponse -> TestTree
responseStartSchemaExtension =
  res
    "StartSchemaExtensionResponse"
    "fixture/StartSchemaExtensionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartSchemaExtension)

responseCreateTrust :: CreateTrustResponse -> TestTree
responseCreateTrust =
  res
    "CreateTrustResponse"
    "fixture/CreateTrustResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTrust)

responseDeleteDirectory :: DeleteDirectoryResponse -> TestTree
responseDeleteDirectory =
  res
    "DeleteDirectoryResponse"
    "fixture/DeleteDirectoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDirectory)

responseCreateSnapshot :: CreateSnapshotResponse -> TestTree
responseCreateSnapshot =
  res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSnapshot)

responseDescribeCertificate :: DescribeCertificateResponse -> TestTree
responseDescribeCertificate =
  res
    "DescribeCertificateResponse"
    "fixture/DescribeCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCertificate)

responseEnableClientAuthentication :: EnableClientAuthenticationResponse -> TestTree
responseEnableClientAuthentication =
  res
    "EnableClientAuthenticationResponse"
    "fixture/EnableClientAuthenticationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableClientAuthentication)

responseCreateComputer :: CreateComputerResponse -> TestTree
responseCreateComputer =
  res
    "CreateComputerResponse"
    "fixture/CreateComputerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateComputer)

responseDescribeSharedDirectories :: DescribeSharedDirectoriesResponse -> TestTree
responseDescribeSharedDirectories =
  res
    "DescribeSharedDirectoriesResponse"
    "fixture/DescribeSharedDirectoriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSharedDirectories)

responseEnableLDAPS :: EnableLDAPSResponse -> TestTree
responseEnableLDAPS =
  res
    "EnableLDAPSResponse"
    "fixture/EnableLDAPSResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableLDAPS)

responseDisableSso :: DisableSsoResponse -> TestTree
responseDisableSso =
  res
    "DisableSsoResponse"
    "fixture/DisableSsoResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableSso)

responseVerifyTrust :: VerifyTrustResponse -> TestTree
responseVerifyTrust =
  res
    "VerifyTrustResponse"
    "fixture/VerifyTrustResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy VerifyTrust)

responseRemoveRegion :: RemoveRegionResponse -> TestTree
responseRemoveRegion =
  res
    "RemoveRegionResponse"
    "fixture/RemoveRegionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveRegion)

responseCreateConditionalForwarder :: CreateConditionalForwarderResponse -> TestTree
responseCreateConditionalForwarder =
  res
    "CreateConditionalForwarderResponse"
    "fixture/CreateConditionalForwarderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConditionalForwarder)

responseDescribeRegions :: DescribeRegionsResponse -> TestTree
responseDescribeRegions =
  res
    "DescribeRegionsResponse"
    "fixture/DescribeRegionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRegions)

responseAddRegion :: AddRegionResponse -> TestTree
responseAddRegion =
  res
    "AddRegionResponse"
    "fixture/AddRegionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddRegion)

responseGetDirectoryLimits :: GetDirectoryLimitsResponse -> TestTree
responseGetDirectoryLimits =
  res
    "GetDirectoryLimitsResponse"
    "fixture/GetDirectoryLimitsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDirectoryLimits)

responseUpdateRadius :: UpdateRadiusResponse -> TestTree
responseUpdateRadius =
  res
    "UpdateRadiusResponse"
    "fixture/UpdateRadiusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRadius)

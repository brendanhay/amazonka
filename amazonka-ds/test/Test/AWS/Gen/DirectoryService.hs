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
--         [ requestConnectDirectory $
--             newConnectDirectory
--
--         , requestRejectSharedDirectory $
--             newRejectSharedDirectory
--
--         , requestDisableRadius $
--             newDisableRadius
--
--         , requestRegisterEventTopic $
--             newRegisterEventTopic
--
--         , requestShareDirectory $
--             newShareDirectory
--
--         , requestAddRegion $
--             newAddRegion
--
--         , requestListIpRoutes $
--             newListIpRoutes
--
--         , requestEnableRadius $
--             newEnableRadius
--
--         , requestListSchemaExtensions $
--             newListSchemaExtensions
--
--         , requestRemoveRegion $
--             newRemoveRegion
--
--         , requestDeleteLogSubscription $
--             newDeleteLogSubscription
--
--         , requestCancelSchemaExtension $
--             newCancelSchemaExtension
--
--         , requestEnableSso $
--             newEnableSso
--
--         , requestCreateConditionalForwarder $
--             newCreateConditionalForwarder
--
--         , requestRemoveTagsFromResource $
--             newRemoveTagsFromResource
--
--         , requestEnableLDAPS $
--             newEnableLDAPS
--
--         , requestDeleteConditionalForwarder $
--             newDeleteConditionalForwarder
--
--         , requestDescribeSharedDirectories $
--             newDescribeSharedDirectories
--
--         , requestUpdateConditionalForwarder $
--             newUpdateConditionalForwarder
--
--         , requestVerifyTrust $
--             newVerifyTrust
--
--         , requestDescribeCertificate $
--             newDescribeCertificate
--
--         , requestCreateTrust $
--             newCreateTrust
--
--         , requestDeleteDirectory $
--             newDeleteDirectory
--
--         , requestDisableClientAuthentication $
--             newDisableClientAuthentication
--
--         , requestCreateMicrosoftAD $
--             newCreateMicrosoftAD
--
--         , requestDeleteSnapshot $
--             newDeleteSnapshot
--
--         , requestRemoveIpRoutes $
--             newRemoveIpRoutes
--
--         , requestUpdateTrust $
--             newUpdateTrust
--
--         , requestDeleteTrust $
--             newDeleteTrust
--
--         , requestCreateDirectory $
--             newCreateDirectory
--
--         , requestRestoreFromSnapshot $
--             newRestoreFromSnapshot
--
--         , requestDescribeDomainControllers $
--             newDescribeDomainControllers
--
--         , requestDescribeTrusts $
--             newDescribeTrusts
--
--         , requestDescribeSnapshots $
--             newDescribeSnapshots
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
--         , requestGetDirectoryLimits $
--             newGetDirectoryLimits
--
--         , requestUpdateRadius $
--             newUpdateRadius
--
--         , requestDisableLDAPS $
--             newDisableLDAPS
--
--         , requestListLogSubscriptions $
--             newListLogSubscriptions
--
--         , requestDescribeRegions $
--             newDescribeRegions
--
--         , requestDisableSso $
--             newDisableSso
--
--         , requestCreateLogSubscription $
--             newCreateLogSubscription
--
--         , requestResetUserPassword $
--             newResetUserPassword
--
--         , requestDescribeEventTopics $
--             newDescribeEventTopics
--
--         , requestCreateComputer $
--             newCreateComputer
--
--         , requestAcceptSharedDirectory $
--             newAcceptSharedDirectory
--
--         , requestEnableClientAuthentication $
--             newEnableClientAuthentication
--
--         , requestCreateSnapshot $
--             newCreateSnapshot
--
--         , requestStartSchemaExtension $
--             newStartSchemaExtension
--
--         , requestDeregisterEventTopic $
--             newDeregisterEventTopic
--
--         , requestDeregisterCertificate $
--             newDeregisterCertificate
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestCreateAlias $
--             newCreateAlias
--
--         , requestAddIpRoutes $
--             newAddIpRoutes
--
--         , requestDescribeDirectories $
--             newDescribeDirectories
--
--         , requestDescribeLDAPSSettings $
--             newDescribeLDAPSSettings
--
--           ]

--     , testGroup "response"
--         [ responseConnectDirectory $
--             newConnectDirectoryResponse
--
--         , responseRejectSharedDirectory $
--             newRejectSharedDirectoryResponse
--
--         , responseDisableRadius $
--             newDisableRadiusResponse
--
--         , responseRegisterEventTopic $
--             newRegisterEventTopicResponse
--
--         , responseShareDirectory $
--             newShareDirectoryResponse
--
--         , responseAddRegion $
--             newAddRegionResponse
--
--         , responseListIpRoutes $
--             newListIpRoutesResponse
--
--         , responseEnableRadius $
--             newEnableRadiusResponse
--
--         , responseListSchemaExtensions $
--             newListSchemaExtensionsResponse
--
--         , responseRemoveRegion $
--             newRemoveRegionResponse
--
--         , responseDeleteLogSubscription $
--             newDeleteLogSubscriptionResponse
--
--         , responseCancelSchemaExtension $
--             newCancelSchemaExtensionResponse
--
--         , responseEnableSso $
--             newEnableSsoResponse
--
--         , responseCreateConditionalForwarder $
--             newCreateConditionalForwarderResponse
--
--         , responseRemoveTagsFromResource $
--             newRemoveTagsFromResourceResponse
--
--         , responseEnableLDAPS $
--             newEnableLDAPSResponse
--
--         , responseDeleteConditionalForwarder $
--             newDeleteConditionalForwarderResponse
--
--         , responseDescribeSharedDirectories $
--             newDescribeSharedDirectoriesResponse
--
--         , responseUpdateConditionalForwarder $
--             newUpdateConditionalForwarderResponse
--
--         , responseVerifyTrust $
--             newVerifyTrustResponse
--
--         , responseDescribeCertificate $
--             newDescribeCertificateResponse
--
--         , responseCreateTrust $
--             newCreateTrustResponse
--
--         , responseDeleteDirectory $
--             newDeleteDirectoryResponse
--
--         , responseDisableClientAuthentication $
--             newDisableClientAuthenticationResponse
--
--         , responseCreateMicrosoftAD $
--             newCreateMicrosoftADResponse
--
--         , responseDeleteSnapshot $
--             newDeleteSnapshotResponse
--
--         , responseRemoveIpRoutes $
--             newRemoveIpRoutesResponse
--
--         , responseUpdateTrust $
--             newUpdateTrustResponse
--
--         , responseDeleteTrust $
--             newDeleteTrustResponse
--
--         , responseCreateDirectory $
--             newCreateDirectoryResponse
--
--         , responseRestoreFromSnapshot $
--             newRestoreFromSnapshotResponse
--
--         , responseDescribeDomainControllers $
--             newDescribeDomainControllersResponse
--
--         , responseDescribeTrusts $
--             newDescribeTrustsResponse
--
--         , responseDescribeSnapshots $
--             newDescribeSnapshotsResponse
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
--         , responseGetDirectoryLimits $
--             newGetDirectoryLimitsResponse
--
--         , responseUpdateRadius $
--             newUpdateRadiusResponse
--
--         , responseDisableLDAPS $
--             newDisableLDAPSResponse
--
--         , responseListLogSubscriptions $
--             newListLogSubscriptionsResponse
--
--         , responseDescribeRegions $
--             newDescribeRegionsResponse
--
--         , responseDisableSso $
--             newDisableSsoResponse
--
--         , responseCreateLogSubscription $
--             newCreateLogSubscriptionResponse
--
--         , responseResetUserPassword $
--             newResetUserPasswordResponse
--
--         , responseDescribeEventTopics $
--             newDescribeEventTopicsResponse
--
--         , responseCreateComputer $
--             newCreateComputerResponse
--
--         , responseAcceptSharedDirectory $
--             newAcceptSharedDirectoryResponse
--
--         , responseEnableClientAuthentication $
--             newEnableClientAuthenticationResponse
--
--         , responseCreateSnapshot $
--             newCreateSnapshotResponse
--
--         , responseStartSchemaExtension $
--             newStartSchemaExtensionResponse
--
--         , responseDeregisterEventTopic $
--             newDeregisterEventTopicResponse
--
--         , responseDeregisterCertificate $
--             newDeregisterCertificateResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseCreateAlias $
--             newCreateAliasResponse
--
--         , responseAddIpRoutes $
--             newAddIpRoutesResponse
--
--         , responseDescribeDirectories $
--             newDescribeDirectoriesResponse
--
--         , responseDescribeLDAPSSettings $
--             newDescribeLDAPSSettingsResponse
--
--           ]
--     ]

-- Requests

requestConnectDirectory :: ConnectDirectory -> TestTree
requestConnectDirectory =
  req
    "ConnectDirectory"
    "fixture/ConnectDirectory.yaml"

requestRejectSharedDirectory :: RejectSharedDirectory -> TestTree
requestRejectSharedDirectory =
  req
    "RejectSharedDirectory"
    "fixture/RejectSharedDirectory.yaml"

requestDisableRadius :: DisableRadius -> TestTree
requestDisableRadius =
  req
    "DisableRadius"
    "fixture/DisableRadius.yaml"

requestRegisterEventTopic :: RegisterEventTopic -> TestTree
requestRegisterEventTopic =
  req
    "RegisterEventTopic"
    "fixture/RegisterEventTopic.yaml"

requestShareDirectory :: ShareDirectory -> TestTree
requestShareDirectory =
  req
    "ShareDirectory"
    "fixture/ShareDirectory.yaml"

requestAddRegion :: AddRegion -> TestTree
requestAddRegion =
  req
    "AddRegion"
    "fixture/AddRegion.yaml"

requestListIpRoutes :: ListIpRoutes -> TestTree
requestListIpRoutes =
  req
    "ListIpRoutes"
    "fixture/ListIpRoutes.yaml"

requestEnableRadius :: EnableRadius -> TestTree
requestEnableRadius =
  req
    "EnableRadius"
    "fixture/EnableRadius.yaml"

requestListSchemaExtensions :: ListSchemaExtensions -> TestTree
requestListSchemaExtensions =
  req
    "ListSchemaExtensions"
    "fixture/ListSchemaExtensions.yaml"

requestRemoveRegion :: RemoveRegion -> TestTree
requestRemoveRegion =
  req
    "RemoveRegion"
    "fixture/RemoveRegion.yaml"

requestDeleteLogSubscription :: DeleteLogSubscription -> TestTree
requestDeleteLogSubscription =
  req
    "DeleteLogSubscription"
    "fixture/DeleteLogSubscription.yaml"

requestCancelSchemaExtension :: CancelSchemaExtension -> TestTree
requestCancelSchemaExtension =
  req
    "CancelSchemaExtension"
    "fixture/CancelSchemaExtension.yaml"

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

requestRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
requestRemoveTagsFromResource =
  req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource.yaml"

requestEnableLDAPS :: EnableLDAPS -> TestTree
requestEnableLDAPS =
  req
    "EnableLDAPS"
    "fixture/EnableLDAPS.yaml"

requestDeleteConditionalForwarder :: DeleteConditionalForwarder -> TestTree
requestDeleteConditionalForwarder =
  req
    "DeleteConditionalForwarder"
    "fixture/DeleteConditionalForwarder.yaml"

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

requestVerifyTrust :: VerifyTrust -> TestTree
requestVerifyTrust =
  req
    "VerifyTrust"
    "fixture/VerifyTrust.yaml"

requestDescribeCertificate :: DescribeCertificate -> TestTree
requestDescribeCertificate =
  req
    "DescribeCertificate"
    "fixture/DescribeCertificate.yaml"

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

requestDisableClientAuthentication :: DisableClientAuthentication -> TestTree
requestDisableClientAuthentication =
  req
    "DisableClientAuthentication"
    "fixture/DisableClientAuthentication.yaml"

requestCreateMicrosoftAD :: CreateMicrosoftAD -> TestTree
requestCreateMicrosoftAD =
  req
    "CreateMicrosoftAD"
    "fixture/CreateMicrosoftAD.yaml"

requestDeleteSnapshot :: DeleteSnapshot -> TestTree
requestDeleteSnapshot =
  req
    "DeleteSnapshot"
    "fixture/DeleteSnapshot.yaml"

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

requestDeleteTrust :: DeleteTrust -> TestTree
requestDeleteTrust =
  req
    "DeleteTrust"
    "fixture/DeleteTrust.yaml"

requestCreateDirectory :: CreateDirectory -> TestTree
requestCreateDirectory =
  req
    "CreateDirectory"
    "fixture/CreateDirectory.yaml"

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

requestDescribeTrusts :: DescribeTrusts -> TestTree
requestDescribeTrusts =
  req
    "DescribeTrusts"
    "fixture/DescribeTrusts.yaml"

requestDescribeSnapshots :: DescribeSnapshots -> TestTree
requestDescribeSnapshots =
  req
    "DescribeSnapshots"
    "fixture/DescribeSnapshots.yaml"

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

requestDisableLDAPS :: DisableLDAPS -> TestTree
requestDisableLDAPS =
  req
    "DisableLDAPS"
    "fixture/DisableLDAPS.yaml"

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

requestResetUserPassword :: ResetUserPassword -> TestTree
requestResetUserPassword =
  req
    "ResetUserPassword"
    "fixture/ResetUserPassword.yaml"

requestDescribeEventTopics :: DescribeEventTopics -> TestTree
requestDescribeEventTopics =
  req
    "DescribeEventTopics"
    "fixture/DescribeEventTopics.yaml"

requestCreateComputer :: CreateComputer -> TestTree
requestCreateComputer =
  req
    "CreateComputer"
    "fixture/CreateComputer.yaml"

requestAcceptSharedDirectory :: AcceptSharedDirectory -> TestTree
requestAcceptSharedDirectory =
  req
    "AcceptSharedDirectory"
    "fixture/AcceptSharedDirectory.yaml"

requestEnableClientAuthentication :: EnableClientAuthentication -> TestTree
requestEnableClientAuthentication =
  req
    "EnableClientAuthentication"
    "fixture/EnableClientAuthentication.yaml"

requestCreateSnapshot :: CreateSnapshot -> TestTree
requestCreateSnapshot =
  req
    "CreateSnapshot"
    "fixture/CreateSnapshot.yaml"

requestStartSchemaExtension :: StartSchemaExtension -> TestTree
requestStartSchemaExtension =
  req
    "StartSchemaExtension"
    "fixture/StartSchemaExtension.yaml"

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

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestCreateAlias :: CreateAlias -> TestTree
requestCreateAlias =
  req
    "CreateAlias"
    "fixture/CreateAlias.yaml"

requestAddIpRoutes :: AddIpRoutes -> TestTree
requestAddIpRoutes =
  req
    "AddIpRoutes"
    "fixture/AddIpRoutes.yaml"

requestDescribeDirectories :: DescribeDirectories -> TestTree
requestDescribeDirectories =
  req
    "DescribeDirectories"
    "fixture/DescribeDirectories.yaml"

requestDescribeLDAPSSettings :: DescribeLDAPSSettings -> TestTree
requestDescribeLDAPSSettings =
  req
    "DescribeLDAPSSettings"
    "fixture/DescribeLDAPSSettings.yaml"

-- Responses

responseConnectDirectory :: ConnectDirectoryResponse -> TestTree
responseConnectDirectory =
  res
    "ConnectDirectoryResponse"
    "fixture/ConnectDirectoryResponse.proto"
    defaultService
    (Proxy :: Proxy ConnectDirectory)

responseRejectSharedDirectory :: RejectSharedDirectoryResponse -> TestTree
responseRejectSharedDirectory =
  res
    "RejectSharedDirectoryResponse"
    "fixture/RejectSharedDirectoryResponse.proto"
    defaultService
    (Proxy :: Proxy RejectSharedDirectory)

responseDisableRadius :: DisableRadiusResponse -> TestTree
responseDisableRadius =
  res
    "DisableRadiusResponse"
    "fixture/DisableRadiusResponse.proto"
    defaultService
    (Proxy :: Proxy DisableRadius)

responseRegisterEventTopic :: RegisterEventTopicResponse -> TestTree
responseRegisterEventTopic =
  res
    "RegisterEventTopicResponse"
    "fixture/RegisterEventTopicResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterEventTopic)

responseShareDirectory :: ShareDirectoryResponse -> TestTree
responseShareDirectory =
  res
    "ShareDirectoryResponse"
    "fixture/ShareDirectoryResponse.proto"
    defaultService
    (Proxy :: Proxy ShareDirectory)

responseAddRegion :: AddRegionResponse -> TestTree
responseAddRegion =
  res
    "AddRegionResponse"
    "fixture/AddRegionResponse.proto"
    defaultService
    (Proxy :: Proxy AddRegion)

responseListIpRoutes :: ListIpRoutesResponse -> TestTree
responseListIpRoutes =
  res
    "ListIpRoutesResponse"
    "fixture/ListIpRoutesResponse.proto"
    defaultService
    (Proxy :: Proxy ListIpRoutes)

responseEnableRadius :: EnableRadiusResponse -> TestTree
responseEnableRadius =
  res
    "EnableRadiusResponse"
    "fixture/EnableRadiusResponse.proto"
    defaultService
    (Proxy :: Proxy EnableRadius)

responseListSchemaExtensions :: ListSchemaExtensionsResponse -> TestTree
responseListSchemaExtensions =
  res
    "ListSchemaExtensionsResponse"
    "fixture/ListSchemaExtensionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSchemaExtensions)

responseRemoveRegion :: RemoveRegionResponse -> TestTree
responseRemoveRegion =
  res
    "RemoveRegionResponse"
    "fixture/RemoveRegionResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveRegion)

responseDeleteLogSubscription :: DeleteLogSubscriptionResponse -> TestTree
responseDeleteLogSubscription =
  res
    "DeleteLogSubscriptionResponse"
    "fixture/DeleteLogSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLogSubscription)

responseCancelSchemaExtension :: CancelSchemaExtensionResponse -> TestTree
responseCancelSchemaExtension =
  res
    "CancelSchemaExtensionResponse"
    "fixture/CancelSchemaExtensionResponse.proto"
    defaultService
    (Proxy :: Proxy CancelSchemaExtension)

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

responseRemoveTagsFromResource :: RemoveTagsFromResourceResponse -> TestTree
responseRemoveTagsFromResource =
  res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveTagsFromResource)

responseEnableLDAPS :: EnableLDAPSResponse -> TestTree
responseEnableLDAPS =
  res
    "EnableLDAPSResponse"
    "fixture/EnableLDAPSResponse.proto"
    defaultService
    (Proxy :: Proxy EnableLDAPS)

responseDeleteConditionalForwarder :: DeleteConditionalForwarderResponse -> TestTree
responseDeleteConditionalForwarder =
  res
    "DeleteConditionalForwarderResponse"
    "fixture/DeleteConditionalForwarderResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConditionalForwarder)

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

responseVerifyTrust :: VerifyTrustResponse -> TestTree
responseVerifyTrust =
  res
    "VerifyTrustResponse"
    "fixture/VerifyTrustResponse.proto"
    defaultService
    (Proxy :: Proxy VerifyTrust)

responseDescribeCertificate :: DescribeCertificateResponse -> TestTree
responseDescribeCertificate =
  res
    "DescribeCertificateResponse"
    "fixture/DescribeCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCertificate)

responseCreateTrust :: CreateTrustResponse -> TestTree
responseCreateTrust =
  res
    "CreateTrustResponse"
    "fixture/CreateTrustResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTrust)

responseDeleteDirectory :: DeleteDirectoryResponse -> TestTree
responseDeleteDirectory =
  res
    "DeleteDirectoryResponse"
    "fixture/DeleteDirectoryResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDirectory)

responseDisableClientAuthentication :: DisableClientAuthenticationResponse -> TestTree
responseDisableClientAuthentication =
  res
    "DisableClientAuthenticationResponse"
    "fixture/DisableClientAuthenticationResponse.proto"
    defaultService
    (Proxy :: Proxy DisableClientAuthentication)

responseCreateMicrosoftAD :: CreateMicrosoftADResponse -> TestTree
responseCreateMicrosoftAD =
  res
    "CreateMicrosoftADResponse"
    "fixture/CreateMicrosoftADResponse.proto"
    defaultService
    (Proxy :: Proxy CreateMicrosoftAD)

responseDeleteSnapshot :: DeleteSnapshotResponse -> TestTree
responseDeleteSnapshot =
  res
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSnapshot)

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

responseDeleteTrust :: DeleteTrustResponse -> TestTree
responseDeleteTrust =
  res
    "DeleteTrustResponse"
    "fixture/DeleteTrustResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTrust)

responseCreateDirectory :: CreateDirectoryResponse -> TestTree
responseCreateDirectory =
  res
    "CreateDirectoryResponse"
    "fixture/CreateDirectoryResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDirectory)

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

responseDescribeTrusts :: DescribeTrustsResponse -> TestTree
responseDescribeTrusts =
  res
    "DescribeTrustsResponse"
    "fixture/DescribeTrustsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTrusts)

responseDescribeSnapshots :: DescribeSnapshotsResponse -> TestTree
responseDescribeSnapshots =
  res
    "DescribeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSnapshots)

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

responseGetDirectoryLimits :: GetDirectoryLimitsResponse -> TestTree
responseGetDirectoryLimits =
  res
    "GetDirectoryLimitsResponse"
    "fixture/GetDirectoryLimitsResponse.proto"
    defaultService
    (Proxy :: Proxy GetDirectoryLimits)

responseUpdateRadius :: UpdateRadiusResponse -> TestTree
responseUpdateRadius =
  res
    "UpdateRadiusResponse"
    "fixture/UpdateRadiusResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRadius)

responseDisableLDAPS :: DisableLDAPSResponse -> TestTree
responseDisableLDAPS =
  res
    "DisableLDAPSResponse"
    "fixture/DisableLDAPSResponse.proto"
    defaultService
    (Proxy :: Proxy DisableLDAPS)

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

responseResetUserPassword :: ResetUserPasswordResponse -> TestTree
responseResetUserPassword =
  res
    "ResetUserPasswordResponse"
    "fixture/ResetUserPasswordResponse.proto"
    defaultService
    (Proxy :: Proxy ResetUserPassword)

responseDescribeEventTopics :: DescribeEventTopicsResponse -> TestTree
responseDescribeEventTopics =
  res
    "DescribeEventTopicsResponse"
    "fixture/DescribeEventTopicsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEventTopics)

responseCreateComputer :: CreateComputerResponse -> TestTree
responseCreateComputer =
  res
    "CreateComputerResponse"
    "fixture/CreateComputerResponse.proto"
    defaultService
    (Proxy :: Proxy CreateComputer)

responseAcceptSharedDirectory :: AcceptSharedDirectoryResponse -> TestTree
responseAcceptSharedDirectory =
  res
    "AcceptSharedDirectoryResponse"
    "fixture/AcceptSharedDirectoryResponse.proto"
    defaultService
    (Proxy :: Proxy AcceptSharedDirectory)

responseEnableClientAuthentication :: EnableClientAuthenticationResponse -> TestTree
responseEnableClientAuthentication =
  res
    "EnableClientAuthenticationResponse"
    "fixture/EnableClientAuthenticationResponse.proto"
    defaultService
    (Proxy :: Proxy EnableClientAuthentication)

responseCreateSnapshot :: CreateSnapshotResponse -> TestTree
responseCreateSnapshot =
  res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSnapshot)

responseStartSchemaExtension :: StartSchemaExtensionResponse -> TestTree
responseStartSchemaExtension =
  res
    "StartSchemaExtensionResponse"
    "fixture/StartSchemaExtensionResponse.proto"
    defaultService
    (Proxy :: Proxy StartSchemaExtension)

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

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseCreateAlias :: CreateAliasResponse -> TestTree
responseCreateAlias =
  res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAlias)

responseAddIpRoutes :: AddIpRoutesResponse -> TestTree
responseAddIpRoutes =
  res
    "AddIpRoutesResponse"
    "fixture/AddIpRoutesResponse.proto"
    defaultService
    (Proxy :: Proxy AddIpRoutes)

responseDescribeDirectories :: DescribeDirectoriesResponse -> TestTree
responseDescribeDirectories =
  res
    "DescribeDirectoriesResponse"
    "fixture/DescribeDirectoriesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDirectories)

responseDescribeLDAPSSettings :: DescribeLDAPSSettingsResponse -> TestTree
responseDescribeLDAPSSettings =
  res
    "DescribeLDAPSSettingsResponse"
    "fixture/DescribeLDAPSSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLDAPSSettings)

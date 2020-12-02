{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DirectoryService
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--         [ requestShareDirectory $
--             shareDirectory
--
--         , requestUpdateNumberOfDomainControllers $
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
--         , requestRegisterCertificate $
--             registerCertificate
--
--         , requestConnectDirectory $
--             connectDirectory
--
--         , requestDescribeLDAPSSettings $
--             describeLDAPSSettings
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
--         , requestUpdateTrust $
--             updateTrust
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
--         , requestAcceptSharedDirectory $
--             acceptSharedDirectory
--
--         , requestCreateLogSubscription $
--             createLogSubscription
--
--         , requestRemoveTagsFromResource $
--             removeTagsFromResource
--
--         , requestDescribeEventTopics $
--             describeEventTopics
--
--         , requestResetUserPassword $
--             resetUserPassword
--
--         , requestUpdateConditionalForwarder $
--             updateConditionalForwarder
--
--         , requestDeleteConditionalForwarder $
--             deleteConditionalForwarder
--
--         , requestDisableLDAPS $
--             disableLDAPS
--
--         , requestDeleteLogSubscription $
--             deleteLogSubscription
--
--         , requestEnableSSO $
--             enableSSO
--
--         , requestCancelSchemaExtension $
--             cancelSchemaExtension
--
--         , requestListLogSubscriptions $
--             listLogSubscriptions
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
--         , requestListCertificates $
--             listCertificates
--
--         , requestRejectSharedDirectory $
--             rejectSharedDirectory
--
--         , requestUnshareDirectory $
--             unshareDirectory
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
--         , requestDeregisterCertificate $
--             deregisterCertificate
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
--         , requestDescribeCertificate $
--             describeCertificate
--
--         , requestCreateComputer $
--             createComputer
--
--         , requestDescribeSharedDirectories $
--             describeSharedDirectories
--
--         , requestEnableLDAPS $
--             enableLDAPS
--
--         , requestDisableSSO $
--             disableSSO
--
--         , requestVerifyTrust $
--             verifyTrust
--
--         , requestRemoveRegion $
--             removeRegion
--
--         , requestCreateConditionalForwarder $
--             createConditionalForwarder
--
--         , requestDescribeRegions $
--             describeRegions
--
--         , requestAddRegion $
--             addRegion
--
--         , requestGetDirectoryLimits $
--             getDirectoryLimits
--
--         , requestUpdateRadius $
--             updateRadius
--
--           ]

--     , testGroup "response"
--         [ responseShareDirectory $
--             shareDirectoryResponse
--
--         , responseUpdateNumberOfDomainControllers $
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
--         , responseRegisterCertificate $
--             registerCertificateResponse
--
--         , responseConnectDirectory $
--             connectDirectoryResponse
--
--         , responseDescribeLDAPSSettings $
--             describeLDAPSSettingsResponse
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
--         , responseUpdateTrust $
--             updateTrustResponse
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
--         , responseAcceptSharedDirectory $
--             acceptSharedDirectoryResponse
--
--         , responseCreateLogSubscription $
--             createLogSubscriptionResponse
--
--         , responseRemoveTagsFromResource $
--             removeTagsFromResourceResponse
--
--         , responseDescribeEventTopics $
--             describeEventTopicsResponse
--
--         , responseResetUserPassword $
--             resetUserPasswordResponse
--
--         , responseUpdateConditionalForwarder $
--             updateConditionalForwarderResponse
--
--         , responseDeleteConditionalForwarder $
--             deleteConditionalForwarderResponse
--
--         , responseDisableLDAPS $
--             disableLDAPSResponse
--
--         , responseDeleteLogSubscription $
--             deleteLogSubscriptionResponse
--
--         , responseEnableSSO $
--             enableSSOResponse
--
--         , responseCancelSchemaExtension $
--             cancelSchemaExtensionResponse
--
--         , responseListLogSubscriptions $
--             listLogSubscriptionsResponse
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
--         , responseListCertificates $
--             listCertificatesResponse
--
--         , responseRejectSharedDirectory $
--             rejectSharedDirectoryResponse
--
--         , responseUnshareDirectory $
--             unshareDirectoryResponse
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
--         , responseDeregisterCertificate $
--             deregisterCertificateResponse
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
--         , responseDescribeCertificate $
--             describeCertificateResponse
--
--         , responseCreateComputer $
--             createComputerResponse
--
--         , responseDescribeSharedDirectories $
--             describeSharedDirectoriesResponse
--
--         , responseEnableLDAPS $
--             enableLDAPSResponse
--
--         , responseDisableSSO $
--             disableSSOResponse
--
--         , responseVerifyTrust $
--             verifyTrustResponse
--
--         , responseRemoveRegion $
--             removeRegionResponse
--
--         , responseCreateConditionalForwarder $
--             createConditionalForwarderResponse
--
--         , responseDescribeRegions $
--             describeRegionsResponse
--
--         , responseAddRegion $
--             addRegionResponse
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

requestAddIPRoutes :: AddIPRoutes -> TestTree
requestAddIPRoutes =
  req
    "AddIPRoutes"
    "fixture/AddIPRoutes.yaml"

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

requestEnableSSO :: EnableSSO -> TestTree
requestEnableSSO =
  req
    "EnableSSO"
    "fixture/EnableSSO.yaml"

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

requestListIPRoutes :: ListIPRoutes -> TestTree
requestListIPRoutes =
  req
    "ListIPRoutes"
    "fixture/ListIPRoutes.yaml"

requestAddTagsToResource :: AddTagsToResource -> TestTree
requestAddTagsToResource =
  req
    "AddTagsToResource"
    "fixture/AddTagsToResource.yaml"

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

requestRemoveIPRoutes :: RemoveIPRoutes -> TestTree
requestRemoveIPRoutes =
  req
    "RemoveIPRoutes"
    "fixture/RemoveIPRoutes.yaml"

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

requestDisableSSO :: DisableSSO -> TestTree
requestDisableSSO =
  req
    "DisableSSO"
    "fixture/DisableSSO.yaml"

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
    directoryService
    (Proxy :: Proxy ShareDirectory)

responseUpdateNumberOfDomainControllers :: UpdateNumberOfDomainControllersResponse -> TestTree
responseUpdateNumberOfDomainControllers =
  res
    "UpdateNumberOfDomainControllersResponse"
    "fixture/UpdateNumberOfDomainControllersResponse.proto"
    directoryService
    (Proxy :: Proxy UpdateNumberOfDomainControllers)

responseDescribeConditionalForwarders :: DescribeConditionalForwardersResponse -> TestTree
responseDescribeConditionalForwarders =
  res
    "DescribeConditionalForwardersResponse"
    "fixture/DescribeConditionalForwardersResponse.proto"
    directoryService
    (Proxy :: Proxy DescribeConditionalForwarders)

responseGetSnapshotLimits :: GetSnapshotLimitsResponse -> TestTree
responseGetSnapshotLimits =
  res
    "GetSnapshotLimitsResponse"
    "fixture/GetSnapshotLimitsResponse.proto"
    directoryService
    (Proxy :: Proxy GetSnapshotLimits)

responseRegisterEventTopic :: RegisterEventTopicResponse -> TestTree
responseRegisterEventTopic =
  res
    "RegisterEventTopicResponse"
    "fixture/RegisterEventTopicResponse.proto"
    directoryService
    (Proxy :: Proxy RegisterEventTopic)

responseRegisterCertificate :: RegisterCertificateResponse -> TestTree
responseRegisterCertificate =
  res
    "RegisterCertificateResponse"
    "fixture/RegisterCertificateResponse.proto"
    directoryService
    (Proxy :: Proxy RegisterCertificate)

responseConnectDirectory :: ConnectDirectoryResponse -> TestTree
responseConnectDirectory =
  res
    "ConnectDirectoryResponse"
    "fixture/ConnectDirectoryResponse.proto"
    directoryService
    (Proxy :: Proxy ConnectDirectory)

responseDescribeLDAPSSettings :: DescribeLDAPSSettingsResponse -> TestTree
responseDescribeLDAPSSettings =
  res
    "DescribeLDAPSSettingsResponse"
    "fixture/DescribeLDAPSSettingsResponse.proto"
    directoryService
    (Proxy :: Proxy DescribeLDAPSSettings)

responseCreateAlias :: CreateAliasResponse -> TestTree
responseCreateAlias =
  res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse.proto"
    directoryService
    (Proxy :: Proxy CreateAlias)

responseDescribeDirectories :: DescribeDirectoriesResponse -> TestTree
responseDescribeDirectories =
  res
    "DescribeDirectoriesResponse"
    "fixture/DescribeDirectoriesResponse.proto"
    directoryService
    (Proxy :: Proxy DescribeDirectories)

responseAddIPRoutes :: AddIPRoutesResponse -> TestTree
responseAddIPRoutes =
  res
    "AddIPRoutesResponse"
    "fixture/AddIPRoutesResponse.proto"
    directoryService
    (Proxy :: Proxy AddIPRoutes)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    directoryService
    (Proxy :: Proxy ListTagsForResource)

responseDescribeTrusts :: DescribeTrustsResponse -> TestTree
responseDescribeTrusts =
  res
    "DescribeTrustsResponse"
    "fixture/DescribeTrustsResponse.proto"
    directoryService
    (Proxy :: Proxy DescribeTrusts)

responseDeleteTrust :: DeleteTrustResponse -> TestTree
responseDeleteTrust =
  res
    "DeleteTrustResponse"
    "fixture/DeleteTrustResponse.proto"
    directoryService
    (Proxy :: Proxy DeleteTrust)

responseUpdateTrust :: UpdateTrustResponse -> TestTree
responseUpdateTrust =
  res
    "UpdateTrustResponse"
    "fixture/UpdateTrustResponse.proto"
    directoryService
    (Proxy :: Proxy UpdateTrust)

responseCreateMicrosoftAD :: CreateMicrosoftADResponse -> TestTree
responseCreateMicrosoftAD =
  res
    "CreateMicrosoftADResponse"
    "fixture/CreateMicrosoftADResponse.proto"
    directoryService
    (Proxy :: Proxy CreateMicrosoftAD)

responseDeregisterEventTopic :: DeregisterEventTopicResponse -> TestTree
responseDeregisterEventTopic =
  res
    "DeregisterEventTopicResponse"
    "fixture/DeregisterEventTopicResponse.proto"
    directoryService
    (Proxy :: Proxy DeregisterEventTopic)

responseCreateDirectory :: CreateDirectoryResponse -> TestTree
responseCreateDirectory =
  res
    "CreateDirectoryResponse"
    "fixture/CreateDirectoryResponse.proto"
    directoryService
    (Proxy :: Proxy CreateDirectory)

responseAcceptSharedDirectory :: AcceptSharedDirectoryResponse -> TestTree
responseAcceptSharedDirectory =
  res
    "AcceptSharedDirectoryResponse"
    "fixture/AcceptSharedDirectoryResponse.proto"
    directoryService
    (Proxy :: Proxy AcceptSharedDirectory)

responseCreateLogSubscription :: CreateLogSubscriptionResponse -> TestTree
responseCreateLogSubscription =
  res
    "CreateLogSubscriptionResponse"
    "fixture/CreateLogSubscriptionResponse.proto"
    directoryService
    (Proxy :: Proxy CreateLogSubscription)

responseRemoveTagsFromResource :: RemoveTagsFromResourceResponse -> TestTree
responseRemoveTagsFromResource =
  res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    directoryService
    (Proxy :: Proxy RemoveTagsFromResource)

responseDescribeEventTopics :: DescribeEventTopicsResponse -> TestTree
responseDescribeEventTopics =
  res
    "DescribeEventTopicsResponse"
    "fixture/DescribeEventTopicsResponse.proto"
    directoryService
    (Proxy :: Proxy DescribeEventTopics)

responseResetUserPassword :: ResetUserPasswordResponse -> TestTree
responseResetUserPassword =
  res
    "ResetUserPasswordResponse"
    "fixture/ResetUserPasswordResponse.proto"
    directoryService
    (Proxy :: Proxy ResetUserPassword)

responseUpdateConditionalForwarder :: UpdateConditionalForwarderResponse -> TestTree
responseUpdateConditionalForwarder =
  res
    "UpdateConditionalForwarderResponse"
    "fixture/UpdateConditionalForwarderResponse.proto"
    directoryService
    (Proxy :: Proxy UpdateConditionalForwarder)

responseDeleteConditionalForwarder :: DeleteConditionalForwarderResponse -> TestTree
responseDeleteConditionalForwarder =
  res
    "DeleteConditionalForwarderResponse"
    "fixture/DeleteConditionalForwarderResponse.proto"
    directoryService
    (Proxy :: Proxy DeleteConditionalForwarder)

responseDisableLDAPS :: DisableLDAPSResponse -> TestTree
responseDisableLDAPS =
  res
    "DisableLDAPSResponse"
    "fixture/DisableLDAPSResponse.proto"
    directoryService
    (Proxy :: Proxy DisableLDAPS)

responseDeleteLogSubscription :: DeleteLogSubscriptionResponse -> TestTree
responseDeleteLogSubscription =
  res
    "DeleteLogSubscriptionResponse"
    "fixture/DeleteLogSubscriptionResponse.proto"
    directoryService
    (Proxy :: Proxy DeleteLogSubscription)

responseEnableSSO :: EnableSSOResponse -> TestTree
responseEnableSSO =
  res
    "EnableSSOResponse"
    "fixture/EnableSSOResponse.proto"
    directoryService
    (Proxy :: Proxy EnableSSO)

responseCancelSchemaExtension :: CancelSchemaExtensionResponse -> TestTree
responseCancelSchemaExtension =
  res
    "CancelSchemaExtensionResponse"
    "fixture/CancelSchemaExtensionResponse.proto"
    directoryService
    (Proxy :: Proxy CancelSchemaExtension)

responseListLogSubscriptions :: ListLogSubscriptionsResponse -> TestTree
responseListLogSubscriptions =
  res
    "ListLogSubscriptionsResponse"
    "fixture/ListLogSubscriptionsResponse.proto"
    directoryService
    (Proxy :: Proxy ListLogSubscriptions)

responseEnableRadius :: EnableRadiusResponse -> TestTree
responseEnableRadius =
  res
    "EnableRadiusResponse"
    "fixture/EnableRadiusResponse.proto"
    directoryService
    (Proxy :: Proxy EnableRadius)

responseListIPRoutes :: ListIPRoutesResponse -> TestTree
responseListIPRoutes =
  res
    "ListIPRoutesResponse"
    "fixture/ListIPRoutesResponse.proto"
    directoryService
    (Proxy :: Proxy ListIPRoutes)

responseAddTagsToResource :: AddTagsToResourceResponse -> TestTree
responseAddTagsToResource =
  res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    directoryService
    (Proxy :: Proxy AddTagsToResource)

responseListSchemaExtensions :: ListSchemaExtensionsResponse -> TestTree
responseListSchemaExtensions =
  res
    "ListSchemaExtensionsResponse"
    "fixture/ListSchemaExtensionsResponse.proto"
    directoryService
    (Proxy :: Proxy ListSchemaExtensions)

responseDisableRadius :: DisableRadiusResponse -> TestTree
responseDisableRadius =
  res
    "DisableRadiusResponse"
    "fixture/DisableRadiusResponse.proto"
    directoryService
    (Proxy :: Proxy DisableRadius)

responseListCertificates :: ListCertificatesResponse -> TestTree
responseListCertificates =
  res
    "ListCertificatesResponse"
    "fixture/ListCertificatesResponse.proto"
    directoryService
    (Proxy :: Proxy ListCertificates)

responseRejectSharedDirectory :: RejectSharedDirectoryResponse -> TestTree
responseRejectSharedDirectory =
  res
    "RejectSharedDirectoryResponse"
    "fixture/RejectSharedDirectoryResponse.proto"
    directoryService
    (Proxy :: Proxy RejectSharedDirectory)

responseUnshareDirectory :: UnshareDirectoryResponse -> TestTree
responseUnshareDirectory =
  res
    "UnshareDirectoryResponse"
    "fixture/UnshareDirectoryResponse.proto"
    directoryService
    (Proxy :: Proxy UnshareDirectory)

responseRestoreFromSnapshot :: RestoreFromSnapshotResponse -> TestTree
responseRestoreFromSnapshot =
  res
    "RestoreFromSnapshotResponse"
    "fixture/RestoreFromSnapshotResponse.proto"
    directoryService
    (Proxy :: Proxy RestoreFromSnapshot)

responseDescribeDomainControllers :: DescribeDomainControllersResponse -> TestTree
responseDescribeDomainControllers =
  res
    "DescribeDomainControllersResponse"
    "fixture/DescribeDomainControllersResponse.proto"
    directoryService
    (Proxy :: Proxy DescribeDomainControllers)

responseDescribeSnapshots :: DescribeSnapshotsResponse -> TestTree
responseDescribeSnapshots =
  res
    "DescribeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse.proto"
    directoryService
    (Proxy :: Proxy DescribeSnapshots)

responseRemoveIPRoutes :: RemoveIPRoutesResponse -> TestTree
responseRemoveIPRoutes =
  res
    "RemoveIPRoutesResponse"
    "fixture/RemoveIPRoutesResponse.proto"
    directoryService
    (Proxy :: Proxy RemoveIPRoutes)

responseDeleteSnapshot :: DeleteSnapshotResponse -> TestTree
responseDeleteSnapshot =
  res
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse.proto"
    directoryService
    (Proxy :: Proxy DeleteSnapshot)

responseDeregisterCertificate :: DeregisterCertificateResponse -> TestTree
responseDeregisterCertificate =
  res
    "DeregisterCertificateResponse"
    "fixture/DeregisterCertificateResponse.proto"
    directoryService
    (Proxy :: Proxy DeregisterCertificate)

responseStartSchemaExtension :: StartSchemaExtensionResponse -> TestTree
responseStartSchemaExtension =
  res
    "StartSchemaExtensionResponse"
    "fixture/StartSchemaExtensionResponse.proto"
    directoryService
    (Proxy :: Proxy StartSchemaExtension)

responseCreateTrust :: CreateTrustResponse -> TestTree
responseCreateTrust =
  res
    "CreateTrustResponse"
    "fixture/CreateTrustResponse.proto"
    directoryService
    (Proxy :: Proxy CreateTrust)

responseDeleteDirectory :: DeleteDirectoryResponse -> TestTree
responseDeleteDirectory =
  res
    "DeleteDirectoryResponse"
    "fixture/DeleteDirectoryResponse.proto"
    directoryService
    (Proxy :: Proxy DeleteDirectory)

responseCreateSnapshot :: CreateSnapshotResponse -> TestTree
responseCreateSnapshot =
  res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    directoryService
    (Proxy :: Proxy CreateSnapshot)

responseDescribeCertificate :: DescribeCertificateResponse -> TestTree
responseDescribeCertificate =
  res
    "DescribeCertificateResponse"
    "fixture/DescribeCertificateResponse.proto"
    directoryService
    (Proxy :: Proxy DescribeCertificate)

responseCreateComputer :: CreateComputerResponse -> TestTree
responseCreateComputer =
  res
    "CreateComputerResponse"
    "fixture/CreateComputerResponse.proto"
    directoryService
    (Proxy :: Proxy CreateComputer)

responseDescribeSharedDirectories :: DescribeSharedDirectoriesResponse -> TestTree
responseDescribeSharedDirectories =
  res
    "DescribeSharedDirectoriesResponse"
    "fixture/DescribeSharedDirectoriesResponse.proto"
    directoryService
    (Proxy :: Proxy DescribeSharedDirectories)

responseEnableLDAPS :: EnableLDAPSResponse -> TestTree
responseEnableLDAPS =
  res
    "EnableLDAPSResponse"
    "fixture/EnableLDAPSResponse.proto"
    directoryService
    (Proxy :: Proxy EnableLDAPS)

responseDisableSSO :: DisableSSOResponse -> TestTree
responseDisableSSO =
  res
    "DisableSSOResponse"
    "fixture/DisableSSOResponse.proto"
    directoryService
    (Proxy :: Proxy DisableSSO)

responseVerifyTrust :: VerifyTrustResponse -> TestTree
responseVerifyTrust =
  res
    "VerifyTrustResponse"
    "fixture/VerifyTrustResponse.proto"
    directoryService
    (Proxy :: Proxy VerifyTrust)

responseRemoveRegion :: RemoveRegionResponse -> TestTree
responseRemoveRegion =
  res
    "RemoveRegionResponse"
    "fixture/RemoveRegionResponse.proto"
    directoryService
    (Proxy :: Proxy RemoveRegion)

responseCreateConditionalForwarder :: CreateConditionalForwarderResponse -> TestTree
responseCreateConditionalForwarder =
  res
    "CreateConditionalForwarderResponse"
    "fixture/CreateConditionalForwarderResponse.proto"
    directoryService
    (Proxy :: Proxy CreateConditionalForwarder)

responseDescribeRegions :: DescribeRegionsResponse -> TestTree
responseDescribeRegions =
  res
    "DescribeRegionsResponse"
    "fixture/DescribeRegionsResponse.proto"
    directoryService
    (Proxy :: Proxy DescribeRegions)

responseAddRegion :: AddRegionResponse -> TestTree
responseAddRegion =
  res
    "AddRegionResponse"
    "fixture/AddRegionResponse.proto"
    directoryService
    (Proxy :: Proxy AddRegion)

responseGetDirectoryLimits :: GetDirectoryLimitsResponse -> TestTree
responseGetDirectoryLimits =
  res
    "GetDirectoryLimitsResponse"
    "fixture/GetDirectoryLimitsResponse.proto"
    directoryService
    (Proxy :: Proxy GetDirectoryLimits)

responseUpdateRadius :: UpdateRadiusResponse -> TestTree
responseUpdateRadius =
  res
    "UpdateRadiusResponse"
    "fixture/UpdateRadiusResponse.proto"
    directoryService
    (Proxy :: Proxy UpdateRadius)

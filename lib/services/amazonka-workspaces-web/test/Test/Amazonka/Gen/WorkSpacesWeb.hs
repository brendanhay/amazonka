{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.WorkSpacesWeb
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.WorkSpacesWeb where

import Amazonka.WorkSpacesWeb
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.WorkSpacesWeb.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateBrowserSettings $
--             newAssociateBrowserSettings
--
--         , requestAssociateIpAccessSettings $
--             newAssociateIpAccessSettings
--
--         , requestAssociateNetworkSettings $
--             newAssociateNetworkSettings
--
--         , requestAssociateTrustStore $
--             newAssociateTrustStore
--
--         , requestAssociateUserAccessLoggingSettings $
--             newAssociateUserAccessLoggingSettings
--
--         , requestAssociateUserSettings $
--             newAssociateUserSettings
--
--         , requestCreateBrowserSettings $
--             newCreateBrowserSettings
--
--         , requestCreateIdentityProvider $
--             newCreateIdentityProvider
--
--         , requestCreateIpAccessSettings $
--             newCreateIpAccessSettings
--
--         , requestCreateNetworkSettings $
--             newCreateNetworkSettings
--
--         , requestCreatePortal $
--             newCreatePortal
--
--         , requestCreateTrustStore $
--             newCreateTrustStore
--
--         , requestCreateUserAccessLoggingSettings $
--             newCreateUserAccessLoggingSettings
--
--         , requestCreateUserSettings $
--             newCreateUserSettings
--
--         , requestDeleteBrowserSettings $
--             newDeleteBrowserSettings
--
--         , requestDeleteIdentityProvider $
--             newDeleteIdentityProvider
--
--         , requestDeleteIpAccessSettings $
--             newDeleteIpAccessSettings
--
--         , requestDeleteNetworkSettings $
--             newDeleteNetworkSettings
--
--         , requestDeletePortal $
--             newDeletePortal
--
--         , requestDeleteTrustStore $
--             newDeleteTrustStore
--
--         , requestDeleteUserAccessLoggingSettings $
--             newDeleteUserAccessLoggingSettings
--
--         , requestDeleteUserSettings $
--             newDeleteUserSettings
--
--         , requestDisassociateBrowserSettings $
--             newDisassociateBrowserSettings
--
--         , requestDisassociateIpAccessSettings $
--             newDisassociateIpAccessSettings
--
--         , requestDisassociateNetworkSettings $
--             newDisassociateNetworkSettings
--
--         , requestDisassociateTrustStore $
--             newDisassociateTrustStore
--
--         , requestDisassociateUserAccessLoggingSettings $
--             newDisassociateUserAccessLoggingSettings
--
--         , requestDisassociateUserSettings $
--             newDisassociateUserSettings
--
--         , requestGetBrowserSettings $
--             newGetBrowserSettings
--
--         , requestGetIdentityProvider $
--             newGetIdentityProvider
--
--         , requestGetIpAccessSettings $
--             newGetIpAccessSettings
--
--         , requestGetNetworkSettings $
--             newGetNetworkSettings
--
--         , requestGetPortal $
--             newGetPortal
--
--         , requestGetPortalServiceProviderMetadata $
--             newGetPortalServiceProviderMetadata
--
--         , requestGetTrustStore $
--             newGetTrustStore
--
--         , requestGetTrustStoreCertificate $
--             newGetTrustStoreCertificate
--
--         , requestGetUserAccessLoggingSettings $
--             newGetUserAccessLoggingSettings
--
--         , requestGetUserSettings $
--             newGetUserSettings
--
--         , requestListBrowserSettings $
--             newListBrowserSettings
--
--         , requestListIdentityProviders $
--             newListIdentityProviders
--
--         , requestListIpAccessSettings $
--             newListIpAccessSettings
--
--         , requestListNetworkSettings $
--             newListNetworkSettings
--
--         , requestListPortals $
--             newListPortals
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListTrustStoreCertificates $
--             newListTrustStoreCertificates
--
--         , requestListTrustStores $
--             newListTrustStores
--
--         , requestListUserAccessLoggingSettings $
--             newListUserAccessLoggingSettings
--
--         , requestListUserSettings $
--             newListUserSettings
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateBrowserSettings $
--             newUpdateBrowserSettings
--
--         , requestUpdateIdentityProvider $
--             newUpdateIdentityProvider
--
--         , requestUpdateIpAccessSettings $
--             newUpdateIpAccessSettings
--
--         , requestUpdateNetworkSettings $
--             newUpdateNetworkSettings
--
--         , requestUpdatePortal $
--             newUpdatePortal
--
--         , requestUpdateTrustStore $
--             newUpdateTrustStore
--
--         , requestUpdateUserAccessLoggingSettings $
--             newUpdateUserAccessLoggingSettings
--
--         , requestUpdateUserSettings $
--             newUpdateUserSettings
--
--           ]

--     , testGroup "response"
--         [ responseAssociateBrowserSettings $
--             newAssociateBrowserSettingsResponse
--
--         , responseAssociateIpAccessSettings $
--             newAssociateIpAccessSettingsResponse
--
--         , responseAssociateNetworkSettings $
--             newAssociateNetworkSettingsResponse
--
--         , responseAssociateTrustStore $
--             newAssociateTrustStoreResponse
--
--         , responseAssociateUserAccessLoggingSettings $
--             newAssociateUserAccessLoggingSettingsResponse
--
--         , responseAssociateUserSettings $
--             newAssociateUserSettingsResponse
--
--         , responseCreateBrowserSettings $
--             newCreateBrowserSettingsResponse
--
--         , responseCreateIdentityProvider $
--             newCreateIdentityProviderResponse
--
--         , responseCreateIpAccessSettings $
--             newCreateIpAccessSettingsResponse
--
--         , responseCreateNetworkSettings $
--             newCreateNetworkSettingsResponse
--
--         , responseCreatePortal $
--             newCreatePortalResponse
--
--         , responseCreateTrustStore $
--             newCreateTrustStoreResponse
--
--         , responseCreateUserAccessLoggingSettings $
--             newCreateUserAccessLoggingSettingsResponse
--
--         , responseCreateUserSettings $
--             newCreateUserSettingsResponse
--
--         , responseDeleteBrowserSettings $
--             newDeleteBrowserSettingsResponse
--
--         , responseDeleteIdentityProvider $
--             newDeleteIdentityProviderResponse
--
--         , responseDeleteIpAccessSettings $
--             newDeleteIpAccessSettingsResponse
--
--         , responseDeleteNetworkSettings $
--             newDeleteNetworkSettingsResponse
--
--         , responseDeletePortal $
--             newDeletePortalResponse
--
--         , responseDeleteTrustStore $
--             newDeleteTrustStoreResponse
--
--         , responseDeleteUserAccessLoggingSettings $
--             newDeleteUserAccessLoggingSettingsResponse
--
--         , responseDeleteUserSettings $
--             newDeleteUserSettingsResponse
--
--         , responseDisassociateBrowserSettings $
--             newDisassociateBrowserSettingsResponse
--
--         , responseDisassociateIpAccessSettings $
--             newDisassociateIpAccessSettingsResponse
--
--         , responseDisassociateNetworkSettings $
--             newDisassociateNetworkSettingsResponse
--
--         , responseDisassociateTrustStore $
--             newDisassociateTrustStoreResponse
--
--         , responseDisassociateUserAccessLoggingSettings $
--             newDisassociateUserAccessLoggingSettingsResponse
--
--         , responseDisassociateUserSettings $
--             newDisassociateUserSettingsResponse
--
--         , responseGetBrowserSettings $
--             newGetBrowserSettingsResponse
--
--         , responseGetIdentityProvider $
--             newGetIdentityProviderResponse
--
--         , responseGetIpAccessSettings $
--             newGetIpAccessSettingsResponse
--
--         , responseGetNetworkSettings $
--             newGetNetworkSettingsResponse
--
--         , responseGetPortal $
--             newGetPortalResponse
--
--         , responseGetPortalServiceProviderMetadata $
--             newGetPortalServiceProviderMetadataResponse
--
--         , responseGetTrustStore $
--             newGetTrustStoreResponse
--
--         , responseGetTrustStoreCertificate $
--             newGetTrustStoreCertificateResponse
--
--         , responseGetUserAccessLoggingSettings $
--             newGetUserAccessLoggingSettingsResponse
--
--         , responseGetUserSettings $
--             newGetUserSettingsResponse
--
--         , responseListBrowserSettings $
--             newListBrowserSettingsResponse
--
--         , responseListIdentityProviders $
--             newListIdentityProvidersResponse
--
--         , responseListIpAccessSettings $
--             newListIpAccessSettingsResponse
--
--         , responseListNetworkSettings $
--             newListNetworkSettingsResponse
--
--         , responseListPortals $
--             newListPortalsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListTrustStoreCertificates $
--             newListTrustStoreCertificatesResponse
--
--         , responseListTrustStores $
--             newListTrustStoresResponse
--
--         , responseListUserAccessLoggingSettings $
--             newListUserAccessLoggingSettingsResponse
--
--         , responseListUserSettings $
--             newListUserSettingsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateBrowserSettings $
--             newUpdateBrowserSettingsResponse
--
--         , responseUpdateIdentityProvider $
--             newUpdateIdentityProviderResponse
--
--         , responseUpdateIpAccessSettings $
--             newUpdateIpAccessSettingsResponse
--
--         , responseUpdateNetworkSettings $
--             newUpdateNetworkSettingsResponse
--
--         , responseUpdatePortal $
--             newUpdatePortalResponse
--
--         , responseUpdateTrustStore $
--             newUpdateTrustStoreResponse
--
--         , responseUpdateUserAccessLoggingSettings $
--             newUpdateUserAccessLoggingSettingsResponse
--
--         , responseUpdateUserSettings $
--             newUpdateUserSettingsResponse
--
--           ]
--     ]

-- Requests

requestAssociateBrowserSettings :: AssociateBrowserSettings -> TestTree
requestAssociateBrowserSettings =
  req
    "AssociateBrowserSettings"
    "fixture/AssociateBrowserSettings.yaml"

requestAssociateIpAccessSettings :: AssociateIpAccessSettings -> TestTree
requestAssociateIpAccessSettings =
  req
    "AssociateIpAccessSettings"
    "fixture/AssociateIpAccessSettings.yaml"

requestAssociateNetworkSettings :: AssociateNetworkSettings -> TestTree
requestAssociateNetworkSettings =
  req
    "AssociateNetworkSettings"
    "fixture/AssociateNetworkSettings.yaml"

requestAssociateTrustStore :: AssociateTrustStore -> TestTree
requestAssociateTrustStore =
  req
    "AssociateTrustStore"
    "fixture/AssociateTrustStore.yaml"

requestAssociateUserAccessLoggingSettings :: AssociateUserAccessLoggingSettings -> TestTree
requestAssociateUserAccessLoggingSettings =
  req
    "AssociateUserAccessLoggingSettings"
    "fixture/AssociateUserAccessLoggingSettings.yaml"

requestAssociateUserSettings :: AssociateUserSettings -> TestTree
requestAssociateUserSettings =
  req
    "AssociateUserSettings"
    "fixture/AssociateUserSettings.yaml"

requestCreateBrowserSettings :: CreateBrowserSettings -> TestTree
requestCreateBrowserSettings =
  req
    "CreateBrowserSettings"
    "fixture/CreateBrowserSettings.yaml"

requestCreateIdentityProvider :: CreateIdentityProvider -> TestTree
requestCreateIdentityProvider =
  req
    "CreateIdentityProvider"
    "fixture/CreateIdentityProvider.yaml"

requestCreateIpAccessSettings :: CreateIpAccessSettings -> TestTree
requestCreateIpAccessSettings =
  req
    "CreateIpAccessSettings"
    "fixture/CreateIpAccessSettings.yaml"

requestCreateNetworkSettings :: CreateNetworkSettings -> TestTree
requestCreateNetworkSettings =
  req
    "CreateNetworkSettings"
    "fixture/CreateNetworkSettings.yaml"

requestCreatePortal :: CreatePortal -> TestTree
requestCreatePortal =
  req
    "CreatePortal"
    "fixture/CreatePortal.yaml"

requestCreateTrustStore :: CreateTrustStore -> TestTree
requestCreateTrustStore =
  req
    "CreateTrustStore"
    "fixture/CreateTrustStore.yaml"

requestCreateUserAccessLoggingSettings :: CreateUserAccessLoggingSettings -> TestTree
requestCreateUserAccessLoggingSettings =
  req
    "CreateUserAccessLoggingSettings"
    "fixture/CreateUserAccessLoggingSettings.yaml"

requestCreateUserSettings :: CreateUserSettings -> TestTree
requestCreateUserSettings =
  req
    "CreateUserSettings"
    "fixture/CreateUserSettings.yaml"

requestDeleteBrowserSettings :: DeleteBrowserSettings -> TestTree
requestDeleteBrowserSettings =
  req
    "DeleteBrowserSettings"
    "fixture/DeleteBrowserSettings.yaml"

requestDeleteIdentityProvider :: DeleteIdentityProvider -> TestTree
requestDeleteIdentityProvider =
  req
    "DeleteIdentityProvider"
    "fixture/DeleteIdentityProvider.yaml"

requestDeleteIpAccessSettings :: DeleteIpAccessSettings -> TestTree
requestDeleteIpAccessSettings =
  req
    "DeleteIpAccessSettings"
    "fixture/DeleteIpAccessSettings.yaml"

requestDeleteNetworkSettings :: DeleteNetworkSettings -> TestTree
requestDeleteNetworkSettings =
  req
    "DeleteNetworkSettings"
    "fixture/DeleteNetworkSettings.yaml"

requestDeletePortal :: DeletePortal -> TestTree
requestDeletePortal =
  req
    "DeletePortal"
    "fixture/DeletePortal.yaml"

requestDeleteTrustStore :: DeleteTrustStore -> TestTree
requestDeleteTrustStore =
  req
    "DeleteTrustStore"
    "fixture/DeleteTrustStore.yaml"

requestDeleteUserAccessLoggingSettings :: DeleteUserAccessLoggingSettings -> TestTree
requestDeleteUserAccessLoggingSettings =
  req
    "DeleteUserAccessLoggingSettings"
    "fixture/DeleteUserAccessLoggingSettings.yaml"

requestDeleteUserSettings :: DeleteUserSettings -> TestTree
requestDeleteUserSettings =
  req
    "DeleteUserSettings"
    "fixture/DeleteUserSettings.yaml"

requestDisassociateBrowserSettings :: DisassociateBrowserSettings -> TestTree
requestDisassociateBrowserSettings =
  req
    "DisassociateBrowserSettings"
    "fixture/DisassociateBrowserSettings.yaml"

requestDisassociateIpAccessSettings :: DisassociateIpAccessSettings -> TestTree
requestDisassociateIpAccessSettings =
  req
    "DisassociateIpAccessSettings"
    "fixture/DisassociateIpAccessSettings.yaml"

requestDisassociateNetworkSettings :: DisassociateNetworkSettings -> TestTree
requestDisassociateNetworkSettings =
  req
    "DisassociateNetworkSettings"
    "fixture/DisassociateNetworkSettings.yaml"

requestDisassociateTrustStore :: DisassociateTrustStore -> TestTree
requestDisassociateTrustStore =
  req
    "DisassociateTrustStore"
    "fixture/DisassociateTrustStore.yaml"

requestDisassociateUserAccessLoggingSettings :: DisassociateUserAccessLoggingSettings -> TestTree
requestDisassociateUserAccessLoggingSettings =
  req
    "DisassociateUserAccessLoggingSettings"
    "fixture/DisassociateUserAccessLoggingSettings.yaml"

requestDisassociateUserSettings :: DisassociateUserSettings -> TestTree
requestDisassociateUserSettings =
  req
    "DisassociateUserSettings"
    "fixture/DisassociateUserSettings.yaml"

requestGetBrowserSettings :: GetBrowserSettings -> TestTree
requestGetBrowserSettings =
  req
    "GetBrowserSettings"
    "fixture/GetBrowserSettings.yaml"

requestGetIdentityProvider :: GetIdentityProvider -> TestTree
requestGetIdentityProvider =
  req
    "GetIdentityProvider"
    "fixture/GetIdentityProvider.yaml"

requestGetIpAccessSettings :: GetIpAccessSettings -> TestTree
requestGetIpAccessSettings =
  req
    "GetIpAccessSettings"
    "fixture/GetIpAccessSettings.yaml"

requestGetNetworkSettings :: GetNetworkSettings -> TestTree
requestGetNetworkSettings =
  req
    "GetNetworkSettings"
    "fixture/GetNetworkSettings.yaml"

requestGetPortal :: GetPortal -> TestTree
requestGetPortal =
  req
    "GetPortal"
    "fixture/GetPortal.yaml"

requestGetPortalServiceProviderMetadata :: GetPortalServiceProviderMetadata -> TestTree
requestGetPortalServiceProviderMetadata =
  req
    "GetPortalServiceProviderMetadata"
    "fixture/GetPortalServiceProviderMetadata.yaml"

requestGetTrustStore :: GetTrustStore -> TestTree
requestGetTrustStore =
  req
    "GetTrustStore"
    "fixture/GetTrustStore.yaml"

requestGetTrustStoreCertificate :: GetTrustStoreCertificate -> TestTree
requestGetTrustStoreCertificate =
  req
    "GetTrustStoreCertificate"
    "fixture/GetTrustStoreCertificate.yaml"

requestGetUserAccessLoggingSettings :: GetUserAccessLoggingSettings -> TestTree
requestGetUserAccessLoggingSettings =
  req
    "GetUserAccessLoggingSettings"
    "fixture/GetUserAccessLoggingSettings.yaml"

requestGetUserSettings :: GetUserSettings -> TestTree
requestGetUserSettings =
  req
    "GetUserSettings"
    "fixture/GetUserSettings.yaml"

requestListBrowserSettings :: ListBrowserSettings -> TestTree
requestListBrowserSettings =
  req
    "ListBrowserSettings"
    "fixture/ListBrowserSettings.yaml"

requestListIdentityProviders :: ListIdentityProviders -> TestTree
requestListIdentityProviders =
  req
    "ListIdentityProviders"
    "fixture/ListIdentityProviders.yaml"

requestListIpAccessSettings :: ListIpAccessSettings -> TestTree
requestListIpAccessSettings =
  req
    "ListIpAccessSettings"
    "fixture/ListIpAccessSettings.yaml"

requestListNetworkSettings :: ListNetworkSettings -> TestTree
requestListNetworkSettings =
  req
    "ListNetworkSettings"
    "fixture/ListNetworkSettings.yaml"

requestListPortals :: ListPortals -> TestTree
requestListPortals =
  req
    "ListPortals"
    "fixture/ListPortals.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListTrustStoreCertificates :: ListTrustStoreCertificates -> TestTree
requestListTrustStoreCertificates =
  req
    "ListTrustStoreCertificates"
    "fixture/ListTrustStoreCertificates.yaml"

requestListTrustStores :: ListTrustStores -> TestTree
requestListTrustStores =
  req
    "ListTrustStores"
    "fixture/ListTrustStores.yaml"

requestListUserAccessLoggingSettings :: ListUserAccessLoggingSettings -> TestTree
requestListUserAccessLoggingSettings =
  req
    "ListUserAccessLoggingSettings"
    "fixture/ListUserAccessLoggingSettings.yaml"

requestListUserSettings :: ListUserSettings -> TestTree
requestListUserSettings =
  req
    "ListUserSettings"
    "fixture/ListUserSettings.yaml"

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

requestUpdateBrowserSettings :: UpdateBrowserSettings -> TestTree
requestUpdateBrowserSettings =
  req
    "UpdateBrowserSettings"
    "fixture/UpdateBrowserSettings.yaml"

requestUpdateIdentityProvider :: UpdateIdentityProvider -> TestTree
requestUpdateIdentityProvider =
  req
    "UpdateIdentityProvider"
    "fixture/UpdateIdentityProvider.yaml"

requestUpdateIpAccessSettings :: UpdateIpAccessSettings -> TestTree
requestUpdateIpAccessSettings =
  req
    "UpdateIpAccessSettings"
    "fixture/UpdateIpAccessSettings.yaml"

requestUpdateNetworkSettings :: UpdateNetworkSettings -> TestTree
requestUpdateNetworkSettings =
  req
    "UpdateNetworkSettings"
    "fixture/UpdateNetworkSettings.yaml"

requestUpdatePortal :: UpdatePortal -> TestTree
requestUpdatePortal =
  req
    "UpdatePortal"
    "fixture/UpdatePortal.yaml"

requestUpdateTrustStore :: UpdateTrustStore -> TestTree
requestUpdateTrustStore =
  req
    "UpdateTrustStore"
    "fixture/UpdateTrustStore.yaml"

requestUpdateUserAccessLoggingSettings :: UpdateUserAccessLoggingSettings -> TestTree
requestUpdateUserAccessLoggingSettings =
  req
    "UpdateUserAccessLoggingSettings"
    "fixture/UpdateUserAccessLoggingSettings.yaml"

requestUpdateUserSettings :: UpdateUserSettings -> TestTree
requestUpdateUserSettings =
  req
    "UpdateUserSettings"
    "fixture/UpdateUserSettings.yaml"

-- Responses

responseAssociateBrowserSettings :: AssociateBrowserSettingsResponse -> TestTree
responseAssociateBrowserSettings =
  res
    "AssociateBrowserSettingsResponse"
    "fixture/AssociateBrowserSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateBrowserSettings)

responseAssociateIpAccessSettings :: AssociateIpAccessSettingsResponse -> TestTree
responseAssociateIpAccessSettings =
  res
    "AssociateIpAccessSettingsResponse"
    "fixture/AssociateIpAccessSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateIpAccessSettings)

responseAssociateNetworkSettings :: AssociateNetworkSettingsResponse -> TestTree
responseAssociateNetworkSettings =
  res
    "AssociateNetworkSettingsResponse"
    "fixture/AssociateNetworkSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateNetworkSettings)

responseAssociateTrustStore :: AssociateTrustStoreResponse -> TestTree
responseAssociateTrustStore =
  res
    "AssociateTrustStoreResponse"
    "fixture/AssociateTrustStoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateTrustStore)

responseAssociateUserAccessLoggingSettings :: AssociateUserAccessLoggingSettingsResponse -> TestTree
responseAssociateUserAccessLoggingSettings =
  res
    "AssociateUserAccessLoggingSettingsResponse"
    "fixture/AssociateUserAccessLoggingSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateUserAccessLoggingSettings)

responseAssociateUserSettings :: AssociateUserSettingsResponse -> TestTree
responseAssociateUserSettings =
  res
    "AssociateUserSettingsResponse"
    "fixture/AssociateUserSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateUserSettings)

responseCreateBrowserSettings :: CreateBrowserSettingsResponse -> TestTree
responseCreateBrowserSettings =
  res
    "CreateBrowserSettingsResponse"
    "fixture/CreateBrowserSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBrowserSettings)

responseCreateIdentityProvider :: CreateIdentityProviderResponse -> TestTree
responseCreateIdentityProvider =
  res
    "CreateIdentityProviderResponse"
    "fixture/CreateIdentityProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateIdentityProvider)

responseCreateIpAccessSettings :: CreateIpAccessSettingsResponse -> TestTree
responseCreateIpAccessSettings =
  res
    "CreateIpAccessSettingsResponse"
    "fixture/CreateIpAccessSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateIpAccessSettings)

responseCreateNetworkSettings :: CreateNetworkSettingsResponse -> TestTree
responseCreateNetworkSettings =
  res
    "CreateNetworkSettingsResponse"
    "fixture/CreateNetworkSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNetworkSettings)

responseCreatePortal :: CreatePortalResponse -> TestTree
responseCreatePortal =
  res
    "CreatePortalResponse"
    "fixture/CreatePortalResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePortal)

responseCreateTrustStore :: CreateTrustStoreResponse -> TestTree
responseCreateTrustStore =
  res
    "CreateTrustStoreResponse"
    "fixture/CreateTrustStoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTrustStore)

responseCreateUserAccessLoggingSettings :: CreateUserAccessLoggingSettingsResponse -> TestTree
responseCreateUserAccessLoggingSettings =
  res
    "CreateUserAccessLoggingSettingsResponse"
    "fixture/CreateUserAccessLoggingSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUserAccessLoggingSettings)

responseCreateUserSettings :: CreateUserSettingsResponse -> TestTree
responseCreateUserSettings =
  res
    "CreateUserSettingsResponse"
    "fixture/CreateUserSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUserSettings)

responseDeleteBrowserSettings :: DeleteBrowserSettingsResponse -> TestTree
responseDeleteBrowserSettings =
  res
    "DeleteBrowserSettingsResponse"
    "fixture/DeleteBrowserSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBrowserSettings)

responseDeleteIdentityProvider :: DeleteIdentityProviderResponse -> TestTree
responseDeleteIdentityProvider =
  res
    "DeleteIdentityProviderResponse"
    "fixture/DeleteIdentityProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIdentityProvider)

responseDeleteIpAccessSettings :: DeleteIpAccessSettingsResponse -> TestTree
responseDeleteIpAccessSettings =
  res
    "DeleteIpAccessSettingsResponse"
    "fixture/DeleteIpAccessSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIpAccessSettings)

responseDeleteNetworkSettings :: DeleteNetworkSettingsResponse -> TestTree
responseDeleteNetworkSettings =
  res
    "DeleteNetworkSettingsResponse"
    "fixture/DeleteNetworkSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNetworkSettings)

responseDeletePortal :: DeletePortalResponse -> TestTree
responseDeletePortal =
  res
    "DeletePortalResponse"
    "fixture/DeletePortalResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePortal)

responseDeleteTrustStore :: DeleteTrustStoreResponse -> TestTree
responseDeleteTrustStore =
  res
    "DeleteTrustStoreResponse"
    "fixture/DeleteTrustStoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTrustStore)

responseDeleteUserAccessLoggingSettings :: DeleteUserAccessLoggingSettingsResponse -> TestTree
responseDeleteUserAccessLoggingSettings =
  res
    "DeleteUserAccessLoggingSettingsResponse"
    "fixture/DeleteUserAccessLoggingSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUserAccessLoggingSettings)

responseDeleteUserSettings :: DeleteUserSettingsResponse -> TestTree
responseDeleteUserSettings =
  res
    "DeleteUserSettingsResponse"
    "fixture/DeleteUserSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUserSettings)

responseDisassociateBrowserSettings :: DisassociateBrowserSettingsResponse -> TestTree
responseDisassociateBrowserSettings =
  res
    "DisassociateBrowserSettingsResponse"
    "fixture/DisassociateBrowserSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateBrowserSettings)

responseDisassociateIpAccessSettings :: DisassociateIpAccessSettingsResponse -> TestTree
responseDisassociateIpAccessSettings =
  res
    "DisassociateIpAccessSettingsResponse"
    "fixture/DisassociateIpAccessSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateIpAccessSettings)

responseDisassociateNetworkSettings :: DisassociateNetworkSettingsResponse -> TestTree
responseDisassociateNetworkSettings =
  res
    "DisassociateNetworkSettingsResponse"
    "fixture/DisassociateNetworkSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateNetworkSettings)

responseDisassociateTrustStore :: DisassociateTrustStoreResponse -> TestTree
responseDisassociateTrustStore =
  res
    "DisassociateTrustStoreResponse"
    "fixture/DisassociateTrustStoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateTrustStore)

responseDisassociateUserAccessLoggingSettings :: DisassociateUserAccessLoggingSettingsResponse -> TestTree
responseDisassociateUserAccessLoggingSettings =
  res
    "DisassociateUserAccessLoggingSettingsResponse"
    "fixture/DisassociateUserAccessLoggingSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateUserAccessLoggingSettings)

responseDisassociateUserSettings :: DisassociateUserSettingsResponse -> TestTree
responseDisassociateUserSettings =
  res
    "DisassociateUserSettingsResponse"
    "fixture/DisassociateUserSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateUserSettings)

responseGetBrowserSettings :: GetBrowserSettingsResponse -> TestTree
responseGetBrowserSettings =
  res
    "GetBrowserSettingsResponse"
    "fixture/GetBrowserSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBrowserSettings)

responseGetIdentityProvider :: GetIdentityProviderResponse -> TestTree
responseGetIdentityProvider =
  res
    "GetIdentityProviderResponse"
    "fixture/GetIdentityProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIdentityProvider)

responseGetIpAccessSettings :: GetIpAccessSettingsResponse -> TestTree
responseGetIpAccessSettings =
  res
    "GetIpAccessSettingsResponse"
    "fixture/GetIpAccessSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIpAccessSettings)

responseGetNetworkSettings :: GetNetworkSettingsResponse -> TestTree
responseGetNetworkSettings =
  res
    "GetNetworkSettingsResponse"
    "fixture/GetNetworkSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetNetworkSettings)

responseGetPortal :: GetPortalResponse -> TestTree
responseGetPortal =
  res
    "GetPortalResponse"
    "fixture/GetPortalResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPortal)

responseGetPortalServiceProviderMetadata :: GetPortalServiceProviderMetadataResponse -> TestTree
responseGetPortalServiceProviderMetadata =
  res
    "GetPortalServiceProviderMetadataResponse"
    "fixture/GetPortalServiceProviderMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPortalServiceProviderMetadata)

responseGetTrustStore :: GetTrustStoreResponse -> TestTree
responseGetTrustStore =
  res
    "GetTrustStoreResponse"
    "fixture/GetTrustStoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTrustStore)

responseGetTrustStoreCertificate :: GetTrustStoreCertificateResponse -> TestTree
responseGetTrustStoreCertificate =
  res
    "GetTrustStoreCertificateResponse"
    "fixture/GetTrustStoreCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTrustStoreCertificate)

responseGetUserAccessLoggingSettings :: GetUserAccessLoggingSettingsResponse -> TestTree
responseGetUserAccessLoggingSettings =
  res
    "GetUserAccessLoggingSettingsResponse"
    "fixture/GetUserAccessLoggingSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUserAccessLoggingSettings)

responseGetUserSettings :: GetUserSettingsResponse -> TestTree
responseGetUserSettings =
  res
    "GetUserSettingsResponse"
    "fixture/GetUserSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUserSettings)

responseListBrowserSettings :: ListBrowserSettingsResponse -> TestTree
responseListBrowserSettings =
  res
    "ListBrowserSettingsResponse"
    "fixture/ListBrowserSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBrowserSettings)

responseListIdentityProviders :: ListIdentityProvidersResponse -> TestTree
responseListIdentityProviders =
  res
    "ListIdentityProvidersResponse"
    "fixture/ListIdentityProvidersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIdentityProviders)

responseListIpAccessSettings :: ListIpAccessSettingsResponse -> TestTree
responseListIpAccessSettings =
  res
    "ListIpAccessSettingsResponse"
    "fixture/ListIpAccessSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIpAccessSettings)

responseListNetworkSettings :: ListNetworkSettingsResponse -> TestTree
responseListNetworkSettings =
  res
    "ListNetworkSettingsResponse"
    "fixture/ListNetworkSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNetworkSettings)

responseListPortals :: ListPortalsResponse -> TestTree
responseListPortals =
  res
    "ListPortalsResponse"
    "fixture/ListPortalsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPortals)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListTrustStoreCertificates :: ListTrustStoreCertificatesResponse -> TestTree
responseListTrustStoreCertificates =
  res
    "ListTrustStoreCertificatesResponse"
    "fixture/ListTrustStoreCertificatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTrustStoreCertificates)

responseListTrustStores :: ListTrustStoresResponse -> TestTree
responseListTrustStores =
  res
    "ListTrustStoresResponse"
    "fixture/ListTrustStoresResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTrustStores)

responseListUserAccessLoggingSettings :: ListUserAccessLoggingSettingsResponse -> TestTree
responseListUserAccessLoggingSettings =
  res
    "ListUserAccessLoggingSettingsResponse"
    "fixture/ListUserAccessLoggingSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUserAccessLoggingSettings)

responseListUserSettings :: ListUserSettingsResponse -> TestTree
responseListUserSettings =
  res
    "ListUserSettingsResponse"
    "fixture/ListUserSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUserSettings)

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

responseUpdateBrowserSettings :: UpdateBrowserSettingsResponse -> TestTree
responseUpdateBrowserSettings =
  res
    "UpdateBrowserSettingsResponse"
    "fixture/UpdateBrowserSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBrowserSettings)

responseUpdateIdentityProvider :: UpdateIdentityProviderResponse -> TestTree
responseUpdateIdentityProvider =
  res
    "UpdateIdentityProviderResponse"
    "fixture/UpdateIdentityProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateIdentityProvider)

responseUpdateIpAccessSettings :: UpdateIpAccessSettingsResponse -> TestTree
responseUpdateIpAccessSettings =
  res
    "UpdateIpAccessSettingsResponse"
    "fixture/UpdateIpAccessSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateIpAccessSettings)

responseUpdateNetworkSettings :: UpdateNetworkSettingsResponse -> TestTree
responseUpdateNetworkSettings =
  res
    "UpdateNetworkSettingsResponse"
    "fixture/UpdateNetworkSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateNetworkSettings)

responseUpdatePortal :: UpdatePortalResponse -> TestTree
responseUpdatePortal =
  res
    "UpdatePortalResponse"
    "fixture/UpdatePortalResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePortal)

responseUpdateTrustStore :: UpdateTrustStoreResponse -> TestTree
responseUpdateTrustStore =
  res
    "UpdateTrustStoreResponse"
    "fixture/UpdateTrustStoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTrustStore)

responseUpdateUserAccessLoggingSettings :: UpdateUserAccessLoggingSettingsResponse -> TestTree
responseUpdateUserAccessLoggingSettings =
  res
    "UpdateUserAccessLoggingSettingsResponse"
    "fixture/UpdateUserAccessLoggingSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUserAccessLoggingSettings)

responseUpdateUserSettings :: UpdateUserSettingsResponse -> TestTree
responseUpdateUserSettings =
  res
    "UpdateUserSettingsResponse"
    "fixture/UpdateUserSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUserSettings)

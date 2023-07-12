{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.WorkSpacesWeb
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-07-08@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- WorkSpaces Web is a low cost, fully managed WorkSpace built specifically
-- to facilitate secure, web-based workloads. WorkSpaces Web makes it easy
-- for customers to safely provide their employees with access to internal
-- websites and SaaS web applications without the administrative burden of
-- appliances or specialized client software. WorkSpaces Web provides
-- simple policy tools tailored for user interactions, while offloading
-- common tasks like capacity management, scaling, and maintaining browser
-- images.
module Amazonka.WorkSpacesWeb
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateBrowserSettings
    AssociateBrowserSettings (AssociateBrowserSettings'),
    newAssociateBrowserSettings,
    AssociateBrowserSettingsResponse (AssociateBrowserSettingsResponse'),
    newAssociateBrowserSettingsResponse,

    -- ** AssociateNetworkSettings
    AssociateNetworkSettings (AssociateNetworkSettings'),
    newAssociateNetworkSettings,
    AssociateNetworkSettingsResponse (AssociateNetworkSettingsResponse'),
    newAssociateNetworkSettingsResponse,

    -- ** AssociateTrustStore
    AssociateTrustStore (AssociateTrustStore'),
    newAssociateTrustStore,
    AssociateTrustStoreResponse (AssociateTrustStoreResponse'),
    newAssociateTrustStoreResponse,

    -- ** AssociateUserAccessLoggingSettings
    AssociateUserAccessLoggingSettings (AssociateUserAccessLoggingSettings'),
    newAssociateUserAccessLoggingSettings,
    AssociateUserAccessLoggingSettingsResponse (AssociateUserAccessLoggingSettingsResponse'),
    newAssociateUserAccessLoggingSettingsResponse,

    -- ** AssociateUserSettings
    AssociateUserSettings (AssociateUserSettings'),
    newAssociateUserSettings,
    AssociateUserSettingsResponse (AssociateUserSettingsResponse'),
    newAssociateUserSettingsResponse,

    -- ** CreateBrowserSettings
    CreateBrowserSettings (CreateBrowserSettings'),
    newCreateBrowserSettings,
    CreateBrowserSettingsResponse (CreateBrowserSettingsResponse'),
    newCreateBrowserSettingsResponse,

    -- ** CreateIdentityProvider
    CreateIdentityProvider (CreateIdentityProvider'),
    newCreateIdentityProvider,
    CreateIdentityProviderResponse (CreateIdentityProviderResponse'),
    newCreateIdentityProviderResponse,

    -- ** CreateNetworkSettings
    CreateNetworkSettings (CreateNetworkSettings'),
    newCreateNetworkSettings,
    CreateNetworkSettingsResponse (CreateNetworkSettingsResponse'),
    newCreateNetworkSettingsResponse,

    -- ** CreatePortal
    CreatePortal (CreatePortal'),
    newCreatePortal,
    CreatePortalResponse (CreatePortalResponse'),
    newCreatePortalResponse,

    -- ** CreateTrustStore
    CreateTrustStore (CreateTrustStore'),
    newCreateTrustStore,
    CreateTrustStoreResponse (CreateTrustStoreResponse'),
    newCreateTrustStoreResponse,

    -- ** CreateUserAccessLoggingSettings
    CreateUserAccessLoggingSettings (CreateUserAccessLoggingSettings'),
    newCreateUserAccessLoggingSettings,
    CreateUserAccessLoggingSettingsResponse (CreateUserAccessLoggingSettingsResponse'),
    newCreateUserAccessLoggingSettingsResponse,

    -- ** CreateUserSettings
    CreateUserSettings (CreateUserSettings'),
    newCreateUserSettings,
    CreateUserSettingsResponse (CreateUserSettingsResponse'),
    newCreateUserSettingsResponse,

    -- ** DeleteBrowserSettings
    DeleteBrowserSettings (DeleteBrowserSettings'),
    newDeleteBrowserSettings,
    DeleteBrowserSettingsResponse (DeleteBrowserSettingsResponse'),
    newDeleteBrowserSettingsResponse,

    -- ** DeleteIdentityProvider
    DeleteIdentityProvider (DeleteIdentityProvider'),
    newDeleteIdentityProvider,
    DeleteIdentityProviderResponse (DeleteIdentityProviderResponse'),
    newDeleteIdentityProviderResponse,

    -- ** DeleteNetworkSettings
    DeleteNetworkSettings (DeleteNetworkSettings'),
    newDeleteNetworkSettings,
    DeleteNetworkSettingsResponse (DeleteNetworkSettingsResponse'),
    newDeleteNetworkSettingsResponse,

    -- ** DeletePortal
    DeletePortal (DeletePortal'),
    newDeletePortal,
    DeletePortalResponse (DeletePortalResponse'),
    newDeletePortalResponse,

    -- ** DeleteTrustStore
    DeleteTrustStore (DeleteTrustStore'),
    newDeleteTrustStore,
    DeleteTrustStoreResponse (DeleteTrustStoreResponse'),
    newDeleteTrustStoreResponse,

    -- ** DeleteUserAccessLoggingSettings
    DeleteUserAccessLoggingSettings (DeleteUserAccessLoggingSettings'),
    newDeleteUserAccessLoggingSettings,
    DeleteUserAccessLoggingSettingsResponse (DeleteUserAccessLoggingSettingsResponse'),
    newDeleteUserAccessLoggingSettingsResponse,

    -- ** DeleteUserSettings
    DeleteUserSettings (DeleteUserSettings'),
    newDeleteUserSettings,
    DeleteUserSettingsResponse (DeleteUserSettingsResponse'),
    newDeleteUserSettingsResponse,

    -- ** DisassociateBrowserSettings
    DisassociateBrowserSettings (DisassociateBrowserSettings'),
    newDisassociateBrowserSettings,
    DisassociateBrowserSettingsResponse (DisassociateBrowserSettingsResponse'),
    newDisassociateBrowserSettingsResponse,

    -- ** DisassociateNetworkSettings
    DisassociateNetworkSettings (DisassociateNetworkSettings'),
    newDisassociateNetworkSettings,
    DisassociateNetworkSettingsResponse (DisassociateNetworkSettingsResponse'),
    newDisassociateNetworkSettingsResponse,

    -- ** DisassociateTrustStore
    DisassociateTrustStore (DisassociateTrustStore'),
    newDisassociateTrustStore,
    DisassociateTrustStoreResponse (DisassociateTrustStoreResponse'),
    newDisassociateTrustStoreResponse,

    -- ** DisassociateUserAccessLoggingSettings
    DisassociateUserAccessLoggingSettings (DisassociateUserAccessLoggingSettings'),
    newDisassociateUserAccessLoggingSettings,
    DisassociateUserAccessLoggingSettingsResponse (DisassociateUserAccessLoggingSettingsResponse'),
    newDisassociateUserAccessLoggingSettingsResponse,

    -- ** DisassociateUserSettings
    DisassociateUserSettings (DisassociateUserSettings'),
    newDisassociateUserSettings,
    DisassociateUserSettingsResponse (DisassociateUserSettingsResponse'),
    newDisassociateUserSettingsResponse,

    -- ** GetBrowserSettings
    GetBrowserSettings (GetBrowserSettings'),
    newGetBrowserSettings,
    GetBrowserSettingsResponse (GetBrowserSettingsResponse'),
    newGetBrowserSettingsResponse,

    -- ** GetIdentityProvider
    GetIdentityProvider (GetIdentityProvider'),
    newGetIdentityProvider,
    GetIdentityProviderResponse (GetIdentityProviderResponse'),
    newGetIdentityProviderResponse,

    -- ** GetNetworkSettings
    GetNetworkSettings (GetNetworkSettings'),
    newGetNetworkSettings,
    GetNetworkSettingsResponse (GetNetworkSettingsResponse'),
    newGetNetworkSettingsResponse,

    -- ** GetPortal
    GetPortal (GetPortal'),
    newGetPortal,
    GetPortalResponse (GetPortalResponse'),
    newGetPortalResponse,

    -- ** GetPortalServiceProviderMetadata
    GetPortalServiceProviderMetadata (GetPortalServiceProviderMetadata'),
    newGetPortalServiceProviderMetadata,
    GetPortalServiceProviderMetadataResponse (GetPortalServiceProviderMetadataResponse'),
    newGetPortalServiceProviderMetadataResponse,

    -- ** GetTrustStore
    GetTrustStore (GetTrustStore'),
    newGetTrustStore,
    GetTrustStoreResponse (GetTrustStoreResponse'),
    newGetTrustStoreResponse,

    -- ** GetTrustStoreCertificate
    GetTrustStoreCertificate (GetTrustStoreCertificate'),
    newGetTrustStoreCertificate,
    GetTrustStoreCertificateResponse (GetTrustStoreCertificateResponse'),
    newGetTrustStoreCertificateResponse,

    -- ** GetUserAccessLoggingSettings
    GetUserAccessLoggingSettings (GetUserAccessLoggingSettings'),
    newGetUserAccessLoggingSettings,
    GetUserAccessLoggingSettingsResponse (GetUserAccessLoggingSettingsResponse'),
    newGetUserAccessLoggingSettingsResponse,

    -- ** GetUserSettings
    GetUserSettings (GetUserSettings'),
    newGetUserSettings,
    GetUserSettingsResponse (GetUserSettingsResponse'),
    newGetUserSettingsResponse,

    -- ** ListBrowserSettings
    ListBrowserSettings (ListBrowserSettings'),
    newListBrowserSettings,
    ListBrowserSettingsResponse (ListBrowserSettingsResponse'),
    newListBrowserSettingsResponse,

    -- ** ListIdentityProviders
    ListIdentityProviders (ListIdentityProviders'),
    newListIdentityProviders,
    ListIdentityProvidersResponse (ListIdentityProvidersResponse'),
    newListIdentityProvidersResponse,

    -- ** ListNetworkSettings
    ListNetworkSettings (ListNetworkSettings'),
    newListNetworkSettings,
    ListNetworkSettingsResponse (ListNetworkSettingsResponse'),
    newListNetworkSettingsResponse,

    -- ** ListPortals
    ListPortals (ListPortals'),
    newListPortals,
    ListPortalsResponse (ListPortalsResponse'),
    newListPortalsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListTrustStoreCertificates
    ListTrustStoreCertificates (ListTrustStoreCertificates'),
    newListTrustStoreCertificates,
    ListTrustStoreCertificatesResponse (ListTrustStoreCertificatesResponse'),
    newListTrustStoreCertificatesResponse,

    -- ** ListTrustStores
    ListTrustStores (ListTrustStores'),
    newListTrustStores,
    ListTrustStoresResponse (ListTrustStoresResponse'),
    newListTrustStoresResponse,

    -- ** ListUserAccessLoggingSettings
    ListUserAccessLoggingSettings (ListUserAccessLoggingSettings'),
    newListUserAccessLoggingSettings,
    ListUserAccessLoggingSettingsResponse (ListUserAccessLoggingSettingsResponse'),
    newListUserAccessLoggingSettingsResponse,

    -- ** ListUserSettings
    ListUserSettings (ListUserSettings'),
    newListUserSettings,
    ListUserSettingsResponse (ListUserSettingsResponse'),
    newListUserSettingsResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateBrowserSettings
    UpdateBrowserSettings (UpdateBrowserSettings'),
    newUpdateBrowserSettings,
    UpdateBrowserSettingsResponse (UpdateBrowserSettingsResponse'),
    newUpdateBrowserSettingsResponse,

    -- ** UpdateIdentityProvider
    UpdateIdentityProvider (UpdateIdentityProvider'),
    newUpdateIdentityProvider,
    UpdateIdentityProviderResponse (UpdateIdentityProviderResponse'),
    newUpdateIdentityProviderResponse,

    -- ** UpdateNetworkSettings
    UpdateNetworkSettings (UpdateNetworkSettings'),
    newUpdateNetworkSettings,
    UpdateNetworkSettingsResponse (UpdateNetworkSettingsResponse'),
    newUpdateNetworkSettingsResponse,

    -- ** UpdatePortal
    UpdatePortal (UpdatePortal'),
    newUpdatePortal,
    UpdatePortalResponse (UpdatePortalResponse'),
    newUpdatePortalResponse,

    -- ** UpdateTrustStore
    UpdateTrustStore (UpdateTrustStore'),
    newUpdateTrustStore,
    UpdateTrustStoreResponse (UpdateTrustStoreResponse'),
    newUpdateTrustStoreResponse,

    -- ** UpdateUserAccessLoggingSettings
    UpdateUserAccessLoggingSettings (UpdateUserAccessLoggingSettings'),
    newUpdateUserAccessLoggingSettings,
    UpdateUserAccessLoggingSettingsResponse (UpdateUserAccessLoggingSettingsResponse'),
    newUpdateUserAccessLoggingSettingsResponse,

    -- ** UpdateUserSettings
    UpdateUserSettings (UpdateUserSettings'),
    newUpdateUserSettings,
    UpdateUserSettingsResponse (UpdateUserSettingsResponse'),
    newUpdateUserSettingsResponse,

    -- * Types

    -- ** BrowserType
    BrowserType (..),

    -- ** EnabledType
    EnabledType (..),

    -- ** IdentityProviderType
    IdentityProviderType (..),

    -- ** PortalStatus
    PortalStatus (..),

    -- ** RendererType
    RendererType (..),

    -- ** BrowserSettings
    BrowserSettings (BrowserSettings'),
    newBrowserSettings,

    -- ** BrowserSettingsSummary
    BrowserSettingsSummary (BrowserSettingsSummary'),
    newBrowserSettingsSummary,

    -- ** Certificate
    Certificate (Certificate'),
    newCertificate,

    -- ** CertificateSummary
    CertificateSummary (CertificateSummary'),
    newCertificateSummary,

    -- ** IdentityProvider
    IdentityProvider (IdentityProvider'),
    newIdentityProvider,

    -- ** IdentityProviderSummary
    IdentityProviderSummary (IdentityProviderSummary'),
    newIdentityProviderSummary,

    -- ** NetworkSettings
    NetworkSettings (NetworkSettings'),
    newNetworkSettings,

    -- ** NetworkSettingsSummary
    NetworkSettingsSummary (NetworkSettingsSummary'),
    newNetworkSettingsSummary,

    -- ** Portal
    Portal (Portal'),
    newPortal,

    -- ** PortalSummary
    PortalSummary (PortalSummary'),
    newPortalSummary,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TrustStore
    TrustStore (TrustStore'),
    newTrustStore,

    -- ** TrustStoreSummary
    TrustStoreSummary (TrustStoreSummary'),
    newTrustStoreSummary,

    -- ** UserAccessLoggingSettings
    UserAccessLoggingSettings (UserAccessLoggingSettings'),
    newUserAccessLoggingSettings,

    -- ** UserAccessLoggingSettingsSummary
    UserAccessLoggingSettingsSummary (UserAccessLoggingSettingsSummary'),
    newUserAccessLoggingSettingsSummary,

    -- ** UserSettings
    UserSettings (UserSettings'),
    newUserSettings,

    -- ** UserSettingsSummary
    UserSettingsSummary (UserSettingsSummary'),
    newUserSettingsSummary,
  )
where

import Amazonka.WorkSpacesWeb.AssociateBrowserSettings
import Amazonka.WorkSpacesWeb.AssociateNetworkSettings
import Amazonka.WorkSpacesWeb.AssociateTrustStore
import Amazonka.WorkSpacesWeb.AssociateUserAccessLoggingSettings
import Amazonka.WorkSpacesWeb.AssociateUserSettings
import Amazonka.WorkSpacesWeb.CreateBrowserSettings
import Amazonka.WorkSpacesWeb.CreateIdentityProvider
import Amazonka.WorkSpacesWeb.CreateNetworkSettings
import Amazonka.WorkSpacesWeb.CreatePortal
import Amazonka.WorkSpacesWeb.CreateTrustStore
import Amazonka.WorkSpacesWeb.CreateUserAccessLoggingSettings
import Amazonka.WorkSpacesWeb.CreateUserSettings
import Amazonka.WorkSpacesWeb.DeleteBrowserSettings
import Amazonka.WorkSpacesWeb.DeleteIdentityProvider
import Amazonka.WorkSpacesWeb.DeleteNetworkSettings
import Amazonka.WorkSpacesWeb.DeletePortal
import Amazonka.WorkSpacesWeb.DeleteTrustStore
import Amazonka.WorkSpacesWeb.DeleteUserAccessLoggingSettings
import Amazonka.WorkSpacesWeb.DeleteUserSettings
import Amazonka.WorkSpacesWeb.DisassociateBrowserSettings
import Amazonka.WorkSpacesWeb.DisassociateNetworkSettings
import Amazonka.WorkSpacesWeb.DisassociateTrustStore
import Amazonka.WorkSpacesWeb.DisassociateUserAccessLoggingSettings
import Amazonka.WorkSpacesWeb.DisassociateUserSettings
import Amazonka.WorkSpacesWeb.GetBrowserSettings
import Amazonka.WorkSpacesWeb.GetIdentityProvider
import Amazonka.WorkSpacesWeb.GetNetworkSettings
import Amazonka.WorkSpacesWeb.GetPortal
import Amazonka.WorkSpacesWeb.GetPortalServiceProviderMetadata
import Amazonka.WorkSpacesWeb.GetTrustStore
import Amazonka.WorkSpacesWeb.GetTrustStoreCertificate
import Amazonka.WorkSpacesWeb.GetUserAccessLoggingSettings
import Amazonka.WorkSpacesWeb.GetUserSettings
import Amazonka.WorkSpacesWeb.Lens
import Amazonka.WorkSpacesWeb.ListBrowserSettings
import Amazonka.WorkSpacesWeb.ListIdentityProviders
import Amazonka.WorkSpacesWeb.ListNetworkSettings
import Amazonka.WorkSpacesWeb.ListPortals
import Amazonka.WorkSpacesWeb.ListTagsForResource
import Amazonka.WorkSpacesWeb.ListTrustStoreCertificates
import Amazonka.WorkSpacesWeb.ListTrustStores
import Amazonka.WorkSpacesWeb.ListUserAccessLoggingSettings
import Amazonka.WorkSpacesWeb.ListUserSettings
import Amazonka.WorkSpacesWeb.TagResource
import Amazonka.WorkSpacesWeb.Types
import Amazonka.WorkSpacesWeb.UntagResource
import Amazonka.WorkSpacesWeb.UpdateBrowserSettings
import Amazonka.WorkSpacesWeb.UpdateIdentityProvider
import Amazonka.WorkSpacesWeb.UpdateNetworkSettings
import Amazonka.WorkSpacesWeb.UpdatePortal
import Amazonka.WorkSpacesWeb.UpdateTrustStore
import Amazonka.WorkSpacesWeb.UpdateUserAccessLoggingSettings
import Amazonka.WorkSpacesWeb.UpdateUserSettings
import Amazonka.WorkSpacesWeb.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'WorkSpacesWeb'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.

{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WorkSpacesWeb.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpacesWeb.Lens
  ( -- * Operations

    -- ** AssociateBrowserSettings
    associateBrowserSettings_browserSettingsArn,
    associateBrowserSettings_portalArn,
    associateBrowserSettingsResponse_httpStatus,
    associateBrowserSettingsResponse_browserSettingsArn,
    associateBrowserSettingsResponse_portalArn,

    -- ** AssociateNetworkSettings
    associateNetworkSettings_networkSettingsArn,
    associateNetworkSettings_portalArn,
    associateNetworkSettingsResponse_httpStatus,
    associateNetworkSettingsResponse_networkSettingsArn,
    associateNetworkSettingsResponse_portalArn,

    -- ** AssociateTrustStore
    associateTrustStore_portalArn,
    associateTrustStore_trustStoreArn,
    associateTrustStoreResponse_httpStatus,
    associateTrustStoreResponse_portalArn,
    associateTrustStoreResponse_trustStoreArn,

    -- ** AssociateUserAccessLoggingSettings
    associateUserAccessLoggingSettings_portalArn,
    associateUserAccessLoggingSettings_userAccessLoggingSettingsArn,
    associateUserAccessLoggingSettingsResponse_httpStatus,
    associateUserAccessLoggingSettingsResponse_portalArn,
    associateUserAccessLoggingSettingsResponse_userAccessLoggingSettingsArn,

    -- ** AssociateUserSettings
    associateUserSettings_portalArn,
    associateUserSettings_userSettingsArn,
    associateUserSettingsResponse_httpStatus,
    associateUserSettingsResponse_portalArn,
    associateUserSettingsResponse_userSettingsArn,

    -- ** CreateBrowserSettings
    createBrowserSettings_additionalEncryptionContext,
    createBrowserSettings_clientToken,
    createBrowserSettings_customerManagedKey,
    createBrowserSettings_tags,
    createBrowserSettings_browserPolicy,
    createBrowserSettingsResponse_httpStatus,
    createBrowserSettingsResponse_browserSettingsArn,

    -- ** CreateIdentityProvider
    createIdentityProvider_clientToken,
    createIdentityProvider_identityProviderDetails,
    createIdentityProvider_identityProviderName,
    createIdentityProvider_identityProviderType,
    createIdentityProvider_portalArn,
    createIdentityProviderResponse_httpStatus,
    createIdentityProviderResponse_identityProviderArn,

    -- ** CreateNetworkSettings
    createNetworkSettings_clientToken,
    createNetworkSettings_tags,
    createNetworkSettings_securityGroupIds,
    createNetworkSettings_subnetIds,
    createNetworkSettings_vpcId,
    createNetworkSettingsResponse_httpStatus,
    createNetworkSettingsResponse_networkSettingsArn,

    -- ** CreatePortal
    createPortal_additionalEncryptionContext,
    createPortal_clientToken,
    createPortal_customerManagedKey,
    createPortal_displayName,
    createPortal_tags,
    createPortalResponse_httpStatus,
    createPortalResponse_portalArn,
    createPortalResponse_portalEndpoint,

    -- ** CreateTrustStore
    createTrustStore_clientToken,
    createTrustStore_tags,
    createTrustStore_certificateList,
    createTrustStoreResponse_httpStatus,
    createTrustStoreResponse_trustStoreArn,

    -- ** CreateUserAccessLoggingSettings
    createUserAccessLoggingSettings_clientToken,
    createUserAccessLoggingSettings_tags,
    createUserAccessLoggingSettings_kinesisStreamArn,
    createUserAccessLoggingSettingsResponse_httpStatus,
    createUserAccessLoggingSettingsResponse_userAccessLoggingSettingsArn,

    -- ** CreateUserSettings
    createUserSettings_clientToken,
    createUserSettings_disconnectTimeoutInMinutes,
    createUserSettings_idleDisconnectTimeoutInMinutes,
    createUserSettings_tags,
    createUserSettings_copyAllowed,
    createUserSettings_downloadAllowed,
    createUserSettings_pasteAllowed,
    createUserSettings_printAllowed,
    createUserSettings_uploadAllowed,
    createUserSettingsResponse_httpStatus,
    createUserSettingsResponse_userSettingsArn,

    -- ** DeleteBrowserSettings
    deleteBrowserSettings_browserSettingsArn,
    deleteBrowserSettingsResponse_httpStatus,

    -- ** DeleteIdentityProvider
    deleteIdentityProvider_identityProviderArn,
    deleteIdentityProviderResponse_httpStatus,

    -- ** DeleteNetworkSettings
    deleteNetworkSettings_networkSettingsArn,
    deleteNetworkSettingsResponse_httpStatus,

    -- ** DeletePortal
    deletePortal_portalArn,
    deletePortalResponse_httpStatus,

    -- ** DeleteTrustStore
    deleteTrustStore_trustStoreArn,
    deleteTrustStoreResponse_httpStatus,

    -- ** DeleteUserAccessLoggingSettings
    deleteUserAccessLoggingSettings_userAccessLoggingSettingsArn,
    deleteUserAccessLoggingSettingsResponse_httpStatus,

    -- ** DeleteUserSettings
    deleteUserSettings_userSettingsArn,
    deleteUserSettingsResponse_httpStatus,

    -- ** DisassociateBrowserSettings
    disassociateBrowserSettings_portalArn,
    disassociateBrowserSettingsResponse_httpStatus,

    -- ** DisassociateNetworkSettings
    disassociateNetworkSettings_portalArn,
    disassociateNetworkSettingsResponse_httpStatus,

    -- ** DisassociateTrustStore
    disassociateTrustStore_portalArn,
    disassociateTrustStoreResponse_httpStatus,

    -- ** DisassociateUserAccessLoggingSettings
    disassociateUserAccessLoggingSettings_portalArn,
    disassociateUserAccessLoggingSettingsResponse_httpStatus,

    -- ** DisassociateUserSettings
    disassociateUserSettings_portalArn,
    disassociateUserSettingsResponse_httpStatus,

    -- ** GetBrowserSettings
    getBrowserSettings_browserSettingsArn,
    getBrowserSettingsResponse_browserSettings,
    getBrowserSettingsResponse_httpStatus,

    -- ** GetIdentityProvider
    getIdentityProvider_identityProviderArn,
    getIdentityProviderResponse_identityProvider,
    getIdentityProviderResponse_httpStatus,

    -- ** GetNetworkSettings
    getNetworkSettings_networkSettingsArn,
    getNetworkSettingsResponse_networkSettings,
    getNetworkSettingsResponse_httpStatus,

    -- ** GetPortal
    getPortal_portalArn,
    getPortalResponse_portal,
    getPortalResponse_httpStatus,

    -- ** GetPortalServiceProviderMetadata
    getPortalServiceProviderMetadata_portalArn,
    getPortalServiceProviderMetadataResponse_serviceProviderSamlMetadata,
    getPortalServiceProviderMetadataResponse_httpStatus,
    getPortalServiceProviderMetadataResponse_portalArn,

    -- ** GetTrustStore
    getTrustStore_trustStoreArn,
    getTrustStoreResponse_trustStore,
    getTrustStoreResponse_httpStatus,

    -- ** GetTrustStoreCertificate
    getTrustStoreCertificate_thumbprint,
    getTrustStoreCertificate_trustStoreArn,
    getTrustStoreCertificateResponse_certificate,
    getTrustStoreCertificateResponse_trustStoreArn,
    getTrustStoreCertificateResponse_httpStatus,

    -- ** GetUserAccessLoggingSettings
    getUserAccessLoggingSettings_userAccessLoggingSettingsArn,
    getUserAccessLoggingSettingsResponse_userAccessLoggingSettings,
    getUserAccessLoggingSettingsResponse_httpStatus,

    -- ** GetUserSettings
    getUserSettings_userSettingsArn,
    getUserSettingsResponse_userSettings,
    getUserSettingsResponse_httpStatus,

    -- ** ListBrowserSettings
    listBrowserSettings_maxResults,
    listBrowserSettings_nextToken,
    listBrowserSettingsResponse_browserSettings,
    listBrowserSettingsResponse_nextToken,
    listBrowserSettingsResponse_httpStatus,

    -- ** ListIdentityProviders
    listIdentityProviders_maxResults,
    listIdentityProviders_nextToken,
    listIdentityProviders_portalArn,
    listIdentityProvidersResponse_identityProviders,
    listIdentityProvidersResponse_nextToken,
    listIdentityProvidersResponse_httpStatus,

    -- ** ListNetworkSettings
    listNetworkSettings_maxResults,
    listNetworkSettings_nextToken,
    listNetworkSettingsResponse_networkSettings,
    listNetworkSettingsResponse_nextToken,
    listNetworkSettingsResponse_httpStatus,

    -- ** ListPortals
    listPortals_maxResults,
    listPortals_nextToken,
    listPortalsResponse_nextToken,
    listPortalsResponse_portals,
    listPortalsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTrustStoreCertificates
    listTrustStoreCertificates_maxResults,
    listTrustStoreCertificates_nextToken,
    listTrustStoreCertificates_trustStoreArn,
    listTrustStoreCertificatesResponse_certificateList,
    listTrustStoreCertificatesResponse_nextToken,
    listTrustStoreCertificatesResponse_trustStoreArn,
    listTrustStoreCertificatesResponse_httpStatus,

    -- ** ListTrustStores
    listTrustStores_maxResults,
    listTrustStores_nextToken,
    listTrustStoresResponse_nextToken,
    listTrustStoresResponse_trustStores,
    listTrustStoresResponse_httpStatus,

    -- ** ListUserAccessLoggingSettings
    listUserAccessLoggingSettings_maxResults,
    listUserAccessLoggingSettings_nextToken,
    listUserAccessLoggingSettingsResponse_nextToken,
    listUserAccessLoggingSettingsResponse_userAccessLoggingSettings,
    listUserAccessLoggingSettingsResponse_httpStatus,

    -- ** ListUserSettings
    listUserSettings_maxResults,
    listUserSettings_nextToken,
    listUserSettingsResponse_nextToken,
    listUserSettingsResponse_userSettings,
    listUserSettingsResponse_httpStatus,

    -- ** TagResource
    tagResource_clientToken,
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateBrowserSettings
    updateBrowserSettings_browserPolicy,
    updateBrowserSettings_clientToken,
    updateBrowserSettings_browserSettingsArn,
    updateBrowserSettingsResponse_httpStatus,
    updateBrowserSettingsResponse_browserSettings,

    -- ** UpdateIdentityProvider
    updateIdentityProvider_clientToken,
    updateIdentityProvider_identityProviderDetails,
    updateIdentityProvider_identityProviderName,
    updateIdentityProvider_identityProviderType,
    updateIdentityProvider_identityProviderArn,
    updateIdentityProviderResponse_httpStatus,
    updateIdentityProviderResponse_identityProvider,

    -- ** UpdateNetworkSettings
    updateNetworkSettings_clientToken,
    updateNetworkSettings_securityGroupIds,
    updateNetworkSettings_subnetIds,
    updateNetworkSettings_vpcId,
    updateNetworkSettings_networkSettingsArn,
    updateNetworkSettingsResponse_httpStatus,
    updateNetworkSettingsResponse_networkSettings,

    -- ** UpdatePortal
    updatePortal_displayName,
    updatePortal_portalArn,
    updatePortalResponse_portal,
    updatePortalResponse_httpStatus,

    -- ** UpdateTrustStore
    updateTrustStore_certificatesToAdd,
    updateTrustStore_certificatesToDelete,
    updateTrustStore_clientToken,
    updateTrustStore_trustStoreArn,
    updateTrustStoreResponse_httpStatus,
    updateTrustStoreResponse_trustStoreArn,

    -- ** UpdateUserAccessLoggingSettings
    updateUserAccessLoggingSettings_clientToken,
    updateUserAccessLoggingSettings_kinesisStreamArn,
    updateUserAccessLoggingSettings_userAccessLoggingSettingsArn,
    updateUserAccessLoggingSettingsResponse_httpStatus,
    updateUserAccessLoggingSettingsResponse_userAccessLoggingSettings,

    -- ** UpdateUserSettings
    updateUserSettings_clientToken,
    updateUserSettings_copyAllowed,
    updateUserSettings_disconnectTimeoutInMinutes,
    updateUserSettings_downloadAllowed,
    updateUserSettings_idleDisconnectTimeoutInMinutes,
    updateUserSettings_pasteAllowed,
    updateUserSettings_printAllowed,
    updateUserSettings_uploadAllowed,
    updateUserSettings_userSettingsArn,
    updateUserSettingsResponse_httpStatus,
    updateUserSettingsResponse_userSettings,

    -- * Types

    -- ** BrowserSettings
    browserSettings_associatedPortalArns,
    browserSettings_browserPolicy,
    browserSettings_browserSettingsArn,

    -- ** BrowserSettingsSummary
    browserSettingsSummary_browserSettingsArn,

    -- ** Certificate
    certificate_body,
    certificate_issuer,
    certificate_notValidAfter,
    certificate_notValidBefore,
    certificate_subject,
    certificate_thumbprint,

    -- ** CertificateSummary
    certificateSummary_issuer,
    certificateSummary_notValidAfter,
    certificateSummary_notValidBefore,
    certificateSummary_subject,
    certificateSummary_thumbprint,

    -- ** IdentityProvider
    identityProvider_identityProviderDetails,
    identityProvider_identityProviderName,
    identityProvider_identityProviderType,
    identityProvider_identityProviderArn,

    -- ** IdentityProviderSummary
    identityProviderSummary_identityProviderArn,
    identityProviderSummary_identityProviderName,
    identityProviderSummary_identityProviderType,

    -- ** NetworkSettings
    networkSettings_associatedPortalArns,
    networkSettings_securityGroupIds,
    networkSettings_subnetIds,
    networkSettings_vpcId,
    networkSettings_networkSettingsArn,

    -- ** NetworkSettingsSummary
    networkSettingsSummary_networkSettingsArn,
    networkSettingsSummary_vpcId,

    -- ** Portal
    portal_browserSettingsArn,
    portal_browserType,
    portal_creationDate,
    portal_displayName,
    portal_networkSettingsArn,
    portal_portalArn,
    portal_portalEndpoint,
    portal_portalStatus,
    portal_rendererType,
    portal_statusReason,
    portal_trustStoreArn,
    portal_userAccessLoggingSettingsArn,
    portal_userSettingsArn,

    -- ** PortalSummary
    portalSummary_browserSettingsArn,
    portalSummary_browserType,
    portalSummary_creationDate,
    portalSummary_displayName,
    portalSummary_networkSettingsArn,
    portalSummary_portalArn,
    portalSummary_portalEndpoint,
    portalSummary_portalStatus,
    portalSummary_rendererType,
    portalSummary_trustStoreArn,
    portalSummary_userAccessLoggingSettingsArn,
    portalSummary_userSettingsArn,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TrustStore
    trustStore_associatedPortalArns,
    trustStore_trustStoreArn,

    -- ** TrustStoreSummary
    trustStoreSummary_trustStoreArn,

    -- ** UserAccessLoggingSettings
    userAccessLoggingSettings_associatedPortalArns,
    userAccessLoggingSettings_kinesisStreamArn,
    userAccessLoggingSettings_userAccessLoggingSettingsArn,

    -- ** UserAccessLoggingSettingsSummary
    userAccessLoggingSettingsSummary_kinesisStreamArn,
    userAccessLoggingSettingsSummary_userAccessLoggingSettingsArn,

    -- ** UserSettings
    userSettings_associatedPortalArns,
    userSettings_copyAllowed,
    userSettings_disconnectTimeoutInMinutes,
    userSettings_downloadAllowed,
    userSettings_idleDisconnectTimeoutInMinutes,
    userSettings_pasteAllowed,
    userSettings_printAllowed,
    userSettings_uploadAllowed,
    userSettings_userSettingsArn,

    -- ** UserSettingsSummary
    userSettingsSummary_copyAllowed,
    userSettingsSummary_disconnectTimeoutInMinutes,
    userSettingsSummary_downloadAllowed,
    userSettingsSummary_idleDisconnectTimeoutInMinutes,
    userSettingsSummary_pasteAllowed,
    userSettingsSummary_printAllowed,
    userSettingsSummary_uploadAllowed,
    userSettingsSummary_userSettingsArn,
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
import Amazonka.WorkSpacesWeb.Types.BrowserSettings
import Amazonka.WorkSpacesWeb.Types.BrowserSettingsSummary
import Amazonka.WorkSpacesWeb.Types.Certificate
import Amazonka.WorkSpacesWeb.Types.CertificateSummary
import Amazonka.WorkSpacesWeb.Types.IdentityProvider
import Amazonka.WorkSpacesWeb.Types.IdentityProviderSummary
import Amazonka.WorkSpacesWeb.Types.NetworkSettings
import Amazonka.WorkSpacesWeb.Types.NetworkSettingsSummary
import Amazonka.WorkSpacesWeb.Types.Portal
import Amazonka.WorkSpacesWeb.Types.PortalSummary
import Amazonka.WorkSpacesWeb.Types.Tag
import Amazonka.WorkSpacesWeb.Types.TrustStore
import Amazonka.WorkSpacesWeb.Types.TrustStoreSummary
import Amazonka.WorkSpacesWeb.Types.UserAccessLoggingSettings
import Amazonka.WorkSpacesWeb.Types.UserAccessLoggingSettingsSummary
import Amazonka.WorkSpacesWeb.Types.UserSettings
import Amazonka.WorkSpacesWeb.Types.UserSettingsSummary
import Amazonka.WorkSpacesWeb.UntagResource
import Amazonka.WorkSpacesWeb.UpdateBrowserSettings
import Amazonka.WorkSpacesWeb.UpdateIdentityProvider
import Amazonka.WorkSpacesWeb.UpdateNetworkSettings
import Amazonka.WorkSpacesWeb.UpdatePortal
import Amazonka.WorkSpacesWeb.UpdateTrustStore
import Amazonka.WorkSpacesWeb.UpdateUserAccessLoggingSettings
import Amazonka.WorkSpacesWeb.UpdateUserSettings

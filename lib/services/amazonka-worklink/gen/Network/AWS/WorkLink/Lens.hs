{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkLink.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkLink.Lens
  ( -- * Operations

    -- ** UpdateDomainMetadata
    updateDomainMetadata_displayName,
    updateDomainMetadata_fleetArn,
    updateDomainMetadata_domainName,
    updateDomainMetadataResponse_httpStatus,

    -- ** SignOutUser
    signOutUser_fleetArn,
    signOutUser_username,
    signOutUserResponse_httpStatus,

    -- ** RestoreDomainAccess
    restoreDomainAccess_fleetArn,
    restoreDomainAccess_domainName,
    restoreDomainAccessResponse_httpStatus,

    -- ** AssociateDomain
    associateDomain_displayName,
    associateDomain_fleetArn,
    associateDomain_domainName,
    associateDomain_acmCertificateArn,
    associateDomainResponse_httpStatus,

    -- ** DeleteFleet
    deleteFleet_fleetArn,
    deleteFleetResponse_httpStatus,

    -- ** DisassociateDomain
    disassociateDomain_fleetArn,
    disassociateDomain_domainName,
    disassociateDomainResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** AssociateWebsiteAuthorizationProvider
    associateWebsiteAuthorizationProvider_domainName,
    associateWebsiteAuthorizationProvider_fleetArn,
    associateWebsiteAuthorizationProvider_authorizationProviderType,
    associateWebsiteAuthorizationProviderResponse_authorizationProviderId,
    associateWebsiteAuthorizationProviderResponse_httpStatus,

    -- ** DescribeDevicePolicyConfiguration
    describeDevicePolicyConfiguration_fleetArn,
    describeDevicePolicyConfigurationResponse_deviceCaCertificate,
    describeDevicePolicyConfigurationResponse_httpStatus,

    -- ** ListWebsiteAuthorizationProviders
    listWebsiteAuthorizationProviders_nextToken,
    listWebsiteAuthorizationProviders_maxResults,
    listWebsiteAuthorizationProviders_fleetArn,
    listWebsiteAuthorizationProvidersResponse_websiteAuthorizationProviders,
    listWebsiteAuthorizationProvidersResponse_nextToken,
    listWebsiteAuthorizationProvidersResponse_httpStatus,

    -- ** DisassociateWebsiteAuthorizationProvider
    disassociateWebsiteAuthorizationProvider_fleetArn,
    disassociateWebsiteAuthorizationProvider_authorizationProviderId,
    disassociateWebsiteAuthorizationProviderResponse_httpStatus,

    -- ** ListWebsiteCertificateAuthorities
    listWebsiteCertificateAuthorities_nextToken,
    listWebsiteCertificateAuthorities_maxResults,
    listWebsiteCertificateAuthorities_fleetArn,
    listWebsiteCertificateAuthoritiesResponse_websiteCertificateAuthorities,
    listWebsiteCertificateAuthoritiesResponse_nextToken,
    listWebsiteCertificateAuthoritiesResponse_httpStatus,

    -- ** UpdateIdentityProviderConfiguration
    updateIdentityProviderConfiguration_identityProviderSamlMetadata,
    updateIdentityProviderConfiguration_fleetArn,
    updateIdentityProviderConfiguration_identityProviderType,
    updateIdentityProviderConfigurationResponse_httpStatus,

    -- ** DescribeFleetMetadata
    describeFleetMetadata_fleetArn,
    describeFleetMetadataResponse_lastUpdatedTime,
    describeFleetMetadataResponse_fleetStatus,
    describeFleetMetadataResponse_companyCode,
    describeFleetMetadataResponse_createdTime,
    describeFleetMetadataResponse_optimizeForEndUserLocation,
    describeFleetMetadataResponse_displayName,
    describeFleetMetadataResponse_fleetName,
    describeFleetMetadataResponse_tags,
    describeFleetMetadataResponse_httpStatus,

    -- ** UpdateDevicePolicyConfiguration
    updateDevicePolicyConfiguration_deviceCaCertificate,
    updateDevicePolicyConfiguration_fleetArn,
    updateDevicePolicyConfigurationResponse_httpStatus,

    -- ** DescribeCompanyNetworkConfiguration
    describeCompanyNetworkConfiguration_fleetArn,
    describeCompanyNetworkConfigurationResponse_securityGroupIds,
    describeCompanyNetworkConfigurationResponse_subnetIds,
    describeCompanyNetworkConfigurationResponse_vpcId,
    describeCompanyNetworkConfigurationResponse_httpStatus,

    -- ** DescribeAuditStreamConfiguration
    describeAuditStreamConfiguration_fleetArn,
    describeAuditStreamConfigurationResponse_auditStreamArn,
    describeAuditStreamConfigurationResponse_httpStatus,

    -- ** RevokeDomainAccess
    revokeDomainAccess_fleetArn,
    revokeDomainAccess_domainName,
    revokeDomainAccessResponse_httpStatus,

    -- ** ListFleets
    listFleets_nextToken,
    listFleets_maxResults,
    listFleetsResponse_fleetSummaryList,
    listFleetsResponse_nextToken,
    listFleetsResponse_httpStatus,

    -- ** UpdateFleetMetadata
    updateFleetMetadata_optimizeForEndUserLocation,
    updateFleetMetadata_displayName,
    updateFleetMetadata_fleetArn,
    updateFleetMetadataResponse_httpStatus,

    -- ** DescribeIdentityProviderConfiguration
    describeIdentityProviderConfiguration_fleetArn,
    describeIdentityProviderConfigurationResponse_identityProviderType,
    describeIdentityProviderConfigurationResponse_serviceProviderSamlMetadata,
    describeIdentityProviderConfigurationResponse_identityProviderSamlMetadata,
    describeIdentityProviderConfigurationResponse_httpStatus,

    -- ** CreateFleet
    createFleet_optimizeForEndUserLocation,
    createFleet_displayName,
    createFleet_tags,
    createFleet_fleetName,
    createFleetResponse_fleetArn,
    createFleetResponse_httpStatus,

    -- ** DescribeWebsiteCertificateAuthority
    describeWebsiteCertificateAuthority_fleetArn,
    describeWebsiteCertificateAuthority_websiteCaId,
    describeWebsiteCertificateAuthorityResponse_createdTime,
    describeWebsiteCertificateAuthorityResponse_certificate,
    describeWebsiteCertificateAuthorityResponse_displayName,
    describeWebsiteCertificateAuthorityResponse_httpStatus,

    -- ** DescribeDomain
    describeDomain_fleetArn,
    describeDomain_domainName,
    describeDomainResponse_domainStatus,
    describeDomainResponse_acmCertificateArn,
    describeDomainResponse_createdTime,
    describeDomainResponse_domainName,
    describeDomainResponse_displayName,
    describeDomainResponse_httpStatus,

    -- ** DisassociateWebsiteCertificateAuthority
    disassociateWebsiteCertificateAuthority_fleetArn,
    disassociateWebsiteCertificateAuthority_websiteCaId,
    disassociateWebsiteCertificateAuthorityResponse_httpStatus,

    -- ** DescribeDevice
    describeDevice_fleetArn,
    describeDevice_deviceId,
    describeDeviceResponse_status,
    describeDeviceResponse_manufacturer,
    describeDeviceResponse_lastAccessedTime,
    describeDeviceResponse_operatingSystem,
    describeDeviceResponse_username,
    describeDeviceResponse_model,
    describeDeviceResponse_operatingSystemVersion,
    describeDeviceResponse_firstAccessedTime,
    describeDeviceResponse_patchLevel,
    describeDeviceResponse_httpStatus,

    -- ** UpdateAuditStreamConfiguration
    updateAuditStreamConfiguration_auditStreamArn,
    updateAuditStreamConfiguration_fleetArn,
    updateAuditStreamConfigurationResponse_httpStatus,

    -- ** UpdateCompanyNetworkConfiguration
    updateCompanyNetworkConfiguration_fleetArn,
    updateCompanyNetworkConfiguration_vpcId,
    updateCompanyNetworkConfiguration_subnetIds,
    updateCompanyNetworkConfiguration_securityGroupIds,
    updateCompanyNetworkConfigurationResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** AssociateWebsiteCertificateAuthority
    associateWebsiteCertificateAuthority_displayName,
    associateWebsiteCertificateAuthority_fleetArn,
    associateWebsiteCertificateAuthority_certificate,
    associateWebsiteCertificateAuthorityResponse_websiteCaId,
    associateWebsiteCertificateAuthorityResponse_httpStatus,

    -- ** ListDomains
    listDomains_nextToken,
    listDomains_maxResults,
    listDomains_fleetArn,
    listDomainsResponse_nextToken,
    listDomainsResponse_domains,
    listDomainsResponse_httpStatus,

    -- ** ListDevices
    listDevices_nextToken,
    listDevices_maxResults,
    listDevices_fleetArn,
    listDevicesResponse_nextToken,
    listDevicesResponse_devices,
    listDevicesResponse_httpStatus,

    -- * Types

    -- ** DeviceSummary
    deviceSummary_deviceStatus,
    deviceSummary_deviceId,

    -- ** DomainSummary
    domainSummary_displayName,
    domainSummary_domainName,
    domainSummary_createdTime,
    domainSummary_domainStatus,

    -- ** FleetSummary
    fleetSummary_lastUpdatedTime,
    fleetSummary_fleetStatus,
    fleetSummary_companyCode,
    fleetSummary_createdTime,
    fleetSummary_fleetArn,
    fleetSummary_displayName,
    fleetSummary_fleetName,
    fleetSummary_tags,

    -- ** WebsiteAuthorizationProviderSummary
    websiteAuthorizationProviderSummary_authorizationProviderId,
    websiteAuthorizationProviderSummary_createdTime,
    websiteAuthorizationProviderSummary_domainName,
    websiteAuthorizationProviderSummary_authorizationProviderType,

    -- ** WebsiteCaSummary
    websiteCaSummary_createdTime,
    websiteCaSummary_websiteCaId,
    websiteCaSummary_displayName,
  )
where

import Network.AWS.WorkLink.AssociateDomain
import Network.AWS.WorkLink.AssociateWebsiteAuthorizationProvider
import Network.AWS.WorkLink.AssociateWebsiteCertificateAuthority
import Network.AWS.WorkLink.CreateFleet
import Network.AWS.WorkLink.DeleteFleet
import Network.AWS.WorkLink.DescribeAuditStreamConfiguration
import Network.AWS.WorkLink.DescribeCompanyNetworkConfiguration
import Network.AWS.WorkLink.DescribeDevice
import Network.AWS.WorkLink.DescribeDevicePolicyConfiguration
import Network.AWS.WorkLink.DescribeDomain
import Network.AWS.WorkLink.DescribeFleetMetadata
import Network.AWS.WorkLink.DescribeIdentityProviderConfiguration
import Network.AWS.WorkLink.DescribeWebsiteCertificateAuthority
import Network.AWS.WorkLink.DisassociateDomain
import Network.AWS.WorkLink.DisassociateWebsiteAuthorizationProvider
import Network.AWS.WorkLink.DisassociateWebsiteCertificateAuthority
import Network.AWS.WorkLink.ListDevices
import Network.AWS.WorkLink.ListDomains
import Network.AWS.WorkLink.ListFleets
import Network.AWS.WorkLink.ListTagsForResource
import Network.AWS.WorkLink.ListWebsiteAuthorizationProviders
import Network.AWS.WorkLink.ListWebsiteCertificateAuthorities
import Network.AWS.WorkLink.RestoreDomainAccess
import Network.AWS.WorkLink.RevokeDomainAccess
import Network.AWS.WorkLink.SignOutUser
import Network.AWS.WorkLink.TagResource
import Network.AWS.WorkLink.Types.DeviceSummary
import Network.AWS.WorkLink.Types.DomainSummary
import Network.AWS.WorkLink.Types.FleetSummary
import Network.AWS.WorkLink.Types.WebsiteAuthorizationProviderSummary
import Network.AWS.WorkLink.Types.WebsiteCaSummary
import Network.AWS.WorkLink.UntagResource
import Network.AWS.WorkLink.UpdateAuditStreamConfiguration
import Network.AWS.WorkLink.UpdateCompanyNetworkConfiguration
import Network.AWS.WorkLink.UpdateDevicePolicyConfiguration
import Network.AWS.WorkLink.UpdateDomainMetadata
import Network.AWS.WorkLink.UpdateFleetMetadata
import Network.AWS.WorkLink.UpdateIdentityProviderConfiguration

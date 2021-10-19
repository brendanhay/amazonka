{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Lens
  ( -- * Operations

    -- ** ShareDirectory
    shareDirectory_shareNotes,
    shareDirectory_directoryId,
    shareDirectory_shareTarget,
    shareDirectory_shareMethod,
    shareDirectoryResponse_sharedDirectoryId,
    shareDirectoryResponse_httpStatus,

    -- ** UpdateNumberOfDomainControllers
    updateNumberOfDomainControllers_directoryId,
    updateNumberOfDomainControllers_desiredNumber,
    updateNumberOfDomainControllersResponse_httpStatus,

    -- ** DescribeConditionalForwarders
    describeConditionalForwarders_remoteDomainNames,
    describeConditionalForwarders_directoryId,
    describeConditionalForwardersResponse_conditionalForwarders,
    describeConditionalForwardersResponse_httpStatus,

    -- ** GetSnapshotLimits
    getSnapshotLimits_directoryId,
    getSnapshotLimitsResponse_snapshotLimits,
    getSnapshotLimitsResponse_httpStatus,

    -- ** RegisterEventTopic
    registerEventTopic_directoryId,
    registerEventTopic_topicName,
    registerEventTopicResponse_httpStatus,

    -- ** RegisterCertificate
    registerCertificate_clientCertAuthSettings,
    registerCertificate_type,
    registerCertificate_directoryId,
    registerCertificate_certificateData,
    registerCertificateResponse_certificateId,
    registerCertificateResponse_httpStatus,

    -- ** ConnectDirectory
    connectDirectory_shortName,
    connectDirectory_description,
    connectDirectory_tags,
    connectDirectory_name,
    connectDirectory_password,
    connectDirectory_size,
    connectDirectory_connectSettings,
    connectDirectoryResponse_directoryId,
    connectDirectoryResponse_httpStatus,

    -- ** DescribeLDAPSSettings
    describeLDAPSSettings_nextToken,
    describeLDAPSSettings_limit,
    describeLDAPSSettings_type,
    describeLDAPSSettings_directoryId,
    describeLDAPSSettingsResponse_lDAPSSettingsInfo,
    describeLDAPSSettingsResponse_nextToken,
    describeLDAPSSettingsResponse_httpStatus,

    -- ** CreateAlias
    createAlias_directoryId,
    createAlias_alias,
    createAliasResponse_directoryId,
    createAliasResponse_alias,
    createAliasResponse_httpStatus,

    -- ** DescribeDirectories
    describeDirectories_nextToken,
    describeDirectories_directoryIds,
    describeDirectories_limit,
    describeDirectoriesResponse_directoryDescriptions,
    describeDirectoriesResponse_nextToken,
    describeDirectoriesResponse_httpStatus,

    -- ** AddIpRoutes
    addIpRoutes_updateSecurityGroupForDirectoryControllers,
    addIpRoutes_directoryId,
    addIpRoutes_ipRoutes,
    addIpRoutesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_limit,
    listTagsForResource_resourceId,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** DescribeTrusts
    describeTrusts_directoryId,
    describeTrusts_nextToken,
    describeTrusts_trustIds,
    describeTrusts_limit,
    describeTrustsResponse_nextToken,
    describeTrustsResponse_trusts,
    describeTrustsResponse_httpStatus,

    -- ** DeleteTrust
    deleteTrust_deleteAssociatedConditionalForwarder,
    deleteTrust_trustId,
    deleteTrustResponse_trustId,
    deleteTrustResponse_httpStatus,

    -- ** UpdateTrust
    updateTrust_selectiveAuth,
    updateTrust_trustId,
    updateTrustResponse_requestId,
    updateTrustResponse_trustId,
    updateTrustResponse_httpStatus,

    -- ** CreateMicrosoftAD
    createMicrosoftAD_edition,
    createMicrosoftAD_shortName,
    createMicrosoftAD_description,
    createMicrosoftAD_tags,
    createMicrosoftAD_name,
    createMicrosoftAD_password,
    createMicrosoftAD_vpcSettings,
    createMicrosoftADResponse_directoryId,
    createMicrosoftADResponse_httpStatus,

    -- ** DisableClientAuthentication
    disableClientAuthentication_directoryId,
    disableClientAuthentication_type,
    disableClientAuthenticationResponse_httpStatus,

    -- ** DeregisterEventTopic
    deregisterEventTopic_directoryId,
    deregisterEventTopic_topicName,
    deregisterEventTopicResponse_httpStatus,

    -- ** CreateDirectory
    createDirectory_shortName,
    createDirectory_vpcSettings,
    createDirectory_description,
    createDirectory_tags,
    createDirectory_name,
    createDirectory_password,
    createDirectory_size,
    createDirectoryResponse_directoryId,
    createDirectoryResponse_httpStatus,

    -- ** AcceptSharedDirectory
    acceptSharedDirectory_sharedDirectoryId,
    acceptSharedDirectoryResponse_sharedDirectory,
    acceptSharedDirectoryResponse_httpStatus,

    -- ** CreateLogSubscription
    createLogSubscription_directoryId,
    createLogSubscription_logGroupName,
    createLogSubscriptionResponse_httpStatus,

    -- ** RemoveTagsFromResource
    removeTagsFromResource_resourceId,
    removeTagsFromResource_tagKeys,
    removeTagsFromResourceResponse_httpStatus,

    -- ** DescribeEventTopics
    describeEventTopics_directoryId,
    describeEventTopics_topicNames,
    describeEventTopicsResponse_eventTopics,
    describeEventTopicsResponse_httpStatus,

    -- ** ResetUserPassword
    resetUserPassword_directoryId,
    resetUserPassword_userName,
    resetUserPassword_newPassword,
    resetUserPasswordResponse_httpStatus,

    -- ** UpdateConditionalForwarder
    updateConditionalForwarder_directoryId,
    updateConditionalForwarder_remoteDomainName,
    updateConditionalForwarder_dnsIpAddrs,
    updateConditionalForwarderResponse_httpStatus,

    -- ** DeleteConditionalForwarder
    deleteConditionalForwarder_directoryId,
    deleteConditionalForwarder_remoteDomainName,
    deleteConditionalForwarderResponse_httpStatus,

    -- ** DisableLDAPS
    disableLDAPS_directoryId,
    disableLDAPS_type,
    disableLDAPSResponse_httpStatus,

    -- ** DeleteLogSubscription
    deleteLogSubscription_directoryId,
    deleteLogSubscriptionResponse_httpStatus,

    -- ** EnableSso
    enableSso_userName,
    enableSso_password,
    enableSso_directoryId,
    enableSsoResponse_httpStatus,

    -- ** CancelSchemaExtension
    cancelSchemaExtension_directoryId,
    cancelSchemaExtension_schemaExtensionId,
    cancelSchemaExtensionResponse_httpStatus,

    -- ** ListLogSubscriptions
    listLogSubscriptions_directoryId,
    listLogSubscriptions_nextToken,
    listLogSubscriptions_limit,
    listLogSubscriptionsResponse_nextToken,
    listLogSubscriptionsResponse_logSubscriptions,
    listLogSubscriptionsResponse_httpStatus,

    -- ** EnableRadius
    enableRadius_directoryId,
    enableRadius_radiusSettings,
    enableRadiusResponse_httpStatus,

    -- ** ListIpRoutes
    listIpRoutes_nextToken,
    listIpRoutes_limit,
    listIpRoutes_directoryId,
    listIpRoutesResponse_ipRoutesInfo,
    listIpRoutesResponse_nextToken,
    listIpRoutesResponse_httpStatus,

    -- ** AddTagsToResource
    addTagsToResource_resourceId,
    addTagsToResource_tags,
    addTagsToResourceResponse_httpStatus,

    -- ** DescribeClientAuthenticationSettings
    describeClientAuthenticationSettings_nextToken,
    describeClientAuthenticationSettings_limit,
    describeClientAuthenticationSettings_type,
    describeClientAuthenticationSettings_directoryId,
    describeClientAuthenticationSettingsResponse_nextToken,
    describeClientAuthenticationSettingsResponse_clientAuthenticationSettingsInfo,
    describeClientAuthenticationSettingsResponse_httpStatus,

    -- ** ListSchemaExtensions
    listSchemaExtensions_nextToken,
    listSchemaExtensions_limit,
    listSchemaExtensions_directoryId,
    listSchemaExtensionsResponse_schemaExtensionsInfo,
    listSchemaExtensionsResponse_nextToken,
    listSchemaExtensionsResponse_httpStatus,

    -- ** DisableRadius
    disableRadius_directoryId,
    disableRadiusResponse_httpStatus,

    -- ** ListCertificates
    listCertificates_nextToken,
    listCertificates_limit,
    listCertificates_directoryId,
    listCertificatesResponse_nextToken,
    listCertificatesResponse_certificatesInfo,
    listCertificatesResponse_httpStatus,

    -- ** RejectSharedDirectory
    rejectSharedDirectory_sharedDirectoryId,
    rejectSharedDirectoryResponse_sharedDirectoryId,
    rejectSharedDirectoryResponse_httpStatus,

    -- ** UnshareDirectory
    unshareDirectory_directoryId,
    unshareDirectory_unshareTarget,
    unshareDirectoryResponse_sharedDirectoryId,
    unshareDirectoryResponse_httpStatus,

    -- ** RestoreFromSnapshot
    restoreFromSnapshot_snapshotId,
    restoreFromSnapshotResponse_httpStatus,

    -- ** DescribeDomainControllers
    describeDomainControllers_nextToken,
    describeDomainControllers_domainControllerIds,
    describeDomainControllers_limit,
    describeDomainControllers_directoryId,
    describeDomainControllersResponse_nextToken,
    describeDomainControllersResponse_domainControllers,
    describeDomainControllersResponse_httpStatus,

    -- ** DescribeSnapshots
    describeSnapshots_directoryId,
    describeSnapshots_nextToken,
    describeSnapshots_snapshotIds,
    describeSnapshots_limit,
    describeSnapshotsResponse_nextToken,
    describeSnapshotsResponse_snapshots,
    describeSnapshotsResponse_httpStatus,

    -- ** RemoveIpRoutes
    removeIpRoutes_directoryId,
    removeIpRoutes_cidrIps,
    removeIpRoutesResponse_httpStatus,

    -- ** DeleteSnapshot
    deleteSnapshot_snapshotId,
    deleteSnapshotResponse_snapshotId,
    deleteSnapshotResponse_httpStatus,

    -- ** DeregisterCertificate
    deregisterCertificate_directoryId,
    deregisterCertificate_certificateId,
    deregisterCertificateResponse_httpStatus,

    -- ** StartSchemaExtension
    startSchemaExtension_directoryId,
    startSchemaExtension_createSnapshotBeforeSchemaExtension,
    startSchemaExtension_ldifContent,
    startSchemaExtension_description,
    startSchemaExtensionResponse_schemaExtensionId,
    startSchemaExtensionResponse_httpStatus,

    -- ** CreateTrust
    createTrust_conditionalForwarderIpAddrs,
    createTrust_trustType,
    createTrust_selectiveAuth,
    createTrust_directoryId,
    createTrust_remoteDomainName,
    createTrust_trustPassword,
    createTrust_trustDirection,
    createTrustResponse_trustId,
    createTrustResponse_httpStatus,

    -- ** DeleteDirectory
    deleteDirectory_directoryId,
    deleteDirectoryResponse_directoryId,
    deleteDirectoryResponse_httpStatus,

    -- ** CreateSnapshot
    createSnapshot_name,
    createSnapshot_directoryId,
    createSnapshotResponse_snapshotId,
    createSnapshotResponse_httpStatus,

    -- ** DescribeCertificate
    describeCertificate_directoryId,
    describeCertificate_certificateId,
    describeCertificateResponse_certificate,
    describeCertificateResponse_httpStatus,

    -- ** EnableClientAuthentication
    enableClientAuthentication_directoryId,
    enableClientAuthentication_type,
    enableClientAuthenticationResponse_httpStatus,

    -- ** CreateComputer
    createComputer_computerAttributes,
    createComputer_organizationalUnitDistinguishedName,
    createComputer_directoryId,
    createComputer_computerName,
    createComputer_password,
    createComputerResponse_computer,
    createComputerResponse_httpStatus,

    -- ** DescribeSharedDirectories
    describeSharedDirectories_sharedDirectoryIds,
    describeSharedDirectories_nextToken,
    describeSharedDirectories_limit,
    describeSharedDirectories_ownerDirectoryId,
    describeSharedDirectoriesResponse_sharedDirectories,
    describeSharedDirectoriesResponse_nextToken,
    describeSharedDirectoriesResponse_httpStatus,

    -- ** EnableLDAPS
    enableLDAPS_directoryId,
    enableLDAPS_type,
    enableLDAPSResponse_httpStatus,

    -- ** DisableSso
    disableSso_userName,
    disableSso_password,
    disableSso_directoryId,
    disableSsoResponse_httpStatus,

    -- ** VerifyTrust
    verifyTrust_trustId,
    verifyTrustResponse_trustId,
    verifyTrustResponse_httpStatus,

    -- ** RemoveRegion
    removeRegion_directoryId,
    removeRegionResponse_httpStatus,

    -- ** CreateConditionalForwarder
    createConditionalForwarder_directoryId,
    createConditionalForwarder_remoteDomainName,
    createConditionalForwarder_dnsIpAddrs,
    createConditionalForwarderResponse_httpStatus,

    -- ** DescribeRegions
    describeRegions_regionName,
    describeRegions_nextToken,
    describeRegions_directoryId,
    describeRegionsResponse_nextToken,
    describeRegionsResponse_regionsDescription,
    describeRegionsResponse_httpStatus,

    -- ** AddRegion
    addRegion_directoryId,
    addRegion_regionName,
    addRegion_vPCSettings,
    addRegionResponse_httpStatus,

    -- ** GetDirectoryLimits
    getDirectoryLimitsResponse_directoryLimits,
    getDirectoryLimitsResponse_httpStatus,

    -- ** UpdateRadius
    updateRadius_directoryId,
    updateRadius_radiusSettings,
    updateRadiusResponse_httpStatus,

    -- * Types

    -- ** Attribute
    attribute_value,
    attribute_name,

    -- ** Certificate
    certificate_clientCertAuthSettings,
    certificate_state,
    certificate_commonName,
    certificate_certificateId,
    certificate_expiryDateTime,
    certificate_registeredDateTime,
    certificate_type,
    certificate_stateReason,

    -- ** CertificateInfo
    certificateInfo_state,
    certificateInfo_commonName,
    certificateInfo_certificateId,
    certificateInfo_expiryDateTime,
    certificateInfo_type,

    -- ** ClientAuthenticationSettingInfo
    clientAuthenticationSettingInfo_status,
    clientAuthenticationSettingInfo_lastUpdatedDateTime,
    clientAuthenticationSettingInfo_type,

    -- ** ClientCertAuthSettings
    clientCertAuthSettings_oCSPUrl,

    -- ** Computer
    computer_computerId,
    computer_computerAttributes,
    computer_computerName,

    -- ** ConditionalForwarder
    conditionalForwarder_dnsIpAddrs,
    conditionalForwarder_remoteDomainName,
    conditionalForwarder_replicationScope,

    -- ** DirectoryConnectSettings
    directoryConnectSettings_vpcId,
    directoryConnectSettings_subnetIds,
    directoryConnectSettings_customerDnsIps,
    directoryConnectSettings_customerUserName,

    -- ** DirectoryConnectSettingsDescription
    directoryConnectSettingsDescription_customerUserName,
    directoryConnectSettingsDescription_subnetIds,
    directoryConnectSettingsDescription_vpcId,
    directoryConnectSettingsDescription_securityGroupId,
    directoryConnectSettingsDescription_connectIps,
    directoryConnectSettingsDescription_availabilityZones,

    -- ** DirectoryDescription
    directoryDescription_edition,
    directoryDescription_radiusStatus,
    directoryDescription_stage,
    directoryDescription_directoryId,
    directoryDescription_accessUrl,
    directoryDescription_shortName,
    directoryDescription_regionsInfo,
    directoryDescription_size,
    directoryDescription_desiredNumberOfDomainControllers,
    directoryDescription_radiusSettings,
    directoryDescription_launchTime,
    directoryDescription_alias,
    directoryDescription_shareStatus,
    directoryDescription_name,
    directoryDescription_shareMethod,
    directoryDescription_stageLastUpdatedDateTime,
    directoryDescription_ssoEnabled,
    directoryDescription_dnsIpAddrs,
    directoryDescription_vpcSettings,
    directoryDescription_type,
    directoryDescription_stageReason,
    directoryDescription_connectSettings,
    directoryDescription_ownerDirectoryDescription,
    directoryDescription_description,
    directoryDescription_shareNotes,

    -- ** DirectoryLimits
    directoryLimits_connectedDirectoriesCurrentCount,
    directoryLimits_cloudOnlyMicrosoftADLimitReached,
    directoryLimits_connectedDirectoriesLimit,
    directoryLimits_connectedDirectoriesLimitReached,
    directoryLimits_cloudOnlyMicrosoftADLimit,
    directoryLimits_cloudOnlyDirectoriesLimit,
    directoryLimits_cloudOnlyDirectoriesCurrentCount,
    directoryLimits_cloudOnlyDirectoriesLimitReached,
    directoryLimits_cloudOnlyMicrosoftADCurrentCount,

    -- ** DirectoryVpcSettings
    directoryVpcSettings_vpcId,
    directoryVpcSettings_subnetIds,

    -- ** DirectoryVpcSettingsDescription
    directoryVpcSettingsDescription_subnetIds,
    directoryVpcSettingsDescription_vpcId,
    directoryVpcSettingsDescription_securityGroupId,
    directoryVpcSettingsDescription_availabilityZones,

    -- ** DomainController
    domainController_status,
    domainController_directoryId,
    domainController_vpcId,
    domainController_launchTime,
    domainController_subnetId,
    domainController_availabilityZone,
    domainController_statusLastUpdatedDateTime,
    domainController_statusReason,
    domainController_dnsIpAddr,
    domainController_domainControllerId,

    -- ** EventTopic
    eventTopic_status,
    eventTopic_directoryId,
    eventTopic_topicName,
    eventTopic_topicArn,
    eventTopic_createdDateTime,

    -- ** IpRoute
    ipRoute_cidrIp,
    ipRoute_description,

    -- ** IpRouteInfo
    ipRouteInfo_directoryId,
    ipRouteInfo_ipRouteStatusReason,
    ipRouteInfo_addedDateTime,
    ipRouteInfo_cidrIp,
    ipRouteInfo_ipRouteStatusMsg,
    ipRouteInfo_description,

    -- ** LDAPSSettingInfo
    lDAPSSettingInfo_lastUpdatedDateTime,
    lDAPSSettingInfo_lDAPSStatusReason,
    lDAPSSettingInfo_lDAPSStatus,

    -- ** LogSubscription
    logSubscription_directoryId,
    logSubscription_logGroupName,
    logSubscription_subscriptionCreatedDateTime,

    -- ** OwnerDirectoryDescription
    ownerDirectoryDescription_radiusStatus,
    ownerDirectoryDescription_directoryId,
    ownerDirectoryDescription_radiusSettings,
    ownerDirectoryDescription_accountId,
    ownerDirectoryDescription_dnsIpAddrs,
    ownerDirectoryDescription_vpcSettings,

    -- ** RadiusSettings
    radiusSettings_displayLabel,
    radiusSettings_radiusRetries,
    radiusSettings_authenticationProtocol,
    radiusSettings_radiusServers,
    radiusSettings_useSameUsername,
    radiusSettings_sharedSecret,
    radiusSettings_radiusTimeout,
    radiusSettings_radiusPort,

    -- ** RegionDescription
    regionDescription_status,
    regionDescription_directoryId,
    regionDescription_regionName,
    regionDescription_desiredNumberOfDomainControllers,
    regionDescription_regionType,
    regionDescription_launchTime,
    regionDescription_lastUpdatedDateTime,
    regionDescription_statusLastUpdatedDateTime,
    regionDescription_vpcSettings,

    -- ** RegionsInfo
    regionsInfo_primaryRegion,
    regionsInfo_additionalRegions,

    -- ** SchemaExtensionInfo
    schemaExtensionInfo_directoryId,
    schemaExtensionInfo_schemaExtensionId,
    schemaExtensionInfo_schemaExtensionStatusReason,
    schemaExtensionInfo_schemaExtensionStatus,
    schemaExtensionInfo_description,
    schemaExtensionInfo_endDateTime,
    schemaExtensionInfo_startDateTime,

    -- ** ShareTarget
    shareTarget_id,
    shareTarget_type,

    -- ** SharedDirectory
    sharedDirectory_sharedAccountId,
    sharedDirectory_ownerAccountId,
    sharedDirectory_lastUpdatedDateTime,
    sharedDirectory_shareStatus,
    sharedDirectory_shareMethod,
    sharedDirectory_ownerDirectoryId,
    sharedDirectory_sharedDirectoryId,
    sharedDirectory_shareNotes,
    sharedDirectory_createdDateTime,

    -- ** Snapshot
    snapshot_status,
    snapshot_directoryId,
    snapshot_startTime,
    snapshot_name,
    snapshot_type,
    snapshot_snapshotId,

    -- ** SnapshotLimits
    snapshotLimits_manualSnapshotsLimitReached,
    snapshotLimits_manualSnapshotsCurrentCount,
    snapshotLimits_manualSnapshotsLimit,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** Trust
    trust_directoryId,
    trust_trustState,
    trust_lastUpdatedDateTime,
    trust_trustDirection,
    trust_stateLastUpdatedDateTime,
    trust_trustType,
    trust_trustStateReason,
    trust_selectiveAuth,
    trust_remoteDomainName,
    trust_trustId,
    trust_createdDateTime,

    -- ** UnshareTarget
    unshareTarget_id,
    unshareTarget_type,
  )
where

import Network.AWS.DirectoryService.AcceptSharedDirectory
import Network.AWS.DirectoryService.AddIpRoutes
import Network.AWS.DirectoryService.AddRegion
import Network.AWS.DirectoryService.AddTagsToResource
import Network.AWS.DirectoryService.CancelSchemaExtension
import Network.AWS.DirectoryService.ConnectDirectory
import Network.AWS.DirectoryService.CreateAlias
import Network.AWS.DirectoryService.CreateComputer
import Network.AWS.DirectoryService.CreateConditionalForwarder
import Network.AWS.DirectoryService.CreateDirectory
import Network.AWS.DirectoryService.CreateLogSubscription
import Network.AWS.DirectoryService.CreateMicrosoftAD
import Network.AWS.DirectoryService.CreateSnapshot
import Network.AWS.DirectoryService.CreateTrust
import Network.AWS.DirectoryService.DeleteConditionalForwarder
import Network.AWS.DirectoryService.DeleteDirectory
import Network.AWS.DirectoryService.DeleteLogSubscription
import Network.AWS.DirectoryService.DeleteSnapshot
import Network.AWS.DirectoryService.DeleteTrust
import Network.AWS.DirectoryService.DeregisterCertificate
import Network.AWS.DirectoryService.DeregisterEventTopic
import Network.AWS.DirectoryService.DescribeCertificate
import Network.AWS.DirectoryService.DescribeClientAuthenticationSettings
import Network.AWS.DirectoryService.DescribeConditionalForwarders
import Network.AWS.DirectoryService.DescribeDirectories
import Network.AWS.DirectoryService.DescribeDomainControllers
import Network.AWS.DirectoryService.DescribeEventTopics
import Network.AWS.DirectoryService.DescribeLDAPSSettings
import Network.AWS.DirectoryService.DescribeRegions
import Network.AWS.DirectoryService.DescribeSharedDirectories
import Network.AWS.DirectoryService.DescribeSnapshots
import Network.AWS.DirectoryService.DescribeTrusts
import Network.AWS.DirectoryService.DisableClientAuthentication
import Network.AWS.DirectoryService.DisableLDAPS
import Network.AWS.DirectoryService.DisableRadius
import Network.AWS.DirectoryService.DisableSso
import Network.AWS.DirectoryService.EnableClientAuthentication
import Network.AWS.DirectoryService.EnableLDAPS
import Network.AWS.DirectoryService.EnableRadius
import Network.AWS.DirectoryService.EnableSso
import Network.AWS.DirectoryService.GetDirectoryLimits
import Network.AWS.DirectoryService.GetSnapshotLimits
import Network.AWS.DirectoryService.ListCertificates
import Network.AWS.DirectoryService.ListIpRoutes
import Network.AWS.DirectoryService.ListLogSubscriptions
import Network.AWS.DirectoryService.ListSchemaExtensions
import Network.AWS.DirectoryService.ListTagsForResource
import Network.AWS.DirectoryService.RegisterCertificate
import Network.AWS.DirectoryService.RegisterEventTopic
import Network.AWS.DirectoryService.RejectSharedDirectory
import Network.AWS.DirectoryService.RemoveIpRoutes
import Network.AWS.DirectoryService.RemoveRegion
import Network.AWS.DirectoryService.RemoveTagsFromResource
import Network.AWS.DirectoryService.ResetUserPassword
import Network.AWS.DirectoryService.RestoreFromSnapshot
import Network.AWS.DirectoryService.ShareDirectory
import Network.AWS.DirectoryService.StartSchemaExtension
import Network.AWS.DirectoryService.Types.Attribute
import Network.AWS.DirectoryService.Types.Certificate
import Network.AWS.DirectoryService.Types.CertificateInfo
import Network.AWS.DirectoryService.Types.ClientAuthenticationSettingInfo
import Network.AWS.DirectoryService.Types.ClientCertAuthSettings
import Network.AWS.DirectoryService.Types.Computer
import Network.AWS.DirectoryService.Types.ConditionalForwarder
import Network.AWS.DirectoryService.Types.DirectoryConnectSettings
import Network.AWS.DirectoryService.Types.DirectoryConnectSettingsDescription
import Network.AWS.DirectoryService.Types.DirectoryDescription
import Network.AWS.DirectoryService.Types.DirectoryLimits
import Network.AWS.DirectoryService.Types.DirectoryVpcSettings
import Network.AWS.DirectoryService.Types.DirectoryVpcSettingsDescription
import Network.AWS.DirectoryService.Types.DomainController
import Network.AWS.DirectoryService.Types.EventTopic
import Network.AWS.DirectoryService.Types.IpRoute
import Network.AWS.DirectoryService.Types.IpRouteInfo
import Network.AWS.DirectoryService.Types.LDAPSSettingInfo
import Network.AWS.DirectoryService.Types.LogSubscription
import Network.AWS.DirectoryService.Types.OwnerDirectoryDescription
import Network.AWS.DirectoryService.Types.RadiusSettings
import Network.AWS.DirectoryService.Types.RegionDescription
import Network.AWS.DirectoryService.Types.RegionsInfo
import Network.AWS.DirectoryService.Types.SchemaExtensionInfo
import Network.AWS.DirectoryService.Types.ShareTarget
import Network.AWS.DirectoryService.Types.SharedDirectory
import Network.AWS.DirectoryService.Types.Snapshot
import Network.AWS.DirectoryService.Types.SnapshotLimits
import Network.AWS.DirectoryService.Types.Tag
import Network.AWS.DirectoryService.Types.Trust
import Network.AWS.DirectoryService.Types.UnshareTarget
import Network.AWS.DirectoryService.UnshareDirectory
import Network.AWS.DirectoryService.UpdateConditionalForwarder
import Network.AWS.DirectoryService.UpdateNumberOfDomainControllers
import Network.AWS.DirectoryService.UpdateRadius
import Network.AWS.DirectoryService.UpdateTrust
import Network.AWS.DirectoryService.VerifyTrust

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

    -- ** ConnectDirectory
    connectDirectory_shortName,
    connectDirectory_tags,
    connectDirectory_description,
    connectDirectory_name,
    connectDirectory_password,
    connectDirectory_size,
    connectDirectory_connectSettings,
    connectDirectoryResponse_directoryId,
    connectDirectoryResponse_httpStatus,

    -- ** RejectSharedDirectory
    rejectSharedDirectory_sharedDirectoryId,
    rejectSharedDirectoryResponse_sharedDirectoryId,
    rejectSharedDirectoryResponse_httpStatus,

    -- ** DisableRadius
    disableRadius_directoryId,
    disableRadiusResponse_httpStatus,

    -- ** RegisterEventTopic
    registerEventTopic_directoryId,
    registerEventTopic_topicName,
    registerEventTopicResponse_httpStatus,

    -- ** ShareDirectory
    shareDirectory_shareNotes,
    shareDirectory_directoryId,
    shareDirectory_shareTarget,
    shareDirectory_shareMethod,
    shareDirectoryResponse_sharedDirectoryId,
    shareDirectoryResponse_httpStatus,

    -- ** AddRegion
    addRegion_directoryId,
    addRegion_regionName,
    addRegion_vPCSettings,
    addRegionResponse_httpStatus,

    -- ** ListIpRoutes
    listIpRoutes_nextToken,
    listIpRoutes_limit,
    listIpRoutes_directoryId,
    listIpRoutesResponse_nextToken,
    listIpRoutesResponse_ipRoutesInfo,
    listIpRoutesResponse_httpStatus,

    -- ** EnableRadius
    enableRadius_directoryId,
    enableRadius_radiusSettings,
    enableRadiusResponse_httpStatus,

    -- ** ListSchemaExtensions
    listSchemaExtensions_nextToken,
    listSchemaExtensions_limit,
    listSchemaExtensions_directoryId,
    listSchemaExtensionsResponse_nextToken,
    listSchemaExtensionsResponse_schemaExtensionsInfo,
    listSchemaExtensionsResponse_httpStatus,

    -- ** RemoveRegion
    removeRegion_directoryId,
    removeRegionResponse_httpStatus,

    -- ** DeleteLogSubscription
    deleteLogSubscription_directoryId,
    deleteLogSubscriptionResponse_httpStatus,

    -- ** CancelSchemaExtension
    cancelSchemaExtension_directoryId,
    cancelSchemaExtension_schemaExtensionId,
    cancelSchemaExtensionResponse_httpStatus,

    -- ** EnableSso
    enableSso_password,
    enableSso_userName,
    enableSso_directoryId,
    enableSsoResponse_httpStatus,

    -- ** CreateConditionalForwarder
    createConditionalForwarder_directoryId,
    createConditionalForwarder_remoteDomainName,
    createConditionalForwarder_dnsIpAddrs,
    createConditionalForwarderResponse_httpStatus,

    -- ** RemoveTagsFromResource
    removeTagsFromResource_resourceId,
    removeTagsFromResource_tagKeys,
    removeTagsFromResourceResponse_httpStatus,

    -- ** EnableLDAPS
    enableLDAPS_directoryId,
    enableLDAPS_type,
    enableLDAPSResponse_httpStatus,

    -- ** DeleteConditionalForwarder
    deleteConditionalForwarder_directoryId,
    deleteConditionalForwarder_remoteDomainName,
    deleteConditionalForwarderResponse_httpStatus,

    -- ** DescribeSharedDirectories
    describeSharedDirectories_nextToken,
    describeSharedDirectories_sharedDirectoryIds,
    describeSharedDirectories_limit,
    describeSharedDirectories_ownerDirectoryId,
    describeSharedDirectoriesResponse_nextToken,
    describeSharedDirectoriesResponse_sharedDirectories,
    describeSharedDirectoriesResponse_httpStatus,

    -- ** UpdateConditionalForwarder
    updateConditionalForwarder_directoryId,
    updateConditionalForwarder_remoteDomainName,
    updateConditionalForwarder_dnsIpAddrs,
    updateConditionalForwarderResponse_httpStatus,

    -- ** VerifyTrust
    verifyTrust_trustId,
    verifyTrustResponse_trustId,
    verifyTrustResponse_httpStatus,

    -- ** DescribeCertificate
    describeCertificate_directoryId,
    describeCertificate_certificateId,
    describeCertificateResponse_certificate,
    describeCertificateResponse_httpStatus,

    -- ** CreateTrust
    createTrust_trustType,
    createTrust_selectiveAuth,
    createTrust_conditionalForwarderIpAddrs,
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

    -- ** DisableClientAuthentication
    disableClientAuthentication_directoryId,
    disableClientAuthentication_type,
    disableClientAuthenticationResponse_httpStatus,

    -- ** CreateMicrosoftAD
    createMicrosoftAD_shortName,
    createMicrosoftAD_edition,
    createMicrosoftAD_tags,
    createMicrosoftAD_description,
    createMicrosoftAD_name,
    createMicrosoftAD_password,
    createMicrosoftAD_vpcSettings,
    createMicrosoftADResponse_directoryId,
    createMicrosoftADResponse_httpStatus,

    -- ** DeleteSnapshot
    deleteSnapshot_snapshotId,
    deleteSnapshotResponse_snapshotId,
    deleteSnapshotResponse_httpStatus,

    -- ** RemoveIpRoutes
    removeIpRoutes_directoryId,
    removeIpRoutes_cidrIps,
    removeIpRoutesResponse_httpStatus,

    -- ** UpdateTrust
    updateTrust_selectiveAuth,
    updateTrust_trustId,
    updateTrustResponse_trustId,
    updateTrustResponse_requestId,
    updateTrustResponse_httpStatus,

    -- ** DeleteTrust
    deleteTrust_deleteAssociatedConditionalForwarder,
    deleteTrust_trustId,
    deleteTrustResponse_trustId,
    deleteTrustResponse_httpStatus,

    -- ** CreateDirectory
    createDirectory_vpcSettings,
    createDirectory_shortName,
    createDirectory_tags,
    createDirectory_description,
    createDirectory_name,
    createDirectory_password,
    createDirectory_size,
    createDirectoryResponse_directoryId,
    createDirectoryResponse_httpStatus,

    -- ** RestoreFromSnapshot
    restoreFromSnapshot_snapshotId,
    restoreFromSnapshotResponse_httpStatus,

    -- ** DescribeDomainControllers
    describeDomainControllers_nextToken,
    describeDomainControllers_domainControllerIds,
    describeDomainControllers_limit,
    describeDomainControllers_directoryId,
    describeDomainControllersResponse_domainControllers,
    describeDomainControllersResponse_nextToken,
    describeDomainControllersResponse_httpStatus,

    -- ** DescribeTrusts
    describeTrusts_nextToken,
    describeTrusts_directoryId,
    describeTrusts_limit,
    describeTrusts_trustIds,
    describeTrustsResponse_nextToken,
    describeTrustsResponse_trusts,
    describeTrustsResponse_httpStatus,

    -- ** DescribeSnapshots
    describeSnapshots_nextToken,
    describeSnapshots_snapshotIds,
    describeSnapshots_directoryId,
    describeSnapshots_limit,
    describeSnapshotsResponse_snapshots,
    describeSnapshotsResponse_nextToken,
    describeSnapshotsResponse_httpStatus,

    -- ** UnshareDirectory
    unshareDirectory_directoryId,
    unshareDirectory_unshareTarget,
    unshareDirectoryResponse_sharedDirectoryId,
    unshareDirectoryResponse_httpStatus,

    -- ** RegisterCertificate
    registerCertificate_clientCertAuthSettings,
    registerCertificate_type,
    registerCertificate_directoryId,
    registerCertificate_certificateData,
    registerCertificateResponse_certificateId,
    registerCertificateResponse_httpStatus,

    -- ** GetSnapshotLimits
    getSnapshotLimits_directoryId,
    getSnapshotLimitsResponse_snapshotLimits,
    getSnapshotLimitsResponse_httpStatus,

    -- ** UpdateNumberOfDomainControllers
    updateNumberOfDomainControllers_directoryId,
    updateNumberOfDomainControllers_desiredNumber,
    updateNumberOfDomainControllersResponse_httpStatus,

    -- ** ListCertificates
    listCertificates_nextToken,
    listCertificates_limit,
    listCertificates_directoryId,
    listCertificatesResponse_nextToken,
    listCertificatesResponse_certificatesInfo,
    listCertificatesResponse_httpStatus,

    -- ** DescribeConditionalForwarders
    describeConditionalForwarders_remoteDomainNames,
    describeConditionalForwarders_directoryId,
    describeConditionalForwardersResponse_conditionalForwarders,
    describeConditionalForwardersResponse_httpStatus,

    -- ** AddTagsToResource
    addTagsToResource_resourceId,
    addTagsToResource_tags,
    addTagsToResourceResponse_httpStatus,

    -- ** GetDirectoryLimits
    getDirectoryLimitsResponse_directoryLimits,
    getDirectoryLimitsResponse_httpStatus,

    -- ** UpdateRadius
    updateRadius_directoryId,
    updateRadius_radiusSettings,
    updateRadiusResponse_httpStatus,

    -- ** DisableLDAPS
    disableLDAPS_directoryId,
    disableLDAPS_type,
    disableLDAPSResponse_httpStatus,

    -- ** ListLogSubscriptions
    listLogSubscriptions_nextToken,
    listLogSubscriptions_directoryId,
    listLogSubscriptions_limit,
    listLogSubscriptionsResponse_nextToken,
    listLogSubscriptionsResponse_logSubscriptions,
    listLogSubscriptionsResponse_httpStatus,

    -- ** DescribeRegions
    describeRegions_regionName,
    describeRegions_nextToken,
    describeRegions_directoryId,
    describeRegionsResponse_nextToken,
    describeRegionsResponse_regionsDescription,
    describeRegionsResponse_httpStatus,

    -- ** DisableSso
    disableSso_password,
    disableSso_userName,
    disableSso_directoryId,
    disableSsoResponse_httpStatus,

    -- ** CreateLogSubscription
    createLogSubscription_directoryId,
    createLogSubscription_logGroupName,
    createLogSubscriptionResponse_httpStatus,

    -- ** ResetUserPassword
    resetUserPassword_directoryId,
    resetUserPassword_userName,
    resetUserPassword_newPassword,
    resetUserPasswordResponse_httpStatus,

    -- ** DescribeEventTopics
    describeEventTopics_directoryId,
    describeEventTopics_topicNames,
    describeEventTopicsResponse_eventTopics,
    describeEventTopicsResponse_httpStatus,

    -- ** CreateComputer
    createComputer_organizationalUnitDistinguishedName,
    createComputer_computerAttributes,
    createComputer_directoryId,
    createComputer_computerName,
    createComputer_password,
    createComputerResponse_computer,
    createComputerResponse_httpStatus,

    -- ** AcceptSharedDirectory
    acceptSharedDirectory_sharedDirectoryId,
    acceptSharedDirectoryResponse_sharedDirectory,
    acceptSharedDirectoryResponse_httpStatus,

    -- ** EnableClientAuthentication
    enableClientAuthentication_directoryId,
    enableClientAuthentication_type,
    enableClientAuthenticationResponse_httpStatus,

    -- ** CreateSnapshot
    createSnapshot_name,
    createSnapshot_directoryId,
    createSnapshotResponse_snapshotId,
    createSnapshotResponse_httpStatus,

    -- ** StartSchemaExtension
    startSchemaExtension_directoryId,
    startSchemaExtension_createSnapshotBeforeSchemaExtension,
    startSchemaExtension_ldifContent,
    startSchemaExtension_description,
    startSchemaExtensionResponse_schemaExtensionId,
    startSchemaExtensionResponse_httpStatus,

    -- ** DeregisterEventTopic
    deregisterEventTopic_directoryId,
    deregisterEventTopic_topicName,
    deregisterEventTopicResponse_httpStatus,

    -- ** DeregisterCertificate
    deregisterCertificate_directoryId,
    deregisterCertificate_certificateId,
    deregisterCertificateResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_limit,
    listTagsForResource_resourceId,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** CreateAlias
    createAlias_directoryId,
    createAlias_alias,
    createAliasResponse_alias,
    createAliasResponse_directoryId,
    createAliasResponse_httpStatus,

    -- ** AddIpRoutes
    addIpRoutes_updateSecurityGroupForDirectoryControllers,
    addIpRoutes_directoryId,
    addIpRoutes_ipRoutes,
    addIpRoutesResponse_httpStatus,

    -- ** DescribeDirectories
    describeDirectories_nextToken,
    describeDirectories_directoryIds,
    describeDirectories_limit,
    describeDirectoriesResponse_nextToken,
    describeDirectoriesResponse_directoryDescriptions,
    describeDirectoriesResponse_httpStatus,

    -- ** DescribeLDAPSSettings
    describeLDAPSSettings_nextToken,
    describeLDAPSSettings_type,
    describeLDAPSSettings_limit,
    describeLDAPSSettings_directoryId,
    describeLDAPSSettingsResponse_nextToken,
    describeLDAPSSettingsResponse_lDAPSSettingsInfo,
    describeLDAPSSettingsResponse_httpStatus,

    -- * Types

    -- ** Attribute
    attribute_name,
    attribute_value,

    -- ** Certificate
    certificate_clientCertAuthSettings,
    certificate_registeredDateTime,
    certificate_stateReason,
    certificate_commonName,
    certificate_state,
    certificate_expiryDateTime,
    certificate_type,
    certificate_certificateId,

    -- ** CertificateInfo
    certificateInfo_commonName,
    certificateInfo_state,
    certificateInfo_expiryDateTime,
    certificateInfo_type,
    certificateInfo_certificateId,

    -- ** ClientCertAuthSettings
    clientCertAuthSettings_oCSPUrl,

    -- ** Computer
    computer_computerName,
    computer_computerAttributes,
    computer_computerId,

    -- ** ConditionalForwarder
    conditionalForwarder_replicationScope,
    conditionalForwarder_remoteDomainName,
    conditionalForwarder_dnsIpAddrs,

    -- ** DirectoryConnectSettings
    directoryConnectSettings_vpcId,
    directoryConnectSettings_subnetIds,
    directoryConnectSettings_customerDnsIps,
    directoryConnectSettings_customerUserName,

    -- ** DirectoryConnectSettingsDescription
    directoryConnectSettingsDescription_securityGroupId,
    directoryConnectSettingsDescription_availabilityZones,
    directoryConnectSettingsDescription_subnetIds,
    directoryConnectSettingsDescription_customerUserName,
    directoryConnectSettingsDescription_connectIps,
    directoryConnectSettingsDescription_vpcId,

    -- ** DirectoryDescription
    directoryDescription_radiusStatus,
    directoryDescription_alias,
    directoryDescription_shareNotes,
    directoryDescription_connectSettings,
    directoryDescription_vpcSettings,
    directoryDescription_stageReason,
    directoryDescription_launchTime,
    directoryDescription_regionsInfo,
    directoryDescription_shortName,
    directoryDescription_shareMethod,
    directoryDescription_accessUrl,
    directoryDescription_name,
    directoryDescription_stage,
    directoryDescription_edition,
    directoryDescription_directoryId,
    directoryDescription_shareStatus,
    directoryDescription_ownerDirectoryDescription,
    directoryDescription_description,
    directoryDescription_type,
    directoryDescription_dnsIpAddrs,
    directoryDescription_radiusSettings,
    directoryDescription_desiredNumberOfDomainControllers,
    directoryDescription_size,
    directoryDescription_stageLastUpdatedDateTime,
    directoryDescription_ssoEnabled,

    -- ** DirectoryLimits
    directoryLimits_cloudOnlyDirectoriesLimit,
    directoryLimits_connectedDirectoriesLimitReached,
    directoryLimits_cloudOnlyMicrosoftADCurrentCount,
    directoryLimits_connectedDirectoriesLimit,
    directoryLimits_connectedDirectoriesCurrentCount,
    directoryLimits_cloudOnlyMicrosoftADLimit,
    directoryLimits_cloudOnlyDirectoriesLimitReached,
    directoryLimits_cloudOnlyDirectoriesCurrentCount,
    directoryLimits_cloudOnlyMicrosoftADLimitReached,

    -- ** DirectoryVpcSettings
    directoryVpcSettings_vpcId,
    directoryVpcSettings_subnetIds,

    -- ** DirectoryVpcSettingsDescription
    directoryVpcSettingsDescription_securityGroupId,
    directoryVpcSettingsDescription_availabilityZones,
    directoryVpcSettingsDescription_subnetIds,
    directoryVpcSettingsDescription_vpcId,

    -- ** DomainController
    domainController_status,
    domainController_dnsIpAddr,
    domainController_launchTime,
    domainController_statusLastUpdatedDateTime,
    domainController_availabilityZone,
    domainController_directoryId,
    domainController_domainControllerId,
    domainController_subnetId,
    domainController_vpcId,
    domainController_statusReason,

    -- ** EventTopic
    eventTopic_status,
    eventTopic_createdDateTime,
    eventTopic_topicName,
    eventTopic_topicArn,
    eventTopic_directoryId,

    -- ** IpRoute
    ipRoute_cidrIp,
    ipRoute_description,

    -- ** IpRouteInfo
    ipRouteInfo_cidrIp,
    ipRouteInfo_ipRouteStatusMsg,
    ipRouteInfo_directoryId,
    ipRouteInfo_addedDateTime,
    ipRouteInfo_description,
    ipRouteInfo_ipRouteStatusReason,

    -- ** LDAPSSettingInfo
    lDAPSSettingInfo_lastUpdatedDateTime,
    lDAPSSettingInfo_lDAPSStatusReason,
    lDAPSSettingInfo_lDAPSStatus,

    -- ** LogSubscription
    logSubscription_subscriptionCreatedDateTime,
    logSubscription_logGroupName,
    logSubscription_directoryId,

    -- ** OwnerDirectoryDescription
    ownerDirectoryDescription_radiusStatus,
    ownerDirectoryDescription_accountId,
    ownerDirectoryDescription_vpcSettings,
    ownerDirectoryDescription_directoryId,
    ownerDirectoryDescription_dnsIpAddrs,
    ownerDirectoryDescription_radiusSettings,

    -- ** RadiusSettings
    radiusSettings_useSameUsername,
    radiusSettings_displayLabel,
    radiusSettings_radiusServers,
    radiusSettings_radiusRetries,
    radiusSettings_radiusTimeout,
    radiusSettings_sharedSecret,
    radiusSettings_radiusPort,
    radiusSettings_authenticationProtocol,

    -- ** RegionDescription
    regionDescription_regionName,
    regionDescription_status,
    regionDescription_lastUpdatedDateTime,
    regionDescription_vpcSettings,
    regionDescription_regionType,
    regionDescription_launchTime,
    regionDescription_statusLastUpdatedDateTime,
    regionDescription_directoryId,
    regionDescription_desiredNumberOfDomainControllers,

    -- ** RegionsInfo
    regionsInfo_additionalRegions,
    regionsInfo_primaryRegion,

    -- ** SchemaExtensionInfo
    schemaExtensionInfo_schemaExtensionStatus,
    schemaExtensionInfo_startDateTime,
    schemaExtensionInfo_schemaExtensionId,
    schemaExtensionInfo_directoryId,
    schemaExtensionInfo_endDateTime,
    schemaExtensionInfo_description,
    schemaExtensionInfo_schemaExtensionStatusReason,

    -- ** ShareTarget
    shareTarget_id,
    shareTarget_type,

    -- ** SharedDirectory
    sharedDirectory_createdDateTime,
    sharedDirectory_shareNotes,
    sharedDirectory_lastUpdatedDateTime,
    sharedDirectory_sharedAccountId,
    sharedDirectory_ownerDirectoryId,
    sharedDirectory_shareMethod,
    sharedDirectory_shareStatus,
    sharedDirectory_ownerAccountId,
    sharedDirectory_sharedDirectoryId,

    -- ** Snapshot
    snapshot_status,
    snapshot_startTime,
    snapshot_name,
    snapshot_directoryId,
    snapshot_snapshotId,
    snapshot_type,

    -- ** SnapshotLimits
    snapshotLimits_manualSnapshotsCurrentCount,
    snapshotLimits_manualSnapshotsLimitReached,
    snapshotLimits_manualSnapshotsLimit,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** Trust
    trust_trustType,
    trust_createdDateTime,
    trust_trustId,
    trust_trustDirection,
    trust_stateLastUpdatedDateTime,
    trust_lastUpdatedDateTime,
    trust_trustState,
    trust_selectiveAuth,
    trust_trustStateReason,
    trust_directoryId,
    trust_remoteDomainName,

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

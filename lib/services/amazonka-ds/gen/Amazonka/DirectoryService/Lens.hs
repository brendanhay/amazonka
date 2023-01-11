{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DirectoryService.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Lens
  ( -- * Operations

    -- ** AcceptSharedDirectory
    acceptSharedDirectory_sharedDirectoryId,
    acceptSharedDirectoryResponse_sharedDirectory,
    acceptSharedDirectoryResponse_httpStatus,

    -- ** AddIpRoutes
    addIpRoutes_updateSecurityGroupForDirectoryControllers,
    addIpRoutes_directoryId,
    addIpRoutes_ipRoutes,
    addIpRoutesResponse_httpStatus,

    -- ** AddRegion
    addRegion_directoryId,
    addRegion_regionName,
    addRegion_vPCSettings,
    addRegionResponse_httpStatus,

    -- ** AddTagsToResource
    addTagsToResource_resourceId,
    addTagsToResource_tags,
    addTagsToResourceResponse_httpStatus,

    -- ** CancelSchemaExtension
    cancelSchemaExtension_directoryId,
    cancelSchemaExtension_schemaExtensionId,
    cancelSchemaExtensionResponse_httpStatus,

    -- ** ConnectDirectory
    connectDirectory_description,
    connectDirectory_shortName,
    connectDirectory_tags,
    connectDirectory_name,
    connectDirectory_password,
    connectDirectory_size,
    connectDirectory_connectSettings,
    connectDirectoryResponse_directoryId,
    connectDirectoryResponse_httpStatus,

    -- ** CreateAlias
    createAlias_directoryId,
    createAlias_alias,
    createAliasResponse_alias,
    createAliasResponse_directoryId,
    createAliasResponse_httpStatus,

    -- ** CreateComputer
    createComputer_computerAttributes,
    createComputer_organizationalUnitDistinguishedName,
    createComputer_directoryId,
    createComputer_computerName,
    createComputer_password,
    createComputerResponse_computer,
    createComputerResponse_httpStatus,

    -- ** CreateConditionalForwarder
    createConditionalForwarder_directoryId,
    createConditionalForwarder_remoteDomainName,
    createConditionalForwarder_dnsIpAddrs,
    createConditionalForwarderResponse_httpStatus,

    -- ** CreateDirectory
    createDirectory_description,
    createDirectory_shortName,
    createDirectory_tags,
    createDirectory_vpcSettings,
    createDirectory_name,
    createDirectory_password,
    createDirectory_size,
    createDirectoryResponse_directoryId,
    createDirectoryResponse_httpStatus,

    -- ** CreateLogSubscription
    createLogSubscription_directoryId,
    createLogSubscription_logGroupName,
    createLogSubscriptionResponse_httpStatus,

    -- ** CreateMicrosoftAD
    createMicrosoftAD_description,
    createMicrosoftAD_edition,
    createMicrosoftAD_shortName,
    createMicrosoftAD_tags,
    createMicrosoftAD_name,
    createMicrosoftAD_password,
    createMicrosoftAD_vpcSettings,
    createMicrosoftADResponse_directoryId,
    createMicrosoftADResponse_httpStatus,

    -- ** CreateSnapshot
    createSnapshot_name,
    createSnapshot_directoryId,
    createSnapshotResponse_snapshotId,
    createSnapshotResponse_httpStatus,

    -- ** CreateTrust
    createTrust_conditionalForwarderIpAddrs,
    createTrust_selectiveAuth,
    createTrust_trustType,
    createTrust_directoryId,
    createTrust_remoteDomainName,
    createTrust_trustPassword,
    createTrust_trustDirection,
    createTrustResponse_trustId,
    createTrustResponse_httpStatus,

    -- ** DeleteConditionalForwarder
    deleteConditionalForwarder_directoryId,
    deleteConditionalForwarder_remoteDomainName,
    deleteConditionalForwarderResponse_httpStatus,

    -- ** DeleteDirectory
    deleteDirectory_directoryId,
    deleteDirectoryResponse_directoryId,
    deleteDirectoryResponse_httpStatus,

    -- ** DeleteLogSubscription
    deleteLogSubscription_directoryId,
    deleteLogSubscriptionResponse_httpStatus,

    -- ** DeleteSnapshot
    deleteSnapshot_snapshotId,
    deleteSnapshotResponse_snapshotId,
    deleteSnapshotResponse_httpStatus,

    -- ** DeleteTrust
    deleteTrust_deleteAssociatedConditionalForwarder,
    deleteTrust_trustId,
    deleteTrustResponse_trustId,
    deleteTrustResponse_httpStatus,

    -- ** DeregisterCertificate
    deregisterCertificate_directoryId,
    deregisterCertificate_certificateId,
    deregisterCertificateResponse_httpStatus,

    -- ** DeregisterEventTopic
    deregisterEventTopic_directoryId,
    deregisterEventTopic_topicName,
    deregisterEventTopicResponse_httpStatus,

    -- ** DescribeCertificate
    describeCertificate_directoryId,
    describeCertificate_certificateId,
    describeCertificateResponse_certificate,
    describeCertificateResponse_httpStatus,

    -- ** DescribeClientAuthenticationSettings
    describeClientAuthenticationSettings_limit,
    describeClientAuthenticationSettings_nextToken,
    describeClientAuthenticationSettings_type,
    describeClientAuthenticationSettings_directoryId,
    describeClientAuthenticationSettingsResponse_clientAuthenticationSettingsInfo,
    describeClientAuthenticationSettingsResponse_nextToken,
    describeClientAuthenticationSettingsResponse_httpStatus,

    -- ** DescribeConditionalForwarders
    describeConditionalForwarders_remoteDomainNames,
    describeConditionalForwarders_directoryId,
    describeConditionalForwardersResponse_conditionalForwarders,
    describeConditionalForwardersResponse_httpStatus,

    -- ** DescribeDirectories
    describeDirectories_directoryIds,
    describeDirectories_limit,
    describeDirectories_nextToken,
    describeDirectoriesResponse_directoryDescriptions,
    describeDirectoriesResponse_nextToken,
    describeDirectoriesResponse_httpStatus,

    -- ** DescribeDomainControllers
    describeDomainControllers_domainControllerIds,
    describeDomainControllers_limit,
    describeDomainControllers_nextToken,
    describeDomainControllers_directoryId,
    describeDomainControllersResponse_domainControllers,
    describeDomainControllersResponse_nextToken,
    describeDomainControllersResponse_httpStatus,

    -- ** DescribeEventTopics
    describeEventTopics_directoryId,
    describeEventTopics_topicNames,
    describeEventTopicsResponse_eventTopics,
    describeEventTopicsResponse_httpStatus,

    -- ** DescribeLDAPSSettings
    describeLDAPSSettings_limit,
    describeLDAPSSettings_nextToken,
    describeLDAPSSettings_type,
    describeLDAPSSettings_directoryId,
    describeLDAPSSettingsResponse_lDAPSSettingsInfo,
    describeLDAPSSettingsResponse_nextToken,
    describeLDAPSSettingsResponse_httpStatus,

    -- ** DescribeRegions
    describeRegions_nextToken,
    describeRegions_regionName,
    describeRegions_directoryId,
    describeRegionsResponse_nextToken,
    describeRegionsResponse_regionsDescription,
    describeRegionsResponse_httpStatus,

    -- ** DescribeSettings
    describeSettings_nextToken,
    describeSettings_status,
    describeSettings_directoryId,
    describeSettingsResponse_directoryId,
    describeSettingsResponse_nextToken,
    describeSettingsResponse_settingEntries,
    describeSettingsResponse_httpStatus,

    -- ** DescribeSharedDirectories
    describeSharedDirectories_limit,
    describeSharedDirectories_nextToken,
    describeSharedDirectories_sharedDirectoryIds,
    describeSharedDirectories_ownerDirectoryId,
    describeSharedDirectoriesResponse_nextToken,
    describeSharedDirectoriesResponse_sharedDirectories,
    describeSharedDirectoriesResponse_httpStatus,

    -- ** DescribeSnapshots
    describeSnapshots_directoryId,
    describeSnapshots_limit,
    describeSnapshots_nextToken,
    describeSnapshots_snapshotIds,
    describeSnapshotsResponse_nextToken,
    describeSnapshotsResponse_snapshots,
    describeSnapshotsResponse_httpStatus,

    -- ** DescribeTrusts
    describeTrusts_directoryId,
    describeTrusts_limit,
    describeTrusts_nextToken,
    describeTrusts_trustIds,
    describeTrustsResponse_nextToken,
    describeTrustsResponse_trusts,
    describeTrustsResponse_httpStatus,

    -- ** DescribeUpdateDirectory
    describeUpdateDirectory_nextToken,
    describeUpdateDirectory_regionName,
    describeUpdateDirectory_directoryId,
    describeUpdateDirectory_updateType,
    describeUpdateDirectoryResponse_nextToken,
    describeUpdateDirectoryResponse_updateActivities,
    describeUpdateDirectoryResponse_httpStatus,

    -- ** DisableClientAuthentication
    disableClientAuthentication_directoryId,
    disableClientAuthentication_type,
    disableClientAuthenticationResponse_httpStatus,

    -- ** DisableLDAPS
    disableLDAPS_directoryId,
    disableLDAPS_type,
    disableLDAPSResponse_httpStatus,

    -- ** DisableRadius
    disableRadius_directoryId,
    disableRadiusResponse_httpStatus,

    -- ** DisableSso
    disableSso_password,
    disableSso_userName,
    disableSso_directoryId,
    disableSsoResponse_httpStatus,

    -- ** EnableClientAuthentication
    enableClientAuthentication_directoryId,
    enableClientAuthentication_type,
    enableClientAuthenticationResponse_httpStatus,

    -- ** EnableLDAPS
    enableLDAPS_directoryId,
    enableLDAPS_type,
    enableLDAPSResponse_httpStatus,

    -- ** EnableRadius
    enableRadius_directoryId,
    enableRadius_radiusSettings,
    enableRadiusResponse_httpStatus,

    -- ** EnableSso
    enableSso_password,
    enableSso_userName,
    enableSso_directoryId,
    enableSsoResponse_httpStatus,

    -- ** GetDirectoryLimits
    getDirectoryLimitsResponse_directoryLimits,
    getDirectoryLimitsResponse_httpStatus,

    -- ** GetSnapshotLimits
    getSnapshotLimits_directoryId,
    getSnapshotLimitsResponse_snapshotLimits,
    getSnapshotLimitsResponse_httpStatus,

    -- ** ListCertificates
    listCertificates_limit,
    listCertificates_nextToken,
    listCertificates_directoryId,
    listCertificatesResponse_certificatesInfo,
    listCertificatesResponse_nextToken,
    listCertificatesResponse_httpStatus,

    -- ** ListIpRoutes
    listIpRoutes_limit,
    listIpRoutes_nextToken,
    listIpRoutes_directoryId,
    listIpRoutesResponse_ipRoutesInfo,
    listIpRoutesResponse_nextToken,
    listIpRoutesResponse_httpStatus,

    -- ** ListLogSubscriptions
    listLogSubscriptions_directoryId,
    listLogSubscriptions_limit,
    listLogSubscriptions_nextToken,
    listLogSubscriptionsResponse_logSubscriptions,
    listLogSubscriptionsResponse_nextToken,
    listLogSubscriptionsResponse_httpStatus,

    -- ** ListSchemaExtensions
    listSchemaExtensions_limit,
    listSchemaExtensions_nextToken,
    listSchemaExtensions_directoryId,
    listSchemaExtensionsResponse_nextToken,
    listSchemaExtensionsResponse_schemaExtensionsInfo,
    listSchemaExtensionsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_limit,
    listTagsForResource_nextToken,
    listTagsForResource_resourceId,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** RegisterCertificate
    registerCertificate_clientCertAuthSettings,
    registerCertificate_type,
    registerCertificate_directoryId,
    registerCertificate_certificateData,
    registerCertificateResponse_certificateId,
    registerCertificateResponse_httpStatus,

    -- ** RegisterEventTopic
    registerEventTopic_directoryId,
    registerEventTopic_topicName,
    registerEventTopicResponse_httpStatus,

    -- ** RejectSharedDirectory
    rejectSharedDirectory_sharedDirectoryId,
    rejectSharedDirectoryResponse_sharedDirectoryId,
    rejectSharedDirectoryResponse_httpStatus,

    -- ** RemoveIpRoutes
    removeIpRoutes_directoryId,
    removeIpRoutes_cidrIps,
    removeIpRoutesResponse_httpStatus,

    -- ** RemoveRegion
    removeRegion_directoryId,
    removeRegionResponse_httpStatus,

    -- ** RemoveTagsFromResource
    removeTagsFromResource_resourceId,
    removeTagsFromResource_tagKeys,
    removeTagsFromResourceResponse_httpStatus,

    -- ** ResetUserPassword
    resetUserPassword_directoryId,
    resetUserPassword_userName,
    resetUserPassword_newPassword,
    resetUserPasswordResponse_httpStatus,

    -- ** RestoreFromSnapshot
    restoreFromSnapshot_snapshotId,
    restoreFromSnapshotResponse_httpStatus,

    -- ** ShareDirectory
    shareDirectory_shareNotes,
    shareDirectory_directoryId,
    shareDirectory_shareTarget,
    shareDirectory_shareMethod,
    shareDirectoryResponse_sharedDirectoryId,
    shareDirectoryResponse_httpStatus,

    -- ** StartSchemaExtension
    startSchemaExtension_directoryId,
    startSchemaExtension_createSnapshotBeforeSchemaExtension,
    startSchemaExtension_ldifContent,
    startSchemaExtension_description,
    startSchemaExtensionResponse_schemaExtensionId,
    startSchemaExtensionResponse_httpStatus,

    -- ** UnshareDirectory
    unshareDirectory_directoryId,
    unshareDirectory_unshareTarget,
    unshareDirectoryResponse_sharedDirectoryId,
    unshareDirectoryResponse_httpStatus,

    -- ** UpdateConditionalForwarder
    updateConditionalForwarder_directoryId,
    updateConditionalForwarder_remoteDomainName,
    updateConditionalForwarder_dnsIpAddrs,
    updateConditionalForwarderResponse_httpStatus,

    -- ** UpdateDirectorySetup
    updateDirectorySetup_createSnapshotBeforeUpdate,
    updateDirectorySetup_oSUpdateSettings,
    updateDirectorySetup_directoryId,
    updateDirectorySetup_updateType,
    updateDirectorySetupResponse_httpStatus,

    -- ** UpdateNumberOfDomainControllers
    updateNumberOfDomainControllers_directoryId,
    updateNumberOfDomainControllers_desiredNumber,
    updateNumberOfDomainControllersResponse_httpStatus,

    -- ** UpdateRadius
    updateRadius_directoryId,
    updateRadius_radiusSettings,
    updateRadiusResponse_httpStatus,

    -- ** UpdateSettings
    updateSettings_directoryId,
    updateSettings_settings,
    updateSettingsResponse_directoryId,
    updateSettingsResponse_httpStatus,

    -- ** UpdateTrust
    updateTrust_selectiveAuth,
    updateTrust_trustId,
    updateTrustResponse_requestId,
    updateTrustResponse_trustId,
    updateTrustResponse_httpStatus,

    -- ** VerifyTrust
    verifyTrust_trustId,
    verifyTrustResponse_trustId,
    verifyTrustResponse_httpStatus,

    -- * Types

    -- ** Attribute
    attribute_name,
    attribute_value,

    -- ** Certificate
    certificate_certificateId,
    certificate_clientCertAuthSettings,
    certificate_commonName,
    certificate_expiryDateTime,
    certificate_registeredDateTime,
    certificate_state,
    certificate_stateReason,
    certificate_type,

    -- ** CertificateInfo
    certificateInfo_certificateId,
    certificateInfo_commonName,
    certificateInfo_expiryDateTime,
    certificateInfo_state,
    certificateInfo_type,

    -- ** ClientAuthenticationSettingInfo
    clientAuthenticationSettingInfo_lastUpdatedDateTime,
    clientAuthenticationSettingInfo_status,
    clientAuthenticationSettingInfo_type,

    -- ** ClientCertAuthSettings
    clientCertAuthSettings_oCSPUrl,

    -- ** Computer
    computer_computerAttributes,
    computer_computerId,
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
    directoryConnectSettingsDescription_availabilityZones,
    directoryConnectSettingsDescription_connectIps,
    directoryConnectSettingsDescription_customerUserName,
    directoryConnectSettingsDescription_securityGroupId,
    directoryConnectSettingsDescription_subnetIds,
    directoryConnectSettingsDescription_vpcId,

    -- ** DirectoryDescription
    directoryDescription_accessUrl,
    directoryDescription_alias,
    directoryDescription_connectSettings,
    directoryDescription_description,
    directoryDescription_desiredNumberOfDomainControllers,
    directoryDescription_directoryId,
    directoryDescription_dnsIpAddrs,
    directoryDescription_edition,
    directoryDescription_launchTime,
    directoryDescription_name,
    directoryDescription_osVersion,
    directoryDescription_ownerDirectoryDescription,
    directoryDescription_radiusSettings,
    directoryDescription_radiusStatus,
    directoryDescription_regionsInfo,
    directoryDescription_shareMethod,
    directoryDescription_shareNotes,
    directoryDescription_shareStatus,
    directoryDescription_shortName,
    directoryDescription_size,
    directoryDescription_ssoEnabled,
    directoryDescription_stage,
    directoryDescription_stageLastUpdatedDateTime,
    directoryDescription_stageReason,
    directoryDescription_type,
    directoryDescription_vpcSettings,

    -- ** DirectoryLimits
    directoryLimits_cloudOnlyDirectoriesCurrentCount,
    directoryLimits_cloudOnlyDirectoriesLimit,
    directoryLimits_cloudOnlyDirectoriesLimitReached,
    directoryLimits_cloudOnlyMicrosoftADCurrentCount,
    directoryLimits_cloudOnlyMicrosoftADLimit,
    directoryLimits_cloudOnlyMicrosoftADLimitReached,
    directoryLimits_connectedDirectoriesCurrentCount,
    directoryLimits_connectedDirectoriesLimit,
    directoryLimits_connectedDirectoriesLimitReached,

    -- ** DirectoryVpcSettings
    directoryVpcSettings_vpcId,
    directoryVpcSettings_subnetIds,

    -- ** DirectoryVpcSettingsDescription
    directoryVpcSettingsDescription_availabilityZones,
    directoryVpcSettingsDescription_securityGroupId,
    directoryVpcSettingsDescription_subnetIds,
    directoryVpcSettingsDescription_vpcId,

    -- ** DomainController
    domainController_availabilityZone,
    domainController_directoryId,
    domainController_dnsIpAddr,
    domainController_domainControllerId,
    domainController_launchTime,
    domainController_status,
    domainController_statusLastUpdatedDateTime,
    domainController_statusReason,
    domainController_subnetId,
    domainController_vpcId,

    -- ** EventTopic
    eventTopic_createdDateTime,
    eventTopic_directoryId,
    eventTopic_status,
    eventTopic_topicArn,
    eventTopic_topicName,

    -- ** IpRoute
    ipRoute_cidrIp,
    ipRoute_description,

    -- ** IpRouteInfo
    ipRouteInfo_addedDateTime,
    ipRouteInfo_cidrIp,
    ipRouteInfo_description,
    ipRouteInfo_directoryId,
    ipRouteInfo_ipRouteStatusMsg,
    ipRouteInfo_ipRouteStatusReason,

    -- ** LDAPSSettingInfo
    lDAPSSettingInfo_lDAPSStatus,
    lDAPSSettingInfo_lDAPSStatusReason,
    lDAPSSettingInfo_lastUpdatedDateTime,

    -- ** LogSubscription
    logSubscription_directoryId,
    logSubscription_logGroupName,
    logSubscription_subscriptionCreatedDateTime,

    -- ** OSUpdateSettings
    oSUpdateSettings_oSVersion,

    -- ** OwnerDirectoryDescription
    ownerDirectoryDescription_accountId,
    ownerDirectoryDescription_directoryId,
    ownerDirectoryDescription_dnsIpAddrs,
    ownerDirectoryDescription_radiusSettings,
    ownerDirectoryDescription_radiusStatus,
    ownerDirectoryDescription_vpcSettings,

    -- ** RadiusSettings
    radiusSettings_authenticationProtocol,
    radiusSettings_displayLabel,
    radiusSettings_radiusPort,
    radiusSettings_radiusRetries,
    radiusSettings_radiusServers,
    radiusSettings_radiusTimeout,
    radiusSettings_sharedSecret,
    radiusSettings_useSameUsername,

    -- ** RegionDescription
    regionDescription_desiredNumberOfDomainControllers,
    regionDescription_directoryId,
    regionDescription_lastUpdatedDateTime,
    regionDescription_launchTime,
    regionDescription_regionName,
    regionDescription_regionType,
    regionDescription_status,
    regionDescription_statusLastUpdatedDateTime,
    regionDescription_vpcSettings,

    -- ** RegionsInfo
    regionsInfo_additionalRegions,
    regionsInfo_primaryRegion,

    -- ** SchemaExtensionInfo
    schemaExtensionInfo_description,
    schemaExtensionInfo_directoryId,
    schemaExtensionInfo_endDateTime,
    schemaExtensionInfo_schemaExtensionId,
    schemaExtensionInfo_schemaExtensionStatus,
    schemaExtensionInfo_schemaExtensionStatusReason,
    schemaExtensionInfo_startDateTime,

    -- ** Setting
    setting_name,
    setting_value,

    -- ** SettingEntry
    settingEntry_allowedValues,
    settingEntry_appliedValue,
    settingEntry_lastRequestedDateTime,
    settingEntry_lastUpdatedDateTime,
    settingEntry_name,
    settingEntry_requestDetailedStatus,
    settingEntry_requestStatus,
    settingEntry_requestStatusMessage,
    settingEntry_requestedValue,
    settingEntry_type,

    -- ** ShareTarget
    shareTarget_id,
    shareTarget_type,

    -- ** SharedDirectory
    sharedDirectory_createdDateTime,
    sharedDirectory_lastUpdatedDateTime,
    sharedDirectory_ownerAccountId,
    sharedDirectory_ownerDirectoryId,
    sharedDirectory_shareMethod,
    sharedDirectory_shareNotes,
    sharedDirectory_shareStatus,
    sharedDirectory_sharedAccountId,
    sharedDirectory_sharedDirectoryId,

    -- ** Snapshot
    snapshot_directoryId,
    snapshot_name,
    snapshot_snapshotId,
    snapshot_startTime,
    snapshot_status,
    snapshot_type,

    -- ** SnapshotLimits
    snapshotLimits_manualSnapshotsCurrentCount,
    snapshotLimits_manualSnapshotsLimit,
    snapshotLimits_manualSnapshotsLimitReached,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** Trust
    trust_createdDateTime,
    trust_directoryId,
    trust_lastUpdatedDateTime,
    trust_remoteDomainName,
    trust_selectiveAuth,
    trust_stateLastUpdatedDateTime,
    trust_trustDirection,
    trust_trustId,
    trust_trustState,
    trust_trustStateReason,
    trust_trustType,

    -- ** UnshareTarget
    unshareTarget_id,
    unshareTarget_type,

    -- ** UpdateInfoEntry
    updateInfoEntry_initiatedBy,
    updateInfoEntry_lastUpdatedDateTime,
    updateInfoEntry_newValue,
    updateInfoEntry_previousValue,
    updateInfoEntry_region,
    updateInfoEntry_startTime,
    updateInfoEntry_status,
    updateInfoEntry_statusReason,

    -- ** UpdateValue
    updateValue_oSUpdateSettings,
  )
where

import Amazonka.DirectoryService.AcceptSharedDirectory
import Amazonka.DirectoryService.AddIpRoutes
import Amazonka.DirectoryService.AddRegion
import Amazonka.DirectoryService.AddTagsToResource
import Amazonka.DirectoryService.CancelSchemaExtension
import Amazonka.DirectoryService.ConnectDirectory
import Amazonka.DirectoryService.CreateAlias
import Amazonka.DirectoryService.CreateComputer
import Amazonka.DirectoryService.CreateConditionalForwarder
import Amazonka.DirectoryService.CreateDirectory
import Amazonka.DirectoryService.CreateLogSubscription
import Amazonka.DirectoryService.CreateMicrosoftAD
import Amazonka.DirectoryService.CreateSnapshot
import Amazonka.DirectoryService.CreateTrust
import Amazonka.DirectoryService.DeleteConditionalForwarder
import Amazonka.DirectoryService.DeleteDirectory
import Amazonka.DirectoryService.DeleteLogSubscription
import Amazonka.DirectoryService.DeleteSnapshot
import Amazonka.DirectoryService.DeleteTrust
import Amazonka.DirectoryService.DeregisterCertificate
import Amazonka.DirectoryService.DeregisterEventTopic
import Amazonka.DirectoryService.DescribeCertificate
import Amazonka.DirectoryService.DescribeClientAuthenticationSettings
import Amazonka.DirectoryService.DescribeConditionalForwarders
import Amazonka.DirectoryService.DescribeDirectories
import Amazonka.DirectoryService.DescribeDomainControllers
import Amazonka.DirectoryService.DescribeEventTopics
import Amazonka.DirectoryService.DescribeLDAPSSettings
import Amazonka.DirectoryService.DescribeRegions
import Amazonka.DirectoryService.DescribeSettings
import Amazonka.DirectoryService.DescribeSharedDirectories
import Amazonka.DirectoryService.DescribeSnapshots
import Amazonka.DirectoryService.DescribeTrusts
import Amazonka.DirectoryService.DescribeUpdateDirectory
import Amazonka.DirectoryService.DisableClientAuthentication
import Amazonka.DirectoryService.DisableLDAPS
import Amazonka.DirectoryService.DisableRadius
import Amazonka.DirectoryService.DisableSso
import Amazonka.DirectoryService.EnableClientAuthentication
import Amazonka.DirectoryService.EnableLDAPS
import Amazonka.DirectoryService.EnableRadius
import Amazonka.DirectoryService.EnableSso
import Amazonka.DirectoryService.GetDirectoryLimits
import Amazonka.DirectoryService.GetSnapshotLimits
import Amazonka.DirectoryService.ListCertificates
import Amazonka.DirectoryService.ListIpRoutes
import Amazonka.DirectoryService.ListLogSubscriptions
import Amazonka.DirectoryService.ListSchemaExtensions
import Amazonka.DirectoryService.ListTagsForResource
import Amazonka.DirectoryService.RegisterCertificate
import Amazonka.DirectoryService.RegisterEventTopic
import Amazonka.DirectoryService.RejectSharedDirectory
import Amazonka.DirectoryService.RemoveIpRoutes
import Amazonka.DirectoryService.RemoveRegion
import Amazonka.DirectoryService.RemoveTagsFromResource
import Amazonka.DirectoryService.ResetUserPassword
import Amazonka.DirectoryService.RestoreFromSnapshot
import Amazonka.DirectoryService.ShareDirectory
import Amazonka.DirectoryService.StartSchemaExtension
import Amazonka.DirectoryService.Types.Attribute
import Amazonka.DirectoryService.Types.Certificate
import Amazonka.DirectoryService.Types.CertificateInfo
import Amazonka.DirectoryService.Types.ClientAuthenticationSettingInfo
import Amazonka.DirectoryService.Types.ClientCertAuthSettings
import Amazonka.DirectoryService.Types.Computer
import Amazonka.DirectoryService.Types.ConditionalForwarder
import Amazonka.DirectoryService.Types.DirectoryConnectSettings
import Amazonka.DirectoryService.Types.DirectoryConnectSettingsDescription
import Amazonka.DirectoryService.Types.DirectoryDescription
import Amazonka.DirectoryService.Types.DirectoryLimits
import Amazonka.DirectoryService.Types.DirectoryVpcSettings
import Amazonka.DirectoryService.Types.DirectoryVpcSettingsDescription
import Amazonka.DirectoryService.Types.DomainController
import Amazonka.DirectoryService.Types.EventTopic
import Amazonka.DirectoryService.Types.IpRoute
import Amazonka.DirectoryService.Types.IpRouteInfo
import Amazonka.DirectoryService.Types.LDAPSSettingInfo
import Amazonka.DirectoryService.Types.LogSubscription
import Amazonka.DirectoryService.Types.OSUpdateSettings
import Amazonka.DirectoryService.Types.OwnerDirectoryDescription
import Amazonka.DirectoryService.Types.RadiusSettings
import Amazonka.DirectoryService.Types.RegionDescription
import Amazonka.DirectoryService.Types.RegionsInfo
import Amazonka.DirectoryService.Types.SchemaExtensionInfo
import Amazonka.DirectoryService.Types.Setting
import Amazonka.DirectoryService.Types.SettingEntry
import Amazonka.DirectoryService.Types.ShareTarget
import Amazonka.DirectoryService.Types.SharedDirectory
import Amazonka.DirectoryService.Types.Snapshot
import Amazonka.DirectoryService.Types.SnapshotLimits
import Amazonka.DirectoryService.Types.Tag
import Amazonka.DirectoryService.Types.Trust
import Amazonka.DirectoryService.Types.UnshareTarget
import Amazonka.DirectoryService.Types.UpdateInfoEntry
import Amazonka.DirectoryService.Types.UpdateValue
import Amazonka.DirectoryService.UnshareDirectory
import Amazonka.DirectoryService.UpdateConditionalForwarder
import Amazonka.DirectoryService.UpdateDirectorySetup
import Amazonka.DirectoryService.UpdateNumberOfDomainControllers
import Amazonka.DirectoryService.UpdateRadius
import Amazonka.DirectoryService.UpdateSettings
import Amazonka.DirectoryService.UpdateTrust
import Amazonka.DirectoryService.VerifyTrust

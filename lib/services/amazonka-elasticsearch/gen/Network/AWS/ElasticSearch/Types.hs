{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidPaginationTokenException,
    _InvalidTypeException,
    _ResourceAlreadyExistsException,
    _InternalException,
    _BaseException,
    _AccessDeniedException,
    _ValidationException,
    _ConflictException,
    _LimitExceededException,
    _ResourceNotFoundException,
    _DisabledOperationException,

    -- * AutoTuneDesiredState
    AutoTuneDesiredState (..),

    -- * AutoTuneState
    AutoTuneState (..),

    -- * AutoTuneType
    AutoTuneType (..),

    -- * DeploymentStatus
    DeploymentStatus (..),

    -- * DescribePackagesFilterName
    DescribePackagesFilterName (..),

    -- * DomainPackageStatus
    DomainPackageStatus (..),

    -- * ESPartitionInstanceType
    ESPartitionInstanceType (..),

    -- * ESWarmPartitionInstanceType
    ESWarmPartitionInstanceType (..),

    -- * EngineType
    EngineType (..),

    -- * InboundCrossClusterSearchConnectionStatusCode
    InboundCrossClusterSearchConnectionStatusCode (..),

    -- * LogType
    LogType (..),

    -- * OptionState
    OptionState (..),

    -- * OutboundCrossClusterSearchConnectionStatusCode
    OutboundCrossClusterSearchConnectionStatusCode (..),

    -- * PackageStatus
    PackageStatus (..),

    -- * PackageType
    PackageType (..),

    -- * ReservedElasticsearchInstancePaymentOption
    ReservedElasticsearchInstancePaymentOption (..),

    -- * RollbackOnDisable
    RollbackOnDisable (..),

    -- * ScheduledAutoTuneActionType
    ScheduledAutoTuneActionType (..),

    -- * ScheduledAutoTuneSeverityType
    ScheduledAutoTuneSeverityType (..),

    -- * TLSSecurityPolicy
    TLSSecurityPolicy (..),

    -- * TimeUnit
    TimeUnit (..),

    -- * UpgradeStatus
    UpgradeStatus (..),

    -- * UpgradeStep
    UpgradeStep (..),

    -- * VolumeType
    VolumeType (..),

    -- * AccessPoliciesStatus
    AccessPoliciesStatus (..),
    newAccessPoliciesStatus,
    accessPoliciesStatus_options,
    accessPoliciesStatus_status,

    -- * AdditionalLimit
    AdditionalLimit (..),
    newAdditionalLimit,
    additionalLimit_limitValues,
    additionalLimit_limitName,

    -- * AdvancedOptionsStatus
    AdvancedOptionsStatus (..),
    newAdvancedOptionsStatus,
    advancedOptionsStatus_options,
    advancedOptionsStatus_status,

    -- * AdvancedSecurityOptions
    AdvancedSecurityOptions (..),
    newAdvancedSecurityOptions,
    advancedSecurityOptions_internalUserDatabaseEnabled,
    advancedSecurityOptions_sAMLOptions,
    advancedSecurityOptions_enabled,

    -- * AdvancedSecurityOptionsInput
    AdvancedSecurityOptionsInput (..),
    newAdvancedSecurityOptionsInput,
    advancedSecurityOptionsInput_internalUserDatabaseEnabled,
    advancedSecurityOptionsInput_sAMLOptions,
    advancedSecurityOptionsInput_enabled,
    advancedSecurityOptionsInput_masterUserOptions,

    -- * AdvancedSecurityOptionsStatus
    AdvancedSecurityOptionsStatus (..),
    newAdvancedSecurityOptionsStatus,
    advancedSecurityOptionsStatus_options,
    advancedSecurityOptionsStatus_status,

    -- * AutoTune
    AutoTune (..),
    newAutoTune,
    autoTune_autoTuneType,
    autoTune_autoTuneDetails,

    -- * AutoTuneDetails
    AutoTuneDetails (..),
    newAutoTuneDetails,
    autoTuneDetails_scheduledAutoTuneDetails,

    -- * AutoTuneMaintenanceSchedule
    AutoTuneMaintenanceSchedule (..),
    newAutoTuneMaintenanceSchedule,
    autoTuneMaintenanceSchedule_duration,
    autoTuneMaintenanceSchedule_startAt,
    autoTuneMaintenanceSchedule_cronExpressionForRecurrence,

    -- * AutoTuneOptions
    AutoTuneOptions (..),
    newAutoTuneOptions,
    autoTuneOptions_desiredState,
    autoTuneOptions_rollbackOnDisable,
    autoTuneOptions_maintenanceSchedules,

    -- * AutoTuneOptionsInput
    AutoTuneOptionsInput (..),
    newAutoTuneOptionsInput,
    autoTuneOptionsInput_desiredState,
    autoTuneOptionsInput_maintenanceSchedules,

    -- * AutoTuneOptionsOutput
    AutoTuneOptionsOutput (..),
    newAutoTuneOptionsOutput,
    autoTuneOptionsOutput_state,
    autoTuneOptionsOutput_errorMessage,

    -- * AutoTuneOptionsStatus
    AutoTuneOptionsStatus (..),
    newAutoTuneOptionsStatus,
    autoTuneOptionsStatus_status,
    autoTuneOptionsStatus_options,

    -- * AutoTuneStatus
    AutoTuneStatus (..),
    newAutoTuneStatus,
    autoTuneStatus_updateVersion,
    autoTuneStatus_errorMessage,
    autoTuneStatus_pendingDeletion,
    autoTuneStatus_creationDate,
    autoTuneStatus_updateDate,
    autoTuneStatus_state,

    -- * CognitoOptions
    CognitoOptions (..),
    newCognitoOptions,
    cognitoOptions_identityPoolId,
    cognitoOptions_roleArn,
    cognitoOptions_userPoolId,
    cognitoOptions_enabled,

    -- * CognitoOptionsStatus
    CognitoOptionsStatus (..),
    newCognitoOptionsStatus,
    cognitoOptionsStatus_options,
    cognitoOptionsStatus_status,

    -- * ColdStorageOptions
    ColdStorageOptions (..),
    newColdStorageOptions,
    coldStorageOptions_enabled,

    -- * CompatibleVersionsMap
    CompatibleVersionsMap (..),
    newCompatibleVersionsMap,
    compatibleVersionsMap_sourceVersion,
    compatibleVersionsMap_targetVersions,

    -- * DescribePackagesFilter
    DescribePackagesFilter (..),
    newDescribePackagesFilter,
    describePackagesFilter_name,
    describePackagesFilter_value,

    -- * DomainEndpointOptions
    DomainEndpointOptions (..),
    newDomainEndpointOptions,
    domainEndpointOptions_customEndpoint,
    domainEndpointOptions_customEndpointCertificateArn,
    domainEndpointOptions_customEndpointEnabled,
    domainEndpointOptions_tLSSecurityPolicy,
    domainEndpointOptions_enforceHTTPS,

    -- * DomainEndpointOptionsStatus
    DomainEndpointOptionsStatus (..),
    newDomainEndpointOptionsStatus,
    domainEndpointOptionsStatus_options,
    domainEndpointOptionsStatus_status,

    -- * DomainInfo
    DomainInfo (..),
    newDomainInfo,
    domainInfo_engineType,
    domainInfo_domainName,

    -- * DomainInformation
    DomainInformation (..),
    newDomainInformation,
    domainInformation_ownerId,
    domainInformation_region,
    domainInformation_domainName,

    -- * DomainPackageDetails
    DomainPackageDetails (..),
    newDomainPackageDetails,
    domainPackageDetails_domainPackageStatus,
    domainPackageDetails_packageVersion,
    domainPackageDetails_packageName,
    domainPackageDetails_packageID,
    domainPackageDetails_domainName,
    domainPackageDetails_lastUpdated,
    domainPackageDetails_referencePath,
    domainPackageDetails_packageType,
    domainPackageDetails_errorDetails,

    -- * Duration
    Duration (..),
    newDuration,
    duration_unit,
    duration_value,

    -- * EBSOptions
    EBSOptions (..),
    newEBSOptions,
    eBSOptions_eBSEnabled,
    eBSOptions_volumeType,
    eBSOptions_iops,
    eBSOptions_volumeSize,

    -- * EBSOptionsStatus
    EBSOptionsStatus (..),
    newEBSOptionsStatus,
    eBSOptionsStatus_options,
    eBSOptionsStatus_status,

    -- * ElasticsearchClusterConfig
    ElasticsearchClusterConfig (..),
    newElasticsearchClusterConfig,
    elasticsearchClusterConfig_zoneAwarenessConfig,
    elasticsearchClusterConfig_dedicatedMasterCount,
    elasticsearchClusterConfig_warmEnabled,
    elasticsearchClusterConfig_instanceType,
    elasticsearchClusterConfig_zoneAwarenessEnabled,
    elasticsearchClusterConfig_dedicatedMasterEnabled,
    elasticsearchClusterConfig_warmCount,
    elasticsearchClusterConfig_dedicatedMasterType,
    elasticsearchClusterConfig_warmType,
    elasticsearchClusterConfig_coldStorageOptions,
    elasticsearchClusterConfig_instanceCount,

    -- * ElasticsearchClusterConfigStatus
    ElasticsearchClusterConfigStatus (..),
    newElasticsearchClusterConfigStatus,
    elasticsearchClusterConfigStatus_options,
    elasticsearchClusterConfigStatus_status,

    -- * ElasticsearchDomainConfig
    ElasticsearchDomainConfig (..),
    newElasticsearchDomainConfig,
    elasticsearchDomainConfig_eBSOptions,
    elasticsearchDomainConfig_snapshotOptions,
    elasticsearchDomainConfig_elasticsearchClusterConfig,
    elasticsearchDomainConfig_domainEndpointOptions,
    elasticsearchDomainConfig_autoTuneOptions,
    elasticsearchDomainConfig_vPCOptions,
    elasticsearchDomainConfig_accessPolicies,
    elasticsearchDomainConfig_encryptionAtRestOptions,
    elasticsearchDomainConfig_cognitoOptions,
    elasticsearchDomainConfig_nodeToNodeEncryptionOptions,
    elasticsearchDomainConfig_elasticsearchVersion,
    elasticsearchDomainConfig_advancedOptions,
    elasticsearchDomainConfig_advancedSecurityOptions,
    elasticsearchDomainConfig_logPublishingOptions,

    -- * ElasticsearchDomainStatus
    ElasticsearchDomainStatus (..),
    newElasticsearchDomainStatus,
    elasticsearchDomainStatus_eBSOptions,
    elasticsearchDomainStatus_snapshotOptions,
    elasticsearchDomainStatus_domainEndpointOptions,
    elasticsearchDomainStatus_upgradeProcessing,
    elasticsearchDomainStatus_endpoints,
    elasticsearchDomainStatus_autoTuneOptions,
    elasticsearchDomainStatus_vPCOptions,
    elasticsearchDomainStatus_accessPolicies,
    elasticsearchDomainStatus_encryptionAtRestOptions,
    elasticsearchDomainStatus_serviceSoftwareOptions,
    elasticsearchDomainStatus_cognitoOptions,
    elasticsearchDomainStatus_nodeToNodeEncryptionOptions,
    elasticsearchDomainStatus_elasticsearchVersion,
    elasticsearchDomainStatus_advancedOptions,
    elasticsearchDomainStatus_endpoint,
    elasticsearchDomainStatus_processing,
    elasticsearchDomainStatus_created,
    elasticsearchDomainStatus_advancedSecurityOptions,
    elasticsearchDomainStatus_logPublishingOptions,
    elasticsearchDomainStatus_deleted,
    elasticsearchDomainStatus_domainId,
    elasticsearchDomainStatus_domainName,
    elasticsearchDomainStatus_arn,
    elasticsearchDomainStatus_elasticsearchClusterConfig,

    -- * ElasticsearchVersionStatus
    ElasticsearchVersionStatus (..),
    newElasticsearchVersionStatus,
    elasticsearchVersionStatus_options,
    elasticsearchVersionStatus_status,

    -- * EncryptionAtRestOptions
    EncryptionAtRestOptions (..),
    newEncryptionAtRestOptions,
    encryptionAtRestOptions_enabled,
    encryptionAtRestOptions_kmsKeyId,

    -- * EncryptionAtRestOptionsStatus
    EncryptionAtRestOptionsStatus (..),
    newEncryptionAtRestOptionsStatus,
    encryptionAtRestOptionsStatus_options,
    encryptionAtRestOptionsStatus_status,

    -- * ErrorDetails
    ErrorDetails (..),
    newErrorDetails,
    errorDetails_errorType,
    errorDetails_errorMessage,

    -- * Filter
    Filter (..),
    newFilter,
    filter_values,
    filter_name,

    -- * InboundCrossClusterSearchConnection
    InboundCrossClusterSearchConnection (..),
    newInboundCrossClusterSearchConnection,
    inboundCrossClusterSearchConnection_crossClusterSearchConnectionId,
    inboundCrossClusterSearchConnection_sourceDomainInfo,
    inboundCrossClusterSearchConnection_connectionStatus,
    inboundCrossClusterSearchConnection_destinationDomainInfo,

    -- * InboundCrossClusterSearchConnectionStatus
    InboundCrossClusterSearchConnectionStatus (..),
    newInboundCrossClusterSearchConnectionStatus,
    inboundCrossClusterSearchConnectionStatus_message,
    inboundCrossClusterSearchConnectionStatus_statusCode,

    -- * InstanceCountLimits
    InstanceCountLimits (..),
    newInstanceCountLimits,
    instanceCountLimits_maximumInstanceCount,
    instanceCountLimits_minimumInstanceCount,

    -- * InstanceLimits
    InstanceLimits (..),
    newInstanceLimits,
    instanceLimits_instanceCountLimits,

    -- * Limits
    Limits (..),
    newLimits,
    limits_instanceLimits,
    limits_additionalLimits,
    limits_storageTypes,

    -- * LogPublishingOption
    LogPublishingOption (..),
    newLogPublishingOption,
    logPublishingOption_enabled,
    logPublishingOption_cloudWatchLogsLogGroupArn,

    -- * LogPublishingOptionsStatus
    LogPublishingOptionsStatus (..),
    newLogPublishingOptionsStatus,
    logPublishingOptionsStatus_status,
    logPublishingOptionsStatus_options,

    -- * MasterUserOptions
    MasterUserOptions (..),
    newMasterUserOptions,
    masterUserOptions_masterUserPassword,
    masterUserOptions_masterUserName,
    masterUserOptions_masterUserARN,

    -- * NodeToNodeEncryptionOptions
    NodeToNodeEncryptionOptions (..),
    newNodeToNodeEncryptionOptions,
    nodeToNodeEncryptionOptions_enabled,

    -- * NodeToNodeEncryptionOptionsStatus
    NodeToNodeEncryptionOptionsStatus (..),
    newNodeToNodeEncryptionOptionsStatus,
    nodeToNodeEncryptionOptionsStatus_options,
    nodeToNodeEncryptionOptionsStatus_status,

    -- * OptionStatus
    OptionStatus (..),
    newOptionStatus,
    optionStatus_updateVersion,
    optionStatus_pendingDeletion,
    optionStatus_creationDate,
    optionStatus_updateDate,
    optionStatus_state,

    -- * OutboundCrossClusterSearchConnection
    OutboundCrossClusterSearchConnection (..),
    newOutboundCrossClusterSearchConnection,
    outboundCrossClusterSearchConnection_crossClusterSearchConnectionId,
    outboundCrossClusterSearchConnection_sourceDomainInfo,
    outboundCrossClusterSearchConnection_connectionAlias,
    outboundCrossClusterSearchConnection_connectionStatus,
    outboundCrossClusterSearchConnection_destinationDomainInfo,

    -- * OutboundCrossClusterSearchConnectionStatus
    OutboundCrossClusterSearchConnectionStatus (..),
    newOutboundCrossClusterSearchConnectionStatus,
    outboundCrossClusterSearchConnectionStatus_message,
    outboundCrossClusterSearchConnectionStatus_statusCode,

    -- * PackageDetails
    PackageDetails (..),
    newPackageDetails,
    packageDetails_availablePackageVersion,
    packageDetails_packageStatus,
    packageDetails_createdAt,
    packageDetails_packageName,
    packageDetails_packageID,
    packageDetails_packageDescription,
    packageDetails_lastUpdatedAt,
    packageDetails_packageType,
    packageDetails_errorDetails,

    -- * PackageSource
    PackageSource (..),
    newPackageSource,
    packageSource_s3Key,
    packageSource_s3BucketName,

    -- * PackageVersionHistory
    PackageVersionHistory (..),
    newPackageVersionHistory,
    packageVersionHistory_packageVersion,
    packageVersionHistory_createdAt,
    packageVersionHistory_commitMessage,

    -- * RecurringCharge
    RecurringCharge (..),
    newRecurringCharge,
    recurringCharge_recurringChargeFrequency,
    recurringCharge_recurringChargeAmount,

    -- * ReservedElasticsearchInstance
    ReservedElasticsearchInstance (..),
    newReservedElasticsearchInstance,
    reservedElasticsearchInstance_reservationName,
    reservedElasticsearchInstance_paymentOption,
    reservedElasticsearchInstance_elasticsearchInstanceCount,
    reservedElasticsearchInstance_duration,
    reservedElasticsearchInstance_startTime,
    reservedElasticsearchInstance_currencyCode,
    reservedElasticsearchInstance_elasticsearchInstanceType,
    reservedElasticsearchInstance_state,
    reservedElasticsearchInstance_fixedPrice,
    reservedElasticsearchInstance_reservedElasticsearchInstanceId,
    reservedElasticsearchInstance_reservedElasticsearchInstanceOfferingId,
    reservedElasticsearchInstance_usagePrice,
    reservedElasticsearchInstance_recurringCharges,

    -- * ReservedElasticsearchInstanceOffering
    ReservedElasticsearchInstanceOffering (..),
    newReservedElasticsearchInstanceOffering,
    reservedElasticsearchInstanceOffering_paymentOption,
    reservedElasticsearchInstanceOffering_duration,
    reservedElasticsearchInstanceOffering_currencyCode,
    reservedElasticsearchInstanceOffering_elasticsearchInstanceType,
    reservedElasticsearchInstanceOffering_fixedPrice,
    reservedElasticsearchInstanceOffering_reservedElasticsearchInstanceOfferingId,
    reservedElasticsearchInstanceOffering_usagePrice,
    reservedElasticsearchInstanceOffering_recurringCharges,

    -- * SAMLIdp
    SAMLIdp (..),
    newSAMLIdp,
    sAMLIdp_metadataContent,
    sAMLIdp_entityId,

    -- * SAMLOptionsInput
    SAMLOptionsInput (..),
    newSAMLOptionsInput,
    sAMLOptionsInput_masterBackendRole,
    sAMLOptionsInput_rolesKey,
    sAMLOptionsInput_sessionTimeoutMinutes,
    sAMLOptionsInput_enabled,
    sAMLOptionsInput_idp,
    sAMLOptionsInput_masterUserName,
    sAMLOptionsInput_subjectKey,

    -- * SAMLOptionsOutput
    SAMLOptionsOutput (..),
    newSAMLOptionsOutput,
    sAMLOptionsOutput_rolesKey,
    sAMLOptionsOutput_sessionTimeoutMinutes,
    sAMLOptionsOutput_enabled,
    sAMLOptionsOutput_idp,
    sAMLOptionsOutput_subjectKey,

    -- * ScheduledAutoTuneDetails
    ScheduledAutoTuneDetails (..),
    newScheduledAutoTuneDetails,
    scheduledAutoTuneDetails_actionType,
    scheduledAutoTuneDetails_severity,
    scheduledAutoTuneDetails_date,
    scheduledAutoTuneDetails_action,

    -- * ServiceSoftwareOptions
    ServiceSoftwareOptions (..),
    newServiceSoftwareOptions,
    serviceSoftwareOptions_newVersion,
    serviceSoftwareOptions_currentVersion,
    serviceSoftwareOptions_cancellable,
    serviceSoftwareOptions_updateAvailable,
    serviceSoftwareOptions_updateStatus,
    serviceSoftwareOptions_optionalDeployment,
    serviceSoftwareOptions_description,
    serviceSoftwareOptions_automatedUpdateDate,

    -- * SnapshotOptions
    SnapshotOptions (..),
    newSnapshotOptions,
    snapshotOptions_automatedSnapshotStartHour,

    -- * SnapshotOptionsStatus
    SnapshotOptionsStatus (..),
    newSnapshotOptionsStatus,
    snapshotOptionsStatus_options,
    snapshotOptionsStatus_status,

    -- * StorageType
    StorageType (..),
    newStorageType,
    storageType_storageTypeLimits,
    storageType_storageTypeName,
    storageType_storageSubTypeName,

    -- * StorageTypeLimit
    StorageTypeLimit (..),
    newStorageTypeLimit,
    storageTypeLimit_limitValues,
    storageTypeLimit_limitName,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * UpgradeHistory
    UpgradeHistory (..),
    newUpgradeHistory,
    upgradeHistory_upgradeName,
    upgradeHistory_startTimestamp,
    upgradeHistory_upgradeStatus,
    upgradeHistory_stepsList,

    -- * UpgradeStepItem
    UpgradeStepItem (..),
    newUpgradeStepItem,
    upgradeStepItem_upgradeStepStatus,
    upgradeStepItem_progressPercent,
    upgradeStepItem_upgradeStep,
    upgradeStepItem_issues,

    -- * VPCDerivedInfo
    VPCDerivedInfo (..),
    newVPCDerivedInfo,
    vPCDerivedInfo_securityGroupIds,
    vPCDerivedInfo_availabilityZones,
    vPCDerivedInfo_subnetIds,
    vPCDerivedInfo_vPCId,

    -- * VPCDerivedInfoStatus
    VPCDerivedInfoStatus (..),
    newVPCDerivedInfoStatus,
    vPCDerivedInfoStatus_options,
    vPCDerivedInfoStatus_status,

    -- * VPCOptions
    VPCOptions (..),
    newVPCOptions,
    vPCOptions_securityGroupIds,
    vPCOptions_subnetIds,

    -- * ZoneAwarenessConfig
    ZoneAwarenessConfig (..),
    newZoneAwarenessConfig,
    zoneAwarenessConfig_availabilityZoneCount,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types.AccessPoliciesStatus
import Network.AWS.ElasticSearch.Types.AdditionalLimit
import Network.AWS.ElasticSearch.Types.AdvancedOptionsStatus
import Network.AWS.ElasticSearch.Types.AdvancedSecurityOptions
import Network.AWS.ElasticSearch.Types.AdvancedSecurityOptionsInput
import Network.AWS.ElasticSearch.Types.AdvancedSecurityOptionsStatus
import Network.AWS.ElasticSearch.Types.AutoTune
import Network.AWS.ElasticSearch.Types.AutoTuneDesiredState
import Network.AWS.ElasticSearch.Types.AutoTuneDetails
import Network.AWS.ElasticSearch.Types.AutoTuneMaintenanceSchedule
import Network.AWS.ElasticSearch.Types.AutoTuneOptions
import Network.AWS.ElasticSearch.Types.AutoTuneOptionsInput
import Network.AWS.ElasticSearch.Types.AutoTuneOptionsOutput
import Network.AWS.ElasticSearch.Types.AutoTuneOptionsStatus
import Network.AWS.ElasticSearch.Types.AutoTuneState
import Network.AWS.ElasticSearch.Types.AutoTuneStatus
import Network.AWS.ElasticSearch.Types.AutoTuneType
import Network.AWS.ElasticSearch.Types.CognitoOptions
import Network.AWS.ElasticSearch.Types.CognitoOptionsStatus
import Network.AWS.ElasticSearch.Types.ColdStorageOptions
import Network.AWS.ElasticSearch.Types.CompatibleVersionsMap
import Network.AWS.ElasticSearch.Types.DeploymentStatus
import Network.AWS.ElasticSearch.Types.DescribePackagesFilter
import Network.AWS.ElasticSearch.Types.DescribePackagesFilterName
import Network.AWS.ElasticSearch.Types.DomainEndpointOptions
import Network.AWS.ElasticSearch.Types.DomainEndpointOptionsStatus
import Network.AWS.ElasticSearch.Types.DomainInfo
import Network.AWS.ElasticSearch.Types.DomainInformation
import Network.AWS.ElasticSearch.Types.DomainPackageDetails
import Network.AWS.ElasticSearch.Types.DomainPackageStatus
import Network.AWS.ElasticSearch.Types.Duration
import Network.AWS.ElasticSearch.Types.EBSOptions
import Network.AWS.ElasticSearch.Types.EBSOptionsStatus
import Network.AWS.ElasticSearch.Types.ESPartitionInstanceType
import Network.AWS.ElasticSearch.Types.ESWarmPartitionInstanceType
import Network.AWS.ElasticSearch.Types.ElasticsearchClusterConfig
import Network.AWS.ElasticSearch.Types.ElasticsearchClusterConfigStatus
import Network.AWS.ElasticSearch.Types.ElasticsearchDomainConfig
import Network.AWS.ElasticSearch.Types.ElasticsearchDomainStatus
import Network.AWS.ElasticSearch.Types.ElasticsearchVersionStatus
import Network.AWS.ElasticSearch.Types.EncryptionAtRestOptions
import Network.AWS.ElasticSearch.Types.EncryptionAtRestOptionsStatus
import Network.AWS.ElasticSearch.Types.EngineType
import Network.AWS.ElasticSearch.Types.ErrorDetails
import Network.AWS.ElasticSearch.Types.Filter
import Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnection
import Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnectionStatus
import Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnectionStatusCode
import Network.AWS.ElasticSearch.Types.InstanceCountLimits
import Network.AWS.ElasticSearch.Types.InstanceLimits
import Network.AWS.ElasticSearch.Types.Limits
import Network.AWS.ElasticSearch.Types.LogPublishingOption
import Network.AWS.ElasticSearch.Types.LogPublishingOptionsStatus
import Network.AWS.ElasticSearch.Types.LogType
import Network.AWS.ElasticSearch.Types.MasterUserOptions
import Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptions
import Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptionsStatus
import Network.AWS.ElasticSearch.Types.OptionState
import Network.AWS.ElasticSearch.Types.OptionStatus
import Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnection
import Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatus
import Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatusCode
import Network.AWS.ElasticSearch.Types.PackageDetails
import Network.AWS.ElasticSearch.Types.PackageSource
import Network.AWS.ElasticSearch.Types.PackageStatus
import Network.AWS.ElasticSearch.Types.PackageType
import Network.AWS.ElasticSearch.Types.PackageVersionHistory
import Network.AWS.ElasticSearch.Types.RecurringCharge
import Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstance
import Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstanceOffering
import Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstancePaymentOption
import Network.AWS.ElasticSearch.Types.RollbackOnDisable
import Network.AWS.ElasticSearch.Types.SAMLIdp
import Network.AWS.ElasticSearch.Types.SAMLOptionsInput
import Network.AWS.ElasticSearch.Types.SAMLOptionsOutput
import Network.AWS.ElasticSearch.Types.ScheduledAutoTuneActionType
import Network.AWS.ElasticSearch.Types.ScheduledAutoTuneDetails
import Network.AWS.ElasticSearch.Types.ScheduledAutoTuneSeverityType
import Network.AWS.ElasticSearch.Types.ServiceSoftwareOptions
import Network.AWS.ElasticSearch.Types.SnapshotOptions
import Network.AWS.ElasticSearch.Types.SnapshotOptionsStatus
import Network.AWS.ElasticSearch.Types.StorageType
import Network.AWS.ElasticSearch.Types.StorageTypeLimit
import Network.AWS.ElasticSearch.Types.TLSSecurityPolicy
import Network.AWS.ElasticSearch.Types.Tag
import Network.AWS.ElasticSearch.Types.TimeUnit
import Network.AWS.ElasticSearch.Types.UpgradeHistory
import Network.AWS.ElasticSearch.Types.UpgradeStatus
import Network.AWS.ElasticSearch.Types.UpgradeStep
import Network.AWS.ElasticSearch.Types.UpgradeStepItem
import Network.AWS.ElasticSearch.Types.VPCDerivedInfo
import Network.AWS.ElasticSearch.Types.VPCDerivedInfoStatus
import Network.AWS.ElasticSearch.Types.VPCOptions
import Network.AWS.ElasticSearch.Types.VolumeType
import Network.AWS.ElasticSearch.Types.ZoneAwarenessConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-01-01@ of the Amazon Elasticsearch Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "ElasticSearch",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "es",
      Core._serviceSigningName = "es",
      Core._serviceVersion = "2015-01-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "ElasticSearch",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The request processing has failed because of invalid pagination token
-- provided by customer. Returns an HTTP status code of 400.
_InvalidPaginationTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPaginationTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidPaginationTokenException"
    Prelude.. Core.hasStatus 400

-- | An exception for trying to create or access sub-resource that is either
-- invalid or not supported. Gives http status code of 409.
_InvalidTypeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTypeException =
  Core._MatchServiceError
    defaultService
    "InvalidTypeException"
    Prelude.. Core.hasStatus 409

-- | An exception for creating a resource that already exists. Gives http
-- status code of 400.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"
    Prelude.. Core.hasStatus 409

-- | The request processing has failed because of an unknown error, exception
-- or failure (the failure is internal to the service) . Gives http status
-- code of 500.
_InternalException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalException =
  Core._MatchServiceError
    defaultService
    "InternalException"
    Prelude.. Core.hasStatus 500

-- | An error occurred while processing the request.
_BaseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BaseException =
  Core._MatchServiceError
    defaultService
    "BaseException"

-- | An error occurred because user does not have permissions to access the
-- resource. Returns HTTP status code 403.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | An exception for missing \/ invalid input fields. Gives http status code
-- of 400.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | An error occurred because the client attempts to remove a resource that
-- is currently in use. Returns HTTP status code 409.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | An exception for trying to create more than allowed resources or
-- sub-resources. Gives http status code of 409.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 409

-- | An exception for accessing or deleting a resource that does not exist.
-- Gives http status code of 400.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 409

-- | An error occured because the client wanted to access a not supported
-- operation. Gives http status code of 409.
_DisabledOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DisabledOperationException =
  Core._MatchServiceError
    defaultService
    "DisabledOperationException"
    Prelude.. Core.hasStatus 409

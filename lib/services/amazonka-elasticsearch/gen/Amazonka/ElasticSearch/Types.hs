{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ElasticSearch.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _BaseException,
    _ConflictException,
    _DisabledOperationException,
    _InternalException,
    _InvalidPaginationTokenException,
    _InvalidTypeException,
    _LimitExceededException,
    _ResourceAlreadyExistsException,
    _ResourceNotFoundException,
    _ValidationException,

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

    -- * OverallChangeStatus
    OverallChangeStatus (..),

    -- * PackageStatus
    PackageStatus (..),

    -- * PackageType
    PackageType (..),

    -- * PrincipalType
    PrincipalType (..),

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

    -- * VpcEndpointErrorCode
    VpcEndpointErrorCode (..),

    -- * VpcEndpointStatus
    VpcEndpointStatus (..),

    -- * AccessPoliciesStatus
    AccessPoliciesStatus (..),
    newAccessPoliciesStatus,
    accessPoliciesStatus_options,
    accessPoliciesStatus_status,

    -- * AdditionalLimit
    AdditionalLimit (..),
    newAdditionalLimit,
    additionalLimit_limitName,
    additionalLimit_limitValues,

    -- * AdvancedOptionsStatus
    AdvancedOptionsStatus (..),
    newAdvancedOptionsStatus,
    advancedOptionsStatus_options,
    advancedOptionsStatus_status,

    -- * AdvancedSecurityOptions
    AdvancedSecurityOptions (..),
    newAdvancedSecurityOptions,
    advancedSecurityOptions_anonymousAuthDisableDate,
    advancedSecurityOptions_anonymousAuthEnabled,
    advancedSecurityOptions_enabled,
    advancedSecurityOptions_internalUserDatabaseEnabled,
    advancedSecurityOptions_sAMLOptions,

    -- * AdvancedSecurityOptionsInput
    AdvancedSecurityOptionsInput (..),
    newAdvancedSecurityOptionsInput,
    advancedSecurityOptionsInput_anonymousAuthEnabled,
    advancedSecurityOptionsInput_enabled,
    advancedSecurityOptionsInput_internalUserDatabaseEnabled,
    advancedSecurityOptionsInput_masterUserOptions,
    advancedSecurityOptionsInput_sAMLOptions,

    -- * AdvancedSecurityOptionsStatus
    AdvancedSecurityOptionsStatus (..),
    newAdvancedSecurityOptionsStatus,
    advancedSecurityOptionsStatus_options,
    advancedSecurityOptionsStatus_status,

    -- * AuthorizedPrincipal
    AuthorizedPrincipal (..),
    newAuthorizedPrincipal,
    authorizedPrincipal_principal,
    authorizedPrincipal_principalType,

    -- * AutoTune
    AutoTune (..),
    newAutoTune,
    autoTune_autoTuneDetails,
    autoTune_autoTuneType,

    -- * AutoTuneDetails
    AutoTuneDetails (..),
    newAutoTuneDetails,
    autoTuneDetails_scheduledAutoTuneDetails,

    -- * AutoTuneMaintenanceSchedule
    AutoTuneMaintenanceSchedule (..),
    newAutoTuneMaintenanceSchedule,
    autoTuneMaintenanceSchedule_cronExpressionForRecurrence,
    autoTuneMaintenanceSchedule_duration,
    autoTuneMaintenanceSchedule_startAt,

    -- * AutoTuneOptions
    AutoTuneOptions (..),
    newAutoTuneOptions,
    autoTuneOptions_desiredState,
    autoTuneOptions_maintenanceSchedules,
    autoTuneOptions_rollbackOnDisable,

    -- * AutoTuneOptionsInput
    AutoTuneOptionsInput (..),
    newAutoTuneOptionsInput,
    autoTuneOptionsInput_desiredState,
    autoTuneOptionsInput_maintenanceSchedules,

    -- * AutoTuneOptionsOutput
    AutoTuneOptionsOutput (..),
    newAutoTuneOptionsOutput,
    autoTuneOptionsOutput_errorMessage,
    autoTuneOptionsOutput_state,

    -- * AutoTuneOptionsStatus
    AutoTuneOptionsStatus (..),
    newAutoTuneOptionsStatus,
    autoTuneOptionsStatus_options,
    autoTuneOptionsStatus_status,

    -- * AutoTuneStatus
    AutoTuneStatus (..),
    newAutoTuneStatus,
    autoTuneStatus_errorMessage,
    autoTuneStatus_pendingDeletion,
    autoTuneStatus_updateVersion,
    autoTuneStatus_creationDate,
    autoTuneStatus_updateDate,
    autoTuneStatus_state,

    -- * ChangeProgressDetails
    ChangeProgressDetails (..),
    newChangeProgressDetails,
    changeProgressDetails_changeId,
    changeProgressDetails_message,

    -- * ChangeProgressStage
    ChangeProgressStage (..),
    newChangeProgressStage,
    changeProgressStage_description,
    changeProgressStage_lastUpdated,
    changeProgressStage_name,
    changeProgressStage_status,

    -- * ChangeProgressStatusDetails
    ChangeProgressStatusDetails (..),
    newChangeProgressStatusDetails,
    changeProgressStatusDetails_changeId,
    changeProgressStatusDetails_changeProgressStages,
    changeProgressStatusDetails_completedProperties,
    changeProgressStatusDetails_pendingProperties,
    changeProgressStatusDetails_startTime,
    changeProgressStatusDetails_status,
    changeProgressStatusDetails_totalNumberOfStages,

    -- * CognitoOptions
    CognitoOptions (..),
    newCognitoOptions,
    cognitoOptions_enabled,
    cognitoOptions_identityPoolId,
    cognitoOptions_roleArn,
    cognitoOptions_userPoolId,

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
    domainEndpointOptions_enforceHTTPS,
    domainEndpointOptions_tLSSecurityPolicy,

    -- * DomainEndpointOptionsStatus
    DomainEndpointOptionsStatus (..),
    newDomainEndpointOptionsStatus,
    domainEndpointOptionsStatus_options,
    domainEndpointOptionsStatus_status,

    -- * DomainInfo
    DomainInfo (..),
    newDomainInfo,
    domainInfo_domainName,
    domainInfo_engineType,

    -- * DomainInformation
    DomainInformation (..),
    newDomainInformation,
    domainInformation_ownerId,
    domainInformation_region,
    domainInformation_domainName,

    -- * DomainPackageDetails
    DomainPackageDetails (..),
    newDomainPackageDetails,
    domainPackageDetails_domainName,
    domainPackageDetails_domainPackageStatus,
    domainPackageDetails_errorDetails,
    domainPackageDetails_lastUpdated,
    domainPackageDetails_packageID,
    domainPackageDetails_packageName,
    domainPackageDetails_packageType,
    domainPackageDetails_packageVersion,
    domainPackageDetails_referencePath,

    -- * DryRunResults
    DryRunResults (..),
    newDryRunResults,
    dryRunResults_deploymentType,
    dryRunResults_message,

    -- * Duration
    Duration (..),
    newDuration,
    duration_unit,
    duration_value,

    -- * EBSOptions
    EBSOptions (..),
    newEBSOptions,
    eBSOptions_eBSEnabled,
    eBSOptions_iops,
    eBSOptions_throughput,
    eBSOptions_volumeSize,
    eBSOptions_volumeType,

    -- * EBSOptionsStatus
    EBSOptionsStatus (..),
    newEBSOptionsStatus,
    eBSOptionsStatus_options,
    eBSOptionsStatus_status,

    -- * ElasticsearchClusterConfig
    ElasticsearchClusterConfig (..),
    newElasticsearchClusterConfig,
    elasticsearchClusterConfig_coldStorageOptions,
    elasticsearchClusterConfig_dedicatedMasterCount,
    elasticsearchClusterConfig_dedicatedMasterEnabled,
    elasticsearchClusterConfig_dedicatedMasterType,
    elasticsearchClusterConfig_instanceCount,
    elasticsearchClusterConfig_instanceType,
    elasticsearchClusterConfig_warmCount,
    elasticsearchClusterConfig_warmEnabled,
    elasticsearchClusterConfig_warmType,
    elasticsearchClusterConfig_zoneAwarenessConfig,
    elasticsearchClusterConfig_zoneAwarenessEnabled,

    -- * ElasticsearchClusterConfigStatus
    ElasticsearchClusterConfigStatus (..),
    newElasticsearchClusterConfigStatus,
    elasticsearchClusterConfigStatus_options,
    elasticsearchClusterConfigStatus_status,

    -- * ElasticsearchDomainConfig
    ElasticsearchDomainConfig (..),
    newElasticsearchDomainConfig,
    elasticsearchDomainConfig_accessPolicies,
    elasticsearchDomainConfig_advancedOptions,
    elasticsearchDomainConfig_advancedSecurityOptions,
    elasticsearchDomainConfig_autoTuneOptions,
    elasticsearchDomainConfig_changeProgressDetails,
    elasticsearchDomainConfig_cognitoOptions,
    elasticsearchDomainConfig_domainEndpointOptions,
    elasticsearchDomainConfig_eBSOptions,
    elasticsearchDomainConfig_elasticsearchClusterConfig,
    elasticsearchDomainConfig_elasticsearchVersion,
    elasticsearchDomainConfig_encryptionAtRestOptions,
    elasticsearchDomainConfig_logPublishingOptions,
    elasticsearchDomainConfig_nodeToNodeEncryptionOptions,
    elasticsearchDomainConfig_snapshotOptions,
    elasticsearchDomainConfig_vPCOptions,

    -- * ElasticsearchDomainStatus
    ElasticsearchDomainStatus (..),
    newElasticsearchDomainStatus,
    elasticsearchDomainStatus_accessPolicies,
    elasticsearchDomainStatus_advancedOptions,
    elasticsearchDomainStatus_advancedSecurityOptions,
    elasticsearchDomainStatus_autoTuneOptions,
    elasticsearchDomainStatus_changeProgressDetails,
    elasticsearchDomainStatus_cognitoOptions,
    elasticsearchDomainStatus_created,
    elasticsearchDomainStatus_deleted,
    elasticsearchDomainStatus_domainEndpointOptions,
    elasticsearchDomainStatus_eBSOptions,
    elasticsearchDomainStatus_elasticsearchVersion,
    elasticsearchDomainStatus_encryptionAtRestOptions,
    elasticsearchDomainStatus_endpoint,
    elasticsearchDomainStatus_endpoints,
    elasticsearchDomainStatus_logPublishingOptions,
    elasticsearchDomainStatus_nodeToNodeEncryptionOptions,
    elasticsearchDomainStatus_processing,
    elasticsearchDomainStatus_serviceSoftwareOptions,
    elasticsearchDomainStatus_snapshotOptions,
    elasticsearchDomainStatus_upgradeProcessing,
    elasticsearchDomainStatus_vPCOptions,
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
    errorDetails_errorMessage,
    errorDetails_errorType,

    -- * Filter
    Filter (..),
    newFilter,
    filter_name,
    filter_values,

    -- * InboundCrossClusterSearchConnection
    InboundCrossClusterSearchConnection (..),
    newInboundCrossClusterSearchConnection,
    inboundCrossClusterSearchConnection_connectionStatus,
    inboundCrossClusterSearchConnection_crossClusterSearchConnectionId,
    inboundCrossClusterSearchConnection_destinationDomainInfo,
    inboundCrossClusterSearchConnection_sourceDomainInfo,

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
    limits_additionalLimits,
    limits_instanceLimits,
    limits_storageTypes,

    -- * LogPublishingOption
    LogPublishingOption (..),
    newLogPublishingOption,
    logPublishingOption_cloudWatchLogsLogGroupArn,
    logPublishingOption_enabled,

    -- * LogPublishingOptionsStatus
    LogPublishingOptionsStatus (..),
    newLogPublishingOptionsStatus,
    logPublishingOptionsStatus_options,
    logPublishingOptionsStatus_status,

    -- * MasterUserOptions
    MasterUserOptions (..),
    newMasterUserOptions,
    masterUserOptions_masterUserARN,
    masterUserOptions_masterUserName,
    masterUserOptions_masterUserPassword,

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
    optionStatus_pendingDeletion,
    optionStatus_updateVersion,
    optionStatus_creationDate,
    optionStatus_updateDate,
    optionStatus_state,

    -- * OutboundCrossClusterSearchConnection
    OutboundCrossClusterSearchConnection (..),
    newOutboundCrossClusterSearchConnection,
    outboundCrossClusterSearchConnection_connectionAlias,
    outboundCrossClusterSearchConnection_connectionStatus,
    outboundCrossClusterSearchConnection_crossClusterSearchConnectionId,
    outboundCrossClusterSearchConnection_destinationDomainInfo,
    outboundCrossClusterSearchConnection_sourceDomainInfo,

    -- * OutboundCrossClusterSearchConnectionStatus
    OutboundCrossClusterSearchConnectionStatus (..),
    newOutboundCrossClusterSearchConnectionStatus,
    outboundCrossClusterSearchConnectionStatus_message,
    outboundCrossClusterSearchConnectionStatus_statusCode,

    -- * PackageDetails
    PackageDetails (..),
    newPackageDetails,
    packageDetails_availablePackageVersion,
    packageDetails_createdAt,
    packageDetails_errorDetails,
    packageDetails_lastUpdatedAt,
    packageDetails_packageDescription,
    packageDetails_packageID,
    packageDetails_packageName,
    packageDetails_packageStatus,
    packageDetails_packageType,

    -- * PackageSource
    PackageSource (..),
    newPackageSource,
    packageSource_s3BucketName,
    packageSource_s3Key,

    -- * PackageVersionHistory
    PackageVersionHistory (..),
    newPackageVersionHistory,
    packageVersionHistory_commitMessage,
    packageVersionHistory_createdAt,
    packageVersionHistory_packageVersion,

    -- * RecurringCharge
    RecurringCharge (..),
    newRecurringCharge,
    recurringCharge_recurringChargeAmount,
    recurringCharge_recurringChargeFrequency,

    -- * ReservedElasticsearchInstance
    ReservedElasticsearchInstance (..),
    newReservedElasticsearchInstance,
    reservedElasticsearchInstance_currencyCode,
    reservedElasticsearchInstance_duration,
    reservedElasticsearchInstance_elasticsearchInstanceCount,
    reservedElasticsearchInstance_elasticsearchInstanceType,
    reservedElasticsearchInstance_fixedPrice,
    reservedElasticsearchInstance_paymentOption,
    reservedElasticsearchInstance_recurringCharges,
    reservedElasticsearchInstance_reservationName,
    reservedElasticsearchInstance_reservedElasticsearchInstanceId,
    reservedElasticsearchInstance_reservedElasticsearchInstanceOfferingId,
    reservedElasticsearchInstance_startTime,
    reservedElasticsearchInstance_state,
    reservedElasticsearchInstance_usagePrice,

    -- * ReservedElasticsearchInstanceOffering
    ReservedElasticsearchInstanceOffering (..),
    newReservedElasticsearchInstanceOffering,
    reservedElasticsearchInstanceOffering_currencyCode,
    reservedElasticsearchInstanceOffering_duration,
    reservedElasticsearchInstanceOffering_elasticsearchInstanceType,
    reservedElasticsearchInstanceOffering_fixedPrice,
    reservedElasticsearchInstanceOffering_paymentOption,
    reservedElasticsearchInstanceOffering_recurringCharges,
    reservedElasticsearchInstanceOffering_reservedElasticsearchInstanceOfferingId,
    reservedElasticsearchInstanceOffering_usagePrice,

    -- * SAMLIdp
    SAMLIdp (..),
    newSAMLIdp,
    sAMLIdp_metadataContent,
    sAMLIdp_entityId,

    -- * SAMLOptionsInput
    SAMLOptionsInput (..),
    newSAMLOptionsInput,
    sAMLOptionsInput_enabled,
    sAMLOptionsInput_idp,
    sAMLOptionsInput_masterBackendRole,
    sAMLOptionsInput_masterUserName,
    sAMLOptionsInput_rolesKey,
    sAMLOptionsInput_sessionTimeoutMinutes,
    sAMLOptionsInput_subjectKey,

    -- * SAMLOptionsOutput
    SAMLOptionsOutput (..),
    newSAMLOptionsOutput,
    sAMLOptionsOutput_enabled,
    sAMLOptionsOutput_idp,
    sAMLOptionsOutput_rolesKey,
    sAMLOptionsOutput_sessionTimeoutMinutes,
    sAMLOptionsOutput_subjectKey,

    -- * ScheduledAutoTuneDetails
    ScheduledAutoTuneDetails (..),
    newScheduledAutoTuneDetails,
    scheduledAutoTuneDetails_action,
    scheduledAutoTuneDetails_actionType,
    scheduledAutoTuneDetails_date,
    scheduledAutoTuneDetails_severity,

    -- * ServiceSoftwareOptions
    ServiceSoftwareOptions (..),
    newServiceSoftwareOptions,
    serviceSoftwareOptions_automatedUpdateDate,
    serviceSoftwareOptions_cancellable,
    serviceSoftwareOptions_currentVersion,
    serviceSoftwareOptions_description,
    serviceSoftwareOptions_newVersion,
    serviceSoftwareOptions_optionalDeployment,
    serviceSoftwareOptions_updateAvailable,
    serviceSoftwareOptions_updateStatus,

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
    storageType_storageSubTypeName,
    storageType_storageTypeLimits,
    storageType_storageTypeName,

    -- * StorageTypeLimit
    StorageTypeLimit (..),
    newStorageTypeLimit,
    storageTypeLimit_limitName,
    storageTypeLimit_limitValues,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * UpgradeHistory
    UpgradeHistory (..),
    newUpgradeHistory,
    upgradeHistory_startTimestamp,
    upgradeHistory_stepsList,
    upgradeHistory_upgradeName,
    upgradeHistory_upgradeStatus,

    -- * UpgradeStepItem
    UpgradeStepItem (..),
    newUpgradeStepItem,
    upgradeStepItem_issues,
    upgradeStepItem_progressPercent,
    upgradeStepItem_upgradeStep,
    upgradeStepItem_upgradeStepStatus,

    -- * VPCDerivedInfo
    VPCDerivedInfo (..),
    newVPCDerivedInfo,
    vPCDerivedInfo_availabilityZones,
    vPCDerivedInfo_securityGroupIds,
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

    -- * VpcEndpoint
    VpcEndpoint (..),
    newVpcEndpoint,
    vpcEndpoint_domainArn,
    vpcEndpoint_endpoint,
    vpcEndpoint_status,
    vpcEndpoint_vpcEndpointId,
    vpcEndpoint_vpcEndpointOwner,
    vpcEndpoint_vpcOptions,

    -- * VpcEndpointError
    VpcEndpointError (..),
    newVpcEndpointError,
    vpcEndpointError_errorCode,
    vpcEndpointError_errorMessage,
    vpcEndpointError_vpcEndpointId,

    -- * VpcEndpointSummary
    VpcEndpointSummary (..),
    newVpcEndpointSummary,
    vpcEndpointSummary_domainArn,
    vpcEndpointSummary_status,
    vpcEndpointSummary_vpcEndpointId,
    vpcEndpointSummary_vpcEndpointOwner,

    -- * ZoneAwarenessConfig
    ZoneAwarenessConfig (..),
    newZoneAwarenessConfig,
    zoneAwarenessConfig_availabilityZoneCount,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ElasticSearch.Types.AccessPoliciesStatus
import Amazonka.ElasticSearch.Types.AdditionalLimit
import Amazonka.ElasticSearch.Types.AdvancedOptionsStatus
import Amazonka.ElasticSearch.Types.AdvancedSecurityOptions
import Amazonka.ElasticSearch.Types.AdvancedSecurityOptionsInput
import Amazonka.ElasticSearch.Types.AdvancedSecurityOptionsStatus
import Amazonka.ElasticSearch.Types.AuthorizedPrincipal
import Amazonka.ElasticSearch.Types.AutoTune
import Amazonka.ElasticSearch.Types.AutoTuneDesiredState
import Amazonka.ElasticSearch.Types.AutoTuneDetails
import Amazonka.ElasticSearch.Types.AutoTuneMaintenanceSchedule
import Amazonka.ElasticSearch.Types.AutoTuneOptions
import Amazonka.ElasticSearch.Types.AutoTuneOptionsInput
import Amazonka.ElasticSearch.Types.AutoTuneOptionsOutput
import Amazonka.ElasticSearch.Types.AutoTuneOptionsStatus
import Amazonka.ElasticSearch.Types.AutoTuneState
import Amazonka.ElasticSearch.Types.AutoTuneStatus
import Amazonka.ElasticSearch.Types.AutoTuneType
import Amazonka.ElasticSearch.Types.ChangeProgressDetails
import Amazonka.ElasticSearch.Types.ChangeProgressStage
import Amazonka.ElasticSearch.Types.ChangeProgressStatusDetails
import Amazonka.ElasticSearch.Types.CognitoOptions
import Amazonka.ElasticSearch.Types.CognitoOptionsStatus
import Amazonka.ElasticSearch.Types.ColdStorageOptions
import Amazonka.ElasticSearch.Types.CompatibleVersionsMap
import Amazonka.ElasticSearch.Types.DeploymentStatus
import Amazonka.ElasticSearch.Types.DescribePackagesFilter
import Amazonka.ElasticSearch.Types.DescribePackagesFilterName
import Amazonka.ElasticSearch.Types.DomainEndpointOptions
import Amazonka.ElasticSearch.Types.DomainEndpointOptionsStatus
import Amazonka.ElasticSearch.Types.DomainInfo
import Amazonka.ElasticSearch.Types.DomainInformation
import Amazonka.ElasticSearch.Types.DomainPackageDetails
import Amazonka.ElasticSearch.Types.DomainPackageStatus
import Amazonka.ElasticSearch.Types.DryRunResults
import Amazonka.ElasticSearch.Types.Duration
import Amazonka.ElasticSearch.Types.EBSOptions
import Amazonka.ElasticSearch.Types.EBSOptionsStatus
import Amazonka.ElasticSearch.Types.ESPartitionInstanceType
import Amazonka.ElasticSearch.Types.ESWarmPartitionInstanceType
import Amazonka.ElasticSearch.Types.ElasticsearchClusterConfig
import Amazonka.ElasticSearch.Types.ElasticsearchClusterConfigStatus
import Amazonka.ElasticSearch.Types.ElasticsearchDomainConfig
import Amazonka.ElasticSearch.Types.ElasticsearchDomainStatus
import Amazonka.ElasticSearch.Types.ElasticsearchVersionStatus
import Amazonka.ElasticSearch.Types.EncryptionAtRestOptions
import Amazonka.ElasticSearch.Types.EncryptionAtRestOptionsStatus
import Amazonka.ElasticSearch.Types.EngineType
import Amazonka.ElasticSearch.Types.ErrorDetails
import Amazonka.ElasticSearch.Types.Filter
import Amazonka.ElasticSearch.Types.InboundCrossClusterSearchConnection
import Amazonka.ElasticSearch.Types.InboundCrossClusterSearchConnectionStatus
import Amazonka.ElasticSearch.Types.InboundCrossClusterSearchConnectionStatusCode
import Amazonka.ElasticSearch.Types.InstanceCountLimits
import Amazonka.ElasticSearch.Types.InstanceLimits
import Amazonka.ElasticSearch.Types.Limits
import Amazonka.ElasticSearch.Types.LogPublishingOption
import Amazonka.ElasticSearch.Types.LogPublishingOptionsStatus
import Amazonka.ElasticSearch.Types.LogType
import Amazonka.ElasticSearch.Types.MasterUserOptions
import Amazonka.ElasticSearch.Types.NodeToNodeEncryptionOptions
import Amazonka.ElasticSearch.Types.NodeToNodeEncryptionOptionsStatus
import Amazonka.ElasticSearch.Types.OptionState
import Amazonka.ElasticSearch.Types.OptionStatus
import Amazonka.ElasticSearch.Types.OutboundCrossClusterSearchConnection
import Amazonka.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatus
import Amazonka.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatusCode
import Amazonka.ElasticSearch.Types.OverallChangeStatus
import Amazonka.ElasticSearch.Types.PackageDetails
import Amazonka.ElasticSearch.Types.PackageSource
import Amazonka.ElasticSearch.Types.PackageStatus
import Amazonka.ElasticSearch.Types.PackageType
import Amazonka.ElasticSearch.Types.PackageVersionHistory
import Amazonka.ElasticSearch.Types.PrincipalType
import Amazonka.ElasticSearch.Types.RecurringCharge
import Amazonka.ElasticSearch.Types.ReservedElasticsearchInstance
import Amazonka.ElasticSearch.Types.ReservedElasticsearchInstanceOffering
import Amazonka.ElasticSearch.Types.ReservedElasticsearchInstancePaymentOption
import Amazonka.ElasticSearch.Types.RollbackOnDisable
import Amazonka.ElasticSearch.Types.SAMLIdp
import Amazonka.ElasticSearch.Types.SAMLOptionsInput
import Amazonka.ElasticSearch.Types.SAMLOptionsOutput
import Amazonka.ElasticSearch.Types.ScheduledAutoTuneActionType
import Amazonka.ElasticSearch.Types.ScheduledAutoTuneDetails
import Amazonka.ElasticSearch.Types.ScheduledAutoTuneSeverityType
import Amazonka.ElasticSearch.Types.ServiceSoftwareOptions
import Amazonka.ElasticSearch.Types.SnapshotOptions
import Amazonka.ElasticSearch.Types.SnapshotOptionsStatus
import Amazonka.ElasticSearch.Types.StorageType
import Amazonka.ElasticSearch.Types.StorageTypeLimit
import Amazonka.ElasticSearch.Types.TLSSecurityPolicy
import Amazonka.ElasticSearch.Types.Tag
import Amazonka.ElasticSearch.Types.TimeUnit
import Amazonka.ElasticSearch.Types.UpgradeHistory
import Amazonka.ElasticSearch.Types.UpgradeStatus
import Amazonka.ElasticSearch.Types.UpgradeStep
import Amazonka.ElasticSearch.Types.UpgradeStepItem
import Amazonka.ElasticSearch.Types.VPCDerivedInfo
import Amazonka.ElasticSearch.Types.VPCDerivedInfoStatus
import Amazonka.ElasticSearch.Types.VPCOptions
import Amazonka.ElasticSearch.Types.VolumeType
import Amazonka.ElasticSearch.Types.VpcEndpoint
import Amazonka.ElasticSearch.Types.VpcEndpointError
import Amazonka.ElasticSearch.Types.VpcEndpointErrorCode
import Amazonka.ElasticSearch.Types.VpcEndpointStatus
import Amazonka.ElasticSearch.Types.VpcEndpointSummary
import Amazonka.ElasticSearch.Types.ZoneAwarenessConfig
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2015-01-01@ of the Amazon Elasticsearch Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "ElasticSearch",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "es",
      Core.signingName = "es",
      Core.version = "2015-01-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "ElasticSearch",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | An error occurred because user does not have permissions to access the
-- resource. Returns HTTP status code 403.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | An error occurred while processing the request.
_BaseException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_BaseException =
  Core._MatchServiceError
    defaultService
    "BaseException"

-- | An error occurred because the client attempts to remove a resource that
-- is currently in use. Returns HTTP status code 409.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | An error occured because the client wanted to access a not supported
-- operation. Gives http status code of 409.
_DisabledOperationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DisabledOperationException =
  Core._MatchServiceError
    defaultService
    "DisabledOperationException"
    Prelude.. Core.hasStatus 409

-- | The request processing has failed because of an unknown error, exception
-- or failure (the failure is internal to the service) . Gives http status
-- code of 500.
_InternalException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalException =
  Core._MatchServiceError
    defaultService
    "InternalException"
    Prelude.. Core.hasStatus 500

-- | The request processing has failed because of invalid pagination token
-- provided by customer. Returns an HTTP status code of 400.
_InvalidPaginationTokenException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidPaginationTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidPaginationTokenException"
    Prelude.. Core.hasStatus 400

-- | An exception for trying to create or access sub-resource that is either
-- invalid or not supported. Gives http status code of 409.
_InvalidTypeException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidTypeException =
  Core._MatchServiceError
    defaultService
    "InvalidTypeException"
    Prelude.. Core.hasStatus 409

-- | An exception for trying to create more than allowed resources or
-- sub-resources. Gives http status code of 409.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 409

-- | An exception for creating a resource that already exists. Gives http
-- status code of 400.
_ResourceAlreadyExistsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"
    Prelude.. Core.hasStatus 409

-- | An exception for accessing or deleting a resource that does not exist.
-- Gives http status code of 400.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 409

-- | An exception for missing \/ invalid input fields. Gives http status code
-- of 400.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

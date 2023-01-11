{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.OpenSearch.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types
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

    -- * EngineType
    EngineType (..),

    -- * InboundConnectionStatusCode
    InboundConnectionStatusCode (..),

    -- * LogType
    LogType (..),

    -- * OpenSearchPartitionInstanceType
    OpenSearchPartitionInstanceType (..),

    -- * OpenSearchWarmPartitionInstanceType
    OpenSearchWarmPartitionInstanceType (..),

    -- * OptionState
    OptionState (..),

    -- * OutboundConnectionStatusCode
    OutboundConnectionStatusCode (..),

    -- * OverallChangeStatus
    OverallChangeStatus (..),

    -- * PackageStatus
    PackageStatus (..),

    -- * PackageType
    PackageType (..),

    -- * PrincipalType
    PrincipalType (..),

    -- * ReservedInstancePaymentOption
    ReservedInstancePaymentOption (..),

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

    -- * AWSDomainInformation
    AWSDomainInformation (..),
    newAWSDomainInformation,
    aWSDomainInformation_ownerId,
    aWSDomainInformation_region,
    aWSDomainInformation_domainName,

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

    -- * ClusterConfig
    ClusterConfig (..),
    newClusterConfig,
    clusterConfig_coldStorageOptions,
    clusterConfig_dedicatedMasterCount,
    clusterConfig_dedicatedMasterEnabled,
    clusterConfig_dedicatedMasterType,
    clusterConfig_instanceCount,
    clusterConfig_instanceType,
    clusterConfig_warmCount,
    clusterConfig_warmEnabled,
    clusterConfig_warmType,
    clusterConfig_zoneAwarenessConfig,
    clusterConfig_zoneAwarenessEnabled,

    -- * ClusterConfigStatus
    ClusterConfigStatus (..),
    newClusterConfigStatus,
    clusterConfigStatus_options,
    clusterConfigStatus_status,

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

    -- * DomainConfig
    DomainConfig (..),
    newDomainConfig,
    domainConfig_accessPolicies,
    domainConfig_advancedOptions,
    domainConfig_advancedSecurityOptions,
    domainConfig_autoTuneOptions,
    domainConfig_changeProgressDetails,
    domainConfig_clusterConfig,
    domainConfig_cognitoOptions,
    domainConfig_domainEndpointOptions,
    domainConfig_eBSOptions,
    domainConfig_encryptionAtRestOptions,
    domainConfig_engineVersion,
    domainConfig_logPublishingOptions,
    domainConfig_nodeToNodeEncryptionOptions,
    domainConfig_snapshotOptions,
    domainConfig_vPCOptions,

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

    -- * DomainInformationContainer
    DomainInformationContainer (..),
    newDomainInformationContainer,
    domainInformationContainer_aWSDomainInformation,

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

    -- * DomainStatus
    DomainStatus (..),
    newDomainStatus,
    domainStatus_accessPolicies,
    domainStatus_advancedOptions,
    domainStatus_advancedSecurityOptions,
    domainStatus_autoTuneOptions,
    domainStatus_changeProgressDetails,
    domainStatus_cognitoOptions,
    domainStatus_created,
    domainStatus_deleted,
    domainStatus_domainEndpointOptions,
    domainStatus_eBSOptions,
    domainStatus_encryptionAtRestOptions,
    domainStatus_endpoint,
    domainStatus_endpoints,
    domainStatus_engineVersion,
    domainStatus_logPublishingOptions,
    domainStatus_nodeToNodeEncryptionOptions,
    domainStatus_processing,
    domainStatus_serviceSoftwareOptions,
    domainStatus_snapshotOptions,
    domainStatus_upgradeProcessing,
    domainStatus_vPCOptions,
    domainStatus_domainId,
    domainStatus_domainName,
    domainStatus_arn,
    domainStatus_clusterConfig,

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

    -- * InboundConnection
    InboundConnection (..),
    newInboundConnection,
    inboundConnection_connectionId,
    inboundConnection_connectionStatus,
    inboundConnection_localDomainInfo,
    inboundConnection_remoteDomainInfo,

    -- * InboundConnectionStatus
    InboundConnectionStatus (..),
    newInboundConnectionStatus,
    inboundConnectionStatus_message,
    inboundConnectionStatus_statusCode,

    -- * InstanceCountLimits
    InstanceCountLimits (..),
    newInstanceCountLimits,
    instanceCountLimits_maximumInstanceCount,
    instanceCountLimits_minimumInstanceCount,

    -- * InstanceLimits
    InstanceLimits (..),
    newInstanceLimits,
    instanceLimits_instanceCountLimits,

    -- * InstanceTypeDetails
    InstanceTypeDetails (..),
    newInstanceTypeDetails,
    instanceTypeDetails_advancedSecurityEnabled,
    instanceTypeDetails_appLogsEnabled,
    instanceTypeDetails_cognitoEnabled,
    instanceTypeDetails_encryptionEnabled,
    instanceTypeDetails_instanceRole,
    instanceTypeDetails_instanceType,
    instanceTypeDetails_warmEnabled,

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

    -- * OutboundConnection
    OutboundConnection (..),
    newOutboundConnection,
    outboundConnection_connectionAlias,
    outboundConnection_connectionId,
    outboundConnection_connectionStatus,
    outboundConnection_localDomainInfo,
    outboundConnection_remoteDomainInfo,

    -- * OutboundConnectionStatus
    OutboundConnectionStatus (..),
    newOutboundConnectionStatus,
    outboundConnectionStatus_message,
    outboundConnectionStatus_statusCode,

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

    -- * ReservedInstance
    ReservedInstance (..),
    newReservedInstance,
    reservedInstance_billingSubscriptionId,
    reservedInstance_currencyCode,
    reservedInstance_duration,
    reservedInstance_fixedPrice,
    reservedInstance_instanceCount,
    reservedInstance_instanceType,
    reservedInstance_paymentOption,
    reservedInstance_recurringCharges,
    reservedInstance_reservationName,
    reservedInstance_reservedInstanceId,
    reservedInstance_reservedInstanceOfferingId,
    reservedInstance_startTime,
    reservedInstance_state,
    reservedInstance_usagePrice,

    -- * ReservedInstanceOffering
    ReservedInstanceOffering (..),
    newReservedInstanceOffering,
    reservedInstanceOffering_currencyCode,
    reservedInstanceOffering_duration,
    reservedInstanceOffering_fixedPrice,
    reservedInstanceOffering_instanceType,
    reservedInstanceOffering_paymentOption,
    reservedInstanceOffering_recurringCharges,
    reservedInstanceOffering_reservedInstanceOfferingId,
    reservedInstanceOffering_usagePrice,

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

    -- * VersionStatus
    VersionStatus (..),
    newVersionStatus,
    versionStatus_options,
    versionStatus_status,

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
import Amazonka.OpenSearch.Types.AWSDomainInformation
import Amazonka.OpenSearch.Types.AccessPoliciesStatus
import Amazonka.OpenSearch.Types.AdditionalLimit
import Amazonka.OpenSearch.Types.AdvancedOptionsStatus
import Amazonka.OpenSearch.Types.AdvancedSecurityOptions
import Amazonka.OpenSearch.Types.AdvancedSecurityOptionsInput
import Amazonka.OpenSearch.Types.AdvancedSecurityOptionsStatus
import Amazonka.OpenSearch.Types.AuthorizedPrincipal
import Amazonka.OpenSearch.Types.AutoTune
import Amazonka.OpenSearch.Types.AutoTuneDesiredState
import Amazonka.OpenSearch.Types.AutoTuneDetails
import Amazonka.OpenSearch.Types.AutoTuneMaintenanceSchedule
import Amazonka.OpenSearch.Types.AutoTuneOptions
import Amazonka.OpenSearch.Types.AutoTuneOptionsInput
import Amazonka.OpenSearch.Types.AutoTuneOptionsOutput
import Amazonka.OpenSearch.Types.AutoTuneOptionsStatus
import Amazonka.OpenSearch.Types.AutoTuneState
import Amazonka.OpenSearch.Types.AutoTuneStatus
import Amazonka.OpenSearch.Types.AutoTuneType
import Amazonka.OpenSearch.Types.ChangeProgressDetails
import Amazonka.OpenSearch.Types.ChangeProgressStage
import Amazonka.OpenSearch.Types.ChangeProgressStatusDetails
import Amazonka.OpenSearch.Types.ClusterConfig
import Amazonka.OpenSearch.Types.ClusterConfigStatus
import Amazonka.OpenSearch.Types.CognitoOptions
import Amazonka.OpenSearch.Types.CognitoOptionsStatus
import Amazonka.OpenSearch.Types.ColdStorageOptions
import Amazonka.OpenSearch.Types.CompatibleVersionsMap
import Amazonka.OpenSearch.Types.DeploymentStatus
import Amazonka.OpenSearch.Types.DescribePackagesFilter
import Amazonka.OpenSearch.Types.DescribePackagesFilterName
import Amazonka.OpenSearch.Types.DomainConfig
import Amazonka.OpenSearch.Types.DomainEndpointOptions
import Amazonka.OpenSearch.Types.DomainEndpointOptionsStatus
import Amazonka.OpenSearch.Types.DomainInfo
import Amazonka.OpenSearch.Types.DomainInformationContainer
import Amazonka.OpenSearch.Types.DomainPackageDetails
import Amazonka.OpenSearch.Types.DomainPackageStatus
import Amazonka.OpenSearch.Types.DomainStatus
import Amazonka.OpenSearch.Types.DryRunResults
import Amazonka.OpenSearch.Types.Duration
import Amazonka.OpenSearch.Types.EBSOptions
import Amazonka.OpenSearch.Types.EBSOptionsStatus
import Amazonka.OpenSearch.Types.EncryptionAtRestOptions
import Amazonka.OpenSearch.Types.EncryptionAtRestOptionsStatus
import Amazonka.OpenSearch.Types.EngineType
import Amazonka.OpenSearch.Types.ErrorDetails
import Amazonka.OpenSearch.Types.Filter
import Amazonka.OpenSearch.Types.InboundConnection
import Amazonka.OpenSearch.Types.InboundConnectionStatus
import Amazonka.OpenSearch.Types.InboundConnectionStatusCode
import Amazonka.OpenSearch.Types.InstanceCountLimits
import Amazonka.OpenSearch.Types.InstanceLimits
import Amazonka.OpenSearch.Types.InstanceTypeDetails
import Amazonka.OpenSearch.Types.Limits
import Amazonka.OpenSearch.Types.LogPublishingOption
import Amazonka.OpenSearch.Types.LogPublishingOptionsStatus
import Amazonka.OpenSearch.Types.LogType
import Amazonka.OpenSearch.Types.MasterUserOptions
import Amazonka.OpenSearch.Types.NodeToNodeEncryptionOptions
import Amazonka.OpenSearch.Types.NodeToNodeEncryptionOptionsStatus
import Amazonka.OpenSearch.Types.OpenSearchPartitionInstanceType
import Amazonka.OpenSearch.Types.OpenSearchWarmPartitionInstanceType
import Amazonka.OpenSearch.Types.OptionState
import Amazonka.OpenSearch.Types.OptionStatus
import Amazonka.OpenSearch.Types.OutboundConnection
import Amazonka.OpenSearch.Types.OutboundConnectionStatus
import Amazonka.OpenSearch.Types.OutboundConnectionStatusCode
import Amazonka.OpenSearch.Types.OverallChangeStatus
import Amazonka.OpenSearch.Types.PackageDetails
import Amazonka.OpenSearch.Types.PackageSource
import Amazonka.OpenSearch.Types.PackageStatus
import Amazonka.OpenSearch.Types.PackageType
import Amazonka.OpenSearch.Types.PackageVersionHistory
import Amazonka.OpenSearch.Types.PrincipalType
import Amazonka.OpenSearch.Types.RecurringCharge
import Amazonka.OpenSearch.Types.ReservedInstance
import Amazonka.OpenSearch.Types.ReservedInstanceOffering
import Amazonka.OpenSearch.Types.ReservedInstancePaymentOption
import Amazonka.OpenSearch.Types.RollbackOnDisable
import Amazonka.OpenSearch.Types.SAMLIdp
import Amazonka.OpenSearch.Types.SAMLOptionsInput
import Amazonka.OpenSearch.Types.SAMLOptionsOutput
import Amazonka.OpenSearch.Types.ScheduledAutoTuneActionType
import Amazonka.OpenSearch.Types.ScheduledAutoTuneDetails
import Amazonka.OpenSearch.Types.ScheduledAutoTuneSeverityType
import Amazonka.OpenSearch.Types.ServiceSoftwareOptions
import Amazonka.OpenSearch.Types.SnapshotOptions
import Amazonka.OpenSearch.Types.SnapshotOptionsStatus
import Amazonka.OpenSearch.Types.StorageType
import Amazonka.OpenSearch.Types.StorageTypeLimit
import Amazonka.OpenSearch.Types.TLSSecurityPolicy
import Amazonka.OpenSearch.Types.Tag
import Amazonka.OpenSearch.Types.TimeUnit
import Amazonka.OpenSearch.Types.UpgradeHistory
import Amazonka.OpenSearch.Types.UpgradeStatus
import Amazonka.OpenSearch.Types.UpgradeStep
import Amazonka.OpenSearch.Types.UpgradeStepItem
import Amazonka.OpenSearch.Types.VPCDerivedInfo
import Amazonka.OpenSearch.Types.VPCDerivedInfoStatus
import Amazonka.OpenSearch.Types.VPCOptions
import Amazonka.OpenSearch.Types.VersionStatus
import Amazonka.OpenSearch.Types.VolumeType
import Amazonka.OpenSearch.Types.VpcEndpoint
import Amazonka.OpenSearch.Types.VpcEndpointError
import Amazonka.OpenSearch.Types.VpcEndpointErrorCode
import Amazonka.OpenSearch.Types.VpcEndpointStatus
import Amazonka.OpenSearch.Types.VpcEndpointSummary
import Amazonka.OpenSearch.Types.ZoneAwarenessConfig
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2021-01-01@ of the Amazon OpenSearch Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "OpenSearch",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "es",
      Core.signingName = "es",
      Core.version = "2021-01-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "OpenSearch",
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
_AccessDeniedException :: Core.AsError a => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | An error occurred while processing the request.
_BaseException :: Core.AsError a => Lens.Fold a Core.ServiceError
_BaseException =
  Core._MatchServiceError
    defaultService
    "BaseException"

-- | An error occurred because the client attempts to remove a resource that
-- is currently in use. Returns HTTP status code 409.
_ConflictException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | An error occured because the client wanted to access a not supported
-- operation. Gives http status code of 409.
_DisabledOperationException :: Core.AsError a => Lens.Fold a Core.ServiceError
_DisabledOperationException =
  Core._MatchServiceError
    defaultService
    "DisabledOperationException"
    Prelude.. Core.hasStatus 409

-- | The request processing has failed because of an unknown error, exception
-- or failure (the failure is internal to the service) . Gives http status
-- code of 500.
_InternalException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InternalException =
  Core._MatchServiceError
    defaultService
    "InternalException"
    Prelude.. Core.hasStatus 500

-- | The request processing has failed because of invalid pagination token
-- provided by customer. Returns an HTTP status code of 400.
_InvalidPaginationTokenException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidPaginationTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidPaginationTokenException"
    Prelude.. Core.hasStatus 400

-- | An exception for trying to create or access sub-resource that is either
-- invalid or not supported. Gives http status code of 409.
_InvalidTypeException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidTypeException =
  Core._MatchServiceError
    defaultService
    "InvalidTypeException"
    Prelude.. Core.hasStatus 409

-- | An exception for trying to create more than allowed resources or
-- sub-resources. Gives http status code of 409.
_LimitExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 409

-- | An exception for creating a resource that already exists. Gives http
-- status code of 400.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"
    Prelude.. Core.hasStatus 409

-- | An exception for accessing or deleting a resource that does not exist.
-- Gives http status code of 400.
_ResourceNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 409

-- | An exception for missing \/ invalid input fields. Gives http status code
-- of 400.
_ValidationException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

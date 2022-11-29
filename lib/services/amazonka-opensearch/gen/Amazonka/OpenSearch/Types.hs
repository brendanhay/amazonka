{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.OpenSearch.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ResourceAlreadyExistsException,
    _AccessDeniedException,
    _InvalidPaginationTokenException,
    _ResourceNotFoundException,
    _InvalidTypeException,
    _LimitExceededException,
    _DisabledOperationException,
    _ConflictException,
    _InternalException,
    _ValidationException,
    _BaseException,

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
    advancedSecurityOptions_internalUserDatabaseEnabled,
    advancedSecurityOptions_sAMLOptions,
    advancedSecurityOptions_anonymousAuthEnabled,
    advancedSecurityOptions_enabled,
    advancedSecurityOptions_anonymousAuthDisableDate,

    -- * AdvancedSecurityOptionsInput
    AdvancedSecurityOptionsInput (..),
    newAdvancedSecurityOptionsInput,
    advancedSecurityOptionsInput_internalUserDatabaseEnabled,
    advancedSecurityOptionsInput_sAMLOptions,
    advancedSecurityOptionsInput_anonymousAuthEnabled,
    advancedSecurityOptionsInput_enabled,
    advancedSecurityOptionsInput_masterUserOptions,

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
    autoTuneMaintenanceSchedule_cronExpressionForRecurrence,
    autoTuneMaintenanceSchedule_startAt,

    -- * AutoTuneOptions
    AutoTuneOptions (..),
    newAutoTuneOptions,
    autoTuneOptions_maintenanceSchedules,
    autoTuneOptions_desiredState,
    autoTuneOptions_rollbackOnDisable,

    -- * AutoTuneOptionsInput
    AutoTuneOptionsInput (..),
    newAutoTuneOptionsInput,
    autoTuneOptionsInput_maintenanceSchedules,
    autoTuneOptionsInput_desiredState,

    -- * AutoTuneOptionsOutput
    AutoTuneOptionsOutput (..),
    newAutoTuneOptionsOutput,
    autoTuneOptionsOutput_errorMessage,
    autoTuneOptionsOutput_state,

    -- * AutoTuneOptionsStatus
    AutoTuneOptionsStatus (..),
    newAutoTuneOptionsStatus,
    autoTuneOptionsStatus_status,
    autoTuneOptionsStatus_options,

    -- * AutoTuneStatus
    AutoTuneStatus (..),
    newAutoTuneStatus,
    autoTuneStatus_pendingDeletion,
    autoTuneStatus_errorMessage,
    autoTuneStatus_updateVersion,
    autoTuneStatus_creationDate,
    autoTuneStatus_updateDate,
    autoTuneStatus_state,

    -- * ChangeProgressDetails
    ChangeProgressDetails (..),
    newChangeProgressDetails,
    changeProgressDetails_message,
    changeProgressDetails_changeId,

    -- * ChangeProgressStage
    ChangeProgressStage (..),
    newChangeProgressStage,
    changeProgressStage_name,
    changeProgressStage_status,
    changeProgressStage_description,
    changeProgressStage_lastUpdated,

    -- * ChangeProgressStatusDetails
    ChangeProgressStatusDetails (..),
    newChangeProgressStatusDetails,
    changeProgressStatusDetails_totalNumberOfStages,
    changeProgressStatusDetails_changeId,
    changeProgressStatusDetails_pendingProperties,
    changeProgressStatusDetails_status,
    changeProgressStatusDetails_changeProgressStages,
    changeProgressStatusDetails_completedProperties,
    changeProgressStatusDetails_startTime,

    -- * ClusterConfig
    ClusterConfig (..),
    newClusterConfig,
    clusterConfig_warmCount,
    clusterConfig_coldStorageOptions,
    clusterConfig_dedicatedMasterType,
    clusterConfig_zoneAwarenessEnabled,
    clusterConfig_dedicatedMasterEnabled,
    clusterConfig_warmType,
    clusterConfig_instanceType,
    clusterConfig_zoneAwarenessConfig,
    clusterConfig_instanceCount,
    clusterConfig_warmEnabled,
    clusterConfig_dedicatedMasterCount,

    -- * ClusterConfigStatus
    ClusterConfigStatus (..),
    newClusterConfigStatus,
    clusterConfigStatus_options,
    clusterConfigStatus_status,

    -- * CognitoOptions
    CognitoOptions (..),
    newCognitoOptions,
    cognitoOptions_roleArn,
    cognitoOptions_enabled,
    cognitoOptions_identityPoolId,
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
    compatibleVersionsMap_targetVersions,
    compatibleVersionsMap_sourceVersion,

    -- * DescribePackagesFilter
    DescribePackagesFilter (..),
    newDescribePackagesFilter,
    describePackagesFilter_name,
    describePackagesFilter_value,

    -- * DomainConfig
    DomainConfig (..),
    newDomainConfig,
    domainConfig_nodeToNodeEncryptionOptions,
    domainConfig_clusterConfig,
    domainConfig_advancedOptions,
    domainConfig_changeProgressDetails,
    domainConfig_advancedSecurityOptions,
    domainConfig_cognitoOptions,
    domainConfig_encryptionAtRestOptions,
    domainConfig_eBSOptions,
    domainConfig_accessPolicies,
    domainConfig_vPCOptions,
    domainConfig_autoTuneOptions,
    domainConfig_domainEndpointOptions,
    domainConfig_snapshotOptions,
    domainConfig_logPublishingOptions,
    domainConfig_engineVersion,

    -- * DomainEndpointOptions
    DomainEndpointOptions (..),
    newDomainEndpointOptions,
    domainEndpointOptions_customEndpointCertificateArn,
    domainEndpointOptions_tLSSecurityPolicy,
    domainEndpointOptions_customEndpointEnabled,
    domainEndpointOptions_enforceHTTPS,
    domainEndpointOptions_customEndpoint,

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

    -- * DomainInformationContainer
    DomainInformationContainer (..),
    newDomainInformationContainer,
    domainInformationContainer_aWSDomainInformation,

    -- * DomainPackageDetails
    DomainPackageDetails (..),
    newDomainPackageDetails,
    domainPackageDetails_referencePath,
    domainPackageDetails_packageName,
    domainPackageDetails_domainName,
    domainPackageDetails_errorDetails,
    domainPackageDetails_domainPackageStatus,
    domainPackageDetails_packageID,
    domainPackageDetails_lastUpdated,
    domainPackageDetails_packageVersion,
    domainPackageDetails_packageType,

    -- * DomainStatus
    DomainStatus (..),
    newDomainStatus,
    domainStatus_nodeToNodeEncryptionOptions,
    domainStatus_advancedOptions,
    domainStatus_changeProgressDetails,
    domainStatus_deleted,
    domainStatus_created,
    domainStatus_advancedSecurityOptions,
    domainStatus_upgradeProcessing,
    domainStatus_processing,
    domainStatus_cognitoOptions,
    domainStatus_encryptionAtRestOptions,
    domainStatus_endpoints,
    domainStatus_eBSOptions,
    domainStatus_accessPolicies,
    domainStatus_vPCOptions,
    domainStatus_autoTuneOptions,
    domainStatus_domainEndpointOptions,
    domainStatus_endpoint,
    domainStatus_serviceSoftwareOptions,
    domainStatus_snapshotOptions,
    domainStatus_logPublishingOptions,
    domainStatus_engineVersion,
    domainStatus_domainId,
    domainStatus_domainName,
    domainStatus_arn,
    domainStatus_clusterConfig,

    -- * DryRunResults
    DryRunResults (..),
    newDryRunResults,
    dryRunResults_message,
    dryRunResults_deploymentType,

    -- * Duration
    Duration (..),
    newDuration,
    duration_unit,
    duration_value,

    -- * EBSOptions
    EBSOptions (..),
    newEBSOptions,
    eBSOptions_volumeType,
    eBSOptions_volumeSize,
    eBSOptions_throughput,
    eBSOptions_eBSEnabled,
    eBSOptions_iops,

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
    inboundConnection_remoteDomainInfo,
    inboundConnection_connectionId,
    inboundConnection_localDomainInfo,
    inboundConnection_connectionStatus,

    -- * InboundConnectionStatus
    InboundConnectionStatus (..),
    newInboundConnectionStatus,
    inboundConnectionStatus_message,
    inboundConnectionStatus_statusCode,

    -- * InstanceCountLimits
    InstanceCountLimits (..),
    newInstanceCountLimits,
    instanceCountLimits_minimumInstanceCount,
    instanceCountLimits_maximumInstanceCount,

    -- * InstanceLimits
    InstanceLimits (..),
    newInstanceLimits,
    instanceLimits_instanceCountLimits,

    -- * InstanceTypeDetails
    InstanceTypeDetails (..),
    newInstanceTypeDetails,
    instanceTypeDetails_advancedSecurityEnabled,
    instanceTypeDetails_encryptionEnabled,
    instanceTypeDetails_instanceType,
    instanceTypeDetails_cognitoEnabled,
    instanceTypeDetails_instanceRole,
    instanceTypeDetails_appLogsEnabled,
    instanceTypeDetails_warmEnabled,

    -- * Limits
    Limits (..),
    newLimits,
    limits_instanceLimits,
    limits_storageTypes,
    limits_additionalLimits,

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
    outboundConnection_remoteDomainInfo,
    outboundConnection_connectionId,
    outboundConnection_localDomainInfo,
    outboundConnection_connectionStatus,

    -- * OutboundConnectionStatus
    OutboundConnectionStatus (..),
    newOutboundConnectionStatus,
    outboundConnectionStatus_message,
    outboundConnectionStatus_statusCode,

    -- * PackageDetails
    PackageDetails (..),
    newPackageDetails,
    packageDetails_packageDescription,
    packageDetails_packageName,
    packageDetails_lastUpdatedAt,
    packageDetails_errorDetails,
    packageDetails_packageID,
    packageDetails_packageType,
    packageDetails_availablePackageVersion,
    packageDetails_packageStatus,
    packageDetails_createdAt,

    -- * PackageSource
    PackageSource (..),
    newPackageSource,
    packageSource_s3BucketName,
    packageSource_s3Key,

    -- * PackageVersionHistory
    PackageVersionHistory (..),
    newPackageVersionHistory,
    packageVersionHistory_packageVersion,
    packageVersionHistory_commitMessage,
    packageVersionHistory_createdAt,

    -- * RecurringCharge
    RecurringCharge (..),
    newRecurringCharge,
    recurringCharge_recurringChargeAmount,
    recurringCharge_recurringChargeFrequency,

    -- * ReservedInstance
    ReservedInstance (..),
    newReservedInstance,
    reservedInstance_billingSubscriptionId,
    reservedInstance_reservedInstanceId,
    reservedInstance_recurringCharges,
    reservedInstance_state,
    reservedInstance_reservationName,
    reservedInstance_instanceType,
    reservedInstance_duration,
    reservedInstance_currencyCode,
    reservedInstance_instanceCount,
    reservedInstance_fixedPrice,
    reservedInstance_reservedInstanceOfferingId,
    reservedInstance_startTime,
    reservedInstance_paymentOption,
    reservedInstance_usagePrice,

    -- * ReservedInstanceOffering
    ReservedInstanceOffering (..),
    newReservedInstanceOffering,
    reservedInstanceOffering_recurringCharges,
    reservedInstanceOffering_instanceType,
    reservedInstanceOffering_duration,
    reservedInstanceOffering_currencyCode,
    reservedInstanceOffering_fixedPrice,
    reservedInstanceOffering_reservedInstanceOfferingId,
    reservedInstanceOffering_paymentOption,
    reservedInstanceOffering_usagePrice,

    -- * SAMLIdp
    SAMLIdp (..),
    newSAMLIdp,
    sAMLIdp_metadataContent,
    sAMLIdp_entityId,

    -- * SAMLOptionsInput
    SAMLOptionsInput (..),
    newSAMLOptionsInput,
    sAMLOptionsInput_masterUserName,
    sAMLOptionsInput_enabled,
    sAMLOptionsInput_sessionTimeoutMinutes,
    sAMLOptionsInput_rolesKey,
    sAMLOptionsInput_masterBackendRole,
    sAMLOptionsInput_idp,
    sAMLOptionsInput_subjectKey,

    -- * SAMLOptionsOutput
    SAMLOptionsOutput (..),
    newSAMLOptionsOutput,
    sAMLOptionsOutput_enabled,
    sAMLOptionsOutput_sessionTimeoutMinutes,
    sAMLOptionsOutput_rolesKey,
    sAMLOptionsOutput_idp,
    sAMLOptionsOutput_subjectKey,

    -- * ScheduledAutoTuneDetails
    ScheduledAutoTuneDetails (..),
    newScheduledAutoTuneDetails,
    scheduledAutoTuneDetails_severity,
    scheduledAutoTuneDetails_actionType,
    scheduledAutoTuneDetails_date,
    scheduledAutoTuneDetails_action,

    -- * ServiceSoftwareOptions
    ServiceSoftwareOptions (..),
    newServiceSoftwareOptions,
    serviceSoftwareOptions_optionalDeployment,
    serviceSoftwareOptions_newVersion,
    serviceSoftwareOptions_updateAvailable,
    serviceSoftwareOptions_cancellable,
    serviceSoftwareOptions_updateStatus,
    serviceSoftwareOptions_automatedUpdateDate,
    serviceSoftwareOptions_description,
    serviceSoftwareOptions_currentVersion,

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
    upgradeStepItem_upgradeStep,
    upgradeStepItem_issues,
    upgradeStepItem_progressPercent,
    upgradeStepItem_upgradeStepStatus,

    -- * VPCDerivedInfo
    VPCDerivedInfo (..),
    newVPCDerivedInfo,
    vPCDerivedInfo_securityGroupIds,
    vPCDerivedInfo_availabilityZones,
    vPCDerivedInfo_vPCId,
    vPCDerivedInfo_subnetIds,

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
    vpcEndpoint_vpcEndpointOwner,
    vpcEndpoint_domainArn,
    vpcEndpoint_status,
    vpcEndpoint_vpcEndpointId,
    vpcEndpoint_vpcOptions,
    vpcEndpoint_endpoint,

    -- * VpcEndpointError
    VpcEndpointError (..),
    newVpcEndpointError,
    vpcEndpointError_errorMessage,
    vpcEndpointError_vpcEndpointId,
    vpcEndpointError_errorCode,

    -- * VpcEndpointSummary
    VpcEndpointSummary (..),
    newVpcEndpointSummary,
    vpcEndpointSummary_vpcEndpointOwner,
    vpcEndpointSummary_domainArn,
    vpcEndpointSummary_status,
    vpcEndpointSummary_vpcEndpointId,

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
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | An exception for creating a resource that already exists. Gives http
-- status code of 400.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"
    Prelude.. Core.hasStatus 409

-- | An error occurred because user does not have permissions to access the
-- resource. Returns HTTP status code 403.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The request processing has failed because of invalid pagination token
-- provided by customer. Returns an HTTP status code of 400.
_InvalidPaginationTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPaginationTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidPaginationTokenException"
    Prelude.. Core.hasStatus 400

-- | An exception for accessing or deleting a resource that does not exist.
-- Gives http status code of 400.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 409

-- | An exception for trying to create or access sub-resource that is either
-- invalid or not supported. Gives http status code of 409.
_InvalidTypeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTypeException =
  Core._MatchServiceError
    defaultService
    "InvalidTypeException"
    Prelude.. Core.hasStatus 409

-- | An exception for trying to create more than allowed resources or
-- sub-resources. Gives http status code of 409.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 409

-- | An error occured because the client wanted to access a not supported
-- operation. Gives http status code of 409.
_DisabledOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DisabledOperationException =
  Core._MatchServiceError
    defaultService
    "DisabledOperationException"
    Prelude.. Core.hasStatus 409

-- | An error occurred because the client attempts to remove a resource that
-- is currently in use. Returns HTTP status code 409.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
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

-- | An exception for missing \/ invalid input fields. Gives http status code
-- of 400.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | An error occurred while processing the request.
_BaseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BaseException =
  Core._MatchServiceError
    defaultService
    "BaseException"

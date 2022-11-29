{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SecurityHub.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidInputException,
    _InvalidAccessException,
    _AccessDeniedException,
    _ResourceNotFoundException,
    _LimitExceededException,
    _ResourceConflictException,
    _InternalException,

    -- * AdminStatus
    AdminStatus (..),

    -- * AutoEnableStandards
    AutoEnableStandards (..),

    -- * AwsIamAccessKeyStatus
    AwsIamAccessKeyStatus (..),

    -- * AwsS3BucketNotificationConfigurationS3KeyFilterRuleName
    AwsS3BucketNotificationConfigurationS3KeyFilterRuleName (..),

    -- * ComplianceStatus
    ComplianceStatus (..),

    -- * ControlStatus
    ControlStatus (..),

    -- * DateRangeUnit
    DateRangeUnit (..),

    -- * IntegrationType
    IntegrationType (..),

    -- * MalwareState
    MalwareState (..),

    -- * MalwareType
    MalwareType (..),

    -- * MapFilterComparison
    MapFilterComparison (..),

    -- * NetworkDirection
    NetworkDirection (..),

    -- * Partition
    Partition (..),

    -- * RecordState
    RecordState (..),

    -- * SeverityLabel
    SeverityLabel (..),

    -- * SeverityRating
    SeverityRating (..),

    -- * SortOrder
    SortOrder (..),

    -- * StandardsStatus
    StandardsStatus (..),

    -- * StatusReasonCode
    StatusReasonCode (..),

    -- * StringFilterComparison
    StringFilterComparison (..),

    -- * ThreatIntelIndicatorCategory
    ThreatIntelIndicatorCategory (..),

    -- * ThreatIntelIndicatorType
    ThreatIntelIndicatorType (..),

    -- * VerificationState
    VerificationState (..),

    -- * VulnerabilityFixAvailable
    VulnerabilityFixAvailable (..),

    -- * WorkflowState
    WorkflowState (..),

    -- * WorkflowStatus
    WorkflowStatus (..),

    -- * AccountDetails
    AccountDetails (..),
    newAccountDetails,
    accountDetails_email,
    accountDetails_accountId,

    -- * Action
    Action (..),
    newAction,
    action_actionType,
    action_networkConnectionAction,
    action_awsApiCallAction,
    action_dnsRequestAction,
    action_portProbeAction,

    -- * ActionLocalIpDetails
    ActionLocalIpDetails (..),
    newActionLocalIpDetails,
    actionLocalIpDetails_ipAddressV4,

    -- * ActionLocalPortDetails
    ActionLocalPortDetails (..),
    newActionLocalPortDetails,
    actionLocalPortDetails_port,
    actionLocalPortDetails_portName,

    -- * ActionRemoteIpDetails
    ActionRemoteIpDetails (..),
    newActionRemoteIpDetails,
    actionRemoteIpDetails_country,
    actionRemoteIpDetails_ipAddressV4,
    actionRemoteIpDetails_city,
    actionRemoteIpDetails_organization,
    actionRemoteIpDetails_geoLocation,

    -- * ActionRemotePortDetails
    ActionRemotePortDetails (..),
    newActionRemotePortDetails,
    actionRemotePortDetails_port,
    actionRemotePortDetails_portName,

    -- * ActionTarget
    ActionTarget (..),
    newActionTarget,
    actionTarget_actionTargetArn,
    actionTarget_name,
    actionTarget_description,

    -- * Adjustment
    Adjustment (..),
    newAdjustment,
    adjustment_metric,
    adjustment_reason,

    -- * AdminAccount
    AdminAccount (..),
    newAdminAccount,
    adminAccount_status,
    adminAccount_accountId,

    -- * AvailabilityZone
    AvailabilityZone (..),
    newAvailabilityZone,
    availabilityZone_zoneName,
    availabilityZone_subnetId,

    -- * AwsApiCallAction
    AwsApiCallAction (..),
    newAwsApiCallAction,
    awsApiCallAction_affectedResources,
    awsApiCallAction_remoteIpDetails,
    awsApiCallAction_lastSeen,
    awsApiCallAction_domainDetails,
    awsApiCallAction_api,
    awsApiCallAction_serviceName,
    awsApiCallAction_callerType,
    awsApiCallAction_firstSeen,

    -- * AwsApiCallActionDomainDetails
    AwsApiCallActionDomainDetails (..),
    newAwsApiCallActionDomainDetails,
    awsApiCallActionDomainDetails_domain,

    -- * AwsApiGatewayAccessLogSettings
    AwsApiGatewayAccessLogSettings (..),
    newAwsApiGatewayAccessLogSettings,
    awsApiGatewayAccessLogSettings_format,
    awsApiGatewayAccessLogSettings_destinationArn,

    -- * AwsApiGatewayCanarySettings
    AwsApiGatewayCanarySettings (..),
    newAwsApiGatewayCanarySettings,
    awsApiGatewayCanarySettings_deploymentId,
    awsApiGatewayCanarySettings_useStageCache,
    awsApiGatewayCanarySettings_stageVariableOverrides,
    awsApiGatewayCanarySettings_percentTraffic,

    -- * AwsApiGatewayEndpointConfiguration
    AwsApiGatewayEndpointConfiguration (..),
    newAwsApiGatewayEndpointConfiguration,
    awsApiGatewayEndpointConfiguration_types,

    -- * AwsApiGatewayMethodSettings
    AwsApiGatewayMethodSettings (..),
    newAwsApiGatewayMethodSettings,
    awsApiGatewayMethodSettings_throttlingRateLimit,
    awsApiGatewayMethodSettings_loggingLevel,
    awsApiGatewayMethodSettings_throttlingBurstLimit,
    awsApiGatewayMethodSettings_resourcePath,
    awsApiGatewayMethodSettings_metricsEnabled,
    awsApiGatewayMethodSettings_requireAuthorizationForCacheControl,
    awsApiGatewayMethodSettings_unauthorizedCacheControlHeaderStrategy,
    awsApiGatewayMethodSettings_httpMethod,
    awsApiGatewayMethodSettings_cacheTtlInSeconds,
    awsApiGatewayMethodSettings_dataTraceEnabled,
    awsApiGatewayMethodSettings_cachingEnabled,
    awsApiGatewayMethodSettings_cacheDataEncrypted,

    -- * AwsApiGatewayRestApiDetails
    AwsApiGatewayRestApiDetails (..),
    newAwsApiGatewayRestApiDetails,
    awsApiGatewayRestApiDetails_name,
    awsApiGatewayRestApiDetails_id,
    awsApiGatewayRestApiDetails_description,
    awsApiGatewayRestApiDetails_binaryMediaTypes,
    awsApiGatewayRestApiDetails_endpointConfiguration,
    awsApiGatewayRestApiDetails_createdDate,
    awsApiGatewayRestApiDetails_apiKeySource,
    awsApiGatewayRestApiDetails_minimumCompressionSize,
    awsApiGatewayRestApiDetails_version,

    -- * AwsApiGatewayStageDetails
    AwsApiGatewayStageDetails (..),
    newAwsApiGatewayStageDetails,
    awsApiGatewayStageDetails_stageName,
    awsApiGatewayStageDetails_webAclArn,
    awsApiGatewayStageDetails_accessLogSettings,
    awsApiGatewayStageDetails_cacheClusterEnabled,
    awsApiGatewayStageDetails_cacheClusterStatus,
    awsApiGatewayStageDetails_deploymentId,
    awsApiGatewayStageDetails_lastUpdatedDate,
    awsApiGatewayStageDetails_methodSettings,
    awsApiGatewayStageDetails_tracingEnabled,
    awsApiGatewayStageDetails_description,
    awsApiGatewayStageDetails_clientCertificateId,
    awsApiGatewayStageDetails_cacheClusterSize,
    awsApiGatewayStageDetails_createdDate,
    awsApiGatewayStageDetails_canarySettings,
    awsApiGatewayStageDetails_documentationVersion,
    awsApiGatewayStageDetails_variables,

    -- * AwsApiGatewayV2ApiDetails
    AwsApiGatewayV2ApiDetails (..),
    newAwsApiGatewayV2ApiDetails,
    awsApiGatewayV2ApiDetails_name,
    awsApiGatewayV2ApiDetails_apiEndpoint,
    awsApiGatewayV2ApiDetails_apiId,
    awsApiGatewayV2ApiDetails_routeSelectionExpression,
    awsApiGatewayV2ApiDetails_description,
    awsApiGatewayV2ApiDetails_apiKeySelectionExpression,
    awsApiGatewayV2ApiDetails_protocolType,
    awsApiGatewayV2ApiDetails_createdDate,
    awsApiGatewayV2ApiDetails_corsConfiguration,
    awsApiGatewayV2ApiDetails_version,

    -- * AwsApiGatewayV2RouteSettings
    AwsApiGatewayV2RouteSettings (..),
    newAwsApiGatewayV2RouteSettings,
    awsApiGatewayV2RouteSettings_throttlingRateLimit,
    awsApiGatewayV2RouteSettings_loggingLevel,
    awsApiGatewayV2RouteSettings_throttlingBurstLimit,
    awsApiGatewayV2RouteSettings_detailedMetricsEnabled,
    awsApiGatewayV2RouteSettings_dataTraceEnabled,

    -- * AwsApiGatewayV2StageDetails
    AwsApiGatewayV2StageDetails (..),
    newAwsApiGatewayV2StageDetails,
    awsApiGatewayV2StageDetails_stageName,
    awsApiGatewayV2StageDetails_accessLogSettings,
    awsApiGatewayV2StageDetails_deploymentId,
    awsApiGatewayV2StageDetails_autoDeploy,
    awsApiGatewayV2StageDetails_lastUpdatedDate,
    awsApiGatewayV2StageDetails_stageVariables,
    awsApiGatewayV2StageDetails_description,
    awsApiGatewayV2StageDetails_clientCertificateId,
    awsApiGatewayV2StageDetails_lastDeploymentStatusMessage,
    awsApiGatewayV2StageDetails_defaultRouteSettings,
    awsApiGatewayV2StageDetails_createdDate,
    awsApiGatewayV2StageDetails_apiGatewayManaged,
    awsApiGatewayV2StageDetails_routeSettings,

    -- * AwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails
    AwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails (..),
    newAwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails,
    awsAutoScalingAutoScalingGroupAvailabilityZonesListDetails_value,

    -- * AwsAutoScalingAutoScalingGroupDetails
    AwsAutoScalingAutoScalingGroupDetails (..),
    newAwsAutoScalingAutoScalingGroupDetails,
    awsAutoScalingAutoScalingGroupDetails_createdTime,
    awsAutoScalingAutoScalingGroupDetails_loadBalancerNames,
    awsAutoScalingAutoScalingGroupDetails_availabilityZones,
    awsAutoScalingAutoScalingGroupDetails_healthCheckGracePeriod,
    awsAutoScalingAutoScalingGroupDetails_launchTemplate,
    awsAutoScalingAutoScalingGroupDetails_launchConfigurationName,
    awsAutoScalingAutoScalingGroupDetails_mixedInstancesPolicy,
    awsAutoScalingAutoScalingGroupDetails_healthCheckType,
    awsAutoScalingAutoScalingGroupDetails_capacityRebalance,

    -- * AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification
    AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification (..),
    newAwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification,
    awsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification_launchTemplateId,
    awsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification_version,
    awsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification_launchTemplateName,

    -- * AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails (..),
    newAwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails_instancesDistribution,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails_launchTemplate,

    -- * AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails (..),
    newAwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_onDemandBaseCapacity,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_onDemandPercentageAboveBaseCapacity,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_onDemandAllocationStrategy,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_spotInstancePools,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_spotMaxPrice,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_spotAllocationStrategy,

    -- * AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails (..),
    newAwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails_launchTemplateSpecification,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails_overrides,

    -- * AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification (..),
    newAwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification_launchTemplateId,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification_version,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification_launchTemplateName,

    -- * AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails (..),
    newAwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails_instanceType,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails_weightedCapacity,

    -- * AwsAutoScalingLaunchConfigurationBlockDeviceMappingsDetails
    AwsAutoScalingLaunchConfigurationBlockDeviceMappingsDetails (..),
    newAwsAutoScalingLaunchConfigurationBlockDeviceMappingsDetails,
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsDetails_ebs,
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsDetails_deviceName,
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsDetails_noDevice,
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsDetails_virtualName,

    -- * AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails
    AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails (..),
    newAwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails,
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_deleteOnTermination,
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_snapshotId,
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_volumeType,
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_volumeSize,
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_encrypted,
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_iops,

    -- * AwsAutoScalingLaunchConfigurationDetails
    AwsAutoScalingLaunchConfigurationDetails (..),
    newAwsAutoScalingLaunchConfigurationDetails,
    awsAutoScalingLaunchConfigurationDetails_ebsOptimized,
    awsAutoScalingLaunchConfigurationDetails_iamInstanceProfile,
    awsAutoScalingLaunchConfigurationDetails_createdTime,
    awsAutoScalingLaunchConfigurationDetails_classicLinkVpcId,
    awsAutoScalingLaunchConfigurationDetails_userData,
    awsAutoScalingLaunchConfigurationDetails_associatePublicIpAddress,
    awsAutoScalingLaunchConfigurationDetails_blockDeviceMappings,
    awsAutoScalingLaunchConfigurationDetails_launchConfigurationName,
    awsAutoScalingLaunchConfigurationDetails_instanceType,
    awsAutoScalingLaunchConfigurationDetails_placementTenancy,
    awsAutoScalingLaunchConfigurationDetails_securityGroups,
    awsAutoScalingLaunchConfigurationDetails_ramdiskId,
    awsAutoScalingLaunchConfigurationDetails_instanceMonitoring,
    awsAutoScalingLaunchConfigurationDetails_keyName,
    awsAutoScalingLaunchConfigurationDetails_kernelId,
    awsAutoScalingLaunchConfigurationDetails_spotPrice,
    awsAutoScalingLaunchConfigurationDetails_imageId,
    awsAutoScalingLaunchConfigurationDetails_classicLinkVpcSecurityGroups,
    awsAutoScalingLaunchConfigurationDetails_metadataOptions,

    -- * AwsAutoScalingLaunchConfigurationInstanceMonitoringDetails
    AwsAutoScalingLaunchConfigurationInstanceMonitoringDetails (..),
    newAwsAutoScalingLaunchConfigurationInstanceMonitoringDetails,
    awsAutoScalingLaunchConfigurationInstanceMonitoringDetails_enabled,

    -- * AwsAutoScalingLaunchConfigurationMetadataOptions
    AwsAutoScalingLaunchConfigurationMetadataOptions (..),
    newAwsAutoScalingLaunchConfigurationMetadataOptions,
    awsAutoScalingLaunchConfigurationMetadataOptions_httpPutResponseHopLimit,
    awsAutoScalingLaunchConfigurationMetadataOptions_httpTokens,
    awsAutoScalingLaunchConfigurationMetadataOptions_httpEndpoint,

    -- * AwsBackupBackupPlanAdvancedBackupSettingsDetails
    AwsBackupBackupPlanAdvancedBackupSettingsDetails (..),
    newAwsBackupBackupPlanAdvancedBackupSettingsDetails,
    awsBackupBackupPlanAdvancedBackupSettingsDetails_resourceType,
    awsBackupBackupPlanAdvancedBackupSettingsDetails_backupOptions,

    -- * AwsBackupBackupPlanBackupPlanDetails
    AwsBackupBackupPlanBackupPlanDetails (..),
    newAwsBackupBackupPlanBackupPlanDetails,
    awsBackupBackupPlanBackupPlanDetails_backupPlanName,
    awsBackupBackupPlanBackupPlanDetails_backupPlanRule,
    awsBackupBackupPlanBackupPlanDetails_advancedBackupSettings,

    -- * AwsBackupBackupPlanDetails
    AwsBackupBackupPlanDetails (..),
    newAwsBackupBackupPlanDetails,
    awsBackupBackupPlanDetails_backupPlan,
    awsBackupBackupPlanDetails_backupPlanArn,
    awsBackupBackupPlanDetails_backupPlanId,
    awsBackupBackupPlanDetails_versionId,

    -- * AwsBackupBackupPlanLifecycleDetails
    AwsBackupBackupPlanLifecycleDetails (..),
    newAwsBackupBackupPlanLifecycleDetails,
    awsBackupBackupPlanLifecycleDetails_deleteAfterDays,
    awsBackupBackupPlanLifecycleDetails_moveToColdStorageAfterDays,

    -- * AwsBackupBackupPlanRuleCopyActionsDetails
    AwsBackupBackupPlanRuleCopyActionsDetails (..),
    newAwsBackupBackupPlanRuleCopyActionsDetails,
    awsBackupBackupPlanRuleCopyActionsDetails_lifecycle,
    awsBackupBackupPlanRuleCopyActionsDetails_destinationBackupVaultArn,

    -- * AwsBackupBackupPlanRuleDetails
    AwsBackupBackupPlanRuleDetails (..),
    newAwsBackupBackupPlanRuleDetails,
    awsBackupBackupPlanRuleDetails_startWindowMinutes,
    awsBackupBackupPlanRuleDetails_lifecycle,
    awsBackupBackupPlanRuleDetails_targetBackupVault,
    awsBackupBackupPlanRuleDetails_ruleId,
    awsBackupBackupPlanRuleDetails_copyActions,
    awsBackupBackupPlanRuleDetails_scheduleExpression,
    awsBackupBackupPlanRuleDetails_enableContinuousBackup,
    awsBackupBackupPlanRuleDetails_ruleName,
    awsBackupBackupPlanRuleDetails_completionWindowMinutes,

    -- * AwsBackupBackupVaultDetails
    AwsBackupBackupVaultDetails (..),
    newAwsBackupBackupVaultDetails,
    awsBackupBackupVaultDetails_notifications,
    awsBackupBackupVaultDetails_encryptionKeyArn,
    awsBackupBackupVaultDetails_backupVaultName,
    awsBackupBackupVaultDetails_backupVaultArn,
    awsBackupBackupVaultDetails_accessPolicy,

    -- * AwsBackupBackupVaultNotificationsDetails
    AwsBackupBackupVaultNotificationsDetails (..),
    newAwsBackupBackupVaultNotificationsDetails,
    awsBackupBackupVaultNotificationsDetails_backupVaultEvents,
    awsBackupBackupVaultNotificationsDetails_snsTopicArn,

    -- * AwsBackupRecoveryPointCalculatedLifecycleDetails
    AwsBackupRecoveryPointCalculatedLifecycleDetails (..),
    newAwsBackupRecoveryPointCalculatedLifecycleDetails,
    awsBackupRecoveryPointCalculatedLifecycleDetails_moveToColdStorageAt,
    awsBackupRecoveryPointCalculatedLifecycleDetails_deleteAt,

    -- * AwsBackupRecoveryPointCreatedByDetails
    AwsBackupRecoveryPointCreatedByDetails (..),
    newAwsBackupRecoveryPointCreatedByDetails,
    awsBackupRecoveryPointCreatedByDetails_backupPlanVersion,
    awsBackupRecoveryPointCreatedByDetails_backupPlanArn,
    awsBackupRecoveryPointCreatedByDetails_backupPlanId,
    awsBackupRecoveryPointCreatedByDetails_backupRuleId,

    -- * AwsBackupRecoveryPointDetails
    AwsBackupRecoveryPointDetails (..),
    newAwsBackupRecoveryPointDetails,
    awsBackupRecoveryPointDetails_encryptionKeyArn,
    awsBackupRecoveryPointDetails_resourceType,
    awsBackupRecoveryPointDetails_lifecycle,
    awsBackupRecoveryPointDetails_recoveryPointArn,
    awsBackupRecoveryPointDetails_completionDate,
    awsBackupRecoveryPointDetails_backupVaultName,
    awsBackupRecoveryPointDetails_creationDate,
    awsBackupRecoveryPointDetails_backupSizeInBytes,
    awsBackupRecoveryPointDetails_status,
    awsBackupRecoveryPointDetails_backupVaultArn,
    awsBackupRecoveryPointDetails_isEncrypted,
    awsBackupRecoveryPointDetails_iamRoleArn,
    awsBackupRecoveryPointDetails_sourceBackupVaultArn,
    awsBackupRecoveryPointDetails_resourceArn,
    awsBackupRecoveryPointDetails_storageClass,
    awsBackupRecoveryPointDetails_statusMessage,
    awsBackupRecoveryPointDetails_createdBy,
    awsBackupRecoveryPointDetails_lastRestoreTime,
    awsBackupRecoveryPointDetails_calculatedLifecycle,

    -- * AwsBackupRecoveryPointLifecycleDetails
    AwsBackupRecoveryPointLifecycleDetails (..),
    newAwsBackupRecoveryPointLifecycleDetails,
    awsBackupRecoveryPointLifecycleDetails_deleteAfterDays,
    awsBackupRecoveryPointLifecycleDetails_moveToColdStorageAfterDays,

    -- * AwsCertificateManagerCertificateDetails
    AwsCertificateManagerCertificateDetails (..),
    newAwsCertificateManagerCertificateDetails,
    awsCertificateManagerCertificateDetails_issuer,
    awsCertificateManagerCertificateDetails_domainValidationOptions,
    awsCertificateManagerCertificateDetails_type,
    awsCertificateManagerCertificateDetails_certificateAuthorityArn,
    awsCertificateManagerCertificateDetails_domainName,
    awsCertificateManagerCertificateDetails_serial,
    awsCertificateManagerCertificateDetails_keyUsages,
    awsCertificateManagerCertificateDetails_renewalSummary,
    awsCertificateManagerCertificateDetails_keyAlgorithm,
    awsCertificateManagerCertificateDetails_extendedKeyUsages,
    awsCertificateManagerCertificateDetails_inUseBy,
    awsCertificateManagerCertificateDetails_status,
    awsCertificateManagerCertificateDetails_options,
    awsCertificateManagerCertificateDetails_importedAt,
    awsCertificateManagerCertificateDetails_notBefore,
    awsCertificateManagerCertificateDetails_signatureAlgorithm,
    awsCertificateManagerCertificateDetails_subject,
    awsCertificateManagerCertificateDetails_notAfter,
    awsCertificateManagerCertificateDetails_renewalEligibility,
    awsCertificateManagerCertificateDetails_createdAt,
    awsCertificateManagerCertificateDetails_subjectAlternativeNames,
    awsCertificateManagerCertificateDetails_failureReason,
    awsCertificateManagerCertificateDetails_issuedAt,

    -- * AwsCertificateManagerCertificateDomainValidationOption
    AwsCertificateManagerCertificateDomainValidationOption (..),
    newAwsCertificateManagerCertificateDomainValidationOption,
    awsCertificateManagerCertificateDomainValidationOption_domainName,
    awsCertificateManagerCertificateDomainValidationOption_validationStatus,
    awsCertificateManagerCertificateDomainValidationOption_validationDomain,
    awsCertificateManagerCertificateDomainValidationOption_resourceRecord,
    awsCertificateManagerCertificateDomainValidationOption_validationEmails,
    awsCertificateManagerCertificateDomainValidationOption_validationMethod,

    -- * AwsCertificateManagerCertificateExtendedKeyUsage
    AwsCertificateManagerCertificateExtendedKeyUsage (..),
    newAwsCertificateManagerCertificateExtendedKeyUsage,
    awsCertificateManagerCertificateExtendedKeyUsage_name,
    awsCertificateManagerCertificateExtendedKeyUsage_oId,

    -- * AwsCertificateManagerCertificateKeyUsage
    AwsCertificateManagerCertificateKeyUsage (..),
    newAwsCertificateManagerCertificateKeyUsage,
    awsCertificateManagerCertificateKeyUsage_name,

    -- * AwsCertificateManagerCertificateOptions
    AwsCertificateManagerCertificateOptions (..),
    newAwsCertificateManagerCertificateOptions,
    awsCertificateManagerCertificateOptions_certificateTransparencyLoggingPreference,

    -- * AwsCertificateManagerCertificateRenewalSummary
    AwsCertificateManagerCertificateRenewalSummary (..),
    newAwsCertificateManagerCertificateRenewalSummary,
    awsCertificateManagerCertificateRenewalSummary_domainValidationOptions,
    awsCertificateManagerCertificateRenewalSummary_renewalStatusReason,
    awsCertificateManagerCertificateRenewalSummary_renewalStatus,
    awsCertificateManagerCertificateRenewalSummary_updatedAt,

    -- * AwsCertificateManagerCertificateResourceRecord
    AwsCertificateManagerCertificateResourceRecord (..),
    newAwsCertificateManagerCertificateResourceRecord,
    awsCertificateManagerCertificateResourceRecord_name,
    awsCertificateManagerCertificateResourceRecord_type,
    awsCertificateManagerCertificateResourceRecord_value,

    -- * AwsCloudFormationStackDetails
    AwsCloudFormationStackDetails (..),
    newAwsCloudFormationStackDetails,
    awsCloudFormationStackDetails_stackId,
    awsCloudFormationStackDetails_roleArn,
    awsCloudFormationStackDetails_timeoutInMinutes,
    awsCloudFormationStackDetails_enableTerminationProtection,
    awsCloudFormationStackDetails_notificationArns,
    awsCloudFormationStackDetails_stackStatusReason,
    awsCloudFormationStackDetails_disableRollback,
    awsCloudFormationStackDetails_lastUpdatedTime,
    awsCloudFormationStackDetails_description,
    awsCloudFormationStackDetails_stackName,
    awsCloudFormationStackDetails_stackStatus,
    awsCloudFormationStackDetails_outputs,
    awsCloudFormationStackDetails_capabilities,
    awsCloudFormationStackDetails_creationTime,
    awsCloudFormationStackDetails_driftInformation,

    -- * AwsCloudFormationStackDriftInformationDetails
    AwsCloudFormationStackDriftInformationDetails (..),
    newAwsCloudFormationStackDriftInformationDetails,
    awsCloudFormationStackDriftInformationDetails_stackDriftStatus,

    -- * AwsCloudFormationStackOutputsDetails
    AwsCloudFormationStackOutputsDetails (..),
    newAwsCloudFormationStackOutputsDetails,
    awsCloudFormationStackOutputsDetails_outputKey,
    awsCloudFormationStackOutputsDetails_description,
    awsCloudFormationStackOutputsDetails_outputValue,

    -- * AwsCloudFrontDistributionCacheBehavior
    AwsCloudFrontDistributionCacheBehavior (..),
    newAwsCloudFrontDistributionCacheBehavior,
    awsCloudFrontDistributionCacheBehavior_viewerProtocolPolicy,

    -- * AwsCloudFrontDistributionCacheBehaviors
    AwsCloudFrontDistributionCacheBehaviors (..),
    newAwsCloudFrontDistributionCacheBehaviors,
    awsCloudFrontDistributionCacheBehaviors_items,

    -- * AwsCloudFrontDistributionDefaultCacheBehavior
    AwsCloudFrontDistributionDefaultCacheBehavior (..),
    newAwsCloudFrontDistributionDefaultCacheBehavior,
    awsCloudFrontDistributionDefaultCacheBehavior_viewerProtocolPolicy,

    -- * AwsCloudFrontDistributionDetails
    AwsCloudFrontDistributionDetails (..),
    newAwsCloudFrontDistributionDetails,
    awsCloudFrontDistributionDetails_domainName,
    awsCloudFrontDistributionDetails_status,
    awsCloudFrontDistributionDetails_defaultRootObject,
    awsCloudFrontDistributionDetails_viewerCertificate,
    awsCloudFrontDistributionDetails_lastModifiedTime,
    awsCloudFrontDistributionDetails_logging,
    awsCloudFrontDistributionDetails_webAclId,
    awsCloudFrontDistributionDetails_originGroups,
    awsCloudFrontDistributionDetails_origins,
    awsCloudFrontDistributionDetails_cacheBehaviors,
    awsCloudFrontDistributionDetails_eTag,
    awsCloudFrontDistributionDetails_defaultCacheBehavior,

    -- * AwsCloudFrontDistributionLogging
    AwsCloudFrontDistributionLogging (..),
    newAwsCloudFrontDistributionLogging,
    awsCloudFrontDistributionLogging_bucket,
    awsCloudFrontDistributionLogging_enabled,
    awsCloudFrontDistributionLogging_includeCookies,
    awsCloudFrontDistributionLogging_prefix,

    -- * AwsCloudFrontDistributionOriginCustomOriginConfig
    AwsCloudFrontDistributionOriginCustomOriginConfig (..),
    newAwsCloudFrontDistributionOriginCustomOriginConfig,
    awsCloudFrontDistributionOriginCustomOriginConfig_httpsPort,
    awsCloudFrontDistributionOriginCustomOriginConfig_httpPort,
    awsCloudFrontDistributionOriginCustomOriginConfig_originKeepaliveTimeout,
    awsCloudFrontDistributionOriginCustomOriginConfig_originProtocolPolicy,
    awsCloudFrontDistributionOriginCustomOriginConfig_originReadTimeout,
    awsCloudFrontDistributionOriginCustomOriginConfig_originSslProtocols,

    -- * AwsCloudFrontDistributionOriginGroup
    AwsCloudFrontDistributionOriginGroup (..),
    newAwsCloudFrontDistributionOriginGroup,
    awsCloudFrontDistributionOriginGroup_failoverCriteria,

    -- * AwsCloudFrontDistributionOriginGroupFailover
    AwsCloudFrontDistributionOriginGroupFailover (..),
    newAwsCloudFrontDistributionOriginGroupFailover,
    awsCloudFrontDistributionOriginGroupFailover_statusCodes,

    -- * AwsCloudFrontDistributionOriginGroupFailoverStatusCodes
    AwsCloudFrontDistributionOriginGroupFailoverStatusCodes (..),
    newAwsCloudFrontDistributionOriginGroupFailoverStatusCodes,
    awsCloudFrontDistributionOriginGroupFailoverStatusCodes_items,
    awsCloudFrontDistributionOriginGroupFailoverStatusCodes_quantity,

    -- * AwsCloudFrontDistributionOriginGroups
    AwsCloudFrontDistributionOriginGroups (..),
    newAwsCloudFrontDistributionOriginGroups,
    awsCloudFrontDistributionOriginGroups_items,

    -- * AwsCloudFrontDistributionOriginItem
    AwsCloudFrontDistributionOriginItem (..),
    newAwsCloudFrontDistributionOriginItem,
    awsCloudFrontDistributionOriginItem_domainName,
    awsCloudFrontDistributionOriginItem_id,
    awsCloudFrontDistributionOriginItem_s3OriginConfig,
    awsCloudFrontDistributionOriginItem_originPath,
    awsCloudFrontDistributionOriginItem_customOriginConfig,

    -- * AwsCloudFrontDistributionOriginS3OriginConfig
    AwsCloudFrontDistributionOriginS3OriginConfig (..),
    newAwsCloudFrontDistributionOriginS3OriginConfig,
    awsCloudFrontDistributionOriginS3OriginConfig_originAccessIdentity,

    -- * AwsCloudFrontDistributionOriginSslProtocols
    AwsCloudFrontDistributionOriginSslProtocols (..),
    newAwsCloudFrontDistributionOriginSslProtocols,
    awsCloudFrontDistributionOriginSslProtocols_items,
    awsCloudFrontDistributionOriginSslProtocols_quantity,

    -- * AwsCloudFrontDistributionOrigins
    AwsCloudFrontDistributionOrigins (..),
    newAwsCloudFrontDistributionOrigins,
    awsCloudFrontDistributionOrigins_items,

    -- * AwsCloudFrontDistributionViewerCertificate
    AwsCloudFrontDistributionViewerCertificate (..),
    newAwsCloudFrontDistributionViewerCertificate,
    awsCloudFrontDistributionViewerCertificate_iamCertificateId,
    awsCloudFrontDistributionViewerCertificate_cloudFrontDefaultCertificate,
    awsCloudFrontDistributionViewerCertificate_certificate,
    awsCloudFrontDistributionViewerCertificate_minimumProtocolVersion,
    awsCloudFrontDistributionViewerCertificate_acmCertificateArn,
    awsCloudFrontDistributionViewerCertificate_sslSupportMethod,
    awsCloudFrontDistributionViewerCertificate_certificateSource,

    -- * AwsCloudTrailTrailDetails
    AwsCloudTrailTrailDetails (..),
    newAwsCloudTrailTrailDetails,
    awsCloudTrailTrailDetails_s3KeyPrefix,
    awsCloudTrailTrailDetails_name,
    awsCloudTrailTrailDetails_logFileValidationEnabled,
    awsCloudTrailTrailDetails_snsTopicName,
    awsCloudTrailTrailDetails_isOrganizationTrail,
    awsCloudTrailTrailDetails_includeGlobalServiceEvents,
    awsCloudTrailTrailDetails_s3BucketName,
    awsCloudTrailTrailDetails_hasCustomEventSelectors,
    awsCloudTrailTrailDetails_snsTopicArn,
    awsCloudTrailTrailDetails_isMultiRegionTrail,
    awsCloudTrailTrailDetails_kmsKeyId,
    awsCloudTrailTrailDetails_cloudWatchLogsRoleArn,
    awsCloudTrailTrailDetails_homeRegion,
    awsCloudTrailTrailDetails_cloudWatchLogsLogGroupArn,
    awsCloudTrailTrailDetails_trailArn,

    -- * AwsCloudWatchAlarmDetails
    AwsCloudWatchAlarmDetails (..),
    newAwsCloudWatchAlarmDetails,
    awsCloudWatchAlarmDetails_alarmActions,
    awsCloudWatchAlarmDetails_alarmDescription,
    awsCloudWatchAlarmDetails_extendedStatistic,
    awsCloudWatchAlarmDetails_actionsEnabled,
    awsCloudWatchAlarmDetails_period,
    awsCloudWatchAlarmDetails_evaluateLowSampleCountPercentile,
    awsCloudWatchAlarmDetails_dimensions,
    awsCloudWatchAlarmDetails_thresholdMetricId,
    awsCloudWatchAlarmDetails_treatMissingData,
    awsCloudWatchAlarmDetails_evaluationPeriods,
    awsCloudWatchAlarmDetails_datapointsToAlarm,
    awsCloudWatchAlarmDetails_insufficientDataActions,
    awsCloudWatchAlarmDetails_alarmArn,
    awsCloudWatchAlarmDetails_metricName,
    awsCloudWatchAlarmDetails_alarmConfigurationUpdatedTimestamp,
    awsCloudWatchAlarmDetails_threshold,
    awsCloudWatchAlarmDetails_okActions,
    awsCloudWatchAlarmDetails_alarmName,
    awsCloudWatchAlarmDetails_comparisonOperator,
    awsCloudWatchAlarmDetails_namespace,
    awsCloudWatchAlarmDetails_statistic,
    awsCloudWatchAlarmDetails_unit,

    -- * AwsCloudWatchAlarmDimensionsDetails
    AwsCloudWatchAlarmDimensionsDetails (..),
    newAwsCloudWatchAlarmDimensionsDetails,
    awsCloudWatchAlarmDimensionsDetails_name,
    awsCloudWatchAlarmDimensionsDetails_value,

    -- * AwsCodeBuildProjectArtifactsDetails
    AwsCodeBuildProjectArtifactsDetails (..),
    newAwsCodeBuildProjectArtifactsDetails,
    awsCodeBuildProjectArtifactsDetails_encryptionDisabled,
    awsCodeBuildProjectArtifactsDetails_name,
    awsCodeBuildProjectArtifactsDetails_type,
    awsCodeBuildProjectArtifactsDetails_path,
    awsCodeBuildProjectArtifactsDetails_artifactIdentifier,
    awsCodeBuildProjectArtifactsDetails_packaging,
    awsCodeBuildProjectArtifactsDetails_location,
    awsCodeBuildProjectArtifactsDetails_overrideArtifactName,
    awsCodeBuildProjectArtifactsDetails_namespaceType,

    -- * AwsCodeBuildProjectDetails
    AwsCodeBuildProjectDetails (..),
    newAwsCodeBuildProjectDetails,
    awsCodeBuildProjectDetails_name,
    awsCodeBuildProjectDetails_environment,
    awsCodeBuildProjectDetails_vpcConfig,
    awsCodeBuildProjectDetails_secondaryArtifacts,
    awsCodeBuildProjectDetails_serviceRole,
    awsCodeBuildProjectDetails_source,
    awsCodeBuildProjectDetails_logsConfig,
    awsCodeBuildProjectDetails_encryptionKey,
    awsCodeBuildProjectDetails_artifacts,

    -- * AwsCodeBuildProjectEnvironment
    AwsCodeBuildProjectEnvironment (..),
    newAwsCodeBuildProjectEnvironment,
    awsCodeBuildProjectEnvironment_privilegedMode,
    awsCodeBuildProjectEnvironment_type,
    awsCodeBuildProjectEnvironment_imagePullCredentialsType,
    awsCodeBuildProjectEnvironment_certificate,
    awsCodeBuildProjectEnvironment_registryCredential,
    awsCodeBuildProjectEnvironment_environmentVariables,

    -- * AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails
    AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails (..),
    newAwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails,
    awsCodeBuildProjectEnvironmentEnvironmentVariablesDetails_name,
    awsCodeBuildProjectEnvironmentEnvironmentVariablesDetails_type,
    awsCodeBuildProjectEnvironmentEnvironmentVariablesDetails_value,

    -- * AwsCodeBuildProjectEnvironmentRegistryCredential
    AwsCodeBuildProjectEnvironmentRegistryCredential (..),
    newAwsCodeBuildProjectEnvironmentRegistryCredential,
    awsCodeBuildProjectEnvironmentRegistryCredential_credential,
    awsCodeBuildProjectEnvironmentRegistryCredential_credentialProvider,

    -- * AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails
    AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails (..),
    newAwsCodeBuildProjectLogsConfigCloudWatchLogsDetails,
    awsCodeBuildProjectLogsConfigCloudWatchLogsDetails_status,
    awsCodeBuildProjectLogsConfigCloudWatchLogsDetails_groupName,
    awsCodeBuildProjectLogsConfigCloudWatchLogsDetails_streamName,

    -- * AwsCodeBuildProjectLogsConfigDetails
    AwsCodeBuildProjectLogsConfigDetails (..),
    newAwsCodeBuildProjectLogsConfigDetails,
    awsCodeBuildProjectLogsConfigDetails_cloudWatchLogs,
    awsCodeBuildProjectLogsConfigDetails_s3Logs,

    -- * AwsCodeBuildProjectLogsConfigS3LogsDetails
    AwsCodeBuildProjectLogsConfigS3LogsDetails (..),
    newAwsCodeBuildProjectLogsConfigS3LogsDetails,
    awsCodeBuildProjectLogsConfigS3LogsDetails_encryptionDisabled,
    awsCodeBuildProjectLogsConfigS3LogsDetails_status,
    awsCodeBuildProjectLogsConfigS3LogsDetails_location,

    -- * AwsCodeBuildProjectSource
    AwsCodeBuildProjectSource (..),
    newAwsCodeBuildProjectSource,
    awsCodeBuildProjectSource_insecureSsl,
    awsCodeBuildProjectSource_type,
    awsCodeBuildProjectSource_location,
    awsCodeBuildProjectSource_gitCloneDepth,

    -- * AwsCodeBuildProjectVpcConfig
    AwsCodeBuildProjectVpcConfig (..),
    newAwsCodeBuildProjectVpcConfig,
    awsCodeBuildProjectVpcConfig_securityGroupIds,
    awsCodeBuildProjectVpcConfig_subnets,
    awsCodeBuildProjectVpcConfig_vpcId,

    -- * AwsCorsConfiguration
    AwsCorsConfiguration (..),
    newAwsCorsConfiguration,
    awsCorsConfiguration_allowHeaders,
    awsCorsConfiguration_exposeHeaders,
    awsCorsConfiguration_allowCredentials,
    awsCorsConfiguration_allowMethods,
    awsCorsConfiguration_allowOrigins,
    awsCorsConfiguration_maxAge,

    -- * AwsDynamoDbTableAttributeDefinition
    AwsDynamoDbTableAttributeDefinition (..),
    newAwsDynamoDbTableAttributeDefinition,
    awsDynamoDbTableAttributeDefinition_attributeType,
    awsDynamoDbTableAttributeDefinition_attributeName,

    -- * AwsDynamoDbTableBillingModeSummary
    AwsDynamoDbTableBillingModeSummary (..),
    newAwsDynamoDbTableBillingModeSummary,
    awsDynamoDbTableBillingModeSummary_billingMode,
    awsDynamoDbTableBillingModeSummary_lastUpdateToPayPerRequestDateTime,

    -- * AwsDynamoDbTableDetails
    AwsDynamoDbTableDetails (..),
    newAwsDynamoDbTableDetails,
    awsDynamoDbTableDetails_tableName,
    awsDynamoDbTableDetails_latestStreamLabel,
    awsDynamoDbTableDetails_billingModeSummary,
    awsDynamoDbTableDetails_tableStatus,
    awsDynamoDbTableDetails_localSecondaryIndexes,
    awsDynamoDbTableDetails_tableSizeBytes,
    awsDynamoDbTableDetails_creationDateTime,
    awsDynamoDbTableDetails_replicas,
    awsDynamoDbTableDetails_itemCount,
    awsDynamoDbTableDetails_provisionedThroughput,
    awsDynamoDbTableDetails_latestStreamArn,
    awsDynamoDbTableDetails_tableId,
    awsDynamoDbTableDetails_keySchema,
    awsDynamoDbTableDetails_restoreSummary,
    awsDynamoDbTableDetails_globalSecondaryIndexes,
    awsDynamoDbTableDetails_streamSpecification,
    awsDynamoDbTableDetails_globalTableVersion,
    awsDynamoDbTableDetails_sseDescription,
    awsDynamoDbTableDetails_attributeDefinitions,

    -- * AwsDynamoDbTableGlobalSecondaryIndex
    AwsDynamoDbTableGlobalSecondaryIndex (..),
    newAwsDynamoDbTableGlobalSecondaryIndex,
    awsDynamoDbTableGlobalSecondaryIndex_itemCount,
    awsDynamoDbTableGlobalSecondaryIndex_provisionedThroughput,
    awsDynamoDbTableGlobalSecondaryIndex_backfilling,
    awsDynamoDbTableGlobalSecondaryIndex_indexName,
    awsDynamoDbTableGlobalSecondaryIndex_indexArn,
    awsDynamoDbTableGlobalSecondaryIndex_indexStatus,
    awsDynamoDbTableGlobalSecondaryIndex_indexSizeBytes,
    awsDynamoDbTableGlobalSecondaryIndex_keySchema,
    awsDynamoDbTableGlobalSecondaryIndex_projection,

    -- * AwsDynamoDbTableKeySchema
    AwsDynamoDbTableKeySchema (..),
    newAwsDynamoDbTableKeySchema,
    awsDynamoDbTableKeySchema_keyType,
    awsDynamoDbTableKeySchema_attributeName,

    -- * AwsDynamoDbTableLocalSecondaryIndex
    AwsDynamoDbTableLocalSecondaryIndex (..),
    newAwsDynamoDbTableLocalSecondaryIndex,
    awsDynamoDbTableLocalSecondaryIndex_indexName,
    awsDynamoDbTableLocalSecondaryIndex_indexArn,
    awsDynamoDbTableLocalSecondaryIndex_keySchema,
    awsDynamoDbTableLocalSecondaryIndex_projection,

    -- * AwsDynamoDbTableProjection
    AwsDynamoDbTableProjection (..),
    newAwsDynamoDbTableProjection,
    awsDynamoDbTableProjection_projectionType,
    awsDynamoDbTableProjection_nonKeyAttributes,

    -- * AwsDynamoDbTableProvisionedThroughput
    AwsDynamoDbTableProvisionedThroughput (..),
    newAwsDynamoDbTableProvisionedThroughput,
    awsDynamoDbTableProvisionedThroughput_readCapacityUnits,
    awsDynamoDbTableProvisionedThroughput_numberOfDecreasesToday,
    awsDynamoDbTableProvisionedThroughput_writeCapacityUnits,
    awsDynamoDbTableProvisionedThroughput_lastIncreaseDateTime,
    awsDynamoDbTableProvisionedThroughput_lastDecreaseDateTime,

    -- * AwsDynamoDbTableProvisionedThroughputOverride
    AwsDynamoDbTableProvisionedThroughputOverride (..),
    newAwsDynamoDbTableProvisionedThroughputOverride,
    awsDynamoDbTableProvisionedThroughputOverride_readCapacityUnits,

    -- * AwsDynamoDbTableReplica
    AwsDynamoDbTableReplica (..),
    newAwsDynamoDbTableReplica,
    awsDynamoDbTableReplica_kmsMasterKeyId,
    awsDynamoDbTableReplica_provisionedThroughputOverride,
    awsDynamoDbTableReplica_regionName,
    awsDynamoDbTableReplica_replicaStatusDescription,
    awsDynamoDbTableReplica_globalSecondaryIndexes,
    awsDynamoDbTableReplica_replicaStatus,

    -- * AwsDynamoDbTableReplicaGlobalSecondaryIndex
    AwsDynamoDbTableReplicaGlobalSecondaryIndex (..),
    newAwsDynamoDbTableReplicaGlobalSecondaryIndex,
    awsDynamoDbTableReplicaGlobalSecondaryIndex_provisionedThroughputOverride,
    awsDynamoDbTableReplicaGlobalSecondaryIndex_indexName,

    -- * AwsDynamoDbTableRestoreSummary
    AwsDynamoDbTableRestoreSummary (..),
    newAwsDynamoDbTableRestoreSummary,
    awsDynamoDbTableRestoreSummary_restoreInProgress,
    awsDynamoDbTableRestoreSummary_sourceBackupArn,
    awsDynamoDbTableRestoreSummary_sourceTableArn,
    awsDynamoDbTableRestoreSummary_restoreDateTime,

    -- * AwsDynamoDbTableSseDescription
    AwsDynamoDbTableSseDescription (..),
    newAwsDynamoDbTableSseDescription,
    awsDynamoDbTableSseDescription_inaccessibleEncryptionDateTime,
    awsDynamoDbTableSseDescription_status,
    awsDynamoDbTableSseDescription_sseType,
    awsDynamoDbTableSseDescription_kmsMasterKeyArn,

    -- * AwsDynamoDbTableStreamSpecification
    AwsDynamoDbTableStreamSpecification (..),
    newAwsDynamoDbTableStreamSpecification,
    awsDynamoDbTableStreamSpecification_streamViewType,
    awsDynamoDbTableStreamSpecification_streamEnabled,

    -- * AwsEc2EipDetails
    AwsEc2EipDetails (..),
    newAwsEc2EipDetails,
    awsEc2EipDetails_allocationId,
    awsEc2EipDetails_networkBorderGroup,
    awsEc2EipDetails_domain,
    awsEc2EipDetails_networkInterfaceOwnerId,
    awsEc2EipDetails_publicIp,
    awsEc2EipDetails_instanceId,
    awsEc2EipDetails_networkInterfaceId,
    awsEc2EipDetails_privateIpAddress,
    awsEc2EipDetails_associationId,
    awsEc2EipDetails_publicIpv4Pool,

    -- * AwsEc2InstanceDetails
    AwsEc2InstanceDetails (..),
    newAwsEc2InstanceDetails,
    awsEc2InstanceDetails_type,
    awsEc2InstanceDetails_ipV4Addresses,
    awsEc2InstanceDetails_virtualizationType,
    awsEc2InstanceDetails_subnetId,
    awsEc2InstanceDetails_iamInstanceProfileArn,
    awsEc2InstanceDetails_keyName,
    awsEc2InstanceDetails_vpcId,
    awsEc2InstanceDetails_launchedAt,
    awsEc2InstanceDetails_ipV6Addresses,
    awsEc2InstanceDetails_imageId,
    awsEc2InstanceDetails_networkInterfaces,
    awsEc2InstanceDetails_metadataOptions,

    -- * AwsEc2InstanceMetadataOptions
    AwsEc2InstanceMetadataOptions (..),
    newAwsEc2InstanceMetadataOptions,
    awsEc2InstanceMetadataOptions_httpPutResponseHopLimit,
    awsEc2InstanceMetadataOptions_httpTokens,
    awsEc2InstanceMetadataOptions_httpEndpoint,
    awsEc2InstanceMetadataOptions_instanceMetadataTags,
    awsEc2InstanceMetadataOptions_httpProtocolIpv6,

    -- * AwsEc2InstanceNetworkInterfacesDetails
    AwsEc2InstanceNetworkInterfacesDetails (..),
    newAwsEc2InstanceNetworkInterfacesDetails,
    awsEc2InstanceNetworkInterfacesDetails_networkInterfaceId,

    -- * AwsEc2NetworkAclAssociation
    AwsEc2NetworkAclAssociation (..),
    newAwsEc2NetworkAclAssociation,
    awsEc2NetworkAclAssociation_networkAclId,
    awsEc2NetworkAclAssociation_subnetId,
    awsEc2NetworkAclAssociation_networkAclAssociationId,

    -- * AwsEc2NetworkAclDetails
    AwsEc2NetworkAclDetails (..),
    newAwsEc2NetworkAclDetails,
    awsEc2NetworkAclDetails_ownerId,
    awsEc2NetworkAclDetails_networkAclId,
    awsEc2NetworkAclDetails_associations,
    awsEc2NetworkAclDetails_isDefault,
    awsEc2NetworkAclDetails_entries,
    awsEc2NetworkAclDetails_vpcId,

    -- * AwsEc2NetworkAclEntry
    AwsEc2NetworkAclEntry (..),
    newAwsEc2NetworkAclEntry,
    awsEc2NetworkAclEntry_icmpTypeCode,
    awsEc2NetworkAclEntry_egress,
    awsEc2NetworkAclEntry_portRange,
    awsEc2NetworkAclEntry_ruleNumber,
    awsEc2NetworkAclEntry_cidrBlock,
    awsEc2NetworkAclEntry_ruleAction,
    awsEc2NetworkAclEntry_protocol,
    awsEc2NetworkAclEntry_ipv6CidrBlock,

    -- * AwsEc2NetworkInterfaceAttachment
    AwsEc2NetworkInterfaceAttachment (..),
    newAwsEc2NetworkInterfaceAttachment,
    awsEc2NetworkInterfaceAttachment_deleteOnTermination,
    awsEc2NetworkInterfaceAttachment_status,
    awsEc2NetworkInterfaceAttachment_attachmentId,
    awsEc2NetworkInterfaceAttachment_instanceId,
    awsEc2NetworkInterfaceAttachment_attachTime,
    awsEc2NetworkInterfaceAttachment_deviceIndex,
    awsEc2NetworkInterfaceAttachment_instanceOwnerId,

    -- * AwsEc2NetworkInterfaceDetails
    AwsEc2NetworkInterfaceDetails (..),
    newAwsEc2NetworkInterfaceDetails,
    awsEc2NetworkInterfaceDetails_attachment,
    awsEc2NetworkInterfaceDetails_sourceDestCheck,
    awsEc2NetworkInterfaceDetails_privateIpAddresses,
    awsEc2NetworkInterfaceDetails_publicIp,
    awsEc2NetworkInterfaceDetails_networkInterfaceId,
    awsEc2NetworkInterfaceDetails_publicDnsName,
    awsEc2NetworkInterfaceDetails_securityGroups,
    awsEc2NetworkInterfaceDetails_ipV6Addresses,

    -- * AwsEc2NetworkInterfaceIpV6AddressDetail
    AwsEc2NetworkInterfaceIpV6AddressDetail (..),
    newAwsEc2NetworkInterfaceIpV6AddressDetail,
    awsEc2NetworkInterfaceIpV6AddressDetail_ipV6Address,

    -- * AwsEc2NetworkInterfacePrivateIpAddressDetail
    AwsEc2NetworkInterfacePrivateIpAddressDetail (..),
    newAwsEc2NetworkInterfacePrivateIpAddressDetail,
    awsEc2NetworkInterfacePrivateIpAddressDetail_privateIpAddress,
    awsEc2NetworkInterfacePrivateIpAddressDetail_privateDnsName,

    -- * AwsEc2NetworkInterfaceSecurityGroup
    AwsEc2NetworkInterfaceSecurityGroup (..),
    newAwsEc2NetworkInterfaceSecurityGroup,
    awsEc2NetworkInterfaceSecurityGroup_groupName,
    awsEc2NetworkInterfaceSecurityGroup_groupId,

    -- * AwsEc2SecurityGroupDetails
    AwsEc2SecurityGroupDetails (..),
    newAwsEc2SecurityGroupDetails,
    awsEc2SecurityGroupDetails_ipPermissionsEgress,
    awsEc2SecurityGroupDetails_ownerId,
    awsEc2SecurityGroupDetails_ipPermissions,
    awsEc2SecurityGroupDetails_groupName,
    awsEc2SecurityGroupDetails_vpcId,
    awsEc2SecurityGroupDetails_groupId,

    -- * AwsEc2SecurityGroupIpPermission
    AwsEc2SecurityGroupIpPermission (..),
    newAwsEc2SecurityGroupIpPermission,
    awsEc2SecurityGroupIpPermission_toPort,
    awsEc2SecurityGroupIpPermission_ipv6Ranges,
    awsEc2SecurityGroupIpPermission_ipProtocol,
    awsEc2SecurityGroupIpPermission_prefixListIds,
    awsEc2SecurityGroupIpPermission_ipRanges,
    awsEc2SecurityGroupIpPermission_userIdGroupPairs,
    awsEc2SecurityGroupIpPermission_fromPort,

    -- * AwsEc2SecurityGroupIpRange
    AwsEc2SecurityGroupIpRange (..),
    newAwsEc2SecurityGroupIpRange,
    awsEc2SecurityGroupIpRange_cidrIp,

    -- * AwsEc2SecurityGroupIpv6Range
    AwsEc2SecurityGroupIpv6Range (..),
    newAwsEc2SecurityGroupIpv6Range,
    awsEc2SecurityGroupIpv6Range_cidrIpv6,

    -- * AwsEc2SecurityGroupPrefixListId
    AwsEc2SecurityGroupPrefixListId (..),
    newAwsEc2SecurityGroupPrefixListId,
    awsEc2SecurityGroupPrefixListId_prefixListId,

    -- * AwsEc2SecurityGroupUserIdGroupPair
    AwsEc2SecurityGroupUserIdGroupPair (..),
    newAwsEc2SecurityGroupUserIdGroupPair,
    awsEc2SecurityGroupUserIdGroupPair_vpcPeeringConnectionId,
    awsEc2SecurityGroupUserIdGroupPair_groupName,
    awsEc2SecurityGroupUserIdGroupPair_peeringStatus,
    awsEc2SecurityGroupUserIdGroupPair_userId,
    awsEc2SecurityGroupUserIdGroupPair_vpcId,
    awsEc2SecurityGroupUserIdGroupPair_groupId,

    -- * AwsEc2SubnetDetails
    AwsEc2SubnetDetails (..),
    newAwsEc2SubnetDetails,
    awsEc2SubnetDetails_ownerId,
    awsEc2SubnetDetails_mapPublicIpOnLaunch,
    awsEc2SubnetDetails_defaultForAz,
    awsEc2SubnetDetails_subnetId,
    awsEc2SubnetDetails_state,
    awsEc2SubnetDetails_availabilityZone,
    awsEc2SubnetDetails_ipv6CidrBlockAssociationSet,
    awsEc2SubnetDetails_subnetArn,
    awsEc2SubnetDetails_cidrBlock,
    awsEc2SubnetDetails_assignIpv6AddressOnCreation,
    awsEc2SubnetDetails_vpcId,
    awsEc2SubnetDetails_availabilityZoneId,
    awsEc2SubnetDetails_availableIpAddressCount,

    -- * AwsEc2TransitGatewayDetails
    AwsEc2TransitGatewayDetails (..),
    newAwsEc2TransitGatewayDetails,
    awsEc2TransitGatewayDetails_associationDefaultRouteTableId,
    awsEc2TransitGatewayDetails_dnsSupport,
    awsEc2TransitGatewayDetails_defaultRouteTableAssociation,
    awsEc2TransitGatewayDetails_propagationDefaultRouteTableId,
    awsEc2TransitGatewayDetails_id,
    awsEc2TransitGatewayDetails_description,
    awsEc2TransitGatewayDetails_autoAcceptSharedAttachments,
    awsEc2TransitGatewayDetails_multicastSupport,
    awsEc2TransitGatewayDetails_amazonSideAsn,
    awsEc2TransitGatewayDetails_vpnEcmpSupport,
    awsEc2TransitGatewayDetails_defaultRouteTablePropagation,
    awsEc2TransitGatewayDetails_transitGatewayCidrBlocks,

    -- * AwsEc2VolumeAttachment
    AwsEc2VolumeAttachment (..),
    newAwsEc2VolumeAttachment,
    awsEc2VolumeAttachment_deleteOnTermination,
    awsEc2VolumeAttachment_status,
    awsEc2VolumeAttachment_instanceId,
    awsEc2VolumeAttachment_attachTime,

    -- * AwsEc2VolumeDetails
    AwsEc2VolumeDetails (..),
    newAwsEc2VolumeDetails,
    awsEc2VolumeDetails_deviceName,
    awsEc2VolumeDetails_snapshotId,
    awsEc2VolumeDetails_size,
    awsEc2VolumeDetails_volumeType,
    awsEc2VolumeDetails_status,
    awsEc2VolumeDetails_attachments,
    awsEc2VolumeDetails_volumeScanStatus,
    awsEc2VolumeDetails_encrypted,
    awsEc2VolumeDetails_kmsKeyId,
    awsEc2VolumeDetails_volumeId,
    awsEc2VolumeDetails_createTime,

    -- * AwsEc2VpcDetails
    AwsEc2VpcDetails (..),
    newAwsEc2VpcDetails,
    awsEc2VpcDetails_state,
    awsEc2VpcDetails_dhcpOptionsId,
    awsEc2VpcDetails_ipv6CidrBlockAssociationSet,
    awsEc2VpcDetails_cidrBlockAssociationSet,

    -- * AwsEc2VpcEndpointServiceDetails
    AwsEc2VpcEndpointServiceDetails (..),
    newAwsEc2VpcEndpointServiceDetails,
    awsEc2VpcEndpointServiceDetails_gatewayLoadBalancerArns,
    awsEc2VpcEndpointServiceDetails_acceptanceRequired,
    awsEc2VpcEndpointServiceDetails_availabilityZones,
    awsEc2VpcEndpointServiceDetails_serviceType,
    awsEc2VpcEndpointServiceDetails_managesVpcEndpoints,
    awsEc2VpcEndpointServiceDetails_baseEndpointDnsNames,
    awsEc2VpcEndpointServiceDetails_networkLoadBalancerArns,
    awsEc2VpcEndpointServiceDetails_privateDnsName,
    awsEc2VpcEndpointServiceDetails_serviceState,
    awsEc2VpcEndpointServiceDetails_serviceName,
    awsEc2VpcEndpointServiceDetails_serviceId,

    -- * AwsEc2VpcEndpointServiceServiceTypeDetails
    AwsEc2VpcEndpointServiceServiceTypeDetails (..),
    newAwsEc2VpcEndpointServiceServiceTypeDetails,
    awsEc2VpcEndpointServiceServiceTypeDetails_serviceType,

    -- * AwsEc2VpcPeeringConnectionDetails
    AwsEc2VpcPeeringConnectionDetails (..),
    newAwsEc2VpcPeeringConnectionDetails,
    awsEc2VpcPeeringConnectionDetails_requesterVpcInfo,
    awsEc2VpcPeeringConnectionDetails_expirationTime,
    awsEc2VpcPeeringConnectionDetails_accepterVpcInfo,
    awsEc2VpcPeeringConnectionDetails_vpcPeeringConnectionId,
    awsEc2VpcPeeringConnectionDetails_status,

    -- * AwsEc2VpcPeeringConnectionStatusDetails
    AwsEc2VpcPeeringConnectionStatusDetails (..),
    newAwsEc2VpcPeeringConnectionStatusDetails,
    awsEc2VpcPeeringConnectionStatusDetails_message,
    awsEc2VpcPeeringConnectionStatusDetails_code,

    -- * AwsEc2VpcPeeringConnectionVpcInfoDetails
    AwsEc2VpcPeeringConnectionVpcInfoDetails (..),
    newAwsEc2VpcPeeringConnectionVpcInfoDetails,
    awsEc2VpcPeeringConnectionVpcInfoDetails_ownerId,
    awsEc2VpcPeeringConnectionVpcInfoDetails_ipv6CidrBlockSet,
    awsEc2VpcPeeringConnectionVpcInfoDetails_peeringOptions,
    awsEc2VpcPeeringConnectionVpcInfoDetails_region,
    awsEc2VpcPeeringConnectionVpcInfoDetails_cidrBlockSet,
    awsEc2VpcPeeringConnectionVpcInfoDetails_cidrBlock,
    awsEc2VpcPeeringConnectionVpcInfoDetails_vpcId,

    -- * AwsEc2VpnConnectionDetails
    AwsEc2VpnConnectionDetails (..),
    newAwsEc2VpnConnectionDetails,
    awsEc2VpnConnectionDetails_type,
    awsEc2VpnConnectionDetails_transitGatewayId,
    awsEc2VpnConnectionDetails_customerGatewayConfiguration,
    awsEc2VpnConnectionDetails_vpnConnectionId,
    awsEc2VpnConnectionDetails_state,
    awsEc2VpnConnectionDetails_options,
    awsEc2VpnConnectionDetails_customerGatewayId,
    awsEc2VpnConnectionDetails_vpnGatewayId,
    awsEc2VpnConnectionDetails_category,
    awsEc2VpnConnectionDetails_vgwTelemetry,
    awsEc2VpnConnectionDetails_routes,

    -- * AwsEc2VpnConnectionOptionsDetails
    AwsEc2VpnConnectionOptionsDetails (..),
    newAwsEc2VpnConnectionOptionsDetails,
    awsEc2VpnConnectionOptionsDetails_tunnelOptions,
    awsEc2VpnConnectionOptionsDetails_staticRoutesOnly,

    -- * AwsEc2VpnConnectionOptionsTunnelOptionsDetails
    AwsEc2VpnConnectionOptionsTunnelOptionsDetails (..),
    newAwsEc2VpnConnectionOptionsTunnelOptionsDetails,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase1LifetimeSeconds,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase2LifetimeSeconds,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase2EncryptionAlgorithms,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase1DhGroupNumbers,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase1IntegrityAlgorithms,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_dpdTimeoutSeconds,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_ikeVersions,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_preSharedKey,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase2DhGroupNumbers,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_rekeyFuzzPercentage,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_rekeyMarginTimeSeconds,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase2IntegrityAlgorithms,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase1EncryptionAlgorithms,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_tunnelInsideCidr,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_outsideIpAddress,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_replayWindowSize,

    -- * AwsEc2VpnConnectionRoutesDetails
    AwsEc2VpnConnectionRoutesDetails (..),
    newAwsEc2VpnConnectionRoutesDetails,
    awsEc2VpnConnectionRoutesDetails_state,
    awsEc2VpnConnectionRoutesDetails_destinationCidrBlock,

    -- * AwsEc2VpnConnectionVgwTelemetryDetails
    AwsEc2VpnConnectionVgwTelemetryDetails (..),
    newAwsEc2VpnConnectionVgwTelemetryDetails,
    awsEc2VpnConnectionVgwTelemetryDetails_acceptedRouteCount,
    awsEc2VpnConnectionVgwTelemetryDetails_status,
    awsEc2VpnConnectionVgwTelemetryDetails_lastStatusChange,
    awsEc2VpnConnectionVgwTelemetryDetails_certificateArn,
    awsEc2VpnConnectionVgwTelemetryDetails_statusMessage,
    awsEc2VpnConnectionVgwTelemetryDetails_outsideIpAddress,

    -- * AwsEcrContainerImageDetails
    AwsEcrContainerImageDetails (..),
    newAwsEcrContainerImageDetails,
    awsEcrContainerImageDetails_repositoryName,
    awsEcrContainerImageDetails_imageTags,
    awsEcrContainerImageDetails_imagePublishedAt,
    awsEcrContainerImageDetails_registryId,
    awsEcrContainerImageDetails_imageDigest,
    awsEcrContainerImageDetails_architecture,

    -- * AwsEcrRepositoryDetails
    AwsEcrRepositoryDetails (..),
    newAwsEcrRepositoryDetails,
    awsEcrRepositoryDetails_repositoryName,
    awsEcrRepositoryDetails_arn,
    awsEcrRepositoryDetails_repositoryPolicyText,
    awsEcrRepositoryDetails_lifecyclePolicy,
    awsEcrRepositoryDetails_imageTagMutability,
    awsEcrRepositoryDetails_imageScanningConfiguration,

    -- * AwsEcrRepositoryImageScanningConfigurationDetails
    AwsEcrRepositoryImageScanningConfigurationDetails (..),
    newAwsEcrRepositoryImageScanningConfigurationDetails,
    awsEcrRepositoryImageScanningConfigurationDetails_scanOnPush,

    -- * AwsEcrRepositoryLifecyclePolicyDetails
    AwsEcrRepositoryLifecyclePolicyDetails (..),
    newAwsEcrRepositoryLifecyclePolicyDetails,
    awsEcrRepositoryLifecyclePolicyDetails_registryId,
    awsEcrRepositoryLifecyclePolicyDetails_lifecyclePolicyText,

    -- * AwsEcsClusterClusterSettingsDetails
    AwsEcsClusterClusterSettingsDetails (..),
    newAwsEcsClusterClusterSettingsDetails,
    awsEcsClusterClusterSettingsDetails_name,
    awsEcsClusterClusterSettingsDetails_value,

    -- * AwsEcsClusterConfigurationDetails
    AwsEcsClusterConfigurationDetails (..),
    newAwsEcsClusterConfigurationDetails,
    awsEcsClusterConfigurationDetails_executeCommandConfiguration,

    -- * AwsEcsClusterConfigurationExecuteCommandConfigurationDetails
    AwsEcsClusterConfigurationExecuteCommandConfigurationDetails (..),
    newAwsEcsClusterConfigurationExecuteCommandConfigurationDetails,
    awsEcsClusterConfigurationExecuteCommandConfigurationDetails_logConfiguration,
    awsEcsClusterConfigurationExecuteCommandConfigurationDetails_logging,
    awsEcsClusterConfigurationExecuteCommandConfigurationDetails_kmsKeyId,

    -- * AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails
    AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails (..),
    newAwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails,
    awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_s3KeyPrefix,
    awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_s3BucketName,
    awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_s3EncryptionEnabled,
    awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_cloudWatchEncryptionEnabled,
    awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_cloudWatchLogGroupName,

    -- * AwsEcsClusterDefaultCapacityProviderStrategyDetails
    AwsEcsClusterDefaultCapacityProviderStrategyDetails (..),
    newAwsEcsClusterDefaultCapacityProviderStrategyDetails,
    awsEcsClusterDefaultCapacityProviderStrategyDetails_capacityProvider,
    awsEcsClusterDefaultCapacityProviderStrategyDetails_base,
    awsEcsClusterDefaultCapacityProviderStrategyDetails_weight,

    -- * AwsEcsClusterDetails
    AwsEcsClusterDetails (..),
    newAwsEcsClusterDetails,
    awsEcsClusterDetails_clusterArn,
    awsEcsClusterDetails_clusterSettings,
    awsEcsClusterDetails_configuration,
    awsEcsClusterDetails_registeredContainerInstancesCount,
    awsEcsClusterDetails_status,
    awsEcsClusterDetails_runningTasksCount,
    awsEcsClusterDetails_capacityProviders,
    awsEcsClusterDetails_activeServicesCount,
    awsEcsClusterDetails_defaultCapacityProviderStrategy,
    awsEcsClusterDetails_clusterName,

    -- * AwsEcsContainerDetails
    AwsEcsContainerDetails (..),
    newAwsEcsContainerDetails,
    awsEcsContainerDetails_name,
    awsEcsContainerDetails_privileged,
    awsEcsContainerDetails_mountPoints,
    awsEcsContainerDetails_image,

    -- * AwsEcsServiceCapacityProviderStrategyDetails
    AwsEcsServiceCapacityProviderStrategyDetails (..),
    newAwsEcsServiceCapacityProviderStrategyDetails,
    awsEcsServiceCapacityProviderStrategyDetails_capacityProvider,
    awsEcsServiceCapacityProviderStrategyDetails_base,
    awsEcsServiceCapacityProviderStrategyDetails_weight,

    -- * AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails
    AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails (..),
    newAwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails,
    awsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails_enable,
    awsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails_rollback,

    -- * AwsEcsServiceDeploymentConfigurationDetails
    AwsEcsServiceDeploymentConfigurationDetails (..),
    newAwsEcsServiceDeploymentConfigurationDetails,
    awsEcsServiceDeploymentConfigurationDetails_minimumHealthyPercent,
    awsEcsServiceDeploymentConfigurationDetails_maximumPercent,
    awsEcsServiceDeploymentConfigurationDetails_deploymentCircuitBreaker,

    -- * AwsEcsServiceDeploymentControllerDetails
    AwsEcsServiceDeploymentControllerDetails (..),
    newAwsEcsServiceDeploymentControllerDetails,
    awsEcsServiceDeploymentControllerDetails_type,

    -- * AwsEcsServiceDetails
    AwsEcsServiceDetails (..),
    newAwsEcsServiceDetails,
    awsEcsServiceDetails_healthCheckGracePeriodSeconds,
    awsEcsServiceDetails_name,
    awsEcsServiceDetails_deploymentConfiguration,
    awsEcsServiceDetails_schedulingStrategy,
    awsEcsServiceDetails_serviceRegistries,
    awsEcsServiceDetails_cluster,
    awsEcsServiceDetails_placementStrategies,
    awsEcsServiceDetails_taskDefinition,
    awsEcsServiceDetails_networkConfiguration,
    awsEcsServiceDetails_desiredCount,
    awsEcsServiceDetails_enableExecuteCommand,
    awsEcsServiceDetails_capacityProviderStrategy,
    awsEcsServiceDetails_placementConstraints,
    awsEcsServiceDetails_propagateTags,
    awsEcsServiceDetails_deploymentController,
    awsEcsServiceDetails_loadBalancers,
    awsEcsServiceDetails_launchType,
    awsEcsServiceDetails_platformVersion,
    awsEcsServiceDetails_role,
    awsEcsServiceDetails_serviceName,
    awsEcsServiceDetails_enableEcsManagedTags,
    awsEcsServiceDetails_serviceArn,

    -- * AwsEcsServiceLoadBalancersDetails
    AwsEcsServiceLoadBalancersDetails (..),
    newAwsEcsServiceLoadBalancersDetails,
    awsEcsServiceLoadBalancersDetails_containerPort,
    awsEcsServiceLoadBalancersDetails_containerName,
    awsEcsServiceLoadBalancersDetails_loadBalancerName,
    awsEcsServiceLoadBalancersDetails_targetGroupArn,

    -- * AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails
    AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails (..),
    newAwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails,
    awsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails_subnets,
    awsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails_securityGroups,
    awsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails_assignPublicIp,

    -- * AwsEcsServiceNetworkConfigurationDetails
    AwsEcsServiceNetworkConfigurationDetails (..),
    newAwsEcsServiceNetworkConfigurationDetails,
    awsEcsServiceNetworkConfigurationDetails_awsVpcConfiguration,

    -- * AwsEcsServicePlacementConstraintsDetails
    AwsEcsServicePlacementConstraintsDetails (..),
    newAwsEcsServicePlacementConstraintsDetails,
    awsEcsServicePlacementConstraintsDetails_type,
    awsEcsServicePlacementConstraintsDetails_expression,

    -- * AwsEcsServicePlacementStrategiesDetails
    AwsEcsServicePlacementStrategiesDetails (..),
    newAwsEcsServicePlacementStrategiesDetails,
    awsEcsServicePlacementStrategiesDetails_type,
    awsEcsServicePlacementStrategiesDetails_field,

    -- * AwsEcsServiceServiceRegistriesDetails
    AwsEcsServiceServiceRegistriesDetails (..),
    newAwsEcsServiceServiceRegistriesDetails,
    awsEcsServiceServiceRegistriesDetails_port,
    awsEcsServiceServiceRegistriesDetails_containerPort,
    awsEcsServiceServiceRegistriesDetails_containerName,
    awsEcsServiceServiceRegistriesDetails_registryArn,

    -- * AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails
    AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails (..),
    newAwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails,
    awsEcsTaskDefinitionContainerDefinitionsDependsOnDetails_containerName,
    awsEcsTaskDefinitionContainerDefinitionsDependsOnDetails_condition,

    -- * AwsEcsTaskDefinitionContainerDefinitionsDetails
    AwsEcsTaskDefinitionContainerDefinitionsDetails (..),
    newAwsEcsTaskDefinitionContainerDefinitionsDetails,
    awsEcsTaskDefinitionContainerDefinitionsDetails_readonlyRootFilesystem,
    awsEcsTaskDefinitionContainerDefinitionsDetails_healthCheck,
    awsEcsTaskDefinitionContainerDefinitionsDetails_name,
    awsEcsTaskDefinitionContainerDefinitionsDetails_extraHosts,
    awsEcsTaskDefinitionContainerDefinitionsDetails_dependsOn,
    awsEcsTaskDefinitionContainerDefinitionsDetails_logConfiguration,
    awsEcsTaskDefinitionContainerDefinitionsDetails_environment,
    awsEcsTaskDefinitionContainerDefinitionsDetails_resourceRequirements,
    awsEcsTaskDefinitionContainerDefinitionsDetails_cpu,
    awsEcsTaskDefinitionContainerDefinitionsDetails_memory,
    awsEcsTaskDefinitionContainerDefinitionsDetails_startTimeout,
    awsEcsTaskDefinitionContainerDefinitionsDetails_dnsServers,
    awsEcsTaskDefinitionContainerDefinitionsDetails_user,
    awsEcsTaskDefinitionContainerDefinitionsDetails_memoryReservation,
    awsEcsTaskDefinitionContainerDefinitionsDetails_ulimits,
    awsEcsTaskDefinitionContainerDefinitionsDetails_repositoryCredentials,
    awsEcsTaskDefinitionContainerDefinitionsDetails_portMappings,
    awsEcsTaskDefinitionContainerDefinitionsDetails_command,
    awsEcsTaskDefinitionContainerDefinitionsDetails_hostname,
    awsEcsTaskDefinitionContainerDefinitionsDetails_environmentFiles,
    awsEcsTaskDefinitionContainerDefinitionsDetails_secrets,
    awsEcsTaskDefinitionContainerDefinitionsDetails_disableNetworking,
    awsEcsTaskDefinitionContainerDefinitionsDetails_dockerLabels,
    awsEcsTaskDefinitionContainerDefinitionsDetails_volumesFrom,
    awsEcsTaskDefinitionContainerDefinitionsDetails_dnsSearchDomains,
    awsEcsTaskDefinitionContainerDefinitionsDetails_privileged,
    awsEcsTaskDefinitionContainerDefinitionsDetails_stopTimeout,
    awsEcsTaskDefinitionContainerDefinitionsDetails_entryPoint,
    awsEcsTaskDefinitionContainerDefinitionsDetails_links,
    awsEcsTaskDefinitionContainerDefinitionsDetails_mountPoints,
    awsEcsTaskDefinitionContainerDefinitionsDetails_image,
    awsEcsTaskDefinitionContainerDefinitionsDetails_essential,
    awsEcsTaskDefinitionContainerDefinitionsDetails_interactive,
    awsEcsTaskDefinitionContainerDefinitionsDetails_linuxParameters,
    awsEcsTaskDefinitionContainerDefinitionsDetails_dockerSecurityOptions,
    awsEcsTaskDefinitionContainerDefinitionsDetails_pseudoTerminal,
    awsEcsTaskDefinitionContainerDefinitionsDetails_systemControls,
    awsEcsTaskDefinitionContainerDefinitionsDetails_firelensConfiguration,
    awsEcsTaskDefinitionContainerDefinitionsDetails_workingDirectory,

    -- * AwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails
    AwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails (..),
    newAwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails,
    awsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails_name,
    awsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails_value,

    -- * AwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails
    AwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails (..),
    newAwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails,
    awsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails_type,
    awsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails_value,

    -- * AwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails
    AwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails (..),
    newAwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails,
    awsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails_hostname,
    awsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails_ipAddress,

    -- * AwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails
    AwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails (..),
    newAwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails,
    awsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails_type,
    awsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails_options,

    -- * AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails
    AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails (..),
    newAwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails,
    awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_timeout,
    awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_startPeriod,
    awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_interval,
    awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_command,
    awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_retries,

    -- * AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails (..),
    newAwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails_drop,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails_add,

    -- * AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails (..),
    newAwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_devices,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_swappiness,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_tmpfs,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_initProcessEnabled,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_maxSwap,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_capabilities,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_sharedMemorySize,

    -- * AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails (..),
    newAwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails_hostPath,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails_containerPath,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails_permissions,

    -- * AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails (..),
    newAwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails_containerPath,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails_size,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails_mountOptions,

    -- * AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails
    AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails (..),
    newAwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails,
    awsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails_secretOptions,
    awsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails_logDriver,
    awsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails_options,

    -- * AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails
    AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails (..),
    newAwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails,
    awsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails_name,
    awsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails_valueFrom,

    -- * AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails
    AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails (..),
    newAwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails,
    awsEcsTaskDefinitionContainerDefinitionsMountPointsDetails_containerPath,
    awsEcsTaskDefinitionContainerDefinitionsMountPointsDetails_sourceVolume,
    awsEcsTaskDefinitionContainerDefinitionsMountPointsDetails_readOnly,

    -- * AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails
    AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails (..),
    newAwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails,
    awsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails_containerPort,
    awsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails_hostPort,
    awsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails_protocol,

    -- * AwsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails
    AwsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails (..),
    newAwsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails,
    awsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails_credentialsParameter,

    -- * AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails
    AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails (..),
    newAwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails,
    awsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails_type,
    awsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails_value,

    -- * AwsEcsTaskDefinitionContainerDefinitionsSecretsDetails
    AwsEcsTaskDefinitionContainerDefinitionsSecretsDetails (..),
    newAwsEcsTaskDefinitionContainerDefinitionsSecretsDetails,
    awsEcsTaskDefinitionContainerDefinitionsSecretsDetails_name,
    awsEcsTaskDefinitionContainerDefinitionsSecretsDetails_valueFrom,

    -- * AwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails
    AwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails (..),
    newAwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails,
    awsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails_namespace,
    awsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails_value,

    -- * AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails
    AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails (..),
    newAwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails,
    awsEcsTaskDefinitionContainerDefinitionsUlimitsDetails_name,
    awsEcsTaskDefinitionContainerDefinitionsUlimitsDetails_softLimit,
    awsEcsTaskDefinitionContainerDefinitionsUlimitsDetails_hardLimit,

    -- * AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails
    AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails (..),
    newAwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails,
    awsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails_readOnly,
    awsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails_sourceContainer,

    -- * AwsEcsTaskDefinitionDetails
    AwsEcsTaskDefinitionDetails (..),
    newAwsEcsTaskDefinitionDetails,
    awsEcsTaskDefinitionDetails_proxyConfiguration,
    awsEcsTaskDefinitionDetails_pidMode,
    awsEcsTaskDefinitionDetails_cpu,
    awsEcsTaskDefinitionDetails_memory,
    awsEcsTaskDefinitionDetails_taskRoleArn,
    awsEcsTaskDefinitionDetails_inferenceAccelerators,
    awsEcsTaskDefinitionDetails_volumes,
    awsEcsTaskDefinitionDetails_requiresCompatibilities,
    awsEcsTaskDefinitionDetails_placementConstraints,
    awsEcsTaskDefinitionDetails_family,
    awsEcsTaskDefinitionDetails_containerDefinitions,
    awsEcsTaskDefinitionDetails_executionRoleArn,
    awsEcsTaskDefinitionDetails_networkMode,
    awsEcsTaskDefinitionDetails_ipcMode,

    -- * AwsEcsTaskDefinitionInferenceAcceleratorsDetails
    AwsEcsTaskDefinitionInferenceAcceleratorsDetails (..),
    newAwsEcsTaskDefinitionInferenceAcceleratorsDetails,
    awsEcsTaskDefinitionInferenceAcceleratorsDetails_deviceName,
    awsEcsTaskDefinitionInferenceAcceleratorsDetails_deviceType,

    -- * AwsEcsTaskDefinitionPlacementConstraintsDetails
    AwsEcsTaskDefinitionPlacementConstraintsDetails (..),
    newAwsEcsTaskDefinitionPlacementConstraintsDetails,
    awsEcsTaskDefinitionPlacementConstraintsDetails_type,
    awsEcsTaskDefinitionPlacementConstraintsDetails_expression,

    -- * AwsEcsTaskDefinitionProxyConfigurationDetails
    AwsEcsTaskDefinitionProxyConfigurationDetails (..),
    newAwsEcsTaskDefinitionProxyConfigurationDetails,
    awsEcsTaskDefinitionProxyConfigurationDetails_containerName,
    awsEcsTaskDefinitionProxyConfigurationDetails_type,
    awsEcsTaskDefinitionProxyConfigurationDetails_proxyConfigurationProperties,

    -- * AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails
    AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails (..),
    newAwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails,
    awsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails_name,
    awsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails_value,

    -- * AwsEcsTaskDefinitionVolumesDetails
    AwsEcsTaskDefinitionVolumesDetails (..),
    newAwsEcsTaskDefinitionVolumesDetails,
    awsEcsTaskDefinitionVolumesDetails_efsVolumeConfiguration,
    awsEcsTaskDefinitionVolumesDetails_name,
    awsEcsTaskDefinitionVolumesDetails_host,
    awsEcsTaskDefinitionVolumesDetails_dockerVolumeConfiguration,

    -- * AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails
    AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails (..),
    newAwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails,
    awsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails_driverOpts,
    awsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails_autoprovision,
    awsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails_labels,
    awsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails_scope,
    awsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails_driver,

    -- * AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails
    AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails (..),
    newAwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails,
    awsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails_iam,
    awsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails_accessPointId,

    -- * AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails
    AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails (..),
    newAwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails,
    awsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails_transitEncryptionPort,
    awsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails_filesystemId,
    awsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails_rootDirectory,
    awsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails_transitEncryption,
    awsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails_authorizationConfig,

    -- * AwsEcsTaskDefinitionVolumesHostDetails
    AwsEcsTaskDefinitionVolumesHostDetails (..),
    newAwsEcsTaskDefinitionVolumesHostDetails,
    awsEcsTaskDefinitionVolumesHostDetails_sourcePath,

    -- * AwsEcsTaskDetails
    AwsEcsTaskDetails (..),
    newAwsEcsTaskDetails,
    awsEcsTaskDetails_clusterArn,
    awsEcsTaskDetails_containers,
    awsEcsTaskDetails_taskDefinitionArn,
    awsEcsTaskDetails_startedBy,
    awsEcsTaskDetails_volumes,
    awsEcsTaskDetails_startedAt,
    awsEcsTaskDetails_group,
    awsEcsTaskDetails_createdAt,
    awsEcsTaskDetails_version,

    -- * AwsEcsTaskVolumeDetails
    AwsEcsTaskVolumeDetails (..),
    newAwsEcsTaskVolumeDetails,
    awsEcsTaskVolumeDetails_name,
    awsEcsTaskVolumeDetails_host,

    -- * AwsEcsTaskVolumeHostDetails
    AwsEcsTaskVolumeHostDetails (..),
    newAwsEcsTaskVolumeHostDetails,
    awsEcsTaskVolumeHostDetails_sourcePath,

    -- * AwsEfsAccessPointDetails
    AwsEfsAccessPointDetails (..),
    newAwsEfsAccessPointDetails,
    awsEfsAccessPointDetails_clientToken,
    awsEfsAccessPointDetails_posixUser,
    awsEfsAccessPointDetails_arn,
    awsEfsAccessPointDetails_fileSystemId,
    awsEfsAccessPointDetails_accessPointId,
    awsEfsAccessPointDetails_rootDirectory,

    -- * AwsEfsAccessPointPosixUserDetails
    AwsEfsAccessPointPosixUserDetails (..),
    newAwsEfsAccessPointPosixUserDetails,
    awsEfsAccessPointPosixUserDetails_gid,
    awsEfsAccessPointPosixUserDetails_secondaryGids,
    awsEfsAccessPointPosixUserDetails_uid,

    -- * AwsEfsAccessPointRootDirectoryCreationInfoDetails
    AwsEfsAccessPointRootDirectoryCreationInfoDetails (..),
    newAwsEfsAccessPointRootDirectoryCreationInfoDetails,
    awsEfsAccessPointRootDirectoryCreationInfoDetails_permissions,
    awsEfsAccessPointRootDirectoryCreationInfoDetails_ownerGid,
    awsEfsAccessPointRootDirectoryCreationInfoDetails_ownerUid,

    -- * AwsEfsAccessPointRootDirectoryDetails
    AwsEfsAccessPointRootDirectoryDetails (..),
    newAwsEfsAccessPointRootDirectoryDetails,
    awsEfsAccessPointRootDirectoryDetails_creationInfo,
    awsEfsAccessPointRootDirectoryDetails_path,

    -- * AwsEksClusterDetails
    AwsEksClusterDetails (..),
    newAwsEksClusterDetails,
    awsEksClusterDetails_name,
    awsEksClusterDetails_roleArn,
    awsEksClusterDetails_arn,
    awsEksClusterDetails_clusterStatus,
    awsEksClusterDetails_logging,
    awsEksClusterDetails_endpoint,
    awsEksClusterDetails_resourcesVpcConfig,
    awsEksClusterDetails_certificateAuthorityData,
    awsEksClusterDetails_version,

    -- * AwsEksClusterLoggingClusterLoggingDetails
    AwsEksClusterLoggingClusterLoggingDetails (..),
    newAwsEksClusterLoggingClusterLoggingDetails,
    awsEksClusterLoggingClusterLoggingDetails_types,
    awsEksClusterLoggingClusterLoggingDetails_enabled,

    -- * AwsEksClusterLoggingDetails
    AwsEksClusterLoggingDetails (..),
    newAwsEksClusterLoggingDetails,
    awsEksClusterLoggingDetails_clusterLogging,

    -- * AwsEksClusterResourcesVpcConfigDetails
    AwsEksClusterResourcesVpcConfigDetails (..),
    newAwsEksClusterResourcesVpcConfigDetails,
    awsEksClusterResourcesVpcConfigDetails_securityGroupIds,
    awsEksClusterResourcesVpcConfigDetails_subnetIds,

    -- * AwsElasticBeanstalkEnvironmentDetails
    AwsElasticBeanstalkEnvironmentDetails (..),
    newAwsElasticBeanstalkEnvironmentDetails,
    awsElasticBeanstalkEnvironmentDetails_cname,
    awsElasticBeanstalkEnvironmentDetails_environmentName,
    awsElasticBeanstalkEnvironmentDetails_status,
    awsElasticBeanstalkEnvironmentDetails_description,
    awsElasticBeanstalkEnvironmentDetails_tier,
    awsElasticBeanstalkEnvironmentDetails_endpointUrl,
    awsElasticBeanstalkEnvironmentDetails_solutionStackName,
    awsElasticBeanstalkEnvironmentDetails_dateUpdated,
    awsElasticBeanstalkEnvironmentDetails_dateCreated,
    awsElasticBeanstalkEnvironmentDetails_environmentArn,
    awsElasticBeanstalkEnvironmentDetails_environmentLinks,
    awsElasticBeanstalkEnvironmentDetails_platformArn,
    awsElasticBeanstalkEnvironmentDetails_environmentId,
    awsElasticBeanstalkEnvironmentDetails_versionLabel,
    awsElasticBeanstalkEnvironmentDetails_applicationName,
    awsElasticBeanstalkEnvironmentDetails_optionSettings,

    -- * AwsElasticBeanstalkEnvironmentEnvironmentLink
    AwsElasticBeanstalkEnvironmentEnvironmentLink (..),
    newAwsElasticBeanstalkEnvironmentEnvironmentLink,
    awsElasticBeanstalkEnvironmentEnvironmentLink_environmentName,
    awsElasticBeanstalkEnvironmentEnvironmentLink_linkName,

    -- * AwsElasticBeanstalkEnvironmentOptionSetting
    AwsElasticBeanstalkEnvironmentOptionSetting (..),
    newAwsElasticBeanstalkEnvironmentOptionSetting,
    awsElasticBeanstalkEnvironmentOptionSetting_resourceName,
    awsElasticBeanstalkEnvironmentOptionSetting_optionName,
    awsElasticBeanstalkEnvironmentOptionSetting_namespace,
    awsElasticBeanstalkEnvironmentOptionSetting_value,

    -- * AwsElasticBeanstalkEnvironmentTier
    AwsElasticBeanstalkEnvironmentTier (..),
    newAwsElasticBeanstalkEnvironmentTier,
    awsElasticBeanstalkEnvironmentTier_name,
    awsElasticBeanstalkEnvironmentTier_type,
    awsElasticBeanstalkEnvironmentTier_version,

    -- * AwsElasticsearchDomainDetails
    AwsElasticsearchDomainDetails (..),
    newAwsElasticsearchDomainDetails,
    awsElasticsearchDomainDetails_nodeToNodeEncryptionOptions,
    awsElasticsearchDomainDetails_elasticsearchClusterConfig,
    awsElasticsearchDomainDetails_domainName,
    awsElasticsearchDomainDetails_encryptionAtRestOptions,
    awsElasticsearchDomainDetails_endpoints,
    awsElasticsearchDomainDetails_elasticsearchVersion,
    awsElasticsearchDomainDetails_accessPolicies,
    awsElasticsearchDomainDetails_vPCOptions,
    awsElasticsearchDomainDetails_domainEndpointOptions,
    awsElasticsearchDomainDetails_domainId,
    awsElasticsearchDomainDetails_endpoint,
    awsElasticsearchDomainDetails_serviceSoftwareOptions,
    awsElasticsearchDomainDetails_logPublishingOptions,

    -- * AwsElasticsearchDomainDomainEndpointOptions
    AwsElasticsearchDomainDomainEndpointOptions (..),
    newAwsElasticsearchDomainDomainEndpointOptions,
    awsElasticsearchDomainDomainEndpointOptions_tLSSecurityPolicy,
    awsElasticsearchDomainDomainEndpointOptions_enforceHTTPS,

    -- * AwsElasticsearchDomainElasticsearchClusterConfigDetails
    AwsElasticsearchDomainElasticsearchClusterConfigDetails (..),
    newAwsElasticsearchDomainElasticsearchClusterConfigDetails,
    awsElasticsearchDomainElasticsearchClusterConfigDetails_dedicatedMasterType,
    awsElasticsearchDomainElasticsearchClusterConfigDetails_zoneAwarenessEnabled,
    awsElasticsearchDomainElasticsearchClusterConfigDetails_dedicatedMasterEnabled,
    awsElasticsearchDomainElasticsearchClusterConfigDetails_instanceType,
    awsElasticsearchDomainElasticsearchClusterConfigDetails_zoneAwarenessConfig,
    awsElasticsearchDomainElasticsearchClusterConfigDetails_instanceCount,
    awsElasticsearchDomainElasticsearchClusterConfigDetails_dedicatedMasterCount,

    -- * AwsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails
    AwsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails (..),
    newAwsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails,
    awsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails_availabilityZoneCount,

    -- * AwsElasticsearchDomainEncryptionAtRestOptions
    AwsElasticsearchDomainEncryptionAtRestOptions (..),
    newAwsElasticsearchDomainEncryptionAtRestOptions,
    awsElasticsearchDomainEncryptionAtRestOptions_enabled,
    awsElasticsearchDomainEncryptionAtRestOptions_kmsKeyId,

    -- * AwsElasticsearchDomainLogPublishingOptions
    AwsElasticsearchDomainLogPublishingOptions (..),
    newAwsElasticsearchDomainLogPublishingOptions,
    awsElasticsearchDomainLogPublishingOptions_indexSlowLogs,
    awsElasticsearchDomainLogPublishingOptions_auditLogs,
    awsElasticsearchDomainLogPublishingOptions_searchSlowLogs,

    -- * AwsElasticsearchDomainLogPublishingOptionsLogConfig
    AwsElasticsearchDomainLogPublishingOptionsLogConfig (..),
    newAwsElasticsearchDomainLogPublishingOptionsLogConfig,
    awsElasticsearchDomainLogPublishingOptionsLogConfig_enabled,
    awsElasticsearchDomainLogPublishingOptionsLogConfig_cloudWatchLogsLogGroupArn,

    -- * AwsElasticsearchDomainNodeToNodeEncryptionOptions
    AwsElasticsearchDomainNodeToNodeEncryptionOptions (..),
    newAwsElasticsearchDomainNodeToNodeEncryptionOptions,
    awsElasticsearchDomainNodeToNodeEncryptionOptions_enabled,

    -- * AwsElasticsearchDomainServiceSoftwareOptions
    AwsElasticsearchDomainServiceSoftwareOptions (..),
    newAwsElasticsearchDomainServiceSoftwareOptions,
    awsElasticsearchDomainServiceSoftwareOptions_newVersion,
    awsElasticsearchDomainServiceSoftwareOptions_updateAvailable,
    awsElasticsearchDomainServiceSoftwareOptions_cancellable,
    awsElasticsearchDomainServiceSoftwareOptions_updateStatus,
    awsElasticsearchDomainServiceSoftwareOptions_automatedUpdateDate,
    awsElasticsearchDomainServiceSoftwareOptions_description,
    awsElasticsearchDomainServiceSoftwareOptions_currentVersion,

    -- * AwsElasticsearchDomainVPCOptions
    AwsElasticsearchDomainVPCOptions (..),
    newAwsElasticsearchDomainVPCOptions,
    awsElasticsearchDomainVPCOptions_securityGroupIds,
    awsElasticsearchDomainVPCOptions_availabilityZones,
    awsElasticsearchDomainVPCOptions_vPCId,
    awsElasticsearchDomainVPCOptions_subnetIds,

    -- * AwsElbAppCookieStickinessPolicy
    AwsElbAppCookieStickinessPolicy (..),
    newAwsElbAppCookieStickinessPolicy,
    awsElbAppCookieStickinessPolicy_policyName,
    awsElbAppCookieStickinessPolicy_cookieName,

    -- * AwsElbLbCookieStickinessPolicy
    AwsElbLbCookieStickinessPolicy (..),
    newAwsElbLbCookieStickinessPolicy,
    awsElbLbCookieStickinessPolicy_policyName,
    awsElbLbCookieStickinessPolicy_cookieExpirationPeriod,

    -- * AwsElbLoadBalancerAccessLog
    AwsElbLoadBalancerAccessLog (..),
    newAwsElbLoadBalancerAccessLog,
    awsElbLoadBalancerAccessLog_s3BucketPrefix,
    awsElbLoadBalancerAccessLog_s3BucketName,
    awsElbLoadBalancerAccessLog_enabled,
    awsElbLoadBalancerAccessLog_emitInterval,

    -- * AwsElbLoadBalancerAdditionalAttribute
    AwsElbLoadBalancerAdditionalAttribute (..),
    newAwsElbLoadBalancerAdditionalAttribute,
    awsElbLoadBalancerAdditionalAttribute_key,
    awsElbLoadBalancerAdditionalAttribute_value,

    -- * AwsElbLoadBalancerAttributes
    AwsElbLoadBalancerAttributes (..),
    newAwsElbLoadBalancerAttributes,
    awsElbLoadBalancerAttributes_connectionSettings,
    awsElbLoadBalancerAttributes_connectionDraining,
    awsElbLoadBalancerAttributes_additionalAttributes,
    awsElbLoadBalancerAttributes_accessLog,
    awsElbLoadBalancerAttributes_crossZoneLoadBalancing,

    -- * AwsElbLoadBalancerBackendServerDescription
    AwsElbLoadBalancerBackendServerDescription (..),
    newAwsElbLoadBalancerBackendServerDescription,
    awsElbLoadBalancerBackendServerDescription_policyNames,
    awsElbLoadBalancerBackendServerDescription_instancePort,

    -- * AwsElbLoadBalancerConnectionDraining
    AwsElbLoadBalancerConnectionDraining (..),
    newAwsElbLoadBalancerConnectionDraining,
    awsElbLoadBalancerConnectionDraining_timeout,
    awsElbLoadBalancerConnectionDraining_enabled,

    -- * AwsElbLoadBalancerConnectionSettings
    AwsElbLoadBalancerConnectionSettings (..),
    newAwsElbLoadBalancerConnectionSettings,
    awsElbLoadBalancerConnectionSettings_idleTimeout,

    -- * AwsElbLoadBalancerCrossZoneLoadBalancing
    AwsElbLoadBalancerCrossZoneLoadBalancing (..),
    newAwsElbLoadBalancerCrossZoneLoadBalancing,
    awsElbLoadBalancerCrossZoneLoadBalancing_enabled,

    -- * AwsElbLoadBalancerDetails
    AwsElbLoadBalancerDetails (..),
    newAwsElbLoadBalancerDetails,
    awsElbLoadBalancerDetails_instances,
    awsElbLoadBalancerDetails_healthCheck,
    awsElbLoadBalancerDetails_scheme,
    awsElbLoadBalancerDetails_createdTime,
    awsElbLoadBalancerDetails_canonicalHostedZoneName,
    awsElbLoadBalancerDetails_canonicalHostedZoneNameID,
    awsElbLoadBalancerDetails_loadBalancerName,
    awsElbLoadBalancerDetails_subnets,
    awsElbLoadBalancerDetails_sourceSecurityGroup,
    awsElbLoadBalancerDetails_availabilityZones,
    awsElbLoadBalancerDetails_listenerDescriptions,
    awsElbLoadBalancerDetails_policies,
    awsElbLoadBalancerDetails_backendServerDescriptions,
    awsElbLoadBalancerDetails_securityGroups,
    awsElbLoadBalancerDetails_vpcId,
    awsElbLoadBalancerDetails_dnsName,
    awsElbLoadBalancerDetails_loadBalancerAttributes,

    -- * AwsElbLoadBalancerHealthCheck
    AwsElbLoadBalancerHealthCheck (..),
    newAwsElbLoadBalancerHealthCheck,
    awsElbLoadBalancerHealthCheck_timeout,
    awsElbLoadBalancerHealthCheck_interval,
    awsElbLoadBalancerHealthCheck_healthyThreshold,
    awsElbLoadBalancerHealthCheck_target,
    awsElbLoadBalancerHealthCheck_unhealthyThreshold,

    -- * AwsElbLoadBalancerInstance
    AwsElbLoadBalancerInstance (..),
    newAwsElbLoadBalancerInstance,
    awsElbLoadBalancerInstance_instanceId,

    -- * AwsElbLoadBalancerListener
    AwsElbLoadBalancerListener (..),
    newAwsElbLoadBalancerListener,
    awsElbLoadBalancerListener_sslCertificateId,
    awsElbLoadBalancerListener_instanceProtocol,
    awsElbLoadBalancerListener_instancePort,
    awsElbLoadBalancerListener_protocol,
    awsElbLoadBalancerListener_loadBalancerPort,

    -- * AwsElbLoadBalancerListenerDescription
    AwsElbLoadBalancerListenerDescription (..),
    newAwsElbLoadBalancerListenerDescription,
    awsElbLoadBalancerListenerDescription_listener,
    awsElbLoadBalancerListenerDescription_policyNames,

    -- * AwsElbLoadBalancerPolicies
    AwsElbLoadBalancerPolicies (..),
    newAwsElbLoadBalancerPolicies,
    awsElbLoadBalancerPolicies_otherPolicies,
    awsElbLoadBalancerPolicies_lbCookieStickinessPolicies,
    awsElbLoadBalancerPolicies_appCookieStickinessPolicies,

    -- * AwsElbLoadBalancerSourceSecurityGroup
    AwsElbLoadBalancerSourceSecurityGroup (..),
    newAwsElbLoadBalancerSourceSecurityGroup,
    awsElbLoadBalancerSourceSecurityGroup_ownerAlias,
    awsElbLoadBalancerSourceSecurityGroup_groupName,

    -- * AwsElbv2LoadBalancerAttribute
    AwsElbv2LoadBalancerAttribute (..),
    newAwsElbv2LoadBalancerAttribute,
    awsElbv2LoadBalancerAttribute_key,
    awsElbv2LoadBalancerAttribute_value,

    -- * AwsElbv2LoadBalancerDetails
    AwsElbv2LoadBalancerDetails (..),
    newAwsElbv2LoadBalancerDetails,
    awsElbv2LoadBalancerDetails_scheme,
    awsElbv2LoadBalancerDetails_type,
    awsElbv2LoadBalancerDetails_createdTime,
    awsElbv2LoadBalancerDetails_availabilityZones,
    awsElbv2LoadBalancerDetails_state,
    awsElbv2LoadBalancerDetails_securityGroups,
    awsElbv2LoadBalancerDetails_dNSName,
    awsElbv2LoadBalancerDetails_vpcId,
    awsElbv2LoadBalancerDetails_loadBalancerAttributes,
    awsElbv2LoadBalancerDetails_canonicalHostedZoneId,
    awsElbv2LoadBalancerDetails_ipAddressType,

    -- * AwsIamAccessKeyDetails
    AwsIamAccessKeyDetails (..),
    newAwsIamAccessKeyDetails,
    awsIamAccessKeyDetails_principalId,
    awsIamAccessKeyDetails_principalName,
    awsIamAccessKeyDetails_userName,
    awsIamAccessKeyDetails_status,
    awsIamAccessKeyDetails_sessionContext,
    awsIamAccessKeyDetails_accountId,
    awsIamAccessKeyDetails_principalType,
    awsIamAccessKeyDetails_createdAt,
    awsIamAccessKeyDetails_accessKeyId,

    -- * AwsIamAccessKeySessionContext
    AwsIamAccessKeySessionContext (..),
    newAwsIamAccessKeySessionContext,
    awsIamAccessKeySessionContext_sessionIssuer,
    awsIamAccessKeySessionContext_attributes,

    -- * AwsIamAccessKeySessionContextAttributes
    AwsIamAccessKeySessionContextAttributes (..),
    newAwsIamAccessKeySessionContextAttributes,
    awsIamAccessKeySessionContextAttributes_mfaAuthenticated,
    awsIamAccessKeySessionContextAttributes_creationDate,

    -- * AwsIamAccessKeySessionContextSessionIssuer
    AwsIamAccessKeySessionContextSessionIssuer (..),
    newAwsIamAccessKeySessionContextSessionIssuer,
    awsIamAccessKeySessionContextSessionIssuer_principalId,
    awsIamAccessKeySessionContextSessionIssuer_type,
    awsIamAccessKeySessionContextSessionIssuer_userName,
    awsIamAccessKeySessionContextSessionIssuer_arn,
    awsIamAccessKeySessionContextSessionIssuer_accountId,

    -- * AwsIamAttachedManagedPolicy
    AwsIamAttachedManagedPolicy (..),
    newAwsIamAttachedManagedPolicy,
    awsIamAttachedManagedPolicy_policyName,
    awsIamAttachedManagedPolicy_policyArn,

    -- * AwsIamGroupDetails
    AwsIamGroupDetails (..),
    newAwsIamGroupDetails,
    awsIamGroupDetails_path,
    awsIamGroupDetails_groupName,
    awsIamGroupDetails_attachedManagedPolicies,
    awsIamGroupDetails_createDate,
    awsIamGroupDetails_groupId,
    awsIamGroupDetails_groupPolicyList,

    -- * AwsIamGroupPolicy
    AwsIamGroupPolicy (..),
    newAwsIamGroupPolicy,
    awsIamGroupPolicy_policyName,

    -- * AwsIamInstanceProfile
    AwsIamInstanceProfile (..),
    newAwsIamInstanceProfile,
    awsIamInstanceProfile_instanceProfileName,
    awsIamInstanceProfile_arn,
    awsIamInstanceProfile_path,
    awsIamInstanceProfile_createDate,
    awsIamInstanceProfile_roles,
    awsIamInstanceProfile_instanceProfileId,

    -- * AwsIamInstanceProfileRole
    AwsIamInstanceProfileRole (..),
    newAwsIamInstanceProfileRole,
    awsIamInstanceProfileRole_roleName,
    awsIamInstanceProfileRole_arn,
    awsIamInstanceProfileRole_path,
    awsIamInstanceProfileRole_assumeRolePolicyDocument,
    awsIamInstanceProfileRole_createDate,
    awsIamInstanceProfileRole_roleId,

    -- * AwsIamPermissionsBoundary
    AwsIamPermissionsBoundary (..),
    newAwsIamPermissionsBoundary,
    awsIamPermissionsBoundary_permissionsBoundaryType,
    awsIamPermissionsBoundary_permissionsBoundaryArn,

    -- * AwsIamPolicyDetails
    AwsIamPolicyDetails (..),
    newAwsIamPolicyDetails,
    awsIamPolicyDetails_policyName,
    awsIamPolicyDetails_policyId,
    awsIamPolicyDetails_defaultVersionId,
    awsIamPolicyDetails_path,
    awsIamPolicyDetails_updateDate,
    awsIamPolicyDetails_policyVersionList,
    awsIamPolicyDetails_description,
    awsIamPolicyDetails_createDate,
    awsIamPolicyDetails_attachmentCount,
    awsIamPolicyDetails_permissionsBoundaryUsageCount,
    awsIamPolicyDetails_isAttachable,

    -- * AwsIamPolicyVersion
    AwsIamPolicyVersion (..),
    newAwsIamPolicyVersion,
    awsIamPolicyVersion_isDefaultVersion,
    awsIamPolicyVersion_createDate,
    awsIamPolicyVersion_versionId,

    -- * AwsIamRoleDetails
    AwsIamRoleDetails (..),
    newAwsIamRoleDetails,
    awsIamRoleDetails_roleName,
    awsIamRoleDetails_instanceProfileList,
    awsIamRoleDetails_path,
    awsIamRoleDetails_assumeRolePolicyDocument,
    awsIamRoleDetails_attachedManagedPolicies,
    awsIamRoleDetails_permissionsBoundary,
    awsIamRoleDetails_createDate,
    awsIamRoleDetails_roleId,
    awsIamRoleDetails_rolePolicyList,
    awsIamRoleDetails_maxSessionDuration,

    -- * AwsIamRolePolicy
    AwsIamRolePolicy (..),
    newAwsIamRolePolicy,
    awsIamRolePolicy_policyName,

    -- * AwsIamUserDetails
    AwsIamUserDetails (..),
    newAwsIamUserDetails,
    awsIamUserDetails_userName,
    awsIamUserDetails_path,
    awsIamUserDetails_groupList,
    awsIamUserDetails_attachedManagedPolicies,
    awsIamUserDetails_permissionsBoundary,
    awsIamUserDetails_userId,
    awsIamUserDetails_createDate,
    awsIamUserDetails_userPolicyList,

    -- * AwsIamUserPolicy
    AwsIamUserPolicy (..),
    newAwsIamUserPolicy,
    awsIamUserPolicy_policyName,

    -- * AwsKinesisStreamDetails
    AwsKinesisStreamDetails (..),
    newAwsKinesisStreamDetails,
    awsKinesisStreamDetails_name,
    awsKinesisStreamDetails_arn,
    awsKinesisStreamDetails_shardCount,
    awsKinesisStreamDetails_retentionPeriodHours,
    awsKinesisStreamDetails_streamEncryption,

    -- * AwsKinesisStreamStreamEncryptionDetails
    AwsKinesisStreamStreamEncryptionDetails (..),
    newAwsKinesisStreamStreamEncryptionDetails,
    awsKinesisStreamStreamEncryptionDetails_encryptionType,
    awsKinesisStreamStreamEncryptionDetails_keyId,

    -- * AwsKmsKeyDetails
    AwsKmsKeyDetails (..),
    newAwsKmsKeyDetails,
    awsKmsKeyDetails_aWSAccountId,
    awsKmsKeyDetails_creationDate,
    awsKmsKeyDetails_description,
    awsKmsKeyDetails_keyRotationStatus,
    awsKmsKeyDetails_keyManager,
    awsKmsKeyDetails_keyState,
    awsKmsKeyDetails_keyId,
    awsKmsKeyDetails_origin,

    -- * AwsLambdaFunctionCode
    AwsLambdaFunctionCode (..),
    newAwsLambdaFunctionCode,
    awsLambdaFunctionCode_s3Bucket,
    awsLambdaFunctionCode_s3Key,
    awsLambdaFunctionCode_zipFile,
    awsLambdaFunctionCode_s3ObjectVersion,

    -- * AwsLambdaFunctionDeadLetterConfig
    AwsLambdaFunctionDeadLetterConfig (..),
    newAwsLambdaFunctionDeadLetterConfig,
    awsLambdaFunctionDeadLetterConfig_targetArn,

    -- * AwsLambdaFunctionDetails
    AwsLambdaFunctionDetails (..),
    newAwsLambdaFunctionDetails,
    awsLambdaFunctionDetails_tracingConfig,
    awsLambdaFunctionDetails_masterArn,
    awsLambdaFunctionDetails_timeout,
    awsLambdaFunctionDetails_memorySize,
    awsLambdaFunctionDetails_codeSha256,
    awsLambdaFunctionDetails_environment,
    awsLambdaFunctionDetails_code,
    awsLambdaFunctionDetails_vpcConfig,
    awsLambdaFunctionDetails_functionName,
    awsLambdaFunctionDetails_runtime,
    awsLambdaFunctionDetails_kmsKeyArn,
    awsLambdaFunctionDetails_handler,
    awsLambdaFunctionDetails_layers,
    awsLambdaFunctionDetails_packageType,
    awsLambdaFunctionDetails_revisionId,
    awsLambdaFunctionDetails_lastModified,
    awsLambdaFunctionDetails_role,
    awsLambdaFunctionDetails_architectures,
    awsLambdaFunctionDetails_version,
    awsLambdaFunctionDetails_deadLetterConfig,

    -- * AwsLambdaFunctionEnvironment
    AwsLambdaFunctionEnvironment (..),
    newAwsLambdaFunctionEnvironment,
    awsLambdaFunctionEnvironment_error,
    awsLambdaFunctionEnvironment_variables,

    -- * AwsLambdaFunctionEnvironmentError
    AwsLambdaFunctionEnvironmentError (..),
    newAwsLambdaFunctionEnvironmentError,
    awsLambdaFunctionEnvironmentError_message,
    awsLambdaFunctionEnvironmentError_errorCode,

    -- * AwsLambdaFunctionLayer
    AwsLambdaFunctionLayer (..),
    newAwsLambdaFunctionLayer,
    awsLambdaFunctionLayer_arn,
    awsLambdaFunctionLayer_codeSize,

    -- * AwsLambdaFunctionTracingConfig
    AwsLambdaFunctionTracingConfig (..),
    newAwsLambdaFunctionTracingConfig,
    awsLambdaFunctionTracingConfig_mode,

    -- * AwsLambdaFunctionVpcConfig
    AwsLambdaFunctionVpcConfig (..),
    newAwsLambdaFunctionVpcConfig,
    awsLambdaFunctionVpcConfig_securityGroupIds,
    awsLambdaFunctionVpcConfig_vpcId,
    awsLambdaFunctionVpcConfig_subnetIds,

    -- * AwsLambdaLayerVersionDetails
    AwsLambdaLayerVersionDetails (..),
    newAwsLambdaLayerVersionDetails,
    awsLambdaLayerVersionDetails_compatibleRuntimes,
    awsLambdaLayerVersionDetails_createdDate,
    awsLambdaLayerVersionDetails_version,

    -- * AwsMountPoint
    AwsMountPoint (..),
    newAwsMountPoint,
    awsMountPoint_containerPath,
    awsMountPoint_sourceVolume,

    -- * AwsNetworkFirewallFirewallDetails
    AwsNetworkFirewallFirewallDetails (..),
    newAwsNetworkFirewallFirewallDetails,
    awsNetworkFirewallFirewallDetails_deleteProtection,
    awsNetworkFirewallFirewallDetails_subnetChangeProtection,
    awsNetworkFirewallFirewallDetails_firewallId,
    awsNetworkFirewallFirewallDetails_description,
    awsNetworkFirewallFirewallDetails_firewallPolicyChangeProtection,
    awsNetworkFirewallFirewallDetails_firewallArn,
    awsNetworkFirewallFirewallDetails_subnetMappings,
    awsNetworkFirewallFirewallDetails_firewallName,
    awsNetworkFirewallFirewallDetails_vpcId,
    awsNetworkFirewallFirewallDetails_firewallPolicyArn,

    -- * AwsNetworkFirewallFirewallPolicyDetails
    AwsNetworkFirewallFirewallPolicyDetails (..),
    newAwsNetworkFirewallFirewallPolicyDetails,
    awsNetworkFirewallFirewallPolicyDetails_firewallPolicyName,
    awsNetworkFirewallFirewallPolicyDetails_firewallPolicyId,
    awsNetworkFirewallFirewallPolicyDetails_description,
    awsNetworkFirewallFirewallPolicyDetails_firewallPolicyArn,
    awsNetworkFirewallFirewallPolicyDetails_firewallPolicy,

    -- * AwsNetworkFirewallFirewallSubnetMappingsDetails
    AwsNetworkFirewallFirewallSubnetMappingsDetails (..),
    newAwsNetworkFirewallFirewallSubnetMappingsDetails,
    awsNetworkFirewallFirewallSubnetMappingsDetails_subnetId,

    -- * AwsNetworkFirewallRuleGroupDetails
    AwsNetworkFirewallRuleGroupDetails (..),
    newAwsNetworkFirewallRuleGroupDetails,
    awsNetworkFirewallRuleGroupDetails_ruleGroupName,
    awsNetworkFirewallRuleGroupDetails_ruleGroup,
    awsNetworkFirewallRuleGroupDetails_type,
    awsNetworkFirewallRuleGroupDetails_description,
    awsNetworkFirewallRuleGroupDetails_ruleGroupId,
    awsNetworkFirewallRuleGroupDetails_ruleGroupArn,
    awsNetworkFirewallRuleGroupDetails_capacity,

    -- * AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails
    AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails (..),
    newAwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails,
    awsOpenSearchServiceDomainAdvancedSecurityOptionsDetails_internalUserDatabaseEnabled,
    awsOpenSearchServiceDomainAdvancedSecurityOptionsDetails_enabled,
    awsOpenSearchServiceDomainAdvancedSecurityOptionsDetails_masterUserOptions,

    -- * AwsOpenSearchServiceDomainClusterConfigDetails
    AwsOpenSearchServiceDomainClusterConfigDetails (..),
    newAwsOpenSearchServiceDomainClusterConfigDetails,
    awsOpenSearchServiceDomainClusterConfigDetails_warmCount,
    awsOpenSearchServiceDomainClusterConfigDetails_dedicatedMasterType,
    awsOpenSearchServiceDomainClusterConfigDetails_zoneAwarenessEnabled,
    awsOpenSearchServiceDomainClusterConfigDetails_dedicatedMasterEnabled,
    awsOpenSearchServiceDomainClusterConfigDetails_warmType,
    awsOpenSearchServiceDomainClusterConfigDetails_instanceType,
    awsOpenSearchServiceDomainClusterConfigDetails_zoneAwarenessConfig,
    awsOpenSearchServiceDomainClusterConfigDetails_instanceCount,
    awsOpenSearchServiceDomainClusterConfigDetails_warmEnabled,
    awsOpenSearchServiceDomainClusterConfigDetails_dedicatedMasterCount,

    -- * AwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails
    AwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails (..),
    newAwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails,
    awsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails_availabilityZoneCount,

    -- * AwsOpenSearchServiceDomainDetails
    AwsOpenSearchServiceDomainDetails (..),
    newAwsOpenSearchServiceDomainDetails,
    awsOpenSearchServiceDomainDetails_nodeToNodeEncryptionOptions,
    awsOpenSearchServiceDomainDetails_clusterConfig,
    awsOpenSearchServiceDomainDetails_advancedSecurityOptions,
    awsOpenSearchServiceDomainDetails_domainName,
    awsOpenSearchServiceDomainDetails_arn,
    awsOpenSearchServiceDomainDetails_encryptionAtRestOptions,
    awsOpenSearchServiceDomainDetails_id,
    awsOpenSearchServiceDomainDetails_accessPolicies,
    awsOpenSearchServiceDomainDetails_vpcOptions,
    awsOpenSearchServiceDomainDetails_domainEndpointOptions,
    awsOpenSearchServiceDomainDetails_domainEndpoints,
    awsOpenSearchServiceDomainDetails_domainEndpoint,
    awsOpenSearchServiceDomainDetails_serviceSoftwareOptions,
    awsOpenSearchServiceDomainDetails_logPublishingOptions,
    awsOpenSearchServiceDomainDetails_engineVersion,

    -- * AwsOpenSearchServiceDomainDomainEndpointOptionsDetails
    AwsOpenSearchServiceDomainDomainEndpointOptionsDetails (..),
    newAwsOpenSearchServiceDomainDomainEndpointOptionsDetails,
    awsOpenSearchServiceDomainDomainEndpointOptionsDetails_customEndpointCertificateArn,
    awsOpenSearchServiceDomainDomainEndpointOptionsDetails_tLSSecurityPolicy,
    awsOpenSearchServiceDomainDomainEndpointOptionsDetails_customEndpointEnabled,
    awsOpenSearchServiceDomainDomainEndpointOptionsDetails_enforceHTTPS,
    awsOpenSearchServiceDomainDomainEndpointOptionsDetails_customEndpoint,

    -- * AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails
    AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails (..),
    newAwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails,
    awsOpenSearchServiceDomainEncryptionAtRestOptionsDetails_enabled,
    awsOpenSearchServiceDomainEncryptionAtRestOptionsDetails_kmsKeyId,

    -- * AwsOpenSearchServiceDomainLogPublishingOption
    AwsOpenSearchServiceDomainLogPublishingOption (..),
    newAwsOpenSearchServiceDomainLogPublishingOption,
    awsOpenSearchServiceDomainLogPublishingOption_enabled,
    awsOpenSearchServiceDomainLogPublishingOption_cloudWatchLogsLogGroupArn,

    -- * AwsOpenSearchServiceDomainLogPublishingOptionsDetails
    AwsOpenSearchServiceDomainLogPublishingOptionsDetails (..),
    newAwsOpenSearchServiceDomainLogPublishingOptionsDetails,
    awsOpenSearchServiceDomainLogPublishingOptionsDetails_indexSlowLogs,
    awsOpenSearchServiceDomainLogPublishingOptionsDetails_auditLogs,
    awsOpenSearchServiceDomainLogPublishingOptionsDetails_searchSlowLogs,

    -- * AwsOpenSearchServiceDomainMasterUserOptionsDetails
    AwsOpenSearchServiceDomainMasterUserOptionsDetails (..),
    newAwsOpenSearchServiceDomainMasterUserOptionsDetails,
    awsOpenSearchServiceDomainMasterUserOptionsDetails_masterUserArn,
    awsOpenSearchServiceDomainMasterUserOptionsDetails_masterUserName,
    awsOpenSearchServiceDomainMasterUserOptionsDetails_masterUserPassword,

    -- * AwsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails
    AwsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails (..),
    newAwsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails,
    awsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails_enabled,

    -- * AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails
    AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails (..),
    newAwsOpenSearchServiceDomainServiceSoftwareOptionsDetails,
    awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_optionalDeployment,
    awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_newVersion,
    awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_updateAvailable,
    awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_cancellable,
    awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_updateStatus,
    awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_automatedUpdateDate,
    awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_description,
    awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_currentVersion,

    -- * AwsOpenSearchServiceDomainVpcOptionsDetails
    AwsOpenSearchServiceDomainVpcOptionsDetails (..),
    newAwsOpenSearchServiceDomainVpcOptionsDetails,
    awsOpenSearchServiceDomainVpcOptionsDetails_securityGroupIds,
    awsOpenSearchServiceDomainVpcOptionsDetails_subnetIds,

    -- * AwsRdsDbClusterAssociatedRole
    AwsRdsDbClusterAssociatedRole (..),
    newAwsRdsDbClusterAssociatedRole,
    awsRdsDbClusterAssociatedRole_roleArn,
    awsRdsDbClusterAssociatedRole_status,

    -- * AwsRdsDbClusterDetails
    AwsRdsDbClusterDetails (..),
    newAwsRdsDbClusterDetails,
    awsRdsDbClusterDetails_port,
    awsRdsDbClusterDetails_hostedZoneId,
    awsRdsDbClusterDetails_preferredBackupWindow,
    awsRdsDbClusterDetails_backupRetentionPeriod,
    awsRdsDbClusterDetails_masterUsername,
    awsRdsDbClusterDetails_copyTagsToSnapshot,
    awsRdsDbClusterDetails_activityStreamStatus,
    awsRdsDbClusterDetails_dbClusterParameterGroup,
    awsRdsDbClusterDetails_dbClusterMembers,
    awsRdsDbClusterDetails_databaseName,
    awsRdsDbClusterDetails_dbClusterIdentifier,
    awsRdsDbClusterDetails_domainMemberships,
    awsRdsDbClusterDetails_availabilityZones,
    awsRdsDbClusterDetails_crossAccountClone,
    awsRdsDbClusterDetails_dbClusterOptionGroupMemberships,
    awsRdsDbClusterDetails_dbSubnetGroup,
    awsRdsDbClusterDetails_status,
    awsRdsDbClusterDetails_httpEndpointEnabled,
    awsRdsDbClusterDetails_customEndpoints,
    awsRdsDbClusterDetails_engineMode,
    awsRdsDbClusterDetails_storageEncrypted,
    awsRdsDbClusterDetails_kmsKeyId,
    awsRdsDbClusterDetails_engine,
    awsRdsDbClusterDetails_allocatedStorage,
    awsRdsDbClusterDetails_readerEndpoint,
    awsRdsDbClusterDetails_deletionProtection,
    awsRdsDbClusterDetails_iamDatabaseAuthenticationEnabled,
    awsRdsDbClusterDetails_preferredMaintenanceWindow,
    awsRdsDbClusterDetails_endpoint,
    awsRdsDbClusterDetails_clusterCreateTime,
    awsRdsDbClusterDetails_readReplicaIdentifiers,
    awsRdsDbClusterDetails_enabledCloudWatchLogsExports,
    awsRdsDbClusterDetails_dbClusterResourceId,
    awsRdsDbClusterDetails_associatedRoles,
    awsRdsDbClusterDetails_multiAz,
    awsRdsDbClusterDetails_engineVersion,
    awsRdsDbClusterDetails_vpcSecurityGroups,

    -- * AwsRdsDbClusterMember
    AwsRdsDbClusterMember (..),
    newAwsRdsDbClusterMember,
    awsRdsDbClusterMember_promotionTier,
    awsRdsDbClusterMember_dbInstanceIdentifier,
    awsRdsDbClusterMember_dbClusterParameterGroupStatus,
    awsRdsDbClusterMember_isClusterWriter,

    -- * AwsRdsDbClusterOptionGroupMembership
    AwsRdsDbClusterOptionGroupMembership (..),
    newAwsRdsDbClusterOptionGroupMembership,
    awsRdsDbClusterOptionGroupMembership_status,
    awsRdsDbClusterOptionGroupMembership_dbClusterOptionGroupName,

    -- * AwsRdsDbClusterSnapshotDetails
    AwsRdsDbClusterSnapshotDetails (..),
    newAwsRdsDbClusterSnapshotDetails,
    awsRdsDbClusterSnapshotDetails_port,
    awsRdsDbClusterSnapshotDetails_percentProgress,
    awsRdsDbClusterSnapshotDetails_masterUsername,
    awsRdsDbClusterSnapshotDetails_dbClusterSnapshotIdentifier,
    awsRdsDbClusterSnapshotDetails_dbClusterIdentifier,
    awsRdsDbClusterSnapshotDetails_availabilityZones,
    awsRdsDbClusterSnapshotDetails_status,
    awsRdsDbClusterSnapshotDetails_snapshotCreateTime,
    awsRdsDbClusterSnapshotDetails_storageEncrypted,
    awsRdsDbClusterSnapshotDetails_kmsKeyId,
    awsRdsDbClusterSnapshotDetails_engine,
    awsRdsDbClusterSnapshotDetails_allocatedStorage,
    awsRdsDbClusterSnapshotDetails_iamDatabaseAuthenticationEnabled,
    awsRdsDbClusterSnapshotDetails_vpcId,
    awsRdsDbClusterSnapshotDetails_clusterCreateTime,
    awsRdsDbClusterSnapshotDetails_engineVersion,
    awsRdsDbClusterSnapshotDetails_licenseModel,
    awsRdsDbClusterSnapshotDetails_snapshotType,

    -- * AwsRdsDbDomainMembership
    AwsRdsDbDomainMembership (..),
    newAwsRdsDbDomainMembership,
    awsRdsDbDomainMembership_domain,
    awsRdsDbDomainMembership_fqdn,
    awsRdsDbDomainMembership_status,
    awsRdsDbDomainMembership_iamRoleName,

    -- * AwsRdsDbInstanceAssociatedRole
    AwsRdsDbInstanceAssociatedRole (..),
    newAwsRdsDbInstanceAssociatedRole,
    awsRdsDbInstanceAssociatedRole_roleArn,
    awsRdsDbInstanceAssociatedRole_featureName,
    awsRdsDbInstanceAssociatedRole_status,

    -- * AwsRdsDbInstanceDetails
    AwsRdsDbInstanceDetails (..),
    newAwsRdsDbInstanceDetails,
    awsRdsDbInstanceDetails_maxAllocatedStorage,
    awsRdsDbInstanceDetails_listenerEndpoint,
    awsRdsDbInstanceDetails_performanceInsightsRetentionPeriod,
    awsRdsDbInstanceDetails_optionGroupMemberships,
    awsRdsDbInstanceDetails_dbInstanceStatus,
    awsRdsDbInstanceDetails_preferredBackupWindow,
    awsRdsDbInstanceDetails_backupRetentionPeriod,
    awsRdsDbInstanceDetails_dbInstanceClass,
    awsRdsDbInstanceDetails_characterSetName,
    awsRdsDbInstanceDetails_masterUsername,
    awsRdsDbInstanceDetails_copyTagsToSnapshot,
    awsRdsDbInstanceDetails_promotionTier,
    awsRdsDbInstanceDetails_secondaryAvailabilityZone,
    awsRdsDbInstanceDetails_autoMinorVersionUpgrade,
    awsRdsDbInstanceDetails_dbInstanceIdentifier,
    awsRdsDbInstanceDetails_dbInstancePort,
    awsRdsDbInstanceDetails_latestRestorableTime,
    awsRdsDbInstanceDetails_readReplicaDBInstanceIdentifiers,
    awsRdsDbInstanceDetails_enhancedMonitoringResourceArn,
    awsRdsDbInstanceDetails_dbClusterIdentifier,
    awsRdsDbInstanceDetails_domainMemberships,
    awsRdsDbInstanceDetails_timezone,
    awsRdsDbInstanceDetails_performanceInsightsKmsKeyId,
    awsRdsDbInstanceDetails_dbSecurityGroups,
    awsRdsDbInstanceDetails_dbSubnetGroup,
    awsRdsDbInstanceDetails_monitoringInterval,
    awsRdsDbInstanceDetails_performanceInsightsEnabled,
    awsRdsDbInstanceDetails_instanceCreateTime,
    awsRdsDbInstanceDetails_availabilityZone,
    awsRdsDbInstanceDetails_publiclyAccessible,
    awsRdsDbInstanceDetails_storageType,
    awsRdsDbInstanceDetails_processorFeatures,
    awsRdsDbInstanceDetails_readReplicaDBClusterIdentifiers,
    awsRdsDbInstanceDetails_tdeCredentialArn,
    awsRdsDbInstanceDetails_cACertificateIdentifier,
    awsRdsDbInstanceDetails_monitoringRoleArn,
    awsRdsDbInstanceDetails_storageEncrypted,
    awsRdsDbInstanceDetails_kmsKeyId,
    awsRdsDbInstanceDetails_engine,
    awsRdsDbInstanceDetails_allocatedStorage,
    awsRdsDbInstanceDetails_iAMDatabaseAuthenticationEnabled,
    awsRdsDbInstanceDetails_deletionProtection,
    awsRdsDbInstanceDetails_pendingModifiedValues,
    awsRdsDbInstanceDetails_preferredMaintenanceWindow,
    awsRdsDbInstanceDetails_endpoint,
    awsRdsDbInstanceDetails_dbiResourceId,
    awsRdsDbInstanceDetails_dbParameterGroups,
    awsRdsDbInstanceDetails_enabledCloudWatchLogsExports,
    awsRdsDbInstanceDetails_iops,
    awsRdsDbInstanceDetails_associatedRoles,
    awsRdsDbInstanceDetails_multiAz,
    awsRdsDbInstanceDetails_engineVersion,
    awsRdsDbInstanceDetails_dbName,
    awsRdsDbInstanceDetails_readReplicaSourceDBInstanceIdentifier,
    awsRdsDbInstanceDetails_licenseModel,
    awsRdsDbInstanceDetails_statusInfos,
    awsRdsDbInstanceDetails_vpcSecurityGroups,

    -- * AwsRdsDbInstanceEndpoint
    AwsRdsDbInstanceEndpoint (..),
    newAwsRdsDbInstanceEndpoint,
    awsRdsDbInstanceEndpoint_port,
    awsRdsDbInstanceEndpoint_hostedZoneId,
    awsRdsDbInstanceEndpoint_address,

    -- * AwsRdsDbInstanceVpcSecurityGroup
    AwsRdsDbInstanceVpcSecurityGroup (..),
    newAwsRdsDbInstanceVpcSecurityGroup,
    awsRdsDbInstanceVpcSecurityGroup_status,
    awsRdsDbInstanceVpcSecurityGroup_vpcSecurityGroupId,

    -- * AwsRdsDbOptionGroupMembership
    AwsRdsDbOptionGroupMembership (..),
    newAwsRdsDbOptionGroupMembership,
    awsRdsDbOptionGroupMembership_optionGroupName,
    awsRdsDbOptionGroupMembership_status,

    -- * AwsRdsDbParameterGroup
    AwsRdsDbParameterGroup (..),
    newAwsRdsDbParameterGroup,
    awsRdsDbParameterGroup_dbParameterGroupName,
    awsRdsDbParameterGroup_parameterApplyStatus,

    -- * AwsRdsDbPendingModifiedValues
    AwsRdsDbPendingModifiedValues (..),
    newAwsRdsDbPendingModifiedValues,
    awsRdsDbPendingModifiedValues_port,
    awsRdsDbPendingModifiedValues_backupRetentionPeriod,
    awsRdsDbPendingModifiedValues_dbInstanceClass,
    awsRdsDbPendingModifiedValues_dbSubnetGroupName,
    awsRdsDbPendingModifiedValues_dbInstanceIdentifier,
    awsRdsDbPendingModifiedValues_pendingCloudWatchLogsExports,
    awsRdsDbPendingModifiedValues_masterUserPassword,
    awsRdsDbPendingModifiedValues_storageType,
    awsRdsDbPendingModifiedValues_processorFeatures,
    awsRdsDbPendingModifiedValues_caCertificateIdentifier,
    awsRdsDbPendingModifiedValues_allocatedStorage,
    awsRdsDbPendingModifiedValues_iops,
    awsRdsDbPendingModifiedValues_engineVersion,
    awsRdsDbPendingModifiedValues_multiAZ,
    awsRdsDbPendingModifiedValues_licenseModel,

    -- * AwsRdsDbProcessorFeature
    AwsRdsDbProcessorFeature (..),
    newAwsRdsDbProcessorFeature,
    awsRdsDbProcessorFeature_name,
    awsRdsDbProcessorFeature_value,

    -- * AwsRdsDbSecurityGroupDetails
    AwsRdsDbSecurityGroupDetails (..),
    newAwsRdsDbSecurityGroupDetails,
    awsRdsDbSecurityGroupDetails_ownerId,
    awsRdsDbSecurityGroupDetails_dbSecurityGroupDescription,
    awsRdsDbSecurityGroupDetails_dbSecurityGroupName,
    awsRdsDbSecurityGroupDetails_ec2SecurityGroups,
    awsRdsDbSecurityGroupDetails_dbSecurityGroupArn,
    awsRdsDbSecurityGroupDetails_ipRanges,
    awsRdsDbSecurityGroupDetails_vpcId,

    -- * AwsRdsDbSecurityGroupEc2SecurityGroup
    AwsRdsDbSecurityGroupEc2SecurityGroup (..),
    newAwsRdsDbSecurityGroupEc2SecurityGroup,
    awsRdsDbSecurityGroupEc2SecurityGroup_ec2SecurityGroupId,
    awsRdsDbSecurityGroupEc2SecurityGroup_ec2SecurityGroupOwnerId,
    awsRdsDbSecurityGroupEc2SecurityGroup_status,
    awsRdsDbSecurityGroupEc2SecurityGroup_ec2SecurityGroupName,

    -- * AwsRdsDbSecurityGroupIpRange
    AwsRdsDbSecurityGroupIpRange (..),
    newAwsRdsDbSecurityGroupIpRange,
    awsRdsDbSecurityGroupIpRange_status,
    awsRdsDbSecurityGroupIpRange_cidrIp,

    -- * AwsRdsDbSnapshotDetails
    AwsRdsDbSnapshotDetails (..),
    newAwsRdsDbSnapshotDetails,
    awsRdsDbSnapshotDetails_port,
    awsRdsDbSnapshotDetails_percentProgress,
    awsRdsDbSnapshotDetails_masterUsername,
    awsRdsDbSnapshotDetails_sourceRegion,
    awsRdsDbSnapshotDetails_dbInstanceIdentifier,
    awsRdsDbSnapshotDetails_dbSnapshotIdentifier,
    awsRdsDbSnapshotDetails_sourceDbSnapshotIdentifier,
    awsRdsDbSnapshotDetails_optionGroupName,
    awsRdsDbSnapshotDetails_timezone,
    awsRdsDbSnapshotDetails_instanceCreateTime,
    awsRdsDbSnapshotDetails_status,
    awsRdsDbSnapshotDetails_availabilityZone,
    awsRdsDbSnapshotDetails_snapshotCreateTime,
    awsRdsDbSnapshotDetails_storageType,
    awsRdsDbSnapshotDetails_processorFeatures,
    awsRdsDbSnapshotDetails_tdeCredentialArn,
    awsRdsDbSnapshotDetails_encrypted,
    awsRdsDbSnapshotDetails_kmsKeyId,
    awsRdsDbSnapshotDetails_engine,
    awsRdsDbSnapshotDetails_allocatedStorage,
    awsRdsDbSnapshotDetails_iamDatabaseAuthenticationEnabled,
    awsRdsDbSnapshotDetails_vpcId,
    awsRdsDbSnapshotDetails_dbiResourceId,
    awsRdsDbSnapshotDetails_iops,
    awsRdsDbSnapshotDetails_engineVersion,
    awsRdsDbSnapshotDetails_licenseModel,
    awsRdsDbSnapshotDetails_snapshotType,

    -- * AwsRdsDbStatusInfo
    AwsRdsDbStatusInfo (..),
    newAwsRdsDbStatusInfo,
    awsRdsDbStatusInfo_message,
    awsRdsDbStatusInfo_status,
    awsRdsDbStatusInfo_normal,
    awsRdsDbStatusInfo_statusType,

    -- * AwsRdsDbSubnetGroup
    AwsRdsDbSubnetGroup (..),
    newAwsRdsDbSubnetGroup,
    awsRdsDbSubnetGroup_dbSubnetGroupName,
    awsRdsDbSubnetGroup_subnetGroupStatus,
    awsRdsDbSubnetGroup_subnets,
    awsRdsDbSubnetGroup_dbSubnetGroupDescription,
    awsRdsDbSubnetGroup_dbSubnetGroupArn,
    awsRdsDbSubnetGroup_vpcId,

    -- * AwsRdsDbSubnetGroupSubnet
    AwsRdsDbSubnetGroupSubnet (..),
    newAwsRdsDbSubnetGroupSubnet,
    awsRdsDbSubnetGroupSubnet_subnetIdentifier,
    awsRdsDbSubnetGroupSubnet_subnetStatus,
    awsRdsDbSubnetGroupSubnet_subnetAvailabilityZone,

    -- * AwsRdsDbSubnetGroupSubnetAvailabilityZone
    AwsRdsDbSubnetGroupSubnetAvailabilityZone (..),
    newAwsRdsDbSubnetGroupSubnetAvailabilityZone,
    awsRdsDbSubnetGroupSubnetAvailabilityZone_name,

    -- * AwsRdsEventSubscriptionDetails
    AwsRdsEventSubscriptionDetails (..),
    newAwsRdsEventSubscriptionDetails,
    awsRdsEventSubscriptionDetails_subscriptionCreationTime,
    awsRdsEventSubscriptionDetails_custSubscriptionId,
    awsRdsEventSubscriptionDetails_sourceIdsList,
    awsRdsEventSubscriptionDetails_status,
    awsRdsEventSubscriptionDetails_sourceType,
    awsRdsEventSubscriptionDetails_enabled,
    awsRdsEventSubscriptionDetails_snsTopicArn,
    awsRdsEventSubscriptionDetails_eventCategoriesList,
    awsRdsEventSubscriptionDetails_eventSubscriptionArn,
    awsRdsEventSubscriptionDetails_customerAwsId,

    -- * AwsRdsPendingCloudWatchLogsExports
    AwsRdsPendingCloudWatchLogsExports (..),
    newAwsRdsPendingCloudWatchLogsExports,
    awsRdsPendingCloudWatchLogsExports_logTypesToEnable,
    awsRdsPendingCloudWatchLogsExports_logTypesToDisable,

    -- * AwsRedshiftClusterClusterNode
    AwsRedshiftClusterClusterNode (..),
    newAwsRedshiftClusterClusterNode,
    awsRedshiftClusterClusterNode_nodeRole,
    awsRedshiftClusterClusterNode_publicIpAddress,
    awsRedshiftClusterClusterNode_privateIpAddress,

    -- * AwsRedshiftClusterClusterParameterGroup
    AwsRedshiftClusterClusterParameterGroup (..),
    newAwsRedshiftClusterClusterParameterGroup,
    awsRedshiftClusterClusterParameterGroup_parameterGroupName,
    awsRedshiftClusterClusterParameterGroup_clusterParameterStatusList,
    awsRedshiftClusterClusterParameterGroup_parameterApplyStatus,

    -- * AwsRedshiftClusterClusterParameterStatus
    AwsRedshiftClusterClusterParameterStatus (..),
    newAwsRedshiftClusterClusterParameterStatus,
    awsRedshiftClusterClusterParameterStatus_parameterApplyErrorDescription,
    awsRedshiftClusterClusterParameterStatus_parameterName,
    awsRedshiftClusterClusterParameterStatus_parameterApplyStatus,

    -- * AwsRedshiftClusterClusterSecurityGroup
    AwsRedshiftClusterClusterSecurityGroup (..),
    newAwsRedshiftClusterClusterSecurityGroup,
    awsRedshiftClusterClusterSecurityGroup_clusterSecurityGroupName,
    awsRedshiftClusterClusterSecurityGroup_status,

    -- * AwsRedshiftClusterClusterSnapshotCopyStatus
    AwsRedshiftClusterClusterSnapshotCopyStatus (..),
    newAwsRedshiftClusterClusterSnapshotCopyStatus,
    awsRedshiftClusterClusterSnapshotCopyStatus_manualSnapshotRetentionPeriod,
    awsRedshiftClusterClusterSnapshotCopyStatus_snapshotCopyGrantName,
    awsRedshiftClusterClusterSnapshotCopyStatus_retentionPeriod,
    awsRedshiftClusterClusterSnapshotCopyStatus_destinationRegion,

    -- * AwsRedshiftClusterDeferredMaintenanceWindow
    AwsRedshiftClusterDeferredMaintenanceWindow (..),
    newAwsRedshiftClusterDeferredMaintenanceWindow,
    awsRedshiftClusterDeferredMaintenanceWindow_deferMaintenanceIdentifier,
    awsRedshiftClusterDeferredMaintenanceWindow_deferMaintenanceEndTime,
    awsRedshiftClusterDeferredMaintenanceWindow_deferMaintenanceStartTime,

    -- * AwsRedshiftClusterDetails
    AwsRedshiftClusterDetails (..),
    newAwsRedshiftClusterDetails,
    awsRedshiftClusterDetails_clusterIdentifier,
    awsRedshiftClusterDetails_clusterPublicKey,
    awsRedshiftClusterDetails_manualSnapshotRetentionPeriod,
    awsRedshiftClusterDetails_masterUsername,
    awsRedshiftClusterDetails_clusterParameterGroups,
    awsRedshiftClusterDetails_allowVersionUpgrade,
    awsRedshiftClusterDetails_expectedNextSnapshotScheduleTimeStatus,
    awsRedshiftClusterDetails_clusterSubnetGroupName,
    awsRedshiftClusterDetails_snapshotScheduleIdentifier,
    awsRedshiftClusterDetails_nextMaintenanceWindowStartTime,
    awsRedshiftClusterDetails_elasticIpStatus,
    awsRedshiftClusterDetails_clusterNodes,
    awsRedshiftClusterDetails_clusterVersion,
    awsRedshiftClusterDetails_loggingStatus,
    awsRedshiftClusterDetails_pendingActions,
    awsRedshiftClusterDetails_clusterRevisionNumber,
    awsRedshiftClusterDetails_maintenanceTrackName,
    awsRedshiftClusterDetails_iamRoles,
    awsRedshiftClusterDetails_availabilityZone,
    awsRedshiftClusterDetails_nodeType,
    awsRedshiftClusterDetails_publiclyAccessible,
    awsRedshiftClusterDetails_clusterSnapshotCopyStatus,
    awsRedshiftClusterDetails_snapshotScheduleState,
    awsRedshiftClusterDetails_hsmStatus,
    awsRedshiftClusterDetails_clusterStatus,
    awsRedshiftClusterDetails_resizeInfo,
    awsRedshiftClusterDetails_deferredMaintenanceWindows,
    awsRedshiftClusterDetails_encrypted,
    awsRedshiftClusterDetails_numberOfNodes,
    awsRedshiftClusterDetails_kmsKeyId,
    awsRedshiftClusterDetails_expectedNextSnapshotScheduleTime,
    awsRedshiftClusterDetails_elasticResizeNumberOfNodeOptions,
    awsRedshiftClusterDetails_pendingModifiedValues,
    awsRedshiftClusterDetails_enhancedVpcRouting,
    awsRedshiftClusterDetails_preferredMaintenanceWindow,
    awsRedshiftClusterDetails_clusterSecurityGroups,
    awsRedshiftClusterDetails_endpoint,
    awsRedshiftClusterDetails_vpcId,
    awsRedshiftClusterDetails_automatedSnapshotRetentionPeriod,
    awsRedshiftClusterDetails_clusterCreateTime,
    awsRedshiftClusterDetails_dbName,
    awsRedshiftClusterDetails_restoreStatus,
    awsRedshiftClusterDetails_clusterAvailabilityStatus,
    awsRedshiftClusterDetails_vpcSecurityGroups,

    -- * AwsRedshiftClusterElasticIpStatus
    AwsRedshiftClusterElasticIpStatus (..),
    newAwsRedshiftClusterElasticIpStatus,
    awsRedshiftClusterElasticIpStatus_elasticIp,
    awsRedshiftClusterElasticIpStatus_status,

    -- * AwsRedshiftClusterEndpoint
    AwsRedshiftClusterEndpoint (..),
    newAwsRedshiftClusterEndpoint,
    awsRedshiftClusterEndpoint_port,
    awsRedshiftClusterEndpoint_address,

    -- * AwsRedshiftClusterHsmStatus
    AwsRedshiftClusterHsmStatus (..),
    newAwsRedshiftClusterHsmStatus,
    awsRedshiftClusterHsmStatus_status,
    awsRedshiftClusterHsmStatus_hsmClientCertificateIdentifier,
    awsRedshiftClusterHsmStatus_hsmConfigurationIdentifier,

    -- * AwsRedshiftClusterIamRole
    AwsRedshiftClusterIamRole (..),
    newAwsRedshiftClusterIamRole,
    awsRedshiftClusterIamRole_iamRoleArn,
    awsRedshiftClusterIamRole_applyStatus,

    -- * AwsRedshiftClusterLoggingStatus
    AwsRedshiftClusterLoggingStatus (..),
    newAwsRedshiftClusterLoggingStatus,
    awsRedshiftClusterLoggingStatus_lastSuccessfulDeliveryTime,
    awsRedshiftClusterLoggingStatus_s3KeyPrefix,
    awsRedshiftClusterLoggingStatus_lastFailureMessage,
    awsRedshiftClusterLoggingStatus_loggingEnabled,
    awsRedshiftClusterLoggingStatus_bucketName,
    awsRedshiftClusterLoggingStatus_lastFailureTime,

    -- * AwsRedshiftClusterPendingModifiedValues
    AwsRedshiftClusterPendingModifiedValues (..),
    newAwsRedshiftClusterPendingModifiedValues,
    awsRedshiftClusterPendingModifiedValues_clusterIdentifier,
    awsRedshiftClusterPendingModifiedValues_clusterVersion,
    awsRedshiftClusterPendingModifiedValues_maintenanceTrackName,
    awsRedshiftClusterPendingModifiedValues_masterUserPassword,
    awsRedshiftClusterPendingModifiedValues_nodeType,
    awsRedshiftClusterPendingModifiedValues_publiclyAccessible,
    awsRedshiftClusterPendingModifiedValues_encryptionType,
    awsRedshiftClusterPendingModifiedValues_numberOfNodes,
    awsRedshiftClusterPendingModifiedValues_enhancedVpcRouting,
    awsRedshiftClusterPendingModifiedValues_clusterType,
    awsRedshiftClusterPendingModifiedValues_automatedSnapshotRetentionPeriod,

    -- * AwsRedshiftClusterResizeInfo
    AwsRedshiftClusterResizeInfo (..),
    newAwsRedshiftClusterResizeInfo,
    awsRedshiftClusterResizeInfo_allowCancelResize,
    awsRedshiftClusterResizeInfo_resizeType,

    -- * AwsRedshiftClusterRestoreStatus
    AwsRedshiftClusterRestoreStatus (..),
    newAwsRedshiftClusterRestoreStatus,
    awsRedshiftClusterRestoreStatus_snapshotSizeInMegaBytes,
    awsRedshiftClusterRestoreStatus_currentRestoreRateInMegaBytesPerSecond,
    awsRedshiftClusterRestoreStatus_status,
    awsRedshiftClusterRestoreStatus_elapsedTimeInSeconds,
    awsRedshiftClusterRestoreStatus_progressInMegaBytes,
    awsRedshiftClusterRestoreStatus_estimatedTimeToCompletionInSeconds,

    -- * AwsRedshiftClusterVpcSecurityGroup
    AwsRedshiftClusterVpcSecurityGroup (..),
    newAwsRedshiftClusterVpcSecurityGroup,
    awsRedshiftClusterVpcSecurityGroup_status,
    awsRedshiftClusterVpcSecurityGroup_vpcSecurityGroupId,

    -- * AwsS3AccountPublicAccessBlockDetails
    AwsS3AccountPublicAccessBlockDetails (..),
    newAwsS3AccountPublicAccessBlockDetails,
    awsS3AccountPublicAccessBlockDetails_restrictPublicBuckets,
    awsS3AccountPublicAccessBlockDetails_ignorePublicAcls,
    awsS3AccountPublicAccessBlockDetails_blockPublicPolicy,
    awsS3AccountPublicAccessBlockDetails_blockPublicAcls,

    -- * AwsS3BucketBucketLifecycleConfigurationDetails
    AwsS3BucketBucketLifecycleConfigurationDetails (..),
    newAwsS3BucketBucketLifecycleConfigurationDetails,
    awsS3BucketBucketLifecycleConfigurationDetails_rules,

    -- * AwsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails
    AwsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails (..),
    newAwsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails,
    awsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails_daysAfterInitiation,

    -- * AwsS3BucketBucketLifecycleConfigurationRulesDetails
    AwsS3BucketBucketLifecycleConfigurationRulesDetails (..),
    newAwsS3BucketBucketLifecycleConfigurationRulesDetails,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_transitions,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_expiredObjectDeleteMarker,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_status,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_id,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_filter,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_expirationInDays,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_noncurrentVersionTransitions,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_abortIncompleteMultipartUpload,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_prefix,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_expirationDate,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_noncurrentVersionExpirationInDays,

    -- * AwsS3BucketBucketLifecycleConfigurationRulesFilterDetails
    AwsS3BucketBucketLifecycleConfigurationRulesFilterDetails (..),
    newAwsS3BucketBucketLifecycleConfigurationRulesFilterDetails,
    awsS3BucketBucketLifecycleConfigurationRulesFilterDetails_predicate,

    -- * AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails (..),
    newAwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails,
    awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_type,
    awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_tag,
    awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_prefix,
    awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_operands,

    -- * AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails (..),
    newAwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails,
    awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails_type,
    awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails_tag,
    awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails_prefix,

    -- * AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails (..),
    newAwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails,
    awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails_key,
    awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails_value,

    -- * AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails (..),
    newAwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails,
    awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails_key,
    awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails_value,

    -- * AwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails
    AwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails (..),
    newAwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails,
    awsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails_days,
    awsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails_storageClass,

    -- * AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails
    AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails (..),
    newAwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails,
    awsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails_date,
    awsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails_days,
    awsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails_storageClass,

    -- * AwsS3BucketBucketVersioningConfiguration
    AwsS3BucketBucketVersioningConfiguration (..),
    newAwsS3BucketBucketVersioningConfiguration,
    awsS3BucketBucketVersioningConfiguration_isMfaDeleteEnabled,
    awsS3BucketBucketVersioningConfiguration_status,

    -- * AwsS3BucketDetails
    AwsS3BucketDetails (..),
    newAwsS3BucketDetails,
    awsS3BucketDetails_accessControlList,
    awsS3BucketDetails_ownerName,
    awsS3BucketDetails_ownerId,
    awsS3BucketDetails_bucketVersioningConfiguration,
    awsS3BucketDetails_bucketNotificationConfiguration,
    awsS3BucketDetails_serverSideEncryptionConfiguration,
    awsS3BucketDetails_bucketLoggingConfiguration,
    awsS3BucketDetails_ownerAccountId,
    awsS3BucketDetails_bucketWebsiteConfiguration,
    awsS3BucketDetails_bucketLifecycleConfiguration,
    awsS3BucketDetails_createdAt,
    awsS3BucketDetails_publicAccessBlockConfiguration,

    -- * AwsS3BucketLoggingConfiguration
    AwsS3BucketLoggingConfiguration (..),
    newAwsS3BucketLoggingConfiguration,
    awsS3BucketLoggingConfiguration_destinationBucketName,
    awsS3BucketLoggingConfiguration_logFilePrefix,

    -- * AwsS3BucketNotificationConfiguration
    AwsS3BucketNotificationConfiguration (..),
    newAwsS3BucketNotificationConfiguration,
    awsS3BucketNotificationConfiguration_configurations,

    -- * AwsS3BucketNotificationConfigurationDetail
    AwsS3BucketNotificationConfigurationDetail (..),
    newAwsS3BucketNotificationConfigurationDetail,
    awsS3BucketNotificationConfigurationDetail_destination,
    awsS3BucketNotificationConfigurationDetail_type,
    awsS3BucketNotificationConfigurationDetail_filter,
    awsS3BucketNotificationConfigurationDetail_events,

    -- * AwsS3BucketNotificationConfigurationFilter
    AwsS3BucketNotificationConfigurationFilter (..),
    newAwsS3BucketNotificationConfigurationFilter,
    awsS3BucketNotificationConfigurationFilter_s3KeyFilter,

    -- * AwsS3BucketNotificationConfigurationS3KeyFilter
    AwsS3BucketNotificationConfigurationS3KeyFilter (..),
    newAwsS3BucketNotificationConfigurationS3KeyFilter,
    awsS3BucketNotificationConfigurationS3KeyFilter_filterRules,

    -- * AwsS3BucketNotificationConfigurationS3KeyFilterRule
    AwsS3BucketNotificationConfigurationS3KeyFilterRule (..),
    newAwsS3BucketNotificationConfigurationS3KeyFilterRule,
    awsS3BucketNotificationConfigurationS3KeyFilterRule_name,
    awsS3BucketNotificationConfigurationS3KeyFilterRule_value,

    -- * AwsS3BucketServerSideEncryptionByDefault
    AwsS3BucketServerSideEncryptionByDefault (..),
    newAwsS3BucketServerSideEncryptionByDefault,
    awsS3BucketServerSideEncryptionByDefault_kmsMasterKeyID,
    awsS3BucketServerSideEncryptionByDefault_sSEAlgorithm,

    -- * AwsS3BucketServerSideEncryptionConfiguration
    AwsS3BucketServerSideEncryptionConfiguration (..),
    newAwsS3BucketServerSideEncryptionConfiguration,
    awsS3BucketServerSideEncryptionConfiguration_rules,

    -- * AwsS3BucketServerSideEncryptionRule
    AwsS3BucketServerSideEncryptionRule (..),
    newAwsS3BucketServerSideEncryptionRule,
    awsS3BucketServerSideEncryptionRule_applyServerSideEncryptionByDefault,

    -- * AwsS3BucketWebsiteConfiguration
    AwsS3BucketWebsiteConfiguration (..),
    newAwsS3BucketWebsiteConfiguration,
    awsS3BucketWebsiteConfiguration_routingRules,
    awsS3BucketWebsiteConfiguration_errorDocument,
    awsS3BucketWebsiteConfiguration_redirectAllRequestsTo,
    awsS3BucketWebsiteConfiguration_indexDocumentSuffix,

    -- * AwsS3BucketWebsiteConfigurationRedirectTo
    AwsS3BucketWebsiteConfigurationRedirectTo (..),
    newAwsS3BucketWebsiteConfigurationRedirectTo,
    awsS3BucketWebsiteConfigurationRedirectTo_hostname,
    awsS3BucketWebsiteConfigurationRedirectTo_protocol,

    -- * AwsS3BucketWebsiteConfigurationRoutingRule
    AwsS3BucketWebsiteConfigurationRoutingRule (..),
    newAwsS3BucketWebsiteConfigurationRoutingRule,
    awsS3BucketWebsiteConfigurationRoutingRule_condition,
    awsS3BucketWebsiteConfigurationRoutingRule_redirect,

    -- * AwsS3BucketWebsiteConfigurationRoutingRuleCondition
    AwsS3BucketWebsiteConfigurationRoutingRuleCondition (..),
    newAwsS3BucketWebsiteConfigurationRoutingRuleCondition,
    awsS3BucketWebsiteConfigurationRoutingRuleCondition_httpErrorCodeReturnedEquals,
    awsS3BucketWebsiteConfigurationRoutingRuleCondition_keyPrefixEquals,

    -- * AwsS3BucketWebsiteConfigurationRoutingRuleRedirect
    AwsS3BucketWebsiteConfigurationRoutingRuleRedirect (..),
    newAwsS3BucketWebsiteConfigurationRoutingRuleRedirect,
    awsS3BucketWebsiteConfigurationRoutingRuleRedirect_hostname,
    awsS3BucketWebsiteConfigurationRoutingRuleRedirect_replaceKeyPrefixWith,
    awsS3BucketWebsiteConfigurationRoutingRuleRedirect_replaceKeyWith,
    awsS3BucketWebsiteConfigurationRoutingRuleRedirect_protocol,
    awsS3BucketWebsiteConfigurationRoutingRuleRedirect_httpRedirectCode,

    -- * AwsS3ObjectDetails
    AwsS3ObjectDetails (..),
    newAwsS3ObjectDetails,
    awsS3ObjectDetails_serverSideEncryption,
    awsS3ObjectDetails_sSEKMSKeyId,
    awsS3ObjectDetails_lastModified,
    awsS3ObjectDetails_eTag,
    awsS3ObjectDetails_contentType,
    awsS3ObjectDetails_versionId,

    -- * AwsSecretsManagerSecretDetails
    AwsSecretsManagerSecretDetails (..),
    newAwsSecretsManagerSecretDetails,
    awsSecretsManagerSecretDetails_name,
    awsSecretsManagerSecretDetails_rotationLambdaArn,
    awsSecretsManagerSecretDetails_rotationRules,
    awsSecretsManagerSecretDetails_deleted,
    awsSecretsManagerSecretDetails_description,
    awsSecretsManagerSecretDetails_rotationOccurredWithinFrequency,
    awsSecretsManagerSecretDetails_rotationEnabled,
    awsSecretsManagerSecretDetails_kmsKeyId,

    -- * AwsSecretsManagerSecretRotationRules
    AwsSecretsManagerSecretRotationRules (..),
    newAwsSecretsManagerSecretRotationRules,
    awsSecretsManagerSecretRotationRules_automaticallyAfterDays,

    -- * AwsSecurityFinding
    AwsSecurityFinding (..),
    newAwsSecurityFinding,
    awsSecurityFinding_productName,
    awsSecurityFinding_criticality,
    awsSecurityFinding_severity,
    awsSecurityFinding_network,
    awsSecurityFinding_recordState,
    awsSecurityFinding_relatedFindings,
    awsSecurityFinding_productFields,
    awsSecurityFinding_companyName,
    awsSecurityFinding_vulnerabilities,
    awsSecurityFinding_threatIntelIndicators,
    awsSecurityFinding_remediation,
    awsSecurityFinding_confidence,
    awsSecurityFinding_patchSummary,
    awsSecurityFinding_userDefinedFields,
    awsSecurityFinding_malware,
    awsSecurityFinding_networkPath,
    awsSecurityFinding_types,
    awsSecurityFinding_region,
    awsSecurityFinding_firstObservedAt,
    awsSecurityFinding_verificationState,
    awsSecurityFinding_lastObservedAt,
    awsSecurityFinding_threats,
    awsSecurityFinding_action,
    awsSecurityFinding_findingProviderFields,
    awsSecurityFinding_process,
    awsSecurityFinding_workflow,
    awsSecurityFinding_note,
    awsSecurityFinding_sourceUrl,
    awsSecurityFinding_workflowState,
    awsSecurityFinding_compliance,
    awsSecurityFinding_sample,
    awsSecurityFinding_schemaVersion,
    awsSecurityFinding_id,
    awsSecurityFinding_productArn,
    awsSecurityFinding_generatorId,
    awsSecurityFinding_awsAccountId,
    awsSecurityFinding_createdAt,
    awsSecurityFinding_updatedAt,
    awsSecurityFinding_title,
    awsSecurityFinding_description,
    awsSecurityFinding_resources,

    -- * AwsSecurityFindingFilters
    AwsSecurityFindingFilters (..),
    newAwsSecurityFindingFilters,
    awsSecurityFindingFilters_noteText,
    awsSecurityFindingFilters_malwarePath,
    awsSecurityFindingFilters_resourceId,
    awsSecurityFindingFilters_severityProduct,
    awsSecurityFindingFilters_productName,
    awsSecurityFindingFilters_networkProtocol,
    awsSecurityFindingFilters_networkDestinationIpV6,
    awsSecurityFindingFilters_awsAccountId,
    awsSecurityFindingFilters_threatIntelIndicatorLastObservedAt,
    awsSecurityFindingFilters_resourceType,
    awsSecurityFindingFilters_recommendationText,
    awsSecurityFindingFilters_criticality,
    awsSecurityFindingFilters_resourceAwsEc2InstanceImageId,
    awsSecurityFindingFilters_recordState,
    awsSecurityFindingFilters_processPid,
    awsSecurityFindingFilters_findingProviderFieldsSeverityOriginal,
    awsSecurityFindingFilters_networkSourceMac,
    awsSecurityFindingFilters_type,
    awsSecurityFindingFilters_productFields,
    awsSecurityFindingFilters_companyName,
    awsSecurityFindingFilters_threatIntelIndicatorValue,
    awsSecurityFindingFilters_processTerminatedAt,
    awsSecurityFindingFilters_networkDestinationIpV4,
    awsSecurityFindingFilters_resourceAwsS3BucketOwnerId,
    awsSecurityFindingFilters_findingProviderFieldsRelatedFindingsId,
    awsSecurityFindingFilters_resourceAwsIamUserUserName,
    awsSecurityFindingFilters_resourceAwsEc2InstanceType,
    awsSecurityFindingFilters_resourceAwsEc2InstanceIpV4Addresses,
    awsSecurityFindingFilters_severityNormalized,
    awsSecurityFindingFilters_resourceContainerLaunchedAt,
    awsSecurityFindingFilters_networkDestinationPort,
    awsSecurityFindingFilters_severityLabel,
    awsSecurityFindingFilters_confidence,
    awsSecurityFindingFilters_userDefinedFields,
    awsSecurityFindingFilters_noteUpdatedAt,
    awsSecurityFindingFilters_resourceAwsS3BucketOwnerName,
    awsSecurityFindingFilters_relatedFindingsId,
    awsSecurityFindingFilters_findingProviderFieldsRelatedFindingsProductArn,
    awsSecurityFindingFilters_findingProviderFieldsConfidence,
    awsSecurityFindingFilters_resourceAwsIamAccessKeyCreatedAt,
    awsSecurityFindingFilters_resourceAwsEc2InstanceKeyName,
    awsSecurityFindingFilters_resourceAwsEc2InstanceIamInstanceProfileArn,
    awsSecurityFindingFilters_complianceStatus,
    awsSecurityFindingFilters_networkDestinationDomain,
    awsSecurityFindingFilters_networkSourceDomain,
    awsSecurityFindingFilters_threatIntelIndicatorSource,
    awsSecurityFindingFilters_malwareState,
    awsSecurityFindingFilters_resourceTags,
    awsSecurityFindingFilters_resourcePartition,
    awsSecurityFindingFilters_id,
    awsSecurityFindingFilters_description,
    awsSecurityFindingFilters_malwareType,
    awsSecurityFindingFilters_threatIntelIndicatorSourceUrl,
    awsSecurityFindingFilters_resourceAwsIamAccessKeyPrincipalName,
    awsSecurityFindingFilters_networkSourceIpV6,
    awsSecurityFindingFilters_region,
    awsSecurityFindingFilters_networkDirection,
    awsSecurityFindingFilters_title,
    awsSecurityFindingFilters_firstObservedAt,
    awsSecurityFindingFilters_resourceRegion,
    awsSecurityFindingFilters_keyword,
    awsSecurityFindingFilters_productArn,
    awsSecurityFindingFilters_verificationState,
    awsSecurityFindingFilters_resourceContainerName,
    awsSecurityFindingFilters_threatIntelIndicatorCategory,
    awsSecurityFindingFilters_resourceAwsIamAccessKeyUserName,
    awsSecurityFindingFilters_findingProviderFieldsCriticality,
    awsSecurityFindingFilters_resourceAwsEc2InstanceSubnetId,
    awsSecurityFindingFilters_lastObservedAt,
    awsSecurityFindingFilters_malwareName,
    awsSecurityFindingFilters_networkSourceIpV4,
    awsSecurityFindingFilters_processName,
    awsSecurityFindingFilters_networkSourcePort,
    awsSecurityFindingFilters_noteUpdatedBy,
    awsSecurityFindingFilters_processLaunchedAt,
    awsSecurityFindingFilters_threatIntelIndicatorType,
    awsSecurityFindingFilters_resourceAwsIamAccessKeyStatus,
    awsSecurityFindingFilters_resourceAwsEc2InstanceLaunchedAt,
    awsSecurityFindingFilters_resourceAwsEc2InstanceVpcId,
    awsSecurityFindingFilters_findingProviderFieldsTypes,
    awsSecurityFindingFilters_workflowStatus,
    awsSecurityFindingFilters_sourceUrl,
    awsSecurityFindingFilters_processParentPid,
    awsSecurityFindingFilters_createdAt,
    awsSecurityFindingFilters_findingProviderFieldsSeverityLabel,
    awsSecurityFindingFilters_resourceAwsEc2InstanceIpV6Addresses,
    awsSecurityFindingFilters_workflowState,
    awsSecurityFindingFilters_generatorId,
    awsSecurityFindingFilters_updatedAt,
    awsSecurityFindingFilters_resourceContainerImageId,
    awsSecurityFindingFilters_resourceContainerImageName,
    awsSecurityFindingFilters_processPath,
    awsSecurityFindingFilters_sample,
    awsSecurityFindingFilters_relatedFindingsProductArn,
    awsSecurityFindingFilters_resourceDetailsOther,

    -- * AwsSecurityFindingIdentifier
    AwsSecurityFindingIdentifier (..),
    newAwsSecurityFindingIdentifier,
    awsSecurityFindingIdentifier_id,
    awsSecurityFindingIdentifier_productArn,

    -- * AwsSnsTopicDetails
    AwsSnsTopicDetails (..),
    newAwsSnsTopicDetails,
    awsSnsTopicDetails_kmsMasterKeyId,
    awsSnsTopicDetails_sqsSuccessFeedbackRoleArn,
    awsSnsTopicDetails_sqsFailureFeedbackRoleArn,
    awsSnsTopicDetails_firehoseSuccessFeedbackRoleArn,
    awsSnsTopicDetails_httpSuccessFeedbackRoleArn,
    awsSnsTopicDetails_owner,
    awsSnsTopicDetails_httpFailureFeedbackRoleArn,
    awsSnsTopicDetails_applicationSuccessFeedbackRoleArn,
    awsSnsTopicDetails_topicName,
    awsSnsTopicDetails_subscription,
    awsSnsTopicDetails_firehoseFailureFeedbackRoleArn,

    -- * AwsSnsTopicSubscription
    AwsSnsTopicSubscription (..),
    newAwsSnsTopicSubscription,
    awsSnsTopicSubscription_protocol,
    awsSnsTopicSubscription_endpoint,

    -- * AwsSqsQueueDetails
    AwsSqsQueueDetails (..),
    newAwsSqsQueueDetails,
    awsSqsQueueDetails_kmsMasterKeyId,
    awsSqsQueueDetails_queueName,
    awsSqsQueueDetails_deadLetterTargetArn,
    awsSqsQueueDetails_kmsDataKeyReusePeriodSeconds,

    -- * AwsSsmComplianceSummary
    AwsSsmComplianceSummary (..),
    newAwsSsmComplianceSummary,
    awsSsmComplianceSummary_compliantInformationalCount,
    awsSsmComplianceSummary_executionType,
    awsSsmComplianceSummary_compliantCriticalCount,
    awsSsmComplianceSummary_compliantUnspecifiedCount,
    awsSsmComplianceSummary_patchBaselineId,
    awsSsmComplianceSummary_nonCompliantHighCount,
    awsSsmComplianceSummary_patchGroup,
    awsSsmComplianceSummary_nonCompliantInformationalCount,
    awsSsmComplianceSummary_status,
    awsSsmComplianceSummary_nonCompliantCriticalCount,
    awsSsmComplianceSummary_compliantHighCount,
    awsSsmComplianceSummary_compliantMediumCount,
    awsSsmComplianceSummary_nonCompliantUnspecifiedCount,
    awsSsmComplianceSummary_nonCompliantMediumCount,
    awsSsmComplianceSummary_compliantLowCount,
    awsSsmComplianceSummary_nonCompliantLowCount,
    awsSsmComplianceSummary_complianceType,
    awsSsmComplianceSummary_overallSeverity,

    -- * AwsSsmPatch
    AwsSsmPatch (..),
    newAwsSsmPatch,
    awsSsmPatch_complianceSummary,

    -- * AwsSsmPatchComplianceDetails
    AwsSsmPatchComplianceDetails (..),
    newAwsSsmPatchComplianceDetails,
    awsSsmPatchComplianceDetails_patch,

    -- * AwsWafRateBasedRuleDetails
    AwsWafRateBasedRuleDetails (..),
    newAwsWafRateBasedRuleDetails,
    awsWafRateBasedRuleDetails_name,
    awsWafRateBasedRuleDetails_ruleId,
    awsWafRateBasedRuleDetails_matchPredicates,
    awsWafRateBasedRuleDetails_rateLimit,
    awsWafRateBasedRuleDetails_metricName,
    awsWafRateBasedRuleDetails_rateKey,

    -- * AwsWafRateBasedRuleMatchPredicate
    AwsWafRateBasedRuleMatchPredicate (..),
    newAwsWafRateBasedRuleMatchPredicate,
    awsWafRateBasedRuleMatchPredicate_type,
    awsWafRateBasedRuleMatchPredicate_dataId,
    awsWafRateBasedRuleMatchPredicate_negated,

    -- * AwsWafRegionalRateBasedRuleDetails
    AwsWafRegionalRateBasedRuleDetails (..),
    newAwsWafRegionalRateBasedRuleDetails,
    awsWafRegionalRateBasedRuleDetails_name,
    awsWafRegionalRateBasedRuleDetails_ruleId,
    awsWafRegionalRateBasedRuleDetails_matchPredicates,
    awsWafRegionalRateBasedRuleDetails_rateLimit,
    awsWafRegionalRateBasedRuleDetails_metricName,
    awsWafRegionalRateBasedRuleDetails_rateKey,

    -- * AwsWafRegionalRateBasedRuleMatchPredicate
    AwsWafRegionalRateBasedRuleMatchPredicate (..),
    newAwsWafRegionalRateBasedRuleMatchPredicate,
    awsWafRegionalRateBasedRuleMatchPredicate_type,
    awsWafRegionalRateBasedRuleMatchPredicate_dataId,
    awsWafRegionalRateBasedRuleMatchPredicate_negated,

    -- * AwsWafRegionalRuleDetails
    AwsWafRegionalRuleDetails (..),
    newAwsWafRegionalRuleDetails,
    awsWafRegionalRuleDetails_name,
    awsWafRegionalRuleDetails_predicateList,
    awsWafRegionalRuleDetails_ruleId,
    awsWafRegionalRuleDetails_metricName,

    -- * AwsWafRegionalRuleGroupDetails
    AwsWafRegionalRuleGroupDetails (..),
    newAwsWafRegionalRuleGroupDetails,
    awsWafRegionalRuleGroupDetails_name,
    awsWafRegionalRuleGroupDetails_rules,
    awsWafRegionalRuleGroupDetails_ruleGroupId,
    awsWafRegionalRuleGroupDetails_metricName,

    -- * AwsWafRegionalRuleGroupRulesActionDetails
    AwsWafRegionalRuleGroupRulesActionDetails (..),
    newAwsWafRegionalRuleGroupRulesActionDetails,
    awsWafRegionalRuleGroupRulesActionDetails_type,

    -- * AwsWafRegionalRuleGroupRulesDetails
    AwsWafRegionalRuleGroupRulesDetails (..),
    newAwsWafRegionalRuleGroupRulesDetails,
    awsWafRegionalRuleGroupRulesDetails_type,
    awsWafRegionalRuleGroupRulesDetails_ruleId,
    awsWafRegionalRuleGroupRulesDetails_priority,
    awsWafRegionalRuleGroupRulesDetails_action,

    -- * AwsWafRegionalRulePredicateListDetails
    AwsWafRegionalRulePredicateListDetails (..),
    newAwsWafRegionalRulePredicateListDetails,
    awsWafRegionalRulePredicateListDetails_type,
    awsWafRegionalRulePredicateListDetails_dataId,
    awsWafRegionalRulePredicateListDetails_negated,

    -- * AwsWafRegionalWebAclDetails
    AwsWafRegionalWebAclDetails (..),
    newAwsWafRegionalWebAclDetails,
    awsWafRegionalWebAclDetails_name,
    awsWafRegionalWebAclDetails_rulesList,
    awsWafRegionalWebAclDetails_webAclId,
    awsWafRegionalWebAclDetails_metricName,
    awsWafRegionalWebAclDetails_defaultAction,

    -- * AwsWafRegionalWebAclRulesListActionDetails
    AwsWafRegionalWebAclRulesListActionDetails (..),
    newAwsWafRegionalWebAclRulesListActionDetails,
    awsWafRegionalWebAclRulesListActionDetails_type,

    -- * AwsWafRegionalWebAclRulesListDetails
    AwsWafRegionalWebAclRulesListDetails (..),
    newAwsWafRegionalWebAclRulesListDetails,
    awsWafRegionalWebAclRulesListDetails_type,
    awsWafRegionalWebAclRulesListDetails_ruleId,
    awsWafRegionalWebAclRulesListDetails_overrideAction,
    awsWafRegionalWebAclRulesListDetails_priority,
    awsWafRegionalWebAclRulesListDetails_action,

    -- * AwsWafRegionalWebAclRulesListOverrideActionDetails
    AwsWafRegionalWebAclRulesListOverrideActionDetails (..),
    newAwsWafRegionalWebAclRulesListOverrideActionDetails,
    awsWafRegionalWebAclRulesListOverrideActionDetails_type,

    -- * AwsWafRuleDetails
    AwsWafRuleDetails (..),
    newAwsWafRuleDetails,
    awsWafRuleDetails_name,
    awsWafRuleDetails_predicateList,
    awsWafRuleDetails_ruleId,
    awsWafRuleDetails_metricName,

    -- * AwsWafRuleGroupDetails
    AwsWafRuleGroupDetails (..),
    newAwsWafRuleGroupDetails,
    awsWafRuleGroupDetails_name,
    awsWafRuleGroupDetails_rules,
    awsWafRuleGroupDetails_ruleGroupId,
    awsWafRuleGroupDetails_metricName,

    -- * AwsWafRuleGroupRulesActionDetails
    AwsWafRuleGroupRulesActionDetails (..),
    newAwsWafRuleGroupRulesActionDetails,
    awsWafRuleGroupRulesActionDetails_type,

    -- * AwsWafRuleGroupRulesDetails
    AwsWafRuleGroupRulesDetails (..),
    newAwsWafRuleGroupRulesDetails,
    awsWafRuleGroupRulesDetails_type,
    awsWafRuleGroupRulesDetails_ruleId,
    awsWafRuleGroupRulesDetails_priority,
    awsWafRuleGroupRulesDetails_action,

    -- * AwsWafRulePredicateListDetails
    AwsWafRulePredicateListDetails (..),
    newAwsWafRulePredicateListDetails,
    awsWafRulePredicateListDetails_type,
    awsWafRulePredicateListDetails_dataId,
    awsWafRulePredicateListDetails_negated,

    -- * AwsWafWebAclDetails
    AwsWafWebAclDetails (..),
    newAwsWafWebAclDetails,
    awsWafWebAclDetails_name,
    awsWafWebAclDetails_rules,
    awsWafWebAclDetails_webAclId,
    awsWafWebAclDetails_defaultAction,

    -- * AwsWafWebAclRule
    AwsWafWebAclRule (..),
    newAwsWafWebAclRule,
    awsWafWebAclRule_type,
    awsWafWebAclRule_ruleId,
    awsWafWebAclRule_overrideAction,
    awsWafWebAclRule_priority,
    awsWafWebAclRule_action,
    awsWafWebAclRule_excludedRules,

    -- * AwsXrayEncryptionConfigDetails
    AwsXrayEncryptionConfigDetails (..),
    newAwsXrayEncryptionConfigDetails,
    awsXrayEncryptionConfigDetails_type,
    awsXrayEncryptionConfigDetails_status,
    awsXrayEncryptionConfigDetails_keyId,

    -- * BatchUpdateFindingsUnprocessedFinding
    BatchUpdateFindingsUnprocessedFinding (..),
    newBatchUpdateFindingsUnprocessedFinding,
    batchUpdateFindingsUnprocessedFinding_findingIdentifier,
    batchUpdateFindingsUnprocessedFinding_errorCode,
    batchUpdateFindingsUnprocessedFinding_errorMessage,

    -- * BooleanFilter
    BooleanFilter (..),
    newBooleanFilter,
    booleanFilter_value,

    -- * Cell
    Cell (..),
    newCell,
    cell_row,
    cell_cellReference,
    cell_columnName,
    cell_column,

    -- * CidrBlockAssociation
    CidrBlockAssociation (..),
    newCidrBlockAssociation,
    cidrBlockAssociation_cidrBlockState,
    cidrBlockAssociation_cidrBlock,
    cidrBlockAssociation_associationId,

    -- * City
    City (..),
    newCity,
    city_cityName,

    -- * ClassificationResult
    ClassificationResult (..),
    newClassificationResult,
    classificationResult_status,
    classificationResult_additionalOccurrences,
    classificationResult_customDataIdentifiers,
    classificationResult_mimeType,
    classificationResult_sizeClassified,
    classificationResult_sensitiveData,

    -- * ClassificationStatus
    ClassificationStatus (..),
    newClassificationStatus,
    classificationStatus_code,
    classificationStatus_reason,

    -- * Compliance
    Compliance (..),
    newCompliance,
    compliance_relatedRequirements,
    compliance_status,
    compliance_statusReasons,

    -- * ContainerDetails
    ContainerDetails (..),
    newContainerDetails,
    containerDetails_name,
    containerDetails_containerRuntime,
    containerDetails_privileged,
    containerDetails_launchedAt,
    containerDetails_volumeMounts,
    containerDetails_imageName,
    containerDetails_imageId,

    -- * Country
    Country (..),
    newCountry,
    country_countryName,
    country_countryCode,

    -- * CustomDataIdentifiersDetections
    CustomDataIdentifiersDetections (..),
    newCustomDataIdentifiersDetections,
    customDataIdentifiersDetections_occurrences,
    customDataIdentifiersDetections_name,
    customDataIdentifiersDetections_arn,
    customDataIdentifiersDetections_count,

    -- * CustomDataIdentifiersResult
    CustomDataIdentifiersResult (..),
    newCustomDataIdentifiersResult,
    customDataIdentifiersResult_detections,
    customDataIdentifiersResult_totalCount,

    -- * Cvss
    Cvss (..),
    newCvss,
    cvss_baseScore,
    cvss_baseVector,
    cvss_adjustments,
    cvss_source,
    cvss_version,

    -- * DataClassificationDetails
    DataClassificationDetails (..),
    newDataClassificationDetails,
    dataClassificationDetails_detailedResultsLocation,
    dataClassificationDetails_result,

    -- * DateFilter
    DateFilter (..),
    newDateFilter,
    dateFilter_start,
    dateFilter_dateRange,
    dateFilter_end,

    -- * DateRange
    DateRange (..),
    newDateRange,
    dateRange_unit,
    dateRange_value,

    -- * DnsRequestAction
    DnsRequestAction (..),
    newDnsRequestAction,
    dnsRequestAction_domain,
    dnsRequestAction_blocked,
    dnsRequestAction_protocol,

    -- * FilePaths
    FilePaths (..),
    newFilePaths,
    filePaths_resourceId,
    filePaths_filePath,
    filePaths_hash,
    filePaths_fileName,

    -- * FindingAggregator
    FindingAggregator (..),
    newFindingAggregator,
    findingAggregator_findingAggregatorArn,

    -- * FindingProviderFields
    FindingProviderFields (..),
    newFindingProviderFields,
    findingProviderFields_criticality,
    findingProviderFields_severity,
    findingProviderFields_relatedFindings,
    findingProviderFields_confidence,
    findingProviderFields_types,

    -- * FindingProviderSeverity
    FindingProviderSeverity (..),
    newFindingProviderSeverity,
    findingProviderSeverity_label,
    findingProviderSeverity_original,

    -- * FirewallPolicyDetails
    FirewallPolicyDetails (..),
    newFirewallPolicyDetails,
    firewallPolicyDetails_statelessCustomActions,
    firewallPolicyDetails_statefulRuleGroupReferences,
    firewallPolicyDetails_statelessRuleGroupReferences,
    firewallPolicyDetails_statelessDefaultActions,
    firewallPolicyDetails_statelessFragmentDefaultActions,

    -- * FirewallPolicyStatefulRuleGroupReferencesDetails
    FirewallPolicyStatefulRuleGroupReferencesDetails (..),
    newFirewallPolicyStatefulRuleGroupReferencesDetails,
    firewallPolicyStatefulRuleGroupReferencesDetails_resourceArn,

    -- * FirewallPolicyStatelessCustomActionsDetails
    FirewallPolicyStatelessCustomActionsDetails (..),
    newFirewallPolicyStatelessCustomActionsDetails,
    firewallPolicyStatelessCustomActionsDetails_actionName,
    firewallPolicyStatelessCustomActionsDetails_actionDefinition,

    -- * FirewallPolicyStatelessRuleGroupReferencesDetails
    FirewallPolicyStatelessRuleGroupReferencesDetails (..),
    newFirewallPolicyStatelessRuleGroupReferencesDetails,
    firewallPolicyStatelessRuleGroupReferencesDetails_priority,
    firewallPolicyStatelessRuleGroupReferencesDetails_resourceArn,

    -- * GeoLocation
    GeoLocation (..),
    newGeoLocation,
    geoLocation_lat,
    geoLocation_lon,

    -- * IcmpTypeCode
    IcmpTypeCode (..),
    newIcmpTypeCode,
    icmpTypeCode_type,
    icmpTypeCode_code,

    -- * ImportFindingsError
    ImportFindingsError (..),
    newImportFindingsError,
    importFindingsError_id,
    importFindingsError_errorCode,
    importFindingsError_errorMessage,

    -- * Insight
    Insight (..),
    newInsight,
    insight_insightArn,
    insight_name,
    insight_filters,
    insight_groupByAttribute,

    -- * InsightResultValue
    InsightResultValue (..),
    newInsightResultValue,
    insightResultValue_groupByAttributeValue,
    insightResultValue_count,

    -- * InsightResults
    InsightResults (..),
    newInsightResults,
    insightResults_insightArn,
    insightResults_groupByAttribute,
    insightResults_resultValues,

    -- * Invitation
    Invitation (..),
    newInvitation,
    invitation_memberStatus,
    invitation_accountId,
    invitation_invitedAt,
    invitation_invitationId,

    -- * IpFilter
    IpFilter (..),
    newIpFilter,
    ipFilter_cidr,

    -- * IpOrganizationDetails
    IpOrganizationDetails (..),
    newIpOrganizationDetails,
    ipOrganizationDetails_isp,
    ipOrganizationDetails_org,
    ipOrganizationDetails_asn,
    ipOrganizationDetails_asnOrg,

    -- * Ipv6CidrBlockAssociation
    Ipv6CidrBlockAssociation (..),
    newIpv6CidrBlockAssociation,
    ipv6CidrBlockAssociation_cidrBlockState,
    ipv6CidrBlockAssociation_associationId,
    ipv6CidrBlockAssociation_ipv6CidrBlock,

    -- * KeywordFilter
    KeywordFilter (..),
    newKeywordFilter,
    keywordFilter_value,

    -- * LoadBalancerState
    LoadBalancerState (..),
    newLoadBalancerState,
    loadBalancerState_code,
    loadBalancerState_reason,

    -- * Malware
    Malware (..),
    newMalware,
    malware_type,
    malware_state,
    malware_path,
    malware_name,

    -- * MapFilter
    MapFilter (..),
    newMapFilter,
    mapFilter_key,
    mapFilter_comparison,
    mapFilter_value,

    -- * Member
    Member (..),
    newMember,
    member_email,
    member_administratorId,
    member_memberStatus,
    member_accountId,
    member_masterId,
    member_invitedAt,
    member_updatedAt,

    -- * Network
    Network (..),
    newNetwork,
    network_destinationDomain,
    network_sourceIpV4,
    network_openPortRange,
    network_destinationIpV6,
    network_sourceIpV6,
    network_sourceDomain,
    network_destinationIpV4,
    network_destinationPort,
    network_sourcePort,
    network_sourceMac,
    network_protocol,
    network_direction,

    -- * NetworkConnectionAction
    NetworkConnectionAction (..),
    newNetworkConnectionAction,
    networkConnectionAction_connectionDirection,
    networkConnectionAction_remoteIpDetails,
    networkConnectionAction_localPortDetails,
    networkConnectionAction_blocked,
    networkConnectionAction_protocol,
    networkConnectionAction_remotePortDetails,

    -- * NetworkHeader
    NetworkHeader (..),
    newNetworkHeader,
    networkHeader_destination,
    networkHeader_source,
    networkHeader_protocol,

    -- * NetworkPathComponent
    NetworkPathComponent (..),
    newNetworkPathComponent,
    networkPathComponent_egress,
    networkPathComponent_ingress,
    networkPathComponent_componentId,
    networkPathComponent_componentType,

    -- * NetworkPathComponentDetails
    NetworkPathComponentDetails (..),
    newNetworkPathComponentDetails,
    networkPathComponentDetails_address,
    networkPathComponentDetails_portRanges,

    -- * Note
    Note (..),
    newNote,
    note_text,
    note_updatedBy,
    note_updatedAt,

    -- * NoteUpdate
    NoteUpdate (..),
    newNoteUpdate,
    noteUpdate_text,
    noteUpdate_updatedBy,

    -- * NumberFilter
    NumberFilter (..),
    newNumberFilter,
    numberFilter_lte,
    numberFilter_gte,
    numberFilter_eq,

    -- * Occurrences
    Occurrences (..),
    newOccurrences,
    occurrences_records,
    occurrences_lineRanges,
    occurrences_offsetRanges,
    occurrences_cells,
    occurrences_pages,

    -- * Page
    Page (..),
    newPage,
    page_offsetRange,
    page_pageNumber,
    page_lineRange,

    -- * PatchSummary
    PatchSummary (..),
    newPatchSummary,
    patchSummary_installedOtherCount,
    patchSummary_rebootOption,
    patchSummary_failedCount,
    patchSummary_operationEndTime,
    patchSummary_installedCount,
    patchSummary_installedRejectedCount,
    patchSummary_operationStartTime,
    patchSummary_missingCount,
    patchSummary_operation,
    patchSummary_installedPendingReboot,
    patchSummary_id,

    -- * PortProbeAction
    PortProbeAction (..),
    newPortProbeAction,
    portProbeAction_blocked,
    portProbeAction_portProbeDetails,

    -- * PortProbeDetail
    PortProbeDetail (..),
    newPortProbeDetail,
    portProbeDetail_remoteIpDetails,
    portProbeDetail_localPortDetails,
    portProbeDetail_localIpDetails,

    -- * PortRange
    PortRange (..),
    newPortRange,
    portRange_begin,
    portRange_end,

    -- * PortRangeFromTo
    PortRangeFromTo (..),
    newPortRangeFromTo,
    portRangeFromTo_from,
    portRangeFromTo_to,

    -- * ProcessDetails
    ProcessDetails (..),
    newProcessDetails,
    processDetails_parentPid,
    processDetails_name,
    processDetails_pid,
    processDetails_terminatedAt,
    processDetails_path,
    processDetails_launchedAt,

    -- * Product
    Product (..),
    newProduct,
    product_integrationTypes,
    product_productName,
    product_companyName,
    product_activationUrl,
    product_description,
    product_productSubscriptionResourcePolicy,
    product_categories,
    product_marketplaceUrl,
    product_productArn,

    -- * Range
    Range (..),
    newRange,
    range_start,
    range_end,
    range_startColumn,

    -- * Recommendation
    Recommendation (..),
    newRecommendation,
    recommendation_url,
    recommendation_text,

    -- * Record
    Record (..),
    newRecord,
    record_jsonPath,
    record_recordIndex,

    -- * RelatedFinding
    RelatedFinding (..),
    newRelatedFinding,
    relatedFinding_productArn,
    relatedFinding_id,

    -- * Remediation
    Remediation (..),
    newRemediation,
    remediation_recommendation,

    -- * Resource
    Resource (..),
    newResource,
    resource_tags,
    resource_dataClassification,
    resource_resourceRole,
    resource_details,
    resource_partition,
    resource_region,
    resource_type,
    resource_id,

    -- * ResourceDetails
    ResourceDetails (..),
    newResourceDetails,
    resourceDetails_awsNetworkFirewallRuleGroup,
    resourceDetails_awsEc2Instance,
    resourceDetails_awsIamRole,
    resourceDetails_awsS3AccountPublicAccessBlock,
    resourceDetails_awsSqsQueue,
    resourceDetails_awsElbv2LoadBalancer,
    resourceDetails_awsCloudWatchAlarm,
    resourceDetails_awsIamPolicy,
    resourceDetails_awsEc2NetworkInterface,
    resourceDetails_awsEksCluster,
    resourceDetails_awsWafRateBasedRule,
    resourceDetails_awsEcsContainer,
    resourceDetails_awsRdsEventSubscription,
    resourceDetails_awsCloudFormationStack,
    resourceDetails_awsWafWebAcl,
    resourceDetails_other,
    resourceDetails_awsSsmPatchCompliance,
    resourceDetails_awsOpenSearchServiceDomain,
    resourceDetails_awsDynamoDbTable,
    resourceDetails_awsEc2Volume,
    resourceDetails_awsCertificateManagerCertificate,
    resourceDetails_awsEc2VpnConnection,
    resourceDetails_awsEc2Subnet,
    resourceDetails_awsIamGroup,
    resourceDetails_awsIamUser,
    resourceDetails_awsBackupBackupVault,
    resourceDetails_awsApiGatewayV2Api,
    resourceDetails_awsEcsTaskDefinition,
    resourceDetails_awsAutoScalingAutoScalingGroup,
    resourceDetails_awsEc2NetworkAcl,
    resourceDetails_awsEfsAccessPoint,
    resourceDetails_awsEc2VpcEndpointService,
    resourceDetails_awsCodeBuildProject,
    resourceDetails_awsRdsDbInstance,
    resourceDetails_awsElasticsearchDomain,
    resourceDetails_awsEcrContainerImage,
    resourceDetails_awsAutoScalingLaunchConfiguration,
    resourceDetails_awsIamAccessKey,
    resourceDetails_awsRedshiftCluster,
    resourceDetails_awsNetworkFirewallFirewallPolicy,
    resourceDetails_awsEc2Vpc,
    resourceDetails_awsEcrRepository,
    resourceDetails_awsWafRegionalRuleGroup,
    resourceDetails_awsWafRegionalRateBasedRule,
    resourceDetails_awsWafRegionalRule,
    resourceDetails_awsApiGatewayV2Stage,
    resourceDetails_awsRdsDbCluster,
    resourceDetails_awsS3Object,
    resourceDetails_awsNetworkFirewallFirewall,
    resourceDetails_awsWafRuleGroup,
    resourceDetails_awsEc2VpcPeeringConnection,
    resourceDetails_awsEc2TransitGateway,
    resourceDetails_awsS3Bucket,
    resourceDetails_awsWafRegionalWebAcl,
    resourceDetails_awsEc2Eip,
    resourceDetails_awsBackupBackupPlan,
    resourceDetails_awsLambdaFunction,
    resourceDetails_container,
    resourceDetails_awsRdsDbClusterSnapshot,
    resourceDetails_awsElbLoadBalancer,
    resourceDetails_awsCloudTrailTrail,
    resourceDetails_awsEcsService,
    resourceDetails_awsApiGatewayStage,
    resourceDetails_awsSnsTopic,
    resourceDetails_awsRdsDbSecurityGroup,
    resourceDetails_awsKinesisStream,
    resourceDetails_awsEc2SecurityGroup,
    resourceDetails_awsEcsCluster,
    resourceDetails_awsApiGatewayRestApi,
    resourceDetails_awsWafRule,
    resourceDetails_awsBackupRecoveryPoint,
    resourceDetails_awsRdsDbSnapshot,
    resourceDetails_awsElasticBeanstalkEnvironment,
    resourceDetails_awsEcsTask,
    resourceDetails_awsXrayEncryptionConfig,
    resourceDetails_awsLambdaLayerVersion,
    resourceDetails_awsKmsKey,
    resourceDetails_awsSecretsManagerSecret,
    resourceDetails_awsCloudFrontDistribution,

    -- * Result
    Result (..),
    newResult,
    result_processingResult,
    result_accountId,

    -- * RuleGroupDetails
    RuleGroupDetails (..),
    newRuleGroupDetails,
    ruleGroupDetails_rulesSource,
    ruleGroupDetails_ruleVariables,

    -- * RuleGroupSource
    RuleGroupSource (..),
    newRuleGroupSource,
    ruleGroupSource_rulesString,
    ruleGroupSource_statefulRules,
    ruleGroupSource_rulesSourceList,
    ruleGroupSource_statelessRulesAndCustomActions,

    -- * RuleGroupSourceCustomActionsDetails
    RuleGroupSourceCustomActionsDetails (..),
    newRuleGroupSourceCustomActionsDetails,
    ruleGroupSourceCustomActionsDetails_actionName,
    ruleGroupSourceCustomActionsDetails_actionDefinition,

    -- * RuleGroupSourceListDetails
    RuleGroupSourceListDetails (..),
    newRuleGroupSourceListDetails,
    ruleGroupSourceListDetails_targetTypes,
    ruleGroupSourceListDetails_generatedRulesType,
    ruleGroupSourceListDetails_targets,

    -- * RuleGroupSourceStatefulRulesDetails
    RuleGroupSourceStatefulRulesDetails (..),
    newRuleGroupSourceStatefulRulesDetails,
    ruleGroupSourceStatefulRulesDetails_header,
    ruleGroupSourceStatefulRulesDetails_ruleOptions,
    ruleGroupSourceStatefulRulesDetails_action,

    -- * RuleGroupSourceStatefulRulesHeaderDetails
    RuleGroupSourceStatefulRulesHeaderDetails (..),
    newRuleGroupSourceStatefulRulesHeaderDetails,
    ruleGroupSourceStatefulRulesHeaderDetails_destination,
    ruleGroupSourceStatefulRulesHeaderDetails_destinationPort,
    ruleGroupSourceStatefulRulesHeaderDetails_sourcePort,
    ruleGroupSourceStatefulRulesHeaderDetails_source,
    ruleGroupSourceStatefulRulesHeaderDetails_protocol,
    ruleGroupSourceStatefulRulesHeaderDetails_direction,

    -- * RuleGroupSourceStatefulRulesOptionsDetails
    RuleGroupSourceStatefulRulesOptionsDetails (..),
    newRuleGroupSourceStatefulRulesOptionsDetails,
    ruleGroupSourceStatefulRulesOptionsDetails_settings,
    ruleGroupSourceStatefulRulesOptionsDetails_keyword,

    -- * RuleGroupSourceStatelessRuleDefinition
    RuleGroupSourceStatelessRuleDefinition (..),
    newRuleGroupSourceStatelessRuleDefinition,
    ruleGroupSourceStatelessRuleDefinition_matchAttributes,
    ruleGroupSourceStatelessRuleDefinition_actions,

    -- * RuleGroupSourceStatelessRuleMatchAttributes
    RuleGroupSourceStatelessRuleMatchAttributes (..),
    newRuleGroupSourceStatelessRuleMatchAttributes,
    ruleGroupSourceStatelessRuleMatchAttributes_destinationPorts,
    ruleGroupSourceStatelessRuleMatchAttributes_sources,
    ruleGroupSourceStatelessRuleMatchAttributes_tcpFlags,
    ruleGroupSourceStatelessRuleMatchAttributes_protocols,
    ruleGroupSourceStatelessRuleMatchAttributes_sourcePorts,
    ruleGroupSourceStatelessRuleMatchAttributes_destinations,

    -- * RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts
    RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts (..),
    newRuleGroupSourceStatelessRuleMatchAttributesDestinationPorts,
    ruleGroupSourceStatelessRuleMatchAttributesDestinationPorts_toPort,
    ruleGroupSourceStatelessRuleMatchAttributesDestinationPorts_fromPort,

    -- * RuleGroupSourceStatelessRuleMatchAttributesDestinations
    RuleGroupSourceStatelessRuleMatchAttributesDestinations (..),
    newRuleGroupSourceStatelessRuleMatchAttributesDestinations,
    ruleGroupSourceStatelessRuleMatchAttributesDestinations_addressDefinition,

    -- * RuleGroupSourceStatelessRuleMatchAttributesSourcePorts
    RuleGroupSourceStatelessRuleMatchAttributesSourcePorts (..),
    newRuleGroupSourceStatelessRuleMatchAttributesSourcePorts,
    ruleGroupSourceStatelessRuleMatchAttributesSourcePorts_toPort,
    ruleGroupSourceStatelessRuleMatchAttributesSourcePorts_fromPort,

    -- * RuleGroupSourceStatelessRuleMatchAttributesSources
    RuleGroupSourceStatelessRuleMatchAttributesSources (..),
    newRuleGroupSourceStatelessRuleMatchAttributesSources,
    ruleGroupSourceStatelessRuleMatchAttributesSources_addressDefinition,

    -- * RuleGroupSourceStatelessRuleMatchAttributesTcpFlags
    RuleGroupSourceStatelessRuleMatchAttributesTcpFlags (..),
    newRuleGroupSourceStatelessRuleMatchAttributesTcpFlags,
    ruleGroupSourceStatelessRuleMatchAttributesTcpFlags_flags,
    ruleGroupSourceStatelessRuleMatchAttributesTcpFlags_masks,

    -- * RuleGroupSourceStatelessRulesAndCustomActionsDetails
    RuleGroupSourceStatelessRulesAndCustomActionsDetails (..),
    newRuleGroupSourceStatelessRulesAndCustomActionsDetails,
    ruleGroupSourceStatelessRulesAndCustomActionsDetails_customActions,
    ruleGroupSourceStatelessRulesAndCustomActionsDetails_statelessRules,

    -- * RuleGroupSourceStatelessRulesDetails
    RuleGroupSourceStatelessRulesDetails (..),
    newRuleGroupSourceStatelessRulesDetails,
    ruleGroupSourceStatelessRulesDetails_ruleDefinition,
    ruleGroupSourceStatelessRulesDetails_priority,

    -- * RuleGroupVariables
    RuleGroupVariables (..),
    newRuleGroupVariables,
    ruleGroupVariables_ipSets,
    ruleGroupVariables_portSets,

    -- * RuleGroupVariablesIpSetsDetails
    RuleGroupVariablesIpSetsDetails (..),
    newRuleGroupVariablesIpSetsDetails,
    ruleGroupVariablesIpSetsDetails_definition,

    -- * RuleGroupVariablesPortSetsDetails
    RuleGroupVariablesPortSetsDetails (..),
    newRuleGroupVariablesPortSetsDetails,
    ruleGroupVariablesPortSetsDetails_definition,

    -- * SensitiveDataDetections
    SensitiveDataDetections (..),
    newSensitiveDataDetections,
    sensitiveDataDetections_occurrences,
    sensitiveDataDetections_type,
    sensitiveDataDetections_count,

    -- * SensitiveDataResult
    SensitiveDataResult (..),
    newSensitiveDataResult,
    sensitiveDataResult_detections,
    sensitiveDataResult_category,
    sensitiveDataResult_totalCount,

    -- * Severity
    Severity (..),
    newSeverity,
    severity_product,
    severity_label,
    severity_normalized,
    severity_original,

    -- * SeverityUpdate
    SeverityUpdate (..),
    newSeverityUpdate,
    severityUpdate_product,
    severityUpdate_label,
    severityUpdate_normalized,

    -- * SoftwarePackage
    SoftwarePackage (..),
    newSoftwarePackage,
    softwarePackage_filePath,
    softwarePackage_name,
    softwarePackage_sourceLayerArn,
    softwarePackage_fixedInVersion,
    softwarePackage_remediation,
    softwarePackage_epoch,
    softwarePackage_packageManager,
    softwarePackage_release,
    softwarePackage_sourceLayerHash,
    softwarePackage_architecture,
    softwarePackage_version,

    -- * SortCriterion
    SortCriterion (..),
    newSortCriterion,
    sortCriterion_sortOrder,
    sortCriterion_field,

    -- * Standard
    Standard (..),
    newStandard,
    standard_name,
    standard_standardsArn,
    standard_description,
    standard_enabledByDefault,

    -- * StandardsControl
    StandardsControl (..),
    newStandardsControl,
    standardsControl_controlId,
    standardsControl_standardsControlArn,
    standardsControl_severityRating,
    standardsControl_controlStatusUpdatedAt,
    standardsControl_disabledReason,
    standardsControl_relatedRequirements,
    standardsControl_description,
    standardsControl_title,
    standardsControl_remediationUrl,
    standardsControl_controlStatus,

    -- * StandardsStatusReason
    StandardsStatusReason (..),
    newStandardsStatusReason,
    standardsStatusReason_statusReasonCode,

    -- * StandardsSubscription
    StandardsSubscription (..),
    newStandardsSubscription,
    standardsSubscription_standardsStatusReason,
    standardsSubscription_standardsSubscriptionArn,
    standardsSubscription_standardsArn,
    standardsSubscription_standardsInput,
    standardsSubscription_standardsStatus,

    -- * StandardsSubscriptionRequest
    StandardsSubscriptionRequest (..),
    newStandardsSubscriptionRequest,
    standardsSubscriptionRequest_standardsInput,
    standardsSubscriptionRequest_standardsArn,

    -- * StatelessCustomActionDefinition
    StatelessCustomActionDefinition (..),
    newStatelessCustomActionDefinition,
    statelessCustomActionDefinition_publishMetricAction,

    -- * StatelessCustomPublishMetricAction
    StatelessCustomPublishMetricAction (..),
    newStatelessCustomPublishMetricAction,
    statelessCustomPublishMetricAction_dimensions,

    -- * StatelessCustomPublishMetricActionDimension
    StatelessCustomPublishMetricActionDimension (..),
    newStatelessCustomPublishMetricActionDimension,
    statelessCustomPublishMetricActionDimension_value,

    -- * StatusReason
    StatusReason (..),
    newStatusReason,
    statusReason_description,
    statusReason_reasonCode,

    -- * StringFilter
    StringFilter (..),
    newStringFilter,
    stringFilter_comparison,
    stringFilter_value,

    -- * Threat
    Threat (..),
    newThreat,
    threat_severity,
    threat_name,
    threat_itemCount,
    threat_filePaths,

    -- * ThreatIntelIndicator
    ThreatIntelIndicator (..),
    newThreatIntelIndicator,
    threatIntelIndicator_type,
    threatIntelIndicator_source,
    threatIntelIndicator_lastObservedAt,
    threatIntelIndicator_category,
    threatIntelIndicator_sourceUrl,
    threatIntelIndicator_value,

    -- * VolumeMount
    VolumeMount (..),
    newVolumeMount,
    volumeMount_name,
    volumeMount_mountPath,

    -- * VpcInfoCidrBlockSetDetails
    VpcInfoCidrBlockSetDetails (..),
    newVpcInfoCidrBlockSetDetails,
    vpcInfoCidrBlockSetDetails_cidrBlock,

    -- * VpcInfoIpv6CidrBlockSetDetails
    VpcInfoIpv6CidrBlockSetDetails (..),
    newVpcInfoIpv6CidrBlockSetDetails,
    vpcInfoIpv6CidrBlockSetDetails_ipv6CidrBlock,

    -- * VpcInfoPeeringOptionsDetails
    VpcInfoPeeringOptionsDetails (..),
    newVpcInfoPeeringOptionsDetails,
    vpcInfoPeeringOptionsDetails_allowDnsResolutionFromRemoteVpc,
    vpcInfoPeeringOptionsDetails_allowEgressFromLocalVpcToRemoteClassicLink,
    vpcInfoPeeringOptionsDetails_allowEgressFromLocalClassicLinkToRemoteVpc,

    -- * Vulnerability
    Vulnerability (..),
    newVulnerability,
    vulnerability_vulnerablePackages,
    vulnerability_referenceUrls,
    vulnerability_cvss,
    vulnerability_relatedVulnerabilities,
    vulnerability_vendor,
    vulnerability_fixAvailable,
    vulnerability_id,

    -- * VulnerabilityVendor
    VulnerabilityVendor (..),
    newVulnerabilityVendor,
    vulnerabilityVendor_vendorUpdatedAt,
    vulnerabilityVendor_url,
    vulnerabilityVendor_vendorCreatedAt,
    vulnerabilityVendor_vendorSeverity,
    vulnerabilityVendor_name,

    -- * WafAction
    WafAction (..),
    newWafAction,
    wafAction_type,

    -- * WafExcludedRule
    WafExcludedRule (..),
    newWafExcludedRule,
    wafExcludedRule_ruleId,

    -- * WafOverrideAction
    WafOverrideAction (..),
    newWafOverrideAction,
    wafOverrideAction_type,

    -- * Workflow
    Workflow (..),
    newWorkflow,
    workflow_status,

    -- * WorkflowUpdate
    WorkflowUpdate (..),
    newWorkflowUpdate,
    workflowUpdate_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AccountDetails
import Amazonka.SecurityHub.Types.Action
import Amazonka.SecurityHub.Types.ActionLocalIpDetails
import Amazonka.SecurityHub.Types.ActionLocalPortDetails
import Amazonka.SecurityHub.Types.ActionRemoteIpDetails
import Amazonka.SecurityHub.Types.ActionRemotePortDetails
import Amazonka.SecurityHub.Types.ActionTarget
import Amazonka.SecurityHub.Types.Adjustment
import Amazonka.SecurityHub.Types.AdminAccount
import Amazonka.SecurityHub.Types.AdminStatus
import Amazonka.SecurityHub.Types.AutoEnableStandards
import Amazonka.SecurityHub.Types.AvailabilityZone
import Amazonka.SecurityHub.Types.AwsApiCallAction
import Amazonka.SecurityHub.Types.AwsApiCallActionDomainDetails
import Amazonka.SecurityHub.Types.AwsApiGatewayAccessLogSettings
import Amazonka.SecurityHub.Types.AwsApiGatewayCanarySettings
import Amazonka.SecurityHub.Types.AwsApiGatewayEndpointConfiguration
import Amazonka.SecurityHub.Types.AwsApiGatewayMethodSettings
import Amazonka.SecurityHub.Types.AwsApiGatewayRestApiDetails
import Amazonka.SecurityHub.Types.AwsApiGatewayStageDetails
import Amazonka.SecurityHub.Types.AwsApiGatewayV2ApiDetails
import Amazonka.SecurityHub.Types.AwsApiGatewayV2RouteSettings
import Amazonka.SecurityHub.Types.AwsApiGatewayV2StageDetails
import Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails
import Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupDetails
import Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification
import Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails
import Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails
import Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails
import Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification
import Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails
import Amazonka.SecurityHub.Types.AwsAutoScalingLaunchConfigurationBlockDeviceMappingsDetails
import Amazonka.SecurityHub.Types.AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails
import Amazonka.SecurityHub.Types.AwsAutoScalingLaunchConfigurationDetails
import Amazonka.SecurityHub.Types.AwsAutoScalingLaunchConfigurationInstanceMonitoringDetails
import Amazonka.SecurityHub.Types.AwsAutoScalingLaunchConfigurationMetadataOptions
import Amazonka.SecurityHub.Types.AwsBackupBackupPlanAdvancedBackupSettingsDetails
import Amazonka.SecurityHub.Types.AwsBackupBackupPlanBackupPlanDetails
import Amazonka.SecurityHub.Types.AwsBackupBackupPlanDetails
import Amazonka.SecurityHub.Types.AwsBackupBackupPlanLifecycleDetails
import Amazonka.SecurityHub.Types.AwsBackupBackupPlanRuleCopyActionsDetails
import Amazonka.SecurityHub.Types.AwsBackupBackupPlanRuleDetails
import Amazonka.SecurityHub.Types.AwsBackupBackupVaultDetails
import Amazonka.SecurityHub.Types.AwsBackupBackupVaultNotificationsDetails
import Amazonka.SecurityHub.Types.AwsBackupRecoveryPointCalculatedLifecycleDetails
import Amazonka.SecurityHub.Types.AwsBackupRecoveryPointCreatedByDetails
import Amazonka.SecurityHub.Types.AwsBackupRecoveryPointDetails
import Amazonka.SecurityHub.Types.AwsBackupRecoveryPointLifecycleDetails
import Amazonka.SecurityHub.Types.AwsCertificateManagerCertificateDetails
import Amazonka.SecurityHub.Types.AwsCertificateManagerCertificateDomainValidationOption
import Amazonka.SecurityHub.Types.AwsCertificateManagerCertificateExtendedKeyUsage
import Amazonka.SecurityHub.Types.AwsCertificateManagerCertificateKeyUsage
import Amazonka.SecurityHub.Types.AwsCertificateManagerCertificateOptions
import Amazonka.SecurityHub.Types.AwsCertificateManagerCertificateRenewalSummary
import Amazonka.SecurityHub.Types.AwsCertificateManagerCertificateResourceRecord
import Amazonka.SecurityHub.Types.AwsCloudFormationStackDetails
import Amazonka.SecurityHub.Types.AwsCloudFormationStackDriftInformationDetails
import Amazonka.SecurityHub.Types.AwsCloudFormationStackOutputsDetails
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionCacheBehavior
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionCacheBehaviors
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionDefaultCacheBehavior
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionDetails
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionLogging
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginCustomOriginConfig
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginGroup
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginGroupFailover
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginGroupFailoverStatusCodes
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginGroups
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginItem
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginS3OriginConfig
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginSslProtocols
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOrigins
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionViewerCertificate
import Amazonka.SecurityHub.Types.AwsCloudTrailTrailDetails
import Amazonka.SecurityHub.Types.AwsCloudWatchAlarmDetails
import Amazonka.SecurityHub.Types.AwsCloudWatchAlarmDimensionsDetails
import Amazonka.SecurityHub.Types.AwsCodeBuildProjectArtifactsDetails
import Amazonka.SecurityHub.Types.AwsCodeBuildProjectDetails
import Amazonka.SecurityHub.Types.AwsCodeBuildProjectEnvironment
import Amazonka.SecurityHub.Types.AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails
import Amazonka.SecurityHub.Types.AwsCodeBuildProjectEnvironmentRegistryCredential
import Amazonka.SecurityHub.Types.AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails
import Amazonka.SecurityHub.Types.AwsCodeBuildProjectLogsConfigDetails
import Amazonka.SecurityHub.Types.AwsCodeBuildProjectLogsConfigS3LogsDetails
import Amazonka.SecurityHub.Types.AwsCodeBuildProjectSource
import Amazonka.SecurityHub.Types.AwsCodeBuildProjectVpcConfig
import Amazonka.SecurityHub.Types.AwsCorsConfiguration
import Amazonka.SecurityHub.Types.AwsDynamoDbTableAttributeDefinition
import Amazonka.SecurityHub.Types.AwsDynamoDbTableBillingModeSummary
import Amazonka.SecurityHub.Types.AwsDynamoDbTableDetails
import Amazonka.SecurityHub.Types.AwsDynamoDbTableGlobalSecondaryIndex
import Amazonka.SecurityHub.Types.AwsDynamoDbTableKeySchema
import Amazonka.SecurityHub.Types.AwsDynamoDbTableLocalSecondaryIndex
import Amazonka.SecurityHub.Types.AwsDynamoDbTableProjection
import Amazonka.SecurityHub.Types.AwsDynamoDbTableProvisionedThroughput
import Amazonka.SecurityHub.Types.AwsDynamoDbTableProvisionedThroughputOverride
import Amazonka.SecurityHub.Types.AwsDynamoDbTableReplica
import Amazonka.SecurityHub.Types.AwsDynamoDbTableReplicaGlobalSecondaryIndex
import Amazonka.SecurityHub.Types.AwsDynamoDbTableRestoreSummary
import Amazonka.SecurityHub.Types.AwsDynamoDbTableSseDescription
import Amazonka.SecurityHub.Types.AwsDynamoDbTableStreamSpecification
import Amazonka.SecurityHub.Types.AwsEc2EipDetails
import Amazonka.SecurityHub.Types.AwsEc2InstanceDetails
import Amazonka.SecurityHub.Types.AwsEc2InstanceMetadataOptions
import Amazonka.SecurityHub.Types.AwsEc2InstanceNetworkInterfacesDetails
import Amazonka.SecurityHub.Types.AwsEc2NetworkAclAssociation
import Amazonka.SecurityHub.Types.AwsEc2NetworkAclDetails
import Amazonka.SecurityHub.Types.AwsEc2NetworkAclEntry
import Amazonka.SecurityHub.Types.AwsEc2NetworkInterfaceAttachment
import Amazonka.SecurityHub.Types.AwsEc2NetworkInterfaceDetails
import Amazonka.SecurityHub.Types.AwsEc2NetworkInterfaceIpV6AddressDetail
import Amazonka.SecurityHub.Types.AwsEc2NetworkInterfacePrivateIpAddressDetail
import Amazonka.SecurityHub.Types.AwsEc2NetworkInterfaceSecurityGroup
import Amazonka.SecurityHub.Types.AwsEc2SecurityGroupDetails
import Amazonka.SecurityHub.Types.AwsEc2SecurityGroupIpPermission
import Amazonka.SecurityHub.Types.AwsEc2SecurityGroupIpRange
import Amazonka.SecurityHub.Types.AwsEc2SecurityGroupIpv6Range
import Amazonka.SecurityHub.Types.AwsEc2SecurityGroupPrefixListId
import Amazonka.SecurityHub.Types.AwsEc2SecurityGroupUserIdGroupPair
import Amazonka.SecurityHub.Types.AwsEc2SubnetDetails
import Amazonka.SecurityHub.Types.AwsEc2TransitGatewayDetails
import Amazonka.SecurityHub.Types.AwsEc2VolumeAttachment
import Amazonka.SecurityHub.Types.AwsEc2VolumeDetails
import Amazonka.SecurityHub.Types.AwsEc2VpcDetails
import Amazonka.SecurityHub.Types.AwsEc2VpcEndpointServiceDetails
import Amazonka.SecurityHub.Types.AwsEc2VpcEndpointServiceServiceTypeDetails
import Amazonka.SecurityHub.Types.AwsEc2VpcPeeringConnectionDetails
import Amazonka.SecurityHub.Types.AwsEc2VpcPeeringConnectionStatusDetails
import Amazonka.SecurityHub.Types.AwsEc2VpcPeeringConnectionVpcInfoDetails
import Amazonka.SecurityHub.Types.AwsEc2VpnConnectionDetails
import Amazonka.SecurityHub.Types.AwsEc2VpnConnectionOptionsDetails
import Amazonka.SecurityHub.Types.AwsEc2VpnConnectionOptionsTunnelOptionsDetails
import Amazonka.SecurityHub.Types.AwsEc2VpnConnectionRoutesDetails
import Amazonka.SecurityHub.Types.AwsEc2VpnConnectionVgwTelemetryDetails
import Amazonka.SecurityHub.Types.AwsEcrContainerImageDetails
import Amazonka.SecurityHub.Types.AwsEcrRepositoryDetails
import Amazonka.SecurityHub.Types.AwsEcrRepositoryImageScanningConfigurationDetails
import Amazonka.SecurityHub.Types.AwsEcrRepositoryLifecyclePolicyDetails
import Amazonka.SecurityHub.Types.AwsEcsClusterClusterSettingsDetails
import Amazonka.SecurityHub.Types.AwsEcsClusterConfigurationDetails
import Amazonka.SecurityHub.Types.AwsEcsClusterConfigurationExecuteCommandConfigurationDetails
import Amazonka.SecurityHub.Types.AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails
import Amazonka.SecurityHub.Types.AwsEcsClusterDefaultCapacityProviderStrategyDetails
import Amazonka.SecurityHub.Types.AwsEcsClusterDetails
import Amazonka.SecurityHub.Types.AwsEcsContainerDetails
import Amazonka.SecurityHub.Types.AwsEcsServiceCapacityProviderStrategyDetails
import Amazonka.SecurityHub.Types.AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails
import Amazonka.SecurityHub.Types.AwsEcsServiceDeploymentConfigurationDetails
import Amazonka.SecurityHub.Types.AwsEcsServiceDeploymentControllerDetails
import Amazonka.SecurityHub.Types.AwsEcsServiceDetails
import Amazonka.SecurityHub.Types.AwsEcsServiceLoadBalancersDetails
import Amazonka.SecurityHub.Types.AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails
import Amazonka.SecurityHub.Types.AwsEcsServiceNetworkConfigurationDetails
import Amazonka.SecurityHub.Types.AwsEcsServicePlacementConstraintsDetails
import Amazonka.SecurityHub.Types.AwsEcsServicePlacementStrategiesDetails
import Amazonka.SecurityHub.Types.AwsEcsServiceServiceRegistriesDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsSecretsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionInferenceAcceleratorsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionPlacementConstraintsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionProxyConfigurationDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionVolumesDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionVolumesHostDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskVolumeDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskVolumeHostDetails
import Amazonka.SecurityHub.Types.AwsEfsAccessPointDetails
import Amazonka.SecurityHub.Types.AwsEfsAccessPointPosixUserDetails
import Amazonka.SecurityHub.Types.AwsEfsAccessPointRootDirectoryCreationInfoDetails
import Amazonka.SecurityHub.Types.AwsEfsAccessPointRootDirectoryDetails
import Amazonka.SecurityHub.Types.AwsEksClusterDetails
import Amazonka.SecurityHub.Types.AwsEksClusterLoggingClusterLoggingDetails
import Amazonka.SecurityHub.Types.AwsEksClusterLoggingDetails
import Amazonka.SecurityHub.Types.AwsEksClusterResourcesVpcConfigDetails
import Amazonka.SecurityHub.Types.AwsElasticBeanstalkEnvironmentDetails
import Amazonka.SecurityHub.Types.AwsElasticBeanstalkEnvironmentEnvironmentLink
import Amazonka.SecurityHub.Types.AwsElasticBeanstalkEnvironmentOptionSetting
import Amazonka.SecurityHub.Types.AwsElasticBeanstalkEnvironmentTier
import Amazonka.SecurityHub.Types.AwsElasticsearchDomainDetails
import Amazonka.SecurityHub.Types.AwsElasticsearchDomainDomainEndpointOptions
import Amazonka.SecurityHub.Types.AwsElasticsearchDomainElasticsearchClusterConfigDetails
import Amazonka.SecurityHub.Types.AwsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails
import Amazonka.SecurityHub.Types.AwsElasticsearchDomainEncryptionAtRestOptions
import Amazonka.SecurityHub.Types.AwsElasticsearchDomainLogPublishingOptions
import Amazonka.SecurityHub.Types.AwsElasticsearchDomainLogPublishingOptionsLogConfig
import Amazonka.SecurityHub.Types.AwsElasticsearchDomainNodeToNodeEncryptionOptions
import Amazonka.SecurityHub.Types.AwsElasticsearchDomainServiceSoftwareOptions
import Amazonka.SecurityHub.Types.AwsElasticsearchDomainVPCOptions
import Amazonka.SecurityHub.Types.AwsElbAppCookieStickinessPolicy
import Amazonka.SecurityHub.Types.AwsElbLbCookieStickinessPolicy
import Amazonka.SecurityHub.Types.AwsElbLoadBalancerAccessLog
import Amazonka.SecurityHub.Types.AwsElbLoadBalancerAdditionalAttribute
import Amazonka.SecurityHub.Types.AwsElbLoadBalancerAttributes
import Amazonka.SecurityHub.Types.AwsElbLoadBalancerBackendServerDescription
import Amazonka.SecurityHub.Types.AwsElbLoadBalancerConnectionDraining
import Amazonka.SecurityHub.Types.AwsElbLoadBalancerConnectionSettings
import Amazonka.SecurityHub.Types.AwsElbLoadBalancerCrossZoneLoadBalancing
import Amazonka.SecurityHub.Types.AwsElbLoadBalancerDetails
import Amazonka.SecurityHub.Types.AwsElbLoadBalancerHealthCheck
import Amazonka.SecurityHub.Types.AwsElbLoadBalancerInstance
import Amazonka.SecurityHub.Types.AwsElbLoadBalancerListener
import Amazonka.SecurityHub.Types.AwsElbLoadBalancerListenerDescription
import Amazonka.SecurityHub.Types.AwsElbLoadBalancerPolicies
import Amazonka.SecurityHub.Types.AwsElbLoadBalancerSourceSecurityGroup
import Amazonka.SecurityHub.Types.AwsElbv2LoadBalancerAttribute
import Amazonka.SecurityHub.Types.AwsElbv2LoadBalancerDetails
import Amazonka.SecurityHub.Types.AwsIamAccessKeyDetails
import Amazonka.SecurityHub.Types.AwsIamAccessKeySessionContext
import Amazonka.SecurityHub.Types.AwsIamAccessKeySessionContextAttributes
import Amazonka.SecurityHub.Types.AwsIamAccessKeySessionContextSessionIssuer
import Amazonka.SecurityHub.Types.AwsIamAccessKeyStatus
import Amazonka.SecurityHub.Types.AwsIamAttachedManagedPolicy
import Amazonka.SecurityHub.Types.AwsIamGroupDetails
import Amazonka.SecurityHub.Types.AwsIamGroupPolicy
import Amazonka.SecurityHub.Types.AwsIamInstanceProfile
import Amazonka.SecurityHub.Types.AwsIamInstanceProfileRole
import Amazonka.SecurityHub.Types.AwsIamPermissionsBoundary
import Amazonka.SecurityHub.Types.AwsIamPolicyDetails
import Amazonka.SecurityHub.Types.AwsIamPolicyVersion
import Amazonka.SecurityHub.Types.AwsIamRoleDetails
import Amazonka.SecurityHub.Types.AwsIamRolePolicy
import Amazonka.SecurityHub.Types.AwsIamUserDetails
import Amazonka.SecurityHub.Types.AwsIamUserPolicy
import Amazonka.SecurityHub.Types.AwsKinesisStreamDetails
import Amazonka.SecurityHub.Types.AwsKinesisStreamStreamEncryptionDetails
import Amazonka.SecurityHub.Types.AwsKmsKeyDetails
import Amazonka.SecurityHub.Types.AwsLambdaFunctionCode
import Amazonka.SecurityHub.Types.AwsLambdaFunctionDeadLetterConfig
import Amazonka.SecurityHub.Types.AwsLambdaFunctionDetails
import Amazonka.SecurityHub.Types.AwsLambdaFunctionEnvironment
import Amazonka.SecurityHub.Types.AwsLambdaFunctionEnvironmentError
import Amazonka.SecurityHub.Types.AwsLambdaFunctionLayer
import Amazonka.SecurityHub.Types.AwsLambdaFunctionTracingConfig
import Amazonka.SecurityHub.Types.AwsLambdaFunctionVpcConfig
import Amazonka.SecurityHub.Types.AwsLambdaLayerVersionDetails
import Amazonka.SecurityHub.Types.AwsMountPoint
import Amazonka.SecurityHub.Types.AwsNetworkFirewallFirewallDetails
import Amazonka.SecurityHub.Types.AwsNetworkFirewallFirewallPolicyDetails
import Amazonka.SecurityHub.Types.AwsNetworkFirewallFirewallSubnetMappingsDetails
import Amazonka.SecurityHub.Types.AwsNetworkFirewallRuleGroupDetails
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainClusterConfigDetails
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainDetails
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainDomainEndpointOptionsDetails
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainLogPublishingOption
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainLogPublishingOptionsDetails
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainMasterUserOptionsDetails
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainVpcOptionsDetails
import Amazonka.SecurityHub.Types.AwsRdsDbClusterAssociatedRole
import Amazonka.SecurityHub.Types.AwsRdsDbClusterDetails
import Amazonka.SecurityHub.Types.AwsRdsDbClusterMember
import Amazonka.SecurityHub.Types.AwsRdsDbClusterOptionGroupMembership
import Amazonka.SecurityHub.Types.AwsRdsDbClusterSnapshotDetails
import Amazonka.SecurityHub.Types.AwsRdsDbDomainMembership
import Amazonka.SecurityHub.Types.AwsRdsDbInstanceAssociatedRole
import Amazonka.SecurityHub.Types.AwsRdsDbInstanceDetails
import Amazonka.SecurityHub.Types.AwsRdsDbInstanceEndpoint
import Amazonka.SecurityHub.Types.AwsRdsDbInstanceVpcSecurityGroup
import Amazonka.SecurityHub.Types.AwsRdsDbOptionGroupMembership
import Amazonka.SecurityHub.Types.AwsRdsDbParameterGroup
import Amazonka.SecurityHub.Types.AwsRdsDbPendingModifiedValues
import Amazonka.SecurityHub.Types.AwsRdsDbProcessorFeature
import Amazonka.SecurityHub.Types.AwsRdsDbSecurityGroupDetails
import Amazonka.SecurityHub.Types.AwsRdsDbSecurityGroupEc2SecurityGroup
import Amazonka.SecurityHub.Types.AwsRdsDbSecurityGroupIpRange
import Amazonka.SecurityHub.Types.AwsRdsDbSnapshotDetails
import Amazonka.SecurityHub.Types.AwsRdsDbStatusInfo
import Amazonka.SecurityHub.Types.AwsRdsDbSubnetGroup
import Amazonka.SecurityHub.Types.AwsRdsDbSubnetGroupSubnet
import Amazonka.SecurityHub.Types.AwsRdsDbSubnetGroupSubnetAvailabilityZone
import Amazonka.SecurityHub.Types.AwsRdsEventSubscriptionDetails
import Amazonka.SecurityHub.Types.AwsRdsPendingCloudWatchLogsExports
import Amazonka.SecurityHub.Types.AwsRedshiftClusterClusterNode
import Amazonka.SecurityHub.Types.AwsRedshiftClusterClusterParameterGroup
import Amazonka.SecurityHub.Types.AwsRedshiftClusterClusterParameterStatus
import Amazonka.SecurityHub.Types.AwsRedshiftClusterClusterSecurityGroup
import Amazonka.SecurityHub.Types.AwsRedshiftClusterClusterSnapshotCopyStatus
import Amazonka.SecurityHub.Types.AwsRedshiftClusterDeferredMaintenanceWindow
import Amazonka.SecurityHub.Types.AwsRedshiftClusterDetails
import Amazonka.SecurityHub.Types.AwsRedshiftClusterElasticIpStatus
import Amazonka.SecurityHub.Types.AwsRedshiftClusterEndpoint
import Amazonka.SecurityHub.Types.AwsRedshiftClusterHsmStatus
import Amazonka.SecurityHub.Types.AwsRedshiftClusterIamRole
import Amazonka.SecurityHub.Types.AwsRedshiftClusterLoggingStatus
import Amazonka.SecurityHub.Types.AwsRedshiftClusterPendingModifiedValues
import Amazonka.SecurityHub.Types.AwsRedshiftClusterResizeInfo
import Amazonka.SecurityHub.Types.AwsRedshiftClusterRestoreStatus
import Amazonka.SecurityHub.Types.AwsRedshiftClusterVpcSecurityGroup
import Amazonka.SecurityHub.Types.AwsS3AccountPublicAccessBlockDetails
import Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationDetails
import Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails
import Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesDetails
import Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesFilterDetails
import Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails
import Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails
import Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails
import Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails
import Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails
import Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails
import Amazonka.SecurityHub.Types.AwsS3BucketBucketVersioningConfiguration
import Amazonka.SecurityHub.Types.AwsS3BucketDetails
import Amazonka.SecurityHub.Types.AwsS3BucketLoggingConfiguration
import Amazonka.SecurityHub.Types.AwsS3BucketNotificationConfiguration
import Amazonka.SecurityHub.Types.AwsS3BucketNotificationConfigurationDetail
import Amazonka.SecurityHub.Types.AwsS3BucketNotificationConfigurationFilter
import Amazonka.SecurityHub.Types.AwsS3BucketNotificationConfigurationS3KeyFilter
import Amazonka.SecurityHub.Types.AwsS3BucketNotificationConfigurationS3KeyFilterRule
import Amazonka.SecurityHub.Types.AwsS3BucketNotificationConfigurationS3KeyFilterRuleName
import Amazonka.SecurityHub.Types.AwsS3BucketServerSideEncryptionByDefault
import Amazonka.SecurityHub.Types.AwsS3BucketServerSideEncryptionConfiguration
import Amazonka.SecurityHub.Types.AwsS3BucketServerSideEncryptionRule
import Amazonka.SecurityHub.Types.AwsS3BucketWebsiteConfiguration
import Amazonka.SecurityHub.Types.AwsS3BucketWebsiteConfigurationRedirectTo
import Amazonka.SecurityHub.Types.AwsS3BucketWebsiteConfigurationRoutingRule
import Amazonka.SecurityHub.Types.AwsS3BucketWebsiteConfigurationRoutingRuleCondition
import Amazonka.SecurityHub.Types.AwsS3BucketWebsiteConfigurationRoutingRuleRedirect
import Amazonka.SecurityHub.Types.AwsS3ObjectDetails
import Amazonka.SecurityHub.Types.AwsSecretsManagerSecretDetails
import Amazonka.SecurityHub.Types.AwsSecretsManagerSecretRotationRules
import Amazonka.SecurityHub.Types.AwsSecurityFinding
import Amazonka.SecurityHub.Types.AwsSecurityFindingFilters
import Amazonka.SecurityHub.Types.AwsSecurityFindingIdentifier
import Amazonka.SecurityHub.Types.AwsSnsTopicDetails
import Amazonka.SecurityHub.Types.AwsSnsTopicSubscription
import Amazonka.SecurityHub.Types.AwsSqsQueueDetails
import Amazonka.SecurityHub.Types.AwsSsmComplianceSummary
import Amazonka.SecurityHub.Types.AwsSsmPatch
import Amazonka.SecurityHub.Types.AwsSsmPatchComplianceDetails
import Amazonka.SecurityHub.Types.AwsWafRateBasedRuleDetails
import Amazonka.SecurityHub.Types.AwsWafRateBasedRuleMatchPredicate
import Amazonka.SecurityHub.Types.AwsWafRegionalRateBasedRuleDetails
import Amazonka.SecurityHub.Types.AwsWafRegionalRateBasedRuleMatchPredicate
import Amazonka.SecurityHub.Types.AwsWafRegionalRuleDetails
import Amazonka.SecurityHub.Types.AwsWafRegionalRuleGroupDetails
import Amazonka.SecurityHub.Types.AwsWafRegionalRuleGroupRulesActionDetails
import Amazonka.SecurityHub.Types.AwsWafRegionalRuleGroupRulesDetails
import Amazonka.SecurityHub.Types.AwsWafRegionalRulePredicateListDetails
import Amazonka.SecurityHub.Types.AwsWafRegionalWebAclDetails
import Amazonka.SecurityHub.Types.AwsWafRegionalWebAclRulesListActionDetails
import Amazonka.SecurityHub.Types.AwsWafRegionalWebAclRulesListDetails
import Amazonka.SecurityHub.Types.AwsWafRegionalWebAclRulesListOverrideActionDetails
import Amazonka.SecurityHub.Types.AwsWafRuleDetails
import Amazonka.SecurityHub.Types.AwsWafRuleGroupDetails
import Amazonka.SecurityHub.Types.AwsWafRuleGroupRulesActionDetails
import Amazonka.SecurityHub.Types.AwsWafRuleGroupRulesDetails
import Amazonka.SecurityHub.Types.AwsWafRulePredicateListDetails
import Amazonka.SecurityHub.Types.AwsWafWebAclDetails
import Amazonka.SecurityHub.Types.AwsWafWebAclRule
import Amazonka.SecurityHub.Types.AwsXrayEncryptionConfigDetails
import Amazonka.SecurityHub.Types.BatchUpdateFindingsUnprocessedFinding
import Amazonka.SecurityHub.Types.BooleanFilter
import Amazonka.SecurityHub.Types.Cell
import Amazonka.SecurityHub.Types.CidrBlockAssociation
import Amazonka.SecurityHub.Types.City
import Amazonka.SecurityHub.Types.ClassificationResult
import Amazonka.SecurityHub.Types.ClassificationStatus
import Amazonka.SecurityHub.Types.Compliance
import Amazonka.SecurityHub.Types.ComplianceStatus
import Amazonka.SecurityHub.Types.ContainerDetails
import Amazonka.SecurityHub.Types.ControlStatus
import Amazonka.SecurityHub.Types.Country
import Amazonka.SecurityHub.Types.CustomDataIdentifiersDetections
import Amazonka.SecurityHub.Types.CustomDataIdentifiersResult
import Amazonka.SecurityHub.Types.Cvss
import Amazonka.SecurityHub.Types.DataClassificationDetails
import Amazonka.SecurityHub.Types.DateFilter
import Amazonka.SecurityHub.Types.DateRange
import Amazonka.SecurityHub.Types.DateRangeUnit
import Amazonka.SecurityHub.Types.DnsRequestAction
import Amazonka.SecurityHub.Types.FilePaths
import Amazonka.SecurityHub.Types.FindingAggregator
import Amazonka.SecurityHub.Types.FindingProviderFields
import Amazonka.SecurityHub.Types.FindingProviderSeverity
import Amazonka.SecurityHub.Types.FirewallPolicyDetails
import Amazonka.SecurityHub.Types.FirewallPolicyStatefulRuleGroupReferencesDetails
import Amazonka.SecurityHub.Types.FirewallPolicyStatelessCustomActionsDetails
import Amazonka.SecurityHub.Types.FirewallPolicyStatelessRuleGroupReferencesDetails
import Amazonka.SecurityHub.Types.GeoLocation
import Amazonka.SecurityHub.Types.IcmpTypeCode
import Amazonka.SecurityHub.Types.ImportFindingsError
import Amazonka.SecurityHub.Types.Insight
import Amazonka.SecurityHub.Types.InsightResultValue
import Amazonka.SecurityHub.Types.InsightResults
import Amazonka.SecurityHub.Types.IntegrationType
import Amazonka.SecurityHub.Types.Invitation
import Amazonka.SecurityHub.Types.IpFilter
import Amazonka.SecurityHub.Types.IpOrganizationDetails
import Amazonka.SecurityHub.Types.Ipv6CidrBlockAssociation
import Amazonka.SecurityHub.Types.KeywordFilter
import Amazonka.SecurityHub.Types.LoadBalancerState
import Amazonka.SecurityHub.Types.Malware
import Amazonka.SecurityHub.Types.MalwareState
import Amazonka.SecurityHub.Types.MalwareType
import Amazonka.SecurityHub.Types.MapFilter
import Amazonka.SecurityHub.Types.MapFilterComparison
import Amazonka.SecurityHub.Types.Member
import Amazonka.SecurityHub.Types.Network
import Amazonka.SecurityHub.Types.NetworkConnectionAction
import Amazonka.SecurityHub.Types.NetworkDirection
import Amazonka.SecurityHub.Types.NetworkHeader
import Amazonka.SecurityHub.Types.NetworkPathComponent
import Amazonka.SecurityHub.Types.NetworkPathComponentDetails
import Amazonka.SecurityHub.Types.Note
import Amazonka.SecurityHub.Types.NoteUpdate
import Amazonka.SecurityHub.Types.NumberFilter
import Amazonka.SecurityHub.Types.Occurrences
import Amazonka.SecurityHub.Types.Page
import Amazonka.SecurityHub.Types.Partition
import Amazonka.SecurityHub.Types.PatchSummary
import Amazonka.SecurityHub.Types.PortProbeAction
import Amazonka.SecurityHub.Types.PortProbeDetail
import Amazonka.SecurityHub.Types.PortRange
import Amazonka.SecurityHub.Types.PortRangeFromTo
import Amazonka.SecurityHub.Types.ProcessDetails
import Amazonka.SecurityHub.Types.Product
import Amazonka.SecurityHub.Types.Range
import Amazonka.SecurityHub.Types.Recommendation
import Amazonka.SecurityHub.Types.Record
import Amazonka.SecurityHub.Types.RecordState
import Amazonka.SecurityHub.Types.RelatedFinding
import Amazonka.SecurityHub.Types.Remediation
import Amazonka.SecurityHub.Types.Resource
import Amazonka.SecurityHub.Types.ResourceDetails
import Amazonka.SecurityHub.Types.Result
import Amazonka.SecurityHub.Types.RuleGroupDetails
import Amazonka.SecurityHub.Types.RuleGroupSource
import Amazonka.SecurityHub.Types.RuleGroupSourceCustomActionsDetails
import Amazonka.SecurityHub.Types.RuleGroupSourceListDetails
import Amazonka.SecurityHub.Types.RuleGroupSourceStatefulRulesDetails
import Amazonka.SecurityHub.Types.RuleGroupSourceStatefulRulesHeaderDetails
import Amazonka.SecurityHub.Types.RuleGroupSourceStatefulRulesOptionsDetails
import Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRuleDefinition
import Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRuleMatchAttributes
import Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts
import Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRuleMatchAttributesDestinations
import Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRuleMatchAttributesSourcePorts
import Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRuleMatchAttributesSources
import Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRuleMatchAttributesTcpFlags
import Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRulesAndCustomActionsDetails
import Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRulesDetails
import Amazonka.SecurityHub.Types.RuleGroupVariables
import Amazonka.SecurityHub.Types.RuleGroupVariablesIpSetsDetails
import Amazonka.SecurityHub.Types.RuleGroupVariablesPortSetsDetails
import Amazonka.SecurityHub.Types.SensitiveDataDetections
import Amazonka.SecurityHub.Types.SensitiveDataResult
import Amazonka.SecurityHub.Types.Severity
import Amazonka.SecurityHub.Types.SeverityLabel
import Amazonka.SecurityHub.Types.SeverityRating
import Amazonka.SecurityHub.Types.SeverityUpdate
import Amazonka.SecurityHub.Types.SoftwarePackage
import Amazonka.SecurityHub.Types.SortCriterion
import Amazonka.SecurityHub.Types.SortOrder
import Amazonka.SecurityHub.Types.Standard
import Amazonka.SecurityHub.Types.StandardsControl
import Amazonka.SecurityHub.Types.StandardsStatus
import Amazonka.SecurityHub.Types.StandardsStatusReason
import Amazonka.SecurityHub.Types.StandardsSubscription
import Amazonka.SecurityHub.Types.StandardsSubscriptionRequest
import Amazonka.SecurityHub.Types.StatelessCustomActionDefinition
import Amazonka.SecurityHub.Types.StatelessCustomPublishMetricAction
import Amazonka.SecurityHub.Types.StatelessCustomPublishMetricActionDimension
import Amazonka.SecurityHub.Types.StatusReason
import Amazonka.SecurityHub.Types.StatusReasonCode
import Amazonka.SecurityHub.Types.StringFilter
import Amazonka.SecurityHub.Types.StringFilterComparison
import Amazonka.SecurityHub.Types.Threat
import Amazonka.SecurityHub.Types.ThreatIntelIndicator
import Amazonka.SecurityHub.Types.ThreatIntelIndicatorCategory
import Amazonka.SecurityHub.Types.ThreatIntelIndicatorType
import Amazonka.SecurityHub.Types.VerificationState
import Amazonka.SecurityHub.Types.VolumeMount
import Amazonka.SecurityHub.Types.VpcInfoCidrBlockSetDetails
import Amazonka.SecurityHub.Types.VpcInfoIpv6CidrBlockSetDetails
import Amazonka.SecurityHub.Types.VpcInfoPeeringOptionsDetails
import Amazonka.SecurityHub.Types.Vulnerability
import Amazonka.SecurityHub.Types.VulnerabilityFixAvailable
import Amazonka.SecurityHub.Types.VulnerabilityVendor
import Amazonka.SecurityHub.Types.WafAction
import Amazonka.SecurityHub.Types.WafExcludedRule
import Amazonka.SecurityHub.Types.WafOverrideAction
import Amazonka.SecurityHub.Types.Workflow
import Amazonka.SecurityHub.Types.WorkflowState
import Amazonka.SecurityHub.Types.WorkflowStatus
import Amazonka.SecurityHub.Types.WorkflowUpdate
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-10-26@ of the Amazon SecurityHub SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "SecurityHub",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "securityhub",
      Core.signingName = "securityhub",
      Core.version = "2018-10-26",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "SecurityHub",
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

-- | The request was rejected because you supplied an invalid or out-of-range
-- value for an input parameter.
_InvalidInputException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInputException =
  Core._MatchServiceError
    defaultService
    "InvalidInputException"
    Prelude.. Core.hasStatus 400

-- | There is an issue with the account used to make the request. Either
-- Security Hub is not enabled for the account, or the account does not
-- have permission to perform this action.
_InvalidAccessException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAccessException =
  Core._MatchServiceError
    defaultService
    "InvalidAccessException"
    Prelude.. Core.hasStatus 401

-- | You don\'t have permission to perform the action specified in the
-- request.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The request was rejected because we can\'t find the specified resource.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The request was rejected because it attempted to create resources beyond
-- the current Amazon Web Services account or throttling limits. The error
-- code describes the limit exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 429

-- | The resource specified in the request conflicts with an existing
-- resource.
_ResourceConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceConflictException =
  Core._MatchServiceError
    defaultService
    "ResourceConflictException"
    Prelude.. Core.hasStatus 409

-- | Internal server error.
_InternalException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalException =
  Core._MatchServiceError
    defaultService
    "InternalException"
    Prelude.. Core.hasStatus 500

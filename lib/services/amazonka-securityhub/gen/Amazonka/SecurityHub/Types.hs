{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SecurityHub.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _InternalException,
    _InvalidAccessException,
    _InvalidInputException,
    _LimitExceededException,
    _ResourceConflictException,
    _ResourceNotFoundException,

    -- * AdminStatus
    AdminStatus (..),

    -- * AssociationStatus
    AssociationStatus (..),

    -- * AutoEnableStandards
    AutoEnableStandards (..),

    -- * AutomationRulesActionType
    AutomationRulesActionType (..),

    -- * AwsIamAccessKeyStatus
    AwsIamAccessKeyStatus (..),

    -- * AwsS3BucketNotificationConfigurationS3KeyFilterRuleName
    AwsS3BucketNotificationConfigurationS3KeyFilterRuleName (..),

    -- * ComplianceStatus
    ComplianceStatus (..),

    -- * ControlFindingGenerator
    ControlFindingGenerator (..),

    -- * ControlStatus
    ControlStatus (..),

    -- * DateRangeUnit
    DateRangeUnit (..),

    -- * FindingHistoryUpdateSourceType
    FindingHistoryUpdateSourceType (..),

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

    -- * RegionAvailabilityStatus
    RegionAvailabilityStatus (..),

    -- * RuleStatus
    RuleStatus (..),

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

    -- * UnprocessedErrorCode
    UnprocessedErrorCode (..),

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
    action_awsApiCallAction,
    action_dnsRequestAction,
    action_networkConnectionAction,
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
    actionRemoteIpDetails_city,
    actionRemoteIpDetails_country,
    actionRemoteIpDetails_geoLocation,
    actionRemoteIpDetails_ipAddressV4,
    actionRemoteIpDetails_organization,

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
    adminAccount_accountId,
    adminAccount_status,

    -- * AssociatedStandard
    AssociatedStandard (..),
    newAssociatedStandard,
    associatedStandard_standardsId,

    -- * AssociationSetDetails
    AssociationSetDetails (..),
    newAssociationSetDetails,
    associationSetDetails_associationState,
    associationSetDetails_gatewayId,
    associationSetDetails_main,
    associationSetDetails_routeTableAssociationId,
    associationSetDetails_routeTableId,
    associationSetDetails_subnetId,

    -- * AssociationStateDetails
    AssociationStateDetails (..),
    newAssociationStateDetails,
    associationStateDetails_state,
    associationStateDetails_statusMessage,

    -- * AutomationRulesAction
    AutomationRulesAction (..),
    newAutomationRulesAction,
    automationRulesAction_findingFieldsUpdate,
    automationRulesAction_type,

    -- * AutomationRulesConfig
    AutomationRulesConfig (..),
    newAutomationRulesConfig,
    automationRulesConfig_actions,
    automationRulesConfig_createdAt,
    automationRulesConfig_createdBy,
    automationRulesConfig_criteria,
    automationRulesConfig_description,
    automationRulesConfig_isTerminal,
    automationRulesConfig_ruleArn,
    automationRulesConfig_ruleName,
    automationRulesConfig_ruleOrder,
    automationRulesConfig_ruleStatus,
    automationRulesConfig_updatedAt,

    -- * AutomationRulesFindingFieldsUpdate
    AutomationRulesFindingFieldsUpdate (..),
    newAutomationRulesFindingFieldsUpdate,
    automationRulesFindingFieldsUpdate_confidence,
    automationRulesFindingFieldsUpdate_criticality,
    automationRulesFindingFieldsUpdate_note,
    automationRulesFindingFieldsUpdate_relatedFindings,
    automationRulesFindingFieldsUpdate_severity,
    automationRulesFindingFieldsUpdate_types,
    automationRulesFindingFieldsUpdate_userDefinedFields,
    automationRulesFindingFieldsUpdate_verificationState,
    automationRulesFindingFieldsUpdate_workflow,

    -- * AutomationRulesFindingFilters
    AutomationRulesFindingFilters (..),
    newAutomationRulesFindingFilters,
    automationRulesFindingFilters_awsAccountId,
    automationRulesFindingFilters_companyName,
    automationRulesFindingFilters_complianceAssociatedStandardsId,
    automationRulesFindingFilters_complianceSecurityControlId,
    automationRulesFindingFilters_complianceStatus,
    automationRulesFindingFilters_confidence,
    automationRulesFindingFilters_createdAt,
    automationRulesFindingFilters_criticality,
    automationRulesFindingFilters_description,
    automationRulesFindingFilters_firstObservedAt,
    automationRulesFindingFilters_generatorId,
    automationRulesFindingFilters_id,
    automationRulesFindingFilters_lastObservedAt,
    automationRulesFindingFilters_noteText,
    automationRulesFindingFilters_noteUpdatedAt,
    automationRulesFindingFilters_noteUpdatedBy,
    automationRulesFindingFilters_productArn,
    automationRulesFindingFilters_productName,
    automationRulesFindingFilters_recordState,
    automationRulesFindingFilters_relatedFindingsId,
    automationRulesFindingFilters_relatedFindingsProductArn,
    automationRulesFindingFilters_resourceDetailsOther,
    automationRulesFindingFilters_resourceId,
    automationRulesFindingFilters_resourcePartition,
    automationRulesFindingFilters_resourceRegion,
    automationRulesFindingFilters_resourceTags,
    automationRulesFindingFilters_resourceType,
    automationRulesFindingFilters_severityLabel,
    automationRulesFindingFilters_sourceUrl,
    automationRulesFindingFilters_title,
    automationRulesFindingFilters_type,
    automationRulesFindingFilters_updatedAt,
    automationRulesFindingFilters_userDefinedFields,
    automationRulesFindingFilters_verificationState,
    automationRulesFindingFilters_workflowStatus,

    -- * AutomationRulesMetadata
    AutomationRulesMetadata (..),
    newAutomationRulesMetadata,
    automationRulesMetadata_createdAt,
    automationRulesMetadata_createdBy,
    automationRulesMetadata_description,
    automationRulesMetadata_isTerminal,
    automationRulesMetadata_ruleArn,
    automationRulesMetadata_ruleName,
    automationRulesMetadata_ruleOrder,
    automationRulesMetadata_ruleStatus,
    automationRulesMetadata_updatedAt,

    -- * AvailabilityZone
    AvailabilityZone (..),
    newAvailabilityZone,
    availabilityZone_subnetId,
    availabilityZone_zoneName,

    -- * AwsAmazonMqBrokerDetails
    AwsAmazonMqBrokerDetails (..),
    newAwsAmazonMqBrokerDetails,
    awsAmazonMqBrokerDetails_authenticationStrategy,
    awsAmazonMqBrokerDetails_autoMinorVersionUpgrade,
    awsAmazonMqBrokerDetails_brokerArn,
    awsAmazonMqBrokerDetails_brokerId,
    awsAmazonMqBrokerDetails_brokerName,
    awsAmazonMqBrokerDetails_deploymentMode,
    awsAmazonMqBrokerDetails_encryptionOptions,
    awsAmazonMqBrokerDetails_engineType,
    awsAmazonMqBrokerDetails_engineVersion,
    awsAmazonMqBrokerDetails_hostInstanceType,
    awsAmazonMqBrokerDetails_ldapServerMetadata,
    awsAmazonMqBrokerDetails_logs,
    awsAmazonMqBrokerDetails_maintenanceWindowStartTime,
    awsAmazonMqBrokerDetails_publiclyAccessible,
    awsAmazonMqBrokerDetails_securityGroups,
    awsAmazonMqBrokerDetails_storageType,
    awsAmazonMqBrokerDetails_subnetIds,
    awsAmazonMqBrokerDetails_users,

    -- * AwsAmazonMqBrokerEncryptionOptionsDetails
    AwsAmazonMqBrokerEncryptionOptionsDetails (..),
    newAwsAmazonMqBrokerEncryptionOptionsDetails,
    awsAmazonMqBrokerEncryptionOptionsDetails_kmsKeyId,
    awsAmazonMqBrokerEncryptionOptionsDetails_useAwsOwnedKey,

    -- * AwsAmazonMqBrokerLdapServerMetadataDetails
    AwsAmazonMqBrokerLdapServerMetadataDetails (..),
    newAwsAmazonMqBrokerLdapServerMetadataDetails,
    awsAmazonMqBrokerLdapServerMetadataDetails_hosts,
    awsAmazonMqBrokerLdapServerMetadataDetails_roleBase,
    awsAmazonMqBrokerLdapServerMetadataDetails_roleName,
    awsAmazonMqBrokerLdapServerMetadataDetails_roleSearchMatching,
    awsAmazonMqBrokerLdapServerMetadataDetails_roleSearchSubtree,
    awsAmazonMqBrokerLdapServerMetadataDetails_serviceAccountUsername,
    awsAmazonMqBrokerLdapServerMetadataDetails_userBase,
    awsAmazonMqBrokerLdapServerMetadataDetails_userRoleName,
    awsAmazonMqBrokerLdapServerMetadataDetails_userSearchMatching,
    awsAmazonMqBrokerLdapServerMetadataDetails_userSearchSubtree,

    -- * AwsAmazonMqBrokerLogsDetails
    AwsAmazonMqBrokerLogsDetails (..),
    newAwsAmazonMqBrokerLogsDetails,
    awsAmazonMqBrokerLogsDetails_audit,
    awsAmazonMqBrokerLogsDetails_auditLogGroup,
    awsAmazonMqBrokerLogsDetails_general,
    awsAmazonMqBrokerLogsDetails_generalLogGroup,
    awsAmazonMqBrokerLogsDetails_pending,

    -- * AwsAmazonMqBrokerLogsPendingDetails
    AwsAmazonMqBrokerLogsPendingDetails (..),
    newAwsAmazonMqBrokerLogsPendingDetails,
    awsAmazonMqBrokerLogsPendingDetails_audit,
    awsAmazonMqBrokerLogsPendingDetails_general,

    -- * AwsAmazonMqBrokerMaintenanceWindowStartTimeDetails
    AwsAmazonMqBrokerMaintenanceWindowStartTimeDetails (..),
    newAwsAmazonMqBrokerMaintenanceWindowStartTimeDetails,
    awsAmazonMqBrokerMaintenanceWindowStartTimeDetails_dayOfWeek,
    awsAmazonMqBrokerMaintenanceWindowStartTimeDetails_timeOfDay,
    awsAmazonMqBrokerMaintenanceWindowStartTimeDetails_timeZone,

    -- * AwsAmazonMqBrokerUsersDetails
    AwsAmazonMqBrokerUsersDetails (..),
    newAwsAmazonMqBrokerUsersDetails,
    awsAmazonMqBrokerUsersDetails_pendingChange,
    awsAmazonMqBrokerUsersDetails_username,

    -- * AwsApiCallAction
    AwsApiCallAction (..),
    newAwsApiCallAction,
    awsApiCallAction_affectedResources,
    awsApiCallAction_api,
    awsApiCallAction_callerType,
    awsApiCallAction_domainDetails,
    awsApiCallAction_firstSeen,
    awsApiCallAction_lastSeen,
    awsApiCallAction_remoteIpDetails,
    awsApiCallAction_serviceName,

    -- * AwsApiCallActionDomainDetails
    AwsApiCallActionDomainDetails (..),
    newAwsApiCallActionDomainDetails,
    awsApiCallActionDomainDetails_domain,

    -- * AwsApiGatewayAccessLogSettings
    AwsApiGatewayAccessLogSettings (..),
    newAwsApiGatewayAccessLogSettings,
    awsApiGatewayAccessLogSettings_destinationArn,
    awsApiGatewayAccessLogSettings_format,

    -- * AwsApiGatewayCanarySettings
    AwsApiGatewayCanarySettings (..),
    newAwsApiGatewayCanarySettings,
    awsApiGatewayCanarySettings_deploymentId,
    awsApiGatewayCanarySettings_percentTraffic,
    awsApiGatewayCanarySettings_stageVariableOverrides,
    awsApiGatewayCanarySettings_useStageCache,

    -- * AwsApiGatewayEndpointConfiguration
    AwsApiGatewayEndpointConfiguration (..),
    newAwsApiGatewayEndpointConfiguration,
    awsApiGatewayEndpointConfiguration_types,

    -- * AwsApiGatewayMethodSettings
    AwsApiGatewayMethodSettings (..),
    newAwsApiGatewayMethodSettings,
    awsApiGatewayMethodSettings_cacheDataEncrypted,
    awsApiGatewayMethodSettings_cacheTtlInSeconds,
    awsApiGatewayMethodSettings_cachingEnabled,
    awsApiGatewayMethodSettings_dataTraceEnabled,
    awsApiGatewayMethodSettings_httpMethod,
    awsApiGatewayMethodSettings_loggingLevel,
    awsApiGatewayMethodSettings_metricsEnabled,
    awsApiGatewayMethodSettings_requireAuthorizationForCacheControl,
    awsApiGatewayMethodSettings_resourcePath,
    awsApiGatewayMethodSettings_throttlingBurstLimit,
    awsApiGatewayMethodSettings_throttlingRateLimit,
    awsApiGatewayMethodSettings_unauthorizedCacheControlHeaderStrategy,

    -- * AwsApiGatewayRestApiDetails
    AwsApiGatewayRestApiDetails (..),
    newAwsApiGatewayRestApiDetails,
    awsApiGatewayRestApiDetails_apiKeySource,
    awsApiGatewayRestApiDetails_binaryMediaTypes,
    awsApiGatewayRestApiDetails_createdDate,
    awsApiGatewayRestApiDetails_description,
    awsApiGatewayRestApiDetails_endpointConfiguration,
    awsApiGatewayRestApiDetails_id,
    awsApiGatewayRestApiDetails_minimumCompressionSize,
    awsApiGatewayRestApiDetails_name,
    awsApiGatewayRestApiDetails_version,

    -- * AwsApiGatewayStageDetails
    AwsApiGatewayStageDetails (..),
    newAwsApiGatewayStageDetails,
    awsApiGatewayStageDetails_accessLogSettings,
    awsApiGatewayStageDetails_cacheClusterEnabled,
    awsApiGatewayStageDetails_cacheClusterSize,
    awsApiGatewayStageDetails_cacheClusterStatus,
    awsApiGatewayStageDetails_canarySettings,
    awsApiGatewayStageDetails_clientCertificateId,
    awsApiGatewayStageDetails_createdDate,
    awsApiGatewayStageDetails_deploymentId,
    awsApiGatewayStageDetails_description,
    awsApiGatewayStageDetails_documentationVersion,
    awsApiGatewayStageDetails_lastUpdatedDate,
    awsApiGatewayStageDetails_methodSettings,
    awsApiGatewayStageDetails_stageName,
    awsApiGatewayStageDetails_tracingEnabled,
    awsApiGatewayStageDetails_variables,
    awsApiGatewayStageDetails_webAclArn,

    -- * AwsApiGatewayV2ApiDetails
    AwsApiGatewayV2ApiDetails (..),
    newAwsApiGatewayV2ApiDetails,
    awsApiGatewayV2ApiDetails_apiEndpoint,
    awsApiGatewayV2ApiDetails_apiId,
    awsApiGatewayV2ApiDetails_apiKeySelectionExpression,
    awsApiGatewayV2ApiDetails_corsConfiguration,
    awsApiGatewayV2ApiDetails_createdDate,
    awsApiGatewayV2ApiDetails_description,
    awsApiGatewayV2ApiDetails_name,
    awsApiGatewayV2ApiDetails_protocolType,
    awsApiGatewayV2ApiDetails_routeSelectionExpression,
    awsApiGatewayV2ApiDetails_version,

    -- * AwsApiGatewayV2RouteSettings
    AwsApiGatewayV2RouteSettings (..),
    newAwsApiGatewayV2RouteSettings,
    awsApiGatewayV2RouteSettings_dataTraceEnabled,
    awsApiGatewayV2RouteSettings_detailedMetricsEnabled,
    awsApiGatewayV2RouteSettings_loggingLevel,
    awsApiGatewayV2RouteSettings_throttlingBurstLimit,
    awsApiGatewayV2RouteSettings_throttlingRateLimit,

    -- * AwsApiGatewayV2StageDetails
    AwsApiGatewayV2StageDetails (..),
    newAwsApiGatewayV2StageDetails,
    awsApiGatewayV2StageDetails_accessLogSettings,
    awsApiGatewayV2StageDetails_apiGatewayManaged,
    awsApiGatewayV2StageDetails_autoDeploy,
    awsApiGatewayV2StageDetails_clientCertificateId,
    awsApiGatewayV2StageDetails_createdDate,
    awsApiGatewayV2StageDetails_defaultRouteSettings,
    awsApiGatewayV2StageDetails_deploymentId,
    awsApiGatewayV2StageDetails_description,
    awsApiGatewayV2StageDetails_lastDeploymentStatusMessage,
    awsApiGatewayV2StageDetails_lastUpdatedDate,
    awsApiGatewayV2StageDetails_routeSettings,
    awsApiGatewayV2StageDetails_stageName,
    awsApiGatewayV2StageDetails_stageVariables,

    -- * AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails
    AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails (..),
    newAwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails,
    awsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails_authenticationType,
    awsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails_lambdaAuthorizerConfig,
    awsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails_openIdConnectConfig,
    awsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails_userPoolConfig,

    -- * AwsAppSyncGraphQlApiDetails
    AwsAppSyncGraphQlApiDetails (..),
    newAwsAppSyncGraphQlApiDetails,
    awsAppSyncGraphQlApiDetails_additionalAuthenticationProviders,
    awsAppSyncGraphQlApiDetails_apiId,
    awsAppSyncGraphQlApiDetails_arn,
    awsAppSyncGraphQlApiDetails_authenticationType,
    awsAppSyncGraphQlApiDetails_id,
    awsAppSyncGraphQlApiDetails_lambdaAuthorizerConfig,
    awsAppSyncGraphQlApiDetails_logConfig,
    awsAppSyncGraphQlApiDetails_name,
    awsAppSyncGraphQlApiDetails_openIdConnectConfig,
    awsAppSyncGraphQlApiDetails_userPoolConfig,
    awsAppSyncGraphQlApiDetails_wafWebAclArn,
    awsAppSyncGraphQlApiDetails_xrayEnabled,

    -- * AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails
    AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails (..),
    newAwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails,
    awsAppSyncGraphQlApiLambdaAuthorizerConfigDetails_authorizerResultTtlInSeconds,
    awsAppSyncGraphQlApiLambdaAuthorizerConfigDetails_authorizerUri,
    awsAppSyncGraphQlApiLambdaAuthorizerConfigDetails_identityValidationExpression,

    -- * AwsAppSyncGraphQlApiLogConfigDetails
    AwsAppSyncGraphQlApiLogConfigDetails (..),
    newAwsAppSyncGraphQlApiLogConfigDetails,
    awsAppSyncGraphQlApiLogConfigDetails_cloudWatchLogsRoleArn,
    awsAppSyncGraphQlApiLogConfigDetails_excludeVerboseContent,
    awsAppSyncGraphQlApiLogConfigDetails_fieldLogLevel,

    -- * AwsAppSyncGraphQlApiOpenIdConnectConfigDetails
    AwsAppSyncGraphQlApiOpenIdConnectConfigDetails (..),
    newAwsAppSyncGraphQlApiOpenIdConnectConfigDetails,
    awsAppSyncGraphQlApiOpenIdConnectConfigDetails_authTtL,
    awsAppSyncGraphQlApiOpenIdConnectConfigDetails_clientId,
    awsAppSyncGraphQlApiOpenIdConnectConfigDetails_iatTtL,
    awsAppSyncGraphQlApiOpenIdConnectConfigDetails_issuer,

    -- * AwsAppSyncGraphQlApiUserPoolConfigDetails
    AwsAppSyncGraphQlApiUserPoolConfigDetails (..),
    newAwsAppSyncGraphQlApiUserPoolConfigDetails,
    awsAppSyncGraphQlApiUserPoolConfigDetails_appIdClientRegex,
    awsAppSyncGraphQlApiUserPoolConfigDetails_awsRegion,
    awsAppSyncGraphQlApiUserPoolConfigDetails_defaultAction,
    awsAppSyncGraphQlApiUserPoolConfigDetails_userPoolId,

    -- * AwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails
    AwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails (..),
    newAwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails,
    awsAutoScalingAutoScalingGroupAvailabilityZonesListDetails_value,

    -- * AwsAutoScalingAutoScalingGroupDetails
    AwsAutoScalingAutoScalingGroupDetails (..),
    newAwsAutoScalingAutoScalingGroupDetails,
    awsAutoScalingAutoScalingGroupDetails_availabilityZones,
    awsAutoScalingAutoScalingGroupDetails_capacityRebalance,
    awsAutoScalingAutoScalingGroupDetails_createdTime,
    awsAutoScalingAutoScalingGroupDetails_healthCheckGracePeriod,
    awsAutoScalingAutoScalingGroupDetails_healthCheckType,
    awsAutoScalingAutoScalingGroupDetails_launchConfigurationName,
    awsAutoScalingAutoScalingGroupDetails_launchTemplate,
    awsAutoScalingAutoScalingGroupDetails_loadBalancerNames,
    awsAutoScalingAutoScalingGroupDetails_mixedInstancesPolicy,

    -- * AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification
    AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification (..),
    newAwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification,
    awsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification_launchTemplateId,
    awsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification_launchTemplateName,
    awsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification_version,

    -- * AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails (..),
    newAwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails_instancesDistribution,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails_launchTemplate,

    -- * AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails (..),
    newAwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_onDemandAllocationStrategy,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_onDemandBaseCapacity,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_onDemandPercentageAboveBaseCapacity,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_spotAllocationStrategy,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_spotInstancePools,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_spotMaxPrice,

    -- * AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails (..),
    newAwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails_launchTemplateSpecification,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails_overrides,

    -- * AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification (..),
    newAwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification_launchTemplateId,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification_launchTemplateName,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification_version,

    -- * AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails (..),
    newAwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails_instanceType,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails_weightedCapacity,

    -- * AwsAutoScalingLaunchConfigurationBlockDeviceMappingsDetails
    AwsAutoScalingLaunchConfigurationBlockDeviceMappingsDetails (..),
    newAwsAutoScalingLaunchConfigurationBlockDeviceMappingsDetails,
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsDetails_deviceName,
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsDetails_ebs,
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsDetails_noDevice,
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsDetails_virtualName,

    -- * AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails
    AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails (..),
    newAwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails,
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_deleteOnTermination,
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_encrypted,
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_iops,
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_snapshotId,
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_volumeSize,
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_volumeType,

    -- * AwsAutoScalingLaunchConfigurationDetails
    AwsAutoScalingLaunchConfigurationDetails (..),
    newAwsAutoScalingLaunchConfigurationDetails,
    awsAutoScalingLaunchConfigurationDetails_associatePublicIpAddress,
    awsAutoScalingLaunchConfigurationDetails_blockDeviceMappings,
    awsAutoScalingLaunchConfigurationDetails_classicLinkVpcId,
    awsAutoScalingLaunchConfigurationDetails_classicLinkVpcSecurityGroups,
    awsAutoScalingLaunchConfigurationDetails_createdTime,
    awsAutoScalingLaunchConfigurationDetails_ebsOptimized,
    awsAutoScalingLaunchConfigurationDetails_iamInstanceProfile,
    awsAutoScalingLaunchConfigurationDetails_imageId,
    awsAutoScalingLaunchConfigurationDetails_instanceMonitoring,
    awsAutoScalingLaunchConfigurationDetails_instanceType,
    awsAutoScalingLaunchConfigurationDetails_kernelId,
    awsAutoScalingLaunchConfigurationDetails_keyName,
    awsAutoScalingLaunchConfigurationDetails_launchConfigurationName,
    awsAutoScalingLaunchConfigurationDetails_metadataOptions,
    awsAutoScalingLaunchConfigurationDetails_placementTenancy,
    awsAutoScalingLaunchConfigurationDetails_ramdiskId,
    awsAutoScalingLaunchConfigurationDetails_securityGroups,
    awsAutoScalingLaunchConfigurationDetails_spotPrice,
    awsAutoScalingLaunchConfigurationDetails_userData,

    -- * AwsAutoScalingLaunchConfigurationInstanceMonitoringDetails
    AwsAutoScalingLaunchConfigurationInstanceMonitoringDetails (..),
    newAwsAutoScalingLaunchConfigurationInstanceMonitoringDetails,
    awsAutoScalingLaunchConfigurationInstanceMonitoringDetails_enabled,

    -- * AwsAutoScalingLaunchConfigurationMetadataOptions
    AwsAutoScalingLaunchConfigurationMetadataOptions (..),
    newAwsAutoScalingLaunchConfigurationMetadataOptions,
    awsAutoScalingLaunchConfigurationMetadataOptions_httpEndpoint,
    awsAutoScalingLaunchConfigurationMetadataOptions_httpPutResponseHopLimit,
    awsAutoScalingLaunchConfigurationMetadataOptions_httpTokens,

    -- * AwsBackupBackupPlanAdvancedBackupSettingsDetails
    AwsBackupBackupPlanAdvancedBackupSettingsDetails (..),
    newAwsBackupBackupPlanAdvancedBackupSettingsDetails,
    awsBackupBackupPlanAdvancedBackupSettingsDetails_backupOptions,
    awsBackupBackupPlanAdvancedBackupSettingsDetails_resourceType,

    -- * AwsBackupBackupPlanBackupPlanDetails
    AwsBackupBackupPlanBackupPlanDetails (..),
    newAwsBackupBackupPlanBackupPlanDetails,
    awsBackupBackupPlanBackupPlanDetails_advancedBackupSettings,
    awsBackupBackupPlanBackupPlanDetails_backupPlanName,
    awsBackupBackupPlanBackupPlanDetails_backupPlanRule,

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
    awsBackupBackupPlanRuleCopyActionsDetails_destinationBackupVaultArn,
    awsBackupBackupPlanRuleCopyActionsDetails_lifecycle,

    -- * AwsBackupBackupPlanRuleDetails
    AwsBackupBackupPlanRuleDetails (..),
    newAwsBackupBackupPlanRuleDetails,
    awsBackupBackupPlanRuleDetails_completionWindowMinutes,
    awsBackupBackupPlanRuleDetails_copyActions,
    awsBackupBackupPlanRuleDetails_enableContinuousBackup,
    awsBackupBackupPlanRuleDetails_lifecycle,
    awsBackupBackupPlanRuleDetails_ruleId,
    awsBackupBackupPlanRuleDetails_ruleName,
    awsBackupBackupPlanRuleDetails_scheduleExpression,
    awsBackupBackupPlanRuleDetails_startWindowMinutes,
    awsBackupBackupPlanRuleDetails_targetBackupVault,

    -- * AwsBackupBackupVaultDetails
    AwsBackupBackupVaultDetails (..),
    newAwsBackupBackupVaultDetails,
    awsBackupBackupVaultDetails_accessPolicy,
    awsBackupBackupVaultDetails_backupVaultArn,
    awsBackupBackupVaultDetails_backupVaultName,
    awsBackupBackupVaultDetails_encryptionKeyArn,
    awsBackupBackupVaultDetails_notifications,

    -- * AwsBackupBackupVaultNotificationsDetails
    AwsBackupBackupVaultNotificationsDetails (..),
    newAwsBackupBackupVaultNotificationsDetails,
    awsBackupBackupVaultNotificationsDetails_backupVaultEvents,
    awsBackupBackupVaultNotificationsDetails_snsTopicArn,

    -- * AwsBackupRecoveryPointCalculatedLifecycleDetails
    AwsBackupRecoveryPointCalculatedLifecycleDetails (..),
    newAwsBackupRecoveryPointCalculatedLifecycleDetails,
    awsBackupRecoveryPointCalculatedLifecycleDetails_deleteAt,
    awsBackupRecoveryPointCalculatedLifecycleDetails_moveToColdStorageAt,

    -- * AwsBackupRecoveryPointCreatedByDetails
    AwsBackupRecoveryPointCreatedByDetails (..),
    newAwsBackupRecoveryPointCreatedByDetails,
    awsBackupRecoveryPointCreatedByDetails_backupPlanArn,
    awsBackupRecoveryPointCreatedByDetails_backupPlanId,
    awsBackupRecoveryPointCreatedByDetails_backupPlanVersion,
    awsBackupRecoveryPointCreatedByDetails_backupRuleId,

    -- * AwsBackupRecoveryPointDetails
    AwsBackupRecoveryPointDetails (..),
    newAwsBackupRecoveryPointDetails,
    awsBackupRecoveryPointDetails_backupSizeInBytes,
    awsBackupRecoveryPointDetails_backupVaultArn,
    awsBackupRecoveryPointDetails_backupVaultName,
    awsBackupRecoveryPointDetails_calculatedLifecycle,
    awsBackupRecoveryPointDetails_completionDate,
    awsBackupRecoveryPointDetails_createdBy,
    awsBackupRecoveryPointDetails_creationDate,
    awsBackupRecoveryPointDetails_encryptionKeyArn,
    awsBackupRecoveryPointDetails_iamRoleArn,
    awsBackupRecoveryPointDetails_isEncrypted,
    awsBackupRecoveryPointDetails_lastRestoreTime,
    awsBackupRecoveryPointDetails_lifecycle,
    awsBackupRecoveryPointDetails_recoveryPointArn,
    awsBackupRecoveryPointDetails_resourceArn,
    awsBackupRecoveryPointDetails_resourceType,
    awsBackupRecoveryPointDetails_sourceBackupVaultArn,
    awsBackupRecoveryPointDetails_status,
    awsBackupRecoveryPointDetails_statusMessage,
    awsBackupRecoveryPointDetails_storageClass,

    -- * AwsBackupRecoveryPointLifecycleDetails
    AwsBackupRecoveryPointLifecycleDetails (..),
    newAwsBackupRecoveryPointLifecycleDetails,
    awsBackupRecoveryPointLifecycleDetails_deleteAfterDays,
    awsBackupRecoveryPointLifecycleDetails_moveToColdStorageAfterDays,

    -- * AwsCertificateManagerCertificateDetails
    AwsCertificateManagerCertificateDetails (..),
    newAwsCertificateManagerCertificateDetails,
    awsCertificateManagerCertificateDetails_certificateAuthorityArn,
    awsCertificateManagerCertificateDetails_createdAt,
    awsCertificateManagerCertificateDetails_domainName,
    awsCertificateManagerCertificateDetails_domainValidationOptions,
    awsCertificateManagerCertificateDetails_extendedKeyUsages,
    awsCertificateManagerCertificateDetails_failureReason,
    awsCertificateManagerCertificateDetails_importedAt,
    awsCertificateManagerCertificateDetails_inUseBy,
    awsCertificateManagerCertificateDetails_issuedAt,
    awsCertificateManagerCertificateDetails_issuer,
    awsCertificateManagerCertificateDetails_keyAlgorithm,
    awsCertificateManagerCertificateDetails_keyUsages,
    awsCertificateManagerCertificateDetails_notAfter,
    awsCertificateManagerCertificateDetails_notBefore,
    awsCertificateManagerCertificateDetails_options,
    awsCertificateManagerCertificateDetails_renewalEligibility,
    awsCertificateManagerCertificateDetails_renewalSummary,
    awsCertificateManagerCertificateDetails_serial,
    awsCertificateManagerCertificateDetails_signatureAlgorithm,
    awsCertificateManagerCertificateDetails_status,
    awsCertificateManagerCertificateDetails_subject,
    awsCertificateManagerCertificateDetails_subjectAlternativeNames,
    awsCertificateManagerCertificateDetails_type,

    -- * AwsCertificateManagerCertificateDomainValidationOption
    AwsCertificateManagerCertificateDomainValidationOption (..),
    newAwsCertificateManagerCertificateDomainValidationOption,
    awsCertificateManagerCertificateDomainValidationOption_domainName,
    awsCertificateManagerCertificateDomainValidationOption_resourceRecord,
    awsCertificateManagerCertificateDomainValidationOption_validationDomain,
    awsCertificateManagerCertificateDomainValidationOption_validationEmails,
    awsCertificateManagerCertificateDomainValidationOption_validationMethod,
    awsCertificateManagerCertificateDomainValidationOption_validationStatus,

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
    awsCertificateManagerCertificateRenewalSummary_renewalStatus,
    awsCertificateManagerCertificateRenewalSummary_renewalStatusReason,
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
    awsCloudFormationStackDetails_capabilities,
    awsCloudFormationStackDetails_creationTime,
    awsCloudFormationStackDetails_description,
    awsCloudFormationStackDetails_disableRollback,
    awsCloudFormationStackDetails_driftInformation,
    awsCloudFormationStackDetails_enableTerminationProtection,
    awsCloudFormationStackDetails_lastUpdatedTime,
    awsCloudFormationStackDetails_notificationArns,
    awsCloudFormationStackDetails_outputs,
    awsCloudFormationStackDetails_roleArn,
    awsCloudFormationStackDetails_stackId,
    awsCloudFormationStackDetails_stackName,
    awsCloudFormationStackDetails_stackStatus,
    awsCloudFormationStackDetails_stackStatusReason,
    awsCloudFormationStackDetails_timeoutInMinutes,

    -- * AwsCloudFormationStackDriftInformationDetails
    AwsCloudFormationStackDriftInformationDetails (..),
    newAwsCloudFormationStackDriftInformationDetails,
    awsCloudFormationStackDriftInformationDetails_stackDriftStatus,

    -- * AwsCloudFormationStackOutputsDetails
    AwsCloudFormationStackOutputsDetails (..),
    newAwsCloudFormationStackOutputsDetails,
    awsCloudFormationStackOutputsDetails_description,
    awsCloudFormationStackOutputsDetails_outputKey,
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
    awsCloudFrontDistributionDetails_cacheBehaviors,
    awsCloudFrontDistributionDetails_defaultCacheBehavior,
    awsCloudFrontDistributionDetails_defaultRootObject,
    awsCloudFrontDistributionDetails_domainName,
    awsCloudFrontDistributionDetails_eTag,
    awsCloudFrontDistributionDetails_lastModifiedTime,
    awsCloudFrontDistributionDetails_logging,
    awsCloudFrontDistributionDetails_originGroups,
    awsCloudFrontDistributionDetails_origins,
    awsCloudFrontDistributionDetails_status,
    awsCloudFrontDistributionDetails_viewerCertificate,
    awsCloudFrontDistributionDetails_webAclId,

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
    awsCloudFrontDistributionOriginCustomOriginConfig_httpPort,
    awsCloudFrontDistributionOriginCustomOriginConfig_httpsPort,
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
    awsCloudFrontDistributionOriginItem_customOriginConfig,
    awsCloudFrontDistributionOriginItem_domainName,
    awsCloudFrontDistributionOriginItem_id,
    awsCloudFrontDistributionOriginItem_originPath,
    awsCloudFrontDistributionOriginItem_s3OriginConfig,

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
    awsCloudFrontDistributionViewerCertificate_acmCertificateArn,
    awsCloudFrontDistributionViewerCertificate_certificate,
    awsCloudFrontDistributionViewerCertificate_certificateSource,
    awsCloudFrontDistributionViewerCertificate_cloudFrontDefaultCertificate,
    awsCloudFrontDistributionViewerCertificate_iamCertificateId,
    awsCloudFrontDistributionViewerCertificate_minimumProtocolVersion,
    awsCloudFrontDistributionViewerCertificate_sslSupportMethod,

    -- * AwsCloudTrailTrailDetails
    AwsCloudTrailTrailDetails (..),
    newAwsCloudTrailTrailDetails,
    awsCloudTrailTrailDetails_cloudWatchLogsLogGroupArn,
    awsCloudTrailTrailDetails_cloudWatchLogsRoleArn,
    awsCloudTrailTrailDetails_hasCustomEventSelectors,
    awsCloudTrailTrailDetails_homeRegion,
    awsCloudTrailTrailDetails_includeGlobalServiceEvents,
    awsCloudTrailTrailDetails_isMultiRegionTrail,
    awsCloudTrailTrailDetails_isOrganizationTrail,
    awsCloudTrailTrailDetails_kmsKeyId,
    awsCloudTrailTrailDetails_logFileValidationEnabled,
    awsCloudTrailTrailDetails_name,
    awsCloudTrailTrailDetails_s3BucketName,
    awsCloudTrailTrailDetails_s3KeyPrefix,
    awsCloudTrailTrailDetails_snsTopicArn,
    awsCloudTrailTrailDetails_snsTopicName,
    awsCloudTrailTrailDetails_trailArn,

    -- * AwsCloudWatchAlarmDetails
    AwsCloudWatchAlarmDetails (..),
    newAwsCloudWatchAlarmDetails,
    awsCloudWatchAlarmDetails_actionsEnabled,
    awsCloudWatchAlarmDetails_alarmActions,
    awsCloudWatchAlarmDetails_alarmArn,
    awsCloudWatchAlarmDetails_alarmConfigurationUpdatedTimestamp,
    awsCloudWatchAlarmDetails_alarmDescription,
    awsCloudWatchAlarmDetails_alarmName,
    awsCloudWatchAlarmDetails_comparisonOperator,
    awsCloudWatchAlarmDetails_datapointsToAlarm,
    awsCloudWatchAlarmDetails_dimensions,
    awsCloudWatchAlarmDetails_evaluateLowSampleCountPercentile,
    awsCloudWatchAlarmDetails_evaluationPeriods,
    awsCloudWatchAlarmDetails_extendedStatistic,
    awsCloudWatchAlarmDetails_insufficientDataActions,
    awsCloudWatchAlarmDetails_metricName,
    awsCloudWatchAlarmDetails_namespace,
    awsCloudWatchAlarmDetails_okActions,
    awsCloudWatchAlarmDetails_period,
    awsCloudWatchAlarmDetails_statistic,
    awsCloudWatchAlarmDetails_threshold,
    awsCloudWatchAlarmDetails_thresholdMetricId,
    awsCloudWatchAlarmDetails_treatMissingData,
    awsCloudWatchAlarmDetails_unit,

    -- * AwsCloudWatchAlarmDimensionsDetails
    AwsCloudWatchAlarmDimensionsDetails (..),
    newAwsCloudWatchAlarmDimensionsDetails,
    awsCloudWatchAlarmDimensionsDetails_name,
    awsCloudWatchAlarmDimensionsDetails_value,

    -- * AwsCodeBuildProjectArtifactsDetails
    AwsCodeBuildProjectArtifactsDetails (..),
    newAwsCodeBuildProjectArtifactsDetails,
    awsCodeBuildProjectArtifactsDetails_artifactIdentifier,
    awsCodeBuildProjectArtifactsDetails_encryptionDisabled,
    awsCodeBuildProjectArtifactsDetails_location,
    awsCodeBuildProjectArtifactsDetails_name,
    awsCodeBuildProjectArtifactsDetails_namespaceType,
    awsCodeBuildProjectArtifactsDetails_overrideArtifactName,
    awsCodeBuildProjectArtifactsDetails_packaging,
    awsCodeBuildProjectArtifactsDetails_path,
    awsCodeBuildProjectArtifactsDetails_type,

    -- * AwsCodeBuildProjectDetails
    AwsCodeBuildProjectDetails (..),
    newAwsCodeBuildProjectDetails,
    awsCodeBuildProjectDetails_artifacts,
    awsCodeBuildProjectDetails_encryptionKey,
    awsCodeBuildProjectDetails_environment,
    awsCodeBuildProjectDetails_logsConfig,
    awsCodeBuildProjectDetails_name,
    awsCodeBuildProjectDetails_secondaryArtifacts,
    awsCodeBuildProjectDetails_serviceRole,
    awsCodeBuildProjectDetails_source,
    awsCodeBuildProjectDetails_vpcConfig,

    -- * AwsCodeBuildProjectEnvironment
    AwsCodeBuildProjectEnvironment (..),
    newAwsCodeBuildProjectEnvironment,
    awsCodeBuildProjectEnvironment_certificate,
    awsCodeBuildProjectEnvironment_environmentVariables,
    awsCodeBuildProjectEnvironment_imagePullCredentialsType,
    awsCodeBuildProjectEnvironment_privilegedMode,
    awsCodeBuildProjectEnvironment_registryCredential,
    awsCodeBuildProjectEnvironment_type,

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
    awsCodeBuildProjectLogsConfigCloudWatchLogsDetails_groupName,
    awsCodeBuildProjectLogsConfigCloudWatchLogsDetails_status,
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
    awsCodeBuildProjectLogsConfigS3LogsDetails_location,
    awsCodeBuildProjectLogsConfigS3LogsDetails_status,

    -- * AwsCodeBuildProjectSource
    AwsCodeBuildProjectSource (..),
    newAwsCodeBuildProjectSource,
    awsCodeBuildProjectSource_gitCloneDepth,
    awsCodeBuildProjectSource_insecureSsl,
    awsCodeBuildProjectSource_location,
    awsCodeBuildProjectSource_type,

    -- * AwsCodeBuildProjectVpcConfig
    AwsCodeBuildProjectVpcConfig (..),
    newAwsCodeBuildProjectVpcConfig,
    awsCodeBuildProjectVpcConfig_securityGroupIds,
    awsCodeBuildProjectVpcConfig_subnets,
    awsCodeBuildProjectVpcConfig_vpcId,

    -- * AwsCorsConfiguration
    AwsCorsConfiguration (..),
    newAwsCorsConfiguration,
    awsCorsConfiguration_allowCredentials,
    awsCorsConfiguration_allowHeaders,
    awsCorsConfiguration_allowMethods,
    awsCorsConfiguration_allowOrigins,
    awsCorsConfiguration_exposeHeaders,
    awsCorsConfiguration_maxAge,

    -- * AwsDynamoDbTableAttributeDefinition
    AwsDynamoDbTableAttributeDefinition (..),
    newAwsDynamoDbTableAttributeDefinition,
    awsDynamoDbTableAttributeDefinition_attributeName,
    awsDynamoDbTableAttributeDefinition_attributeType,

    -- * AwsDynamoDbTableBillingModeSummary
    AwsDynamoDbTableBillingModeSummary (..),
    newAwsDynamoDbTableBillingModeSummary,
    awsDynamoDbTableBillingModeSummary_billingMode,
    awsDynamoDbTableBillingModeSummary_lastUpdateToPayPerRequestDateTime,

    -- * AwsDynamoDbTableDetails
    AwsDynamoDbTableDetails (..),
    newAwsDynamoDbTableDetails,
    awsDynamoDbTableDetails_attributeDefinitions,
    awsDynamoDbTableDetails_billingModeSummary,
    awsDynamoDbTableDetails_creationDateTime,
    awsDynamoDbTableDetails_globalSecondaryIndexes,
    awsDynamoDbTableDetails_globalTableVersion,
    awsDynamoDbTableDetails_itemCount,
    awsDynamoDbTableDetails_keySchema,
    awsDynamoDbTableDetails_latestStreamArn,
    awsDynamoDbTableDetails_latestStreamLabel,
    awsDynamoDbTableDetails_localSecondaryIndexes,
    awsDynamoDbTableDetails_provisionedThroughput,
    awsDynamoDbTableDetails_replicas,
    awsDynamoDbTableDetails_restoreSummary,
    awsDynamoDbTableDetails_sseDescription,
    awsDynamoDbTableDetails_streamSpecification,
    awsDynamoDbTableDetails_tableId,
    awsDynamoDbTableDetails_tableName,
    awsDynamoDbTableDetails_tableSizeBytes,
    awsDynamoDbTableDetails_tableStatus,

    -- * AwsDynamoDbTableGlobalSecondaryIndex
    AwsDynamoDbTableGlobalSecondaryIndex (..),
    newAwsDynamoDbTableGlobalSecondaryIndex,
    awsDynamoDbTableGlobalSecondaryIndex_backfilling,
    awsDynamoDbTableGlobalSecondaryIndex_indexArn,
    awsDynamoDbTableGlobalSecondaryIndex_indexName,
    awsDynamoDbTableGlobalSecondaryIndex_indexSizeBytes,
    awsDynamoDbTableGlobalSecondaryIndex_indexStatus,
    awsDynamoDbTableGlobalSecondaryIndex_itemCount,
    awsDynamoDbTableGlobalSecondaryIndex_keySchema,
    awsDynamoDbTableGlobalSecondaryIndex_projection,
    awsDynamoDbTableGlobalSecondaryIndex_provisionedThroughput,

    -- * AwsDynamoDbTableKeySchema
    AwsDynamoDbTableKeySchema (..),
    newAwsDynamoDbTableKeySchema,
    awsDynamoDbTableKeySchema_attributeName,
    awsDynamoDbTableKeySchema_keyType,

    -- * AwsDynamoDbTableLocalSecondaryIndex
    AwsDynamoDbTableLocalSecondaryIndex (..),
    newAwsDynamoDbTableLocalSecondaryIndex,
    awsDynamoDbTableLocalSecondaryIndex_indexArn,
    awsDynamoDbTableLocalSecondaryIndex_indexName,
    awsDynamoDbTableLocalSecondaryIndex_keySchema,
    awsDynamoDbTableLocalSecondaryIndex_projection,

    -- * AwsDynamoDbTableProjection
    AwsDynamoDbTableProjection (..),
    newAwsDynamoDbTableProjection,
    awsDynamoDbTableProjection_nonKeyAttributes,
    awsDynamoDbTableProjection_projectionType,

    -- * AwsDynamoDbTableProvisionedThroughput
    AwsDynamoDbTableProvisionedThroughput (..),
    newAwsDynamoDbTableProvisionedThroughput,
    awsDynamoDbTableProvisionedThroughput_lastDecreaseDateTime,
    awsDynamoDbTableProvisionedThroughput_lastIncreaseDateTime,
    awsDynamoDbTableProvisionedThroughput_numberOfDecreasesToday,
    awsDynamoDbTableProvisionedThroughput_readCapacityUnits,
    awsDynamoDbTableProvisionedThroughput_writeCapacityUnits,

    -- * AwsDynamoDbTableProvisionedThroughputOverride
    AwsDynamoDbTableProvisionedThroughputOverride (..),
    newAwsDynamoDbTableProvisionedThroughputOverride,
    awsDynamoDbTableProvisionedThroughputOverride_readCapacityUnits,

    -- * AwsDynamoDbTableReplica
    AwsDynamoDbTableReplica (..),
    newAwsDynamoDbTableReplica,
    awsDynamoDbTableReplica_globalSecondaryIndexes,
    awsDynamoDbTableReplica_kmsMasterKeyId,
    awsDynamoDbTableReplica_provisionedThroughputOverride,
    awsDynamoDbTableReplica_regionName,
    awsDynamoDbTableReplica_replicaStatus,
    awsDynamoDbTableReplica_replicaStatusDescription,

    -- * AwsDynamoDbTableReplicaGlobalSecondaryIndex
    AwsDynamoDbTableReplicaGlobalSecondaryIndex (..),
    newAwsDynamoDbTableReplicaGlobalSecondaryIndex,
    awsDynamoDbTableReplicaGlobalSecondaryIndex_indexName,
    awsDynamoDbTableReplicaGlobalSecondaryIndex_provisionedThroughputOverride,

    -- * AwsDynamoDbTableRestoreSummary
    AwsDynamoDbTableRestoreSummary (..),
    newAwsDynamoDbTableRestoreSummary,
    awsDynamoDbTableRestoreSummary_restoreDateTime,
    awsDynamoDbTableRestoreSummary_restoreInProgress,
    awsDynamoDbTableRestoreSummary_sourceBackupArn,
    awsDynamoDbTableRestoreSummary_sourceTableArn,

    -- * AwsDynamoDbTableSseDescription
    AwsDynamoDbTableSseDescription (..),
    newAwsDynamoDbTableSseDescription,
    awsDynamoDbTableSseDescription_inaccessibleEncryptionDateTime,
    awsDynamoDbTableSseDescription_kmsMasterKeyArn,
    awsDynamoDbTableSseDescription_sseType,
    awsDynamoDbTableSseDescription_status,

    -- * AwsDynamoDbTableStreamSpecification
    AwsDynamoDbTableStreamSpecification (..),
    newAwsDynamoDbTableStreamSpecification,
    awsDynamoDbTableStreamSpecification_streamEnabled,
    awsDynamoDbTableStreamSpecification_streamViewType,

    -- * AwsEc2EipDetails
    AwsEc2EipDetails (..),
    newAwsEc2EipDetails,
    awsEc2EipDetails_allocationId,
    awsEc2EipDetails_associationId,
    awsEc2EipDetails_domain,
    awsEc2EipDetails_instanceId,
    awsEc2EipDetails_networkBorderGroup,
    awsEc2EipDetails_networkInterfaceId,
    awsEc2EipDetails_networkInterfaceOwnerId,
    awsEc2EipDetails_privateIpAddress,
    awsEc2EipDetails_publicIp,
    awsEc2EipDetails_publicIpv4Pool,

    -- * AwsEc2InstanceDetails
    AwsEc2InstanceDetails (..),
    newAwsEc2InstanceDetails,
    awsEc2InstanceDetails_iamInstanceProfileArn,
    awsEc2InstanceDetails_imageId,
    awsEc2InstanceDetails_ipV4Addresses,
    awsEc2InstanceDetails_ipV6Addresses,
    awsEc2InstanceDetails_keyName,
    awsEc2InstanceDetails_launchedAt,
    awsEc2InstanceDetails_metadataOptions,
    awsEc2InstanceDetails_monitoring,
    awsEc2InstanceDetails_networkInterfaces,
    awsEc2InstanceDetails_subnetId,
    awsEc2InstanceDetails_type,
    awsEc2InstanceDetails_virtualizationType,
    awsEc2InstanceDetails_vpcId,

    -- * AwsEc2InstanceMetadataOptions
    AwsEc2InstanceMetadataOptions (..),
    newAwsEc2InstanceMetadataOptions,
    awsEc2InstanceMetadataOptions_httpEndpoint,
    awsEc2InstanceMetadataOptions_httpProtocolIpv6,
    awsEc2InstanceMetadataOptions_httpPutResponseHopLimit,
    awsEc2InstanceMetadataOptions_httpTokens,
    awsEc2InstanceMetadataOptions_instanceMetadataTags,

    -- * AwsEc2InstanceMonitoringDetails
    AwsEc2InstanceMonitoringDetails (..),
    newAwsEc2InstanceMonitoringDetails,
    awsEc2InstanceMonitoringDetails_state,

    -- * AwsEc2InstanceNetworkInterfacesDetails
    AwsEc2InstanceNetworkInterfacesDetails (..),
    newAwsEc2InstanceNetworkInterfacesDetails,
    awsEc2InstanceNetworkInterfacesDetails_networkInterfaceId,

    -- * AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails
    AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails (..),
    newAwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails,
    awsEc2LaunchTemplateDataBlockDeviceMappingSetDetails_deviceName,
    awsEc2LaunchTemplateDataBlockDeviceMappingSetDetails_ebs,
    awsEc2LaunchTemplateDataBlockDeviceMappingSetDetails_noDevice,
    awsEc2LaunchTemplateDataBlockDeviceMappingSetDetails_virtualName,

    -- * AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails
    AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails (..),
    newAwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails,
    awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_deleteOnTermination,
    awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_encrypted,
    awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_iops,
    awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_kmsKeyId,
    awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_snapshotId,
    awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_throughput,
    awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_volumeSize,
    awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_volumeType,

    -- * AwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails
    AwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails (..),
    newAwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails,
    awsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails_capacityReservationId,
    awsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails_capacityReservationResourceGroupArn,

    -- * AwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails
    AwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails (..),
    newAwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails,
    awsEc2LaunchTemplateDataCapacityReservationSpecificationDetails_capacityReservationPreference,
    awsEc2LaunchTemplateDataCapacityReservationSpecificationDetails_capacityReservationTarget,

    -- * AwsEc2LaunchTemplateDataCpuOptionsDetails
    AwsEc2LaunchTemplateDataCpuOptionsDetails (..),
    newAwsEc2LaunchTemplateDataCpuOptionsDetails,
    awsEc2LaunchTemplateDataCpuOptionsDetails_coreCount,
    awsEc2LaunchTemplateDataCpuOptionsDetails_threadsPerCore,

    -- * AwsEc2LaunchTemplateDataCreditSpecificationDetails
    AwsEc2LaunchTemplateDataCreditSpecificationDetails (..),
    newAwsEc2LaunchTemplateDataCreditSpecificationDetails,
    awsEc2LaunchTemplateDataCreditSpecificationDetails_cpuCredits,

    -- * AwsEc2LaunchTemplateDataDetails
    AwsEc2LaunchTemplateDataDetails (..),
    newAwsEc2LaunchTemplateDataDetails,
    awsEc2LaunchTemplateDataDetails_blockDeviceMappingSet,
    awsEc2LaunchTemplateDataDetails_capacityReservationSpecification,
    awsEc2LaunchTemplateDataDetails_cpuOptions,
    awsEc2LaunchTemplateDataDetails_creditSpecification,
    awsEc2LaunchTemplateDataDetails_disableApiStop,
    awsEc2LaunchTemplateDataDetails_disableApiTermination,
    awsEc2LaunchTemplateDataDetails_ebsOptimized,
    awsEc2LaunchTemplateDataDetails_elasticGpuSpecificationSet,
    awsEc2LaunchTemplateDataDetails_elasticInferenceAcceleratorSet,
    awsEc2LaunchTemplateDataDetails_enclaveOptions,
    awsEc2LaunchTemplateDataDetails_hibernationOptions,
    awsEc2LaunchTemplateDataDetails_iamInstanceProfile,
    awsEc2LaunchTemplateDataDetails_imageId,
    awsEc2LaunchTemplateDataDetails_instanceInitiatedShutdownBehavior,
    awsEc2LaunchTemplateDataDetails_instanceMarketOptions,
    awsEc2LaunchTemplateDataDetails_instanceRequirements,
    awsEc2LaunchTemplateDataDetails_instanceType,
    awsEc2LaunchTemplateDataDetails_kernelId,
    awsEc2LaunchTemplateDataDetails_keyName,
    awsEc2LaunchTemplateDataDetails_licenseSet,
    awsEc2LaunchTemplateDataDetails_maintenanceOptions,
    awsEc2LaunchTemplateDataDetails_metadataOptions,
    awsEc2LaunchTemplateDataDetails_monitoring,
    awsEc2LaunchTemplateDataDetails_networkInterfaceSet,
    awsEc2LaunchTemplateDataDetails_placement,
    awsEc2LaunchTemplateDataDetails_privateDnsNameOptions,
    awsEc2LaunchTemplateDataDetails_ramDiskId,
    awsEc2LaunchTemplateDataDetails_securityGroupIdSet,
    awsEc2LaunchTemplateDataDetails_securityGroupSet,
    awsEc2LaunchTemplateDataDetails_userData,

    -- * AwsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails
    AwsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails (..),
    newAwsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails,
    awsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails_type,

    -- * AwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails
    AwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails (..),
    newAwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails,
    awsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails_count,
    awsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails_type,

    -- * AwsEc2LaunchTemplateDataEnclaveOptionsDetails
    AwsEc2LaunchTemplateDataEnclaveOptionsDetails (..),
    newAwsEc2LaunchTemplateDataEnclaveOptionsDetails,
    awsEc2LaunchTemplateDataEnclaveOptionsDetails_enabled,

    -- * AwsEc2LaunchTemplateDataHibernationOptionsDetails
    AwsEc2LaunchTemplateDataHibernationOptionsDetails (..),
    newAwsEc2LaunchTemplateDataHibernationOptionsDetails,
    awsEc2LaunchTemplateDataHibernationOptionsDetails_configured,

    -- * AwsEc2LaunchTemplateDataIamInstanceProfileDetails
    AwsEc2LaunchTemplateDataIamInstanceProfileDetails (..),
    newAwsEc2LaunchTemplateDataIamInstanceProfileDetails,
    awsEc2LaunchTemplateDataIamInstanceProfileDetails_arn,
    awsEc2LaunchTemplateDataIamInstanceProfileDetails_name,

    -- * AwsEc2LaunchTemplateDataInstanceMarketOptionsDetails
    AwsEc2LaunchTemplateDataInstanceMarketOptionsDetails (..),
    newAwsEc2LaunchTemplateDataInstanceMarketOptionsDetails,
    awsEc2LaunchTemplateDataInstanceMarketOptionsDetails_marketType,
    awsEc2LaunchTemplateDataInstanceMarketOptionsDetails_spotOptions,

    -- * AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails
    AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails (..),
    newAwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails,
    awsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails_blockDurationMinutes,
    awsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails_instanceInterruptionBehavior,
    awsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails_maxPrice,
    awsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails_spotInstanceType,
    awsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails_validUntil,

    -- * AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails
    AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails (..),
    newAwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails,
    awsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails_max,
    awsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails_min,

    -- * AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails
    AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails (..),
    newAwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails,
    awsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails_max,
    awsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails_min,

    -- * AwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails
    AwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails (..),
    newAwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails,
    awsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails_max,
    awsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails_min,

    -- * AwsEc2LaunchTemplateDataInstanceRequirementsDetails
    AwsEc2LaunchTemplateDataInstanceRequirementsDetails (..),
    newAwsEc2LaunchTemplateDataInstanceRequirementsDetails,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_acceleratorCount,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_acceleratorManufacturers,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_acceleratorNames,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_acceleratorTotalMemoryMiB,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_acceleratorTypes,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_bareMetal,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_baselineEbsBandwidthMbps,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_burstablePerformance,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_cpuManufacturers,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_excludedInstanceTypes,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_instanceGenerations,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_localStorage,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_localStorageTypes,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_memoryGiBPerVCpu,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_memoryMiB,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_networkInterfaceCount,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_onDemandMaxPricePercentageOverLowestPrice,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_requireHibernateSupport,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_spotMaxPricePercentageOverLowestPrice,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_totalLocalStorageGB,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_vCpuCount,

    -- * AwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails
    AwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails (..),
    newAwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails,
    awsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails_max,
    awsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails_min,

    -- * AwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails
    AwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails (..),
    newAwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails,
    awsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails_max,
    awsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails_min,

    -- * AwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails
    AwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails (..),
    newAwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails,
    awsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails_max,
    awsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails_min,

    -- * AwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails
    AwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails (..),
    newAwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails,
    awsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails_max,
    awsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails_min,

    -- * AwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails
    AwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails (..),
    newAwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails,
    awsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails_max,
    awsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails_min,

    -- * AwsEc2LaunchTemplateDataLicenseSetDetails
    AwsEc2LaunchTemplateDataLicenseSetDetails (..),
    newAwsEc2LaunchTemplateDataLicenseSetDetails,
    awsEc2LaunchTemplateDataLicenseSetDetails_licenseConfigurationArn,

    -- * AwsEc2LaunchTemplateDataMaintenanceOptionsDetails
    AwsEc2LaunchTemplateDataMaintenanceOptionsDetails (..),
    newAwsEc2LaunchTemplateDataMaintenanceOptionsDetails,
    awsEc2LaunchTemplateDataMaintenanceOptionsDetails_autoRecovery,

    -- * AwsEc2LaunchTemplateDataMetadataOptionsDetails
    AwsEc2LaunchTemplateDataMetadataOptionsDetails (..),
    newAwsEc2LaunchTemplateDataMetadataOptionsDetails,
    awsEc2LaunchTemplateDataMetadataOptionsDetails_httpEndpoint,
    awsEc2LaunchTemplateDataMetadataOptionsDetails_httpProtocolIpv6,
    awsEc2LaunchTemplateDataMetadataOptionsDetails_httpPutResponseHopLimit,
    awsEc2LaunchTemplateDataMetadataOptionsDetails_httpTokens,
    awsEc2LaunchTemplateDataMetadataOptionsDetails_instanceMetadataTags,

    -- * AwsEc2LaunchTemplateDataMonitoringDetails
    AwsEc2LaunchTemplateDataMonitoringDetails (..),
    newAwsEc2LaunchTemplateDataMonitoringDetails,
    awsEc2LaunchTemplateDataMonitoringDetails_enabled,

    -- * AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails
    AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails (..),
    newAwsEc2LaunchTemplateDataNetworkInterfaceSetDetails,
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_associateCarrierIpAddress,
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_associatePublicIpAddress,
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_deleteOnTermination,
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_description,
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_deviceIndex,
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_groups,
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_interfaceType,
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_ipv4PrefixCount,
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_ipv4Prefixes,
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_ipv6AddressCount,
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_ipv6Addresses,
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_ipv6PrefixCount,
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_ipv6Prefixes,
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_networkCardIndex,
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_networkInterfaceId,
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_privateIpAddress,
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_privateIpAddresses,
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_secondaryPrivateIpAddressCount,
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_subnetId,

    -- * AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails
    AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails (..),
    newAwsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails,
    awsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails_ipv4Prefix,

    -- * AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails
    AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails (..),
    newAwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails,
    awsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails_ipv6Address,

    -- * AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails
    AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails (..),
    newAwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails,
    awsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails_ipv6Prefix,

    -- * AwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails
    AwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails (..),
    newAwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails,
    awsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails_primary,
    awsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails_privateIpAddress,

    -- * AwsEc2LaunchTemplateDataPlacementDetails
    AwsEc2LaunchTemplateDataPlacementDetails (..),
    newAwsEc2LaunchTemplateDataPlacementDetails,
    awsEc2LaunchTemplateDataPlacementDetails_affinity,
    awsEc2LaunchTemplateDataPlacementDetails_availabilityZone,
    awsEc2LaunchTemplateDataPlacementDetails_groupName,
    awsEc2LaunchTemplateDataPlacementDetails_hostId,
    awsEc2LaunchTemplateDataPlacementDetails_hostResourceGroupArn,
    awsEc2LaunchTemplateDataPlacementDetails_partitionNumber,
    awsEc2LaunchTemplateDataPlacementDetails_spreadDomain,
    awsEc2LaunchTemplateDataPlacementDetails_tenancy,

    -- * AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails
    AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails (..),
    newAwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails,
    awsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails_enableResourceNameDnsAAAARecord,
    awsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails_enableResourceNameDnsARecord,
    awsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails_hostnameType,

    -- * AwsEc2LaunchTemplateDetails
    AwsEc2LaunchTemplateDetails (..),
    newAwsEc2LaunchTemplateDetails,
    awsEc2LaunchTemplateDetails_defaultVersionNumber,
    awsEc2LaunchTemplateDetails_id,
    awsEc2LaunchTemplateDetails_latestVersionNumber,
    awsEc2LaunchTemplateDetails_launchTemplateData,
    awsEc2LaunchTemplateDetails_launchTemplateName,

    -- * AwsEc2NetworkAclAssociation
    AwsEc2NetworkAclAssociation (..),
    newAwsEc2NetworkAclAssociation,
    awsEc2NetworkAclAssociation_networkAclAssociationId,
    awsEc2NetworkAclAssociation_networkAclId,
    awsEc2NetworkAclAssociation_subnetId,

    -- * AwsEc2NetworkAclDetails
    AwsEc2NetworkAclDetails (..),
    newAwsEc2NetworkAclDetails,
    awsEc2NetworkAclDetails_associations,
    awsEc2NetworkAclDetails_entries,
    awsEc2NetworkAclDetails_isDefault,
    awsEc2NetworkAclDetails_networkAclId,
    awsEc2NetworkAclDetails_ownerId,
    awsEc2NetworkAclDetails_vpcId,

    -- * AwsEc2NetworkAclEntry
    AwsEc2NetworkAclEntry (..),
    newAwsEc2NetworkAclEntry,
    awsEc2NetworkAclEntry_cidrBlock,
    awsEc2NetworkAclEntry_egress,
    awsEc2NetworkAclEntry_icmpTypeCode,
    awsEc2NetworkAclEntry_ipv6CidrBlock,
    awsEc2NetworkAclEntry_portRange,
    awsEc2NetworkAclEntry_protocol,
    awsEc2NetworkAclEntry_ruleAction,
    awsEc2NetworkAclEntry_ruleNumber,

    -- * AwsEc2NetworkInterfaceAttachment
    AwsEc2NetworkInterfaceAttachment (..),
    newAwsEc2NetworkInterfaceAttachment,
    awsEc2NetworkInterfaceAttachment_attachTime,
    awsEc2NetworkInterfaceAttachment_attachmentId,
    awsEc2NetworkInterfaceAttachment_deleteOnTermination,
    awsEc2NetworkInterfaceAttachment_deviceIndex,
    awsEc2NetworkInterfaceAttachment_instanceId,
    awsEc2NetworkInterfaceAttachment_instanceOwnerId,
    awsEc2NetworkInterfaceAttachment_status,

    -- * AwsEc2NetworkInterfaceDetails
    AwsEc2NetworkInterfaceDetails (..),
    newAwsEc2NetworkInterfaceDetails,
    awsEc2NetworkInterfaceDetails_attachment,
    awsEc2NetworkInterfaceDetails_ipV6Addresses,
    awsEc2NetworkInterfaceDetails_networkInterfaceId,
    awsEc2NetworkInterfaceDetails_privateIpAddresses,
    awsEc2NetworkInterfaceDetails_publicDnsName,
    awsEc2NetworkInterfaceDetails_publicIp,
    awsEc2NetworkInterfaceDetails_securityGroups,
    awsEc2NetworkInterfaceDetails_sourceDestCheck,

    -- * AwsEc2NetworkInterfaceIpV6AddressDetail
    AwsEc2NetworkInterfaceIpV6AddressDetail (..),
    newAwsEc2NetworkInterfaceIpV6AddressDetail,
    awsEc2NetworkInterfaceIpV6AddressDetail_ipV6Address,

    -- * AwsEc2NetworkInterfacePrivateIpAddressDetail
    AwsEc2NetworkInterfacePrivateIpAddressDetail (..),
    newAwsEc2NetworkInterfacePrivateIpAddressDetail,
    awsEc2NetworkInterfacePrivateIpAddressDetail_privateDnsName,
    awsEc2NetworkInterfacePrivateIpAddressDetail_privateIpAddress,

    -- * AwsEc2NetworkInterfaceSecurityGroup
    AwsEc2NetworkInterfaceSecurityGroup (..),
    newAwsEc2NetworkInterfaceSecurityGroup,
    awsEc2NetworkInterfaceSecurityGroup_groupId,
    awsEc2NetworkInterfaceSecurityGroup_groupName,

    -- * AwsEc2RouteTableDetails
    AwsEc2RouteTableDetails (..),
    newAwsEc2RouteTableDetails,
    awsEc2RouteTableDetails_associationSet,
    awsEc2RouteTableDetails_ownerId,
    awsEc2RouteTableDetails_propagatingVgwSet,
    awsEc2RouteTableDetails_routeSet,
    awsEc2RouteTableDetails_routeTableId,
    awsEc2RouteTableDetails_vpcId,

    -- * AwsEc2SecurityGroupDetails
    AwsEc2SecurityGroupDetails (..),
    newAwsEc2SecurityGroupDetails,
    awsEc2SecurityGroupDetails_groupId,
    awsEc2SecurityGroupDetails_groupName,
    awsEc2SecurityGroupDetails_ipPermissions,
    awsEc2SecurityGroupDetails_ipPermissionsEgress,
    awsEc2SecurityGroupDetails_ownerId,
    awsEc2SecurityGroupDetails_vpcId,

    -- * AwsEc2SecurityGroupIpPermission
    AwsEc2SecurityGroupIpPermission (..),
    newAwsEc2SecurityGroupIpPermission,
    awsEc2SecurityGroupIpPermission_fromPort,
    awsEc2SecurityGroupIpPermission_ipProtocol,
    awsEc2SecurityGroupIpPermission_ipRanges,
    awsEc2SecurityGroupIpPermission_ipv6Ranges,
    awsEc2SecurityGroupIpPermission_prefixListIds,
    awsEc2SecurityGroupIpPermission_toPort,
    awsEc2SecurityGroupIpPermission_userIdGroupPairs,

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
    awsEc2SecurityGroupUserIdGroupPair_groupId,
    awsEc2SecurityGroupUserIdGroupPair_groupName,
    awsEc2SecurityGroupUserIdGroupPair_peeringStatus,
    awsEc2SecurityGroupUserIdGroupPair_userId,
    awsEc2SecurityGroupUserIdGroupPair_vpcId,
    awsEc2SecurityGroupUserIdGroupPair_vpcPeeringConnectionId,

    -- * AwsEc2SubnetDetails
    AwsEc2SubnetDetails (..),
    newAwsEc2SubnetDetails,
    awsEc2SubnetDetails_assignIpv6AddressOnCreation,
    awsEc2SubnetDetails_availabilityZone,
    awsEc2SubnetDetails_availabilityZoneId,
    awsEc2SubnetDetails_availableIpAddressCount,
    awsEc2SubnetDetails_cidrBlock,
    awsEc2SubnetDetails_defaultForAz,
    awsEc2SubnetDetails_ipv6CidrBlockAssociationSet,
    awsEc2SubnetDetails_mapPublicIpOnLaunch,
    awsEc2SubnetDetails_ownerId,
    awsEc2SubnetDetails_state,
    awsEc2SubnetDetails_subnetArn,
    awsEc2SubnetDetails_subnetId,
    awsEc2SubnetDetails_vpcId,

    -- * AwsEc2TransitGatewayDetails
    AwsEc2TransitGatewayDetails (..),
    newAwsEc2TransitGatewayDetails,
    awsEc2TransitGatewayDetails_amazonSideAsn,
    awsEc2TransitGatewayDetails_associationDefaultRouteTableId,
    awsEc2TransitGatewayDetails_autoAcceptSharedAttachments,
    awsEc2TransitGatewayDetails_defaultRouteTableAssociation,
    awsEc2TransitGatewayDetails_defaultRouteTablePropagation,
    awsEc2TransitGatewayDetails_description,
    awsEc2TransitGatewayDetails_dnsSupport,
    awsEc2TransitGatewayDetails_id,
    awsEc2TransitGatewayDetails_multicastSupport,
    awsEc2TransitGatewayDetails_propagationDefaultRouteTableId,
    awsEc2TransitGatewayDetails_transitGatewayCidrBlocks,
    awsEc2TransitGatewayDetails_vpnEcmpSupport,

    -- * AwsEc2VolumeAttachment
    AwsEc2VolumeAttachment (..),
    newAwsEc2VolumeAttachment,
    awsEc2VolumeAttachment_attachTime,
    awsEc2VolumeAttachment_deleteOnTermination,
    awsEc2VolumeAttachment_instanceId,
    awsEc2VolumeAttachment_status,

    -- * AwsEc2VolumeDetails
    AwsEc2VolumeDetails (..),
    newAwsEc2VolumeDetails,
    awsEc2VolumeDetails_attachments,
    awsEc2VolumeDetails_createTime,
    awsEc2VolumeDetails_deviceName,
    awsEc2VolumeDetails_encrypted,
    awsEc2VolumeDetails_kmsKeyId,
    awsEc2VolumeDetails_size,
    awsEc2VolumeDetails_snapshotId,
    awsEc2VolumeDetails_status,
    awsEc2VolumeDetails_volumeId,
    awsEc2VolumeDetails_volumeScanStatus,
    awsEc2VolumeDetails_volumeType,

    -- * AwsEc2VpcDetails
    AwsEc2VpcDetails (..),
    newAwsEc2VpcDetails,
    awsEc2VpcDetails_cidrBlockAssociationSet,
    awsEc2VpcDetails_dhcpOptionsId,
    awsEc2VpcDetails_ipv6CidrBlockAssociationSet,
    awsEc2VpcDetails_state,

    -- * AwsEc2VpcEndpointServiceDetails
    AwsEc2VpcEndpointServiceDetails (..),
    newAwsEc2VpcEndpointServiceDetails,
    awsEc2VpcEndpointServiceDetails_acceptanceRequired,
    awsEc2VpcEndpointServiceDetails_availabilityZones,
    awsEc2VpcEndpointServiceDetails_baseEndpointDnsNames,
    awsEc2VpcEndpointServiceDetails_gatewayLoadBalancerArns,
    awsEc2VpcEndpointServiceDetails_managesVpcEndpoints,
    awsEc2VpcEndpointServiceDetails_networkLoadBalancerArns,
    awsEc2VpcEndpointServiceDetails_privateDnsName,
    awsEc2VpcEndpointServiceDetails_serviceId,
    awsEc2VpcEndpointServiceDetails_serviceName,
    awsEc2VpcEndpointServiceDetails_serviceState,
    awsEc2VpcEndpointServiceDetails_serviceType,

    -- * AwsEc2VpcEndpointServiceServiceTypeDetails
    AwsEc2VpcEndpointServiceServiceTypeDetails (..),
    newAwsEc2VpcEndpointServiceServiceTypeDetails,
    awsEc2VpcEndpointServiceServiceTypeDetails_serviceType,

    -- * AwsEc2VpcPeeringConnectionDetails
    AwsEc2VpcPeeringConnectionDetails (..),
    newAwsEc2VpcPeeringConnectionDetails,
    awsEc2VpcPeeringConnectionDetails_accepterVpcInfo,
    awsEc2VpcPeeringConnectionDetails_expirationTime,
    awsEc2VpcPeeringConnectionDetails_requesterVpcInfo,
    awsEc2VpcPeeringConnectionDetails_status,
    awsEc2VpcPeeringConnectionDetails_vpcPeeringConnectionId,

    -- * AwsEc2VpcPeeringConnectionStatusDetails
    AwsEc2VpcPeeringConnectionStatusDetails (..),
    newAwsEc2VpcPeeringConnectionStatusDetails,
    awsEc2VpcPeeringConnectionStatusDetails_code,
    awsEc2VpcPeeringConnectionStatusDetails_message,

    -- * AwsEc2VpcPeeringConnectionVpcInfoDetails
    AwsEc2VpcPeeringConnectionVpcInfoDetails (..),
    newAwsEc2VpcPeeringConnectionVpcInfoDetails,
    awsEc2VpcPeeringConnectionVpcInfoDetails_cidrBlock,
    awsEc2VpcPeeringConnectionVpcInfoDetails_cidrBlockSet,
    awsEc2VpcPeeringConnectionVpcInfoDetails_ipv6CidrBlockSet,
    awsEc2VpcPeeringConnectionVpcInfoDetails_ownerId,
    awsEc2VpcPeeringConnectionVpcInfoDetails_peeringOptions,
    awsEc2VpcPeeringConnectionVpcInfoDetails_region,
    awsEc2VpcPeeringConnectionVpcInfoDetails_vpcId,

    -- * AwsEc2VpnConnectionDetails
    AwsEc2VpnConnectionDetails (..),
    newAwsEc2VpnConnectionDetails,
    awsEc2VpnConnectionDetails_category,
    awsEc2VpnConnectionDetails_customerGatewayConfiguration,
    awsEc2VpnConnectionDetails_customerGatewayId,
    awsEc2VpnConnectionDetails_options,
    awsEc2VpnConnectionDetails_routes,
    awsEc2VpnConnectionDetails_state,
    awsEc2VpnConnectionDetails_transitGatewayId,
    awsEc2VpnConnectionDetails_type,
    awsEc2VpnConnectionDetails_vgwTelemetry,
    awsEc2VpnConnectionDetails_vpnConnectionId,
    awsEc2VpnConnectionDetails_vpnGatewayId,

    -- * AwsEc2VpnConnectionOptionsDetails
    AwsEc2VpnConnectionOptionsDetails (..),
    newAwsEc2VpnConnectionOptionsDetails,
    awsEc2VpnConnectionOptionsDetails_staticRoutesOnly,
    awsEc2VpnConnectionOptionsDetails_tunnelOptions,

    -- * AwsEc2VpnConnectionOptionsTunnelOptionsDetails
    AwsEc2VpnConnectionOptionsTunnelOptionsDetails (..),
    newAwsEc2VpnConnectionOptionsTunnelOptionsDetails,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_dpdTimeoutSeconds,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_ikeVersions,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_outsideIpAddress,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase1DhGroupNumbers,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase1EncryptionAlgorithms,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase1IntegrityAlgorithms,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase1LifetimeSeconds,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase2DhGroupNumbers,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase2EncryptionAlgorithms,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase2IntegrityAlgorithms,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase2LifetimeSeconds,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_preSharedKey,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_rekeyFuzzPercentage,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_rekeyMarginTimeSeconds,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_replayWindowSize,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_tunnelInsideCidr,

    -- * AwsEc2VpnConnectionRoutesDetails
    AwsEc2VpnConnectionRoutesDetails (..),
    newAwsEc2VpnConnectionRoutesDetails,
    awsEc2VpnConnectionRoutesDetails_destinationCidrBlock,
    awsEc2VpnConnectionRoutesDetails_state,

    -- * AwsEc2VpnConnectionVgwTelemetryDetails
    AwsEc2VpnConnectionVgwTelemetryDetails (..),
    newAwsEc2VpnConnectionVgwTelemetryDetails,
    awsEc2VpnConnectionVgwTelemetryDetails_acceptedRouteCount,
    awsEc2VpnConnectionVgwTelemetryDetails_certificateArn,
    awsEc2VpnConnectionVgwTelemetryDetails_lastStatusChange,
    awsEc2VpnConnectionVgwTelemetryDetails_outsideIpAddress,
    awsEc2VpnConnectionVgwTelemetryDetails_status,
    awsEc2VpnConnectionVgwTelemetryDetails_statusMessage,

    -- * AwsEcrContainerImageDetails
    AwsEcrContainerImageDetails (..),
    newAwsEcrContainerImageDetails,
    awsEcrContainerImageDetails_architecture,
    awsEcrContainerImageDetails_imageDigest,
    awsEcrContainerImageDetails_imagePublishedAt,
    awsEcrContainerImageDetails_imageTags,
    awsEcrContainerImageDetails_registryId,
    awsEcrContainerImageDetails_repositoryName,

    -- * AwsEcrRepositoryDetails
    AwsEcrRepositoryDetails (..),
    newAwsEcrRepositoryDetails,
    awsEcrRepositoryDetails_arn,
    awsEcrRepositoryDetails_imageScanningConfiguration,
    awsEcrRepositoryDetails_imageTagMutability,
    awsEcrRepositoryDetails_lifecyclePolicy,
    awsEcrRepositoryDetails_repositoryName,
    awsEcrRepositoryDetails_repositoryPolicyText,

    -- * AwsEcrRepositoryImageScanningConfigurationDetails
    AwsEcrRepositoryImageScanningConfigurationDetails (..),
    newAwsEcrRepositoryImageScanningConfigurationDetails,
    awsEcrRepositoryImageScanningConfigurationDetails_scanOnPush,

    -- * AwsEcrRepositoryLifecyclePolicyDetails
    AwsEcrRepositoryLifecyclePolicyDetails (..),
    newAwsEcrRepositoryLifecyclePolicyDetails,
    awsEcrRepositoryLifecyclePolicyDetails_lifecyclePolicyText,
    awsEcrRepositoryLifecyclePolicyDetails_registryId,

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
    awsEcsClusterConfigurationExecuteCommandConfigurationDetails_kmsKeyId,
    awsEcsClusterConfigurationExecuteCommandConfigurationDetails_logConfiguration,
    awsEcsClusterConfigurationExecuteCommandConfigurationDetails_logging,

    -- * AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails
    AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails (..),
    newAwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails,
    awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_cloudWatchEncryptionEnabled,
    awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_cloudWatchLogGroupName,
    awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_s3BucketName,
    awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_s3EncryptionEnabled,
    awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_s3KeyPrefix,

    -- * AwsEcsClusterDefaultCapacityProviderStrategyDetails
    AwsEcsClusterDefaultCapacityProviderStrategyDetails (..),
    newAwsEcsClusterDefaultCapacityProviderStrategyDetails,
    awsEcsClusterDefaultCapacityProviderStrategyDetails_base,
    awsEcsClusterDefaultCapacityProviderStrategyDetails_capacityProvider,
    awsEcsClusterDefaultCapacityProviderStrategyDetails_weight,

    -- * AwsEcsClusterDetails
    AwsEcsClusterDetails (..),
    newAwsEcsClusterDetails,
    awsEcsClusterDetails_activeServicesCount,
    awsEcsClusterDetails_capacityProviders,
    awsEcsClusterDetails_clusterArn,
    awsEcsClusterDetails_clusterName,
    awsEcsClusterDetails_clusterSettings,
    awsEcsClusterDetails_configuration,
    awsEcsClusterDetails_defaultCapacityProviderStrategy,
    awsEcsClusterDetails_registeredContainerInstancesCount,
    awsEcsClusterDetails_runningTasksCount,
    awsEcsClusterDetails_status,

    -- * AwsEcsContainerDetails
    AwsEcsContainerDetails (..),
    newAwsEcsContainerDetails,
    awsEcsContainerDetails_image,
    awsEcsContainerDetails_mountPoints,
    awsEcsContainerDetails_name,
    awsEcsContainerDetails_privileged,

    -- * AwsEcsServiceCapacityProviderStrategyDetails
    AwsEcsServiceCapacityProviderStrategyDetails (..),
    newAwsEcsServiceCapacityProviderStrategyDetails,
    awsEcsServiceCapacityProviderStrategyDetails_base,
    awsEcsServiceCapacityProviderStrategyDetails_capacityProvider,
    awsEcsServiceCapacityProviderStrategyDetails_weight,

    -- * AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails
    AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails (..),
    newAwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails,
    awsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails_enable,
    awsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails_rollback,

    -- * AwsEcsServiceDeploymentConfigurationDetails
    AwsEcsServiceDeploymentConfigurationDetails (..),
    newAwsEcsServiceDeploymentConfigurationDetails,
    awsEcsServiceDeploymentConfigurationDetails_deploymentCircuitBreaker,
    awsEcsServiceDeploymentConfigurationDetails_maximumPercent,
    awsEcsServiceDeploymentConfigurationDetails_minimumHealthyPercent,

    -- * AwsEcsServiceDeploymentControllerDetails
    AwsEcsServiceDeploymentControllerDetails (..),
    newAwsEcsServiceDeploymentControllerDetails,
    awsEcsServiceDeploymentControllerDetails_type,

    -- * AwsEcsServiceDetails
    AwsEcsServiceDetails (..),
    newAwsEcsServiceDetails,
    awsEcsServiceDetails_capacityProviderStrategy,
    awsEcsServiceDetails_cluster,
    awsEcsServiceDetails_deploymentConfiguration,
    awsEcsServiceDetails_deploymentController,
    awsEcsServiceDetails_desiredCount,
    awsEcsServiceDetails_enableEcsManagedTags,
    awsEcsServiceDetails_enableExecuteCommand,
    awsEcsServiceDetails_healthCheckGracePeriodSeconds,
    awsEcsServiceDetails_launchType,
    awsEcsServiceDetails_loadBalancers,
    awsEcsServiceDetails_name,
    awsEcsServiceDetails_networkConfiguration,
    awsEcsServiceDetails_placementConstraints,
    awsEcsServiceDetails_placementStrategies,
    awsEcsServiceDetails_platformVersion,
    awsEcsServiceDetails_propagateTags,
    awsEcsServiceDetails_role,
    awsEcsServiceDetails_schedulingStrategy,
    awsEcsServiceDetails_serviceArn,
    awsEcsServiceDetails_serviceName,
    awsEcsServiceDetails_serviceRegistries,
    awsEcsServiceDetails_taskDefinition,

    -- * AwsEcsServiceLoadBalancersDetails
    AwsEcsServiceLoadBalancersDetails (..),
    newAwsEcsServiceLoadBalancersDetails,
    awsEcsServiceLoadBalancersDetails_containerName,
    awsEcsServiceLoadBalancersDetails_containerPort,
    awsEcsServiceLoadBalancersDetails_loadBalancerName,
    awsEcsServiceLoadBalancersDetails_targetGroupArn,

    -- * AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails
    AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails (..),
    newAwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails,
    awsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails_assignPublicIp,
    awsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails_securityGroups,
    awsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails_subnets,

    -- * AwsEcsServiceNetworkConfigurationDetails
    AwsEcsServiceNetworkConfigurationDetails (..),
    newAwsEcsServiceNetworkConfigurationDetails,
    awsEcsServiceNetworkConfigurationDetails_awsVpcConfiguration,

    -- * AwsEcsServicePlacementConstraintsDetails
    AwsEcsServicePlacementConstraintsDetails (..),
    newAwsEcsServicePlacementConstraintsDetails,
    awsEcsServicePlacementConstraintsDetails_expression,
    awsEcsServicePlacementConstraintsDetails_type,

    -- * AwsEcsServicePlacementStrategiesDetails
    AwsEcsServicePlacementStrategiesDetails (..),
    newAwsEcsServicePlacementStrategiesDetails,
    awsEcsServicePlacementStrategiesDetails_field,
    awsEcsServicePlacementStrategiesDetails_type,

    -- * AwsEcsServiceServiceRegistriesDetails
    AwsEcsServiceServiceRegistriesDetails (..),
    newAwsEcsServiceServiceRegistriesDetails,
    awsEcsServiceServiceRegistriesDetails_containerName,
    awsEcsServiceServiceRegistriesDetails_containerPort,
    awsEcsServiceServiceRegistriesDetails_port,
    awsEcsServiceServiceRegistriesDetails_registryArn,

    -- * AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails
    AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails (..),
    newAwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails,
    awsEcsTaskDefinitionContainerDefinitionsDependsOnDetails_condition,
    awsEcsTaskDefinitionContainerDefinitionsDependsOnDetails_containerName,

    -- * AwsEcsTaskDefinitionContainerDefinitionsDetails
    AwsEcsTaskDefinitionContainerDefinitionsDetails (..),
    newAwsEcsTaskDefinitionContainerDefinitionsDetails,
    awsEcsTaskDefinitionContainerDefinitionsDetails_command,
    awsEcsTaskDefinitionContainerDefinitionsDetails_cpu,
    awsEcsTaskDefinitionContainerDefinitionsDetails_dependsOn,
    awsEcsTaskDefinitionContainerDefinitionsDetails_disableNetworking,
    awsEcsTaskDefinitionContainerDefinitionsDetails_dnsSearchDomains,
    awsEcsTaskDefinitionContainerDefinitionsDetails_dnsServers,
    awsEcsTaskDefinitionContainerDefinitionsDetails_dockerLabels,
    awsEcsTaskDefinitionContainerDefinitionsDetails_dockerSecurityOptions,
    awsEcsTaskDefinitionContainerDefinitionsDetails_entryPoint,
    awsEcsTaskDefinitionContainerDefinitionsDetails_environment,
    awsEcsTaskDefinitionContainerDefinitionsDetails_environmentFiles,
    awsEcsTaskDefinitionContainerDefinitionsDetails_essential,
    awsEcsTaskDefinitionContainerDefinitionsDetails_extraHosts,
    awsEcsTaskDefinitionContainerDefinitionsDetails_firelensConfiguration,
    awsEcsTaskDefinitionContainerDefinitionsDetails_healthCheck,
    awsEcsTaskDefinitionContainerDefinitionsDetails_hostname,
    awsEcsTaskDefinitionContainerDefinitionsDetails_image,
    awsEcsTaskDefinitionContainerDefinitionsDetails_interactive,
    awsEcsTaskDefinitionContainerDefinitionsDetails_links,
    awsEcsTaskDefinitionContainerDefinitionsDetails_linuxParameters,
    awsEcsTaskDefinitionContainerDefinitionsDetails_logConfiguration,
    awsEcsTaskDefinitionContainerDefinitionsDetails_memory,
    awsEcsTaskDefinitionContainerDefinitionsDetails_memoryReservation,
    awsEcsTaskDefinitionContainerDefinitionsDetails_mountPoints,
    awsEcsTaskDefinitionContainerDefinitionsDetails_name,
    awsEcsTaskDefinitionContainerDefinitionsDetails_portMappings,
    awsEcsTaskDefinitionContainerDefinitionsDetails_privileged,
    awsEcsTaskDefinitionContainerDefinitionsDetails_pseudoTerminal,
    awsEcsTaskDefinitionContainerDefinitionsDetails_readonlyRootFilesystem,
    awsEcsTaskDefinitionContainerDefinitionsDetails_repositoryCredentials,
    awsEcsTaskDefinitionContainerDefinitionsDetails_resourceRequirements,
    awsEcsTaskDefinitionContainerDefinitionsDetails_secrets,
    awsEcsTaskDefinitionContainerDefinitionsDetails_startTimeout,
    awsEcsTaskDefinitionContainerDefinitionsDetails_stopTimeout,
    awsEcsTaskDefinitionContainerDefinitionsDetails_systemControls,
    awsEcsTaskDefinitionContainerDefinitionsDetails_ulimits,
    awsEcsTaskDefinitionContainerDefinitionsDetails_user,
    awsEcsTaskDefinitionContainerDefinitionsDetails_volumesFrom,
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
    awsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails_options,
    awsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails_type,

    -- * AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails
    AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails (..),
    newAwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails,
    awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_command,
    awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_interval,
    awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_retries,
    awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_startPeriod,
    awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_timeout,

    -- * AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails (..),
    newAwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails_add,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails_drop,

    -- * AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails (..),
    newAwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_capabilities,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_devices,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_initProcessEnabled,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_maxSwap,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_sharedMemorySize,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_swappiness,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_tmpfs,

    -- * AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails (..),
    newAwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails_containerPath,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails_hostPath,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails_permissions,

    -- * AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails (..),
    newAwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails_containerPath,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails_mountOptions,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails_size,

    -- * AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails
    AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails (..),
    newAwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails,
    awsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails_logDriver,
    awsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails_options,
    awsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails_secretOptions,

    -- * AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails
    AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails (..),
    newAwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails,
    awsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails_name,
    awsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails_valueFrom,

    -- * AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails
    AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails (..),
    newAwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails,
    awsEcsTaskDefinitionContainerDefinitionsMountPointsDetails_containerPath,
    awsEcsTaskDefinitionContainerDefinitionsMountPointsDetails_readOnly,
    awsEcsTaskDefinitionContainerDefinitionsMountPointsDetails_sourceVolume,

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
    awsEcsTaskDefinitionContainerDefinitionsUlimitsDetails_hardLimit,
    awsEcsTaskDefinitionContainerDefinitionsUlimitsDetails_name,
    awsEcsTaskDefinitionContainerDefinitionsUlimitsDetails_softLimit,

    -- * AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails
    AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails (..),
    newAwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails,
    awsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails_readOnly,
    awsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails_sourceContainer,

    -- * AwsEcsTaskDefinitionDetails
    AwsEcsTaskDefinitionDetails (..),
    newAwsEcsTaskDefinitionDetails,
    awsEcsTaskDefinitionDetails_containerDefinitions,
    awsEcsTaskDefinitionDetails_cpu,
    awsEcsTaskDefinitionDetails_executionRoleArn,
    awsEcsTaskDefinitionDetails_family,
    awsEcsTaskDefinitionDetails_inferenceAccelerators,
    awsEcsTaskDefinitionDetails_ipcMode,
    awsEcsTaskDefinitionDetails_memory,
    awsEcsTaskDefinitionDetails_networkMode,
    awsEcsTaskDefinitionDetails_pidMode,
    awsEcsTaskDefinitionDetails_placementConstraints,
    awsEcsTaskDefinitionDetails_proxyConfiguration,
    awsEcsTaskDefinitionDetails_requiresCompatibilities,
    awsEcsTaskDefinitionDetails_taskRoleArn,
    awsEcsTaskDefinitionDetails_volumes,

    -- * AwsEcsTaskDefinitionInferenceAcceleratorsDetails
    AwsEcsTaskDefinitionInferenceAcceleratorsDetails (..),
    newAwsEcsTaskDefinitionInferenceAcceleratorsDetails,
    awsEcsTaskDefinitionInferenceAcceleratorsDetails_deviceName,
    awsEcsTaskDefinitionInferenceAcceleratorsDetails_deviceType,

    -- * AwsEcsTaskDefinitionPlacementConstraintsDetails
    AwsEcsTaskDefinitionPlacementConstraintsDetails (..),
    newAwsEcsTaskDefinitionPlacementConstraintsDetails,
    awsEcsTaskDefinitionPlacementConstraintsDetails_expression,
    awsEcsTaskDefinitionPlacementConstraintsDetails_type,

    -- * AwsEcsTaskDefinitionProxyConfigurationDetails
    AwsEcsTaskDefinitionProxyConfigurationDetails (..),
    newAwsEcsTaskDefinitionProxyConfigurationDetails,
    awsEcsTaskDefinitionProxyConfigurationDetails_containerName,
    awsEcsTaskDefinitionProxyConfigurationDetails_proxyConfigurationProperties,
    awsEcsTaskDefinitionProxyConfigurationDetails_type,

    -- * AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails
    AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails (..),
    newAwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails,
    awsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails_name,
    awsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails_value,

    -- * AwsEcsTaskDefinitionVolumesDetails
    AwsEcsTaskDefinitionVolumesDetails (..),
    newAwsEcsTaskDefinitionVolumesDetails,
    awsEcsTaskDefinitionVolumesDetails_dockerVolumeConfiguration,
    awsEcsTaskDefinitionVolumesDetails_efsVolumeConfiguration,
    awsEcsTaskDefinitionVolumesDetails_host,
    awsEcsTaskDefinitionVolumesDetails_name,

    -- * AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails
    AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails (..),
    newAwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails,
    awsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails_autoprovision,
    awsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails_driver,
    awsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails_driverOpts,
    awsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails_labels,
    awsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails_scope,

    -- * AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails
    AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails (..),
    newAwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails,
    awsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails_accessPointId,
    awsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails_iam,

    -- * AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails
    AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails (..),
    newAwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails,
    awsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails_authorizationConfig,
    awsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails_filesystemId,
    awsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails_rootDirectory,
    awsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails_transitEncryption,
    awsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails_transitEncryptionPort,

    -- * AwsEcsTaskDefinitionVolumesHostDetails
    AwsEcsTaskDefinitionVolumesHostDetails (..),
    newAwsEcsTaskDefinitionVolumesHostDetails,
    awsEcsTaskDefinitionVolumesHostDetails_sourcePath,

    -- * AwsEcsTaskDetails
    AwsEcsTaskDetails (..),
    newAwsEcsTaskDetails,
    awsEcsTaskDetails_clusterArn,
    awsEcsTaskDetails_containers,
    awsEcsTaskDetails_createdAt,
    awsEcsTaskDetails_group,
    awsEcsTaskDetails_startedAt,
    awsEcsTaskDetails_startedBy,
    awsEcsTaskDetails_taskDefinitionArn,
    awsEcsTaskDetails_version,
    awsEcsTaskDetails_volumes,

    -- * AwsEcsTaskVolumeDetails
    AwsEcsTaskVolumeDetails (..),
    newAwsEcsTaskVolumeDetails,
    awsEcsTaskVolumeDetails_host,
    awsEcsTaskVolumeDetails_name,

    -- * AwsEcsTaskVolumeHostDetails
    AwsEcsTaskVolumeHostDetails (..),
    newAwsEcsTaskVolumeHostDetails,
    awsEcsTaskVolumeHostDetails_sourcePath,

    -- * AwsEfsAccessPointDetails
    AwsEfsAccessPointDetails (..),
    newAwsEfsAccessPointDetails,
    awsEfsAccessPointDetails_accessPointId,
    awsEfsAccessPointDetails_arn,
    awsEfsAccessPointDetails_clientToken,
    awsEfsAccessPointDetails_fileSystemId,
    awsEfsAccessPointDetails_posixUser,
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
    awsEfsAccessPointRootDirectoryCreationInfoDetails_ownerGid,
    awsEfsAccessPointRootDirectoryCreationInfoDetails_ownerUid,
    awsEfsAccessPointRootDirectoryCreationInfoDetails_permissions,

    -- * AwsEfsAccessPointRootDirectoryDetails
    AwsEfsAccessPointRootDirectoryDetails (..),
    newAwsEfsAccessPointRootDirectoryDetails,
    awsEfsAccessPointRootDirectoryDetails_creationInfo,
    awsEfsAccessPointRootDirectoryDetails_path,

    -- * AwsEksClusterDetails
    AwsEksClusterDetails (..),
    newAwsEksClusterDetails,
    awsEksClusterDetails_arn,
    awsEksClusterDetails_certificateAuthorityData,
    awsEksClusterDetails_clusterStatus,
    awsEksClusterDetails_endpoint,
    awsEksClusterDetails_logging,
    awsEksClusterDetails_name,
    awsEksClusterDetails_resourcesVpcConfig,
    awsEksClusterDetails_roleArn,
    awsEksClusterDetails_version,

    -- * AwsEksClusterLoggingClusterLoggingDetails
    AwsEksClusterLoggingClusterLoggingDetails (..),
    newAwsEksClusterLoggingClusterLoggingDetails,
    awsEksClusterLoggingClusterLoggingDetails_enabled,
    awsEksClusterLoggingClusterLoggingDetails_types,

    -- * AwsEksClusterLoggingDetails
    AwsEksClusterLoggingDetails (..),
    newAwsEksClusterLoggingDetails,
    awsEksClusterLoggingDetails_clusterLogging,

    -- * AwsEksClusterResourcesVpcConfigDetails
    AwsEksClusterResourcesVpcConfigDetails (..),
    newAwsEksClusterResourcesVpcConfigDetails,
    awsEksClusterResourcesVpcConfigDetails_endpointPublicAccess,
    awsEksClusterResourcesVpcConfigDetails_securityGroupIds,
    awsEksClusterResourcesVpcConfigDetails_subnetIds,

    -- * AwsElasticBeanstalkEnvironmentDetails
    AwsElasticBeanstalkEnvironmentDetails (..),
    newAwsElasticBeanstalkEnvironmentDetails,
    awsElasticBeanstalkEnvironmentDetails_applicationName,
    awsElasticBeanstalkEnvironmentDetails_cname,
    awsElasticBeanstalkEnvironmentDetails_dateCreated,
    awsElasticBeanstalkEnvironmentDetails_dateUpdated,
    awsElasticBeanstalkEnvironmentDetails_description,
    awsElasticBeanstalkEnvironmentDetails_endpointUrl,
    awsElasticBeanstalkEnvironmentDetails_environmentArn,
    awsElasticBeanstalkEnvironmentDetails_environmentId,
    awsElasticBeanstalkEnvironmentDetails_environmentLinks,
    awsElasticBeanstalkEnvironmentDetails_environmentName,
    awsElasticBeanstalkEnvironmentDetails_optionSettings,
    awsElasticBeanstalkEnvironmentDetails_platformArn,
    awsElasticBeanstalkEnvironmentDetails_solutionStackName,
    awsElasticBeanstalkEnvironmentDetails_status,
    awsElasticBeanstalkEnvironmentDetails_tier,
    awsElasticBeanstalkEnvironmentDetails_versionLabel,

    -- * AwsElasticBeanstalkEnvironmentEnvironmentLink
    AwsElasticBeanstalkEnvironmentEnvironmentLink (..),
    newAwsElasticBeanstalkEnvironmentEnvironmentLink,
    awsElasticBeanstalkEnvironmentEnvironmentLink_environmentName,
    awsElasticBeanstalkEnvironmentEnvironmentLink_linkName,

    -- * AwsElasticBeanstalkEnvironmentOptionSetting
    AwsElasticBeanstalkEnvironmentOptionSetting (..),
    newAwsElasticBeanstalkEnvironmentOptionSetting,
    awsElasticBeanstalkEnvironmentOptionSetting_namespace,
    awsElasticBeanstalkEnvironmentOptionSetting_optionName,
    awsElasticBeanstalkEnvironmentOptionSetting_resourceName,
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
    awsElasticsearchDomainDetails_accessPolicies,
    awsElasticsearchDomainDetails_domainEndpointOptions,
    awsElasticsearchDomainDetails_domainId,
    awsElasticsearchDomainDetails_domainName,
    awsElasticsearchDomainDetails_elasticsearchClusterConfig,
    awsElasticsearchDomainDetails_elasticsearchVersion,
    awsElasticsearchDomainDetails_encryptionAtRestOptions,
    awsElasticsearchDomainDetails_endpoint,
    awsElasticsearchDomainDetails_endpoints,
    awsElasticsearchDomainDetails_logPublishingOptions,
    awsElasticsearchDomainDetails_nodeToNodeEncryptionOptions,
    awsElasticsearchDomainDetails_serviceSoftwareOptions,
    awsElasticsearchDomainDetails_vPCOptions,

    -- * AwsElasticsearchDomainDomainEndpointOptions
    AwsElasticsearchDomainDomainEndpointOptions (..),
    newAwsElasticsearchDomainDomainEndpointOptions,
    awsElasticsearchDomainDomainEndpointOptions_enforceHTTPS,
    awsElasticsearchDomainDomainEndpointOptions_tLSSecurityPolicy,

    -- * AwsElasticsearchDomainElasticsearchClusterConfigDetails
    AwsElasticsearchDomainElasticsearchClusterConfigDetails (..),
    newAwsElasticsearchDomainElasticsearchClusterConfigDetails,
    awsElasticsearchDomainElasticsearchClusterConfigDetails_dedicatedMasterCount,
    awsElasticsearchDomainElasticsearchClusterConfigDetails_dedicatedMasterEnabled,
    awsElasticsearchDomainElasticsearchClusterConfigDetails_dedicatedMasterType,
    awsElasticsearchDomainElasticsearchClusterConfigDetails_instanceCount,
    awsElasticsearchDomainElasticsearchClusterConfigDetails_instanceType,
    awsElasticsearchDomainElasticsearchClusterConfigDetails_zoneAwarenessConfig,
    awsElasticsearchDomainElasticsearchClusterConfigDetails_zoneAwarenessEnabled,

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
    awsElasticsearchDomainLogPublishingOptions_auditLogs,
    awsElasticsearchDomainLogPublishingOptions_indexSlowLogs,
    awsElasticsearchDomainLogPublishingOptions_searchSlowLogs,

    -- * AwsElasticsearchDomainLogPublishingOptionsLogConfig
    AwsElasticsearchDomainLogPublishingOptionsLogConfig (..),
    newAwsElasticsearchDomainLogPublishingOptionsLogConfig,
    awsElasticsearchDomainLogPublishingOptionsLogConfig_cloudWatchLogsLogGroupArn,
    awsElasticsearchDomainLogPublishingOptionsLogConfig_enabled,

    -- * AwsElasticsearchDomainNodeToNodeEncryptionOptions
    AwsElasticsearchDomainNodeToNodeEncryptionOptions (..),
    newAwsElasticsearchDomainNodeToNodeEncryptionOptions,
    awsElasticsearchDomainNodeToNodeEncryptionOptions_enabled,

    -- * AwsElasticsearchDomainServiceSoftwareOptions
    AwsElasticsearchDomainServiceSoftwareOptions (..),
    newAwsElasticsearchDomainServiceSoftwareOptions,
    awsElasticsearchDomainServiceSoftwareOptions_automatedUpdateDate,
    awsElasticsearchDomainServiceSoftwareOptions_cancellable,
    awsElasticsearchDomainServiceSoftwareOptions_currentVersion,
    awsElasticsearchDomainServiceSoftwareOptions_description,
    awsElasticsearchDomainServiceSoftwareOptions_newVersion,
    awsElasticsearchDomainServiceSoftwareOptions_updateAvailable,
    awsElasticsearchDomainServiceSoftwareOptions_updateStatus,

    -- * AwsElasticsearchDomainVPCOptions
    AwsElasticsearchDomainVPCOptions (..),
    newAwsElasticsearchDomainVPCOptions,
    awsElasticsearchDomainVPCOptions_availabilityZones,
    awsElasticsearchDomainVPCOptions_securityGroupIds,
    awsElasticsearchDomainVPCOptions_subnetIds,
    awsElasticsearchDomainVPCOptions_vPCId,

    -- * AwsElbAppCookieStickinessPolicy
    AwsElbAppCookieStickinessPolicy (..),
    newAwsElbAppCookieStickinessPolicy,
    awsElbAppCookieStickinessPolicy_cookieName,
    awsElbAppCookieStickinessPolicy_policyName,

    -- * AwsElbLbCookieStickinessPolicy
    AwsElbLbCookieStickinessPolicy (..),
    newAwsElbLbCookieStickinessPolicy,
    awsElbLbCookieStickinessPolicy_cookieExpirationPeriod,
    awsElbLbCookieStickinessPolicy_policyName,

    -- * AwsElbLoadBalancerAccessLog
    AwsElbLoadBalancerAccessLog (..),
    newAwsElbLoadBalancerAccessLog,
    awsElbLoadBalancerAccessLog_emitInterval,
    awsElbLoadBalancerAccessLog_enabled,
    awsElbLoadBalancerAccessLog_s3BucketName,
    awsElbLoadBalancerAccessLog_s3BucketPrefix,

    -- * AwsElbLoadBalancerAdditionalAttribute
    AwsElbLoadBalancerAdditionalAttribute (..),
    newAwsElbLoadBalancerAdditionalAttribute,
    awsElbLoadBalancerAdditionalAttribute_key,
    awsElbLoadBalancerAdditionalAttribute_value,

    -- * AwsElbLoadBalancerAttributes
    AwsElbLoadBalancerAttributes (..),
    newAwsElbLoadBalancerAttributes,
    awsElbLoadBalancerAttributes_accessLog,
    awsElbLoadBalancerAttributes_additionalAttributes,
    awsElbLoadBalancerAttributes_connectionDraining,
    awsElbLoadBalancerAttributes_connectionSettings,
    awsElbLoadBalancerAttributes_crossZoneLoadBalancing,

    -- * AwsElbLoadBalancerBackendServerDescription
    AwsElbLoadBalancerBackendServerDescription (..),
    newAwsElbLoadBalancerBackendServerDescription,
    awsElbLoadBalancerBackendServerDescription_instancePort,
    awsElbLoadBalancerBackendServerDescription_policyNames,

    -- * AwsElbLoadBalancerConnectionDraining
    AwsElbLoadBalancerConnectionDraining (..),
    newAwsElbLoadBalancerConnectionDraining,
    awsElbLoadBalancerConnectionDraining_enabled,
    awsElbLoadBalancerConnectionDraining_timeout,

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
    awsElbLoadBalancerDetails_availabilityZones,
    awsElbLoadBalancerDetails_backendServerDescriptions,
    awsElbLoadBalancerDetails_canonicalHostedZoneName,
    awsElbLoadBalancerDetails_canonicalHostedZoneNameID,
    awsElbLoadBalancerDetails_createdTime,
    awsElbLoadBalancerDetails_dnsName,
    awsElbLoadBalancerDetails_healthCheck,
    awsElbLoadBalancerDetails_instances,
    awsElbLoadBalancerDetails_listenerDescriptions,
    awsElbLoadBalancerDetails_loadBalancerAttributes,
    awsElbLoadBalancerDetails_loadBalancerName,
    awsElbLoadBalancerDetails_policies,
    awsElbLoadBalancerDetails_scheme,
    awsElbLoadBalancerDetails_securityGroups,
    awsElbLoadBalancerDetails_sourceSecurityGroup,
    awsElbLoadBalancerDetails_subnets,
    awsElbLoadBalancerDetails_vpcId,

    -- * AwsElbLoadBalancerHealthCheck
    AwsElbLoadBalancerHealthCheck (..),
    newAwsElbLoadBalancerHealthCheck,
    awsElbLoadBalancerHealthCheck_healthyThreshold,
    awsElbLoadBalancerHealthCheck_interval,
    awsElbLoadBalancerHealthCheck_target,
    awsElbLoadBalancerHealthCheck_timeout,
    awsElbLoadBalancerHealthCheck_unhealthyThreshold,

    -- * AwsElbLoadBalancerInstance
    AwsElbLoadBalancerInstance (..),
    newAwsElbLoadBalancerInstance,
    awsElbLoadBalancerInstance_instanceId,

    -- * AwsElbLoadBalancerListener
    AwsElbLoadBalancerListener (..),
    newAwsElbLoadBalancerListener,
    awsElbLoadBalancerListener_instancePort,
    awsElbLoadBalancerListener_instanceProtocol,
    awsElbLoadBalancerListener_loadBalancerPort,
    awsElbLoadBalancerListener_protocol,
    awsElbLoadBalancerListener_sslCertificateId,

    -- * AwsElbLoadBalancerListenerDescription
    AwsElbLoadBalancerListenerDescription (..),
    newAwsElbLoadBalancerListenerDescription,
    awsElbLoadBalancerListenerDescription_listener,
    awsElbLoadBalancerListenerDescription_policyNames,

    -- * AwsElbLoadBalancerPolicies
    AwsElbLoadBalancerPolicies (..),
    newAwsElbLoadBalancerPolicies,
    awsElbLoadBalancerPolicies_appCookieStickinessPolicies,
    awsElbLoadBalancerPolicies_lbCookieStickinessPolicies,
    awsElbLoadBalancerPolicies_otherPolicies,

    -- * AwsElbLoadBalancerSourceSecurityGroup
    AwsElbLoadBalancerSourceSecurityGroup (..),
    newAwsElbLoadBalancerSourceSecurityGroup,
    awsElbLoadBalancerSourceSecurityGroup_groupName,
    awsElbLoadBalancerSourceSecurityGroup_ownerAlias,

    -- * AwsElbv2LoadBalancerAttribute
    AwsElbv2LoadBalancerAttribute (..),
    newAwsElbv2LoadBalancerAttribute,
    awsElbv2LoadBalancerAttribute_key,
    awsElbv2LoadBalancerAttribute_value,

    -- * AwsElbv2LoadBalancerDetails
    AwsElbv2LoadBalancerDetails (..),
    newAwsElbv2LoadBalancerDetails,
    awsElbv2LoadBalancerDetails_availabilityZones,
    awsElbv2LoadBalancerDetails_canonicalHostedZoneId,
    awsElbv2LoadBalancerDetails_createdTime,
    awsElbv2LoadBalancerDetails_dNSName,
    awsElbv2LoadBalancerDetails_ipAddressType,
    awsElbv2LoadBalancerDetails_loadBalancerAttributes,
    awsElbv2LoadBalancerDetails_scheme,
    awsElbv2LoadBalancerDetails_securityGroups,
    awsElbv2LoadBalancerDetails_state,
    awsElbv2LoadBalancerDetails_type,
    awsElbv2LoadBalancerDetails_vpcId,

    -- * AwsEventSchemasRegistryDetails
    AwsEventSchemasRegistryDetails (..),
    newAwsEventSchemasRegistryDetails,
    awsEventSchemasRegistryDetails_description,
    awsEventSchemasRegistryDetails_registryArn,
    awsEventSchemasRegistryDetails_registryName,

    -- * AwsGuardDutyDetectorDataSourcesCloudTrailDetails
    AwsGuardDutyDetectorDataSourcesCloudTrailDetails (..),
    newAwsGuardDutyDetectorDataSourcesCloudTrailDetails,
    awsGuardDutyDetectorDataSourcesCloudTrailDetails_status,

    -- * AwsGuardDutyDetectorDataSourcesDetails
    AwsGuardDutyDetectorDataSourcesDetails (..),
    newAwsGuardDutyDetectorDataSourcesDetails,
    awsGuardDutyDetectorDataSourcesDetails_cloudTrail,
    awsGuardDutyDetectorDataSourcesDetails_dnsLogs,
    awsGuardDutyDetectorDataSourcesDetails_flowLogs,
    awsGuardDutyDetectorDataSourcesDetails_kubernetes,
    awsGuardDutyDetectorDataSourcesDetails_malwareProtection,
    awsGuardDutyDetectorDataSourcesDetails_s3Logs,

    -- * AwsGuardDutyDetectorDataSourcesDnsLogsDetails
    AwsGuardDutyDetectorDataSourcesDnsLogsDetails (..),
    newAwsGuardDutyDetectorDataSourcesDnsLogsDetails,
    awsGuardDutyDetectorDataSourcesDnsLogsDetails_status,

    -- * AwsGuardDutyDetectorDataSourcesFlowLogsDetails
    AwsGuardDutyDetectorDataSourcesFlowLogsDetails (..),
    newAwsGuardDutyDetectorDataSourcesFlowLogsDetails,
    awsGuardDutyDetectorDataSourcesFlowLogsDetails_status,

    -- * AwsGuardDutyDetectorDataSourcesKubernetesAuditLogsDetails
    AwsGuardDutyDetectorDataSourcesKubernetesAuditLogsDetails (..),
    newAwsGuardDutyDetectorDataSourcesKubernetesAuditLogsDetails,
    awsGuardDutyDetectorDataSourcesKubernetesAuditLogsDetails_status,

    -- * AwsGuardDutyDetectorDataSourcesKubernetesDetails
    AwsGuardDutyDetectorDataSourcesKubernetesDetails (..),
    newAwsGuardDutyDetectorDataSourcesKubernetesDetails,
    awsGuardDutyDetectorDataSourcesKubernetesDetails_auditLogs,

    -- * AwsGuardDutyDetectorDataSourcesMalwareProtectionDetails
    AwsGuardDutyDetectorDataSourcesMalwareProtectionDetails (..),
    newAwsGuardDutyDetectorDataSourcesMalwareProtectionDetails,
    awsGuardDutyDetectorDataSourcesMalwareProtectionDetails_scanEc2InstanceWithFindings,
    awsGuardDutyDetectorDataSourcesMalwareProtectionDetails_serviceRole,

    -- * AwsGuardDutyDetectorDataSourcesMalwareProtectionScanEc2InstanceWithFindingsDetails
    AwsGuardDutyDetectorDataSourcesMalwareProtectionScanEc2InstanceWithFindingsDetails (..),
    newAwsGuardDutyDetectorDataSourcesMalwareProtectionScanEc2InstanceWithFindingsDetails,
    awsGuardDutyDetectorDataSourcesMalwareProtectionScanEc2InstanceWithFindingsDetails_ebsVolumes,

    -- * AwsGuardDutyDetectorDataSourcesMalwareProtectionScanEc2InstanceWithFindingsEbsVolumesDetails
    AwsGuardDutyDetectorDataSourcesMalwareProtectionScanEc2InstanceWithFindingsEbsVolumesDetails (..),
    newAwsGuardDutyDetectorDataSourcesMalwareProtectionScanEc2InstanceWithFindingsEbsVolumesDetails,
    awsGuardDutyDetectorDataSourcesMalwareProtectionScanEc2InstanceWithFindingsEbsVolumesDetails_reason,
    awsGuardDutyDetectorDataSourcesMalwareProtectionScanEc2InstanceWithFindingsEbsVolumesDetails_status,

    -- * AwsGuardDutyDetectorDataSourcesS3LogsDetails
    AwsGuardDutyDetectorDataSourcesS3LogsDetails (..),
    newAwsGuardDutyDetectorDataSourcesS3LogsDetails,
    awsGuardDutyDetectorDataSourcesS3LogsDetails_status,

    -- * AwsGuardDutyDetectorDetails
    AwsGuardDutyDetectorDetails (..),
    newAwsGuardDutyDetectorDetails,
    awsGuardDutyDetectorDetails_dataSources,
    awsGuardDutyDetectorDetails_features,
    awsGuardDutyDetectorDetails_findingPublishingFrequency,
    awsGuardDutyDetectorDetails_serviceRole,
    awsGuardDutyDetectorDetails_status,

    -- * AwsGuardDutyDetectorFeaturesDetails
    AwsGuardDutyDetectorFeaturesDetails (..),
    newAwsGuardDutyDetectorFeaturesDetails,
    awsGuardDutyDetectorFeaturesDetails_name,
    awsGuardDutyDetectorFeaturesDetails_status,

    -- * AwsIamAccessKeyDetails
    AwsIamAccessKeyDetails (..),
    newAwsIamAccessKeyDetails,
    awsIamAccessKeyDetails_accessKeyId,
    awsIamAccessKeyDetails_accountId,
    awsIamAccessKeyDetails_createdAt,
    awsIamAccessKeyDetails_principalId,
    awsIamAccessKeyDetails_principalName,
    awsIamAccessKeyDetails_principalType,
    awsIamAccessKeyDetails_sessionContext,
    awsIamAccessKeyDetails_status,
    awsIamAccessKeyDetails_userName,

    -- * AwsIamAccessKeySessionContext
    AwsIamAccessKeySessionContext (..),
    newAwsIamAccessKeySessionContext,
    awsIamAccessKeySessionContext_attributes,
    awsIamAccessKeySessionContext_sessionIssuer,

    -- * AwsIamAccessKeySessionContextAttributes
    AwsIamAccessKeySessionContextAttributes (..),
    newAwsIamAccessKeySessionContextAttributes,
    awsIamAccessKeySessionContextAttributes_creationDate,
    awsIamAccessKeySessionContextAttributes_mfaAuthenticated,

    -- * AwsIamAccessKeySessionContextSessionIssuer
    AwsIamAccessKeySessionContextSessionIssuer (..),
    newAwsIamAccessKeySessionContextSessionIssuer,
    awsIamAccessKeySessionContextSessionIssuer_accountId,
    awsIamAccessKeySessionContextSessionIssuer_arn,
    awsIamAccessKeySessionContextSessionIssuer_principalId,
    awsIamAccessKeySessionContextSessionIssuer_type,
    awsIamAccessKeySessionContextSessionIssuer_userName,

    -- * AwsIamAttachedManagedPolicy
    AwsIamAttachedManagedPolicy (..),
    newAwsIamAttachedManagedPolicy,
    awsIamAttachedManagedPolicy_policyArn,
    awsIamAttachedManagedPolicy_policyName,

    -- * AwsIamGroupDetails
    AwsIamGroupDetails (..),
    newAwsIamGroupDetails,
    awsIamGroupDetails_attachedManagedPolicies,
    awsIamGroupDetails_createDate,
    awsIamGroupDetails_groupId,
    awsIamGroupDetails_groupName,
    awsIamGroupDetails_groupPolicyList,
    awsIamGroupDetails_path,

    -- * AwsIamGroupPolicy
    AwsIamGroupPolicy (..),
    newAwsIamGroupPolicy,
    awsIamGroupPolicy_policyName,

    -- * AwsIamInstanceProfile
    AwsIamInstanceProfile (..),
    newAwsIamInstanceProfile,
    awsIamInstanceProfile_arn,
    awsIamInstanceProfile_createDate,
    awsIamInstanceProfile_instanceProfileId,
    awsIamInstanceProfile_instanceProfileName,
    awsIamInstanceProfile_path,
    awsIamInstanceProfile_roles,

    -- * AwsIamInstanceProfileRole
    AwsIamInstanceProfileRole (..),
    newAwsIamInstanceProfileRole,
    awsIamInstanceProfileRole_arn,
    awsIamInstanceProfileRole_assumeRolePolicyDocument,
    awsIamInstanceProfileRole_createDate,
    awsIamInstanceProfileRole_path,
    awsIamInstanceProfileRole_roleId,
    awsIamInstanceProfileRole_roleName,

    -- * AwsIamPermissionsBoundary
    AwsIamPermissionsBoundary (..),
    newAwsIamPermissionsBoundary,
    awsIamPermissionsBoundary_permissionsBoundaryArn,
    awsIamPermissionsBoundary_permissionsBoundaryType,

    -- * AwsIamPolicyDetails
    AwsIamPolicyDetails (..),
    newAwsIamPolicyDetails,
    awsIamPolicyDetails_attachmentCount,
    awsIamPolicyDetails_createDate,
    awsIamPolicyDetails_defaultVersionId,
    awsIamPolicyDetails_description,
    awsIamPolicyDetails_isAttachable,
    awsIamPolicyDetails_path,
    awsIamPolicyDetails_permissionsBoundaryUsageCount,
    awsIamPolicyDetails_policyId,
    awsIamPolicyDetails_policyName,
    awsIamPolicyDetails_policyVersionList,
    awsIamPolicyDetails_updateDate,

    -- * AwsIamPolicyVersion
    AwsIamPolicyVersion (..),
    newAwsIamPolicyVersion,
    awsIamPolicyVersion_createDate,
    awsIamPolicyVersion_isDefaultVersion,
    awsIamPolicyVersion_versionId,

    -- * AwsIamRoleDetails
    AwsIamRoleDetails (..),
    newAwsIamRoleDetails,
    awsIamRoleDetails_assumeRolePolicyDocument,
    awsIamRoleDetails_attachedManagedPolicies,
    awsIamRoleDetails_createDate,
    awsIamRoleDetails_instanceProfileList,
    awsIamRoleDetails_maxSessionDuration,
    awsIamRoleDetails_path,
    awsIamRoleDetails_permissionsBoundary,
    awsIamRoleDetails_roleId,
    awsIamRoleDetails_roleName,
    awsIamRoleDetails_rolePolicyList,

    -- * AwsIamRolePolicy
    AwsIamRolePolicy (..),
    newAwsIamRolePolicy,
    awsIamRolePolicy_policyName,

    -- * AwsIamUserDetails
    AwsIamUserDetails (..),
    newAwsIamUserDetails,
    awsIamUserDetails_attachedManagedPolicies,
    awsIamUserDetails_createDate,
    awsIamUserDetails_groupList,
    awsIamUserDetails_path,
    awsIamUserDetails_permissionsBoundary,
    awsIamUserDetails_userId,
    awsIamUserDetails_userName,
    awsIamUserDetails_userPolicyList,

    -- * AwsIamUserPolicy
    AwsIamUserPolicy (..),
    newAwsIamUserPolicy,
    awsIamUserPolicy_policyName,

    -- * AwsKinesisStreamDetails
    AwsKinesisStreamDetails (..),
    newAwsKinesisStreamDetails,
    awsKinesisStreamDetails_arn,
    awsKinesisStreamDetails_name,
    awsKinesisStreamDetails_retentionPeriodHours,
    awsKinesisStreamDetails_shardCount,
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
    awsKmsKeyDetails_keyId,
    awsKmsKeyDetails_keyManager,
    awsKmsKeyDetails_keyRotationStatus,
    awsKmsKeyDetails_keyState,
    awsKmsKeyDetails_origin,

    -- * AwsLambdaFunctionCode
    AwsLambdaFunctionCode (..),
    newAwsLambdaFunctionCode,
    awsLambdaFunctionCode_s3Bucket,
    awsLambdaFunctionCode_s3Key,
    awsLambdaFunctionCode_s3ObjectVersion,
    awsLambdaFunctionCode_zipFile,

    -- * AwsLambdaFunctionDeadLetterConfig
    AwsLambdaFunctionDeadLetterConfig (..),
    newAwsLambdaFunctionDeadLetterConfig,
    awsLambdaFunctionDeadLetterConfig_targetArn,

    -- * AwsLambdaFunctionDetails
    AwsLambdaFunctionDetails (..),
    newAwsLambdaFunctionDetails,
    awsLambdaFunctionDetails_architectures,
    awsLambdaFunctionDetails_code,
    awsLambdaFunctionDetails_codeSha256,
    awsLambdaFunctionDetails_deadLetterConfig,
    awsLambdaFunctionDetails_environment,
    awsLambdaFunctionDetails_functionName,
    awsLambdaFunctionDetails_handler,
    awsLambdaFunctionDetails_kmsKeyArn,
    awsLambdaFunctionDetails_lastModified,
    awsLambdaFunctionDetails_layers,
    awsLambdaFunctionDetails_masterArn,
    awsLambdaFunctionDetails_memorySize,
    awsLambdaFunctionDetails_packageType,
    awsLambdaFunctionDetails_revisionId,
    awsLambdaFunctionDetails_role,
    awsLambdaFunctionDetails_runtime,
    awsLambdaFunctionDetails_timeout,
    awsLambdaFunctionDetails_tracingConfig,
    awsLambdaFunctionDetails_version,
    awsLambdaFunctionDetails_vpcConfig,

    -- * AwsLambdaFunctionEnvironment
    AwsLambdaFunctionEnvironment (..),
    newAwsLambdaFunctionEnvironment,
    awsLambdaFunctionEnvironment_error,
    awsLambdaFunctionEnvironment_variables,

    -- * AwsLambdaFunctionEnvironmentError
    AwsLambdaFunctionEnvironmentError (..),
    newAwsLambdaFunctionEnvironmentError,
    awsLambdaFunctionEnvironmentError_errorCode,
    awsLambdaFunctionEnvironmentError_message,

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
    awsLambdaFunctionVpcConfig_subnetIds,
    awsLambdaFunctionVpcConfig_vpcId,

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
    awsNetworkFirewallFirewallDetails_description,
    awsNetworkFirewallFirewallDetails_firewallArn,
    awsNetworkFirewallFirewallDetails_firewallId,
    awsNetworkFirewallFirewallDetails_firewallName,
    awsNetworkFirewallFirewallDetails_firewallPolicyArn,
    awsNetworkFirewallFirewallDetails_firewallPolicyChangeProtection,
    awsNetworkFirewallFirewallDetails_subnetChangeProtection,
    awsNetworkFirewallFirewallDetails_subnetMappings,
    awsNetworkFirewallFirewallDetails_vpcId,

    -- * AwsNetworkFirewallFirewallPolicyDetails
    AwsNetworkFirewallFirewallPolicyDetails (..),
    newAwsNetworkFirewallFirewallPolicyDetails,
    awsNetworkFirewallFirewallPolicyDetails_description,
    awsNetworkFirewallFirewallPolicyDetails_firewallPolicy,
    awsNetworkFirewallFirewallPolicyDetails_firewallPolicyArn,
    awsNetworkFirewallFirewallPolicyDetails_firewallPolicyId,
    awsNetworkFirewallFirewallPolicyDetails_firewallPolicyName,

    -- * AwsNetworkFirewallFirewallSubnetMappingsDetails
    AwsNetworkFirewallFirewallSubnetMappingsDetails (..),
    newAwsNetworkFirewallFirewallSubnetMappingsDetails,
    awsNetworkFirewallFirewallSubnetMappingsDetails_subnetId,

    -- * AwsNetworkFirewallRuleGroupDetails
    AwsNetworkFirewallRuleGroupDetails (..),
    newAwsNetworkFirewallRuleGroupDetails,
    awsNetworkFirewallRuleGroupDetails_capacity,
    awsNetworkFirewallRuleGroupDetails_description,
    awsNetworkFirewallRuleGroupDetails_ruleGroup,
    awsNetworkFirewallRuleGroupDetails_ruleGroupArn,
    awsNetworkFirewallRuleGroupDetails_ruleGroupId,
    awsNetworkFirewallRuleGroupDetails_ruleGroupName,
    awsNetworkFirewallRuleGroupDetails_type,

    -- * AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails
    AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails (..),
    newAwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails,
    awsOpenSearchServiceDomainAdvancedSecurityOptionsDetails_enabled,
    awsOpenSearchServiceDomainAdvancedSecurityOptionsDetails_internalUserDatabaseEnabled,
    awsOpenSearchServiceDomainAdvancedSecurityOptionsDetails_masterUserOptions,

    -- * AwsOpenSearchServiceDomainClusterConfigDetails
    AwsOpenSearchServiceDomainClusterConfigDetails (..),
    newAwsOpenSearchServiceDomainClusterConfigDetails,
    awsOpenSearchServiceDomainClusterConfigDetails_dedicatedMasterCount,
    awsOpenSearchServiceDomainClusterConfigDetails_dedicatedMasterEnabled,
    awsOpenSearchServiceDomainClusterConfigDetails_dedicatedMasterType,
    awsOpenSearchServiceDomainClusterConfigDetails_instanceCount,
    awsOpenSearchServiceDomainClusterConfigDetails_instanceType,
    awsOpenSearchServiceDomainClusterConfigDetails_warmCount,
    awsOpenSearchServiceDomainClusterConfigDetails_warmEnabled,
    awsOpenSearchServiceDomainClusterConfigDetails_warmType,
    awsOpenSearchServiceDomainClusterConfigDetails_zoneAwarenessConfig,
    awsOpenSearchServiceDomainClusterConfigDetails_zoneAwarenessEnabled,

    -- * AwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails
    AwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails (..),
    newAwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails,
    awsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails_availabilityZoneCount,

    -- * AwsOpenSearchServiceDomainDetails
    AwsOpenSearchServiceDomainDetails (..),
    newAwsOpenSearchServiceDomainDetails,
    awsOpenSearchServiceDomainDetails_accessPolicies,
    awsOpenSearchServiceDomainDetails_advancedSecurityOptions,
    awsOpenSearchServiceDomainDetails_arn,
    awsOpenSearchServiceDomainDetails_clusterConfig,
    awsOpenSearchServiceDomainDetails_domainEndpoint,
    awsOpenSearchServiceDomainDetails_domainEndpointOptions,
    awsOpenSearchServiceDomainDetails_domainEndpoints,
    awsOpenSearchServiceDomainDetails_domainName,
    awsOpenSearchServiceDomainDetails_encryptionAtRestOptions,
    awsOpenSearchServiceDomainDetails_engineVersion,
    awsOpenSearchServiceDomainDetails_id,
    awsOpenSearchServiceDomainDetails_logPublishingOptions,
    awsOpenSearchServiceDomainDetails_nodeToNodeEncryptionOptions,
    awsOpenSearchServiceDomainDetails_serviceSoftwareOptions,
    awsOpenSearchServiceDomainDetails_vpcOptions,

    -- * AwsOpenSearchServiceDomainDomainEndpointOptionsDetails
    AwsOpenSearchServiceDomainDomainEndpointOptionsDetails (..),
    newAwsOpenSearchServiceDomainDomainEndpointOptionsDetails,
    awsOpenSearchServiceDomainDomainEndpointOptionsDetails_customEndpoint,
    awsOpenSearchServiceDomainDomainEndpointOptionsDetails_customEndpointCertificateArn,
    awsOpenSearchServiceDomainDomainEndpointOptionsDetails_customEndpointEnabled,
    awsOpenSearchServiceDomainDomainEndpointOptionsDetails_enforceHTTPS,
    awsOpenSearchServiceDomainDomainEndpointOptionsDetails_tLSSecurityPolicy,

    -- * AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails
    AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails (..),
    newAwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails,
    awsOpenSearchServiceDomainEncryptionAtRestOptionsDetails_enabled,
    awsOpenSearchServiceDomainEncryptionAtRestOptionsDetails_kmsKeyId,

    -- * AwsOpenSearchServiceDomainLogPublishingOption
    AwsOpenSearchServiceDomainLogPublishingOption (..),
    newAwsOpenSearchServiceDomainLogPublishingOption,
    awsOpenSearchServiceDomainLogPublishingOption_cloudWatchLogsLogGroupArn,
    awsOpenSearchServiceDomainLogPublishingOption_enabled,

    -- * AwsOpenSearchServiceDomainLogPublishingOptionsDetails
    AwsOpenSearchServiceDomainLogPublishingOptionsDetails (..),
    newAwsOpenSearchServiceDomainLogPublishingOptionsDetails,
    awsOpenSearchServiceDomainLogPublishingOptionsDetails_auditLogs,
    awsOpenSearchServiceDomainLogPublishingOptionsDetails_indexSlowLogs,
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
    awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_automatedUpdateDate,
    awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_cancellable,
    awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_currentVersion,
    awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_description,
    awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_newVersion,
    awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_optionalDeployment,
    awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_updateAvailable,
    awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_updateStatus,

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
    awsRdsDbClusterDetails_activityStreamStatus,
    awsRdsDbClusterDetails_allocatedStorage,
    awsRdsDbClusterDetails_associatedRoles,
    awsRdsDbClusterDetails_availabilityZones,
    awsRdsDbClusterDetails_backupRetentionPeriod,
    awsRdsDbClusterDetails_clusterCreateTime,
    awsRdsDbClusterDetails_copyTagsToSnapshot,
    awsRdsDbClusterDetails_crossAccountClone,
    awsRdsDbClusterDetails_customEndpoints,
    awsRdsDbClusterDetails_databaseName,
    awsRdsDbClusterDetails_dbClusterIdentifier,
    awsRdsDbClusterDetails_dbClusterMembers,
    awsRdsDbClusterDetails_dbClusterOptionGroupMemberships,
    awsRdsDbClusterDetails_dbClusterParameterGroup,
    awsRdsDbClusterDetails_dbClusterResourceId,
    awsRdsDbClusterDetails_dbSubnetGroup,
    awsRdsDbClusterDetails_deletionProtection,
    awsRdsDbClusterDetails_domainMemberships,
    awsRdsDbClusterDetails_enabledCloudWatchLogsExports,
    awsRdsDbClusterDetails_endpoint,
    awsRdsDbClusterDetails_engine,
    awsRdsDbClusterDetails_engineMode,
    awsRdsDbClusterDetails_engineVersion,
    awsRdsDbClusterDetails_hostedZoneId,
    awsRdsDbClusterDetails_httpEndpointEnabled,
    awsRdsDbClusterDetails_iamDatabaseAuthenticationEnabled,
    awsRdsDbClusterDetails_kmsKeyId,
    awsRdsDbClusterDetails_masterUsername,
    awsRdsDbClusterDetails_multiAz,
    awsRdsDbClusterDetails_port,
    awsRdsDbClusterDetails_preferredBackupWindow,
    awsRdsDbClusterDetails_preferredMaintenanceWindow,
    awsRdsDbClusterDetails_readReplicaIdentifiers,
    awsRdsDbClusterDetails_readerEndpoint,
    awsRdsDbClusterDetails_status,
    awsRdsDbClusterDetails_storageEncrypted,
    awsRdsDbClusterDetails_vpcSecurityGroups,

    -- * AwsRdsDbClusterMember
    AwsRdsDbClusterMember (..),
    newAwsRdsDbClusterMember,
    awsRdsDbClusterMember_dbClusterParameterGroupStatus,
    awsRdsDbClusterMember_dbInstanceIdentifier,
    awsRdsDbClusterMember_isClusterWriter,
    awsRdsDbClusterMember_promotionTier,

    -- * AwsRdsDbClusterOptionGroupMembership
    AwsRdsDbClusterOptionGroupMembership (..),
    newAwsRdsDbClusterOptionGroupMembership,
    awsRdsDbClusterOptionGroupMembership_dbClusterOptionGroupName,
    awsRdsDbClusterOptionGroupMembership_status,

    -- * AwsRdsDbClusterSnapshotDetails
    AwsRdsDbClusterSnapshotDetails (..),
    newAwsRdsDbClusterSnapshotDetails,
    awsRdsDbClusterSnapshotDetails_allocatedStorage,
    awsRdsDbClusterSnapshotDetails_availabilityZones,
    awsRdsDbClusterSnapshotDetails_clusterCreateTime,
    awsRdsDbClusterSnapshotDetails_dbClusterIdentifier,
    awsRdsDbClusterSnapshotDetails_dbClusterSnapshotIdentifier,
    awsRdsDbClusterSnapshotDetails_engine,
    awsRdsDbClusterSnapshotDetails_engineVersion,
    awsRdsDbClusterSnapshotDetails_iamDatabaseAuthenticationEnabled,
    awsRdsDbClusterSnapshotDetails_kmsKeyId,
    awsRdsDbClusterSnapshotDetails_licenseModel,
    awsRdsDbClusterSnapshotDetails_masterUsername,
    awsRdsDbClusterSnapshotDetails_percentProgress,
    awsRdsDbClusterSnapshotDetails_port,
    awsRdsDbClusterSnapshotDetails_snapshotCreateTime,
    awsRdsDbClusterSnapshotDetails_snapshotType,
    awsRdsDbClusterSnapshotDetails_status,
    awsRdsDbClusterSnapshotDetails_storageEncrypted,
    awsRdsDbClusterSnapshotDetails_vpcId,

    -- * AwsRdsDbDomainMembership
    AwsRdsDbDomainMembership (..),
    newAwsRdsDbDomainMembership,
    awsRdsDbDomainMembership_domain,
    awsRdsDbDomainMembership_fqdn,
    awsRdsDbDomainMembership_iamRoleName,
    awsRdsDbDomainMembership_status,

    -- * AwsRdsDbInstanceAssociatedRole
    AwsRdsDbInstanceAssociatedRole (..),
    newAwsRdsDbInstanceAssociatedRole,
    awsRdsDbInstanceAssociatedRole_featureName,
    awsRdsDbInstanceAssociatedRole_roleArn,
    awsRdsDbInstanceAssociatedRole_status,

    -- * AwsRdsDbInstanceDetails
    AwsRdsDbInstanceDetails (..),
    newAwsRdsDbInstanceDetails,
    awsRdsDbInstanceDetails_allocatedStorage,
    awsRdsDbInstanceDetails_associatedRoles,
    awsRdsDbInstanceDetails_autoMinorVersionUpgrade,
    awsRdsDbInstanceDetails_availabilityZone,
    awsRdsDbInstanceDetails_backupRetentionPeriod,
    awsRdsDbInstanceDetails_cACertificateIdentifier,
    awsRdsDbInstanceDetails_characterSetName,
    awsRdsDbInstanceDetails_copyTagsToSnapshot,
    awsRdsDbInstanceDetails_dbClusterIdentifier,
    awsRdsDbInstanceDetails_dbInstanceClass,
    awsRdsDbInstanceDetails_dbInstanceIdentifier,
    awsRdsDbInstanceDetails_dbName,
    awsRdsDbInstanceDetails_dbInstancePort,
    awsRdsDbInstanceDetails_dbInstanceStatus,
    awsRdsDbInstanceDetails_dbParameterGroups,
    awsRdsDbInstanceDetails_dbSecurityGroups,
    awsRdsDbInstanceDetails_dbSubnetGroup,
    awsRdsDbInstanceDetails_dbiResourceId,
    awsRdsDbInstanceDetails_deletionProtection,
    awsRdsDbInstanceDetails_domainMemberships,
    awsRdsDbInstanceDetails_enabledCloudWatchLogsExports,
    awsRdsDbInstanceDetails_endpoint,
    awsRdsDbInstanceDetails_engine,
    awsRdsDbInstanceDetails_engineVersion,
    awsRdsDbInstanceDetails_enhancedMonitoringResourceArn,
    awsRdsDbInstanceDetails_iAMDatabaseAuthenticationEnabled,
    awsRdsDbInstanceDetails_instanceCreateTime,
    awsRdsDbInstanceDetails_iops,
    awsRdsDbInstanceDetails_kmsKeyId,
    awsRdsDbInstanceDetails_latestRestorableTime,
    awsRdsDbInstanceDetails_licenseModel,
    awsRdsDbInstanceDetails_listenerEndpoint,
    awsRdsDbInstanceDetails_masterUsername,
    awsRdsDbInstanceDetails_maxAllocatedStorage,
    awsRdsDbInstanceDetails_monitoringInterval,
    awsRdsDbInstanceDetails_monitoringRoleArn,
    awsRdsDbInstanceDetails_multiAz,
    awsRdsDbInstanceDetails_optionGroupMemberships,
    awsRdsDbInstanceDetails_pendingModifiedValues,
    awsRdsDbInstanceDetails_performanceInsightsEnabled,
    awsRdsDbInstanceDetails_performanceInsightsKmsKeyId,
    awsRdsDbInstanceDetails_performanceInsightsRetentionPeriod,
    awsRdsDbInstanceDetails_preferredBackupWindow,
    awsRdsDbInstanceDetails_preferredMaintenanceWindow,
    awsRdsDbInstanceDetails_processorFeatures,
    awsRdsDbInstanceDetails_promotionTier,
    awsRdsDbInstanceDetails_publiclyAccessible,
    awsRdsDbInstanceDetails_readReplicaDBClusterIdentifiers,
    awsRdsDbInstanceDetails_readReplicaDBInstanceIdentifiers,
    awsRdsDbInstanceDetails_readReplicaSourceDBInstanceIdentifier,
    awsRdsDbInstanceDetails_secondaryAvailabilityZone,
    awsRdsDbInstanceDetails_statusInfos,
    awsRdsDbInstanceDetails_storageEncrypted,
    awsRdsDbInstanceDetails_storageType,
    awsRdsDbInstanceDetails_tdeCredentialArn,
    awsRdsDbInstanceDetails_timezone,
    awsRdsDbInstanceDetails_vpcSecurityGroups,

    -- * AwsRdsDbInstanceEndpoint
    AwsRdsDbInstanceEndpoint (..),
    newAwsRdsDbInstanceEndpoint,
    awsRdsDbInstanceEndpoint_address,
    awsRdsDbInstanceEndpoint_hostedZoneId,
    awsRdsDbInstanceEndpoint_port,

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
    awsRdsDbPendingModifiedValues_allocatedStorage,
    awsRdsDbPendingModifiedValues_backupRetentionPeriod,
    awsRdsDbPendingModifiedValues_caCertificateIdentifier,
    awsRdsDbPendingModifiedValues_dbInstanceClass,
    awsRdsDbPendingModifiedValues_dbInstanceIdentifier,
    awsRdsDbPendingModifiedValues_dbSubnetGroupName,
    awsRdsDbPendingModifiedValues_engineVersion,
    awsRdsDbPendingModifiedValues_iops,
    awsRdsDbPendingModifiedValues_licenseModel,
    awsRdsDbPendingModifiedValues_masterUserPassword,
    awsRdsDbPendingModifiedValues_multiAZ,
    awsRdsDbPendingModifiedValues_pendingCloudWatchLogsExports,
    awsRdsDbPendingModifiedValues_port,
    awsRdsDbPendingModifiedValues_processorFeatures,
    awsRdsDbPendingModifiedValues_storageType,

    -- * AwsRdsDbProcessorFeature
    AwsRdsDbProcessorFeature (..),
    newAwsRdsDbProcessorFeature,
    awsRdsDbProcessorFeature_name,
    awsRdsDbProcessorFeature_value,

    -- * AwsRdsDbSecurityGroupDetails
    AwsRdsDbSecurityGroupDetails (..),
    newAwsRdsDbSecurityGroupDetails,
    awsRdsDbSecurityGroupDetails_dbSecurityGroupArn,
    awsRdsDbSecurityGroupDetails_dbSecurityGroupDescription,
    awsRdsDbSecurityGroupDetails_dbSecurityGroupName,
    awsRdsDbSecurityGroupDetails_ec2SecurityGroups,
    awsRdsDbSecurityGroupDetails_ipRanges,
    awsRdsDbSecurityGroupDetails_ownerId,
    awsRdsDbSecurityGroupDetails_vpcId,

    -- * AwsRdsDbSecurityGroupEc2SecurityGroup
    AwsRdsDbSecurityGroupEc2SecurityGroup (..),
    newAwsRdsDbSecurityGroupEc2SecurityGroup,
    awsRdsDbSecurityGroupEc2SecurityGroup_ec2SecurityGroupId,
    awsRdsDbSecurityGroupEc2SecurityGroup_ec2SecurityGroupName,
    awsRdsDbSecurityGroupEc2SecurityGroup_ec2SecurityGroupOwnerId,
    awsRdsDbSecurityGroupEc2SecurityGroup_status,

    -- * AwsRdsDbSecurityGroupIpRange
    AwsRdsDbSecurityGroupIpRange (..),
    newAwsRdsDbSecurityGroupIpRange,
    awsRdsDbSecurityGroupIpRange_cidrIp,
    awsRdsDbSecurityGroupIpRange_status,

    -- * AwsRdsDbSnapshotDetails
    AwsRdsDbSnapshotDetails (..),
    newAwsRdsDbSnapshotDetails,
    awsRdsDbSnapshotDetails_allocatedStorage,
    awsRdsDbSnapshotDetails_availabilityZone,
    awsRdsDbSnapshotDetails_dbInstanceIdentifier,
    awsRdsDbSnapshotDetails_dbSnapshotIdentifier,
    awsRdsDbSnapshotDetails_dbiResourceId,
    awsRdsDbSnapshotDetails_encrypted,
    awsRdsDbSnapshotDetails_engine,
    awsRdsDbSnapshotDetails_engineVersion,
    awsRdsDbSnapshotDetails_iamDatabaseAuthenticationEnabled,
    awsRdsDbSnapshotDetails_instanceCreateTime,
    awsRdsDbSnapshotDetails_iops,
    awsRdsDbSnapshotDetails_kmsKeyId,
    awsRdsDbSnapshotDetails_licenseModel,
    awsRdsDbSnapshotDetails_masterUsername,
    awsRdsDbSnapshotDetails_optionGroupName,
    awsRdsDbSnapshotDetails_percentProgress,
    awsRdsDbSnapshotDetails_port,
    awsRdsDbSnapshotDetails_processorFeatures,
    awsRdsDbSnapshotDetails_snapshotCreateTime,
    awsRdsDbSnapshotDetails_snapshotType,
    awsRdsDbSnapshotDetails_sourceDbSnapshotIdentifier,
    awsRdsDbSnapshotDetails_sourceRegion,
    awsRdsDbSnapshotDetails_status,
    awsRdsDbSnapshotDetails_storageType,
    awsRdsDbSnapshotDetails_tdeCredentialArn,
    awsRdsDbSnapshotDetails_timezone,
    awsRdsDbSnapshotDetails_vpcId,

    -- * AwsRdsDbStatusInfo
    AwsRdsDbStatusInfo (..),
    newAwsRdsDbStatusInfo,
    awsRdsDbStatusInfo_message,
    awsRdsDbStatusInfo_normal,
    awsRdsDbStatusInfo_status,
    awsRdsDbStatusInfo_statusType,

    -- * AwsRdsDbSubnetGroup
    AwsRdsDbSubnetGroup (..),
    newAwsRdsDbSubnetGroup,
    awsRdsDbSubnetGroup_dbSubnetGroupArn,
    awsRdsDbSubnetGroup_dbSubnetGroupDescription,
    awsRdsDbSubnetGroup_dbSubnetGroupName,
    awsRdsDbSubnetGroup_subnetGroupStatus,
    awsRdsDbSubnetGroup_subnets,
    awsRdsDbSubnetGroup_vpcId,

    -- * AwsRdsDbSubnetGroupSubnet
    AwsRdsDbSubnetGroupSubnet (..),
    newAwsRdsDbSubnetGroupSubnet,
    awsRdsDbSubnetGroupSubnet_subnetAvailabilityZone,
    awsRdsDbSubnetGroupSubnet_subnetIdentifier,
    awsRdsDbSubnetGroupSubnet_subnetStatus,

    -- * AwsRdsDbSubnetGroupSubnetAvailabilityZone
    AwsRdsDbSubnetGroupSubnetAvailabilityZone (..),
    newAwsRdsDbSubnetGroupSubnetAvailabilityZone,
    awsRdsDbSubnetGroupSubnetAvailabilityZone_name,

    -- * AwsRdsEventSubscriptionDetails
    AwsRdsEventSubscriptionDetails (..),
    newAwsRdsEventSubscriptionDetails,
    awsRdsEventSubscriptionDetails_custSubscriptionId,
    awsRdsEventSubscriptionDetails_customerAwsId,
    awsRdsEventSubscriptionDetails_enabled,
    awsRdsEventSubscriptionDetails_eventCategoriesList,
    awsRdsEventSubscriptionDetails_eventSubscriptionArn,
    awsRdsEventSubscriptionDetails_snsTopicArn,
    awsRdsEventSubscriptionDetails_sourceIdsList,
    awsRdsEventSubscriptionDetails_sourceType,
    awsRdsEventSubscriptionDetails_status,
    awsRdsEventSubscriptionDetails_subscriptionCreationTime,

    -- * AwsRdsPendingCloudWatchLogsExports
    AwsRdsPendingCloudWatchLogsExports (..),
    newAwsRdsPendingCloudWatchLogsExports,
    awsRdsPendingCloudWatchLogsExports_logTypesToDisable,
    awsRdsPendingCloudWatchLogsExports_logTypesToEnable,

    -- * AwsRedshiftClusterClusterNode
    AwsRedshiftClusterClusterNode (..),
    newAwsRedshiftClusterClusterNode,
    awsRedshiftClusterClusterNode_nodeRole,
    awsRedshiftClusterClusterNode_privateIpAddress,
    awsRedshiftClusterClusterNode_publicIpAddress,

    -- * AwsRedshiftClusterClusterParameterGroup
    AwsRedshiftClusterClusterParameterGroup (..),
    newAwsRedshiftClusterClusterParameterGroup,
    awsRedshiftClusterClusterParameterGroup_clusterParameterStatusList,
    awsRedshiftClusterClusterParameterGroup_parameterApplyStatus,
    awsRedshiftClusterClusterParameterGroup_parameterGroupName,

    -- * AwsRedshiftClusterClusterParameterStatus
    AwsRedshiftClusterClusterParameterStatus (..),
    newAwsRedshiftClusterClusterParameterStatus,
    awsRedshiftClusterClusterParameterStatus_parameterApplyErrorDescription,
    awsRedshiftClusterClusterParameterStatus_parameterApplyStatus,
    awsRedshiftClusterClusterParameterStatus_parameterName,

    -- * AwsRedshiftClusterClusterSecurityGroup
    AwsRedshiftClusterClusterSecurityGroup (..),
    newAwsRedshiftClusterClusterSecurityGroup,
    awsRedshiftClusterClusterSecurityGroup_clusterSecurityGroupName,
    awsRedshiftClusterClusterSecurityGroup_status,

    -- * AwsRedshiftClusterClusterSnapshotCopyStatus
    AwsRedshiftClusterClusterSnapshotCopyStatus (..),
    newAwsRedshiftClusterClusterSnapshotCopyStatus,
    awsRedshiftClusterClusterSnapshotCopyStatus_destinationRegion,
    awsRedshiftClusterClusterSnapshotCopyStatus_manualSnapshotRetentionPeriod,
    awsRedshiftClusterClusterSnapshotCopyStatus_retentionPeriod,
    awsRedshiftClusterClusterSnapshotCopyStatus_snapshotCopyGrantName,

    -- * AwsRedshiftClusterDeferredMaintenanceWindow
    AwsRedshiftClusterDeferredMaintenanceWindow (..),
    newAwsRedshiftClusterDeferredMaintenanceWindow,
    awsRedshiftClusterDeferredMaintenanceWindow_deferMaintenanceEndTime,
    awsRedshiftClusterDeferredMaintenanceWindow_deferMaintenanceIdentifier,
    awsRedshiftClusterDeferredMaintenanceWindow_deferMaintenanceStartTime,

    -- * AwsRedshiftClusterDetails
    AwsRedshiftClusterDetails (..),
    newAwsRedshiftClusterDetails,
    awsRedshiftClusterDetails_allowVersionUpgrade,
    awsRedshiftClusterDetails_automatedSnapshotRetentionPeriod,
    awsRedshiftClusterDetails_availabilityZone,
    awsRedshiftClusterDetails_clusterAvailabilityStatus,
    awsRedshiftClusterDetails_clusterCreateTime,
    awsRedshiftClusterDetails_clusterIdentifier,
    awsRedshiftClusterDetails_clusterNodes,
    awsRedshiftClusterDetails_clusterParameterGroups,
    awsRedshiftClusterDetails_clusterPublicKey,
    awsRedshiftClusterDetails_clusterRevisionNumber,
    awsRedshiftClusterDetails_clusterSecurityGroups,
    awsRedshiftClusterDetails_clusterSnapshotCopyStatus,
    awsRedshiftClusterDetails_clusterStatus,
    awsRedshiftClusterDetails_clusterSubnetGroupName,
    awsRedshiftClusterDetails_clusterVersion,
    awsRedshiftClusterDetails_dbName,
    awsRedshiftClusterDetails_deferredMaintenanceWindows,
    awsRedshiftClusterDetails_elasticIpStatus,
    awsRedshiftClusterDetails_elasticResizeNumberOfNodeOptions,
    awsRedshiftClusterDetails_encrypted,
    awsRedshiftClusterDetails_endpoint,
    awsRedshiftClusterDetails_enhancedVpcRouting,
    awsRedshiftClusterDetails_expectedNextSnapshotScheduleTime,
    awsRedshiftClusterDetails_expectedNextSnapshotScheduleTimeStatus,
    awsRedshiftClusterDetails_hsmStatus,
    awsRedshiftClusterDetails_iamRoles,
    awsRedshiftClusterDetails_kmsKeyId,
    awsRedshiftClusterDetails_loggingStatus,
    awsRedshiftClusterDetails_maintenanceTrackName,
    awsRedshiftClusterDetails_manualSnapshotRetentionPeriod,
    awsRedshiftClusterDetails_masterUsername,
    awsRedshiftClusterDetails_nextMaintenanceWindowStartTime,
    awsRedshiftClusterDetails_nodeType,
    awsRedshiftClusterDetails_numberOfNodes,
    awsRedshiftClusterDetails_pendingActions,
    awsRedshiftClusterDetails_pendingModifiedValues,
    awsRedshiftClusterDetails_preferredMaintenanceWindow,
    awsRedshiftClusterDetails_publiclyAccessible,
    awsRedshiftClusterDetails_resizeInfo,
    awsRedshiftClusterDetails_restoreStatus,
    awsRedshiftClusterDetails_snapshotScheduleIdentifier,
    awsRedshiftClusterDetails_snapshotScheduleState,
    awsRedshiftClusterDetails_vpcId,
    awsRedshiftClusterDetails_vpcSecurityGroups,

    -- * AwsRedshiftClusterElasticIpStatus
    AwsRedshiftClusterElasticIpStatus (..),
    newAwsRedshiftClusterElasticIpStatus,
    awsRedshiftClusterElasticIpStatus_elasticIp,
    awsRedshiftClusterElasticIpStatus_status,

    -- * AwsRedshiftClusterEndpoint
    AwsRedshiftClusterEndpoint (..),
    newAwsRedshiftClusterEndpoint,
    awsRedshiftClusterEndpoint_address,
    awsRedshiftClusterEndpoint_port,

    -- * AwsRedshiftClusterHsmStatus
    AwsRedshiftClusterHsmStatus (..),
    newAwsRedshiftClusterHsmStatus,
    awsRedshiftClusterHsmStatus_hsmClientCertificateIdentifier,
    awsRedshiftClusterHsmStatus_hsmConfigurationIdentifier,
    awsRedshiftClusterHsmStatus_status,

    -- * AwsRedshiftClusterIamRole
    AwsRedshiftClusterIamRole (..),
    newAwsRedshiftClusterIamRole,
    awsRedshiftClusterIamRole_applyStatus,
    awsRedshiftClusterIamRole_iamRoleArn,

    -- * AwsRedshiftClusterLoggingStatus
    AwsRedshiftClusterLoggingStatus (..),
    newAwsRedshiftClusterLoggingStatus,
    awsRedshiftClusterLoggingStatus_bucketName,
    awsRedshiftClusterLoggingStatus_lastFailureMessage,
    awsRedshiftClusterLoggingStatus_lastFailureTime,
    awsRedshiftClusterLoggingStatus_lastSuccessfulDeliveryTime,
    awsRedshiftClusterLoggingStatus_loggingEnabled,
    awsRedshiftClusterLoggingStatus_s3KeyPrefix,

    -- * AwsRedshiftClusterPendingModifiedValues
    AwsRedshiftClusterPendingModifiedValues (..),
    newAwsRedshiftClusterPendingModifiedValues,
    awsRedshiftClusterPendingModifiedValues_automatedSnapshotRetentionPeriod,
    awsRedshiftClusterPendingModifiedValues_clusterIdentifier,
    awsRedshiftClusterPendingModifiedValues_clusterType,
    awsRedshiftClusterPendingModifiedValues_clusterVersion,
    awsRedshiftClusterPendingModifiedValues_encryptionType,
    awsRedshiftClusterPendingModifiedValues_enhancedVpcRouting,
    awsRedshiftClusterPendingModifiedValues_maintenanceTrackName,
    awsRedshiftClusterPendingModifiedValues_masterUserPassword,
    awsRedshiftClusterPendingModifiedValues_nodeType,
    awsRedshiftClusterPendingModifiedValues_numberOfNodes,
    awsRedshiftClusterPendingModifiedValues_publiclyAccessible,

    -- * AwsRedshiftClusterResizeInfo
    AwsRedshiftClusterResizeInfo (..),
    newAwsRedshiftClusterResizeInfo,
    awsRedshiftClusterResizeInfo_allowCancelResize,
    awsRedshiftClusterResizeInfo_resizeType,

    -- * AwsRedshiftClusterRestoreStatus
    AwsRedshiftClusterRestoreStatus (..),
    newAwsRedshiftClusterRestoreStatus,
    awsRedshiftClusterRestoreStatus_currentRestoreRateInMegaBytesPerSecond,
    awsRedshiftClusterRestoreStatus_elapsedTimeInSeconds,
    awsRedshiftClusterRestoreStatus_estimatedTimeToCompletionInSeconds,
    awsRedshiftClusterRestoreStatus_progressInMegaBytes,
    awsRedshiftClusterRestoreStatus_snapshotSizeInMegaBytes,
    awsRedshiftClusterRestoreStatus_status,

    -- * AwsRedshiftClusterVpcSecurityGroup
    AwsRedshiftClusterVpcSecurityGroup (..),
    newAwsRedshiftClusterVpcSecurityGroup,
    awsRedshiftClusterVpcSecurityGroup_status,
    awsRedshiftClusterVpcSecurityGroup_vpcSecurityGroupId,

    -- * AwsS3AccountPublicAccessBlockDetails
    AwsS3AccountPublicAccessBlockDetails (..),
    newAwsS3AccountPublicAccessBlockDetails,
    awsS3AccountPublicAccessBlockDetails_blockPublicAcls,
    awsS3AccountPublicAccessBlockDetails_blockPublicPolicy,
    awsS3AccountPublicAccessBlockDetails_ignorePublicAcls,
    awsS3AccountPublicAccessBlockDetails_restrictPublicBuckets,

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
    awsS3BucketBucketLifecycleConfigurationRulesDetails_abortIncompleteMultipartUpload,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_expirationDate,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_expirationInDays,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_expiredObjectDeleteMarker,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_filter,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_id,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_noncurrentVersionExpirationInDays,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_noncurrentVersionTransitions,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_prefix,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_status,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_transitions,

    -- * AwsS3BucketBucketLifecycleConfigurationRulesFilterDetails
    AwsS3BucketBucketLifecycleConfigurationRulesFilterDetails (..),
    newAwsS3BucketBucketLifecycleConfigurationRulesFilterDetails,
    awsS3BucketBucketLifecycleConfigurationRulesFilterDetails_predicate,

    -- * AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails (..),
    newAwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails,
    awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_operands,
    awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_prefix,
    awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_tag,
    awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_type,

    -- * AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails (..),
    newAwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails,
    awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails_prefix,
    awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails_tag,
    awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails_type,

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
    awsS3BucketDetails_bucketLifecycleConfiguration,
    awsS3BucketDetails_bucketLoggingConfiguration,
    awsS3BucketDetails_bucketNotificationConfiguration,
    awsS3BucketDetails_bucketVersioningConfiguration,
    awsS3BucketDetails_bucketWebsiteConfiguration,
    awsS3BucketDetails_createdAt,
    awsS3BucketDetails_objectLockConfiguration,
    awsS3BucketDetails_ownerAccountId,
    awsS3BucketDetails_ownerId,
    awsS3BucketDetails_ownerName,
    awsS3BucketDetails_publicAccessBlockConfiguration,
    awsS3BucketDetails_serverSideEncryptionConfiguration,

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
    awsS3BucketNotificationConfigurationDetail_events,
    awsS3BucketNotificationConfigurationDetail_filter,
    awsS3BucketNotificationConfigurationDetail_type,

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

    -- * AwsS3BucketObjectLockConfiguration
    AwsS3BucketObjectLockConfiguration (..),
    newAwsS3BucketObjectLockConfiguration,
    awsS3BucketObjectLockConfiguration_objectLockEnabled,
    awsS3BucketObjectLockConfiguration_rule,

    -- * AwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails
    AwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails (..),
    newAwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails,
    awsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails_days,
    awsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails_mode,
    awsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails_years,

    -- * AwsS3BucketObjectLockConfigurationRuleDetails
    AwsS3BucketObjectLockConfigurationRuleDetails (..),
    newAwsS3BucketObjectLockConfigurationRuleDetails,
    awsS3BucketObjectLockConfigurationRuleDetails_defaultRetention,

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
    awsS3BucketWebsiteConfiguration_errorDocument,
    awsS3BucketWebsiteConfiguration_indexDocumentSuffix,
    awsS3BucketWebsiteConfiguration_redirectAllRequestsTo,
    awsS3BucketWebsiteConfiguration_routingRules,

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
    awsS3BucketWebsiteConfigurationRoutingRuleRedirect_httpRedirectCode,
    awsS3BucketWebsiteConfigurationRoutingRuleRedirect_protocol,
    awsS3BucketWebsiteConfigurationRoutingRuleRedirect_replaceKeyPrefixWith,
    awsS3BucketWebsiteConfigurationRoutingRuleRedirect_replaceKeyWith,

    -- * AwsS3ObjectDetails
    AwsS3ObjectDetails (..),
    newAwsS3ObjectDetails,
    awsS3ObjectDetails_contentType,
    awsS3ObjectDetails_eTag,
    awsS3ObjectDetails_lastModified,
    awsS3ObjectDetails_sSEKMSKeyId,
    awsS3ObjectDetails_serverSideEncryption,
    awsS3ObjectDetails_versionId,

    -- * AwsSageMakerNotebookInstanceDetails
    AwsSageMakerNotebookInstanceDetails (..),
    newAwsSageMakerNotebookInstanceDetails,
    awsSageMakerNotebookInstanceDetails_acceleratorTypes,
    awsSageMakerNotebookInstanceDetails_additionalCodeRepositories,
    awsSageMakerNotebookInstanceDetails_defaultCodeRepository,
    awsSageMakerNotebookInstanceDetails_directInternetAccess,
    awsSageMakerNotebookInstanceDetails_failureReason,
    awsSageMakerNotebookInstanceDetails_instanceMetadataServiceConfiguration,
    awsSageMakerNotebookInstanceDetails_instanceType,
    awsSageMakerNotebookInstanceDetails_kmsKeyId,
    awsSageMakerNotebookInstanceDetails_networkInterfaceId,
    awsSageMakerNotebookInstanceDetails_notebookInstanceArn,
    awsSageMakerNotebookInstanceDetails_notebookInstanceLifecycleConfigName,
    awsSageMakerNotebookInstanceDetails_notebookInstanceName,
    awsSageMakerNotebookInstanceDetails_notebookInstanceStatus,
    awsSageMakerNotebookInstanceDetails_platformIdentifier,
    awsSageMakerNotebookInstanceDetails_roleArn,
    awsSageMakerNotebookInstanceDetails_rootAccess,
    awsSageMakerNotebookInstanceDetails_securityGroups,
    awsSageMakerNotebookInstanceDetails_subnetId,
    awsSageMakerNotebookInstanceDetails_url,
    awsSageMakerNotebookInstanceDetails_volumeSizeInGB,

    -- * AwsSageMakerNotebookInstanceMetadataServiceConfigurationDetails
    AwsSageMakerNotebookInstanceMetadataServiceConfigurationDetails (..),
    newAwsSageMakerNotebookInstanceMetadataServiceConfigurationDetails,
    awsSageMakerNotebookInstanceMetadataServiceConfigurationDetails_minimumInstanceMetadataServiceVersion,

    -- * AwsSecretsManagerSecretDetails
    AwsSecretsManagerSecretDetails (..),
    newAwsSecretsManagerSecretDetails,
    awsSecretsManagerSecretDetails_deleted,
    awsSecretsManagerSecretDetails_description,
    awsSecretsManagerSecretDetails_kmsKeyId,
    awsSecretsManagerSecretDetails_name,
    awsSecretsManagerSecretDetails_rotationEnabled,
    awsSecretsManagerSecretDetails_rotationLambdaArn,
    awsSecretsManagerSecretDetails_rotationOccurredWithinFrequency,
    awsSecretsManagerSecretDetails_rotationRules,

    -- * AwsSecretsManagerSecretRotationRules
    AwsSecretsManagerSecretRotationRules (..),
    newAwsSecretsManagerSecretRotationRules,
    awsSecretsManagerSecretRotationRules_automaticallyAfterDays,

    -- * AwsSecurityFinding
    AwsSecurityFinding (..),
    newAwsSecurityFinding,
    awsSecurityFinding_action,
    awsSecurityFinding_companyName,
    awsSecurityFinding_compliance,
    awsSecurityFinding_confidence,
    awsSecurityFinding_criticality,
    awsSecurityFinding_findingProviderFields,
    awsSecurityFinding_firstObservedAt,
    awsSecurityFinding_lastObservedAt,
    awsSecurityFinding_malware,
    awsSecurityFinding_network,
    awsSecurityFinding_networkPath,
    awsSecurityFinding_note,
    awsSecurityFinding_patchSummary,
    awsSecurityFinding_process,
    awsSecurityFinding_productFields,
    awsSecurityFinding_productName,
    awsSecurityFinding_recordState,
    awsSecurityFinding_region,
    awsSecurityFinding_relatedFindings,
    awsSecurityFinding_remediation,
    awsSecurityFinding_sample,
    awsSecurityFinding_severity,
    awsSecurityFinding_sourceUrl,
    awsSecurityFinding_threatIntelIndicators,
    awsSecurityFinding_threats,
    awsSecurityFinding_types,
    awsSecurityFinding_userDefinedFields,
    awsSecurityFinding_verificationState,
    awsSecurityFinding_vulnerabilities,
    awsSecurityFinding_workflow,
    awsSecurityFinding_workflowState,
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
    awsSecurityFindingFilters_awsAccountId,
    awsSecurityFindingFilters_companyName,
    awsSecurityFindingFilters_complianceAssociatedStandardsId,
    awsSecurityFindingFilters_complianceSecurityControlId,
    awsSecurityFindingFilters_complianceStatus,
    awsSecurityFindingFilters_confidence,
    awsSecurityFindingFilters_createdAt,
    awsSecurityFindingFilters_criticality,
    awsSecurityFindingFilters_description,
    awsSecurityFindingFilters_findingProviderFieldsConfidence,
    awsSecurityFindingFilters_findingProviderFieldsCriticality,
    awsSecurityFindingFilters_findingProviderFieldsRelatedFindingsId,
    awsSecurityFindingFilters_findingProviderFieldsRelatedFindingsProductArn,
    awsSecurityFindingFilters_findingProviderFieldsSeverityLabel,
    awsSecurityFindingFilters_findingProviderFieldsSeverityOriginal,
    awsSecurityFindingFilters_findingProviderFieldsTypes,
    awsSecurityFindingFilters_firstObservedAt,
    awsSecurityFindingFilters_generatorId,
    awsSecurityFindingFilters_id,
    awsSecurityFindingFilters_keyword,
    awsSecurityFindingFilters_lastObservedAt,
    awsSecurityFindingFilters_malwareName,
    awsSecurityFindingFilters_malwarePath,
    awsSecurityFindingFilters_malwareState,
    awsSecurityFindingFilters_malwareType,
    awsSecurityFindingFilters_networkDestinationDomain,
    awsSecurityFindingFilters_networkDestinationIpV4,
    awsSecurityFindingFilters_networkDestinationIpV6,
    awsSecurityFindingFilters_networkDestinationPort,
    awsSecurityFindingFilters_networkDirection,
    awsSecurityFindingFilters_networkProtocol,
    awsSecurityFindingFilters_networkSourceDomain,
    awsSecurityFindingFilters_networkSourceIpV4,
    awsSecurityFindingFilters_networkSourceIpV6,
    awsSecurityFindingFilters_networkSourceMac,
    awsSecurityFindingFilters_networkSourcePort,
    awsSecurityFindingFilters_noteText,
    awsSecurityFindingFilters_noteUpdatedAt,
    awsSecurityFindingFilters_noteUpdatedBy,
    awsSecurityFindingFilters_processLaunchedAt,
    awsSecurityFindingFilters_processName,
    awsSecurityFindingFilters_processParentPid,
    awsSecurityFindingFilters_processPath,
    awsSecurityFindingFilters_processPid,
    awsSecurityFindingFilters_processTerminatedAt,
    awsSecurityFindingFilters_productArn,
    awsSecurityFindingFilters_productFields,
    awsSecurityFindingFilters_productName,
    awsSecurityFindingFilters_recommendationText,
    awsSecurityFindingFilters_recordState,
    awsSecurityFindingFilters_region,
    awsSecurityFindingFilters_relatedFindingsId,
    awsSecurityFindingFilters_relatedFindingsProductArn,
    awsSecurityFindingFilters_resourceAwsEc2InstanceIamInstanceProfileArn,
    awsSecurityFindingFilters_resourceAwsEc2InstanceImageId,
    awsSecurityFindingFilters_resourceAwsEc2InstanceIpV4Addresses,
    awsSecurityFindingFilters_resourceAwsEc2InstanceIpV6Addresses,
    awsSecurityFindingFilters_resourceAwsEc2InstanceKeyName,
    awsSecurityFindingFilters_resourceAwsEc2InstanceLaunchedAt,
    awsSecurityFindingFilters_resourceAwsEc2InstanceSubnetId,
    awsSecurityFindingFilters_resourceAwsEc2InstanceType,
    awsSecurityFindingFilters_resourceAwsEc2InstanceVpcId,
    awsSecurityFindingFilters_resourceAwsIamAccessKeyCreatedAt,
    awsSecurityFindingFilters_resourceAwsIamAccessKeyPrincipalName,
    awsSecurityFindingFilters_resourceAwsIamAccessKeyStatus,
    awsSecurityFindingFilters_resourceAwsIamAccessKeyUserName,
    awsSecurityFindingFilters_resourceAwsIamUserUserName,
    awsSecurityFindingFilters_resourceAwsS3BucketOwnerId,
    awsSecurityFindingFilters_resourceAwsS3BucketOwnerName,
    awsSecurityFindingFilters_resourceContainerImageId,
    awsSecurityFindingFilters_resourceContainerImageName,
    awsSecurityFindingFilters_resourceContainerLaunchedAt,
    awsSecurityFindingFilters_resourceContainerName,
    awsSecurityFindingFilters_resourceDetailsOther,
    awsSecurityFindingFilters_resourceId,
    awsSecurityFindingFilters_resourcePartition,
    awsSecurityFindingFilters_resourceRegion,
    awsSecurityFindingFilters_resourceTags,
    awsSecurityFindingFilters_resourceType,
    awsSecurityFindingFilters_sample,
    awsSecurityFindingFilters_severityLabel,
    awsSecurityFindingFilters_severityNormalized,
    awsSecurityFindingFilters_severityProduct,
    awsSecurityFindingFilters_sourceUrl,
    awsSecurityFindingFilters_threatIntelIndicatorCategory,
    awsSecurityFindingFilters_threatIntelIndicatorLastObservedAt,
    awsSecurityFindingFilters_threatIntelIndicatorSource,
    awsSecurityFindingFilters_threatIntelIndicatorSourceUrl,
    awsSecurityFindingFilters_threatIntelIndicatorType,
    awsSecurityFindingFilters_threatIntelIndicatorValue,
    awsSecurityFindingFilters_title,
    awsSecurityFindingFilters_type,
    awsSecurityFindingFilters_updatedAt,
    awsSecurityFindingFilters_userDefinedFields,
    awsSecurityFindingFilters_verificationState,
    awsSecurityFindingFilters_workflowState,
    awsSecurityFindingFilters_workflowStatus,

    -- * AwsSecurityFindingIdentifier
    AwsSecurityFindingIdentifier (..),
    newAwsSecurityFindingIdentifier,
    awsSecurityFindingIdentifier_id,
    awsSecurityFindingIdentifier_productArn,

    -- * AwsSnsTopicDetails
    AwsSnsTopicDetails (..),
    newAwsSnsTopicDetails,
    awsSnsTopicDetails_applicationSuccessFeedbackRoleArn,
    awsSnsTopicDetails_firehoseFailureFeedbackRoleArn,
    awsSnsTopicDetails_firehoseSuccessFeedbackRoleArn,
    awsSnsTopicDetails_httpFailureFeedbackRoleArn,
    awsSnsTopicDetails_httpSuccessFeedbackRoleArn,
    awsSnsTopicDetails_kmsMasterKeyId,
    awsSnsTopicDetails_owner,
    awsSnsTopicDetails_sqsFailureFeedbackRoleArn,
    awsSnsTopicDetails_sqsSuccessFeedbackRoleArn,
    awsSnsTopicDetails_subscription,
    awsSnsTopicDetails_topicName,

    -- * AwsSnsTopicSubscription
    AwsSnsTopicSubscription (..),
    newAwsSnsTopicSubscription,
    awsSnsTopicSubscription_endpoint,
    awsSnsTopicSubscription_protocol,

    -- * AwsSqsQueueDetails
    AwsSqsQueueDetails (..),
    newAwsSqsQueueDetails,
    awsSqsQueueDetails_deadLetterTargetArn,
    awsSqsQueueDetails_kmsDataKeyReusePeriodSeconds,
    awsSqsQueueDetails_kmsMasterKeyId,
    awsSqsQueueDetails_queueName,

    -- * AwsSsmComplianceSummary
    AwsSsmComplianceSummary (..),
    newAwsSsmComplianceSummary,
    awsSsmComplianceSummary_complianceType,
    awsSsmComplianceSummary_compliantCriticalCount,
    awsSsmComplianceSummary_compliantHighCount,
    awsSsmComplianceSummary_compliantInformationalCount,
    awsSsmComplianceSummary_compliantLowCount,
    awsSsmComplianceSummary_compliantMediumCount,
    awsSsmComplianceSummary_compliantUnspecifiedCount,
    awsSsmComplianceSummary_executionType,
    awsSsmComplianceSummary_nonCompliantCriticalCount,
    awsSsmComplianceSummary_nonCompliantHighCount,
    awsSsmComplianceSummary_nonCompliantInformationalCount,
    awsSsmComplianceSummary_nonCompliantLowCount,
    awsSsmComplianceSummary_nonCompliantMediumCount,
    awsSsmComplianceSummary_nonCompliantUnspecifiedCount,
    awsSsmComplianceSummary_overallSeverity,
    awsSsmComplianceSummary_patchBaselineId,
    awsSsmComplianceSummary_patchGroup,
    awsSsmComplianceSummary_status,

    -- * AwsSsmPatch
    AwsSsmPatch (..),
    newAwsSsmPatch,
    awsSsmPatch_complianceSummary,

    -- * AwsSsmPatchComplianceDetails
    AwsSsmPatchComplianceDetails (..),
    newAwsSsmPatchComplianceDetails,
    awsSsmPatchComplianceDetails_patch,

    -- * AwsStepFunctionStateMachineDetails
    AwsStepFunctionStateMachineDetails (..),
    newAwsStepFunctionStateMachineDetails,
    awsStepFunctionStateMachineDetails_label,
    awsStepFunctionStateMachineDetails_loggingConfiguration,
    awsStepFunctionStateMachineDetails_name,
    awsStepFunctionStateMachineDetails_roleArn,
    awsStepFunctionStateMachineDetails_stateMachineArn,
    awsStepFunctionStateMachineDetails_status,
    awsStepFunctionStateMachineDetails_tracingConfiguration,
    awsStepFunctionStateMachineDetails_type,

    -- * AwsStepFunctionStateMachineLoggingConfigurationDestinationsCloudWatchLogsLogGroupDetails
    AwsStepFunctionStateMachineLoggingConfigurationDestinationsCloudWatchLogsLogGroupDetails (..),
    newAwsStepFunctionStateMachineLoggingConfigurationDestinationsCloudWatchLogsLogGroupDetails,
    awsStepFunctionStateMachineLoggingConfigurationDestinationsCloudWatchLogsLogGroupDetails_logGroupArn,

    -- * AwsStepFunctionStateMachineLoggingConfigurationDestinationsDetails
    AwsStepFunctionStateMachineLoggingConfigurationDestinationsDetails (..),
    newAwsStepFunctionStateMachineLoggingConfigurationDestinationsDetails,
    awsStepFunctionStateMachineLoggingConfigurationDestinationsDetails_cloudWatchLogsLogGroup,

    -- * AwsStepFunctionStateMachineLoggingConfigurationDetails
    AwsStepFunctionStateMachineLoggingConfigurationDetails (..),
    newAwsStepFunctionStateMachineLoggingConfigurationDetails,
    awsStepFunctionStateMachineLoggingConfigurationDetails_destinations,
    awsStepFunctionStateMachineLoggingConfigurationDetails_includeExecutionData,
    awsStepFunctionStateMachineLoggingConfigurationDetails_level,

    -- * AwsStepFunctionStateMachineTracingConfigurationDetails
    AwsStepFunctionStateMachineTracingConfigurationDetails (..),
    newAwsStepFunctionStateMachineTracingConfigurationDetails,
    awsStepFunctionStateMachineTracingConfigurationDetails_enabled,

    -- * AwsWafRateBasedRuleDetails
    AwsWafRateBasedRuleDetails (..),
    newAwsWafRateBasedRuleDetails,
    awsWafRateBasedRuleDetails_matchPredicates,
    awsWafRateBasedRuleDetails_metricName,
    awsWafRateBasedRuleDetails_name,
    awsWafRateBasedRuleDetails_rateKey,
    awsWafRateBasedRuleDetails_rateLimit,
    awsWafRateBasedRuleDetails_ruleId,

    -- * AwsWafRateBasedRuleMatchPredicate
    AwsWafRateBasedRuleMatchPredicate (..),
    newAwsWafRateBasedRuleMatchPredicate,
    awsWafRateBasedRuleMatchPredicate_dataId,
    awsWafRateBasedRuleMatchPredicate_negated,
    awsWafRateBasedRuleMatchPredicate_type,

    -- * AwsWafRegionalRateBasedRuleDetails
    AwsWafRegionalRateBasedRuleDetails (..),
    newAwsWafRegionalRateBasedRuleDetails,
    awsWafRegionalRateBasedRuleDetails_matchPredicates,
    awsWafRegionalRateBasedRuleDetails_metricName,
    awsWafRegionalRateBasedRuleDetails_name,
    awsWafRegionalRateBasedRuleDetails_rateKey,
    awsWafRegionalRateBasedRuleDetails_rateLimit,
    awsWafRegionalRateBasedRuleDetails_ruleId,

    -- * AwsWafRegionalRateBasedRuleMatchPredicate
    AwsWafRegionalRateBasedRuleMatchPredicate (..),
    newAwsWafRegionalRateBasedRuleMatchPredicate,
    awsWafRegionalRateBasedRuleMatchPredicate_dataId,
    awsWafRegionalRateBasedRuleMatchPredicate_negated,
    awsWafRegionalRateBasedRuleMatchPredicate_type,

    -- * AwsWafRegionalRuleDetails
    AwsWafRegionalRuleDetails (..),
    newAwsWafRegionalRuleDetails,
    awsWafRegionalRuleDetails_metricName,
    awsWafRegionalRuleDetails_name,
    awsWafRegionalRuleDetails_predicateList,
    awsWafRegionalRuleDetails_ruleId,

    -- * AwsWafRegionalRuleGroupDetails
    AwsWafRegionalRuleGroupDetails (..),
    newAwsWafRegionalRuleGroupDetails,
    awsWafRegionalRuleGroupDetails_metricName,
    awsWafRegionalRuleGroupDetails_name,
    awsWafRegionalRuleGroupDetails_ruleGroupId,
    awsWafRegionalRuleGroupDetails_rules,

    -- * AwsWafRegionalRuleGroupRulesActionDetails
    AwsWafRegionalRuleGroupRulesActionDetails (..),
    newAwsWafRegionalRuleGroupRulesActionDetails,
    awsWafRegionalRuleGroupRulesActionDetails_type,

    -- * AwsWafRegionalRuleGroupRulesDetails
    AwsWafRegionalRuleGroupRulesDetails (..),
    newAwsWafRegionalRuleGroupRulesDetails,
    awsWafRegionalRuleGroupRulesDetails_action,
    awsWafRegionalRuleGroupRulesDetails_priority,
    awsWafRegionalRuleGroupRulesDetails_ruleId,
    awsWafRegionalRuleGroupRulesDetails_type,

    -- * AwsWafRegionalRulePredicateListDetails
    AwsWafRegionalRulePredicateListDetails (..),
    newAwsWafRegionalRulePredicateListDetails,
    awsWafRegionalRulePredicateListDetails_dataId,
    awsWafRegionalRulePredicateListDetails_negated,
    awsWafRegionalRulePredicateListDetails_type,

    -- * AwsWafRegionalWebAclDetails
    AwsWafRegionalWebAclDetails (..),
    newAwsWafRegionalWebAclDetails,
    awsWafRegionalWebAclDetails_defaultAction,
    awsWafRegionalWebAclDetails_metricName,
    awsWafRegionalWebAclDetails_name,
    awsWafRegionalWebAclDetails_rulesList,
    awsWafRegionalWebAclDetails_webAclId,

    -- * AwsWafRegionalWebAclRulesListActionDetails
    AwsWafRegionalWebAclRulesListActionDetails (..),
    newAwsWafRegionalWebAclRulesListActionDetails,
    awsWafRegionalWebAclRulesListActionDetails_type,

    -- * AwsWafRegionalWebAclRulesListDetails
    AwsWafRegionalWebAclRulesListDetails (..),
    newAwsWafRegionalWebAclRulesListDetails,
    awsWafRegionalWebAclRulesListDetails_action,
    awsWafRegionalWebAclRulesListDetails_overrideAction,
    awsWafRegionalWebAclRulesListDetails_priority,
    awsWafRegionalWebAclRulesListDetails_ruleId,
    awsWafRegionalWebAclRulesListDetails_type,

    -- * AwsWafRegionalWebAclRulesListOverrideActionDetails
    AwsWafRegionalWebAclRulesListOverrideActionDetails (..),
    newAwsWafRegionalWebAclRulesListOverrideActionDetails,
    awsWafRegionalWebAclRulesListOverrideActionDetails_type,

    -- * AwsWafRuleDetails
    AwsWafRuleDetails (..),
    newAwsWafRuleDetails,
    awsWafRuleDetails_metricName,
    awsWafRuleDetails_name,
    awsWafRuleDetails_predicateList,
    awsWafRuleDetails_ruleId,

    -- * AwsWafRuleGroupDetails
    AwsWafRuleGroupDetails (..),
    newAwsWafRuleGroupDetails,
    awsWafRuleGroupDetails_metricName,
    awsWafRuleGroupDetails_name,
    awsWafRuleGroupDetails_ruleGroupId,
    awsWafRuleGroupDetails_rules,

    -- * AwsWafRuleGroupRulesActionDetails
    AwsWafRuleGroupRulesActionDetails (..),
    newAwsWafRuleGroupRulesActionDetails,
    awsWafRuleGroupRulesActionDetails_type,

    -- * AwsWafRuleGroupRulesDetails
    AwsWafRuleGroupRulesDetails (..),
    newAwsWafRuleGroupRulesDetails,
    awsWafRuleGroupRulesDetails_action,
    awsWafRuleGroupRulesDetails_priority,
    awsWafRuleGroupRulesDetails_ruleId,
    awsWafRuleGroupRulesDetails_type,

    -- * AwsWafRulePredicateListDetails
    AwsWafRulePredicateListDetails (..),
    newAwsWafRulePredicateListDetails,
    awsWafRulePredicateListDetails_dataId,
    awsWafRulePredicateListDetails_negated,
    awsWafRulePredicateListDetails_type,

    -- * AwsWafWebAclDetails
    AwsWafWebAclDetails (..),
    newAwsWafWebAclDetails,
    awsWafWebAclDetails_defaultAction,
    awsWafWebAclDetails_name,
    awsWafWebAclDetails_rules,
    awsWafWebAclDetails_webAclId,

    -- * AwsWafWebAclRule
    AwsWafWebAclRule (..),
    newAwsWafWebAclRule,
    awsWafWebAclRule_action,
    awsWafWebAclRule_excludedRules,
    awsWafWebAclRule_overrideAction,
    awsWafWebAclRule_priority,
    awsWafWebAclRule_ruleId,
    awsWafWebAclRule_type,

    -- * AwsWafv2ActionAllowDetails
    AwsWafv2ActionAllowDetails (..),
    newAwsWafv2ActionAllowDetails,
    awsWafv2ActionAllowDetails_customRequestHandling,

    -- * AwsWafv2ActionBlockDetails
    AwsWafv2ActionBlockDetails (..),
    newAwsWafv2ActionBlockDetails,
    awsWafv2ActionBlockDetails_customResponse,

    -- * AwsWafv2CustomHttpHeader
    AwsWafv2CustomHttpHeader (..),
    newAwsWafv2CustomHttpHeader,
    awsWafv2CustomHttpHeader_name,
    awsWafv2CustomHttpHeader_value,

    -- * AwsWafv2CustomRequestHandlingDetails
    AwsWafv2CustomRequestHandlingDetails (..),
    newAwsWafv2CustomRequestHandlingDetails,
    awsWafv2CustomRequestHandlingDetails_insertHeaders,

    -- * AwsWafv2CustomResponseDetails
    AwsWafv2CustomResponseDetails (..),
    newAwsWafv2CustomResponseDetails,
    awsWafv2CustomResponseDetails_customResponseBodyKey,
    awsWafv2CustomResponseDetails_responseCode,
    awsWafv2CustomResponseDetails_responseHeaders,

    -- * AwsWafv2RuleGroupDetails
    AwsWafv2RuleGroupDetails (..),
    newAwsWafv2RuleGroupDetails,
    awsWafv2RuleGroupDetails_arn,
    awsWafv2RuleGroupDetails_capacity,
    awsWafv2RuleGroupDetails_description,
    awsWafv2RuleGroupDetails_id,
    awsWafv2RuleGroupDetails_name,
    awsWafv2RuleGroupDetails_rules,
    awsWafv2RuleGroupDetails_scope,
    awsWafv2RuleGroupDetails_visibilityConfig,

    -- * AwsWafv2RulesActionCaptchaDetails
    AwsWafv2RulesActionCaptchaDetails (..),
    newAwsWafv2RulesActionCaptchaDetails,
    awsWafv2RulesActionCaptchaDetails_customRequestHandling,

    -- * AwsWafv2RulesActionCountDetails
    AwsWafv2RulesActionCountDetails (..),
    newAwsWafv2RulesActionCountDetails,
    awsWafv2RulesActionCountDetails_customRequestHandling,

    -- * AwsWafv2RulesActionDetails
    AwsWafv2RulesActionDetails (..),
    newAwsWafv2RulesActionDetails,
    awsWafv2RulesActionDetails_allow,
    awsWafv2RulesActionDetails_block,
    awsWafv2RulesActionDetails_captcha,
    awsWafv2RulesActionDetails_count,

    -- * AwsWafv2RulesDetails
    AwsWafv2RulesDetails (..),
    newAwsWafv2RulesDetails,
    awsWafv2RulesDetails_action,
    awsWafv2RulesDetails_name,
    awsWafv2RulesDetails_overrideAction,
    awsWafv2RulesDetails_priority,
    awsWafv2RulesDetails_visibilityConfig,

    -- * AwsWafv2VisibilityConfigDetails
    AwsWafv2VisibilityConfigDetails (..),
    newAwsWafv2VisibilityConfigDetails,
    awsWafv2VisibilityConfigDetails_cloudWatchMetricsEnabled,
    awsWafv2VisibilityConfigDetails_metricName,
    awsWafv2VisibilityConfigDetails_sampledRequestsEnabled,

    -- * AwsWafv2WebAclActionDetails
    AwsWafv2WebAclActionDetails (..),
    newAwsWafv2WebAclActionDetails,
    awsWafv2WebAclActionDetails_allow,
    awsWafv2WebAclActionDetails_block,

    -- * AwsWafv2WebAclCaptchaConfigDetails
    AwsWafv2WebAclCaptchaConfigDetails (..),
    newAwsWafv2WebAclCaptchaConfigDetails,
    awsWafv2WebAclCaptchaConfigDetails_immunityTimeProperty,

    -- * AwsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails
    AwsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails (..),
    newAwsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails,
    awsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails_immunityTime,

    -- * AwsWafv2WebAclDetails
    AwsWafv2WebAclDetails (..),
    newAwsWafv2WebAclDetails,
    awsWafv2WebAclDetails_arn,
    awsWafv2WebAclDetails_capacity,
    awsWafv2WebAclDetails_captchaConfig,
    awsWafv2WebAclDetails_defaultAction,
    awsWafv2WebAclDetails_description,
    awsWafv2WebAclDetails_id,
    awsWafv2WebAclDetails_managedbyFirewallManager,
    awsWafv2WebAclDetails_name,
    awsWafv2WebAclDetails_rules,
    awsWafv2WebAclDetails_visibilityConfig,

    -- * AwsXrayEncryptionConfigDetails
    AwsXrayEncryptionConfigDetails (..),
    newAwsXrayEncryptionConfigDetails,
    awsXrayEncryptionConfigDetails_keyId,
    awsXrayEncryptionConfigDetails_status,
    awsXrayEncryptionConfigDetails_type,

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
    cell_cellReference,
    cell_column,
    cell_columnName,
    cell_row,

    -- * CidrBlockAssociation
    CidrBlockAssociation (..),
    newCidrBlockAssociation,
    cidrBlockAssociation_associationId,
    cidrBlockAssociation_cidrBlock,
    cidrBlockAssociation_cidrBlockState,

    -- * City
    City (..),
    newCity,
    city_cityName,

    -- * ClassificationResult
    ClassificationResult (..),
    newClassificationResult,
    classificationResult_additionalOccurrences,
    classificationResult_customDataIdentifiers,
    classificationResult_mimeType,
    classificationResult_sensitiveData,
    classificationResult_sizeClassified,
    classificationResult_status,

    -- * ClassificationStatus
    ClassificationStatus (..),
    newClassificationStatus,
    classificationStatus_code,
    classificationStatus_reason,

    -- * Compliance
    Compliance (..),
    newCompliance,
    compliance_associatedStandards,
    compliance_relatedRequirements,
    compliance_securityControlId,
    compliance_status,
    compliance_statusReasons,

    -- * ContainerDetails
    ContainerDetails (..),
    newContainerDetails,
    containerDetails_containerRuntime,
    containerDetails_imageId,
    containerDetails_imageName,
    containerDetails_launchedAt,
    containerDetails_name,
    containerDetails_privileged,
    containerDetails_volumeMounts,

    -- * Country
    Country (..),
    newCountry,
    country_countryCode,
    country_countryName,

    -- * CustomDataIdentifiersDetections
    CustomDataIdentifiersDetections (..),
    newCustomDataIdentifiersDetections,
    customDataIdentifiersDetections_arn,
    customDataIdentifiersDetections_count,
    customDataIdentifiersDetections_name,
    customDataIdentifiersDetections_occurrences,

    -- * CustomDataIdentifiersResult
    CustomDataIdentifiersResult (..),
    newCustomDataIdentifiersResult,
    customDataIdentifiersResult_detections,
    customDataIdentifiersResult_totalCount,

    -- * Cvss
    Cvss (..),
    newCvss,
    cvss_adjustments,
    cvss_baseScore,
    cvss_baseVector,
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
    dateFilter_dateRange,
    dateFilter_end,
    dateFilter_start,

    -- * DateRange
    DateRange (..),
    newDateRange,
    dateRange_unit,
    dateRange_value,

    -- * DnsRequestAction
    DnsRequestAction (..),
    newDnsRequestAction,
    dnsRequestAction_blocked,
    dnsRequestAction_domain,
    dnsRequestAction_protocol,

    -- * FilePaths
    FilePaths (..),
    newFilePaths,
    filePaths_fileName,
    filePaths_filePath,
    filePaths_hash,
    filePaths_resourceId,

    -- * FindingAggregator
    FindingAggregator (..),
    newFindingAggregator,
    findingAggregator_findingAggregatorArn,

    -- * FindingHistoryRecord
    FindingHistoryRecord (..),
    newFindingHistoryRecord,
    findingHistoryRecord_findingCreated,
    findingHistoryRecord_findingIdentifier,
    findingHistoryRecord_nextToken,
    findingHistoryRecord_updateSource,
    findingHistoryRecord_updateTime,
    findingHistoryRecord_updates,

    -- * FindingHistoryUpdate
    FindingHistoryUpdate (..),
    newFindingHistoryUpdate,
    findingHistoryUpdate_newValue,
    findingHistoryUpdate_oldValue,
    findingHistoryUpdate_updatedField,

    -- * FindingHistoryUpdateSource
    FindingHistoryUpdateSource (..),
    newFindingHistoryUpdateSource,
    findingHistoryUpdateSource_identity,
    findingHistoryUpdateSource_type,

    -- * FindingProviderFields
    FindingProviderFields (..),
    newFindingProviderFields,
    findingProviderFields_confidence,
    findingProviderFields_criticality,
    findingProviderFields_relatedFindings,
    findingProviderFields_severity,
    findingProviderFields_types,

    -- * FindingProviderSeverity
    FindingProviderSeverity (..),
    newFindingProviderSeverity,
    findingProviderSeverity_label,
    findingProviderSeverity_original,

    -- * FirewallPolicyDetails
    FirewallPolicyDetails (..),
    newFirewallPolicyDetails,
    firewallPolicyDetails_statefulRuleGroupReferences,
    firewallPolicyDetails_statelessCustomActions,
    firewallPolicyDetails_statelessDefaultActions,
    firewallPolicyDetails_statelessFragmentDefaultActions,
    firewallPolicyDetails_statelessRuleGroupReferences,

    -- * FirewallPolicyStatefulRuleGroupReferencesDetails
    FirewallPolicyStatefulRuleGroupReferencesDetails (..),
    newFirewallPolicyStatefulRuleGroupReferencesDetails,
    firewallPolicyStatefulRuleGroupReferencesDetails_resourceArn,

    -- * FirewallPolicyStatelessCustomActionsDetails
    FirewallPolicyStatelessCustomActionsDetails (..),
    newFirewallPolicyStatelessCustomActionsDetails,
    firewallPolicyStatelessCustomActionsDetails_actionDefinition,
    firewallPolicyStatelessCustomActionsDetails_actionName,

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
    icmpTypeCode_code,
    icmpTypeCode_type,

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
    invitation_accountId,
    invitation_invitationId,
    invitation_invitedAt,
    invitation_memberStatus,

    -- * IpFilter
    IpFilter (..),
    newIpFilter,
    ipFilter_cidr,

    -- * IpOrganizationDetails
    IpOrganizationDetails (..),
    newIpOrganizationDetails,
    ipOrganizationDetails_asn,
    ipOrganizationDetails_asnOrg,
    ipOrganizationDetails_isp,
    ipOrganizationDetails_org,

    -- * Ipv6CidrBlockAssociation
    Ipv6CidrBlockAssociation (..),
    newIpv6CidrBlockAssociation,
    ipv6CidrBlockAssociation_associationId,
    ipv6CidrBlockAssociation_cidrBlockState,
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
    malware_path,
    malware_state,
    malware_type,
    malware_name,

    -- * MapFilter
    MapFilter (..),
    newMapFilter,
    mapFilter_comparison,
    mapFilter_key,
    mapFilter_value,

    -- * Member
    Member (..),
    newMember,
    member_accountId,
    member_administratorId,
    member_email,
    member_invitedAt,
    member_masterId,
    member_memberStatus,
    member_updatedAt,

    -- * Network
    Network (..),
    newNetwork,
    network_destinationDomain,
    network_destinationIpV4,
    network_destinationIpV6,
    network_destinationPort,
    network_direction,
    network_openPortRange,
    network_protocol,
    network_sourceDomain,
    network_sourceIpV4,
    network_sourceIpV6,
    network_sourceMac,
    network_sourcePort,

    -- * NetworkConnectionAction
    NetworkConnectionAction (..),
    newNetworkConnectionAction,
    networkConnectionAction_blocked,
    networkConnectionAction_connectionDirection,
    networkConnectionAction_localPortDetails,
    networkConnectionAction_protocol,
    networkConnectionAction_remoteIpDetails,
    networkConnectionAction_remotePortDetails,

    -- * NetworkHeader
    NetworkHeader (..),
    newNetworkHeader,
    networkHeader_destination,
    networkHeader_protocol,
    networkHeader_source,

    -- * NetworkPathComponent
    NetworkPathComponent (..),
    newNetworkPathComponent,
    networkPathComponent_componentId,
    networkPathComponent_componentType,
    networkPathComponent_egress,
    networkPathComponent_ingress,

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
    numberFilter_eq,
    numberFilter_gte,
    numberFilter_lte,

    -- * Occurrences
    Occurrences (..),
    newOccurrences,
    occurrences_cells,
    occurrences_lineRanges,
    occurrences_offsetRanges,
    occurrences_pages,
    occurrences_records,

    -- * Page
    Page (..),
    newPage,
    page_lineRange,
    page_offsetRange,
    page_pageNumber,

    -- * PatchSummary
    PatchSummary (..),
    newPatchSummary,
    patchSummary_failedCount,
    patchSummary_installedCount,
    patchSummary_installedOtherCount,
    patchSummary_installedPendingReboot,
    patchSummary_installedRejectedCount,
    patchSummary_missingCount,
    patchSummary_operation,
    patchSummary_operationEndTime,
    patchSummary_operationStartTime,
    patchSummary_rebootOption,
    patchSummary_id,

    -- * PortProbeAction
    PortProbeAction (..),
    newPortProbeAction,
    portProbeAction_blocked,
    portProbeAction_portProbeDetails,

    -- * PortProbeDetail
    PortProbeDetail (..),
    newPortProbeDetail,
    portProbeDetail_localIpDetails,
    portProbeDetail_localPortDetails,
    portProbeDetail_remoteIpDetails,

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
    processDetails_launchedAt,
    processDetails_name,
    processDetails_parentPid,
    processDetails_path,
    processDetails_pid,
    processDetails_terminatedAt,

    -- * Product
    Product (..),
    newProduct,
    product_activationUrl,
    product_categories,
    product_companyName,
    product_description,
    product_integrationTypes,
    product_marketplaceUrl,
    product_productName,
    product_productSubscriptionResourcePolicy,
    product_productArn,

    -- * PropagatingVgwSetDetails
    PropagatingVgwSetDetails (..),
    newPropagatingVgwSetDetails,
    propagatingVgwSetDetails_gatewayId,

    -- * Range
    Range (..),
    newRange,
    range_end,
    range_start,
    range_startColumn,

    -- * Recommendation
    Recommendation (..),
    newRecommendation,
    recommendation_text,
    recommendation_url,

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
    resource_dataClassification,
    resource_details,
    resource_partition,
    resource_region,
    resource_resourceRole,
    resource_tags,
    resource_type,
    resource_id,

    -- * ResourceDetails
    ResourceDetails (..),
    newResourceDetails,
    resourceDetails_awsAmazonMqBroker,
    resourceDetails_awsApiGatewayRestApi,
    resourceDetails_awsApiGatewayStage,
    resourceDetails_awsApiGatewayV2Api,
    resourceDetails_awsApiGatewayV2Stage,
    resourceDetails_awsAppSyncGraphQlApi,
    resourceDetails_awsAutoScalingAutoScalingGroup,
    resourceDetails_awsAutoScalingLaunchConfiguration,
    resourceDetails_awsBackupBackupPlan,
    resourceDetails_awsBackupBackupVault,
    resourceDetails_awsBackupRecoveryPoint,
    resourceDetails_awsCertificateManagerCertificate,
    resourceDetails_awsCloudFormationStack,
    resourceDetails_awsCloudFrontDistribution,
    resourceDetails_awsCloudTrailTrail,
    resourceDetails_awsCloudWatchAlarm,
    resourceDetails_awsCodeBuildProject,
    resourceDetails_awsDynamoDbTable,
    resourceDetails_awsEc2Eip,
    resourceDetails_awsEc2Instance,
    resourceDetails_awsEc2LaunchTemplate,
    resourceDetails_awsEc2NetworkAcl,
    resourceDetails_awsEc2NetworkInterface,
    resourceDetails_awsEc2RouteTable,
    resourceDetails_awsEc2SecurityGroup,
    resourceDetails_awsEc2Subnet,
    resourceDetails_awsEc2TransitGateway,
    resourceDetails_awsEc2Volume,
    resourceDetails_awsEc2Vpc,
    resourceDetails_awsEc2VpcEndpointService,
    resourceDetails_awsEc2VpcPeeringConnection,
    resourceDetails_awsEc2VpnConnection,
    resourceDetails_awsEcrContainerImage,
    resourceDetails_awsEcrRepository,
    resourceDetails_awsEcsCluster,
    resourceDetails_awsEcsContainer,
    resourceDetails_awsEcsService,
    resourceDetails_awsEcsTask,
    resourceDetails_awsEcsTaskDefinition,
    resourceDetails_awsEfsAccessPoint,
    resourceDetails_awsEksCluster,
    resourceDetails_awsElasticBeanstalkEnvironment,
    resourceDetails_awsElasticsearchDomain,
    resourceDetails_awsElbLoadBalancer,
    resourceDetails_awsElbv2LoadBalancer,
    resourceDetails_awsEventSchemasRegistry,
    resourceDetails_awsGuardDutyDetector,
    resourceDetails_awsIamAccessKey,
    resourceDetails_awsIamGroup,
    resourceDetails_awsIamPolicy,
    resourceDetails_awsIamRole,
    resourceDetails_awsIamUser,
    resourceDetails_awsKinesisStream,
    resourceDetails_awsKmsKey,
    resourceDetails_awsLambdaFunction,
    resourceDetails_awsLambdaLayerVersion,
    resourceDetails_awsNetworkFirewallFirewall,
    resourceDetails_awsNetworkFirewallFirewallPolicy,
    resourceDetails_awsNetworkFirewallRuleGroup,
    resourceDetails_awsOpenSearchServiceDomain,
    resourceDetails_awsRdsDbCluster,
    resourceDetails_awsRdsDbClusterSnapshot,
    resourceDetails_awsRdsDbInstance,
    resourceDetails_awsRdsDbSecurityGroup,
    resourceDetails_awsRdsDbSnapshot,
    resourceDetails_awsRdsEventSubscription,
    resourceDetails_awsRedshiftCluster,
    resourceDetails_awsS3AccountPublicAccessBlock,
    resourceDetails_awsS3Bucket,
    resourceDetails_awsS3Object,
    resourceDetails_awsSageMakerNotebookInstance,
    resourceDetails_awsSecretsManagerSecret,
    resourceDetails_awsSnsTopic,
    resourceDetails_awsSqsQueue,
    resourceDetails_awsSsmPatchCompliance,
    resourceDetails_awsStepFunctionStateMachine,
    resourceDetails_awsWafRateBasedRule,
    resourceDetails_awsWafRegionalRateBasedRule,
    resourceDetails_awsWafRegionalRule,
    resourceDetails_awsWafRegionalRuleGroup,
    resourceDetails_awsWafRegionalWebAcl,
    resourceDetails_awsWafRule,
    resourceDetails_awsWafRuleGroup,
    resourceDetails_awsWafWebAcl,
    resourceDetails_awsWafv2RuleGroup,
    resourceDetails_awsWafv2WebAcl,
    resourceDetails_awsXrayEncryptionConfig,
    resourceDetails_container,
    resourceDetails_other,

    -- * Result
    Result (..),
    newResult,
    result_accountId,
    result_processingResult,

    -- * RouteSetDetails
    RouteSetDetails (..),
    newRouteSetDetails,
    routeSetDetails_carrierGatewayId,
    routeSetDetails_coreNetworkArn,
    routeSetDetails_destinationCidrBlock,
    routeSetDetails_destinationIpv6CidrBlock,
    routeSetDetails_destinationPrefixListId,
    routeSetDetails_egressOnlyInternetGatewayId,
    routeSetDetails_gatewayId,
    routeSetDetails_instanceId,
    routeSetDetails_instanceOwnerId,
    routeSetDetails_localGatewayId,
    routeSetDetails_natGatewayId,
    routeSetDetails_networkInterfaceId,
    routeSetDetails_origin,
    routeSetDetails_state,
    routeSetDetails_transitGatewayId,
    routeSetDetails_vpcPeeringConnectionId,

    -- * RuleGroupDetails
    RuleGroupDetails (..),
    newRuleGroupDetails,
    ruleGroupDetails_ruleVariables,
    ruleGroupDetails_rulesSource,

    -- * RuleGroupSource
    RuleGroupSource (..),
    newRuleGroupSource,
    ruleGroupSource_rulesSourceList,
    ruleGroupSource_rulesString,
    ruleGroupSource_statefulRules,
    ruleGroupSource_statelessRulesAndCustomActions,

    -- * RuleGroupSourceCustomActionsDetails
    RuleGroupSourceCustomActionsDetails (..),
    newRuleGroupSourceCustomActionsDetails,
    ruleGroupSourceCustomActionsDetails_actionDefinition,
    ruleGroupSourceCustomActionsDetails_actionName,

    -- * RuleGroupSourceListDetails
    RuleGroupSourceListDetails (..),
    newRuleGroupSourceListDetails,
    ruleGroupSourceListDetails_generatedRulesType,
    ruleGroupSourceListDetails_targetTypes,
    ruleGroupSourceListDetails_targets,

    -- * RuleGroupSourceStatefulRulesDetails
    RuleGroupSourceStatefulRulesDetails (..),
    newRuleGroupSourceStatefulRulesDetails,
    ruleGroupSourceStatefulRulesDetails_action,
    ruleGroupSourceStatefulRulesDetails_header,
    ruleGroupSourceStatefulRulesDetails_ruleOptions,

    -- * RuleGroupSourceStatefulRulesHeaderDetails
    RuleGroupSourceStatefulRulesHeaderDetails (..),
    newRuleGroupSourceStatefulRulesHeaderDetails,
    ruleGroupSourceStatefulRulesHeaderDetails_destination,
    ruleGroupSourceStatefulRulesHeaderDetails_destinationPort,
    ruleGroupSourceStatefulRulesHeaderDetails_direction,
    ruleGroupSourceStatefulRulesHeaderDetails_protocol,
    ruleGroupSourceStatefulRulesHeaderDetails_source,
    ruleGroupSourceStatefulRulesHeaderDetails_sourcePort,

    -- * RuleGroupSourceStatefulRulesOptionsDetails
    RuleGroupSourceStatefulRulesOptionsDetails (..),
    newRuleGroupSourceStatefulRulesOptionsDetails,
    ruleGroupSourceStatefulRulesOptionsDetails_keyword,
    ruleGroupSourceStatefulRulesOptionsDetails_settings,

    -- * RuleGroupSourceStatelessRuleDefinition
    RuleGroupSourceStatelessRuleDefinition (..),
    newRuleGroupSourceStatelessRuleDefinition,
    ruleGroupSourceStatelessRuleDefinition_actions,
    ruleGroupSourceStatelessRuleDefinition_matchAttributes,

    -- * RuleGroupSourceStatelessRuleMatchAttributes
    RuleGroupSourceStatelessRuleMatchAttributes (..),
    newRuleGroupSourceStatelessRuleMatchAttributes,
    ruleGroupSourceStatelessRuleMatchAttributes_destinationPorts,
    ruleGroupSourceStatelessRuleMatchAttributes_destinations,
    ruleGroupSourceStatelessRuleMatchAttributes_protocols,
    ruleGroupSourceStatelessRuleMatchAttributes_sourcePorts,
    ruleGroupSourceStatelessRuleMatchAttributes_sources,
    ruleGroupSourceStatelessRuleMatchAttributes_tcpFlags,

    -- * RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts
    RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts (..),
    newRuleGroupSourceStatelessRuleMatchAttributesDestinationPorts,
    ruleGroupSourceStatelessRuleMatchAttributesDestinationPorts_fromPort,
    ruleGroupSourceStatelessRuleMatchAttributesDestinationPorts_toPort,

    -- * RuleGroupSourceStatelessRuleMatchAttributesDestinations
    RuleGroupSourceStatelessRuleMatchAttributesDestinations (..),
    newRuleGroupSourceStatelessRuleMatchAttributesDestinations,
    ruleGroupSourceStatelessRuleMatchAttributesDestinations_addressDefinition,

    -- * RuleGroupSourceStatelessRuleMatchAttributesSourcePorts
    RuleGroupSourceStatelessRuleMatchAttributesSourcePorts (..),
    newRuleGroupSourceStatelessRuleMatchAttributesSourcePorts,
    ruleGroupSourceStatelessRuleMatchAttributesSourcePorts_fromPort,
    ruleGroupSourceStatelessRuleMatchAttributesSourcePorts_toPort,

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
    ruleGroupSourceStatelessRulesDetails_priority,
    ruleGroupSourceStatelessRulesDetails_ruleDefinition,

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

    -- * SecurityControl
    SecurityControl (..),
    newSecurityControl,
    securityControl_securityControlId,
    securityControl_securityControlArn,
    securityControl_title,
    securityControl_description,
    securityControl_remediationUrl,
    securityControl_severityRating,
    securityControl_securityControlStatus,

    -- * SecurityControlDefinition
    SecurityControlDefinition (..),
    newSecurityControlDefinition,
    securityControlDefinition_securityControlId,
    securityControlDefinition_title,
    securityControlDefinition_description,
    securityControlDefinition_remediationUrl,
    securityControlDefinition_severityRating,
    securityControlDefinition_currentRegionAvailability,

    -- * SensitiveDataDetections
    SensitiveDataDetections (..),
    newSensitiveDataDetections,
    sensitiveDataDetections_count,
    sensitiveDataDetections_occurrences,
    sensitiveDataDetections_type,

    -- * SensitiveDataResult
    SensitiveDataResult (..),
    newSensitiveDataResult,
    sensitiveDataResult_category,
    sensitiveDataResult_detections,
    sensitiveDataResult_totalCount,

    -- * Severity
    Severity (..),
    newSeverity,
    severity_label,
    severity_normalized,
    severity_original,
    severity_product,

    -- * SeverityUpdate
    SeverityUpdate (..),
    newSeverityUpdate,
    severityUpdate_label,
    severityUpdate_normalized,
    severityUpdate_product,

    -- * SoftwarePackage
    SoftwarePackage (..),
    newSoftwarePackage,
    softwarePackage_architecture,
    softwarePackage_epoch,
    softwarePackage_filePath,
    softwarePackage_fixedInVersion,
    softwarePackage_name,
    softwarePackage_packageManager,
    softwarePackage_release,
    softwarePackage_remediation,
    softwarePackage_sourceLayerArn,
    softwarePackage_sourceLayerHash,
    softwarePackage_version,

    -- * SortCriterion
    SortCriterion (..),
    newSortCriterion,
    sortCriterion_field,
    sortCriterion_sortOrder,

    -- * Standard
    Standard (..),
    newStandard,
    standard_description,
    standard_enabledByDefault,
    standard_name,
    standard_standardsArn,
    standard_standardsManagedBy,

    -- * StandardsControl
    StandardsControl (..),
    newStandardsControl,
    standardsControl_controlId,
    standardsControl_controlStatus,
    standardsControl_controlStatusUpdatedAt,
    standardsControl_description,
    standardsControl_disabledReason,
    standardsControl_relatedRequirements,
    standardsControl_remediationUrl,
    standardsControl_severityRating,
    standardsControl_standardsControlArn,
    standardsControl_title,

    -- * StandardsControlAssociationDetail
    StandardsControlAssociationDetail (..),
    newStandardsControlAssociationDetail,
    standardsControlAssociationDetail_relatedRequirements,
    standardsControlAssociationDetail_standardsControlArns,
    standardsControlAssociationDetail_standardsControlDescription,
    standardsControlAssociationDetail_standardsControlTitle,
    standardsControlAssociationDetail_updatedAt,
    standardsControlAssociationDetail_updatedReason,
    standardsControlAssociationDetail_standardsArn,
    standardsControlAssociationDetail_securityControlId,
    standardsControlAssociationDetail_securityControlArn,
    standardsControlAssociationDetail_associationStatus,

    -- * StandardsControlAssociationId
    StandardsControlAssociationId (..),
    newStandardsControlAssociationId,
    standardsControlAssociationId_securityControlId,
    standardsControlAssociationId_standardsArn,

    -- * StandardsControlAssociationSummary
    StandardsControlAssociationSummary (..),
    newStandardsControlAssociationSummary,
    standardsControlAssociationSummary_relatedRequirements,
    standardsControlAssociationSummary_standardsControlDescription,
    standardsControlAssociationSummary_standardsControlTitle,
    standardsControlAssociationSummary_updatedAt,
    standardsControlAssociationSummary_updatedReason,
    standardsControlAssociationSummary_standardsArn,
    standardsControlAssociationSummary_securityControlId,
    standardsControlAssociationSummary_securityControlArn,
    standardsControlAssociationSummary_associationStatus,

    -- * StandardsControlAssociationUpdate
    StandardsControlAssociationUpdate (..),
    newStandardsControlAssociationUpdate,
    standardsControlAssociationUpdate_updatedReason,
    standardsControlAssociationUpdate_standardsArn,
    standardsControlAssociationUpdate_securityControlId,
    standardsControlAssociationUpdate_associationStatus,

    -- * StandardsManagedBy
    StandardsManagedBy (..),
    newStandardsManagedBy,
    standardsManagedBy_company,
    standardsManagedBy_product,

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
    threat_filePaths,
    threat_itemCount,
    threat_name,
    threat_severity,

    -- * ThreatIntelIndicator
    ThreatIntelIndicator (..),
    newThreatIntelIndicator,
    threatIntelIndicator_category,
    threatIntelIndicator_lastObservedAt,
    threatIntelIndicator_source,
    threatIntelIndicator_sourceUrl,
    threatIntelIndicator_type,
    threatIntelIndicator_value,

    -- * UnprocessedAutomationRule
    UnprocessedAutomationRule (..),
    newUnprocessedAutomationRule,
    unprocessedAutomationRule_errorCode,
    unprocessedAutomationRule_errorMessage,
    unprocessedAutomationRule_ruleArn,

    -- * UnprocessedSecurityControl
    UnprocessedSecurityControl (..),
    newUnprocessedSecurityControl,
    unprocessedSecurityControl_errorReason,
    unprocessedSecurityControl_securityControlId,
    unprocessedSecurityControl_errorCode,

    -- * UnprocessedStandardsControlAssociation
    UnprocessedStandardsControlAssociation (..),
    newUnprocessedStandardsControlAssociation,
    unprocessedStandardsControlAssociation_errorReason,
    unprocessedStandardsControlAssociation_standardsControlAssociationId,
    unprocessedStandardsControlAssociation_errorCode,

    -- * UnprocessedStandardsControlAssociationUpdate
    UnprocessedStandardsControlAssociationUpdate (..),
    newUnprocessedStandardsControlAssociationUpdate,
    unprocessedStandardsControlAssociationUpdate_errorReason,
    unprocessedStandardsControlAssociationUpdate_standardsControlAssociationUpdate,
    unprocessedStandardsControlAssociationUpdate_errorCode,

    -- * UpdateAutomationRulesRequestItem
    UpdateAutomationRulesRequestItem (..),
    newUpdateAutomationRulesRequestItem,
    updateAutomationRulesRequestItem_actions,
    updateAutomationRulesRequestItem_criteria,
    updateAutomationRulesRequestItem_description,
    updateAutomationRulesRequestItem_isTerminal,
    updateAutomationRulesRequestItem_ruleName,
    updateAutomationRulesRequestItem_ruleOrder,
    updateAutomationRulesRequestItem_ruleStatus,
    updateAutomationRulesRequestItem_ruleArn,

    -- * VolumeMount
    VolumeMount (..),
    newVolumeMount,
    volumeMount_mountPath,
    volumeMount_name,

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
    vpcInfoPeeringOptionsDetails_allowEgressFromLocalClassicLinkToRemoteVpc,
    vpcInfoPeeringOptionsDetails_allowEgressFromLocalVpcToRemoteClassicLink,

    -- * Vulnerability
    Vulnerability (..),
    newVulnerability,
    vulnerability_cvss,
    vulnerability_fixAvailable,
    vulnerability_referenceUrls,
    vulnerability_relatedVulnerabilities,
    vulnerability_vendor,
    vulnerability_vulnerablePackages,
    vulnerability_id,

    -- * VulnerabilityVendor
    VulnerabilityVendor (..),
    newVulnerabilityVendor,
    vulnerabilityVendor_url,
    vulnerabilityVendor_vendorCreatedAt,
    vulnerabilityVendor_vendorSeverity,
    vulnerabilityVendor_vendorUpdatedAt,
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
import Amazonka.SecurityHub.Types.AssociatedStandard
import Amazonka.SecurityHub.Types.AssociationSetDetails
import Amazonka.SecurityHub.Types.AssociationStateDetails
import Amazonka.SecurityHub.Types.AssociationStatus
import Amazonka.SecurityHub.Types.AutoEnableStandards
import Amazonka.SecurityHub.Types.AutomationRulesAction
import Amazonka.SecurityHub.Types.AutomationRulesActionType
import Amazonka.SecurityHub.Types.AutomationRulesConfig
import Amazonka.SecurityHub.Types.AutomationRulesFindingFieldsUpdate
import Amazonka.SecurityHub.Types.AutomationRulesFindingFilters
import Amazonka.SecurityHub.Types.AutomationRulesMetadata
import Amazonka.SecurityHub.Types.AvailabilityZone
import Amazonka.SecurityHub.Types.AwsAmazonMqBrokerDetails
import Amazonka.SecurityHub.Types.AwsAmazonMqBrokerEncryptionOptionsDetails
import Amazonka.SecurityHub.Types.AwsAmazonMqBrokerLdapServerMetadataDetails
import Amazonka.SecurityHub.Types.AwsAmazonMqBrokerLogsDetails
import Amazonka.SecurityHub.Types.AwsAmazonMqBrokerLogsPendingDetails
import Amazonka.SecurityHub.Types.AwsAmazonMqBrokerMaintenanceWindowStartTimeDetails
import Amazonka.SecurityHub.Types.AwsAmazonMqBrokerUsersDetails
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
import Amazonka.SecurityHub.Types.AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails
import Amazonka.SecurityHub.Types.AwsAppSyncGraphQlApiDetails
import Amazonka.SecurityHub.Types.AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails
import Amazonka.SecurityHub.Types.AwsAppSyncGraphQlApiLogConfigDetails
import Amazonka.SecurityHub.Types.AwsAppSyncGraphQlApiOpenIdConnectConfigDetails
import Amazonka.SecurityHub.Types.AwsAppSyncGraphQlApiUserPoolConfigDetails
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
import Amazonka.SecurityHub.Types.AwsEc2InstanceMonitoringDetails
import Amazonka.SecurityHub.Types.AwsEc2InstanceNetworkInterfacesDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataCpuOptionsDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataCreditSpecificationDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataEnclaveOptionsDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataHibernationOptionsDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataIamInstanceProfileDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceMarketOptionsDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataLicenseSetDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataMaintenanceOptionsDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataMetadataOptionsDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataMonitoringDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataPlacementDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDetails
import Amazonka.SecurityHub.Types.AwsEc2NetworkAclAssociation
import Amazonka.SecurityHub.Types.AwsEc2NetworkAclDetails
import Amazonka.SecurityHub.Types.AwsEc2NetworkAclEntry
import Amazonka.SecurityHub.Types.AwsEc2NetworkInterfaceAttachment
import Amazonka.SecurityHub.Types.AwsEc2NetworkInterfaceDetails
import Amazonka.SecurityHub.Types.AwsEc2NetworkInterfaceIpV6AddressDetail
import Amazonka.SecurityHub.Types.AwsEc2NetworkInterfacePrivateIpAddressDetail
import Amazonka.SecurityHub.Types.AwsEc2NetworkInterfaceSecurityGroup
import Amazonka.SecurityHub.Types.AwsEc2RouteTableDetails
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
import Amazonka.SecurityHub.Types.AwsEventSchemasRegistryDetails
import Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesCloudTrailDetails
import Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesDetails
import Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesDnsLogsDetails
import Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesFlowLogsDetails
import Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesKubernetesAuditLogsDetails
import Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesKubernetesDetails
import Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesMalwareProtectionDetails
import Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesMalwareProtectionScanEc2InstanceWithFindingsDetails
import Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesMalwareProtectionScanEc2InstanceWithFindingsEbsVolumesDetails
import Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesS3LogsDetails
import Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDetails
import Amazonka.SecurityHub.Types.AwsGuardDutyDetectorFeaturesDetails
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
import Amazonka.SecurityHub.Types.AwsS3BucketObjectLockConfiguration
import Amazonka.SecurityHub.Types.AwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails
import Amazonka.SecurityHub.Types.AwsS3BucketObjectLockConfigurationRuleDetails
import Amazonka.SecurityHub.Types.AwsS3BucketServerSideEncryptionByDefault
import Amazonka.SecurityHub.Types.AwsS3BucketServerSideEncryptionConfiguration
import Amazonka.SecurityHub.Types.AwsS3BucketServerSideEncryptionRule
import Amazonka.SecurityHub.Types.AwsS3BucketWebsiteConfiguration
import Amazonka.SecurityHub.Types.AwsS3BucketWebsiteConfigurationRedirectTo
import Amazonka.SecurityHub.Types.AwsS3BucketWebsiteConfigurationRoutingRule
import Amazonka.SecurityHub.Types.AwsS3BucketWebsiteConfigurationRoutingRuleCondition
import Amazonka.SecurityHub.Types.AwsS3BucketWebsiteConfigurationRoutingRuleRedirect
import Amazonka.SecurityHub.Types.AwsS3ObjectDetails
import Amazonka.SecurityHub.Types.AwsSageMakerNotebookInstanceDetails
import Amazonka.SecurityHub.Types.AwsSageMakerNotebookInstanceMetadataServiceConfigurationDetails
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
import Amazonka.SecurityHub.Types.AwsStepFunctionStateMachineDetails
import Amazonka.SecurityHub.Types.AwsStepFunctionStateMachineLoggingConfigurationDestinationsCloudWatchLogsLogGroupDetails
import Amazonka.SecurityHub.Types.AwsStepFunctionStateMachineLoggingConfigurationDestinationsDetails
import Amazonka.SecurityHub.Types.AwsStepFunctionStateMachineLoggingConfigurationDetails
import Amazonka.SecurityHub.Types.AwsStepFunctionStateMachineTracingConfigurationDetails
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
import Amazonka.SecurityHub.Types.AwsWafv2ActionAllowDetails
import Amazonka.SecurityHub.Types.AwsWafv2ActionBlockDetails
import Amazonka.SecurityHub.Types.AwsWafv2CustomHttpHeader
import Amazonka.SecurityHub.Types.AwsWafv2CustomRequestHandlingDetails
import Amazonka.SecurityHub.Types.AwsWafv2CustomResponseDetails
import Amazonka.SecurityHub.Types.AwsWafv2RuleGroupDetails
import Amazonka.SecurityHub.Types.AwsWafv2RulesActionCaptchaDetails
import Amazonka.SecurityHub.Types.AwsWafv2RulesActionCountDetails
import Amazonka.SecurityHub.Types.AwsWafv2RulesActionDetails
import Amazonka.SecurityHub.Types.AwsWafv2RulesDetails
import Amazonka.SecurityHub.Types.AwsWafv2VisibilityConfigDetails
import Amazonka.SecurityHub.Types.AwsWafv2WebAclActionDetails
import Amazonka.SecurityHub.Types.AwsWafv2WebAclCaptchaConfigDetails
import Amazonka.SecurityHub.Types.AwsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails
import Amazonka.SecurityHub.Types.AwsWafv2WebAclDetails
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
import Amazonka.SecurityHub.Types.ControlFindingGenerator
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
import Amazonka.SecurityHub.Types.FindingHistoryRecord
import Amazonka.SecurityHub.Types.FindingHistoryUpdate
import Amazonka.SecurityHub.Types.FindingHistoryUpdateSource
import Amazonka.SecurityHub.Types.FindingHistoryUpdateSourceType
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
import Amazonka.SecurityHub.Types.PropagatingVgwSetDetails
import Amazonka.SecurityHub.Types.Range
import Amazonka.SecurityHub.Types.Recommendation
import Amazonka.SecurityHub.Types.Record
import Amazonka.SecurityHub.Types.RecordState
import Amazonka.SecurityHub.Types.RegionAvailabilityStatus
import Amazonka.SecurityHub.Types.RelatedFinding
import Amazonka.SecurityHub.Types.Remediation
import Amazonka.SecurityHub.Types.Resource
import Amazonka.SecurityHub.Types.ResourceDetails
import Amazonka.SecurityHub.Types.Result
import Amazonka.SecurityHub.Types.RouteSetDetails
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
import Amazonka.SecurityHub.Types.RuleStatus
import Amazonka.SecurityHub.Types.SecurityControl
import Amazonka.SecurityHub.Types.SecurityControlDefinition
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
import Amazonka.SecurityHub.Types.StandardsControlAssociationDetail
import Amazonka.SecurityHub.Types.StandardsControlAssociationId
import Amazonka.SecurityHub.Types.StandardsControlAssociationSummary
import Amazonka.SecurityHub.Types.StandardsControlAssociationUpdate
import Amazonka.SecurityHub.Types.StandardsManagedBy
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
import Amazonka.SecurityHub.Types.UnprocessedAutomationRule
import Amazonka.SecurityHub.Types.UnprocessedErrorCode
import Amazonka.SecurityHub.Types.UnprocessedSecurityControl
import Amazonka.SecurityHub.Types.UnprocessedStandardsControlAssociation
import Amazonka.SecurityHub.Types.UnprocessedStandardsControlAssociationUpdate
import Amazonka.SecurityHub.Types.UpdateAutomationRulesRequestItem
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

-- | You don\'t have permission to perform the action specified in the
-- request.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | Internal server error.
_InternalException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalException =
  Core._MatchServiceError
    defaultService
    "InternalException"
    Prelude.. Core.hasStatus 500

-- | The account doesn\'t have permission to perform this action.
_InvalidAccessException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidAccessException =
  Core._MatchServiceError
    defaultService
    "InvalidAccessException"
    Prelude.. Core.hasStatus 401

-- | The request was rejected because you supplied an invalid or out-of-range
-- value for an input parameter.
_InvalidInputException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidInputException =
  Core._MatchServiceError
    defaultService
    "InvalidInputException"
    Prelude.. Core.hasStatus 400

-- | The request was rejected because it attempted to create resources beyond
-- the current Amazon Web Services account or throttling limits. The error
-- code describes the limit exceeded.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 429

-- | The resource specified in the request conflicts with an existing
-- resource.
_ResourceConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceConflictException =
  Core._MatchServiceError
    defaultService
    "ResourceConflictException"
    Prelude.. Core.hasStatus 409

-- | The request was rejected because we can\'t find the specified resource.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

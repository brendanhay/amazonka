{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS IoT__
--
-- AWS IoT provides secure, bi-directional communication between Internet-connected devices (such as sensors, actuators, embedded devices, or smart appliances) and the AWS cloud. You can discover your custom IoT-Data endpoint to communicate with, configure rules for data processing and integration with other services, organize resources associated with each device (Registry), configure logging, and create and manage policies and credentials to authenticate devices.
--
-- The service endpoints that expose this API are listed in <https://docs.aws.amazon.com/general/latest/gr/iot-core.html AWS IoT Core Endpoints and Quotas> . You must use the endpoint for the region that has the resources you want to access.
--
-- The service name used by <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html AWS Signature Version 4> to sign the request is: /execute-api/ .
--
-- For more information about how AWS IoT works, see the <https://docs.aws.amazon.com/iot/latest/developerguide/aws-iot-how-it-works.html Developer Guide> .
--
-- For information about how to use the credentials provider for AWS IoT, see <https://docs.aws.amazon.com/iot/latest/developerguide/authorizing-direct-aws.html Authorizing Direct Calls to AWS Services> .
module Network.AWS.IoT
  ( -- * Service Configuration
    ioT,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetCardinality
    module Network.AWS.IoT.GetCardinality,

    -- ** CreateDomainConfiguration
    module Network.AWS.IoT.CreateDomainConfiguration,

    -- ** DeleteSecurityProfile
    module Network.AWS.IoT.DeleteSecurityProfile,

    -- ** UpdateSecurityProfile
    module Network.AWS.IoT.UpdateSecurityProfile,

    -- ** ListSecurityProfiles (Paginated)
    module Network.AWS.IoT.ListSecurityProfiles,

    -- ** ListPolicies (Paginated)
    module Network.AWS.IoT.ListPolicies,

    -- ** DescribeProvisioningTemplate
    module Network.AWS.IoT.DescribeProvisioningTemplate,

    -- ** UpdateMitigationAction
    module Network.AWS.IoT.UpdateMitigationAction,

    -- ** DeleteMitigationAction
    module Network.AWS.IoT.DeleteMitigationAction,

    -- ** DeleteJobExecution
    module Network.AWS.IoT.DeleteJobExecution,

    -- ** CreatePolicy
    module Network.AWS.IoT.CreatePolicy,

    -- ** RegisterCertificate
    module Network.AWS.IoT.RegisterCertificate,

    -- ** DeleteDynamicThingGroup
    module Network.AWS.IoT.DeleteDynamicThingGroup,

    -- ** ListThingPrincipals (Paginated)
    module Network.AWS.IoT.ListThingPrincipals,

    -- ** UpdateDynamicThingGroup
    module Network.AWS.IoT.UpdateDynamicThingGroup,

    -- ** DescribeRoleAlias
    module Network.AWS.IoT.DescribeRoleAlias,

    -- ** CreateProvisioningTemplateVersion
    module Network.AWS.IoT.CreateProvisioningTemplateVersion,

    -- ** CreateOTAUpdate
    module Network.AWS.IoT.CreateOTAUpdate,

    -- ** DescribeDefaultAuthorizer
    module Network.AWS.IoT.DescribeDefaultAuthorizer,

    -- ** ListAuditMitigationActionsTasks (Paginated)
    module Network.AWS.IoT.ListAuditMitigationActionsTasks,

    -- ** ListThingRegistrationTaskReports (Paginated)
    module Network.AWS.IoT.ListThingRegistrationTaskReports,

    -- ** ListPrincipalThings (Paginated)
    module Network.AWS.IoT.ListPrincipalThings,

    -- ** RemoveThingFromThingGroup
    module Network.AWS.IoT.RemoveThingFromThingGroup,

    -- ** DescribeEventConfigurations
    module Network.AWS.IoT.DescribeEventConfigurations,

    -- ** ListTopicRuleDestinations (Paginated)
    module Network.AWS.IoT.ListTopicRuleDestinations,

    -- ** RegisterCertificateWithoutCA
    module Network.AWS.IoT.RegisterCertificateWithoutCA,

    -- ** ListTagsForResource (Paginated)
    module Network.AWS.IoT.ListTagsForResource,

    -- ** ListThingGroups (Paginated)
    module Network.AWS.IoT.ListThingGroups,

    -- ** ListScheduledAudits (Paginated)
    module Network.AWS.IoT.ListScheduledAudits,

    -- ** DescribeThingRegistrationTask
    module Network.AWS.IoT.DescribeThingRegistrationTask,

    -- ** UpdateScheduledAudit
    module Network.AWS.IoT.UpdateScheduledAudit,

    -- ** DeleteScheduledAudit
    module Network.AWS.IoT.DeleteScheduledAudit,

    -- ** DescribeAuditFinding
    module Network.AWS.IoT.DescribeAuditFinding,

    -- ** DescribeDimension
    module Network.AWS.IoT.DescribeDimension,

    -- ** GetLoggingOptions
    module Network.AWS.IoT.GetLoggingOptions,

    -- ** DeleteAccountAuditConfiguration
    module Network.AWS.IoT.DeleteAccountAuditConfiguration,

    -- ** UpdateAccountAuditConfiguration
    module Network.AWS.IoT.UpdateAccountAuditConfiguration,

    -- ** GetOTAUpdate
    module Network.AWS.IoT.GetOTAUpdate,

    -- ** GetEffectivePolicies
    module Network.AWS.IoT.GetEffectivePolicies,

    -- ** ListThingTypes (Paginated)
    module Network.AWS.IoT.ListThingTypes,

    -- ** SetV2LoggingOptions
    module Network.AWS.IoT.SetV2LoggingOptions,

    -- ** CreateProvisioningTemplate
    module Network.AWS.IoT.CreateProvisioningTemplate,

    -- ** ListThingGroupsForThing (Paginated)
    module Network.AWS.IoT.ListThingGroupsForThing,

    -- ** CreateCertificateFromCSR
    module Network.AWS.IoT.CreateCertificateFromCSR,

    -- ** DeleteThing
    module Network.AWS.IoT.DeleteThing,

    -- ** UpdateThing
    module Network.AWS.IoT.UpdateThing,

    -- ** DeleteProvisioningTemplate
    module Network.AWS.IoT.DeleteProvisioningTemplate,

    -- ** UpdateProvisioningTemplate
    module Network.AWS.IoT.UpdateProvisioningTemplate,

    -- ** DescribeMitigationAction
    module Network.AWS.IoT.DescribeMitigationAction,

    -- ** StartThingRegistrationTask
    module Network.AWS.IoT.StartThingRegistrationTask,

    -- ** CreateScheduledAudit
    module Network.AWS.IoT.CreateScheduledAudit,

    -- ** ListAuthorizers (Paginated)
    module Network.AWS.IoT.ListAuthorizers,

    -- ** ListJobExecutionsForJob (Paginated)
    module Network.AWS.IoT.ListJobExecutionsForJob,

    -- ** RemoveThingFromBillingGroup
    module Network.AWS.IoT.RemoveThingFromBillingGroup,

    -- ** SearchIndex
    module Network.AWS.IoT.SearchIndex,

    -- ** CreateThingType
    module Network.AWS.IoT.CreateThingType,

    -- ** DescribeSecurityProfile
    module Network.AWS.IoT.DescribeSecurityProfile,

    -- ** DeleteV2LoggingLevel
    module Network.AWS.IoT.DeleteV2LoggingLevel,

    -- ** SetDefaultAuthorizer
    module Network.AWS.IoT.SetDefaultAuthorizer,

    -- ** DescribeJobExecution
    module Network.AWS.IoT.DescribeJobExecution,

    -- ** CancelCertificateTransfer
    module Network.AWS.IoT.CancelCertificateTransfer,

    -- ** GetIndexingConfiguration
    module Network.AWS.IoT.GetIndexingConfiguration,

    -- ** ListAuditMitigationActionsExecutions (Paginated)
    module Network.AWS.IoT.ListAuditMitigationActionsExecutions,

    -- ** DescribeAuditMitigationActionsTask
    module Network.AWS.IoT.DescribeAuditMitigationActionsTask,

    -- ** GetStatistics
    module Network.AWS.IoT.GetStatistics,

    -- ** DeleteRoleAlias
    module Network.AWS.IoT.DeleteRoleAlias,

    -- ** UpdateRoleAlias
    module Network.AWS.IoT.UpdateRoleAlias,

    -- ** DeletePolicyVersion
    module Network.AWS.IoT.DeletePolicyVersion,

    -- ** DisableTopicRule
    module Network.AWS.IoT.DisableTopicRule,

    -- ** CreateTopicRule
    module Network.AWS.IoT.CreateTopicRule,

    -- ** CreateJob
    module Network.AWS.IoT.CreateJob,

    -- ** DescribeIndex
    module Network.AWS.IoT.DescribeIndex,

    -- ** AssociateTargetsWithJob
    module Network.AWS.IoT.AssociateTargetsWithJob,

    -- ** AttachSecurityProfile
    module Network.AWS.IoT.AttachSecurityProfile,

    -- ** ListAttachedPolicies (Paginated)
    module Network.AWS.IoT.ListAttachedPolicies,

    -- ** CreatePolicyVersion
    module Network.AWS.IoT.CreatePolicyVersion,

    -- ** ListCACertificates (Paginated)
    module Network.AWS.IoT.ListCACertificates,

    -- ** DeleteTopicRule
    module Network.AWS.IoT.DeleteTopicRule,

    -- ** GetJobDocument
    module Network.AWS.IoT.GetJobDocument,

    -- ** DescribeProvisioningTemplateVersion
    module Network.AWS.IoT.DescribeProvisioningTemplateVersion,

    -- ** CancelAuditTask
    module Network.AWS.IoT.CancelAuditTask,

    -- ** CreateRoleAlias
    module Network.AWS.IoT.CreateRoleAlias,

    -- ** DeleteCACertificate
    module Network.AWS.IoT.DeleteCACertificate,

    -- ** UpdateCACertificate
    module Network.AWS.IoT.UpdateCACertificate,

    -- ** ListTopicRules (Paginated)
    module Network.AWS.IoT.ListTopicRules,

    -- ** TransferCertificate
    module Network.AWS.IoT.TransferCertificate,

    -- ** ListJobs (Paginated)
    module Network.AWS.IoT.ListJobs,

    -- ** ListRoleAliases (Paginated)
    module Network.AWS.IoT.ListRoleAliases,

    -- ** StartOnDemandAuditTask
    module Network.AWS.IoT.StartOnDemandAuditTask,

    -- ** DescribeThingGroup
    module Network.AWS.IoT.DescribeThingGroup,

    -- ** DeleteJob
    module Network.AWS.IoT.DeleteJob,

    -- ** ListTargetsForSecurityProfile (Paginated)
    module Network.AWS.IoT.ListTargetsForSecurityProfile,

    -- ** UpdateJob
    module Network.AWS.IoT.UpdateJob,

    -- ** StartAuditMitigationActionsTask
    module Network.AWS.IoT.StartAuditMitigationActionsTask,

    -- ** GetTopicRule
    module Network.AWS.IoT.GetTopicRule,

    -- ** DescribeThing
    module Network.AWS.IoT.DescribeThing,

    -- ** ListDomainConfigurations (Paginated)
    module Network.AWS.IoT.ListDomainConfigurations,

    -- ** ListAuditTasks (Paginated)
    module Network.AWS.IoT.ListAuditTasks,

    -- ** DescribeAccountAuditConfiguration
    module Network.AWS.IoT.DescribeAccountAuditConfiguration,

    -- ** DeleteDimension
    module Network.AWS.IoT.DeleteDimension,

    -- ** UpdateDimension
    module Network.AWS.IoT.UpdateDimension,

    -- ** DeletePolicy
    module Network.AWS.IoT.DeletePolicy,

    -- ** ListThingsInThingGroup (Paginated)
    module Network.AWS.IoT.ListThingsInThingGroup,

    -- ** ListAuditFindings (Paginated)
    module Network.AWS.IoT.ListAuditFindings,

    -- ** DescribeScheduledAudit
    module Network.AWS.IoT.DescribeScheduledAudit,

    -- ** CreateMitigationAction
    module Network.AWS.IoT.CreateMitigationAction,

    -- ** ConfirmTopicRuleDestination
    module Network.AWS.IoT.ConfirmTopicRuleDestination,

    -- ** ListCertificates (Paginated)
    module Network.AWS.IoT.ListCertificates,

    -- ** ListMitigationActions (Paginated)
    module Network.AWS.IoT.ListMitigationActions,

    -- ** DescribeAuthorizer
    module Network.AWS.IoT.DescribeAuthorizer,

    -- ** GetPolicyVersion
    module Network.AWS.IoT.GetPolicyVersion,

    -- ** ListActiveViolations (Paginated)
    module Network.AWS.IoT.ListActiveViolations,

    -- ** ValidateSecurityProfileBehaviors
    module Network.AWS.IoT.ValidateSecurityProfileBehaviors,

    -- ** ListViolationEvents (Paginated)
    module Network.AWS.IoT.ListViolationEvents,

    -- ** DeleteCertificate
    module Network.AWS.IoT.DeleteCertificate,

    -- ** UpdateCertificate
    module Network.AWS.IoT.UpdateCertificate,

    -- ** CreateDimension
    module Network.AWS.IoT.CreateDimension,

    -- ** UpdateIndexingConfiguration
    module Network.AWS.IoT.UpdateIndexingConfiguration,

    -- ** CreateProvisioningClaim
    module Network.AWS.IoT.CreateProvisioningClaim,

    -- ** TestInvokeAuthorizer
    module Network.AWS.IoT.TestInvokeAuthorizer,

    -- ** CreateThingGroup
    module Network.AWS.IoT.CreateThingGroup,

    -- ** CreateTopicRuleDestination
    module Network.AWS.IoT.CreateTopicRuleDestination,

    -- ** DetachPolicy
    module Network.AWS.IoT.DetachPolicy,

    -- ** DescribeJob
    module Network.AWS.IoT.DescribeJob,

    -- ** AddThingToBillingGroup
    module Network.AWS.IoT.AddThingToBillingGroup,

    -- ** UpdateTopicRuleDestination
    module Network.AWS.IoT.UpdateTopicRuleDestination,

    -- ** DeleteTopicRuleDestination
    module Network.AWS.IoT.DeleteTopicRuleDestination,

    -- ** DeleteThingGroup
    module Network.AWS.IoT.DeleteThingGroup,

    -- ** UpdateThingGroup
    module Network.AWS.IoT.UpdateThingGroup,

    -- ** ListOTAUpdates (Paginated)
    module Network.AWS.IoT.ListOTAUpdates,

    -- ** DeleteOTAUpdate
    module Network.AWS.IoT.DeleteOTAUpdate,

    -- ** CreateDynamicThingGroup
    module Network.AWS.IoT.CreateDynamicThingGroup,

    -- ** DetachSecurityProfile
    module Network.AWS.IoT.DetachSecurityProfile,

    -- ** ListOutgoingCertificates (Paginated)
    module Network.AWS.IoT.ListOutgoingCertificates,

    -- ** DeleteProvisioningTemplateVersion
    module Network.AWS.IoT.DeleteProvisioningTemplateVersion,

    -- ** DescribeCACertificate
    module Network.AWS.IoT.DescribeCACertificate,

    -- ** ListProvisioningTemplateVersions (Paginated)
    module Network.AWS.IoT.ListProvisioningTemplateVersions,

    -- ** GetRegistrationCode
    module Network.AWS.IoT.GetRegistrationCode,

    -- ** ListBillingGroups (Paginated)
    module Network.AWS.IoT.ListBillingGroups,

    -- ** DeleteThingType
    module Network.AWS.IoT.DeleteThingType,

    -- ** DeleteBillingGroup
    module Network.AWS.IoT.DeleteBillingGroup,

    -- ** AddThingToThingGroup
    module Network.AWS.IoT.AddThingToThingGroup,

    -- ** UpdateBillingGroup
    module Network.AWS.IoT.UpdateBillingGroup,

    -- ** GetTopicRuleDestination
    module Network.AWS.IoT.GetTopicRuleDestination,

    -- ** ListCertificatesByCA (Paginated)
    module Network.AWS.IoT.ListCertificatesByCA,

    -- ** UpdateAuditSuppression
    module Network.AWS.IoT.UpdateAuditSuppression,

    -- ** AttachThingPrincipal
    module Network.AWS.IoT.AttachThingPrincipal,

    -- ** ListThings (Paginated)
    module Network.AWS.IoT.ListThings,

    -- ** DeleteAuditSuppression
    module Network.AWS.IoT.DeleteAuditSuppression,

    -- ** RegisterThing
    module Network.AWS.IoT.RegisterThing,

    -- ** ListAuditSuppressions (Paginated)
    module Network.AWS.IoT.ListAuditSuppressions,

    -- ** DescribeDomainConfiguration
    module Network.AWS.IoT.DescribeDomainConfiguration,

    -- ** DescribeAuditTask
    module Network.AWS.IoT.DescribeAuditTask,

    -- ** DeleteRegistrationCode
    module Network.AWS.IoT.DeleteRegistrationCode,

    -- ** UpdateStream
    module Network.AWS.IoT.UpdateStream,

    -- ** DeleteStream
    module Network.AWS.IoT.DeleteStream,

    -- ** ListStreams (Paginated)
    module Network.AWS.IoT.ListStreams,

    -- ** CreateAuthorizer
    module Network.AWS.IoT.CreateAuthorizer,

    -- ** TestAuthorization
    module Network.AWS.IoT.TestAuthorization,

    -- ** ListIndices (Paginated)
    module Network.AWS.IoT.ListIndices,

    -- ** UpdateAuthorizer
    module Network.AWS.IoT.UpdateAuthorizer,

    -- ** DeleteAuthorizer
    module Network.AWS.IoT.DeleteAuthorizer,

    -- ** CreateThing
    module Network.AWS.IoT.CreateThing,

    -- ** CreateStream
    module Network.AWS.IoT.CreateStream,

    -- ** CancelAuditMitigationActionsTask
    module Network.AWS.IoT.CancelAuditMitigationActionsTask,

    -- ** CreateAuditSuppression
    module Network.AWS.IoT.CreateAuditSuppression,

    -- ** CreateBillingGroup
    module Network.AWS.IoT.CreateBillingGroup,

    -- ** ListProvisioningTemplates (Paginated)
    module Network.AWS.IoT.ListProvisioningTemplates,

    -- ** ListV2LoggingLevels (Paginated)
    module Network.AWS.IoT.ListV2LoggingLevels,

    -- ** TagResource
    module Network.AWS.IoT.TagResource,

    -- ** StopThingRegistrationTask
    module Network.AWS.IoT.StopThingRegistrationTask,

    -- ** DescribeCertificate
    module Network.AWS.IoT.DescribeCertificate,

    -- ** ListTargetsForPolicy (Paginated)
    module Network.AWS.IoT.ListTargetsForPolicy,

    -- ** ClearDefaultAuthorizer
    module Network.AWS.IoT.ClearDefaultAuthorizer,

    -- ** ReplaceTopicRule
    module Network.AWS.IoT.ReplaceTopicRule,

    -- ** UntagResource
    module Network.AWS.IoT.UntagResource,

    -- ** SetDefaultPolicyVersion
    module Network.AWS.IoT.SetDefaultPolicyVersion,

    -- ** CancelJobExecution
    module Network.AWS.IoT.CancelJobExecution,

    -- ** ListPolicyVersions
    module Network.AWS.IoT.ListPolicyVersions,

    -- ** SetV2LoggingLevel
    module Network.AWS.IoT.SetV2LoggingLevel,

    -- ** ListJobExecutionsForThing (Paginated)
    module Network.AWS.IoT.ListJobExecutionsForThing,

    -- ** AttachPolicy
    module Network.AWS.IoT.AttachPolicy,

    -- ** CreateKeysAndCertificate
    module Network.AWS.IoT.CreateKeysAndCertificate,

    -- ** ListThingsInBillingGroup (Paginated)
    module Network.AWS.IoT.ListThingsInBillingGroup,

    -- ** UpdateThingGroupsForThing
    module Network.AWS.IoT.UpdateThingGroupsForThing,

    -- ** EnableTopicRule
    module Network.AWS.IoT.EnableTopicRule,

    -- ** AcceptCertificateTransfer
    module Network.AWS.IoT.AcceptCertificateTransfer,

    -- ** GetPercentiles
    module Network.AWS.IoT.GetPercentiles,

    -- ** GetPolicy
    module Network.AWS.IoT.GetPolicy,

    -- ** DescribeEndpoint
    module Network.AWS.IoT.DescribeEndpoint,

    -- ** ListSecurityProfilesForTarget (Paginated)
    module Network.AWS.IoT.ListSecurityProfilesForTarget,

    -- ** UpdateEventConfigurations
    module Network.AWS.IoT.UpdateEventConfigurations,

    -- ** RegisterCACertificate
    module Network.AWS.IoT.RegisterCACertificate,

    -- ** DeleteDomainConfiguration
    module Network.AWS.IoT.DeleteDomainConfiguration,

    -- ** UpdateDomainConfiguration
    module Network.AWS.IoT.UpdateDomainConfiguration,

    -- ** SetLoggingOptions
    module Network.AWS.IoT.SetLoggingOptions,

    -- ** DescribeThingType
    module Network.AWS.IoT.DescribeThingType,

    -- ** ListDimensions (Paginated)
    module Network.AWS.IoT.ListDimensions,

    -- ** GetV2LoggingOptions
    module Network.AWS.IoT.GetV2LoggingOptions,

    -- ** ListThingRegistrationTasks (Paginated)
    module Network.AWS.IoT.ListThingRegistrationTasks,

    -- ** RejectCertificateTransfer
    module Network.AWS.IoT.RejectCertificateTransfer,

    -- ** DescribeAuditSuppression
    module Network.AWS.IoT.DescribeAuditSuppression,

    -- ** DescribeStream
    module Network.AWS.IoT.DescribeStream,

    -- ** CreateSecurityProfile
    module Network.AWS.IoT.CreateSecurityProfile,

    -- ** DescribeBillingGroup
    module Network.AWS.IoT.DescribeBillingGroup,

    -- ** DetachThingPrincipal
    module Network.AWS.IoT.DetachThingPrincipal,

    -- ** CancelJob
    module Network.AWS.IoT.CancelJob,

    -- ** DeprecateThingType
    module Network.AWS.IoT.DeprecateThingType,

    -- * Types

    -- ** AWSJobAbortCriteriaAbortAction
    AWSJobAbortCriteriaAbortAction (..),

    -- ** AWSJobAbortCriteriaFailureType
    AWSJobAbortCriteriaFailureType (..),

    -- ** AbortAction
    AbortAction (..),

    -- ** ActionType
    ActionType (..),

    -- ** AlertTargetType
    AlertTargetType (..),

    -- ** AuditCheckRunStatus
    AuditCheckRunStatus (..),

    -- ** AuditFindingSeverity
    AuditFindingSeverity (..),

    -- ** AuditFrequency
    AuditFrequency (..),

    -- ** AuditMitigationActionsExecutionStatus
    AuditMitigationActionsExecutionStatus (..),

    -- ** AuditMitigationActionsTaskStatus
    AuditMitigationActionsTaskStatus (..),

    -- ** AuditNotificationType
    AuditNotificationType (..),

    -- ** AuditTaskStatus
    AuditTaskStatus (..),

    -- ** AuditTaskType
    AuditTaskType (..),

    -- ** AuthDecision
    AuthDecision (..),

    -- ** AuthorizerStatus
    AuthorizerStatus (..),

    -- ** AutoRegistrationStatus
    AutoRegistrationStatus (..),

    -- ** CACertificateStatus
    CACertificateStatus (..),

    -- ** CACertificateUpdateAction
    CACertificateUpdateAction (..),

    -- ** CannedAccessControlList
    CannedAccessControlList (..),

    -- ** CertificateMode
    CertificateMode (..),

    -- ** CertificateStatus
    CertificateStatus (..),

    -- ** ComparisonOperator
    ComparisonOperator (..),

    -- ** DayOfWeek
    DayOfWeek (..),

    -- ** DeviceCertificateUpdateAction
    DeviceCertificateUpdateAction (..),

    -- ** DimensionType
    DimensionType (..),

    -- ** DimensionValueOperator
    DimensionValueOperator (..),

    -- ** DomainConfigurationStatus
    DomainConfigurationStatus (..),

    -- ** DomainType
    DomainType (..),

    -- ** DynamicGroupStatus
    DynamicGroupStatus (..),

    -- ** DynamoKeyType
    DynamoKeyType (..),

    -- ** EventType
    EventType (..),

    -- ** FieldType
    FieldType (..),

    -- ** IndexStatus
    IndexStatus (..),

    -- ** JobExecutionFailureType
    JobExecutionFailureType (..),

    -- ** JobExecutionStatus
    JobExecutionStatus (..),

    -- ** JobStatus
    JobStatus (..),

    -- ** LogLevel
    LogLevel (..),

    -- ** LogTargetType
    LogTargetType (..),

    -- ** MessageFormat
    MessageFormat (..),

    -- ** MitigationActionType
    MitigationActionType (..),

    -- ** OTAUpdateStatus
    OTAUpdateStatus (..),

    -- ** PolicyTemplateName
    PolicyTemplateName (..),

    -- ** Protocol
    Protocol (..),

    -- ** ReportType
    ReportType (..),

    -- ** ResourceType
    ResourceType (..),

    -- ** ServerCertificateStatus
    ServerCertificateStatus (..),

    -- ** ServiceType
    ServiceType (..),

    -- ** TargetSelection
    TargetSelection (..),

    -- ** TaskStatus
    TaskStatus (..),

    -- ** ThingConnectivityIndexingMode
    ThingConnectivityIndexingMode (..),

    -- ** ThingGroupIndexingMode
    ThingGroupIndexingMode (..),

    -- ** ThingIndexingMode
    ThingIndexingMode (..),

    -- ** TopicRuleDestinationStatus
    TopicRuleDestinationStatus (..),

    -- ** ViolationEventType
    ViolationEventType (..),

    -- ** AWSJobAbortConfig
    AWSJobAbortConfig,
    awsJobAbortConfig,
    ajacAbortCriteriaList,

    -- ** AWSJobAbortCriteria
    AWSJobAbortCriteria,
    awsJobAbortCriteria,
    ajacFailureType,
    ajacAction,
    ajacThresholdPercentage,
    ajacMinNumberOfExecutedThings,

    -- ** AWSJobExecutionsRolloutConfig
    AWSJobExecutionsRolloutConfig,
    awsJobExecutionsRolloutConfig,
    ajercExponentialRate,
    ajercMaximumPerMinute,

    -- ** AWSJobExponentialRolloutRate
    AWSJobExponentialRolloutRate,
    awsJobExponentialRolloutRate,
    ajerrBaseRatePerMinute,
    ajerrIncrementFactor,
    ajerrRateIncreaseCriteria,

    -- ** AWSJobPresignedURLConfig
    AWSJobPresignedURLConfig,
    awsJobPresignedURLConfig,
    ajpucExpiresInSec,

    -- ** AWSJobRateIncreaseCriteria
    AWSJobRateIncreaseCriteria,
    awsJobRateIncreaseCriteria,
    ajricNumberOfNotifiedThings,
    ajricNumberOfSucceededThings,

    -- ** AWSJobTimeoutConfig
    AWSJobTimeoutConfig,
    awsJobTimeoutConfig,
    ajtcInProgressTimeoutInMinutes,

    -- ** AbortConfig
    AbortConfig,
    abortConfig,
    acCriteriaList,

    -- ** AbortCriteria
    AbortCriteria,
    abortCriteria,
    acFailureType,
    acAction,
    acThresholdPercentage,
    acMinNumberOfExecutedThings,

    -- ** Action
    Action,
    action,
    aCloudwatchMetric,
    aCloudwatchLogs,
    aDynamoDBv2,
    aStepFunctions,
    aCloudwatchAlarm,
    aSns,
    aDynamoDB,
    aFirehose,
    aTimestream,
    aIotSiteWise,
    aIotAnalytics,
    aLambda,
    aIotEvents,
    aSalesforce,
    aKinesis,
    aS3,
    aHttp,
    aElasticsearch,
    aRepublish,
    aSqs,

    -- ** ActiveViolation
    ActiveViolation,
    activeViolation,
    avLastViolationValue,
    avLastViolationTime,
    avViolationStartTime,
    avViolationId,
    avBehavior,
    avSecurityProfileName,
    avThingName,

    -- ** AddThingsToThingGroupParams
    AddThingsToThingGroupParams,
    addThingsToThingGroupParams,
    atttgpOverrideDynamicGroups,
    atttgpThingGroupNames,

    -- ** AlertTarget
    AlertTarget,
    alertTarget,
    atAlertTargetARN,
    atRoleARN,

    -- ** Allowed
    Allowed,
    allowed,
    aPolicies,

    -- ** AssetPropertyTimestamp
    AssetPropertyTimestamp,
    assetPropertyTimestamp,
    aptOffsetInNanos,
    aptTimeInSeconds,

    -- ** AssetPropertyValue
    AssetPropertyValue,
    assetPropertyValue,
    apvQuality,
    apvValue,
    apvTimestamp,

    -- ** AssetPropertyVariant
    AssetPropertyVariant,
    assetPropertyVariant,
    apvIntegerValue,
    apvDoubleValue,
    apvStringValue,
    apvBooleanValue,

    -- ** AttributePayload
    AttributePayload,
    attributePayload,
    apAttributes,
    apMerge,

    -- ** AuditCheckConfiguration
    AuditCheckConfiguration,
    auditCheckConfiguration,
    accEnabled,

    -- ** AuditCheckDetails
    AuditCheckDetails,
    auditCheckDetails,
    acdSuppressedNonCompliantResourcesCount,
    acdTotalResourcesCount,
    acdCheckCompliant,
    acdNonCompliantResourcesCount,
    acdErrorCode,
    acdMessage,
    acdCheckRunStatus,

    -- ** AuditFinding
    AuditFinding,
    auditFinding,
    afIsSuppressed,
    afTaskId,
    afFindingTime,
    afTaskStartTime,
    afReasonForNonComplianceCode,
    afSeverity,
    afRelatedResources,
    afCheckName,
    afNonCompliantResource,
    afReasonForNonCompliance,
    afFindingId,

    -- ** AuditMitigationActionExecutionMetadata
    AuditMitigationActionExecutionMetadata,
    auditMitigationActionExecutionMetadata,
    amaemStatus,
    amaemStartTime,
    amaemTaskId,
    amaemActionId,
    amaemActionName,
    amaemEndTime,
    amaemErrorCode,
    amaemFindingId,
    amaemMessage,

    -- ** AuditMitigationActionsTaskMetadata
    AuditMitigationActionsTaskMetadata,
    auditMitigationActionsTaskMetadata,
    amatmStartTime,
    amatmTaskId,
    amatmTaskStatus,

    -- ** AuditMitigationActionsTaskTarget
    AuditMitigationActionsTaskTarget,
    auditMitigationActionsTaskTarget,
    amattAuditTaskId,
    amattFindingIds,
    amattAuditCheckToReasonCodeFilter,

    -- ** AuditNotificationTarget
    AuditNotificationTarget,
    auditNotificationTarget,
    antTargetARN,
    antEnabled,
    antRoleARN,

    -- ** AuditSuppression
    AuditSuppression,
    auditSuppression,
    asExpirationDate,
    asSuppressIndefinitely,
    asDescription,
    asCheckName,
    asResourceIdentifier,

    -- ** AuditTaskMetadata
    AuditTaskMetadata,
    auditTaskMetadata,
    atmTaskType,
    atmTaskId,
    atmTaskStatus,

    -- ** AuthInfo
    AuthInfo,
    authInfo,
    aiActionType,
    aiResources,

    -- ** AuthResult
    AuthResult,
    authResult,
    arDenied,
    arAuthDecision,
    arAllowed,
    arMissingContextValues,
    arAuthInfo,

    -- ** AuthorizerConfig
    AuthorizerConfig,
    authorizerConfig,
    acAllowAuthorizerOverride,
    acDefaultAuthorizerName,

    -- ** AuthorizerDescription
    AuthorizerDescription,
    authorizerDescription,
    adStatus,
    adLastModifiedDate,
    adSigningDisabled,
    adAuthorizerName,
    adAuthorizerFunctionARN,
    adAuthorizerARN,
    adCreationDate,
    adTokenSigningPublicKeys,
    adTokenKeyName,

    -- ** AuthorizerSummary
    AuthorizerSummary,
    authorizerSummary,
    asAuthorizerName,
    asAuthorizerARN,

    -- ** Behavior
    Behavior,
    behavior,
    bMetricDimension,
    bMetric,
    bCriteria,
    bName,

    -- ** BehaviorCriteria
    BehaviorCriteria,
    behaviorCriteria,
    bcValue,
    bcConsecutiveDatapointsToAlarm,
    bcComparisonOperator,
    bcStatisticalThreshold,
    bcDurationSeconds,
    bcConsecutiveDatapointsToClear,

    -- ** BillingGroupMetadata
    BillingGroupMetadata,
    billingGroupMetadata,
    bgmCreationDate,

    -- ** BillingGroupProperties
    BillingGroupProperties,
    billingGroupProperties,
    bgpBillingGroupDescription,

    -- ** CACertificate
    CACertificate,
    cACertificate,
    cacStatus,
    cacCertificateARN,
    cacCertificateId,
    cacCreationDate,

    -- ** CACertificateDescription
    CACertificateDescription,
    cACertificateDescription,
    cacdStatus,
    cacdOwnedBy,
    cacdLastModifiedDate,
    cacdCertificatePem,
    cacdCertificateARN,
    cacdCertificateId,
    cacdValidity,
    cacdAutoRegistrationStatus,
    cacdCreationDate,
    cacdGenerationId,
    cacdCustomerVersion,

    -- ** Certificate
    Certificate,
    certificate,
    cStatus,
    cCertificateARN,
    cCertificateId,
    cCertificateMode,
    cCreationDate,

    -- ** CertificateDescription
    CertificateDescription,
    certificateDescription,
    cdStatus,
    cdOwnedBy,
    cdLastModifiedDate,
    cdCaCertificateId,
    cdPreviousOwnedBy,
    cdCertificatePem,
    cdCertificateARN,
    cdCertificateId,
    cdCertificateMode,
    cdValidity,
    cdCreationDate,
    cdGenerationId,
    cdTransferData,
    cdCustomerVersion,

    -- ** CertificateValidity
    CertificateValidity,
    certificateValidity,
    cvNotBefore,
    cvNotAfter,

    -- ** CloudwatchAlarmAction
    CloudwatchAlarmAction,
    cloudwatchAlarmAction,
    caaRoleARN,
    caaAlarmName,
    caaStateReason,
    caaStateValue,

    -- ** CloudwatchLogsAction
    CloudwatchLogsAction,
    cloudwatchLogsAction,
    claRoleARN,
    claLogGroupName,

    -- ** CloudwatchMetricAction
    CloudwatchMetricAction,
    cloudwatchMetricAction,
    cmaMetricTimestamp,
    cmaRoleARN,
    cmaMetricNamespace,
    cmaMetricName,
    cmaMetricValue,
    cmaMetricUnit,

    -- ** CodeSigning
    CodeSigning,
    codeSigning,
    csCustomCodeSigning,
    csStartSigningJobParameter,
    csAwsSignerJobId,

    -- ** CodeSigningCertificateChain
    CodeSigningCertificateChain,
    codeSigningCertificateChain,
    csccCertificateName,
    csccInlineDocument,

    -- ** CodeSigningSignature
    CodeSigningSignature,
    codeSigningSignature,
    cssInlineDocument,

    -- ** Configuration
    Configuration,
    configuration,
    cEnabled,

    -- ** CustomCodeSigning
    CustomCodeSigning,
    customCodeSigning,
    ccsSignature,
    ccsHashAlgorithm,
    ccsCertificateChain,
    ccsSignatureAlgorithm,

    -- ** Denied
    Denied,
    denied,
    dImplicitDeny,
    dExplicitDeny,

    -- ** Destination
    Destination,
    destination,
    dS3Destination,

    -- ** DomainConfigurationSummary
    DomainConfigurationSummary,
    domainConfigurationSummary,
    dcsDomainConfigurationName,
    dcsDomainConfigurationARN,
    dcsServiceType,

    -- ** DynamoDBAction
    DynamoDBAction,
    dynamoDBAction,
    ddbaHashKeyType,
    ddbaOperation,
    ddbaRangeKeyType,
    ddbaPayloadField,
    ddbaRangeKeyField,
    ddbaRangeKeyValue,
    ddbaTableName,
    ddbaRoleARN,
    ddbaHashKeyField,
    ddbaHashKeyValue,

    -- ** DynamoDBv2Action
    DynamoDBv2Action,
    dynamoDBv2Action,
    ddaRoleARN,
    ddaPutItem,

    -- ** EffectivePolicy
    EffectivePolicy,
    effectivePolicy,
    epPolicyName,
    epPolicyDocument,
    epPolicyARN,

    -- ** ElasticsearchAction
    ElasticsearchAction,
    elasticsearchAction,
    eaRoleARN,
    eaEndpoint,
    eaIndex,
    eaType,
    eaId,

    -- ** EnableIOTLoggingParams
    EnableIOTLoggingParams,
    enableIOTLoggingParams,
    eiotlpRoleARNForLogging,
    eiotlpLogLevel,

    -- ** ErrorInfo
    ErrorInfo,
    errorInfo,
    eiCode,
    eiMessage,

    -- ** ExplicitDeny
    ExplicitDeny,
    explicitDeny,
    edPolicies,

    -- ** ExponentialRolloutRate
    ExponentialRolloutRate,
    exponentialRolloutRate,
    errBaseRatePerMinute,
    errIncrementFactor,
    errRateIncreaseCriteria,

    -- ** Field
    Field,
    field,
    fName,
    fType,

    -- ** FileLocation
    FileLocation,
    fileLocation,
    flStream,
    flS3Location,

    -- ** FirehoseAction
    FirehoseAction,
    firehoseAction,
    faBatchMode,
    faSeparator,
    faRoleARN,
    faDeliveryStreamName,

    -- ** GroupNameAndARN
    GroupNameAndARN,
    groupNameAndARN,
    gnaaGroupARN,
    gnaaGroupName,

    -- ** HTTPAction
    HTTPAction,
    hTTPAction,
    httpaConfirmationURL,
    httpaAuth,
    httpaHeaders,
    httpaUrl,

    -- ** HTTPActionHeader
    HTTPActionHeader,
    hTTPActionHeader,
    httpahKey,
    httpahValue,

    -- ** HTTPAuthorization
    HTTPAuthorization,
    hTTPAuthorization,
    httpaSigv4,

    -- ** HTTPContext
    HTTPContext,
    hTTPContext,
    httpcHeaders,
    httpcQueryString,

    -- ** HTTPURLDestinationConfiguration
    HTTPURLDestinationConfiguration,
    hTTPURLDestinationConfiguration,
    httpudcConfirmationURL,

    -- ** HTTPURLDestinationProperties
    HTTPURLDestinationProperties,
    hTTPURLDestinationProperties,
    httpudpConfirmationURL,

    -- ** HTTPURLDestinationSummary
    HTTPURLDestinationSummary,
    hTTPURLDestinationSummary,
    httpudsConfirmationURL,

    -- ** ImplicitDeny
    ImplicitDeny,
    implicitDeny,
    idPolicies,

    -- ** IotAnalyticsAction
    IotAnalyticsAction,
    iotAnalyticsAction,
    iaaBatchMode,
    iaaChannelARN,
    iaaChannelName,
    iaaRoleARN,

    -- ** IotEventsAction
    IotEventsAction,
    iotEventsAction,
    ieaBatchMode,
    ieaMessageId,
    ieaInputName,
    ieaRoleARN,

    -- ** IotSiteWiseAction
    IotSiteWiseAction,
    iotSiteWiseAction,
    iswaPutAssetPropertyValueEntries,
    iswaRoleARN,

    -- ** Job
    Job,
    job,
    jobStatus,
    jobJobExecutionsRolloutConfig,
    jobJobId,
    jobLastUpdatedAt,
    jobJobARN,
    jobCreatedAt,
    jobAbortConfig,
    jobJobProcessDetails,
    jobNamespaceId,
    jobReasonCode,
    jobPresignedURLConfig,
    jobForceCanceled,
    jobTargets,
    jobCompletedAt,
    jobComment,
    jobDescription,
    jobTargetSelection,
    jobTimeoutConfig,

    -- ** JobExecution
    JobExecution,
    jobExecution,
    jeStatus,
    jeJobId,
    jeLastUpdatedAt,
    jeApproximateSecondsBeforeTimedOut,
    jeQueuedAt,
    jeStatusDetails,
    jeThingARN,
    jeExecutionNumber,
    jeVersionNumber,
    jeStartedAt,
    jeForceCanceled,

    -- ** JobExecutionStatusDetails
    JobExecutionStatusDetails,
    jobExecutionStatusDetails,
    jesdDetailsMap,

    -- ** JobExecutionSummary
    JobExecutionSummary,
    jobExecutionSummary,
    jesStatus,
    jesLastUpdatedAt,
    jesQueuedAt,
    jesExecutionNumber,
    jesStartedAt,

    -- ** JobExecutionSummaryForJob
    JobExecutionSummaryForJob,
    jobExecutionSummaryForJob,
    jesfjJobExecutionSummary,
    jesfjThingARN,

    -- ** JobExecutionSummaryForThing
    JobExecutionSummaryForThing,
    jobExecutionSummaryForThing,
    jesftJobId,
    jesftJobExecutionSummary,

    -- ** JobExecutionsRolloutConfig
    JobExecutionsRolloutConfig,
    jobExecutionsRolloutConfig,
    jercExponentialRate,
    jercMaximumPerMinute,

    -- ** JobProcessDetails
    JobProcessDetails,
    jobProcessDetails,
    jpdNumberOfRemovedThings,
    jpdNumberOfQueuedThings,
    jpdNumberOfFailedThings,
    jpdNumberOfSucceededThings,
    jpdNumberOfInProgressThings,
    jpdNumberOfCanceledThings,
    jpdNumberOfTimedOutThings,
    jpdNumberOfRejectedThings,
    jpdProcessingTargets,

    -- ** JobSummary
    JobSummary,
    jobSummary,
    jsStatus,
    jsJobId,
    jsLastUpdatedAt,
    jsJobARN,
    jsCreatedAt,
    jsThingGroupId,
    jsCompletedAt,
    jsTargetSelection,

    -- ** KeyPair
    KeyPair,
    keyPair,
    kpPrivateKey,
    kpPublicKey,

    -- ** KinesisAction
    KinesisAction,
    kinesisAction,
    kaPartitionKey,
    kaRoleARN,
    kaStreamName,

    -- ** LambdaAction
    LambdaAction,
    lambdaAction,
    laFunctionARN,

    -- ** LogTarget
    LogTarget,
    logTarget,
    ltTargetName,
    ltTargetType,

    -- ** LogTargetConfiguration
    LogTargetConfiguration,
    logTargetConfiguration,
    ltcLogLevel,
    ltcLogTarget,

    -- ** LoggingOptionsPayload
    LoggingOptionsPayload,
    loggingOptionsPayload,
    lopLogLevel,
    lopRoleARN,

    -- ** MetricDimension
    MetricDimension,
    metricDimension,
    mdOperator,
    mdDimensionName,

    -- ** MetricToRetain
    MetricToRetain,
    metricToRetain,
    mtrMetricDimension,
    mtrMetric,

    -- ** MetricValue
    MetricValue,
    metricValue,
    mvCidrs,
    mvCount,
    mvPorts,

    -- ** MitigationAction
    MitigationAction,
    mitigationAction,
    maActionParams,
    maName,
    maId,
    maRoleARN,

    -- ** MitigationActionIdentifier
    MitigationActionIdentifier,
    mitigationActionIdentifier,
    maiActionName,
    maiCreationDate,
    maiActionARN,

    -- ** MitigationActionParams
    MitigationActionParams,
    mitigationActionParams,
    mapEnableIOTLoggingParams,
    mapAddThingsToThingGroupParams,
    mapUpdateCACertificateParams,
    mapUpdateDeviceCertificateParams,
    mapReplaceDefaultPolicyVersionParams,
    mapPublishFindingToSNSParams,

    -- ** MqttContext
    MqttContext,
    mqttContext,
    mcClientId,
    mcUsername,
    mcPassword,

    -- ** NonCompliantResource
    NonCompliantResource,
    nonCompliantResource,
    ncrAdditionalInfo,
    ncrResourceType,
    ncrResourceIdentifier,

    -- ** OTAUpdateFile
    OTAUpdateFile,
    oTAUpdateFile,
    otaufFileLocation,
    otaufFileType,
    otaufFileVersion,
    otaufAttributes,
    otaufCodeSigning,
    otaufFileName,

    -- ** OTAUpdateInfo
    OTAUpdateInfo,
    oTAUpdateInfo,
    otauiLastModifiedDate,
    otauiAwsJobExecutionsRolloutConfig,
    otauiAwsIotJobId,
    otauiProtocols,
    otauiAwsJobPresignedURLConfig,
    otauiOtaUpdateFiles,
    otauiOtaUpdateStatus,
    otauiTargets,
    otauiAwsIotJobARN,
    otauiCreationDate,
    otauiAdditionalParameters,
    otauiOtaUpdateId,
    otauiErrorInfo,
    otauiOtaUpdateARN,
    otauiDescription,
    otauiTargetSelection,

    -- ** OTAUpdateSummary
    OTAUpdateSummary,
    oTAUpdateSummary,
    otausCreationDate,
    otausOtaUpdateId,
    otausOtaUpdateARN,

    -- ** OutgoingCertificate
    OutgoingCertificate,
    outgoingCertificate,
    ocTransferDate,
    ocCertificateARN,
    ocCertificateId,
    ocTransferredTo,
    ocCreationDate,
    ocTransferMessage,

    -- ** PercentPair
    PercentPair,
    percentPair,
    ppValue,
    ppPercent,

    -- ** Policy
    Policy,
    policy,
    pPolicyName,
    pPolicyARN,

    -- ** PolicyVersion
    PolicyVersion,
    policyVersion,
    pvVersionId,
    pvCreateDate,
    pvIsDefaultVersion,

    -- ** PolicyVersionIdentifier
    PolicyVersionIdentifier,
    policyVersionIdentifier,
    pviPolicyName,
    pviPolicyVersionId,

    -- ** PresignedURLConfig
    PresignedURLConfig,
    presignedURLConfig,
    pucExpiresInSec,
    pucRoleARN,

    -- ** ProvisioningHook
    ProvisioningHook,
    provisioningHook,
    phPayloadVersion,
    phTargetARN,

    -- ** ProvisioningTemplateSummary
    ProvisioningTemplateSummary,
    provisioningTemplateSummary,
    ptsLastModifiedDate,
    ptsTemplateName,
    ptsEnabled,
    ptsCreationDate,
    ptsTemplateARN,
    ptsDescription,

    -- ** ProvisioningTemplateVersionSummary
    ProvisioningTemplateVersionSummary,
    provisioningTemplateVersionSummary,
    ptvsVersionId,
    ptvsCreationDate,
    ptvsIsDefaultVersion,

    -- ** PublishFindingToSNSParams
    PublishFindingToSNSParams,
    publishFindingToSNSParams,
    pftspTopicARN,

    -- ** PutAssetPropertyValueEntry
    PutAssetPropertyValueEntry,
    putAssetPropertyValueEntry,
    papveEntryId,
    papvePropertyAlias,
    papvePropertyId,
    papveAssetId,
    papvePropertyValues,

    -- ** PutItemInput
    PutItemInput,
    putItemInput,
    piiTableName,

    -- ** RateIncreaseCriteria
    RateIncreaseCriteria,
    rateIncreaseCriteria,
    ricNumberOfNotifiedThings,
    ricNumberOfSucceededThings,

    -- ** RegistrationConfig
    RegistrationConfig,
    registrationConfig,
    rcTemplateBody,
    rcRoleARN,

    -- ** RelatedResource
    RelatedResource,
    relatedResource,
    rrAdditionalInfo,
    rrResourceType,
    rrResourceIdentifier,

    -- ** ReplaceDefaultPolicyVersionParams
    ReplaceDefaultPolicyVersionParams,
    replaceDefaultPolicyVersionParams,
    rdpvpTemplateName,

    -- ** RepublishAction
    RepublishAction,
    republishAction,
    raQos,
    raRoleARN,
    raTopic,

    -- ** ResourceIdentifier
    ResourceIdentifier,
    resourceIdentifier,
    riIamRoleARN,
    riClientId,
    riRoleAliasARN,
    riCaCertificateId,
    riDeviceCertificateId,
    riAccount,
    riPolicyVersionIdentifier,
    riCognitoIdentityPoolId,

    -- ** RoleAliasDescription
    RoleAliasDescription,
    roleAliasDescription,
    radRoleAliasARN,
    radLastModifiedDate,
    radRoleAlias,
    radOwner,
    radCreationDate,
    radCredentialDurationSeconds,
    radRoleARN,

    -- ** S3Action
    S3Action,
    s3Action,
    sCannedACL,
    sRoleARN,
    sBucketName,
    sKey,

    -- ** S3Destination
    S3Destination,
    s3Destination,
    sdPrefix,
    sdBucket,

    -- ** S3Location
    S3Location,
    s3Location,
    slBucket,
    slKey,
    slVersion,

    -- ** SNSAction
    SNSAction,
    snsAction,
    snsaMessageFormat,
    snsaTargetARN,
    snsaRoleARN,

    -- ** SalesforceAction
    SalesforceAction,
    salesforceAction,
    saToken,
    saUrl,

    -- ** ScheduledAuditMetadata
    ScheduledAuditMetadata,
    scheduledAuditMetadata,
    samFrequency,
    samScheduledAuditName,
    samDayOfMonth,
    samDayOfWeek,
    samScheduledAuditARN,

    -- ** SecurityProfileIdentifier
    SecurityProfileIdentifier,
    securityProfileIdentifier,
    spiName,
    spiArn,

    -- ** SecurityProfileTarget
    SecurityProfileTarget,
    securityProfileTarget,
    sptArn,

    -- ** SecurityProfileTargetMapping
    SecurityProfileTargetMapping,
    securityProfileTargetMapping,
    sptmSecurityProfileIdentifier,
    sptmTarget,

    -- ** ServerCertificateSummary
    ServerCertificateSummary,
    serverCertificateSummary,
    scsServerCertificateStatusDetail,
    scsServerCertificateStatus,
    scsServerCertificateARN,

    -- ** SigV4Authorization
    SigV4Authorization,
    sigV4Authorization,
    svaSigningRegion,
    svaServiceName,
    svaRoleARN,

    -- ** SigningProfileParameter
    SigningProfileParameter,
    signingProfileParameter,
    sppPlatform,
    sppCertificateARN,
    sppCertificatePathOnDevice,

    -- ** SqsAction
    SqsAction,
    sqsAction,
    saUseBase64,
    saRoleARN,
    saQueueURL,

    -- ** StartSigningJobParameter
    StartSigningJobParameter,
    startSigningJobParameter,
    ssjpDestination,
    ssjpSigningProfileName,
    ssjpSigningProfileParameter,

    -- ** StatisticalThreshold
    StatisticalThreshold,
    statisticalThreshold,
    stStatistic,

    -- ** Statistics
    Statistics,
    statistics,
    sStdDeviation,
    sMaximum,
    sAverage,
    sCount,
    sMinimum,
    sVariance,
    sSumOfSquares,
    sSum,

    -- ** StepFunctionsAction
    StepFunctionsAction,
    stepFunctionsAction,
    sfaExecutionNamePrefix,
    sfaStateMachineName,
    sfaRoleARN,

    -- ** Stream
    Stream,
    stream,
    sFileId,
    sStreamId,

    -- ** StreamFile
    StreamFile,
    streamFile,
    sfS3Location,
    sfFileId,

    -- ** StreamInfo
    StreamInfo,
    streamInfo,
    siLastUpdatedAt,
    siCreatedAt,
    siStreamVersion,
    siStreamARN,
    siFiles,
    siDescription,
    siStreamId,
    siRoleARN,

    -- ** StreamSummary
    StreamSummary,
    streamSummary,
    ssStreamVersion,
    ssStreamARN,
    ssDescription,
    ssStreamId,

    -- ** TLSContext
    TLSContext,
    tlsContext,
    tcServerName,

    -- ** Tag
    Tag,
    tag,
    tagValue,
    tagKey,

    -- ** TaskStatistics
    TaskStatistics,
    taskStatistics,
    tsNonCompliantChecks,
    tsWaitingForDataCollectionChecks,
    tsFailedChecks,
    tsTotalChecks,
    tsInProgressChecks,
    tsCompliantChecks,
    tsCanceledChecks,

    -- ** TaskStatisticsForAuditCheck
    TaskStatisticsForAuditCheck,
    taskStatisticsForAuditCheck,
    tsfacCanceledFindingsCount,
    tsfacSkippedFindingsCount,
    tsfacTotalFindingsCount,
    tsfacFailedFindingsCount,
    tsfacSucceededFindingsCount,

    -- ** ThingAttribute
    ThingAttribute,
    thingAttribute,
    taThingTypeName,
    taThingARN,
    taAttributes,
    taVersion,
    taThingName,

    -- ** ThingConnectivity
    ThingConnectivity,
    thingConnectivity,
    tcConnected,
    tcTimestamp,

    -- ** ThingDocument
    ThingDocument,
    thingDocument,
    tdThingGroupNames,
    tdThingTypeName,
    tdShadow,
    tdAttributes,
    tdConnectivity,
    tdThingName,
    tdThingId,

    -- ** ThingGroupDocument
    ThingGroupDocument,
    thingGroupDocument,
    tgdParentGroupNames,
    tgdThingGroupId,
    tgdThingGroupName,
    tgdAttributes,
    tgdThingGroupDescription,

    -- ** ThingGroupIndexingConfiguration
    ThingGroupIndexingConfiguration,
    thingGroupIndexingConfiguration,
    tgicManagedFields,
    tgicCustomFields,
    tgicThingGroupIndexingMode,

    -- ** ThingGroupMetadata
    ThingGroupMetadata,
    thingGroupMetadata,
    tgmRootToParentThingGroups,
    tgmParentGroupName,
    tgmCreationDate,

    -- ** ThingGroupProperties
    ThingGroupProperties,
    thingGroupProperties,
    tgpAttributePayload,
    tgpThingGroupDescription,

    -- ** ThingIndexingConfiguration
    ThingIndexingConfiguration,
    thingIndexingConfiguration,
    ticManagedFields,
    ticThingConnectivityIndexingMode,
    ticCustomFields,
    ticThingIndexingMode,

    -- ** ThingTypeDefinition
    ThingTypeDefinition,
    thingTypeDefinition,
    ttdThingTypeProperties,
    ttdThingTypeName,
    ttdThingTypeMetadata,
    ttdThingTypeARN,

    -- ** ThingTypeMetadata
    ThingTypeMetadata,
    thingTypeMetadata,
    ttmDeprecationDate,
    ttmCreationDate,
    ttmDeprecated,

    -- ** ThingTypeProperties
    ThingTypeProperties,
    thingTypeProperties,
    ttpSearchableAttributes,
    ttpThingTypeDescription,

    -- ** TimeoutConfig
    TimeoutConfig,
    timeoutConfig,
    tcInProgressTimeoutInMinutes,

    -- ** TimestreamAction
    TimestreamAction,
    timestreamAction,
    taTimestamp,
    taRoleARN,
    taDatabaseName,
    taTableName,
    taDimensions,

    -- ** TimestreamDimension
    TimestreamDimension,
    timestreamDimension,
    tdName,
    tdValue,

    -- ** TimestreamTimestamp
    TimestreamTimestamp,
    timestreamTimestamp,
    ttValue,
    ttUnit,

    -- ** TopicRule
    TopicRule,
    topicRule,
    trCreatedAt,
    trActions,
    trAwsIotSqlVersion,
    trErrorAction,
    trRuleDisabled,
    trRuleName,
    trSql,
    trDescription,

    -- ** TopicRuleDestination
    TopicRuleDestination,
    topicRuleDestination,
    trdStatus,
    trdHttpURLProperties,
    trdArn,
    trdStatusReason,

    -- ** TopicRuleDestinationConfiguration
    TopicRuleDestinationConfiguration,
    topicRuleDestinationConfiguration,
    trdcHttpURLConfiguration,

    -- ** TopicRuleDestinationSummary
    TopicRuleDestinationSummary,
    topicRuleDestinationSummary,
    trdsStatus,
    trdsHttpURLSummary,
    trdsArn,
    trdsStatusReason,

    -- ** TopicRuleListItem
    TopicRuleListItem,
    topicRuleListItem,
    trliCreatedAt,
    trliRuleDisabled,
    trliRuleName,
    trliRuleARN,
    trliTopicPattern,

    -- ** TopicRulePayload
    TopicRulePayload,
    topicRulePayload,
    trpAwsIotSqlVersion,
    trpErrorAction,
    trpRuleDisabled,
    trpDescription,
    trpSql,
    trpActions,

    -- ** TransferData
    TransferData,
    transferData,
    tdTransferDate,
    tdAcceptDate,
    tdTransferMessage,
    tdRejectDate,
    tdRejectReason,

    -- ** UpdateCACertificateParams
    UpdateCACertificateParams,
    updateCACertificateParams,
    ucacpAction,

    -- ** UpdateDeviceCertificateParams
    UpdateDeviceCertificateParams,
    updateDeviceCertificateParams,
    udcpAction,

    -- ** ValidationError
    ValidationError,
    validationError,
    veErrorMessage,

    -- ** ViolationEvent
    ViolationEvent,
    violationEvent,
    veViolationEventType,
    veViolationId,
    veBehavior,
    veMetricValue,
    veSecurityProfileName,
    veViolationEventTime,
    veThingName,
  )
where

import Network.AWS.IoT.AcceptCertificateTransfer
import Network.AWS.IoT.AddThingToBillingGroup
import Network.AWS.IoT.AddThingToThingGroup
import Network.AWS.IoT.AssociateTargetsWithJob
import Network.AWS.IoT.AttachPolicy
import Network.AWS.IoT.AttachSecurityProfile
import Network.AWS.IoT.AttachThingPrincipal
import Network.AWS.IoT.CancelAuditMitigationActionsTask
import Network.AWS.IoT.CancelAuditTask
import Network.AWS.IoT.CancelCertificateTransfer
import Network.AWS.IoT.CancelJob
import Network.AWS.IoT.CancelJobExecution
import Network.AWS.IoT.ClearDefaultAuthorizer
import Network.AWS.IoT.ConfirmTopicRuleDestination
import Network.AWS.IoT.CreateAuditSuppression
import Network.AWS.IoT.CreateAuthorizer
import Network.AWS.IoT.CreateBillingGroup
import Network.AWS.IoT.CreateCertificateFromCSR
import Network.AWS.IoT.CreateDimension
import Network.AWS.IoT.CreateDomainConfiguration
import Network.AWS.IoT.CreateDynamicThingGroup
import Network.AWS.IoT.CreateJob
import Network.AWS.IoT.CreateKeysAndCertificate
import Network.AWS.IoT.CreateMitigationAction
import Network.AWS.IoT.CreateOTAUpdate
import Network.AWS.IoT.CreatePolicy
import Network.AWS.IoT.CreatePolicyVersion
import Network.AWS.IoT.CreateProvisioningClaim
import Network.AWS.IoT.CreateProvisioningTemplate
import Network.AWS.IoT.CreateProvisioningTemplateVersion
import Network.AWS.IoT.CreateRoleAlias
import Network.AWS.IoT.CreateScheduledAudit
import Network.AWS.IoT.CreateSecurityProfile
import Network.AWS.IoT.CreateStream
import Network.AWS.IoT.CreateThing
import Network.AWS.IoT.CreateThingGroup
import Network.AWS.IoT.CreateThingType
import Network.AWS.IoT.CreateTopicRule
import Network.AWS.IoT.CreateTopicRuleDestination
import Network.AWS.IoT.DeleteAccountAuditConfiguration
import Network.AWS.IoT.DeleteAuditSuppression
import Network.AWS.IoT.DeleteAuthorizer
import Network.AWS.IoT.DeleteBillingGroup
import Network.AWS.IoT.DeleteCACertificate
import Network.AWS.IoT.DeleteCertificate
import Network.AWS.IoT.DeleteDimension
import Network.AWS.IoT.DeleteDomainConfiguration
import Network.AWS.IoT.DeleteDynamicThingGroup
import Network.AWS.IoT.DeleteJob
import Network.AWS.IoT.DeleteJobExecution
import Network.AWS.IoT.DeleteMitigationAction
import Network.AWS.IoT.DeleteOTAUpdate
import Network.AWS.IoT.DeletePolicy
import Network.AWS.IoT.DeletePolicyVersion
import Network.AWS.IoT.DeleteProvisioningTemplate
import Network.AWS.IoT.DeleteProvisioningTemplateVersion
import Network.AWS.IoT.DeleteRegistrationCode
import Network.AWS.IoT.DeleteRoleAlias
import Network.AWS.IoT.DeleteScheduledAudit
import Network.AWS.IoT.DeleteSecurityProfile
import Network.AWS.IoT.DeleteStream
import Network.AWS.IoT.DeleteThing
import Network.AWS.IoT.DeleteThingGroup
import Network.AWS.IoT.DeleteThingType
import Network.AWS.IoT.DeleteTopicRule
import Network.AWS.IoT.DeleteTopicRuleDestination
import Network.AWS.IoT.DeleteV2LoggingLevel
import Network.AWS.IoT.DeprecateThingType
import Network.AWS.IoT.DescribeAccountAuditConfiguration
import Network.AWS.IoT.DescribeAuditFinding
import Network.AWS.IoT.DescribeAuditMitigationActionsTask
import Network.AWS.IoT.DescribeAuditSuppression
import Network.AWS.IoT.DescribeAuditTask
import Network.AWS.IoT.DescribeAuthorizer
import Network.AWS.IoT.DescribeBillingGroup
import Network.AWS.IoT.DescribeCACertificate
import Network.AWS.IoT.DescribeCertificate
import Network.AWS.IoT.DescribeDefaultAuthorizer
import Network.AWS.IoT.DescribeDimension
import Network.AWS.IoT.DescribeDomainConfiguration
import Network.AWS.IoT.DescribeEndpoint
import Network.AWS.IoT.DescribeEventConfigurations
import Network.AWS.IoT.DescribeIndex
import Network.AWS.IoT.DescribeJob
import Network.AWS.IoT.DescribeJobExecution
import Network.AWS.IoT.DescribeMitigationAction
import Network.AWS.IoT.DescribeProvisioningTemplate
import Network.AWS.IoT.DescribeProvisioningTemplateVersion
import Network.AWS.IoT.DescribeRoleAlias
import Network.AWS.IoT.DescribeScheduledAudit
import Network.AWS.IoT.DescribeSecurityProfile
import Network.AWS.IoT.DescribeStream
import Network.AWS.IoT.DescribeThing
import Network.AWS.IoT.DescribeThingGroup
import Network.AWS.IoT.DescribeThingRegistrationTask
import Network.AWS.IoT.DescribeThingType
import Network.AWS.IoT.DetachPolicy
import Network.AWS.IoT.DetachSecurityProfile
import Network.AWS.IoT.DetachThingPrincipal
import Network.AWS.IoT.DisableTopicRule
import Network.AWS.IoT.EnableTopicRule
import Network.AWS.IoT.GetCardinality
import Network.AWS.IoT.GetEffectivePolicies
import Network.AWS.IoT.GetIndexingConfiguration
import Network.AWS.IoT.GetJobDocument
import Network.AWS.IoT.GetLoggingOptions
import Network.AWS.IoT.GetOTAUpdate
import Network.AWS.IoT.GetPercentiles
import Network.AWS.IoT.GetPolicy
import Network.AWS.IoT.GetPolicyVersion
import Network.AWS.IoT.GetRegistrationCode
import Network.AWS.IoT.GetStatistics
import Network.AWS.IoT.GetTopicRule
import Network.AWS.IoT.GetTopicRuleDestination
import Network.AWS.IoT.GetV2LoggingOptions
import Network.AWS.IoT.ListActiveViolations
import Network.AWS.IoT.ListAttachedPolicies
import Network.AWS.IoT.ListAuditFindings
import Network.AWS.IoT.ListAuditMitigationActionsExecutions
import Network.AWS.IoT.ListAuditMitigationActionsTasks
import Network.AWS.IoT.ListAuditSuppressions
import Network.AWS.IoT.ListAuditTasks
import Network.AWS.IoT.ListAuthorizers
import Network.AWS.IoT.ListBillingGroups
import Network.AWS.IoT.ListCACertificates
import Network.AWS.IoT.ListCertificates
import Network.AWS.IoT.ListCertificatesByCA
import Network.AWS.IoT.ListDimensions
import Network.AWS.IoT.ListDomainConfigurations
import Network.AWS.IoT.ListIndices
import Network.AWS.IoT.ListJobExecutionsForJob
import Network.AWS.IoT.ListJobExecutionsForThing
import Network.AWS.IoT.ListJobs
import Network.AWS.IoT.ListMitigationActions
import Network.AWS.IoT.ListOTAUpdates
import Network.AWS.IoT.ListOutgoingCertificates
import Network.AWS.IoT.ListPolicies
import Network.AWS.IoT.ListPolicyVersions
import Network.AWS.IoT.ListPrincipalThings
import Network.AWS.IoT.ListProvisioningTemplateVersions
import Network.AWS.IoT.ListProvisioningTemplates
import Network.AWS.IoT.ListRoleAliases
import Network.AWS.IoT.ListScheduledAudits
import Network.AWS.IoT.ListSecurityProfiles
import Network.AWS.IoT.ListSecurityProfilesForTarget
import Network.AWS.IoT.ListStreams
import Network.AWS.IoT.ListTagsForResource
import Network.AWS.IoT.ListTargetsForPolicy
import Network.AWS.IoT.ListTargetsForSecurityProfile
import Network.AWS.IoT.ListThingGroups
import Network.AWS.IoT.ListThingGroupsForThing
import Network.AWS.IoT.ListThingPrincipals
import Network.AWS.IoT.ListThingRegistrationTaskReports
import Network.AWS.IoT.ListThingRegistrationTasks
import Network.AWS.IoT.ListThingTypes
import Network.AWS.IoT.ListThings
import Network.AWS.IoT.ListThingsInBillingGroup
import Network.AWS.IoT.ListThingsInThingGroup
import Network.AWS.IoT.ListTopicRuleDestinations
import Network.AWS.IoT.ListTopicRules
import Network.AWS.IoT.ListV2LoggingLevels
import Network.AWS.IoT.ListViolationEvents
import Network.AWS.IoT.RegisterCACertificate
import Network.AWS.IoT.RegisterCertificate
import Network.AWS.IoT.RegisterCertificateWithoutCA
import Network.AWS.IoT.RegisterThing
import Network.AWS.IoT.RejectCertificateTransfer
import Network.AWS.IoT.RemoveThingFromBillingGroup
import Network.AWS.IoT.RemoveThingFromThingGroup
import Network.AWS.IoT.ReplaceTopicRule
import Network.AWS.IoT.SearchIndex
import Network.AWS.IoT.SetDefaultAuthorizer
import Network.AWS.IoT.SetDefaultPolicyVersion
import Network.AWS.IoT.SetLoggingOptions
import Network.AWS.IoT.SetV2LoggingLevel
import Network.AWS.IoT.SetV2LoggingOptions
import Network.AWS.IoT.StartAuditMitigationActionsTask
import Network.AWS.IoT.StartOnDemandAuditTask
import Network.AWS.IoT.StartThingRegistrationTask
import Network.AWS.IoT.StopThingRegistrationTask
import Network.AWS.IoT.TagResource
import Network.AWS.IoT.TestAuthorization
import Network.AWS.IoT.TestInvokeAuthorizer
import Network.AWS.IoT.TransferCertificate
import Network.AWS.IoT.Types
import Network.AWS.IoT.UntagResource
import Network.AWS.IoT.UpdateAccountAuditConfiguration
import Network.AWS.IoT.UpdateAuditSuppression
import Network.AWS.IoT.UpdateAuthorizer
import Network.AWS.IoT.UpdateBillingGroup
import Network.AWS.IoT.UpdateCACertificate
import Network.AWS.IoT.UpdateCertificate
import Network.AWS.IoT.UpdateDimension
import Network.AWS.IoT.UpdateDomainConfiguration
import Network.AWS.IoT.UpdateDynamicThingGroup
import Network.AWS.IoT.UpdateEventConfigurations
import Network.AWS.IoT.UpdateIndexingConfiguration
import Network.AWS.IoT.UpdateJob
import Network.AWS.IoT.UpdateMitigationAction
import Network.AWS.IoT.UpdateProvisioningTemplate
import Network.AWS.IoT.UpdateRoleAlias
import Network.AWS.IoT.UpdateScheduledAudit
import Network.AWS.IoT.UpdateSecurityProfile
import Network.AWS.IoT.UpdateStream
import Network.AWS.IoT.UpdateThing
import Network.AWS.IoT.UpdateThingGroup
import Network.AWS.IoT.UpdateThingGroupsForThing
import Network.AWS.IoT.UpdateTopicRuleDestination
import Network.AWS.IoT.ValidateSecurityProfileBehaviors
import Network.AWS.IoT.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'IoT'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.

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
-- The service endpoints that expose this API are listed in <https://docs.aws.amazon.com/general/latest/gr/iot-core.html AWS IoT Core Endpoints and Quotas> . You must use the endpoint for the region that has the resources you want to access.
-- The service name used by <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html AWS Signature Version 4> to sign the request is: /execute-api/ .
-- For more information about how AWS IoT works, see the <https://docs.aws.amazon.com/iot/latest/developerguide/aws-iot-how-it-works.html Developer Guide> .
-- For information about how to use the credentials provider for AWS IoT, see <https://docs.aws.amazon.com/iot/latest/developerguide/authorizing-direct-aws.html Authorizing Direct Calls to AWS Services> .
module Network.AWS.IoT
  ( -- * Service configuration
    ioTService,

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
    AWSJobAbortConfig (..),
    mkAWSJobAbortConfig,
    ajacAbortCriteriaList,

    -- ** AWSJobAbortCriteria
    AWSJobAbortCriteria (..),
    mkAWSJobAbortCriteria,
    ajacThresholdPercentage,
    ajacFailureType,
    ajacAction,
    ajacMinNumberOfExecutedThings,

    -- ** AWSJobExecutionsRolloutConfig
    AWSJobExecutionsRolloutConfig (..),
    mkAWSJobExecutionsRolloutConfig,
    ajercExponentialRate,
    ajercMaximumPerMinute,

    -- ** AWSJobExponentialRolloutRate
    AWSJobExponentialRolloutRate (..),
    mkAWSJobExponentialRolloutRate,
    ajerrBaseRatePerMinute,
    ajerrIncrementFactor,
    ajerrRateIncreaseCriteria,

    -- ** AWSJobPresignedURLConfig
    AWSJobPresignedURLConfig (..),
    mkAWSJobPresignedURLConfig,
    ajpucExpiresInSec,

    -- ** AWSJobRateIncreaseCriteria
    AWSJobRateIncreaseCriteria (..),
    mkAWSJobRateIncreaseCriteria,
    ajricNumberOfNotifiedThings,
    ajricNumberOfSucceededThings,

    -- ** AWSJobTimeoutConfig
    AWSJobTimeoutConfig (..),
    mkAWSJobTimeoutConfig,
    ajtcInProgressTimeoutInMinutes,

    -- ** AbortConfig
    AbortConfig (..),
    mkAbortConfig,
    acCriteriaList,

    -- ** AbortCriteria
    AbortCriteria (..),
    mkAbortCriteria,
    acThresholdPercentage,
    acFailureType,
    acAction,
    acMinNumberOfExecutedThings,

    -- ** Action
    Action (..),
    mkAction,
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
    ActiveViolation (..),
    mkActiveViolation,
    avLastViolationValue,
    avLastViolationTime,
    avViolationStartTime,
    avViolationId,
    avBehavior,
    avSecurityProfileName,
    avThingName,

    -- ** AddThingsToThingGroupParams
    AddThingsToThingGroupParams (..),
    mkAddThingsToThingGroupParams,
    atttgpThingGroupNames,
    atttgpOverrideDynamicGroups,

    -- ** AlertTarget
    AlertTarget (..),
    mkAlertTarget,
    atAlertTargetARN,
    atRoleARN,

    -- ** Allowed
    Allowed (..),
    mkAllowed,
    aPolicies,

    -- ** AssetPropertyTimestamp
    AssetPropertyTimestamp (..),
    mkAssetPropertyTimestamp,
    aptTimeInSeconds,
    aptOffsetInNanos,

    -- ** AssetPropertyValue
    AssetPropertyValue (..),
    mkAssetPropertyValue,
    apvValue,
    apvQuality,
    apvTimestamp,

    -- ** AssetPropertyVariant
    AssetPropertyVariant (..),
    mkAssetPropertyVariant,
    apvIntegerValue,
    apvDoubleValue,
    apvStringValue,
    apvBooleanValue,

    -- ** AttributePayload
    AttributePayload (..),
    mkAttributePayload,
    apAttributes,
    apMerge,

    -- ** AuditCheckConfiguration
    AuditCheckConfiguration (..),
    mkAuditCheckConfiguration,
    accEnabled,

    -- ** AuditCheckDetails
    AuditCheckDetails (..),
    mkAuditCheckDetails,
    acdSuppressedNonCompliantResourcesCount,
    acdTotalResourcesCount,
    acdCheckCompliant,
    acdNonCompliantResourcesCount,
    acdErrorCode,
    acdMessage,
    acdCheckRunStatus,

    -- ** AuditFinding
    AuditFinding (..),
    mkAuditFinding,
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
    AuditMitigationActionExecutionMetadata (..),
    mkAuditMitigationActionExecutionMetadata,
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
    AuditMitigationActionsTaskMetadata (..),
    mkAuditMitigationActionsTaskMetadata,
    amatmStartTime,
    amatmTaskId,
    amatmTaskStatus,

    -- ** AuditMitigationActionsTaskTarget
    AuditMitigationActionsTaskTarget (..),
    mkAuditMitigationActionsTaskTarget,
    amattAuditTaskId,
    amattFindingIds,
    amattAuditCheckToReasonCodeFilter,

    -- ** AuditNotificationTarget
    AuditNotificationTarget (..),
    mkAuditNotificationTarget,
    antTargetARN,
    antEnabled,
    antRoleARN,

    -- ** AuditSuppression
    AuditSuppression (..),
    mkAuditSuppression,
    asCheckName,
    asExpirationDate,
    asSuppressIndefinitely,
    asDescription,
    asResourceIdentifier,

    -- ** AuditTaskMetadata
    AuditTaskMetadata (..),
    mkAuditTaskMetadata,
    atmTaskType,
    atmTaskId,
    atmTaskStatus,

    -- ** AuthInfo
    AuthInfo (..),
    mkAuthInfo,
    aiResources,
    aiActionType,

    -- ** AuthResult
    AuthResult (..),
    mkAuthResult,
    arDenied,
    arAuthDecision,
    arAllowed,
    arMissingContextValues,
    arAuthInfo,

    -- ** AuthorizerConfig
    AuthorizerConfig (..),
    mkAuthorizerConfig,
    acAllowAuthorizerOverride,
    acDefaultAuthorizerName,

    -- ** AuthorizerDescription
    AuthorizerDescription (..),
    mkAuthorizerDescription,
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
    AuthorizerSummary (..),
    mkAuthorizerSummary,
    asAuthorizerName,
    asAuthorizerARN,

    -- ** Behavior
    Behavior (..),
    mkBehavior,
    bMetricDimension,
    bMetric,
    bName,
    bCriteria,

    -- ** BehaviorCriteria
    BehaviorCriteria (..),
    mkBehaviorCriteria,
    bcValue,
    bcConsecutiveDatapointsToAlarm,
    bcComparisonOperator,
    bcStatisticalThreshold,
    bcDurationSeconds,
    bcConsecutiveDatapointsToClear,

    -- ** BillingGroupMetadata
    BillingGroupMetadata (..),
    mkBillingGroupMetadata,
    bgmCreationDate,

    -- ** BillingGroupProperties
    BillingGroupProperties (..),
    mkBillingGroupProperties,
    bgpBillingGroupDescription,

    -- ** CACertificate
    CACertificate (..),
    mkCACertificate,
    cacStatus,
    cacCertificateARN,
    cacCertificateId,
    cacCreationDate,

    -- ** CACertificateDescription
    CACertificateDescription (..),
    mkCACertificateDescription,
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
    Certificate (..),
    mkCertificate,
    cStatus,
    cCertificateARN,
    cCertificateId,
    cCertificateMode,
    cCreationDate,

    -- ** CertificateDescription
    CertificateDescription (..),
    mkCertificateDescription,
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
    CertificateValidity (..),
    mkCertificateValidity,
    cvNotBefore,
    cvNotAfter,

    -- ** CloudwatchAlarmAction
    CloudwatchAlarmAction (..),
    mkCloudwatchAlarmAction,
    caaAlarmName,
    caaStateValue,
    caaStateReason,
    caaRoleARN,

    -- ** CloudwatchLogsAction
    CloudwatchLogsAction (..),
    mkCloudwatchLogsAction,
    claLogGroupName,
    claRoleARN,

    -- ** CloudwatchMetricAction
    CloudwatchMetricAction (..),
    mkCloudwatchMetricAction,
    cmaMetricName,
    cmaMetricNamespace,
    cmaMetricValue,
    cmaMetricTimestamp,
    cmaMetricUnit,
    cmaRoleARN,

    -- ** CodeSigning
    CodeSigning (..),
    mkCodeSigning,
    csCustomCodeSigning,
    csStartSigningJobParameter,
    csAwsSignerJobId,

    -- ** CodeSigningCertificateChain
    CodeSigningCertificateChain (..),
    mkCodeSigningCertificateChain,
    csccCertificateName,
    csccInlineDocument,

    -- ** CodeSigningSignature
    CodeSigningSignature (..),
    mkCodeSigningSignature,
    cssInlineDocument,

    -- ** Configuration
    Configuration (..),
    mkConfiguration,
    cEnabled,

    -- ** CustomCodeSigning
    CustomCodeSigning (..),
    mkCustomCodeSigning,
    ccsSignature,
    ccsHashAlgorithm,
    ccsCertificateChain,
    ccsSignatureAlgorithm,

    -- ** Denied
    Denied (..),
    mkDenied,
    dImplicitDeny,
    dExplicitDeny,

    -- ** Destination
    Destination (..),
    mkDestination,
    dS3Destination,

    -- ** DomainConfigurationSummary
    DomainConfigurationSummary (..),
    mkDomainConfigurationSummary,
    dcsDomainConfigurationName,
    dcsDomainConfigurationARN,
    dcsServiceType,

    -- ** DynamoDBAction
    DynamoDBAction (..),
    mkDynamoDBAction,
    ddbaHashKeyField,
    ddbaHashKeyType,
    ddbaOperation,
    ddbaRangeKeyType,
    ddbaPayloadField,
    ddbaRangeKeyField,
    ddbaRangeKeyValue,
    ddbaHashKeyValue,
    ddbaTableName,
    ddbaRoleARN,

    -- ** DynamoDBv2Action
    DynamoDBv2Action (..),
    mkDynamoDBv2Action,
    ddaPutItem,
    ddaRoleARN,

    -- ** EffectivePolicy
    EffectivePolicy (..),
    mkEffectivePolicy,
    epPolicyName,
    epPolicyDocument,
    epPolicyARN,

    -- ** ElasticsearchAction
    ElasticsearchAction (..),
    mkElasticsearchAction,
    eaId,
    eaType,
    eaEndpoint,
    eaIndex,
    eaRoleARN,

    -- ** EnableIOTLoggingParams
    EnableIOTLoggingParams (..),
    mkEnableIOTLoggingParams,
    eiotlpLogLevel,
    eiotlpRoleARNForLogging,

    -- ** ErrorInfo
    ErrorInfo (..),
    mkErrorInfo,
    eiCode,
    eiMessage,

    -- ** ExplicitDeny
    ExplicitDeny (..),
    mkExplicitDeny,
    edPolicies,

    -- ** ExponentialRolloutRate
    ExponentialRolloutRate (..),
    mkExponentialRolloutRate,
    errBaseRatePerMinute,
    errIncrementFactor,
    errRateIncreaseCriteria,

    -- ** Field
    Field (..),
    mkField,
    fName,
    fType,

    -- ** FileLocation
    FileLocation (..),
    mkFileLocation,
    flStream,
    flS3Location,

    -- ** FirehoseAction
    FirehoseAction (..),
    mkFirehoseAction,
    faBatchMode,
    faSeparator,
    faDeliveryStreamName,
    faRoleARN,

    -- ** GroupNameAndARN
    GroupNameAndARN (..),
    mkGroupNameAndARN,
    gnaaGroupARN,
    gnaaGroupName,

    -- ** HTTPAction
    HTTPAction (..),
    mkHTTPAction,
    httpaConfirmationURL,
    httpaAuth,
    httpaUrl,
    httpaHeaders,

    -- ** HTTPActionHeader
    HTTPActionHeader (..),
    mkHTTPActionHeader,
    httpahValue,
    httpahKey,

    -- ** HTTPAuthorization
    HTTPAuthorization (..),
    mkHTTPAuthorization,
    httpaSigv4,

    -- ** HTTPContext
    HTTPContext (..),
    mkHTTPContext,
    httpcHeaders,
    httpcQueryString,

    -- ** HTTPURLDestinationConfiguration
    HTTPURLDestinationConfiguration (..),
    mkHTTPURLDestinationConfiguration,
    httpudcConfirmationURL,

    -- ** HTTPURLDestinationProperties
    HTTPURLDestinationProperties (..),
    mkHTTPURLDestinationProperties,
    httpudpConfirmationURL,

    -- ** HTTPURLDestinationSummary
    HTTPURLDestinationSummary (..),
    mkHTTPURLDestinationSummary,
    httpudsConfirmationURL,

    -- ** ImplicitDeny
    ImplicitDeny (..),
    mkImplicitDeny,
    idPolicies,

    -- ** IotAnalyticsAction
    IotAnalyticsAction (..),
    mkIotAnalyticsAction,
    iaaBatchMode,
    iaaChannelARN,
    iaaChannelName,
    iaaRoleARN,

    -- ** IotEventsAction
    IotEventsAction (..),
    mkIotEventsAction,
    ieaBatchMode,
    ieaInputName,
    ieaMessageId,
    ieaRoleARN,

    -- ** IotSiteWiseAction
    IotSiteWiseAction (..),
    mkIotSiteWiseAction,
    iswaPutAssetPropertyValueEntries,
    iswaRoleARN,

    -- ** Job
    Job (..),
    mkJob,
    jStatus,
    jJobExecutionsRolloutConfig,
    jJobId,
    jLastUpdatedAt,
    jJobARN,
    jCreatedAt,
    jAbortConfig,
    jJobProcessDetails,
    jNamespaceId,
    jReasonCode,
    jPresignedURLConfig,
    jForceCanceled,
    jTargets,
    jCompletedAt,
    jComment,
    jDescription,
    jTargetSelection,
    jTimeoutConfig,

    -- ** JobExecution
    JobExecution (..),
    mkJobExecution,
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
    JobExecutionStatusDetails (..),
    mkJobExecutionStatusDetails,
    jesdDetailsMap,

    -- ** JobExecutionSummary
    JobExecutionSummary (..),
    mkJobExecutionSummary,
    jesStatus,
    jesLastUpdatedAt,
    jesQueuedAt,
    jesExecutionNumber,
    jesStartedAt,

    -- ** JobExecutionSummaryForJob
    JobExecutionSummaryForJob (..),
    mkJobExecutionSummaryForJob,
    jesfjJobExecutionSummary,
    jesfjThingARN,

    -- ** JobExecutionSummaryForThing
    JobExecutionSummaryForThing (..),
    mkJobExecutionSummaryForThing,
    jesftJobId,
    jesftJobExecutionSummary,

    -- ** JobExecutionsRolloutConfig
    JobExecutionsRolloutConfig (..),
    mkJobExecutionsRolloutConfig,
    jercExponentialRate,
    jercMaximumPerMinute,

    -- ** JobProcessDetails
    JobProcessDetails (..),
    mkJobProcessDetails,
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
    JobSummary (..),
    mkJobSummary,
    jsStatus,
    jsJobId,
    jsLastUpdatedAt,
    jsJobARN,
    jsCreatedAt,
    jsThingGroupId,
    jsCompletedAt,
    jsTargetSelection,

    -- ** KeyPair
    KeyPair (..),
    mkKeyPair,
    kpPrivateKey,
    kpPublicKey,

    -- ** KinesisAction
    KinesisAction (..),
    mkKinesisAction,
    kaPartitionKey,
    kaStreamName,
    kaRoleARN,

    -- ** LambdaAction
    LambdaAction (..),
    mkLambdaAction,
    laFunctionARN,

    -- ** LogTarget
    LogTarget (..),
    mkLogTarget,
    ltTargetType,
    ltTargetName,

    -- ** LogTargetConfiguration
    LogTargetConfiguration (..),
    mkLogTargetConfiguration,
    ltcLogLevel,
    ltcLogTarget,

    -- ** LoggingOptionsPayload
    LoggingOptionsPayload (..),
    mkLoggingOptionsPayload,
    lopLogLevel,
    lopRoleARN,

    -- ** MetricDimension
    MetricDimension (..),
    mkMetricDimension,
    mdOperator,
    mdDimensionName,

    -- ** MetricToRetain
    MetricToRetain (..),
    mkMetricToRetain,
    mtrMetricDimension,
    mtrMetric,

    -- ** MetricValue
    MetricValue (..),
    mkMetricValue,
    mvCidrs,
    mvCount,
    mvPorts,

    -- ** MitigationAction
    MitigationAction (..),
    mkMitigationAction,
    maActionParams,
    maName,
    maId,
    maRoleARN,

    -- ** MitigationActionIdentifier
    MitigationActionIdentifier (..),
    mkMitigationActionIdentifier,
    maiActionName,
    maiCreationDate,
    maiActionARN,

    -- ** MitigationActionParams
    MitigationActionParams (..),
    mkMitigationActionParams,
    mapEnableIOTLoggingParams,
    mapAddThingsToThingGroupParams,
    mapUpdateCACertificateParams,
    mapUpdateDeviceCertificateParams,
    mapReplaceDefaultPolicyVersionParams,
    mapPublishFindingToSNSParams,

    -- ** MqttContext
    MqttContext (..),
    mkMqttContext,
    mcClientId,
    mcUsername,
    mcPassword,

    -- ** NonCompliantResource
    NonCompliantResource (..),
    mkNonCompliantResource,
    ncrAdditionalInfo,
    ncrResourceType,
    ncrResourceIdentifier,

    -- ** OTAUpdateFile
    OTAUpdateFile (..),
    mkOTAUpdateFile,
    otaufFileLocation,
    otaufFileType,
    otaufFileVersion,
    otaufAttributes,
    otaufCodeSigning,
    otaufFileName,

    -- ** OTAUpdateInfo
    OTAUpdateInfo (..),
    mkOTAUpdateInfo,
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
    OTAUpdateSummary (..),
    mkOTAUpdateSummary,
    otausCreationDate,
    otausOtaUpdateId,
    otausOtaUpdateARN,

    -- ** OutgoingCertificate
    OutgoingCertificate (..),
    mkOutgoingCertificate,
    ocTransferDate,
    ocCertificateARN,
    ocCertificateId,
    ocTransferredTo,
    ocCreationDate,
    ocTransferMessage,

    -- ** PercentPair
    PercentPair (..),
    mkPercentPair,
    ppValue,
    ppPercent,

    -- ** Policy
    Policy (..),
    mkPolicy,
    pPolicyName,
    pPolicyARN,

    -- ** PolicyVersion
    PolicyVersion (..),
    mkPolicyVersion,
    pvVersionId,
    pvCreateDate,
    pvIsDefaultVersion,

    -- ** PolicyVersionIdentifier
    PolicyVersionIdentifier (..),
    mkPolicyVersionIdentifier,
    pviPolicyName,
    pviPolicyVersionId,

    -- ** PresignedURLConfig
    PresignedURLConfig (..),
    mkPresignedURLConfig,
    pucExpiresInSec,
    pucRoleARN,

    -- ** ProvisioningHook
    ProvisioningHook (..),
    mkProvisioningHook,
    phTargetARN,
    phPayloadVersion,

    -- ** ProvisioningTemplateSummary
    ProvisioningTemplateSummary (..),
    mkProvisioningTemplateSummary,
    ptsLastModifiedDate,
    ptsTemplateName,
    ptsEnabled,
    ptsCreationDate,
    ptsTemplateARN,
    ptsDescription,

    -- ** ProvisioningTemplateVersionSummary
    ProvisioningTemplateVersionSummary (..),
    mkProvisioningTemplateVersionSummary,
    ptvsVersionId,
    ptvsCreationDate,
    ptvsIsDefaultVersion,

    -- ** PublishFindingToSNSParams
    PublishFindingToSNSParams (..),
    mkPublishFindingToSNSParams,
    pftspTopicARN,

    -- ** PutAssetPropertyValueEntry
    PutAssetPropertyValueEntry (..),
    mkPutAssetPropertyValueEntry,
    papvePropertyValues,
    papveEntryId,
    papvePropertyAlias,
    papvePropertyId,
    papveAssetId,

    -- ** PutItemInput
    PutItemInput (..),
    mkPutItemInput,
    piiTableName,

    -- ** RateIncreaseCriteria
    RateIncreaseCriteria (..),
    mkRateIncreaseCriteria,
    ricNumberOfNotifiedThings,
    ricNumberOfSucceededThings,

    -- ** RegistrationConfig
    RegistrationConfig (..),
    mkRegistrationConfig,
    rcTemplateBody,
    rcRoleARN,

    -- ** RelatedResource
    RelatedResource (..),
    mkRelatedResource,
    rrAdditionalInfo,
    rrResourceType,
    rrResourceIdentifier,

    -- ** ReplaceDefaultPolicyVersionParams
    ReplaceDefaultPolicyVersionParams (..),
    mkReplaceDefaultPolicyVersionParams,
    rdpvpTemplateName,

    -- ** RepublishAction
    RepublishAction (..),
    mkRepublishAction,
    raTopic,
    raQos,
    raRoleARN,

    -- ** ResourceIdentifier
    ResourceIdentifier (..),
    mkResourceIdentifier,
    riIamRoleARN,
    riClientId,
    riRoleAliasARN,
    riCaCertificateId,
    riDeviceCertificateId,
    riAccount,
    riPolicyVersionIdentifier,
    riCognitoIdentityPoolId,

    -- ** RoleAliasDescription
    RoleAliasDescription (..),
    mkRoleAliasDescription,
    radRoleAliasARN,
    radLastModifiedDate,
    radRoleAlias,
    radOwner,
    radCreationDate,
    radCredentialDurationSeconds,
    radRoleARN,

    -- ** S3Action
    S3Action (..),
    mkS3Action,
    sCannedACL,
    sBucketName,
    sKey,
    sRoleARN,

    -- ** S3Destination
    S3Destination (..),
    mkS3Destination,
    sdPrefix,
    sdBucket,

    -- ** S3Location
    S3Location (..),
    mkS3Location,
    slBucket,
    slKey,
    slVersion,

    -- ** SNSAction
    SNSAction (..),
    mkSNSAction,
    snsaTargetARN,
    snsaMessageFormat,
    snsaRoleARN,

    -- ** SalesforceAction
    SalesforceAction (..),
    mkSalesforceAction,
    saUrl,
    saToken,

    -- ** ScheduledAuditMetadata
    ScheduledAuditMetadata (..),
    mkScheduledAuditMetadata,
    samFrequency,
    samScheduledAuditName,
    samDayOfMonth,
    samDayOfWeek,
    samScheduledAuditARN,

    -- ** SecurityProfileIdentifier
    SecurityProfileIdentifier (..),
    mkSecurityProfileIdentifier,
    spiArn,
    spiName,

    -- ** SecurityProfileTarget
    SecurityProfileTarget (..),
    mkSecurityProfileTarget,
    sptArn,

    -- ** SecurityProfileTargetMapping
    SecurityProfileTargetMapping (..),
    mkSecurityProfileTargetMapping,
    sptmSecurityProfileIdentifier,
    sptmTarget,

    -- ** ServerCertificateSummary
    ServerCertificateSummary (..),
    mkServerCertificateSummary,
    scsServerCertificateStatusDetail,
    scsServerCertificateStatus,
    scsServerCertificateARN,

    -- ** SigV4Authorization
    SigV4Authorization (..),
    mkSigV4Authorization,
    svaServiceName,
    svaSigningRegion,
    svaRoleARN,

    -- ** SigningProfileParameter
    SigningProfileParameter (..),
    mkSigningProfileParameter,
    sppPlatform,
    sppCertificateARN,
    sppCertificatePathOnDevice,

    -- ** SqsAction
    SqsAction (..),
    mkSqsAction,
    saUseBase64,
    saQueueURL,
    saRoleARN,

    -- ** StartSigningJobParameter
    StartSigningJobParameter (..),
    mkStartSigningJobParameter,
    ssjpDestination,
    ssjpSigningProfileName,
    ssjpSigningProfileParameter,

    -- ** StatisticalThreshold
    StatisticalThreshold (..),
    mkStatisticalThreshold,
    stStatistic,

    -- ** Statistics
    Statistics (..),
    mkStatistics,
    sStdDeviation,
    sMaximum,
    sAverage,
    sCount,
    sMinimum,
    sVariance,
    sSumOfSquares,
    sSum,

    -- ** StepFunctionsAction
    StepFunctionsAction (..),
    mkStepFunctionsAction,
    sfaExecutionNamePrefix,
    sfaStateMachineName,
    sfaRoleARN,

    -- ** Stream
    Stream (..),
    mkStream,
    sFileId,
    sStreamId,

    -- ** StreamFile
    StreamFile (..),
    mkStreamFile,
    sfS3Location,
    sfFileId,

    -- ** StreamInfo
    StreamInfo (..),
    mkStreamInfo,
    siLastUpdatedAt,
    siCreatedAt,
    siStreamVersion,
    siStreamARN,
    siFiles,
    siDescription,
    siStreamId,
    siRoleARN,

    -- ** StreamSummary
    StreamSummary (..),
    mkStreamSummary,
    ssStreamVersion,
    ssStreamARN,
    ssDescription,
    ssStreamId,

    -- ** TLSContext
    TLSContext (..),
    mkTLSContext,
    tcServerName,

    -- ** Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- ** TaskStatistics
    TaskStatistics (..),
    mkTaskStatistics,
    tsNonCompliantChecks,
    tsWaitingForDataCollectionChecks,
    tsFailedChecks,
    tsTotalChecks,
    tsInProgressChecks,
    tsCompliantChecks,
    tsCanceledChecks,

    -- ** TaskStatisticsForAuditCheck
    TaskStatisticsForAuditCheck (..),
    mkTaskStatisticsForAuditCheck,
    tsfacCanceledFindingsCount,
    tsfacSkippedFindingsCount,
    tsfacTotalFindingsCount,
    tsfacFailedFindingsCount,
    tsfacSucceededFindingsCount,

    -- ** ThingAttribute
    ThingAttribute (..),
    mkThingAttribute,
    taThingTypeName,
    taThingARN,
    taAttributes,
    taVersion,
    taThingName,

    -- ** ThingConnectivity
    ThingConnectivity (..),
    mkThingConnectivity,
    tcConnected,
    tcTimestamp,

    -- ** ThingDocument
    ThingDocument (..),
    mkThingDocument,
    tdThingGroupNames,
    tdThingTypeName,
    tdShadow,
    tdAttributes,
    tdConnectivity,
    tdThingName,
    tdThingId,

    -- ** ThingGroupDocument
    ThingGroupDocument (..),
    mkThingGroupDocument,
    tgdParentGroupNames,
    tgdThingGroupId,
    tgdThingGroupName,
    tgdAttributes,
    tgdThingGroupDescription,

    -- ** ThingGroupIndexingConfiguration
    ThingGroupIndexingConfiguration (..),
    mkThingGroupIndexingConfiguration,
    tgicManagedFields,
    tgicCustomFields,
    tgicThingGroupIndexingMode,

    -- ** ThingGroupMetadata
    ThingGroupMetadata (..),
    mkThingGroupMetadata,
    tgmRootToParentThingGroups,
    tgmParentGroupName,
    tgmCreationDate,

    -- ** ThingGroupProperties
    ThingGroupProperties (..),
    mkThingGroupProperties,
    tgpAttributePayload,
    tgpThingGroupDescription,

    -- ** ThingIndexingConfiguration
    ThingIndexingConfiguration (..),
    mkThingIndexingConfiguration,
    ticThingIndexingMode,
    ticManagedFields,
    ticThingConnectivityIndexingMode,
    ticCustomFields,

    -- ** ThingTypeDefinition
    ThingTypeDefinition (..),
    mkThingTypeDefinition,
    ttdThingTypeProperties,
    ttdThingTypeName,
    ttdThingTypeMetadata,
    ttdThingTypeARN,

    -- ** ThingTypeMetadata
    ThingTypeMetadata (..),
    mkThingTypeMetadata,
    ttmDeprecationDate,
    ttmCreationDate,
    ttmDeprecated,

    -- ** ThingTypeProperties
    ThingTypeProperties (..),
    mkThingTypeProperties,
    ttpSearchableAttributes,
    ttpThingTypeDescription,

    -- ** TimeoutConfig
    TimeoutConfig (..),
    mkTimeoutConfig,
    tcInProgressTimeoutInMinutes,

    -- ** TimestreamAction
    TimestreamAction (..),
    mkTimestreamAction,
    taDatabaseName,
    taDimensions,
    taTimestamp,
    taTableName,
    taRoleARN,

    -- ** TimestreamDimension
    TimestreamDimension (..),
    mkTimestreamDimension,
    tdValue,
    tdName,

    -- ** TimestreamTimestamp
    TimestreamTimestamp (..),
    mkTimestreamTimestamp,
    ttValue,
    ttUnit,

    -- ** TopicRule
    TopicRule (..),
    mkTopicRule,
    trCreatedAt,
    trActions,
    trAwsIotSqlVersion,
    trErrorAction,
    trRuleDisabled,
    trRuleName,
    trSql,
    trDescription,

    -- ** TopicRuleDestination
    TopicRuleDestination (..),
    mkTopicRuleDestination,
    trdStatus,
    trdHttpURLProperties,
    trdArn,
    trdStatusReason,

    -- ** TopicRuleDestinationConfiguration
    TopicRuleDestinationConfiguration (..),
    mkTopicRuleDestinationConfiguration,
    trdcHttpURLConfiguration,

    -- ** TopicRuleDestinationSummary
    TopicRuleDestinationSummary (..),
    mkTopicRuleDestinationSummary,
    trdsStatus,
    trdsHttpURLSummary,
    trdsArn,
    trdsStatusReason,

    -- ** TopicRuleListItem
    TopicRuleListItem (..),
    mkTopicRuleListItem,
    trliCreatedAt,
    trliRuleDisabled,
    trliRuleName,
    trliRuleARN,
    trliTopicPattern,

    -- ** TopicRulePayload
    TopicRulePayload (..),
    mkTopicRulePayload,
    trpActions,
    trpAwsIotSqlVersion,
    trpErrorAction,
    trpRuleDisabled,
    trpSql,
    trpDescription,

    -- ** TransferData
    TransferData (..),
    mkTransferData,
    tdTransferDate,
    tdAcceptDate,
    tdTransferMessage,
    tdRejectDate,
    tdRejectReason,

    -- ** UpdateCACertificateParams
    UpdateCACertificateParams (..),
    mkUpdateCACertificateParams,
    ucacpAction,

    -- ** UpdateDeviceCertificateParams
    UpdateDeviceCertificateParams (..),
    mkUpdateDeviceCertificateParams,
    udcpAction,

    -- ** ValidationError
    ValidationError (..),
    mkValidationError,
    veErrorMessage,

    -- ** ViolationEvent
    ViolationEvent (..),
    mkViolationEvent,
    veViolationEventType,
    veViolationId,
    veBehavior,
    veMetricValue,
    veSecurityProfileName,
    veViolationEventTime,
    veThingName,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.DateTime,
    Lude.Timestamp,
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
import qualified Network.AWS.Prelude as Lude

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

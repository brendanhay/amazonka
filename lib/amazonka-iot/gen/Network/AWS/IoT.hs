{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

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
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- ** TaskAlreadyExistsException
    , _TaskAlreadyExistsException

    -- ** CertificateConflictException
    , _CertificateConflictException

    -- ** SqlParseException
    , _SqlParseException

    -- ** IndexNotReadyException
    , _IndexNotReadyException

    -- ** InvalidRequestException
    , _InvalidRequestException

    -- ** TransferConflictException
    , _TransferConflictException

    -- ** CertificateStateException
    , _CertificateStateException

    -- ** InvalidResponseException
    , _InvalidResponseException

    -- ** RegistrationCodeValidationException
    , _RegistrationCodeValidationException

    -- ** MalformedPolicyException
    , _MalformedPolicyException

    -- ** DeleteConflictException
    , _DeleteConflictException

    -- ** ResourceAlreadyExistsException
    , _ResourceAlreadyExistsException

    -- ** NotConfiguredException
    , _NotConfiguredException

    -- ** CertificateValidationException
    , _CertificateValidationException

    -- ** ResourceRegistrationFailureException
    , _ResourceRegistrationFailureException

    -- ** InvalidQueryException
    , _InvalidQueryException

    -- ** TransferAlreadyCompletedException
    , _TransferAlreadyCompletedException

    -- ** ThrottlingException
    , _ThrottlingException

    -- ** InvalidAggregationException
    , _InvalidAggregationException

    -- ** ConflictingResourceUpdateException
    , _ConflictingResourceUpdateException

    -- ** InternalFailureException
    , _InternalFailureException

    -- ** VersionsLimitExceededException
    , _VersionsLimitExceededException

    -- ** ServiceUnavailableException
    , _ServiceUnavailableException

    -- ** InternalException
    , _InternalException

    -- ** VersionConflictException
    , _VersionConflictException

    -- ** UnauthorizedException
    , _UnauthorizedException

    -- ** InvalidStateTransitionException
    , _InvalidStateTransitionException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetCardinality 
    , module Network.AWS.IoT.GetCardinality

    -- ** CreateDomainConfiguration 
    , module Network.AWS.IoT.CreateDomainConfiguration

    -- ** DeleteSecurityProfile 
    , module Network.AWS.IoT.DeleteSecurityProfile

    -- ** UpdateSecurityProfile 
    , module Network.AWS.IoT.UpdateSecurityProfile

    -- ** ListSecurityProfiles (Paginated)
    , module Network.AWS.IoT.ListSecurityProfiles

    -- ** ListPolicies (Paginated)
    , module Network.AWS.IoT.ListPolicies

    -- ** DescribeProvisioningTemplate 
    , module Network.AWS.IoT.DescribeProvisioningTemplate

    -- ** UpdateMitigationAction 
    , module Network.AWS.IoT.UpdateMitigationAction

    -- ** DeleteMitigationAction 
    , module Network.AWS.IoT.DeleteMitigationAction

    -- ** DeleteJobExecution 
    , module Network.AWS.IoT.DeleteJobExecution

    -- ** CreatePolicy 
    , module Network.AWS.IoT.CreatePolicy

    -- ** RegisterCertificate 
    , module Network.AWS.IoT.RegisterCertificate

    -- ** DeleteDynamicThingGroup 
    , module Network.AWS.IoT.DeleteDynamicThingGroup

    -- ** ListThingPrincipals (Paginated)
    , module Network.AWS.IoT.ListThingPrincipals

    -- ** UpdateDynamicThingGroup 
    , module Network.AWS.IoT.UpdateDynamicThingGroup

    -- ** DescribeRoleAlias 
    , module Network.AWS.IoT.DescribeRoleAlias

    -- ** CreateProvisioningTemplateVersion 
    , module Network.AWS.IoT.CreateProvisioningTemplateVersion

    -- ** CreateOTAUpdate 
    , module Network.AWS.IoT.CreateOTAUpdate

    -- ** DescribeDefaultAuthorizer 
    , module Network.AWS.IoT.DescribeDefaultAuthorizer

    -- ** ListAuditMitigationActionsTasks (Paginated)
    , module Network.AWS.IoT.ListAuditMitigationActionsTasks

    -- ** ListThingRegistrationTaskReports (Paginated)
    , module Network.AWS.IoT.ListThingRegistrationTaskReports

    -- ** ListPrincipalThings (Paginated)
    , module Network.AWS.IoT.ListPrincipalThings

    -- ** RemoveThingFromThingGroup 
    , module Network.AWS.IoT.RemoveThingFromThingGroup

    -- ** DescribeEventConfigurations 
    , module Network.AWS.IoT.DescribeEventConfigurations

    -- ** ListTopicRuleDestinations (Paginated)
    , module Network.AWS.IoT.ListTopicRuleDestinations

    -- ** RegisterCertificateWithoutCA 
    , module Network.AWS.IoT.RegisterCertificateWithoutCA

    -- ** ListTagsForResource (Paginated)
    , module Network.AWS.IoT.ListTagsForResource

    -- ** ListThingGroups (Paginated)
    , module Network.AWS.IoT.ListThingGroups

    -- ** ListScheduledAudits (Paginated)
    , module Network.AWS.IoT.ListScheduledAudits

    -- ** DescribeThingRegistrationTask 
    , module Network.AWS.IoT.DescribeThingRegistrationTask

    -- ** UpdateScheduledAudit 
    , module Network.AWS.IoT.UpdateScheduledAudit

    -- ** DeleteScheduledAudit 
    , module Network.AWS.IoT.DeleteScheduledAudit

    -- ** DescribeAuditFinding 
    , module Network.AWS.IoT.DescribeAuditFinding

    -- ** DescribeDimension 
    , module Network.AWS.IoT.DescribeDimension

    -- ** GetLoggingOptions 
    , module Network.AWS.IoT.GetLoggingOptions

    -- ** DeleteAccountAuditConfiguration 
    , module Network.AWS.IoT.DeleteAccountAuditConfiguration

    -- ** UpdateAccountAuditConfiguration 
    , module Network.AWS.IoT.UpdateAccountAuditConfiguration

    -- ** GetOTAUpdate 
    , module Network.AWS.IoT.GetOTAUpdate

    -- ** GetEffectivePolicies 
    , module Network.AWS.IoT.GetEffectivePolicies

    -- ** ListThingTypes (Paginated)
    , module Network.AWS.IoT.ListThingTypes

    -- ** SetV2LoggingOptions 
    , module Network.AWS.IoT.SetV2LoggingOptions

    -- ** CreateProvisioningTemplate 
    , module Network.AWS.IoT.CreateProvisioningTemplate

    -- ** ListThingGroupsForThing (Paginated)
    , module Network.AWS.IoT.ListThingGroupsForThing

    -- ** CreateCertificateFromCsr 
    , module Network.AWS.IoT.CreateCertificateFromCsr

    -- ** DeleteThing 
    , module Network.AWS.IoT.DeleteThing

    -- ** UpdateThing 
    , module Network.AWS.IoT.UpdateThing

    -- ** DeleteProvisioningTemplate 
    , module Network.AWS.IoT.DeleteProvisioningTemplate

    -- ** UpdateProvisioningTemplate 
    , module Network.AWS.IoT.UpdateProvisioningTemplate

    -- ** DescribeMitigationAction 
    , module Network.AWS.IoT.DescribeMitigationAction

    -- ** StartThingRegistrationTask 
    , module Network.AWS.IoT.StartThingRegistrationTask

    -- ** CreateScheduledAudit 
    , module Network.AWS.IoT.CreateScheduledAudit

    -- ** ListAuthorizers (Paginated)
    , module Network.AWS.IoT.ListAuthorizers

    -- ** ListJobExecutionsForJob (Paginated)
    , module Network.AWS.IoT.ListJobExecutionsForJob

    -- ** RemoveThingFromBillingGroup 
    , module Network.AWS.IoT.RemoveThingFromBillingGroup

    -- ** SearchIndex 
    , module Network.AWS.IoT.SearchIndex

    -- ** CreateThingType 
    , module Network.AWS.IoT.CreateThingType

    -- ** DescribeSecurityProfile 
    , module Network.AWS.IoT.DescribeSecurityProfile

    -- ** DeleteV2LoggingLevel 
    , module Network.AWS.IoT.DeleteV2LoggingLevel

    -- ** SetDefaultAuthorizer 
    , module Network.AWS.IoT.SetDefaultAuthorizer

    -- ** DescribeJobExecution 
    , module Network.AWS.IoT.DescribeJobExecution

    -- ** CancelCertificateTransfer 
    , module Network.AWS.IoT.CancelCertificateTransfer

    -- ** GetIndexingConfiguration 
    , module Network.AWS.IoT.GetIndexingConfiguration

    -- ** ListAuditMitigationActionsExecutions (Paginated)
    , module Network.AWS.IoT.ListAuditMitigationActionsExecutions

    -- ** DescribeAuditMitigationActionsTask 
    , module Network.AWS.IoT.DescribeAuditMitigationActionsTask

    -- ** GetStatistics 
    , module Network.AWS.IoT.GetStatistics

    -- ** DeleteRoleAlias 
    , module Network.AWS.IoT.DeleteRoleAlias

    -- ** UpdateRoleAlias 
    , module Network.AWS.IoT.UpdateRoleAlias

    -- ** DeletePolicyVersion 
    , module Network.AWS.IoT.DeletePolicyVersion

    -- ** DisableTopicRule 
    , module Network.AWS.IoT.DisableTopicRule

    -- ** CreateTopicRule 
    , module Network.AWS.IoT.CreateTopicRule

    -- ** CreateJob 
    , module Network.AWS.IoT.CreateJob

    -- ** DescribeIndex 
    , module Network.AWS.IoT.DescribeIndex

    -- ** AssociateTargetsWithJob 
    , module Network.AWS.IoT.AssociateTargetsWithJob

    -- ** AttachSecurityProfile 
    , module Network.AWS.IoT.AttachSecurityProfile

    -- ** ListAttachedPolicies (Paginated)
    , module Network.AWS.IoT.ListAttachedPolicies

    -- ** CreatePolicyVersion 
    , module Network.AWS.IoT.CreatePolicyVersion

    -- ** ListCACertificates (Paginated)
    , module Network.AWS.IoT.ListCACertificates

    -- ** DeleteTopicRule 
    , module Network.AWS.IoT.DeleteTopicRule

    -- ** GetJobDocument 
    , module Network.AWS.IoT.GetJobDocument

    -- ** DescribeProvisioningTemplateVersion 
    , module Network.AWS.IoT.DescribeProvisioningTemplateVersion

    -- ** CancelAuditTask 
    , module Network.AWS.IoT.CancelAuditTask

    -- ** CreateRoleAlias 
    , module Network.AWS.IoT.CreateRoleAlias

    -- ** DeleteCACertificate 
    , module Network.AWS.IoT.DeleteCACertificate

    -- ** UpdateCACertificate 
    , module Network.AWS.IoT.UpdateCACertificate

    -- ** ListTopicRules (Paginated)
    , module Network.AWS.IoT.ListTopicRules

    -- ** TransferCertificate 
    , module Network.AWS.IoT.TransferCertificate

    -- ** ListJobs (Paginated)
    , module Network.AWS.IoT.ListJobs

    -- ** ListRoleAliases (Paginated)
    , module Network.AWS.IoT.ListRoleAliases

    -- ** StartOnDemandAuditTask 
    , module Network.AWS.IoT.StartOnDemandAuditTask

    -- ** DescribeThingGroup 
    , module Network.AWS.IoT.DescribeThingGroup

    -- ** DeleteJob 
    , module Network.AWS.IoT.DeleteJob

    -- ** ListTargetsForSecurityProfile (Paginated)
    , module Network.AWS.IoT.ListTargetsForSecurityProfile

    -- ** UpdateJob 
    , module Network.AWS.IoT.UpdateJob

    -- ** StartAuditMitigationActionsTask 
    , module Network.AWS.IoT.StartAuditMitigationActionsTask

    -- ** GetTopicRule 
    , module Network.AWS.IoT.GetTopicRule

    -- ** DescribeThing 
    , module Network.AWS.IoT.DescribeThing

    -- ** ListDomainConfigurations (Paginated)
    , module Network.AWS.IoT.ListDomainConfigurations

    -- ** ListAuditTasks (Paginated)
    , module Network.AWS.IoT.ListAuditTasks

    -- ** DescribeAccountAuditConfiguration 
    , module Network.AWS.IoT.DescribeAccountAuditConfiguration

    -- ** DeleteDimension 
    , module Network.AWS.IoT.DeleteDimension

    -- ** UpdateDimension 
    , module Network.AWS.IoT.UpdateDimension

    -- ** DeletePolicy 
    , module Network.AWS.IoT.DeletePolicy

    -- ** ListThingsInThingGroup (Paginated)
    , module Network.AWS.IoT.ListThingsInThingGroup

    -- ** ListAuditFindings (Paginated)
    , module Network.AWS.IoT.ListAuditFindings

    -- ** DescribeScheduledAudit 
    , module Network.AWS.IoT.DescribeScheduledAudit

    -- ** CreateMitigationAction 
    , module Network.AWS.IoT.CreateMitigationAction

    -- ** ConfirmTopicRuleDestination 
    , module Network.AWS.IoT.ConfirmTopicRuleDestination

    -- ** ListCertificates (Paginated)
    , module Network.AWS.IoT.ListCertificates

    -- ** ListMitigationActions (Paginated)
    , module Network.AWS.IoT.ListMitigationActions

    -- ** DescribeAuthorizer 
    , module Network.AWS.IoT.DescribeAuthorizer

    -- ** GetPolicyVersion 
    , module Network.AWS.IoT.GetPolicyVersion

    -- ** ListActiveViolations (Paginated)
    , module Network.AWS.IoT.ListActiveViolations

    -- ** ValidateSecurityProfileBehaviors 
    , module Network.AWS.IoT.ValidateSecurityProfileBehaviors

    -- ** ListViolationEvents (Paginated)
    , module Network.AWS.IoT.ListViolationEvents

    -- ** DeleteCertificate 
    , module Network.AWS.IoT.DeleteCertificate

    -- ** UpdateCertificate 
    , module Network.AWS.IoT.UpdateCertificate

    -- ** CreateDimension 
    , module Network.AWS.IoT.CreateDimension

    -- ** UpdateIndexingConfiguration 
    , module Network.AWS.IoT.UpdateIndexingConfiguration

    -- ** CreateProvisioningClaim 
    , module Network.AWS.IoT.CreateProvisioningClaim

    -- ** TestInvokeAuthorizer 
    , module Network.AWS.IoT.TestInvokeAuthorizer

    -- ** CreateThingGroup 
    , module Network.AWS.IoT.CreateThingGroup

    -- ** CreateTopicRuleDestination 
    , module Network.AWS.IoT.CreateTopicRuleDestination

    -- ** DetachPolicy 
    , module Network.AWS.IoT.DetachPolicy

    -- ** DescribeJob 
    , module Network.AWS.IoT.DescribeJob

    -- ** AddThingToBillingGroup 
    , module Network.AWS.IoT.AddThingToBillingGroup

    -- ** UpdateTopicRuleDestination 
    , module Network.AWS.IoT.UpdateTopicRuleDestination

    -- ** DeleteTopicRuleDestination 
    , module Network.AWS.IoT.DeleteTopicRuleDestination

    -- ** DeleteThingGroup 
    , module Network.AWS.IoT.DeleteThingGroup

    -- ** UpdateThingGroup 
    , module Network.AWS.IoT.UpdateThingGroup

    -- ** ListOTAUpdates (Paginated)
    , module Network.AWS.IoT.ListOTAUpdates

    -- ** DeleteOTAUpdate 
    , module Network.AWS.IoT.DeleteOTAUpdate

    -- ** CreateDynamicThingGroup 
    , module Network.AWS.IoT.CreateDynamicThingGroup

    -- ** DetachSecurityProfile 
    , module Network.AWS.IoT.DetachSecurityProfile

    -- ** ListOutgoingCertificates (Paginated)
    , module Network.AWS.IoT.ListOutgoingCertificates

    -- ** DeleteProvisioningTemplateVersion 
    , module Network.AWS.IoT.DeleteProvisioningTemplateVersion

    -- ** DescribeCACertificate 
    , module Network.AWS.IoT.DescribeCACertificate

    -- ** ListProvisioningTemplateVersions (Paginated)
    , module Network.AWS.IoT.ListProvisioningTemplateVersions

    -- ** GetRegistrationCode 
    , module Network.AWS.IoT.GetRegistrationCode

    -- ** ListBillingGroups (Paginated)
    , module Network.AWS.IoT.ListBillingGroups

    -- ** DeleteThingType 
    , module Network.AWS.IoT.DeleteThingType

    -- ** DeleteBillingGroup 
    , module Network.AWS.IoT.DeleteBillingGroup

    -- ** AddThingToThingGroup 
    , module Network.AWS.IoT.AddThingToThingGroup

    -- ** UpdateBillingGroup 
    , module Network.AWS.IoT.UpdateBillingGroup

    -- ** GetTopicRuleDestination 
    , module Network.AWS.IoT.GetTopicRuleDestination

    -- ** ListCertificatesByCA (Paginated)
    , module Network.AWS.IoT.ListCertificatesByCA

    -- ** UpdateAuditSuppression 
    , module Network.AWS.IoT.UpdateAuditSuppression

    -- ** AttachThingPrincipal 
    , module Network.AWS.IoT.AttachThingPrincipal

    -- ** ListThings (Paginated)
    , module Network.AWS.IoT.ListThings

    -- ** DeleteAuditSuppression 
    , module Network.AWS.IoT.DeleteAuditSuppression

    -- ** RegisterThing 
    , module Network.AWS.IoT.RegisterThing

    -- ** ListAuditSuppressions (Paginated)
    , module Network.AWS.IoT.ListAuditSuppressions

    -- ** DescribeDomainConfiguration 
    , module Network.AWS.IoT.DescribeDomainConfiguration

    -- ** DescribeAuditTask 
    , module Network.AWS.IoT.DescribeAuditTask

    -- ** DeleteRegistrationCode 
    , module Network.AWS.IoT.DeleteRegistrationCode

    -- ** UpdateStream 
    , module Network.AWS.IoT.UpdateStream

    -- ** DeleteStream 
    , module Network.AWS.IoT.DeleteStream

    -- ** ListStreams (Paginated)
    , module Network.AWS.IoT.ListStreams

    -- ** CreateAuthorizer 
    , module Network.AWS.IoT.CreateAuthorizer

    -- ** TestAuthorization 
    , module Network.AWS.IoT.TestAuthorization

    -- ** ListIndices (Paginated)
    , module Network.AWS.IoT.ListIndices

    -- ** UpdateAuthorizer 
    , module Network.AWS.IoT.UpdateAuthorizer

    -- ** DeleteAuthorizer 
    , module Network.AWS.IoT.DeleteAuthorizer

    -- ** CreateThing 
    , module Network.AWS.IoT.CreateThing

    -- ** CreateStream 
    , module Network.AWS.IoT.CreateStream

    -- ** CancelAuditMitigationActionsTask 
    , module Network.AWS.IoT.CancelAuditMitigationActionsTask

    -- ** CreateAuditSuppression 
    , module Network.AWS.IoT.CreateAuditSuppression

    -- ** CreateBillingGroup 
    , module Network.AWS.IoT.CreateBillingGroup

    -- ** ListProvisioningTemplates (Paginated)
    , module Network.AWS.IoT.ListProvisioningTemplates

    -- ** ListV2LoggingLevels (Paginated)
    , module Network.AWS.IoT.ListV2LoggingLevels

    -- ** TagResource 
    , module Network.AWS.IoT.TagResource

    -- ** StopThingRegistrationTask 
    , module Network.AWS.IoT.StopThingRegistrationTask

    -- ** DescribeCertificate 
    , module Network.AWS.IoT.DescribeCertificate

    -- ** ListTargetsForPolicy (Paginated)
    , module Network.AWS.IoT.ListTargetsForPolicy

    -- ** ClearDefaultAuthorizer 
    , module Network.AWS.IoT.ClearDefaultAuthorizer

    -- ** ReplaceTopicRule 
    , module Network.AWS.IoT.ReplaceTopicRule

    -- ** UntagResource 
    , module Network.AWS.IoT.UntagResource

    -- ** SetDefaultPolicyVersion 
    , module Network.AWS.IoT.SetDefaultPolicyVersion

    -- ** CancelJobExecution 
    , module Network.AWS.IoT.CancelJobExecution

    -- ** ListPolicyVersions 
    , module Network.AWS.IoT.ListPolicyVersions

    -- ** SetV2LoggingLevel 
    , module Network.AWS.IoT.SetV2LoggingLevel

    -- ** ListJobExecutionsForThing (Paginated)
    , module Network.AWS.IoT.ListJobExecutionsForThing

    -- ** AttachPolicy 
    , module Network.AWS.IoT.AttachPolicy

    -- ** CreateKeysAndCertificate 
    , module Network.AWS.IoT.CreateKeysAndCertificate

    -- ** ListThingsInBillingGroup (Paginated)
    , module Network.AWS.IoT.ListThingsInBillingGroup

    -- ** UpdateThingGroupsForThing 
    , module Network.AWS.IoT.UpdateThingGroupsForThing

    -- ** EnableTopicRule 
    , module Network.AWS.IoT.EnableTopicRule

    -- ** AcceptCertificateTransfer 
    , module Network.AWS.IoT.AcceptCertificateTransfer

    -- ** GetPercentiles 
    , module Network.AWS.IoT.GetPercentiles

    -- ** GetPolicy 
    , module Network.AWS.IoT.GetPolicy

    -- ** DescribeEndpoint 
    , module Network.AWS.IoT.DescribeEndpoint

    -- ** ListSecurityProfilesForTarget (Paginated)
    , module Network.AWS.IoT.ListSecurityProfilesForTarget

    -- ** UpdateEventConfigurations 
    , module Network.AWS.IoT.UpdateEventConfigurations

    -- ** RegisterCACertificate 
    , module Network.AWS.IoT.RegisterCACertificate

    -- ** DeleteDomainConfiguration 
    , module Network.AWS.IoT.DeleteDomainConfiguration

    -- ** UpdateDomainConfiguration 
    , module Network.AWS.IoT.UpdateDomainConfiguration

    -- ** SetLoggingOptions 
    , module Network.AWS.IoT.SetLoggingOptions

    -- ** DescribeThingType 
    , module Network.AWS.IoT.DescribeThingType

    -- ** ListDimensions (Paginated)
    , module Network.AWS.IoT.ListDimensions

    -- ** GetV2LoggingOptions 
    , module Network.AWS.IoT.GetV2LoggingOptions

    -- ** ListThingRegistrationTasks (Paginated)
    , module Network.AWS.IoT.ListThingRegistrationTasks

    -- ** RejectCertificateTransfer 
    , module Network.AWS.IoT.RejectCertificateTransfer

    -- ** DescribeAuditSuppression 
    , module Network.AWS.IoT.DescribeAuditSuppression

    -- ** DescribeStream 
    , module Network.AWS.IoT.DescribeStream

    -- ** CreateSecurityProfile 
    , module Network.AWS.IoT.CreateSecurityProfile

    -- ** DescribeBillingGroup 
    , module Network.AWS.IoT.DescribeBillingGroup

    -- ** DetachThingPrincipal 
    , module Network.AWS.IoT.DetachThingPrincipal

    -- ** CancelJob 
    , module Network.AWS.IoT.CancelJob

    -- ** DeprecateThingType 
    , module Network.AWS.IoT.DeprecateThingType

    -- * Types

    -- ** AlarmName
    , AlarmName (..)

    -- ** PayloadVersion
    , PayloadVersion (..)

    -- ** TimestreamTimestampValue
    , TimestreamTimestampValue (..)

    -- ** RegistrationCode
    , RegistrationCode (..)

    -- ** TaskStatus
    , TaskStatus (..)

    -- ** AwsJobExponentialRolloutRate
    , AwsJobExponentialRolloutRate (..)
    , mkAwsJobExponentialRolloutRate
    , ajerrBaseRatePerMinute
    , ajerrIncrementFactor
    , ajerrRateIncreaseCriteria

    -- ** ConfirmationToken
    , ConfirmationToken (..)

    -- ** RegistryS3BucketName
    , RegistryS3BucketName (..)

    -- ** AuditSuppression
    , AuditSuppression (..)
    , mkAuditSuppression
    , asCheckName
    , asResourceIdentifier
    , asDescription
    , asExpirationDate
    , asSuppressIndefinitely

    -- ** RoleAliasDescription
    , RoleAliasDescription (..)
    , mkRoleAliasDescription
    , radCreationDate
    , radCredentialDurationSeconds
    , radLastModifiedDate
    , radOwner
    , radRoleAlias
    , radRoleAliasArn
    , radRoleArn

    -- ** DeviceDefenderThingName
    , DeviceDefenderThingName (..)

    -- ** TargetArn
    , TargetArn (..)

    -- ** Stream
    , Stream (..)
    , mkStream
    , sFileId
    , sStreamId

    -- ** EnableIoTLoggingParams
    , EnableIoTLoggingParams (..)
    , mkEnableIoTLoggingParams
    , eitlpRoleArnForLogging
    , eitlpLogLevel

    -- ** Destination
    , Destination (..)
    , mkDestination
    , dS3Destination

    -- ** KeyValue
    , KeyValue (..)

    -- ** ClientId
    , ClientId (..)

    -- ** ResourceLogicalId
    , ResourceLogicalId (..)

    -- ** HeaderValue
    , HeaderValue (..)

    -- ** HttpUrlDestinationConfiguration
    , HttpUrlDestinationConfiguration (..)
    , mkHttpUrlDestinationConfiguration
    , hudcConfirmationUrl

    -- ** AuditMitigationActionsTaskTarget
    , AuditMitigationActionsTaskTarget (..)
    , mkAuditMitigationActionsTaskTarget
    , amattAuditCheckToReasonCodeFilter
    , amattAuditTaskId
    , amattFindingIds

    -- ** OTAUpdateSummary
    , OTAUpdateSummary (..)
    , mkOTAUpdateSummary
    , otausCreationDate
    , otausOtaUpdateArn
    , otausOtaUpdateId

    -- ** HttpQueryString
    , HttpQueryString (..)

    -- ** TopicRuleDestinationStatus
    , TopicRuleDestinationStatus (..)

    -- ** AcmCertificateArn
    , AcmCertificateArn (..)

    -- ** PolicyDocument
    , PolicyDocument (..)

    -- ** CertificateValidity
    , CertificateValidity (..)
    , mkCertificateValidity
    , cvNotAfter
    , cvNotBefore

    -- ** TemplateName
    , TemplateName (..)

    -- ** DomainConfigurationName
    , DomainConfigurationName (..)

    -- ** AlertTarget
    , AlertTarget (..)
    , mkAlertTarget
    , atAlertTargetArn
    , atRoleArn

    -- ** BehaviorCriteria
    , BehaviorCriteria (..)
    , mkBehaviorCriteria
    , bcComparisonOperator
    , bcConsecutiveDatapointsToAlarm
    , bcConsecutiveDatapointsToClear
    , bcDurationSeconds
    , bcStatisticalThreshold
    , bcValue

    -- ** Platform
    , Platform (..)

    -- ** SigningProfileName
    , SigningProfileName (..)

    -- ** IndexSchema
    , IndexSchema (..)

    -- ** ProvisioningTemplateVersionSummary
    , ProvisioningTemplateVersionSummary (..)
    , mkProvisioningTemplateVersionSummary
    , ptvsCreationDate
    , ptvsIsDefaultVersion
    , ptvsVersionId

    -- ** HashKeyField
    , HashKeyField (..)

    -- ** RoleAliasArn
    , RoleAliasArn (..)

    -- ** PrincipalId
    , PrincipalId (..)

    -- ** AttributeValue
    , AttributeValue (..)

    -- ** PolicyName
    , PolicyName (..)

    -- ** SqsAction
    , SqsAction (..)
    , mkSqsAction
    , saRoleArn
    , saQueueUrl
    , saUseBase64

    -- ** S3Key
    , S3Key (..)

    -- ** JobExecutionsRolloutConfig
    , JobExecutionsRolloutConfig (..)
    , mkJobExecutionsRolloutConfig
    , jercExponentialRate
    , jercMaximumPerMinute

    -- ** FunctionArn
    , FunctionArn (..)

    -- ** Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- ** PrivateKey
    , PrivateKey (..)

    -- ** AwsJobExecutionsRolloutConfig
    , AwsJobExecutionsRolloutConfig (..)
    , mkAwsJobExecutionsRolloutConfig
    , ajercExponentialRate
    , ajercMaximumPerMinute

    -- ** Field
    , Field (..)
    , mkField
    , fName
    , fType

    -- ** AssetPropertyIntegerValue
    , AssetPropertyIntegerValue (..)

    -- ** StreamDescription
    , StreamDescription (..)

    -- ** AddThingsToThingGroupParams
    , AddThingsToThingGroupParams (..)
    , mkAddThingsToThingGroupParams
    , atttgpThingGroupNames
    , atttgpOverrideDynamicGroups

    -- ** TimestreamDimensionValue
    , TimestreamDimensionValue (..)

    -- ** AwsJobAbortConfig
    , AwsJobAbortConfig (..)
    , mkAwsJobAbortConfig
    , ajacAbortCriteriaList

    -- ** CertificateName
    , CertificateName (..)

    -- ** SnsTopicArn
    , SnsTopicArn (..)

    -- ** S3Version
    , S3Version (..)

    -- ** PolicyVersion
    , PolicyVersion (..)
    , mkPolicyVersion
    , pvCreateDate
    , pvIsDefaultVersion
    , pvVersionId

    -- ** StreamInfo
    , StreamInfo (..)
    , mkStreamInfo
    , siCreatedAt
    , siDescription
    , siFiles
    , siLastUpdatedAt
    , siRoleArn
    , siStreamArn
    , siStreamId
    , siStreamVersion

    -- ** MqttUsername
    , MqttUsername (..)

    -- ** ServerName
    , ServerName (..)

    -- ** IndexStatus
    , IndexStatus (..)

    -- ** CannedAccessControlList
    , CannedAccessControlList (..)

    -- ** IotAnalyticsAction
    , IotAnalyticsAction (..)
    , mkIotAnalyticsAction
    , iaaBatchMode
    , iaaChannelArn
    , iaaChannelName
    , iaaRoleArn

    -- ** LogLevel
    , LogLevel (..)

    -- ** LogTargetName
    , LogTargetName (..)

    -- ** ResourceType
    , ResourceType (..)

    -- ** StreamFile
    , StreamFile (..)
    , mkStreamFile
    , sfFileId
    , sfS3Location

    -- ** CodeSigningSignature
    , CodeSigningSignature (..)
    , mkCodeSigningSignature
    , cssInlineDocument

    -- ** MitigationActionName
    , MitigationActionName (..)

    -- ** JobId
    , JobId (..)

    -- ** UpdateCACertificateParams
    , UpdateCACertificateParams (..)
    , mkUpdateCACertificateParams
    , ucacpAction

    -- ** JobArn
    , JobArn (..)

    -- ** ProvisioningHook
    , ProvisioningHook (..)
    , mkProvisioningHook
    , phTargetArn
    , phPayloadVersion

    -- ** RoleAlias
    , RoleAlias (..)

    -- ** AssetPropertyQuality
    , AssetPropertyQuality (..)

    -- ** BillingGroupDescription
    , BillingGroupDescription (..)

    -- ** EffectivePolicy
    , EffectivePolicy (..)
    , mkEffectivePolicy
    , epPolicyArn
    , epPolicyDocument
    , epPolicyName

    -- ** StateMachineName
    , StateMachineName (..)

    -- ** OTAUpdateDescription
    , OTAUpdateDescription (..)

    -- ** LogTargetType
    , LogTargetType (..)

    -- ** AuthorizerStatus
    , AuthorizerStatus (..)

    -- ** DetailsKey
    , DetailsKey (..)

    -- ** PartitionKey
    , PartitionKey (..)

    -- ** AuditTaskId
    , AuditTaskId (..)

    -- ** ProvisioningTemplateSummary
    , ProvisioningTemplateSummary (..)
    , mkProvisioningTemplateSummary
    , ptsCreationDate
    , ptsDescription
    , ptsEnabled
    , ptsLastModifiedDate
    , ptsTemplateArn
    , ptsTemplateName

    -- ** ThingTypeProperties
    , ThingTypeProperties (..)
    , mkThingTypeProperties
    , ttpSearchableAttributes
    , ttpThingTypeDescription

    -- ** ThingGroupIndexingConfiguration
    , ThingGroupIndexingConfiguration (..)
    , mkThingGroupIndexingConfiguration
    , tgicThingGroupIndexingMode
    , tgicCustomFields
    , tgicManagedFields

    -- ** AwsIotJobId
    , AwsIotJobId (..)

    -- ** ElasticsearchAction
    , ElasticsearchAction (..)
    , mkElasticsearchAction
    , eaRoleArn
    , eaEndpoint
    , eaIndex
    , eaType
    , eaId

    -- ** JobExecutionFailureType
    , JobExecutionFailureType (..)

    -- ** KeyPair
    , KeyPair (..)
    , mkKeyPair
    , kpPrivateKey
    , kpPublicKey

    -- ** CertificatePem
    , CertificatePem (..)

    -- ** ServerCertificateStatusDetail
    , ServerCertificateStatusDetail (..)

    -- ** MitigationActionType
    , MitigationActionType (..)

    -- ** DimensionArn
    , DimensionArn (..)

    -- ** AuditTaskMetadata
    , AuditTaskMetadata (..)
    , mkAuditTaskMetadata
    , atmTaskId
    , atmTaskStatus
    , atmTaskType

    -- ** ViolationEventType
    , ViolationEventType (..)

    -- ** ExecutionNamePrefix
    , ExecutionNamePrefix (..)

    -- ** AuditMitigationActionsExecutionStatus
    , AuditMitigationActionsExecutionStatus (..)

    -- ** CertificateId
    , CertificateId (..)

    -- ** CertificateArn
    , CertificateArn (..)

    -- ** SnsAction
    , SnsAction (..)
    , mkSnsAction
    , safTargetArn
    , safRoleArn
    , safMessageFormat

    -- ** TaskStatistics
    , TaskStatistics (..)
    , mkTaskStatistics
    , tsCanceledChecks
    , tsCompliantChecks
    , tsFailedChecks
    , tsInProgressChecks
    , tsNonCompliantChecks
    , tsTotalChecks
    , tsWaitingForDataCollectionChecks

    -- ** MissingContextValue
    , MissingContextValue (..)

    -- ** SecurityProfileTargetMapping
    , SecurityProfileTargetMapping (..)
    , mkSecurityProfileTargetMapping
    , sptmSecurityProfileIdentifier
    , sptmTarget

    -- ** TopicRuleListItem
    , TopicRuleListItem (..)
    , mkTopicRuleListItem
    , trliCreatedAt
    , trliRuleArn
    , trliRuleDisabled
    , trliRuleName
    , trliTopicPattern

    -- ** AssetPropertyOffsetInNanos
    , AssetPropertyOffsetInNanos (..)

    -- ** AuditTaskType
    , AuditTaskType (..)

    -- ** QueryVersion
    , QueryVersion (..)

    -- ** JobDocument
    , JobDocument (..)

    -- ** ThingAttribute
    , ThingAttribute (..)
    , mkThingAttribute
    , taAttributes
    , taThingArn
    , taThingName
    , taThingTypeName
    , taVersion

    -- ** UpdateDeviceCertificateParams
    , UpdateDeviceCertificateParams (..)
    , mkUpdateDeviceCertificateParams
    , udcpAction

    -- ** TaskId
    , TaskId (..)

    -- ** KeyName
    , KeyName (..)

    -- ** DynamoDBAction
    , DynamoDBAction (..)
    , mkDynamoDBAction
    , ddbaTableName
    , ddbaRoleArn
    , ddbaHashKeyField
    , ddbaHashKeyValue
    , ddbaHashKeyType
    , ddbaOperation
    , ddbaPayloadField
    , ddbaRangeKeyField
    , ddbaRangeKeyType
    , ddbaRangeKeyValue

    -- ** EvaluationStatistic
    , EvaluationStatistic (..)

    -- ** MitigationActionId
    , MitigationActionId (..)

    -- ** MitigationActionArn
    , MitigationActionArn (..)

    -- ** OTAUpdateErrorMessage
    , OTAUpdateErrorMessage (..)

    -- ** Prefix
    , Prefix (..)

    -- ** TimestreamTableName
    , TimestreamTableName (..)

    -- ** ReasonForNonComplianceCode
    , ReasonForNonComplianceCode (..)

    -- ** BehaviorMetric
    , BehaviorMetric (..)

    -- ** AbortConfig
    , AbortConfig (..)
    , mkAbortConfig
    , acCriteriaList

    -- ** StreamSummary
    , StreamSummary (..)
    , mkStreamSummary
    , ssDescription
    , ssStreamArn
    , ssStreamId
    , ssStreamVersion

    -- ** Token
    , Token (..)

    -- ** TopicRuleDestinationConfiguration
    , TopicRuleDestinationConfiguration (..)
    , mkTopicRuleDestinationConfiguration
    , trdcHttpUrlConfiguration

    -- ** ThingTypeName
    , ThingTypeName (..)

    -- ** AuditMitigationActionsTaskId
    , AuditMitigationActionsTaskId (..)

    -- ** SalesforceAction
    , SalesforceAction (..)
    , mkSalesforceAction
    , saToken
    , saUrl

    -- ** AuthorizerConfig
    , AuthorizerConfig (..)
    , mkAuthorizerConfig
    , acAllowAuthorizerOverride
    , acDefaultAuthorizerName

    -- ** IotEventsAction
    , IotEventsAction (..)
    , mkIotEventsAction
    , ieaInputName
    , ieaRoleArn
    , ieaBatchMode
    , ieaMessageId

    -- ** AuditCheckConfiguration
    , AuditCheckConfiguration (..)
    , mkAuditCheckConfiguration
    , accEnabled

    -- ** JobSummary
    , JobSummary (..)
    , mkJobSummary
    , jsCompletedAt
    , jsCreatedAt
    , jsJobArn
    , jsJobId
    , jsLastUpdatedAt
    , jsStatus
    , jsTargetSelection
    , jsThingGroupId

    -- ** BehaviorName
    , BehaviorName (..)

    -- ** ScheduledAuditName
    , ScheduledAuditName (..)

    -- ** AwsIotSqlVersion
    , AwsIotSqlVersion (..)

    -- ** Url
    , Url (..)

    -- ** AuditMitigationActionsTaskMetadata
    , AuditMitigationActionsTaskMetadata (..)
    , mkAuditMitigationActionsTaskMetadata
    , amatmStartTime
    , amatmTaskId
    , amatmTaskStatus

    -- ** TopicRuleDestination
    , TopicRuleDestination (..)
    , mkTopicRuleDestination
    , trdArn
    , trdHttpUrlProperties
    , trdStatus
    , trdStatusReason

    -- ** CertificateDescription
    , CertificateDescription (..)
    , mkCertificateDescription
    , cdCaCertificateId
    , cdCertificateArn
    , cdCertificateId
    , cdCertificateMode
    , cdCertificatePem
    , cdCreationDate
    , cdCustomerVersion
    , cdGenerationId
    , cdLastModifiedDate
    , cdOwnedBy
    , cdPreviousOwnedBy
    , cdStatus
    , cdTransferData
    , cdValidity

    -- ** Value
    , Value (..)

    -- ** ValidationError
    , ValidationError (..)
    , mkValidationError
    , veErrorMessage

    -- ** FileLocation
    , FileLocation (..)
    , mkFileLocation
    , flS3Location
    , flStream

    -- ** AwsJobAbortCriteriaFailureType
    , AwsJobAbortCriteriaFailureType (..)

    -- ** EndpointType
    , EndpointType (..)

    -- ** DimensionValueOperator
    , DimensionValueOperator (..)

    -- ** AuthorizerFunctionArn
    , AuthorizerFunctionArn (..)

    -- ** NamespaceId
    , NamespaceId (..)

    -- ** CertificateMode
    , CertificateMode (..)

    -- ** Action
    , Action (..)
    , mkAction
    , aCloudwatchAlarm
    , aCloudwatchLogs
    , aCloudwatchMetric
    , aDynamoDB
    , aDynamoDBv2
    , aElasticsearch
    , aFirehose
    , aHttp
    , aIotAnalytics
    , aIotEvents
    , aIotSiteWise
    , aKinesis
    , aLambda
    , aRepublish
    , aS3
    , aSalesforce
    , aSns
    , aSqs
    , aStepFunctions
    , aTimestream

    -- ** InlineDocument
    , InlineDocument (..)

    -- ** CodeSigningCertificateChain
    , CodeSigningCertificateChain (..)
    , mkCodeSigningCertificateChain
    , csccCertificateName
    , csccInlineDocument

    -- ** PublicKey
    , PublicKey (..)

    -- ** AssetPropertyTimeInSeconds
    , AssetPropertyTimeInSeconds (..)

    -- ** Protocol
    , Protocol (..)

    -- ** HttpUrlDestinationProperties
    , HttpUrlDestinationProperties (..)
    , mkHttpUrlDestinationProperties
    , hudpConfirmationUrl

    -- ** JobProcessDetails
    , JobProcessDetails (..)
    , mkJobProcessDetails
    , jpdNumberOfCanceledThings
    , jpdNumberOfFailedThings
    , jpdNumberOfInProgressThings
    , jpdNumberOfQueuedThings
    , jpdNumberOfRejectedThings
    , jpdNumberOfRemovedThings
    , jpdNumberOfSucceededThings
    , jpdNumberOfTimedOutThings
    , jpdProcessingTargets

    -- ** ThingGroupMetadata
    , ThingGroupMetadata (..)
    , mkThingGroupMetadata
    , tgmCreationDate
    , tgmParentGroupName
    , tgmRootToParentThingGroups

    -- ** ThingGroupArn
    , ThingGroupArn (..)

    -- ** AwsJobPresignedUrlConfig
    , AwsJobPresignedUrlConfig (..)
    , mkAwsJobPresignedUrlConfig
    , ajpucExpiresInSec

    -- ** AssetPropertyEntryId
    , AssetPropertyEntryId (..)

    -- ** DynamicGroupStatus
    , DynamicGroupStatus (..)

    -- ** TemplateDescription
    , TemplateDescription (..)

    -- ** DimensionStringValue
    , DimensionStringValue (..)

    -- ** AuditCheckRunStatus
    , AuditCheckRunStatus (..)

    -- ** ThingGroupId
    , ThingGroupId (..)

    -- ** MitigationActionIdentifier
    , MitigationActionIdentifier (..)
    , mkMitigationActionIdentifier
    , maiActionArn
    , maiActionName
    , maiCreationDate

    -- ** AuthorizerName
    , AuthorizerName (..)

    -- ** AggregationField
    , AggregationField (..)

    -- ** LambdaAction
    , LambdaAction (..)
    , mkLambdaAction
    , laFunctionArn

    -- ** CACertificateDescription
    , CACertificateDescription (..)
    , mkCACertificateDescription
    , cacdAutoRegistrationStatus
    , cacdCertificateArn
    , cacdCertificateId
    , cacdCertificatePem
    , cacdCreationDate
    , cacdCustomerVersion
    , cacdGenerationId
    , cacdLastModifiedDate
    , cacdOwnedBy
    , cacdStatus
    , cacdValidity

    -- ** ImplicitDeny
    , ImplicitDeny (..)
    , mkImplicitDeny
    , idPolicies

    -- ** AssetPropertyAlias
    , AssetPropertyAlias (..)

    -- ** ThingArn
    , ThingArn (..)

    -- ** AwsAccountId
    , AwsAccountId (..)

    -- ** StreamArn
    , StreamArn (..)

    -- ** AssetPropertyStringValue
    , AssetPropertyStringValue (..)

    -- ** ComparisonOperator
    , ComparisonOperator (..)

    -- ** RuleName
    , RuleName (..)

    -- ** AssetPropertyDoubleValue
    , AssetPropertyDoubleValue (..)

    -- ** CertificatePathOnDevice
    , CertificatePathOnDevice (..)

    -- ** ThingTypeId
    , ThingTypeId (..)

    -- ** JobDescription
    , JobDescription (..)

    -- ** PutAssetPropertyValueEntry
    , PutAssetPropertyValueEntry (..)
    , mkPutAssetPropertyValueEntry
    , papvePropertyValues
    , papveAssetId
    , papveEntryId
    , papvePropertyAlias
    , papvePropertyId

    -- ** JobExecutionSummary
    , JobExecutionSummary (..)
    , mkJobExecutionSummary
    , jesExecutionNumber
    , jesLastUpdatedAt
    , jesQueuedAt
    , jesStartedAt
    , jesStatus

    -- ** ThingTypeMetadata
    , ThingTypeMetadata (..)
    , mkThingTypeMetadata
    , ttmCreationDate
    , ttmDeprecated
    , ttmDeprecationDate

    -- ** ThingIndexingMode
    , ThingIndexingMode (..)

    -- ** BillingGroupArn
    , BillingGroupArn (..)

    -- ** InputName
    , InputName (..)

    -- ** ReplaceDefaultPolicyVersionParams
    , ReplaceDefaultPolicyVersionParams (..)
    , mkReplaceDefaultPolicyVersionParams
    , rdpvpTemplateName

    -- ** MitigationActionParams
    , MitigationActionParams (..)
    , mkMitigationActionParams
    , mapAddThingsToThingGroupParams
    , mapEnableIoTLoggingParams
    , mapPublishFindingToSnsParams
    , mapReplaceDefaultPolicyVersionParams
    , mapUpdateCACertificateParams
    , mapUpdateDeviceCertificateParams

    -- ** ReasonCode
    , ReasonCode (..)

    -- ** SigningProfileParameter
    , SigningProfileParameter (..)
    , mkSigningProfileParameter
    , sppCertificateArn
    , sppCertificatePathOnDevice
    , sppPlatform

    -- ** AuditCheckDetails
    , AuditCheckDetails (..)
    , mkAuditCheckDetails
    , acdCheckCompliant
    , acdCheckRunStatus
    , acdErrorCode
    , acdMessage
    , acdNonCompliantResourcesCount
    , acdSuppressedNonCompliantResourcesCount
    , acdTotalResourcesCount

    -- ** PolicyVersionIdentifier
    , PolicyVersionIdentifier (..)
    , mkPolicyVersionIdentifier
    , pviPolicyName
    , pviPolicyVersionId

    -- ** Topic
    , Topic (..)

    -- ** AuthResult
    , AuthResult (..)
    , mkAuthResult
    , arAllowed
    , arAuthDecision
    , arAuthInfo
    , arDenied
    , arMissingContextValues

    -- ** BucketName
    , BucketName (..)

    -- ** ScheduledAuditMetadata
    , ScheduledAuditMetadata (..)
    , mkScheduledAuditMetadata
    , samDayOfMonth
    , samDayOfWeek
    , samFrequency
    , samScheduledAuditArn
    , samScheduledAuditName

    -- ** PolicyTarget
    , PolicyTarget (..)

    -- ** LogGroupName
    , LogGroupName (..)

    -- ** ThingConnectivityIndexingMode
    , ThingConnectivityIndexingMode (..)

    -- ** HttpHeaderValue
    , HttpHeaderValue (..)

    -- ** MetricDimension
    , MetricDimension (..)
    , mkMetricDimension
    , mdDimensionName
    , mdOperator

    -- ** PresignedUrlConfig
    , PresignedUrlConfig (..)
    , mkPresignedUrlConfig
    , pucExpiresInSec
    , pucRoleArn

    -- ** AuditCheckName
    , AuditCheckName (..)

    -- ** Behavior
    , Behavior (..)
    , mkBehavior
    , bName
    , bCriteria
    , bMetric
    , bMetricDimension

    -- ** AuthorizerArn
    , AuthorizerArn (..)

    -- ** AbortAction
    , AbortAction (..)

    -- ** ServerCertificateStatus
    , ServerCertificateStatus (..)

    -- ** NextToken
    , NextToken (..)

    -- ** ViolationId
    , ViolationId (..)

    -- ** ChannelName
    , ChannelName (..)

    -- ** Cidr
    , Cidr (..)

    -- ** S3Destination
    , S3Destination (..)
    , mkS3Destination
    , sdBucket
    , sdPrefix

    -- ** DynamoDBv2Action
    , DynamoDBv2Action (..)
    , mkDynamoDBv2Action
    , dRoleArn
    , dPutItem

    -- ** AuditTaskStatus
    , AuditTaskStatus (..)

    -- ** StatisticalThreshold
    , StatisticalThreshold (..)
    , mkStatisticalThreshold
    , stStatistic

    -- ** ThingGroupDocument
    , ThingGroupDocument (..)
    , mkThingGroupDocument
    , tgdAttributes
    , tgdParentGroupNames
    , tgdThingGroupDescription
    , tgdThingGroupId
    , tgdThingGroupName

    -- ** ThingGroupName
    , ThingGroupName (..)

    -- ** ServerCertificateSummary
    , ServerCertificateSummary (..)
    , mkServerCertificateSummary
    , scsServerCertificateArn
    , scsServerCertificateStatus
    , scsServerCertificateStatusDetail

    -- ** FirehoseSeparator
    , FirehoseSeparator (..)

    -- ** EventType
    , EventType (..)

    -- ** SigV4Authorization
    , SigV4Authorization (..)
    , mkSigV4Authorization
    , svaSigningRegion
    , svaServiceName
    , svaRoleArn

    -- ** PayloadField
    , PayloadField (..)

    -- ** DomainConfigurationStatus
    , DomainConfigurationStatus (..)

    -- ** SecurityProfileTarget
    , SecurityProfileTarget (..)
    , mkSecurityProfileTarget
    , sptArn

    -- ** AssetPropertyValue
    , AssetPropertyValue (..)
    , mkAssetPropertyValue
    , apvValue
    , apvTimestamp
    , apvQuality

    -- ** TlsContext
    , TlsContext (..)
    , mkTlsContext
    , tcServerName

    -- ** StateValue
    , StateValue (..)

    -- ** DomainConfigurationSummary
    , DomainConfigurationSummary (..)
    , mkDomainConfigurationSummary
    , dcsDomainConfigurationArn
    , dcsDomainConfigurationName
    , dcsServiceType

    -- ** FirehoseAction
    , FirehoseAction (..)
    , mkFirehoseAction
    , faRoleArn
    , faDeliveryStreamName
    , faBatchMode
    , faSeparator

    -- ** TimestreamTimestamp
    , TimestreamTimestamp (..)
    , mkTimestreamTimestamp
    , ttValue
    , ttUnit

    -- ** JsonDocument
    , JsonDocument (..)

    -- ** OTAUpdateStatus
    , OTAUpdateStatus (..)

    -- ** Job
    , Job (..)
    , mkJob
    , jAbortConfig
    , jComment
    , jCompletedAt
    , jCreatedAt
    , jDescription
    , jForceCanceled
    , jJobArn
    , jJobExecutionsRolloutConfig
    , jJobId
    , jJobProcessDetails
    , jLastUpdatedAt
    , jNamespaceId
    , jPresignedUrlConfig
    , jReasonCode
    , jStatus
    , jTargetSelection
    , jTargets
    , jTimeoutConfig

    -- ** PrincipalArn
    , PrincipalArn (..)

    -- ** TopicRule
    , TopicRule (..)
    , mkTopicRule
    , trActions
    , trAwsIotSqlVersion
    , trCreatedAt
    , trDescription
    , trErrorAction
    , trRuleDisabled
    , trRuleName
    , trSql

    -- ** Denied
    , Denied (..)
    , mkDenied
    , dExplicitDeny
    , dImplicitDeny

    -- ** OTAUpdateFileVersion
    , OTAUpdateFileVersion (..)

    -- ** ResourceArn
    , ResourceArn (..)

    -- ** DimensionName
    , DimensionName (..)

    -- ** DynamoKeyType
    , DynamoKeyType (..)

    -- ** Key
    , Key (..)

    -- ** PolicyVersionId
    , PolicyVersionId (..)

    -- ** DomainName
    , DomainName (..)

    -- ** AuthorizerDescription
    , AuthorizerDescription (..)
    , mkAuthorizerDescription
    , adAuthorizerArn
    , adAuthorizerFunctionArn
    , adAuthorizerName
    , adCreationDate
    , adLastModifiedDate
    , adSigningDisabled
    , adStatus
    , adTokenKeyName
    , adTokenSigningPublicKeys

    -- ** AwsJobAbortCriteriaAbortAction
    , AwsJobAbortCriteriaAbortAction (..)

    -- ** AuditMitigationActionExecutionMetadata
    , AuditMitigationActionExecutionMetadata (..)
    , mkAuditMitigationActionExecutionMetadata
    , amaemActionId
    , amaemActionName
    , amaemEndTime
    , amaemErrorCode
    , amaemFindingId
    , amaemMessage
    , amaemStartTime
    , amaemStatus
    , amaemTaskId

    -- ** TopicRuleDestinationSummary
    , TopicRuleDestinationSummary (..)
    , mkTopicRuleDestinationSummary
    , trdsArn
    , trdsHttpUrlSummary
    , trdsStatus
    , trdsStatusReason

    -- ** MetricValue
    , MetricValue (..)
    , mkMetricValue
    , mvCidrs
    , mvCount
    , mvPorts

    -- ** AwsJobRateIncreaseCriteria
    , AwsJobRateIncreaseCriteria (..)
    , mkAwsJobRateIncreaseCriteria
    , ajricNumberOfNotifiedThings
    , ajricNumberOfSucceededThings

    -- ** CertificateSigningRequest
    , CertificateSigningRequest (..)

    -- ** PolicyTemplateName
    , PolicyTemplateName (..)

    -- ** HttpContext
    , HttpContext (..)
    , mkHttpContext
    , hcHeaders
    , hcQueryString

    -- ** TimestreamAction
    , TimestreamAction (..)
    , mkTimestreamAction
    , taRoleArn
    , taDatabaseName
    , taTableName
    , taDimensions
    , taTimestamp

    -- ** QueryString
    , QueryString (..)

    -- ** CACertificate
    , CACertificate (..)
    , mkCACertificate
    , cacCertificateArn
    , cacCertificateId
    , cacCreationDate
    , cacStatus

    -- ** AlertTargetType
    , AlertTargetType (..)

    -- ** IotSiteWiseAction
    , IotSiteWiseAction (..)
    , mkIotSiteWiseAction
    , iswaPutAssetPropertyValueEntries
    , iswaRoleArn

    -- ** DeliveryStreamName
    , DeliveryStreamName (..)

    -- ** TokenSignature
    , TokenSignature (..)

    -- ** Principal
    , Principal (..)

    -- ** DeviceCertificateUpdateAction
    , DeviceCertificateUpdateAction (..)

    -- ** AttributePayload
    , AttributePayload (..)
    , mkAttributePayload
    , apAttributes
    , apMerge

    -- ** CloudwatchMetricAction
    , CloudwatchMetricAction (..)
    , mkCloudwatchMetricAction
    , cmaRoleArn
    , cmaMetricNamespace
    , cmaMetricName
    , cmaMetricValue
    , cmaMetricUnit
    , cmaMetricTimestamp

    -- ** RangeKeyField
    , RangeKeyField (..)

    -- ** ThingTypeDescription
    , ThingTypeDescription (..)

    -- ** FieldType
    , FieldType (..)

    -- ** AlertTargetArn
    , AlertTargetArn (..)

    -- ** ThingGroupProperties
    , ThingGroupProperties (..)
    , mkThingGroupProperties
    , tgpAttributePayload
    , tgpThingGroupDescription

    -- ** NonCompliantResource
    , NonCompliantResource (..)
    , mkNonCompliantResource
    , ncrAdditionalInfo
    , ncrResourceIdentifier
    , ncrResourceType

    -- ** AuditMitigationActionsTaskStatus
    , AuditMitigationActionsTaskStatus (..)

    -- ** AutoRegistrationStatus
    , AutoRegistrationStatus (..)

    -- ** Marker
    , Marker (..)

    -- ** ServiceName
    , ServiceName (..)

    -- ** ThingIndexingConfiguration
    , ThingIndexingConfiguration (..)
    , mkThingIndexingConfiguration
    , ticThingIndexingMode
    , ticCustomFields
    , ticManagedFields
    , ticThingConnectivityIndexingMode

    -- ** MetricToRetain
    , MetricToRetain (..)
    , mkMetricToRetain
    , mtrMetric
    , mtrMetricDimension

    -- ** SecurityProfileName
    , SecurityProfileName (..)

    -- ** Resource
    , Resource (..)

    -- ** PutItemInput
    , PutItemInput (..)
    , mkPutItemInput
    , piiTableName

    -- ** QueueUrl
    , QueueUrl (..)

    -- ** AwsIotJobArn
    , AwsIotJobArn (..)

    -- ** LoggingOptionsPayload
    , LoggingOptionsPayload (..)
    , mkLoggingOptionsPayload
    , lopRoleArn
    , lopLogLevel

    -- ** DayOfMonth
    , DayOfMonth (..)

    -- ** ErrorCode
    , ErrorCode (..)

    -- ** ServiceType
    , ServiceType (..)

    -- ** Certificate
    , Certificate (..)
    , mkCertificate
    , cCertificateArn
    , cCertificateId
    , cCertificateMode
    , cCreationDate
    , cStatus

    -- ** BillingGroupProperties
    , BillingGroupProperties (..)
    , mkBillingGroupProperties
    , bgpBillingGroupDescription

    -- ** LogTargetConfiguration
    , LogTargetConfiguration (..)
    , mkLogTargetConfiguration
    , ltcLogLevel
    , ltcLogTarget

    -- ** EndpointAddress
    , EndpointAddress (..)

    -- ** JobExecutionSummaryForJob
    , JobExecutionSummaryForJob (..)
    , mkJobExecutionSummaryForJob
    , jesfjJobExecutionSummary
    , jesfjThingArn

    -- ** RateIncreaseCriteria
    , RateIncreaseCriteria (..)
    , mkRateIncreaseCriteria
    , ricNumberOfNotifiedThings
    , ricNumberOfSucceededThings

    -- ** LogTarget
    , LogTarget (..)
    , mkLogTarget
    , ltTargetType
    , ltTargetName

    -- ** ThingGroupDescription
    , ThingGroupDescription (..)

    -- ** OTAUpdateFile
    , OTAUpdateFile (..)
    , mkOTAUpdateFile
    , otaufAttributes
    , otaufCodeSigning
    , otaufFileLocation
    , otaufFileName
    , otaufFileType
    , otaufFileVersion

    -- ** JobDocumentSource
    , JobDocumentSource (..)

    -- ** MitigationAction
    , MitigationAction (..)
    , mkMitigationAction
    , maActionParams
    , maId
    , maName
    , maRoleArn

    -- ** PublishFindingToSnsParams
    , PublishFindingToSnsParams (..)
    , mkPublishFindingToSnsParams
    , pftspTopicArn

    -- ** StepFunctionsAction
    , StepFunctionsAction (..)
    , mkStepFunctionsAction
    , sfaStateMachineName
    , sfaRoleArn
    , sfaExecutionNamePrefix

    -- ** RegistryS3KeyName
    , RegistryS3KeyName (..)

    -- ** RelatedResource
    , RelatedResource (..)
    , mkRelatedResource
    , rrAdditionalInfo
    , rrResourceIdentifier
    , rrResourceType

    -- ** S3FileUrl
    , S3FileUrl (..)

    -- ** TemplateArn
    , TemplateArn (..)

    -- ** CustomCodeSigning
    , CustomCodeSigning (..)
    , mkCustomCodeSigning
    , ccsCertificateChain
    , ccsHashAlgorithm
    , ccsSignature
    , ccsSignatureAlgorithm

    -- ** AssetId
    , AssetId (..)

    -- ** Statistics
    , Statistics (..)
    , mkStatistics
    , sAverage
    , sCount
    , sMaximum
    , sMinimum
    , sStdDeviation
    , sSum
    , sSumOfSquares
    , sVariance

    -- ** ActiveViolation
    , ActiveViolation (..)
    , mkActiveViolation
    , avBehavior
    , avLastViolationTime
    , avLastViolationValue
    , avSecurityProfileName
    , avThingName
    , avViolationId
    , avViolationStartTime

    -- ** ProcessingTargetName
    , ProcessingTargetName (..)

    -- ** OTAUpdateInfo
    , OTAUpdateInfo (..)
    , mkOTAUpdateInfo
    , otauiAdditionalParameters
    , otauiAwsIotJobArn
    , otauiAwsIotJobId
    , otauiAwsJobExecutionsRolloutConfig
    , otauiAwsJobPresignedUrlConfig
    , otauiCreationDate
    , otauiDescription
    , otauiErrorInfo
    , otauiLastModifiedDate
    , otauiOtaUpdateArn
    , otauiOtaUpdateFiles
    , otauiOtaUpdateId
    , otauiOtaUpdateStatus
    , otauiProtocols
    , otauiTargetSelection
    , otauiTargets

    -- ** StartSigningJobParameter
    , StartSigningJobParameter (..)
    , mkStartSigningJobParameter
    , ssjpDestination
    , ssjpSigningProfileName
    , ssjpSigningProfileParameter

    -- ** CloudwatchAlarmAction
    , CloudwatchAlarmAction (..)
    , mkCloudwatchAlarmAction
    , caaRoleArn
    , caaAlarmName
    , caaStateReason
    , caaStateValue

    -- ** PolicyArn
    , PolicyArn (..)

    -- ** ViolationEvent
    , ViolationEvent (..)
    , mkViolationEvent
    , veBehavior
    , veMetricValue
    , veSecurityProfileName
    , veThingName
    , veViolationEventTime
    , veViolationEventType
    , veViolationId

    -- ** ReasonForNonCompliance
    , ReasonForNonCompliance (..)

    -- ** JobExecution
    , JobExecution (..)
    , mkJobExecution
    , jeApproximateSecondsBeforeTimedOut
    , jeExecutionNumber
    , jeForceCanceled
    , jeJobId
    , jeLastUpdatedAt
    , jeQueuedAt
    , jeStartedAt
    , jeStatus
    , jeStatusDetails
    , jeThingArn
    , jeVersionNumber

    -- ** Code
    , Code (..)

    -- ** AuthorizerSummary
    , AuthorizerSummary (..)
    , mkAuthorizerSummary
    , asAuthorizerArn
    , asAuthorizerName

    -- ** TagKey
    , TagKey (..)

    -- ** S3Location
    , S3Location (..)
    , mkS3Location
    , slBucket
    , slKey
    , slVersion

    -- ** DomainConfigurationArn
    , DomainConfigurationArn (..)

    -- ** HashAlgorithm
    , HashAlgorithm (..)

    -- ** DomainType
    , DomainType (..)

    -- ** MqttClientId
    , MqttClientId (..)

    -- ** MessageFormat
    , MessageFormat (..)

    -- ** AwsArn
    , AwsArn (..)

    -- ** HeaderKey
    , HeaderKey (..)

    -- ** FindingId
    , FindingId (..)

    -- ** Configuration
    , Configuration (..)
    , mkConfiguration
    , cEnabled

    -- ** TaskStatisticsForAuditCheck
    , TaskStatisticsForAuditCheck (..)
    , mkTaskStatisticsForAuditCheck
    , tsfacCanceledFindingsCount
    , tsfacFailedFindingsCount
    , tsfacSkippedFindingsCount
    , tsfacSucceededFindingsCount
    , tsfacTotalFindingsCount

    -- ** Policy
    , Policy (..)
    , mkPolicy
    , pPolicyArn
    , pPolicyName

    -- ** AssetPropertyVariant
    , AssetPropertyVariant (..)
    , mkAssetPropertyVariant
    , apvBooleanValue
    , apvDoubleValue
    , apvIntegerValue
    , apvStringValue

    -- ** AuditFinding
    , AuditFinding (..)
    , mkAuditFinding
    , afCheckName
    , afFindingId
    , afFindingTime
    , afIsSuppressed
    , afNonCompliantResource
    , afReasonForNonCompliance
    , afReasonForNonComplianceCode
    , afRelatedResources
    , afSeverity
    , afTaskId
    , afTaskStartTime

    -- ** TemplateBody
    , TemplateBody (..)

    -- ** ExplicitDeny
    , ExplicitDeny (..)
    , mkExplicitDeny
    , edPolicies

    -- ** HttpHeaderName
    , HttpHeaderName (..)

    -- ** RepublishAction
    , RepublishAction (..)
    , mkRepublishAction
    , raRoleArn
    , raTopic
    , raQos

    -- ** AuditNotificationType
    , AuditNotificationType (..)

    -- ** CognitoIdentityPoolId
    , CognitoIdentityPoolId (..)

    -- ** CACertificateUpdateAction
    , CACertificateUpdateAction (..)

    -- ** AuthDecision
    , AuthDecision (..)

    -- ** DimensionType
    , DimensionType (..)

    -- ** Allowed
    , Allowed (..)
    , mkAllowed
    , aPolicies

    -- ** SecurityProfileArn
    , SecurityProfileArn (..)

    -- ** JobStatus
    , JobStatus (..)

    -- ** AuditFindingSeverity
    , AuditFindingSeverity (..)

    -- ** BillingGroupName
    , BillingGroupName (..)

    -- ** RegistrationConfig
    , RegistrationConfig (..)
    , mkRegistrationConfig
    , rcRoleArn
    , rcTemplateBody

    -- ** GenerationId
    , GenerationId (..)

    -- ** JobExecutionStatusDetails
    , JobExecutionStatusDetails (..)
    , mkJobExecutionStatusDetails
    , jesdDetailsMap

    -- ** AttributeKey
    , AttributeKey (..)

    -- ** HttpAuthorization
    , HttpAuthorization (..)
    , mkHttpAuthorization
    , haSigv4

    -- ** SigningRegion
    , SigningRegion (..)

    -- ** RangeKeyValue
    , RangeKeyValue (..)

    -- ** ThingDocument
    , ThingDocument (..)
    , mkThingDocument
    , tdAttributes
    , tdConnectivity
    , tdShadow
    , tdThingGroupNames
    , tdThingId
    , tdThingName
    , tdThingTypeName

    -- ** TransferData
    , TransferData (..)
    , mkTransferData
    , tdAcceptDate
    , tdRejectDate
    , tdRejectReason
    , tdTransferDate
    , tdTransferMessage

    -- ** TokenKeyName
    , TokenKeyName (..)

    -- ** ThingName
    , ThingName (..)

    -- ** ErrorMessage
    , ErrorMessage (..)

    -- ** CACertificateStatus
    , CACertificateStatus (..)

    -- ** StateReason
    , StateReason (..)

    -- ** HttpUrlDestinationSummary
    , HttpUrlDestinationSummary (..)
    , mkHttpUrlDestinationSummary
    , hudsConfirmationUrl

    -- ** Message
    , Message (..)

    -- ** SignatureAlgorithm
    , SignatureAlgorithm (..)

    -- ** OutgoingCertificate
    , OutgoingCertificate (..)
    , mkOutgoingCertificate
    , ocCertificateArn
    , ocCertificateId
    , ocCreationDate
    , ocTransferDate
    , ocTransferMessage
    , ocTransferredTo

    -- ** SecurityProfileDescription
    , SecurityProfileDescription (..)

    -- ** RuleArn
    , RuleArn (..)

    -- ** AttributeName
    , AttributeName (..)

    -- ** MqttContext
    , MqttContext (..)
    , mkMqttContext
    , mcClientId
    , mcPassword
    , mcUsername

    -- ** AwsJobAbortCriteria
    , AwsJobAbortCriteria (..)
    , mkAwsJobAbortCriteria
    , ajacFailureType
    , ajacAction
    , ajacThresholdPercentage
    , ajacMinNumberOfExecutedThings

    -- ** StreamName
    , StreamName (..)

    -- ** ClientRequestToken
    , ClientRequestToken (..)

    -- ** AwsJobTimeoutConfig
    , AwsJobTimeoutConfig (..)
    , mkAwsJobTimeoutConfig
    , ajtcInProgressTimeoutInMinutes

    -- ** Comment
    , Comment (..)

    -- ** HttpAction
    , HttpAction (..)
    , mkHttpAction
    , haUrl
    , haAuth
    , haConfirmationUrl
    , haHeaders

    -- ** ThingGroupIndexingMode
    , ThingGroupIndexingMode (..)

    -- ** Description
    , Description (..)

    -- ** SalesforceEndpoint
    , SalesforceEndpoint (..)

    -- ** JobExecutionSummaryForThing
    , JobExecutionSummaryForThing (..)
    , mkJobExecutionSummaryForThing
    , jesftJobExecutionSummary
    , jesftJobId

    -- ** AuthInfo
    , AuthInfo (..)
    , mkAuthInfo
    , aiResources
    , aiActionType

    -- ** ErrorInfo
    , ErrorInfo (..)
    , mkErrorInfo
    , eiCode
    , eiMessage

    -- ** ReportType
    , ReportType (..)

    -- ** CodeSigning
    , CodeSigning (..)
    , mkCodeSigning
    , csAwsSignerJobId
    , csCustomCodeSigning
    , csStartSigningJobParameter

    -- ** ExponentialRolloutRate
    , ExponentialRolloutRate (..)
    , mkExponentialRolloutRate
    , errBaseRatePerMinute
    , errIncrementFactor
    , errRateIncreaseCriteria

    -- ** GroupNameAndArn
    , GroupNameAndArn (..)
    , mkGroupNameAndArn
    , gnaaGroupArn
    , gnaaGroupName

    -- ** ActionType
    , ActionType (..)

    -- ** SQL
    , SQL (..)

    -- ** S3Action
    , S3Action (..)
    , mkS3Action
    , sRoleArn
    , sBucketName
    , sKey
    , sCannedAcl

    -- ** SecurityProfileIdentifier
    , SecurityProfileIdentifier (..)
    , mkSecurityProfileIdentifier
    , spiName
    , spiArn

    -- ** KinesisAction
    , KinesisAction (..)
    , mkKinesisAction
    , kaRoleArn
    , kaStreamName
    , kaPartitionKey

    -- ** MessageId
    , MessageId (..)

    -- ** TimestreamDimensionName
    , TimestreamDimensionName (..)

    -- ** HashKeyValue
    , HashKeyValue (..)

    -- ** ResourceIdentifier
    , ResourceIdentifier (..)
    , mkResourceIdentifier
    , riAccount
    , riCaCertificateId
    , riClientId
    , riCognitoIdentityPoolId
    , riDeviceCertificateId
    , riIamRoleArn
    , riPolicyVersionIdentifier
    , riRoleAliasArn

    -- ** AbortCriteria
    , AbortCriteria (..)
    , mkAbortCriteria
    , acFailureType
    , acAction
    , acThresholdPercentage
    , acMinNumberOfExecutedThings

    -- ** CertificateStatus
    , CertificateStatus (..)

    -- ** CloudwatchLogsAction
    , CloudwatchLogsAction (..)
    , mkCloudwatchLogsAction
    , claRoleArn
    , claLogGroupName

    -- ** PercentPair
    , PercentPair (..)
    , mkPercentPair
    , ppPercent
    , ppValue

    -- ** TableName
    , TableName (..)

    -- ** DetailsValue
    , DetailsValue (..)

    -- ** AssetPropertyTimestamp
    , AssetPropertyTimestamp (..)
    , mkAssetPropertyTimestamp
    , aptTimeInSeconds
    , aptOffsetInNanos

    -- ** Parameter
    , Parameter (..)

    -- ** AuditNotificationTarget
    , AuditNotificationTarget (..)
    , mkAuditNotificationTarget
    , antEnabled
    , antRoleArn
    , antTargetArn

    -- ** AuditFrequency
    , AuditFrequency (..)

    -- ** TimestreamDimension
    , TimestreamDimension (..)
    , mkTimestreamDimension
    , tdName
    , tdValue

    -- ** StreamId
    , StreamId (..)

    -- ** ThingConnectivity
    , ThingConnectivity (..)
    , mkThingConnectivity
    , tcConnected
    , tcTimestamp

    -- ** FileName
    , FileName (..)

    -- ** SecurityProfileTargetArn
    , SecurityProfileTargetArn (..)

    -- ** ThingTypeArn
    , ThingTypeArn (..)

    -- ** BillingGroupId
    , BillingGroupId (..)

    -- ** DayOfWeek
    , DayOfWeek (..)

    -- ** ThingTypeDefinition
    , ThingTypeDefinition (..)
    , mkThingTypeDefinition
    , ttdThingTypeArn
    , ttdThingTypeMetadata
    , ttdThingTypeName
    , ttdThingTypeProperties

    -- ** JobExecutionStatus
    , JobExecutionStatus (..)

    -- ** TargetSelection
    , TargetSelection (..)

    -- ** ThingId
    , ThingId (..)

    -- ** BillingGroupMetadata
    , BillingGroupMetadata (..)
    , mkBillingGroupMetadata
    , bgmCreationDate

    -- ** Target
    , Target (..)

    -- ** TimeoutConfig
    , TimeoutConfig (..)
    , mkTimeoutConfig
    , tcInProgressTimeoutInMinutes

    -- ** HttpActionHeader
    , HttpActionHeader (..)
    , mkHttpActionHeader
    , hahKey
    , hahValue

    -- ** TopicPattern
    , TopicPattern (..)

    -- ** TopicRulePayload
    , TopicRulePayload (..)
    , mkTopicRulePayload
    , trpSql
    , trpActions
    , trpAwsIotSqlVersion
    , trpDescription
    , trpErrorAction
    , trpRuleDisabled

    -- ** RoleArn
    , RoleArn (..)

    -- ** IndexName
    , IndexName (..)

    -- ** ScheduledAuditArn
    , ScheduledAuditArn (..)

    -- ** OtaUpdateId
    , OtaUpdateId (..)

    -- ** Name
    , Name (..)

    -- ** CheckName
    , CheckName (..)

    -- ** Owner
    , Owner (..)

    -- ** ProvisioningRoleArn
    , ProvisioningRoleArn (..)

    -- ** RoleArnForLogging
    , RoleArnForLogging (..)

    -- ** OtaUpdateArn
    , OtaUpdateArn (..)

    -- ** ConfirmationUrl
    , ConfirmationUrl (..)

    -- ** ActionArn
    , ActionArn (..)

    -- ** ActionId
    , ActionId (..)

    -- ** NextMarker
    , NextMarker (..)

    -- ** TargetAwsAccount
    , TargetAwsAccount (..)

    -- ** TransferMessage
    , TransferMessage (..)

    -- ** VersionId
    , VersionId (..)

    -- ** Arn
    , Arn (..)

    -- ** ChannelArn
    , ChannelArn (..)

    -- ** Document
    , Document (..)

    -- ** DocumentSource
    , DocumentSource (..)

    -- ** Endpoint
    , Endpoint (..)

    -- ** Index
    , Index (..)

    -- ** Type
    , Type (..)

    -- ** Id
    , Id (..)

    -- ** DefaultVersionId
    , DefaultVersionId (..)

    -- ** Operation
    , Operation (..)

    -- ** DefaultAuthorizerName
    , DefaultAuthorizerName (..)

    -- ** OwnedBy
    , OwnedBy (..)

    -- ** PreviousOwnedBy
    , PreviousOwnedBy (..)

    -- ** ParentGroupName
    , ParentGroupName (..)

    -- ** PropertyId
    , PropertyId (..)

    -- ** NamePrefixFilter
    , NamePrefixFilter (..)

    -- ** Bucket
    , Bucket (..)

    -- ** Unit
    , Unit (..)

    -- ** Sql
    , Sql (..)

    -- ** DatabaseName
    , DatabaseName (..)

    -- ** BooleanValue
    , BooleanValue (..)

    -- ** RejectReason
    , RejectReason (..)

    -- ** AwsSignerJobId
    , AwsSignerJobId (..)

    -- ** IamRoleArn
    , IamRoleArn (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Waiters
import Network.AWS.IoT.GetCardinality
import Network.AWS.IoT.CreateDomainConfiguration
import Network.AWS.IoT.DeleteSecurityProfile
import Network.AWS.IoT.UpdateSecurityProfile
import Network.AWS.IoT.ListSecurityProfiles
import Network.AWS.IoT.ListPolicies
import Network.AWS.IoT.DescribeProvisioningTemplate
import Network.AWS.IoT.UpdateMitigationAction
import Network.AWS.IoT.DeleteMitigationAction
import Network.AWS.IoT.DeleteJobExecution
import Network.AWS.IoT.CreatePolicy
import Network.AWS.IoT.RegisterCertificate
import Network.AWS.IoT.DeleteDynamicThingGroup
import Network.AWS.IoT.ListThingPrincipals
import Network.AWS.IoT.UpdateDynamicThingGroup
import Network.AWS.IoT.DescribeRoleAlias
import Network.AWS.IoT.CreateProvisioningTemplateVersion
import Network.AWS.IoT.CreateOTAUpdate
import Network.AWS.IoT.DescribeDefaultAuthorizer
import Network.AWS.IoT.ListAuditMitigationActionsTasks
import Network.AWS.IoT.ListThingRegistrationTaskReports
import Network.AWS.IoT.ListPrincipalThings
import Network.AWS.IoT.RemoveThingFromThingGroup
import Network.AWS.IoT.DescribeEventConfigurations
import Network.AWS.IoT.ListTopicRuleDestinations
import Network.AWS.IoT.RegisterCertificateWithoutCA
import Network.AWS.IoT.ListTagsForResource
import Network.AWS.IoT.ListThingGroups
import Network.AWS.IoT.ListScheduledAudits
import Network.AWS.IoT.DescribeThingRegistrationTask
import Network.AWS.IoT.UpdateScheduledAudit
import Network.AWS.IoT.DeleteScheduledAudit
import Network.AWS.IoT.DescribeAuditFinding
import Network.AWS.IoT.DescribeDimension
import Network.AWS.IoT.GetLoggingOptions
import Network.AWS.IoT.DeleteAccountAuditConfiguration
import Network.AWS.IoT.UpdateAccountAuditConfiguration
import Network.AWS.IoT.GetOTAUpdate
import Network.AWS.IoT.GetEffectivePolicies
import Network.AWS.IoT.ListThingTypes
import Network.AWS.IoT.SetV2LoggingOptions
import Network.AWS.IoT.CreateProvisioningTemplate
import Network.AWS.IoT.ListThingGroupsForThing
import Network.AWS.IoT.CreateCertificateFromCsr
import Network.AWS.IoT.DeleteThing
import Network.AWS.IoT.UpdateThing
import Network.AWS.IoT.DeleteProvisioningTemplate
import Network.AWS.IoT.UpdateProvisioningTemplate
import Network.AWS.IoT.DescribeMitigationAction
import Network.AWS.IoT.StartThingRegistrationTask
import Network.AWS.IoT.CreateScheduledAudit
import Network.AWS.IoT.ListAuthorizers
import Network.AWS.IoT.ListJobExecutionsForJob
import Network.AWS.IoT.RemoveThingFromBillingGroup
import Network.AWS.IoT.SearchIndex
import Network.AWS.IoT.CreateThingType
import Network.AWS.IoT.DescribeSecurityProfile
import Network.AWS.IoT.DeleteV2LoggingLevel
import Network.AWS.IoT.SetDefaultAuthorizer
import Network.AWS.IoT.DescribeJobExecution
import Network.AWS.IoT.CancelCertificateTransfer
import Network.AWS.IoT.GetIndexingConfiguration
import Network.AWS.IoT.ListAuditMitigationActionsExecutions
import Network.AWS.IoT.DescribeAuditMitigationActionsTask
import Network.AWS.IoT.GetStatistics
import Network.AWS.IoT.DeleteRoleAlias
import Network.AWS.IoT.UpdateRoleAlias
import Network.AWS.IoT.DeletePolicyVersion
import Network.AWS.IoT.DisableTopicRule
import Network.AWS.IoT.CreateTopicRule
import Network.AWS.IoT.CreateJob
import Network.AWS.IoT.DescribeIndex
import Network.AWS.IoT.AssociateTargetsWithJob
import Network.AWS.IoT.AttachSecurityProfile
import Network.AWS.IoT.ListAttachedPolicies
import Network.AWS.IoT.CreatePolicyVersion
import Network.AWS.IoT.ListCACertificates
import Network.AWS.IoT.DeleteTopicRule
import Network.AWS.IoT.GetJobDocument
import Network.AWS.IoT.DescribeProvisioningTemplateVersion
import Network.AWS.IoT.CancelAuditTask
import Network.AWS.IoT.CreateRoleAlias
import Network.AWS.IoT.DeleteCACertificate
import Network.AWS.IoT.UpdateCACertificate
import Network.AWS.IoT.ListTopicRules
import Network.AWS.IoT.TransferCertificate
import Network.AWS.IoT.ListJobs
import Network.AWS.IoT.ListRoleAliases
import Network.AWS.IoT.StartOnDemandAuditTask
import Network.AWS.IoT.DescribeThingGroup
import Network.AWS.IoT.DeleteJob
import Network.AWS.IoT.ListTargetsForSecurityProfile
import Network.AWS.IoT.UpdateJob
import Network.AWS.IoT.StartAuditMitigationActionsTask
import Network.AWS.IoT.GetTopicRule
import Network.AWS.IoT.DescribeThing
import Network.AWS.IoT.ListDomainConfigurations
import Network.AWS.IoT.ListAuditTasks
import Network.AWS.IoT.DescribeAccountAuditConfiguration
import Network.AWS.IoT.DeleteDimension
import Network.AWS.IoT.UpdateDimension
import Network.AWS.IoT.DeletePolicy
import Network.AWS.IoT.ListThingsInThingGroup
import Network.AWS.IoT.ListAuditFindings
import Network.AWS.IoT.DescribeScheduledAudit
import Network.AWS.IoT.CreateMitigationAction
import Network.AWS.IoT.ConfirmTopicRuleDestination
import Network.AWS.IoT.ListCertificates
import Network.AWS.IoT.ListMitigationActions
import Network.AWS.IoT.DescribeAuthorizer
import Network.AWS.IoT.GetPolicyVersion
import Network.AWS.IoT.ListActiveViolations
import Network.AWS.IoT.ValidateSecurityProfileBehaviors
import Network.AWS.IoT.ListViolationEvents
import Network.AWS.IoT.DeleteCertificate
import Network.AWS.IoT.UpdateCertificate
import Network.AWS.IoT.CreateDimension
import Network.AWS.IoT.UpdateIndexingConfiguration
import Network.AWS.IoT.CreateProvisioningClaim
import Network.AWS.IoT.TestInvokeAuthorizer
import Network.AWS.IoT.CreateThingGroup
import Network.AWS.IoT.CreateTopicRuleDestination
import Network.AWS.IoT.DetachPolicy
import Network.AWS.IoT.DescribeJob
import Network.AWS.IoT.AddThingToBillingGroup
import Network.AWS.IoT.UpdateTopicRuleDestination
import Network.AWS.IoT.DeleteTopicRuleDestination
import Network.AWS.IoT.DeleteThingGroup
import Network.AWS.IoT.UpdateThingGroup
import Network.AWS.IoT.ListOTAUpdates
import Network.AWS.IoT.DeleteOTAUpdate
import Network.AWS.IoT.CreateDynamicThingGroup
import Network.AWS.IoT.DetachSecurityProfile
import Network.AWS.IoT.ListOutgoingCertificates
import Network.AWS.IoT.DeleteProvisioningTemplateVersion
import Network.AWS.IoT.DescribeCACertificate
import Network.AWS.IoT.ListProvisioningTemplateVersions
import Network.AWS.IoT.GetRegistrationCode
import Network.AWS.IoT.ListBillingGroups
import Network.AWS.IoT.DeleteThingType
import Network.AWS.IoT.DeleteBillingGroup
import Network.AWS.IoT.AddThingToThingGroup
import Network.AWS.IoT.UpdateBillingGroup
import Network.AWS.IoT.GetTopicRuleDestination
import Network.AWS.IoT.ListCertificatesByCA
import Network.AWS.IoT.UpdateAuditSuppression
import Network.AWS.IoT.AttachThingPrincipal
import Network.AWS.IoT.ListThings
import Network.AWS.IoT.DeleteAuditSuppression
import Network.AWS.IoT.RegisterThing
import Network.AWS.IoT.ListAuditSuppressions
import Network.AWS.IoT.DescribeDomainConfiguration
import Network.AWS.IoT.DescribeAuditTask
import Network.AWS.IoT.DeleteRegistrationCode
import Network.AWS.IoT.UpdateStream
import Network.AWS.IoT.DeleteStream
import Network.AWS.IoT.ListStreams
import Network.AWS.IoT.CreateAuthorizer
import Network.AWS.IoT.TestAuthorization
import Network.AWS.IoT.ListIndices
import Network.AWS.IoT.UpdateAuthorizer
import Network.AWS.IoT.DeleteAuthorizer
import Network.AWS.IoT.CreateThing
import Network.AWS.IoT.CreateStream
import Network.AWS.IoT.CancelAuditMitigationActionsTask
import Network.AWS.IoT.CreateAuditSuppression
import Network.AWS.IoT.CreateBillingGroup
import Network.AWS.IoT.ListProvisioningTemplates
import Network.AWS.IoT.ListV2LoggingLevels
import Network.AWS.IoT.TagResource
import Network.AWS.IoT.StopThingRegistrationTask
import Network.AWS.IoT.DescribeCertificate
import Network.AWS.IoT.ListTargetsForPolicy
import Network.AWS.IoT.ClearDefaultAuthorizer
import Network.AWS.IoT.ReplaceTopicRule
import Network.AWS.IoT.UntagResource
import Network.AWS.IoT.SetDefaultPolicyVersion
import Network.AWS.IoT.CancelJobExecution
import Network.AWS.IoT.ListPolicyVersions
import Network.AWS.IoT.SetV2LoggingLevel
import Network.AWS.IoT.ListJobExecutionsForThing
import Network.AWS.IoT.AttachPolicy
import Network.AWS.IoT.CreateKeysAndCertificate
import Network.AWS.IoT.ListThingsInBillingGroup
import Network.AWS.IoT.UpdateThingGroupsForThing
import Network.AWS.IoT.EnableTopicRule
import Network.AWS.IoT.AcceptCertificateTransfer
import Network.AWS.IoT.GetPercentiles
import Network.AWS.IoT.GetPolicy
import Network.AWS.IoT.DescribeEndpoint
import Network.AWS.IoT.ListSecurityProfilesForTarget
import Network.AWS.IoT.UpdateEventConfigurations
import Network.AWS.IoT.RegisterCACertificate
import Network.AWS.IoT.DeleteDomainConfiguration
import Network.AWS.IoT.UpdateDomainConfiguration
import Network.AWS.IoT.SetLoggingOptions
import Network.AWS.IoT.DescribeThingType
import Network.AWS.IoT.ListDimensions
import Network.AWS.IoT.GetV2LoggingOptions
import Network.AWS.IoT.ListThingRegistrationTasks
import Network.AWS.IoT.RejectCertificateTransfer
import Network.AWS.IoT.DescribeAuditSuppression
import Network.AWS.IoT.DescribeStream
import Network.AWS.IoT.CreateSecurityProfile
import Network.AWS.IoT.DescribeBillingGroup
import Network.AWS.IoT.DetachThingPrincipal
import Network.AWS.IoT.CancelJob
import Network.AWS.IoT.DeprecateThingType
import qualified Network.AWS.Prelude as Lude

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'IoT'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}

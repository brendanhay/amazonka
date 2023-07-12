{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Inspector.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _AgentsAlreadyRunningAssessmentException,
    _AssessmentRunInProgressException,
    _InternalException,
    _InvalidCrossAccountRoleException,
    _InvalidInputException,
    _LimitExceededException,
    _NoSuchEntityException,
    _PreviewGenerationInProgressException,
    _ServiceTemporarilyUnavailableException,
    _UnsupportedFeatureException,

    -- * AgentHealth
    AgentHealth (..),

    -- * AgentHealthCode
    AgentHealthCode (..),

    -- * AssessmentRunNotificationSnsStatusCode
    AssessmentRunNotificationSnsStatusCode (..),

    -- * AssessmentRunState
    AssessmentRunState (..),

    -- * AssetType
    AssetType (..),

    -- * FailedItemErrorCode
    FailedItemErrorCode (..),

    -- * InspectorEvent
    InspectorEvent (..),

    -- * Locale
    Locale (..),

    -- * PreviewStatus
    PreviewStatus (..),

    -- * ReportFileFormat
    ReportFileFormat (..),

    -- * ReportStatus
    ReportStatus (..),

    -- * ReportType
    ReportType (..),

    -- * ScopeType
    ScopeType (..),

    -- * Severity
    Severity (..),

    -- * StopAction
    StopAction (..),

    -- * AgentFilter
    AgentFilter (..),
    newAgentFilter,
    agentFilter_agentHealths,
    agentFilter_agentHealthCodes,

    -- * AgentPreview
    AgentPreview (..),
    newAgentPreview,
    agentPreview_agentHealth,
    agentPreview_agentVersion,
    agentPreview_autoScalingGroup,
    agentPreview_hostname,
    agentPreview_ipv4Address,
    agentPreview_kernelVersion,
    agentPreview_operatingSystem,
    agentPreview_agentId,

    -- * AssessmentRun
    AssessmentRun (..),
    newAssessmentRun,
    assessmentRun_completedAt,
    assessmentRun_startedAt,
    assessmentRun_arn,
    assessmentRun_name,
    assessmentRun_assessmentTemplateArn,
    assessmentRun_state,
    assessmentRun_durationInSeconds,
    assessmentRun_rulesPackageArns,
    assessmentRun_userAttributesForFindings,
    assessmentRun_createdAt,
    assessmentRun_stateChangedAt,
    assessmentRun_dataCollected,
    assessmentRun_stateChanges,
    assessmentRun_notifications,
    assessmentRun_findingCounts,

    -- * AssessmentRunAgent
    AssessmentRunAgent (..),
    newAssessmentRunAgent,
    assessmentRunAgent_agentHealthDetails,
    assessmentRunAgent_autoScalingGroup,
    assessmentRunAgent_agentId,
    assessmentRunAgent_assessmentRunArn,
    assessmentRunAgent_agentHealth,
    assessmentRunAgent_agentHealthCode,
    assessmentRunAgent_telemetryMetadata,

    -- * AssessmentRunFilter
    AssessmentRunFilter (..),
    newAssessmentRunFilter,
    assessmentRunFilter_completionTimeRange,
    assessmentRunFilter_durationRange,
    assessmentRunFilter_namePattern,
    assessmentRunFilter_rulesPackageArns,
    assessmentRunFilter_startTimeRange,
    assessmentRunFilter_stateChangeTimeRange,
    assessmentRunFilter_states,

    -- * AssessmentRunNotification
    AssessmentRunNotification (..),
    newAssessmentRunNotification,
    assessmentRunNotification_message,
    assessmentRunNotification_snsPublishStatusCode,
    assessmentRunNotification_snsTopicArn,
    assessmentRunNotification_date,
    assessmentRunNotification_event,
    assessmentRunNotification_error,

    -- * AssessmentRunStateChange
    AssessmentRunStateChange (..),
    newAssessmentRunStateChange,
    assessmentRunStateChange_stateChangedAt,
    assessmentRunStateChange_state,

    -- * AssessmentTarget
    AssessmentTarget (..),
    newAssessmentTarget,
    assessmentTarget_resourceGroupArn,
    assessmentTarget_arn,
    assessmentTarget_name,
    assessmentTarget_createdAt,
    assessmentTarget_updatedAt,

    -- * AssessmentTargetFilter
    AssessmentTargetFilter (..),
    newAssessmentTargetFilter,
    assessmentTargetFilter_assessmentTargetNamePattern,

    -- * AssessmentTemplate
    AssessmentTemplate (..),
    newAssessmentTemplate,
    assessmentTemplate_lastAssessmentRunArn,
    assessmentTemplate_arn,
    assessmentTemplate_name,
    assessmentTemplate_assessmentTargetArn,
    assessmentTemplate_durationInSeconds,
    assessmentTemplate_rulesPackageArns,
    assessmentTemplate_userAttributesForFindings,
    assessmentTemplate_assessmentRunCount,
    assessmentTemplate_createdAt,

    -- * AssessmentTemplateFilter
    AssessmentTemplateFilter (..),
    newAssessmentTemplateFilter,
    assessmentTemplateFilter_durationRange,
    assessmentTemplateFilter_namePattern,
    assessmentTemplateFilter_rulesPackageArns,

    -- * AssetAttributes
    AssetAttributes (..),
    newAssetAttributes,
    assetAttributes_agentId,
    assetAttributes_amiId,
    assetAttributes_autoScalingGroup,
    assetAttributes_hostname,
    assetAttributes_ipv4Addresses,
    assetAttributes_networkInterfaces,
    assetAttributes_tags,
    assetAttributes_schemaVersion,

    -- * Attribute
    Attribute (..),
    newAttribute,
    attribute_value,
    attribute_key,

    -- * DurationRange
    DurationRange (..),
    newDurationRange,
    durationRange_maxSeconds,
    durationRange_minSeconds,

    -- * EventSubscription
    EventSubscription (..),
    newEventSubscription,
    eventSubscription_event,
    eventSubscription_subscribedAt,

    -- * Exclusion
    Exclusion (..),
    newExclusion,
    exclusion_attributes,
    exclusion_arn,
    exclusion_title,
    exclusion_description,
    exclusion_recommendation,
    exclusion_scopes,

    -- * ExclusionPreview
    ExclusionPreview (..),
    newExclusionPreview,
    exclusionPreview_attributes,
    exclusionPreview_title,
    exclusionPreview_description,
    exclusionPreview_recommendation,
    exclusionPreview_scopes,

    -- * FailedItemDetails
    FailedItemDetails (..),
    newFailedItemDetails,
    failedItemDetails_failureCode,
    failedItemDetails_retryable,

    -- * Finding
    Finding (..),
    newFinding,
    finding_assetAttributes,
    finding_assetType,
    finding_confidence,
    finding_description,
    finding_id,
    finding_indicatorOfCompromise,
    finding_numericSeverity,
    finding_recommendation,
    finding_schemaVersion,
    finding_service,
    finding_serviceAttributes,
    finding_severity,
    finding_title,
    finding_arn,
    finding_attributes,
    finding_userAttributes,
    finding_createdAt,
    finding_updatedAt,

    -- * FindingFilter
    FindingFilter (..),
    newFindingFilter,
    findingFilter_agentIds,
    findingFilter_attributes,
    findingFilter_autoScalingGroups,
    findingFilter_creationTimeRange,
    findingFilter_ruleNames,
    findingFilter_rulesPackageArns,
    findingFilter_severities,
    findingFilter_userAttributes,

    -- * InspectorServiceAttributes
    InspectorServiceAttributes (..),
    newInspectorServiceAttributes,
    inspectorServiceAttributes_assessmentRunArn,
    inspectorServiceAttributes_rulesPackageArn,
    inspectorServiceAttributes_schemaVersion,

    -- * NetworkInterface
    NetworkInterface (..),
    newNetworkInterface,
    networkInterface_ipv6Addresses,
    networkInterface_networkInterfaceId,
    networkInterface_privateDnsName,
    networkInterface_privateIpAddress,
    networkInterface_privateIpAddresses,
    networkInterface_publicDnsName,
    networkInterface_publicIp,
    networkInterface_securityGroups,
    networkInterface_subnetId,
    networkInterface_vpcId,

    -- * PrivateIp
    PrivateIp (..),
    newPrivateIp,
    privateIp_privateDnsName,
    privateIp_privateIpAddress,

    -- * ResourceGroup
    ResourceGroup (..),
    newResourceGroup,
    resourceGroup_arn,
    resourceGroup_tags,
    resourceGroup_createdAt,

    -- * ResourceGroupTag
    ResourceGroupTag (..),
    newResourceGroupTag,
    resourceGroupTag_value,
    resourceGroupTag_key,

    -- * RulesPackage
    RulesPackage (..),
    newRulesPackage,
    rulesPackage_description,
    rulesPackage_arn,
    rulesPackage_name,
    rulesPackage_version,
    rulesPackage_provider,

    -- * Scope
    Scope (..),
    newScope,
    scope_key,
    scope_value,

    -- * SecurityGroup
    SecurityGroup (..),
    newSecurityGroup,
    securityGroup_groupId,
    securityGroup_groupName,

    -- * Subscription
    Subscription (..),
    newSubscription,
    subscription_resourceArn,
    subscription_topicArn,
    subscription_eventSubscriptions,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,

    -- * TelemetryMetadata
    TelemetryMetadata (..),
    newTelemetryMetadata,
    telemetryMetadata_dataSize,
    telemetryMetadata_messageType,
    telemetryMetadata_count,

    -- * TimestampRange
    TimestampRange (..),
    newTimestampRange,
    timestampRange_beginDate,
    timestampRange_endDate,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Inspector.Types.AgentFilter
import Amazonka.Inspector.Types.AgentHealth
import Amazonka.Inspector.Types.AgentHealthCode
import Amazonka.Inspector.Types.AgentPreview
import Amazonka.Inspector.Types.AssessmentRun
import Amazonka.Inspector.Types.AssessmentRunAgent
import Amazonka.Inspector.Types.AssessmentRunFilter
import Amazonka.Inspector.Types.AssessmentRunNotification
import Amazonka.Inspector.Types.AssessmentRunNotificationSnsStatusCode
import Amazonka.Inspector.Types.AssessmentRunState
import Amazonka.Inspector.Types.AssessmentRunStateChange
import Amazonka.Inspector.Types.AssessmentTarget
import Amazonka.Inspector.Types.AssessmentTargetFilter
import Amazonka.Inspector.Types.AssessmentTemplate
import Amazonka.Inspector.Types.AssessmentTemplateFilter
import Amazonka.Inspector.Types.AssetAttributes
import Amazonka.Inspector.Types.AssetType
import Amazonka.Inspector.Types.Attribute
import Amazonka.Inspector.Types.DurationRange
import Amazonka.Inspector.Types.EventSubscription
import Amazonka.Inspector.Types.Exclusion
import Amazonka.Inspector.Types.ExclusionPreview
import Amazonka.Inspector.Types.FailedItemDetails
import Amazonka.Inspector.Types.FailedItemErrorCode
import Amazonka.Inspector.Types.Finding
import Amazonka.Inspector.Types.FindingFilter
import Amazonka.Inspector.Types.InspectorEvent
import Amazonka.Inspector.Types.InspectorServiceAttributes
import Amazonka.Inspector.Types.Locale
import Amazonka.Inspector.Types.NetworkInterface
import Amazonka.Inspector.Types.PreviewStatus
import Amazonka.Inspector.Types.PrivateIp
import Amazonka.Inspector.Types.ReportFileFormat
import Amazonka.Inspector.Types.ReportStatus
import Amazonka.Inspector.Types.ReportType
import Amazonka.Inspector.Types.ResourceGroup
import Amazonka.Inspector.Types.ResourceGroupTag
import Amazonka.Inspector.Types.RulesPackage
import Amazonka.Inspector.Types.Scope
import Amazonka.Inspector.Types.ScopeType
import Amazonka.Inspector.Types.SecurityGroup
import Amazonka.Inspector.Types.Severity
import Amazonka.Inspector.Types.StopAction
import Amazonka.Inspector.Types.Subscription
import Amazonka.Inspector.Types.Tag
import Amazonka.Inspector.Types.TelemetryMetadata
import Amazonka.Inspector.Types.TimestampRange
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2016-02-16@ of the Amazon Inspector SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Inspector",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "inspector",
      Core.signingName = "inspector",
      Core.version = "2016-02-16",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Inspector",
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

-- | You do not have required permissions to access the requested resource.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | You started an assessment run, but one of the instances is already
-- participating in another assessment run.
_AgentsAlreadyRunningAssessmentException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AgentsAlreadyRunningAssessmentException =
  Core._MatchServiceError
    defaultService
    "AgentsAlreadyRunningAssessmentException"

-- | You cannot perform a specified action if an assessment run is currently
-- in progress.
_AssessmentRunInProgressException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AssessmentRunInProgressException =
  Core._MatchServiceError
    defaultService
    "AssessmentRunInProgressException"

-- | Internal server error.
_InternalException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalException =
  Core._MatchServiceError
    defaultService
    "InternalException"

-- | Amazon Inspector cannot assume the cross-account role that it needs to
-- list your EC2 instances during the assessment run.
_InvalidCrossAccountRoleException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidCrossAccountRoleException =
  Core._MatchServiceError
    defaultService
    "InvalidCrossAccountRoleException"

-- | The request was rejected because an invalid or out-of-range value was
-- supplied for an input parameter.
_InvalidInputException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidInputException =
  Core._MatchServiceError
    defaultService
    "InvalidInputException"

-- | The request was rejected because it attempted to create resources beyond
-- the current AWS account limits. The error code describes the limit
-- exceeded.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The request was rejected because it referenced an entity that does not
-- exist. The error code describes the entity.
_NoSuchEntityException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoSuchEntityException =
  Core._MatchServiceError
    defaultService
    "NoSuchEntityException"

-- | The request is rejected. The specified assessment template is currently
-- generating an exclusions preview.
_PreviewGenerationInProgressException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PreviewGenerationInProgressException =
  Core._MatchServiceError
    defaultService
    "PreviewGenerationInProgressException"

-- | The serice is temporary unavailable.
_ServiceTemporarilyUnavailableException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceTemporarilyUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceTemporarilyUnavailableException"

-- | Used by the GetAssessmentReport API. The request was rejected because
-- you tried to generate a report for an assessment run that existed before
-- reporting was supported in Amazon Inspector. You can only generate
-- reports for assessment runs that took place or will take place after
-- generating reports in Amazon Inspector became available.
_UnsupportedFeatureException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnsupportedFeatureException =
  Core._MatchServiceError
    defaultService
    "UnsupportedFeatureException"

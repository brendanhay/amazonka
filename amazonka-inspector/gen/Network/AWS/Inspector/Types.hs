{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _PreviewGenerationInProgressException,
    _ServiceTemporarilyUnavailableException,
    _UnsupportedFeatureException,
    _InternalException,
    _InvalidInputException,
    _InvalidCrossAccountRoleException,
    _AssessmentRunInProgressException,
    _AgentsAlreadyRunningAssessmentException,
    _AccessDeniedException,
    _LimitExceededException,
    _NoSuchEntityException,

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
    agentPreview_hostname,
    agentPreview_agentVersion,
    agentPreview_kernelVersion,
    agentPreview_operatingSystem,
    agentPreview_agentHealth,
    agentPreview_autoScalingGroup,
    agentPreview_ipv4Address,
    agentPreview_agentId,

    -- * AssessmentRun
    AssessmentRun (..),
    newAssessmentRun,
    assessmentRun_startedAt,
    assessmentRun_completedAt,
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
    assessmentRunFilter_states,
    assessmentRunFilter_rulesPackageArns,
    assessmentRunFilter_durationRange,
    assessmentRunFilter_stateChangeTimeRange,
    assessmentRunFilter_startTimeRange,
    assessmentRunFilter_namePattern,
    assessmentRunFilter_completionTimeRange,

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
    assessmentTemplateFilter_rulesPackageArns,
    assessmentTemplateFilter_durationRange,
    assessmentTemplateFilter_namePattern,

    -- * AssetAttributes
    AssetAttributes (..),
    newAssetAttributes,
    assetAttributes_hostname,
    assetAttributes_agentId,
    assetAttributes_amiId,
    assetAttributes_tags,
    assetAttributes_ipv4Addresses,
    assetAttributes_networkInterfaces,
    assetAttributes_autoScalingGroup,
    assetAttributes_schemaVersion,

    -- * Attribute
    Attribute (..),
    newAttribute,
    attribute_value,
    attribute_key,

    -- * DurationRange
    DurationRange (..),
    newDurationRange,
    durationRange_minSeconds,
    durationRange_maxSeconds,

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
    finding_severity,
    finding_title,
    finding_assetType,
    finding_numericSeverity,
    finding_id,
    finding_service,
    finding_serviceAttributes,
    finding_confidence,
    finding_recommendation,
    finding_indicatorOfCompromise,
    finding_description,
    finding_schemaVersion,
    finding_arn,
    finding_attributes,
    finding_userAttributes,
    finding_createdAt,
    finding_updatedAt,

    -- * FindingFilter
    FindingFilter (..),
    newFindingFilter,
    findingFilter_agentIds,
    findingFilter_rulesPackageArns,
    findingFilter_creationTimeRange,
    findingFilter_severities,
    findingFilter_attributes,
    findingFilter_userAttributes,
    findingFilter_autoScalingGroups,
    findingFilter_ruleNames,

    -- * InspectorServiceAttributes
    InspectorServiceAttributes (..),
    newInspectorServiceAttributes,
    inspectorServiceAttributes_rulesPackageArn,
    inspectorServiceAttributes_assessmentRunArn,
    inspectorServiceAttributes_schemaVersion,

    -- * NetworkInterface
    NetworkInterface (..),
    newNetworkInterface,
    networkInterface_privateIpAddresses,
    networkInterface_ipv6Addresses,
    networkInterface_securityGroups,
    networkInterface_publicDnsName,
    networkInterface_subnetId,
    networkInterface_networkInterfaceId,
    networkInterface_privateDnsName,
    networkInterface_vpcId,
    networkInterface_publicIp,
    networkInterface_privateIpAddress,

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
    securityGroup_groupName,
    securityGroup_groupId,

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

import Network.AWS.Inspector.Types.AgentFilter
import Network.AWS.Inspector.Types.AgentHealth
import Network.AWS.Inspector.Types.AgentHealthCode
import Network.AWS.Inspector.Types.AgentPreview
import Network.AWS.Inspector.Types.AssessmentRun
import Network.AWS.Inspector.Types.AssessmentRunAgent
import Network.AWS.Inspector.Types.AssessmentRunFilter
import Network.AWS.Inspector.Types.AssessmentRunNotification
import Network.AWS.Inspector.Types.AssessmentRunNotificationSnsStatusCode
import Network.AWS.Inspector.Types.AssessmentRunState
import Network.AWS.Inspector.Types.AssessmentRunStateChange
import Network.AWS.Inspector.Types.AssessmentTarget
import Network.AWS.Inspector.Types.AssessmentTargetFilter
import Network.AWS.Inspector.Types.AssessmentTemplate
import Network.AWS.Inspector.Types.AssessmentTemplateFilter
import Network.AWS.Inspector.Types.AssetAttributes
import Network.AWS.Inspector.Types.AssetType
import Network.AWS.Inspector.Types.Attribute
import Network.AWS.Inspector.Types.DurationRange
import Network.AWS.Inspector.Types.EventSubscription
import Network.AWS.Inspector.Types.Exclusion
import Network.AWS.Inspector.Types.ExclusionPreview
import Network.AWS.Inspector.Types.FailedItemDetails
import Network.AWS.Inspector.Types.FailedItemErrorCode
import Network.AWS.Inspector.Types.Finding
import Network.AWS.Inspector.Types.FindingFilter
import Network.AWS.Inspector.Types.InspectorEvent
import Network.AWS.Inspector.Types.InspectorServiceAttributes
import Network.AWS.Inspector.Types.Locale
import Network.AWS.Inspector.Types.NetworkInterface
import Network.AWS.Inspector.Types.PreviewStatus
import Network.AWS.Inspector.Types.PrivateIp
import Network.AWS.Inspector.Types.ReportFileFormat
import Network.AWS.Inspector.Types.ReportStatus
import Network.AWS.Inspector.Types.ReportType
import Network.AWS.Inspector.Types.ResourceGroup
import Network.AWS.Inspector.Types.ResourceGroupTag
import Network.AWS.Inspector.Types.RulesPackage
import Network.AWS.Inspector.Types.Scope
import Network.AWS.Inspector.Types.ScopeType
import Network.AWS.Inspector.Types.SecurityGroup
import Network.AWS.Inspector.Types.Severity
import Network.AWS.Inspector.Types.StopAction
import Network.AWS.Inspector.Types.Subscription
import Network.AWS.Inspector.Types.Tag
import Network.AWS.Inspector.Types.TelemetryMetadata
import Network.AWS.Inspector.Types.TimestampRange
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-02-16@ of the Amazon Inspector SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "Inspector",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcEndpointPrefix = "inspector",
      Prelude._svcSigningName = "inspector",
      Prelude._svcVersion = "2016-02-16",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "Inspector",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The request is rejected. The specified assessment template is currently
-- generating an exclusions preview.
_PreviewGenerationInProgressException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_PreviewGenerationInProgressException =
  Prelude._MatchServiceError
    defaultService
    "PreviewGenerationInProgressException"

-- | The serice is temporary unavailable.
_ServiceTemporarilyUnavailableException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ServiceTemporarilyUnavailableException =
  Prelude._MatchServiceError
    defaultService
    "ServiceTemporarilyUnavailableException"

-- | Used by the GetAssessmentReport API. The request was rejected because
-- you tried to generate a report for an assessment run that existed before
-- reporting was supported in Amazon Inspector. You can only generate
-- reports for assessment runs that took place or will take place after
-- generating reports in Amazon Inspector became available.
_UnsupportedFeatureException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UnsupportedFeatureException =
  Prelude._MatchServiceError
    defaultService
    "UnsupportedFeatureException"

-- | Internal server error.
_InternalException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InternalException =
  Prelude._MatchServiceError
    defaultService
    "InternalException"

-- | The request was rejected because an invalid or out-of-range value was
-- supplied for an input parameter.
_InvalidInputException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidInputException =
  Prelude._MatchServiceError
    defaultService
    "InvalidInputException"

-- | Amazon Inspector cannot assume the cross-account role that it needs to
-- list your EC2 instances during the assessment run.
_InvalidCrossAccountRoleException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidCrossAccountRoleException =
  Prelude._MatchServiceError
    defaultService
    "InvalidCrossAccountRoleException"

-- | You cannot perform a specified action if an assessment run is currently
-- in progress.
_AssessmentRunInProgressException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AssessmentRunInProgressException =
  Prelude._MatchServiceError
    defaultService
    "AssessmentRunInProgressException"

-- | You started an assessment run, but one of the instances is already
-- participating in another assessment run.
_AgentsAlreadyRunningAssessmentException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AgentsAlreadyRunningAssessmentException =
  Prelude._MatchServiceError
    defaultService
    "AgentsAlreadyRunningAssessmentException"

-- | You do not have required permissions to access the requested resource.
_AccessDeniedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AccessDeniedException =
  Prelude._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | The request was rejected because it attempted to create resources beyond
-- the current AWS account limits. The error code describes the limit
-- exceeded.
_LimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The request was rejected because it referenced an entity that does not
-- exist. The error code describes the entity.
_NoSuchEntityException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NoSuchEntityException =
  Prelude._MatchServiceError
    defaultService
    "NoSuchEntityException"

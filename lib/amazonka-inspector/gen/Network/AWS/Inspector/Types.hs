-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _AccessDeniedException,
    _AssessmentRunInProgressException,
    _NoSuchEntityException,
    _UnsupportedFeatureException,
    _PreviewGenerationInProgressException,
    _AgentsAlreadyRunningAssessmentException,
    _InvalidCrossAccountRoleException,
    _InvalidInputException,
    _InternalException,
    _ServiceTemporarilyUnavailableException,
    _LimitExceededException,

    -- * Exclusion
    Exclusion (..),
    mkExclusion,
    eArn,
    eTitle,
    eDescription,
    eRecommendation,
    eScopes,
    eAttributes,

    -- * Attribute
    Attribute (..),
    mkAttribute,
    aKey,
    aValue,

    -- * PrivateIp
    PrivateIp (..),
    mkPrivateIp,
    piPrivateDnsName,
    piPrivateIpAddress,

    -- * PaginationToken
    PaginationToken (..),

    -- * NamePattern
    NamePattern (..),

    -- * Hostname
    Hostname (..),

    -- * ResourceGroupTag
    ResourceGroupTag (..),
    mkResourceGroupTag,
    rgtKey,
    rgtValue,

    -- * AssessmentTargetFilter
    AssessmentTargetFilter (..),
    mkAssessmentTargetFilter,
    atfAssessmentTargetNamePattern,

    -- * AssessmentRun
    AssessmentRun (..),
    mkAssessmentRun,
    arArn,
    arName,
    arAssessmentTemplateArn,
    arState,
    arDurationInSeconds,
    arRulesPackageArns,
    arUserAttributesForFindings,
    arCreatedAt,
    arStateChangedAt,
    arDataCollected,
    arStateChanges,
    arNotifications,
    arFindingCounts,
    arCompletedAt,
    arStartedAt,

    -- * RulesPackage
    RulesPackage (..),
    mkRulesPackage,
    rpArn,
    rpName,
    rpVersion,
    rpProvider,
    rpDescription,

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * ScopeType
    ScopeType (..),

    -- * AssessmentRunNotification
    AssessmentRunNotification (..),
    mkAssessmentRunNotification,
    arnDate,
    arnEvent,
    arnError,
    arnMessage,
    arnSnsPublishStatusCode,
    arnSnsTopicArn,

    -- * AutoScalingGroup
    AutoScalingGroup (..),

    -- * Arn
    Arn (..),

    -- * Text
    Text (..),

    -- * Finding
    Finding (..),
    mkFinding,
    fArn,
    fAttributes,
    fUserAttributes,
    fCreatedAt,
    fUpdatedAt,
    fAssetAttributes,
    fAssetType,
    fConfidence,
    fDescription,
    fId,
    fIndicatorOfCompromise,
    fNumericSeverity,
    fRecommendation,
    fSchemaVersion,
    fService,
    fServiceAttributes,
    fSeverity,
    fTitle,

    -- * AssessmentRunNotificationSnsStatusCode
    AssessmentRunNotificationSnsStatusCode (..),

    -- * AssessmentTargetName
    AssessmentTargetName (..),

    -- * OperatingSystem
    OperatingSystem (..),

    -- * Locale
    Locale (..),

    -- * AgentHealthCode
    AgentHealthCode (..),

    -- * AgentPreview
    AgentPreview (..),
    mkAgentPreview,
    apAgentId,
    apAgentHealth,
    apAgentVersion,
    apAutoScalingGroup,
    apHostname,
    apIpv4Address,
    apKernelVersion,
    apOperatingSystem,

    -- * Severity
    Severity (..),

    -- * NetworkInterface
    NetworkInterface (..),
    mkNetworkInterface,
    niIpv6Addresses,
    niNetworkInterfaceId,
    niPrivateDnsName,
    niPrivateIpAddress,
    niPrivateIpAddresses,
    niPublicDnsName,
    niPublicIp,
    niSecurityGroups,
    niSubnetId,
    niVpcId,

    -- * AgentVersion
    AgentVersion (..),

    -- * Url
    Url (..),

    -- * MessageType
    MessageType (..),

    -- * AssessmentTemplate
    AssessmentTemplate (..),
    mkAssessmentTemplate,
    atArn,
    atName,
    atAssessmentTargetArn,
    atDurationInSeconds,
    atRulesPackageArns,
    atUserAttributesForFindings,
    atAssessmentRunCount,
    atCreatedAt,
    atLastAssessmentRunArn,

    -- * ExclusionPreview
    ExclusionPreview (..),
    mkExclusionPreview,
    epTitle,
    epDescription,
    epRecommendation,
    epScopes,
    epAttributes,

    -- * AssessmentRunAgent
    AssessmentRunAgent (..),
    mkAssessmentRunAgent,
    araAgentId,
    araAssessmentRunArn,
    araAgentHealth,
    araAgentHealthCode,
    araTelemetryMetadata,
    araAgentHealthDetails,
    araAutoScalingGroup,

    -- * PreviewStatus
    PreviewStatus (..),

    -- * InspectorServiceAttributes
    InspectorServiceAttributes (..),
    mkInspectorServiceAttributes,
    isaSchemaVersion,
    isaAssessmentRunArn,
    isaRulesPackageArn,

    -- * RuleName
    RuleName (..),

    -- * SecurityGroup
    SecurityGroup (..),
    mkSecurityGroup,
    sgGroupId,
    sgGroupName,

    -- * KernelVersion
    KernelVersion (..),

    -- * AssessmentTemplateName
    AssessmentTemplateName (..),

    -- * AgentId
    AgentId (..),

    -- * FailedItemDetails
    FailedItemDetails (..),
    mkFailedItemDetails,
    fidFailureCode,
    fidRetryable,

    -- * InspectorEvent
    InspectorEvent (..),

    -- * AssetAttributes
    AssetAttributes (..),
    mkAssetAttributes,
    aaSchemaVersion,
    aaAgentId,
    aaAmiId,
    aaAutoScalingGroup,
    aaHostname,
    aaIpv4Addresses,
    aaNetworkInterfaces,
    aaTags,

    -- * AssessmentRunFilter
    AssessmentRunFilter (..),
    mkAssessmentRunFilter,
    arfCompletionTimeRange,
    arfDurationRange,
    arfNamePattern,
    arfRulesPackageArns,
    arfStartTimeRange,
    arfStateChangeTimeRange,
    arfStates,

    -- * EventSubscription
    EventSubscription (..),
    mkEventSubscription,
    esEvent,
    esSubscribedAt,

    -- * AssessmentTarget
    AssessmentTarget (..),
    mkAssessmentTarget,
    aArn,
    aName,
    aCreatedAt,
    aUpdatedAt,
    aResourceGroupArn,

    -- * TelemetryMetadata
    TelemetryMetadata (..),
    mkTelemetryMetadata,
    tmMessageType,
    tmCount,
    tmDataSize,

    -- * ReportStatus
    ReportStatus (..),

    -- * Version
    Version (..),

    -- * Scope
    Scope (..),
    mkScope,
    sKey,
    sValue,

    -- * FindingFilter
    FindingFilter (..),
    mkFindingFilter,
    ffAgentIds,
    ffAttributes,
    ffAutoScalingGroups,
    ffCreationTimeRange,
    ffRuleNames,
    ffRulesPackageArns,
    ffSeverities,
    ffUserAttributes,

    -- * AgentHealth
    AgentHealth (..),

    -- * ResourceGroup
    ResourceGroup (..),
    mkResourceGroup,
    rgArn,
    rgTags,
    rgCreatedAt,

    -- * Ipv4Address
    Ipv4Address (..),

    -- * AssetType
    AssetType (..),

    -- * AssessmentRunState
    AssessmentRunState (..),

    -- * AssessmentRunName
    AssessmentRunName (..),

    -- * AmiId
    AmiId (..),

    -- * AttributeKey
    AttributeKey (..),

    -- * Subscription
    Subscription (..),
    mkSubscription,
    sResourceArn,
    sTopicArn,
    sEventSubscriptions,

    -- * Message
    Message (..),

    -- * ReportFileFormat
    ReportFileFormat (..),

    -- * FailedItemErrorCode
    FailedItemErrorCode (..),

    -- * AssessmentTemplateFilter
    AssessmentTemplateFilter (..),
    mkAssessmentTemplateFilter,
    atfDurationRange,
    atfNamePattern,
    atfRulesPackageArns,

    -- * StopAction
    StopAction (..),

    -- * ReportType
    ReportType (..),

    -- * AssessmentRunStateChange
    AssessmentRunStateChange (..),
    mkAssessmentRunStateChange,
    arscStateChangedAt,
    arscState,

    -- * DurationRange
    DurationRange (..),
    mkDurationRange,
    drMaxSeconds,
    drMinSeconds,

    -- * AgentFilter
    AgentFilter (..),
    mkAgentFilter,
    afAgentHealths,
    afAgentHealthCodes,

    -- * TimestampRange
    TimestampRange (..),
    mkTimestampRange,
    trBeginDate,
    trEndDate,

    -- * NextToken
    NextToken (..),

    -- * Title
    Title (..),

    -- * Description
    Description (..),

    -- * Recommendation
    Recommendation (..),

    -- * Key
    Key (..),

    -- * Value
    Value (..),

    -- * AssessmentRunArn
    AssessmentRunArn (..),

    -- * PrivateDnsName
    PrivateDnsName (..),

    -- * PrivateIpAddress
    PrivateIpAddress (..),

    -- * RoleArn
    RoleArn (..),

    -- * ResourceArn
    ResourceArn (..),

    -- * Name
    Name (..),

    -- * AssessmentTemplateArn
    AssessmentTemplateArn (..),

    -- * Provider
    Provider (..),

    -- * ResourceGroupArn
    ResourceGroupArn (..),

    -- * PreviewToken
    PreviewToken (..),

    -- * SnsTopicArn
    SnsTopicArn (..),

    -- * Id
    Id (..),

    -- * Service
    Service (..),

    -- * AgentHealthDetails
    AgentHealthDetails (..),
  )
where

import Network.AWS.Inspector.Types.AgentFilter
import Network.AWS.Inspector.Types.AgentHealth
import Network.AWS.Inspector.Types.AgentHealthCode
import Network.AWS.Inspector.Types.AgentHealthDetails
import Network.AWS.Inspector.Types.AgentId
import Network.AWS.Inspector.Types.AgentPreview
import Network.AWS.Inspector.Types.AgentVersion
import Network.AWS.Inspector.Types.AmiId
import Network.AWS.Inspector.Types.Arn
import Network.AWS.Inspector.Types.AssessmentRun
import Network.AWS.Inspector.Types.AssessmentRunAgent
import Network.AWS.Inspector.Types.AssessmentRunArn
import Network.AWS.Inspector.Types.AssessmentRunFilter
import Network.AWS.Inspector.Types.AssessmentRunName
import Network.AWS.Inspector.Types.AssessmentRunNotification
import Network.AWS.Inspector.Types.AssessmentRunNotificationSnsStatusCode
import Network.AWS.Inspector.Types.AssessmentRunState
import Network.AWS.Inspector.Types.AssessmentRunStateChange
import Network.AWS.Inspector.Types.AssessmentTarget
import Network.AWS.Inspector.Types.AssessmentTargetFilter
import Network.AWS.Inspector.Types.AssessmentTargetName
import Network.AWS.Inspector.Types.AssessmentTemplate
import Network.AWS.Inspector.Types.AssessmentTemplateArn
import Network.AWS.Inspector.Types.AssessmentTemplateFilter
import Network.AWS.Inspector.Types.AssessmentTemplateName
import Network.AWS.Inspector.Types.AssetAttributes
import Network.AWS.Inspector.Types.AssetType
import Network.AWS.Inspector.Types.Attribute
import Network.AWS.Inspector.Types.AttributeKey
import Network.AWS.Inspector.Types.AutoScalingGroup
import Network.AWS.Inspector.Types.Description
import Network.AWS.Inspector.Types.DurationRange
import Network.AWS.Inspector.Types.EventSubscription
import Network.AWS.Inspector.Types.Exclusion
import Network.AWS.Inspector.Types.ExclusionPreview
import Network.AWS.Inspector.Types.FailedItemDetails
import Network.AWS.Inspector.Types.FailedItemErrorCode
import Network.AWS.Inspector.Types.Finding
import Network.AWS.Inspector.Types.FindingFilter
import Network.AWS.Inspector.Types.Hostname
import Network.AWS.Inspector.Types.Id
import Network.AWS.Inspector.Types.InspectorEvent
import Network.AWS.Inspector.Types.InspectorServiceAttributes
import Network.AWS.Inspector.Types.Ipv4Address
import Network.AWS.Inspector.Types.KernelVersion
import Network.AWS.Inspector.Types.Key
import Network.AWS.Inspector.Types.Locale
import Network.AWS.Inspector.Types.Message
import Network.AWS.Inspector.Types.MessageType
import Network.AWS.Inspector.Types.Name
import Network.AWS.Inspector.Types.NamePattern
import Network.AWS.Inspector.Types.NetworkInterface
import Network.AWS.Inspector.Types.NextToken
import Network.AWS.Inspector.Types.OperatingSystem
import Network.AWS.Inspector.Types.PaginationToken
import Network.AWS.Inspector.Types.PreviewStatus
import Network.AWS.Inspector.Types.PreviewToken
import Network.AWS.Inspector.Types.PrivateDnsName
import Network.AWS.Inspector.Types.PrivateIp
import Network.AWS.Inspector.Types.PrivateIpAddress
import Network.AWS.Inspector.Types.Provider
import Network.AWS.Inspector.Types.Recommendation
import Network.AWS.Inspector.Types.ReportFileFormat
import Network.AWS.Inspector.Types.ReportStatus
import Network.AWS.Inspector.Types.ReportType
import Network.AWS.Inspector.Types.ResourceArn
import Network.AWS.Inspector.Types.ResourceGroup
import Network.AWS.Inspector.Types.ResourceGroupArn
import Network.AWS.Inspector.Types.ResourceGroupTag
import Network.AWS.Inspector.Types.RoleArn
import Network.AWS.Inspector.Types.RuleName
import Network.AWS.Inspector.Types.RulesPackage
import Network.AWS.Inspector.Types.Scope
import Network.AWS.Inspector.Types.ScopeType
import Network.AWS.Inspector.Types.SecurityGroup
import Network.AWS.Inspector.Types.Service
import Network.AWS.Inspector.Types.Severity
import Network.AWS.Inspector.Types.SnsTopicArn
import Network.AWS.Inspector.Types.StopAction
import Network.AWS.Inspector.Types.Subscription
import Network.AWS.Inspector.Types.Tag
import Network.AWS.Inspector.Types.TelemetryMetadata
import Network.AWS.Inspector.Types.Text
import Network.AWS.Inspector.Types.TimestampRange
import Network.AWS.Inspector.Types.Title
import Network.AWS.Inspector.Types.Url
import Network.AWS.Inspector.Types.Value
import Network.AWS.Inspector.Types.Version
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-02-16@ of the Amazon Inspector SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "Inspector",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "inspector",
      Core._svcVersion = "2016-02-16",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "Inspector",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | You do not have required permissions to access the requested resource.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError mkServiceConfig "AccessDeniedException"
{-# DEPRECATED _AccessDeniedException "Use generic-lens or generic-optics instead." #-}

-- | You cannot perform a specified action if an assessment run is currently in progress.
_AssessmentRunInProgressException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AssessmentRunInProgressException =
  Core._MatchServiceError
    mkServiceConfig
    "AssessmentRunInProgressException"
{-# DEPRECATED _AssessmentRunInProgressException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because it referenced an entity that does not exist. The error code describes the entity.
_NoSuchEntityException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchEntityException =
  Core._MatchServiceError mkServiceConfig "NoSuchEntityException"
{-# DEPRECATED _NoSuchEntityException "Use generic-lens or generic-optics instead." #-}

-- | Used by the 'GetAssessmentReport' API. The request was rejected because you tried to generate a report for an assessment run that existed before reporting was supported in Amazon Inspector. You can only generate reports for assessment runs that took place or will take place after generating reports in Amazon Inspector became available.
_UnsupportedFeatureException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedFeatureException =
  Core._MatchServiceError
    mkServiceConfig
    "UnsupportedFeatureException"
{-# DEPRECATED _UnsupportedFeatureException "Use generic-lens or generic-optics instead." #-}

-- | The request is rejected. The specified assessment template is currently generating an exclusions preview.
_PreviewGenerationInProgressException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PreviewGenerationInProgressException =
  Core._MatchServiceError
    mkServiceConfig
    "PreviewGenerationInProgressException"
{-# DEPRECATED _PreviewGenerationInProgressException "Use generic-lens or generic-optics instead." #-}

-- | You started an assessment run, but one of the instances is already participating in another assessment run.
_AgentsAlreadyRunningAssessmentException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AgentsAlreadyRunningAssessmentException =
  Core._MatchServiceError
    mkServiceConfig
    "AgentsAlreadyRunningAssessmentException"
{-# DEPRECATED _AgentsAlreadyRunningAssessmentException "Use generic-lens or generic-optics instead." #-}

-- | Amazon Inspector cannot assume the cross-account role that it needs to list your EC2 instances during the assessment run.
_InvalidCrossAccountRoleException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidCrossAccountRoleException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidCrossAccountRoleException"
{-# DEPRECATED _InvalidCrossAccountRoleException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because an invalid or out-of-range value was supplied for an input parameter.
_InvalidInputException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidInputException =
  Core._MatchServiceError mkServiceConfig "InvalidInputException"
{-# DEPRECATED _InvalidInputException "Use generic-lens or generic-optics instead." #-}

-- | Internal server error.
_InternalException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalException =
  Core._MatchServiceError mkServiceConfig "InternalException"
{-# DEPRECATED _InternalException "Use generic-lens or generic-optics instead." #-}

-- | The serice is temporary unavailable.
_ServiceTemporarilyUnavailableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceTemporarilyUnavailableException =
  Core._MatchServiceError
    mkServiceConfig
    "ServiceTemporarilyUnavailableException"
{-# DEPRECATED _ServiceTemporarilyUnavailableException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because it attempted to create resources beyond the current AWS account limits. The error code describes the limit exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError mkServiceConfig "LimitExceededException"
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead." #-}

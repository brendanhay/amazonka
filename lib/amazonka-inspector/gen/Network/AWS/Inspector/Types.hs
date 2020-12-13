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
    inspectorService,

    -- * Errors

    -- * AgentHealth
    AgentHealth (..),

    -- * AgentHealthCode
    AgentHealthCode (..),

    -- * AssessmentRunNotificationSNSStatusCode
    AssessmentRunNotificationSNSStatusCode (..),

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
    mkAgentFilter,
    afAgentHealths,
    afAgentHealthCodes,

    -- * AgentPreview
    AgentPreview (..),
    mkAgentPreview,
    apHostname,
    apAutoScalingGroup,
    apOperatingSystem,
    apAgentVersion,
    apKernelVersion,
    apAgentId,
    apAgentHealth,
    apIpv4Address,

    -- * AssessmentRun
    AssessmentRun (..),
    mkAssessmentRun,
    arDataCollected,
    arState,
    arArn,
    arCreatedAt,
    arFindingCounts,
    arUserAttributesForFindings,
    arRulesPackageARNs,
    arStartedAt,
    arName,
    arDurationInSeconds,
    arStateChanges,
    arCompletedAt,
    arStateChangedAt,
    arAssessmentTemplateARN,
    arNotifications,

    -- * AssessmentRunAgent
    AssessmentRunAgent (..),
    mkAssessmentRunAgent,
    araAutoScalingGroup,
    araAgentHealthCode,
    araAgentId,
    araAgentHealthDetails,
    araTelemetryMetadata,
    araAssessmentRunARN,
    araAgentHealth,

    -- * AssessmentRunFilter
    AssessmentRunFilter (..),
    mkAssessmentRunFilter,
    arfStates,
    arfNamePattern,
    arfStartTimeRange,
    arfStateChangeTimeRange,
    arfRulesPackageARNs,
    arfCompletionTimeRange,
    arfDurationRange,

    -- * AssessmentRunNotification
    AssessmentRunNotification (..),
    mkAssessmentRunNotification,
    arnEvent,
    arnSnsTopicARN,
    arnError,
    arnSnsPublishStatusCode,
    arnDate,
    arnMessage,

    -- * AssessmentRunStateChange
    AssessmentRunStateChange (..),
    mkAssessmentRunStateChange,
    arscState,
    arscStateChangedAt,

    -- * AssessmentTarget
    AssessmentTarget (..),
    mkAssessmentTarget,
    aArn,
    aCreatedAt,
    aResourceGroupARN,
    aName,
    aUpdatedAt,

    -- * AssessmentTargetFilter
    AssessmentTargetFilter (..),
    mkAssessmentTargetFilter,
    atfAssessmentTargetNamePattern,

    -- * AssessmentTemplate
    AssessmentTemplate (..),
    mkAssessmentTemplate,
    atAssessmentTargetARN,
    atArn,
    atCreatedAt,
    atLastAssessmentRunARN,
    atUserAttributesForFindings,
    atRulesPackageARNs,
    atAssessmentRunCount,
    atName,
    atDurationInSeconds,

    -- * AssessmentTemplateFilter
    AssessmentTemplateFilter (..),
    mkAssessmentTemplateFilter,
    atfNamePattern,
    atfRulesPackageARNs,
    atfDurationRange,

    -- * AssetAttributes
    AssetAttributes (..),
    mkAssetAttributes,
    aaHostname,
    aaAutoScalingGroup,
    aaNetworkInterfaces,
    aaIpv4Addresses,
    aaSchemaVersion,
    aaAgentId,
    aaAmiId,
    aaTags,

    -- * Attribute
    Attribute (..),
    mkAttribute,
    aValue,
    aKey,

    -- * DurationRange
    DurationRange (..),
    mkDurationRange,
    drMinSeconds,
    drMaxSeconds,

    -- * EventSubscription
    EventSubscription (..),
    mkEventSubscription,
    esEvent,
    esSubscribedAt,

    -- * Exclusion
    Exclusion (..),
    mkExclusion,
    eArn,
    eScopes,
    eAttributes,
    eTitle,
    eDescription,
    eRecommendation,

    -- * ExclusionPreview
    ExclusionPreview (..),
    mkExclusionPreview,
    epScopes,
    epAttributes,
    epTitle,
    epDescription,
    epRecommendation,

    -- * FailedItemDetails
    FailedItemDetails (..),
    mkFailedItemDetails,
    fidFailureCode,
    fidRetryable,

    -- * Finding
    Finding (..),
    mkFinding,
    fArn,
    fCreatedAt,
    fService,
    fSeverity,
    fSchemaVersion,
    fUserAttributes,
    fConfidence,
    fAssetAttributes,
    fAttributes,
    fServiceAttributes,
    fId,
    fNumericSeverity,
    fUpdatedAt,
    fAssetType,
    fTitle,
    fIndicatorOfCompromise,
    fDescription,
    fRecommendation,

    -- * FindingFilter
    FindingFilter (..),
    mkFindingFilter,
    ffAgentIds,
    ffRuleNames,
    ffUserAttributes,
    ffRulesPackageARNs,
    ffAttributes,
    ffSeverities,
    ffCreationTimeRange,
    ffAutoScalingGroups,

    -- * InspectorServiceAttributes
    InspectorServiceAttributes (..),
    mkInspectorServiceAttributes,
    isaSchemaVersion,
    isaRulesPackageARN,
    isaAssessmentRunARN,

    -- * NetworkInterface
    NetworkInterface (..),
    mkNetworkInterface,
    niPrivateIPAddresses,
    niPublicDNSName,
    niSecurityGroups,
    niVpcId,
    niSubnetId,
    niNetworkInterfaceId,
    niPrivateIPAddress,
    niPublicIP,
    niPrivateDNSName,
    niIpv6Addresses,

    -- * PrivateIP
    PrivateIP (..),
    mkPrivateIP,
    piPrivateIPAddress,
    piPrivateDNSName,

    -- * ResourceGroup
    ResourceGroup (..),
    mkResourceGroup,
    rgArn,
    rgCreatedAt,
    rgTags,

    -- * ResourceGroupTag
    ResourceGroupTag (..),
    mkResourceGroupTag,
    rgtValue,
    rgtKey,

    -- * RulesPackage
    RulesPackage (..),
    mkRulesPackage,
    rpArn,
    rpName,
    rpVersion,
    rpDescription,
    rpProvider,

    -- * Scope
    Scope (..),
    mkScope,
    sValue,
    sKey,

    -- * SecurityGroup
    SecurityGroup (..),
    mkSecurityGroup,
    sgGroupId,
    sgGroupName,

    -- * Subscription
    Subscription (..),
    mkSubscription,
    sTopicARN,
    sEventSubscriptions,
    sResourceARN,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * TelemetryMetadata
    TelemetryMetadata (..),
    mkTelemetryMetadata,
    tmDataSize,
    tmMessageType,
    tmCount,

    -- * TimestampRange
    TimestampRange (..),
    mkTimestampRange,
    trEndDate,
    trBeginDate,
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
import Network.AWS.Inspector.Types.AssessmentRunNotificationSNSStatusCode
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
import Network.AWS.Inspector.Types.PrivateIP
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
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-02-16@ of the Amazon Inspector SDK configuration.
inspectorService :: Lude.Service
inspectorService =
  Lude.Service
    { Lude._svcAbbrev = "Inspector",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "inspector",
      Lude._svcVersion = "2016-02-16",
      Lude._svcEndpoint = Lude.defaultEndpoint inspectorService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "Inspector",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing

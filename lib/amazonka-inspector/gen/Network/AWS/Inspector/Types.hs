{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types
  ( -- * Service Configuration
    inspector,

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
    AgentFilter,
    agentFilter,
    afAgentHealths,
    afAgentHealthCodes,

    -- * AgentPreview
    AgentPreview,
    agentPreview,
    apHostname,
    apAutoScalingGroup,
    apOperatingSystem,
    apAgentVersion,
    apKernelVersion,
    apAgentHealth,
    apIpv4Address,
    apAgentId,

    -- * AssessmentRun
    AssessmentRun,
    assessmentRun,
    arStartedAt,
    arCompletedAt,
    arArn,
    arName,
    arAssessmentTemplateARN,
    arState,
    arDurationInSeconds,
    arRulesPackageARNs,
    arUserAttributesForFindings,
    arCreatedAt,
    arStateChangedAt,
    arDataCollected,
    arStateChanges,
    arNotifications,
    arFindingCounts,

    -- * AssessmentRunAgent
    AssessmentRunAgent,
    assessmentRunAgent,
    araAutoScalingGroup,
    araAgentHealthDetails,
    araAgentId,
    araAssessmentRunARN,
    araAgentHealth,
    araAgentHealthCode,
    araTelemetryMetadata,

    -- * AssessmentRunFilter
    AssessmentRunFilter,
    assessmentRunFilter,
    arfStates,
    arfNamePattern,
    arfStartTimeRange,
    arfStateChangeTimeRange,
    arfRulesPackageARNs,
    arfCompletionTimeRange,
    arfDurationRange,

    -- * AssessmentRunNotification
    AssessmentRunNotification,
    assessmentRunNotification,
    arnSnsTopicARN,
    arnSnsPublishStatusCode,
    arnMessage,
    arnDate,
    arnEvent,
    arnError,

    -- * AssessmentRunStateChange
    AssessmentRunStateChange,
    assessmentRunStateChange,
    arscStateChangedAt,
    arscState,

    -- * AssessmentTarget
    AssessmentTarget,
    assessmentTarget,
    aResourceGroupARN,
    aArn,
    aName,
    aCreatedAt,
    aUpdatedAt,

    -- * AssessmentTargetFilter
    AssessmentTargetFilter,
    assessmentTargetFilter,
    atfAssessmentTargetNamePattern,

    -- * AssessmentTemplate
    AssessmentTemplate,
    assessmentTemplate,
    atLastAssessmentRunARN,
    atArn,
    atName,
    atAssessmentTargetARN,
    atDurationInSeconds,
    atRulesPackageARNs,
    atUserAttributesForFindings,
    atAssessmentRunCount,
    atCreatedAt,

    -- * AssessmentTemplateFilter
    AssessmentTemplateFilter,
    assessmentTemplateFilter,
    atfNamePattern,
    atfRulesPackageARNs,
    atfDurationRange,

    -- * AssetAttributes
    AssetAttributes,
    assetAttributes,
    aaHostname,
    aaAutoScalingGroup,
    aaNetworkInterfaces,
    aaIpv4Addresses,
    aaAgentId,
    aaAmiId,
    aaTags,
    aaSchemaVersion,

    -- * Attribute
    Attribute,
    attribute,
    aValue,
    aKey,

    -- * DurationRange
    DurationRange,
    durationRange,
    drMinSeconds,
    drMaxSeconds,

    -- * EventSubscription
    EventSubscription,
    eventSubscription,
    esEvent,
    esSubscribedAt,

    -- * Exclusion
    Exclusion,
    exclusion,
    eAttributes,
    eArn,
    eTitle,
    eDescription,
    eRecommendation,
    eScopes,

    -- * ExclusionPreview
    ExclusionPreview,
    exclusionPreview,
    epAttributes,
    epTitle,
    epDescription,
    epRecommendation,
    epScopes,

    -- * FailedItemDetails
    FailedItemDetails,
    failedItemDetails,
    fidFailureCode,
    fidRetryable,

    -- * Finding
    Finding,
    finding,
    fService,
    fSeverity,
    fSchemaVersion,
    fConfidence,
    fAssetAttributes,
    fServiceAttributes,
    fId,
    fNumericSeverity,
    fAssetType,
    fTitle,
    fIndicatorOfCompromise,
    fDescription,
    fRecommendation,
    fArn,
    fAttributes,
    fUserAttributes,
    fCreatedAt,
    fUpdatedAt,

    -- * FindingFilter
    FindingFilter,
    findingFilter,
    ffAgentIds,
    ffRuleNames,
    ffUserAttributes,
    ffRulesPackageARNs,
    ffAttributes,
    ffSeverities,
    ffCreationTimeRange,
    ffAutoScalingGroups,

    -- * InspectorServiceAttributes
    InspectorServiceAttributes,
    inspectorServiceAttributes,
    isaRulesPackageARN,
    isaAssessmentRunARN,
    isaSchemaVersion,

    -- * NetworkInterface
    NetworkInterface,
    networkInterface,
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
    PrivateIP,
    privateIP,
    piPrivateIPAddress,
    piPrivateDNSName,

    -- * ResourceGroup
    ResourceGroup,
    resourceGroup,
    rgArn,
    rgTags,
    rgCreatedAt,

    -- * ResourceGroupTag
    ResourceGroupTag,
    resourceGroupTag,
    rgtValue,
    rgtKey,

    -- * RulesPackage
    RulesPackage,
    rulesPackage,
    rpDescription,
    rpArn,
    rpName,
    rpVersion,
    rpProvider,

    -- * Scope
    Scope,
    scope,
    sValue,
    sKey,

    -- * SecurityGroup
    SecurityGroup,
    securityGroup,
    sgGroupId,
    sgGroupName,

    -- * Subscription
    Subscription,
    subscription,
    sResourceARN,
    sTopicARN,
    sEventSubscriptions,

    -- * Tag
    Tag,
    tag,
    tagValue,
    tagKey,

    -- * TelemetryMetadata
    TelemetryMetadata,
    telemetryMetadata,
    tmDataSize,
    tmMessageType,
    tmCount,

    -- * TimestampRange
    TimestampRange,
    timestampRange,
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2016-02-16@ of the Amazon Inspector SDK configuration.
inspector :: Service
inspector =
  Service
    { _svcAbbrev = "Inspector",
      _svcSigner = v4,
      _svcPrefix = "inspector",
      _svcVersion = "2016-02-16",
      _svcEndpoint = defaultEndpoint inspector,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "Inspector",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

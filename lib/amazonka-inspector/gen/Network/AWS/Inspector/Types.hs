{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Inspector.Types
    (
    -- * Service Configuration
      inspector

    -- * Errors
    , _AccessDeniedException
    , _AssessmentRunInProgressException
    , _NoSuchEntityException
    , _UnsupportedFeatureException
    , _AgentsAlreadyRunningAssessmentException
    , _InvalidCrossAccountRoleException
    , _InvalidInputException
    , _InternalException
    , _LimitExceededException

    -- * AgentHealth
    , AgentHealth (..)

    -- * AgentHealthCode
    , AgentHealthCode (..)

    -- * AssessmentRunNotificationSNSStatusCode
    , AssessmentRunNotificationSNSStatusCode (..)

    -- * AssessmentRunState
    , AssessmentRunState (..)

    -- * AssetType
    , AssetType (..)

    -- * FailedItemErrorCode
    , FailedItemErrorCode (..)

    -- * InspectorEvent
    , InspectorEvent (..)

    -- * Locale
    , Locale (..)

    -- * ReportFileFormat
    , ReportFileFormat (..)

    -- * ReportStatus
    , ReportStatus (..)

    -- * ReportType
    , ReportType (..)

    -- * Severity
    , Severity (..)

    -- * StopAction
    , StopAction (..)

    -- * AgentFilter
    , AgentFilter
    , agentFilter
    , afAgentHealths
    , afAgentHealthCodes

    -- * AgentPreview
    , AgentPreview
    , agentPreview
    , apHostname
    , apAutoScalingGroup
    , apOperatingSystem
    , apAgentVersion
    , apKernelVersion
    , apAgentHealth
    , apIpv4Address
    , apAgentId

    -- * AssessmentRun
    , AssessmentRun
    , assessmentRun
    , arStartedAt
    , arCompletedAt
    , arArn
    , arName
    , arAssessmentTemplateARN
    , arState
    , arDurationInSeconds
    , arRulesPackageARNs
    , arUserAttributesForFindings
    , arCreatedAt
    , arStateChangedAt
    , arDataCollected
    , arStateChanges
    , arNotifications
    , arFindingCounts

    -- * AssessmentRunAgent
    , AssessmentRunAgent
    , assessmentRunAgent
    , araAutoScalingGroup
    , araAgentHealthDetails
    , araAgentId
    , araAssessmentRunARN
    , araAgentHealth
    , araAgentHealthCode
    , araTelemetryMetadata

    -- * AssessmentRunFilter
    , AssessmentRunFilter
    , assessmentRunFilter
    , arfStates
    , arfNamePattern
    , arfStartTimeRange
    , arfStateChangeTimeRange
    , arfRulesPackageARNs
    , arfCompletionTimeRange
    , arfDurationRange

    -- * AssessmentRunNotification
    , AssessmentRunNotification
    , assessmentRunNotification
    , arnSnsTopicARN
    , arnSnsPublishStatusCode
    , arnMessage
    , arnDate
    , arnEvent
    , arnError

    -- * AssessmentRunStateChange
    , AssessmentRunStateChange
    , assessmentRunStateChange
    , arscStateChangedAt
    , arscState

    -- * AssessmentTarget
    , AssessmentTarget
    , assessmentTarget
    , aArn
    , aName
    , aResourceGroupARN
    , aCreatedAt
    , aUpdatedAt

    -- * AssessmentTargetFilter
    , AssessmentTargetFilter
    , assessmentTargetFilter
    , atfAssessmentTargetNamePattern

    -- * AssessmentTemplate
    , AssessmentTemplate
    , assessmentTemplate
    , atLastAssessmentRunARN
    , atArn
    , atName
    , atAssessmentTargetARN
    , atDurationInSeconds
    , atRulesPackageARNs
    , atUserAttributesForFindings
    , atAssessmentRunCount
    , atCreatedAt

    -- * AssessmentTemplateFilter
    , AssessmentTemplateFilter
    , assessmentTemplateFilter
    , atfNamePattern
    , atfRulesPackageARNs
    , atfDurationRange

    -- * AssetAttributes
    , AssetAttributes
    , assetAttributes
    , aaHostname
    , aaAutoScalingGroup
    , aaIpv4Addresses
    , aaAgentId
    , aaAmiId
    , aaSchemaVersion

    -- * Attribute
    , Attribute
    , attribute
    , aValue
    , aKey

    -- * DurationRange
    , DurationRange
    , durationRange
    , drMinSeconds
    , drMaxSeconds

    -- * EventSubscription
    , EventSubscription
    , eventSubscription
    , esEvent
    , esSubscribedAt

    -- * FailedItemDetails
    , FailedItemDetails
    , failedItemDetails
    , fidFailureCode
    , fidRetryable

    -- * Finding
    , Finding
    , finding
    , fService
    , fSeverity
    , fSchemaVersion
    , fConfidence
    , fAssetAttributes
    , fServiceAttributes
    , fId
    , fNumericSeverity
    , fAssetType
    , fTitle
    , fIndicatorOfCompromise
    , fDescription
    , fRecommendation
    , fArn
    , fAttributes
    , fUserAttributes
    , fCreatedAt
    , fUpdatedAt

    -- * FindingFilter
    , FindingFilter
    , findingFilter
    , ffAgentIds
    , ffRuleNames
    , ffUserAttributes
    , ffRulesPackageARNs
    , ffAttributes
    , ffSeverities
    , ffCreationTimeRange
    , ffAutoScalingGroups

    -- * InspectorServiceAttributes
    , InspectorServiceAttributes
    , inspectorServiceAttributes
    , isaRulesPackageARN
    , isaAssessmentRunARN
    , isaSchemaVersion

    -- * ResourceGroup
    , ResourceGroup
    , resourceGroup
    , rgArn
    , rgTags
    , rgCreatedAt

    -- * ResourceGroupTag
    , ResourceGroupTag
    , resourceGroupTag
    , rgtValue
    , rgtKey

    -- * RulesPackage
    , RulesPackage
    , rulesPackage
    , rpDescription
    , rpArn
    , rpName
    , rpVersion
    , rpProvider

    -- * Subscription
    , Subscription
    , subscription
    , sResourceARN
    , sTopicARN
    , sEventSubscriptions

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- * TelemetryMetadata
    , TelemetryMetadata
    , telemetryMetadata
    , tmDataSize
    , tmMessageType
    , tmCount

    -- * TimestampRange
    , TimestampRange
    , timestampRange
    , trEndDate
    , trBeginDate
    ) where

import Network.AWS.Inspector.Types.Product
import Network.AWS.Inspector.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2016-02-16@ of the Amazon Inspector SDK configuration.
inspector :: Service
inspector =
  Service
    { _svcAbbrev = "Inspector"
    , _svcSigner = v4
    , _svcPrefix = "inspector"
    , _svcVersion = "2016-02-16"
    , _svcEndpoint = defaultEndpoint inspector
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "Inspector"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | You do not have required permissions to access the requested resource.
--
--
_AccessDeniedException :: AsError a => Getting (First ServiceError) a ServiceError
_AccessDeniedException = _MatchServiceError inspector "AccessDeniedException"


-- | You cannot perform a specified action if an assessment run is currently in progress.
--
--
_AssessmentRunInProgressException :: AsError a => Getting (First ServiceError) a ServiceError
_AssessmentRunInProgressException =
  _MatchServiceError inspector "AssessmentRunInProgressException"


-- | The request was rejected because it referenced an entity that does not exist. The error code describes the entity.
--
--
_NoSuchEntityException :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchEntityException = _MatchServiceError inspector "NoSuchEntityException"


-- | Used by the 'GetAssessmentReport' API. The request was rejected because you tried to generate a report for an assessment run that existed before reporting was supported in Amazon Inspector. You can only generate reports for assessment runs that took place or will take place after generating reports in Amazon Inspector became available.
--
--
_UnsupportedFeatureException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedFeatureException =
  _MatchServiceError inspector "UnsupportedFeatureException"


-- | You started an assessment run, but one of the instances is already participating in another assessment run.
--
--
_AgentsAlreadyRunningAssessmentException :: AsError a => Getting (First ServiceError) a ServiceError
_AgentsAlreadyRunningAssessmentException =
  _MatchServiceError inspector "AgentsAlreadyRunningAssessmentException"


-- | Amazon Inspector cannot assume the cross-account role that it needs to list your EC2 instances during the assessment run.
--
--
_InvalidCrossAccountRoleException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidCrossAccountRoleException =
  _MatchServiceError inspector "InvalidCrossAccountRoleException"


-- | The request was rejected because an invalid or out-of-range value was supplied for an input parameter.
--
--
_InvalidInputException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInputException = _MatchServiceError inspector "InvalidInputException"


-- | Internal server error.
--
--
_InternalException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalException = _MatchServiceError inspector "InternalException"


-- | The request was rejected because it attempted to create resources beyond the current AWS account limits. The error code describes the limit exceeded.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException = _MatchServiceError inspector "LimitExceededException"


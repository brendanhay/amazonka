{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Inspector.Types
    (
    -- * Service Configuration
      inspector

    -- * Errors
    , _AccessDeniedException
    , _NoSuchEntityException
    , _OperationInProgressException
    , _InvalidCrossAccountRoleException
    , _InvalidInputException
    , _InternalException

    -- * Agent
    , Agent
    , agent
    , aTelemetry
    , aAutoScalingGroup
    , aAgentHealthCode
    , aAssessmentARN
    , aAgentId
    , aAccountId
    , aAgentHealthDetails
    , aAgentHealth

    -- * AgentPreview
    , AgentPreview
    , agentPreview
    , apAutoScalingGroup
    , apAgentId

    -- * AgentsFilter
    , AgentsFilter
    , agentsFilter
    , afAgentHealthList

    -- * Application
    , Application
    , application
    , aApplicationARN
    , aResourceGroupARN
    , aApplicationName

    -- * ApplicationsFilter
    , ApplicationsFilter
    , applicationsFilter
    , afApplicationNamePatterns

    -- * Assessment
    , Assessment
    , assessment
    , assDataCollected
    , assApplicationARN
    , assStartTime
    , assAssessmentARN
    , assUserAttributesForFindings
    , assFailureMessage
    , assAssessmentState
    , assEndTime
    , assDurationInSeconds
    , assAssessmentName

    -- * AssessmentsFilter
    , AssessmentsFilter
    , assessmentsFilter
    , afDataCollected
    , afAssessmentStates
    , afStartTimeRange
    , afAssessmentNamePatterns
    , afEndTimeRange
    , afDurationRange

    -- * Attribute
    , Attribute
    , attribute
    , aValue
    , aKey

    -- * DurationRange
    , DurationRange
    , durationRange
    , drMaximum
    , drMinimum

    -- * Finding
    , Finding
    , finding
    , fAutoScalingGroup
    , fFinding
    , fSeverity
    , fUserAttributes
    , fRuleName
    , fAgentId
    , fRunARN
    , fAttributes
    , fRulesPackageARN
    , fFindingARN
    , fDescription
    , fRecommendation

    -- * FindingsFilter
    , FindingsFilter
    , findingsFilter
    , ffRuleNames
    , ffUserAttributes
    , ffRulesPackageARNs
    , ffAttributes
    , ffSeverities

    -- * LocalizedText
    , LocalizedText
    , localizedText
    , ltKey
    , ltParameters

    -- * LocalizedTextKey
    , LocalizedTextKey
    , localizedTextKey
    , ltkFacility
    , ltkId

    -- * MessageTypeTelemetry
    , MessageTypeTelemetry
    , messageTypeTelemetry
    , mttDataSize
    , mttMessageType
    , mttCount

    -- * Parameter
    , Parameter
    , parameter
    , pValue
    , pName

    -- * ResourceGroup
    , ResourceGroup
    , resourceGroup
    , rgResourceGroupTags
    , rgResourceGroupARN

    -- * RulesPackage
    , RulesPackage
    , rulesPackage
    , rpVersion
    , rpRulesPackageARN
    , rpRulesPackageName
    , rpDescription
    , rpProvider

    -- * Run
    , Run
    , run
    , runCreationTime
    , runRulesPackages
    , runAssessmentARN
    , runRunState
    , runRunName
    , runCompletionTime
    , runRunARN

    -- * RunsFilter
    , RunsFilter
    , runsFilter
    , rfCreationTime
    , rfRulesPackages
    , rfRunStates
    , rfRunNamePatterns
    , rfCompletionTime

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- * Telemetry
    , Telemetry
    , telemetry
    , tStatus
    , tMessageTypeTelemetries

    -- * TimestampRange
    , TimestampRange
    , timestampRange
    , trMaximum
    , trMinimum
    ) where

import           Network.AWS.Inspector.Types.Product
import           Network.AWS.Inspector.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version '2015-08-18' of the Amazon Inspector SDK configuration.
inspector :: Service
inspector =
    Service
    { _svcAbbrev = "Inspector"
    , _svcSigner = v4
    , _svcPrefix = "inspector"
    , _svcVersion = "2015-08-18"
    , _svcEndpoint = defaultEndpoint inspector
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError
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
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | Prism for AccessDeniedException' errors.
_AccessDeniedException :: AsError a => Getting (First ServiceError) a ServiceError
_AccessDeniedException = _ServiceError . hasCode "AccessDeniedException"

-- | Prism for NoSuchEntityException' errors.
_NoSuchEntityException :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchEntityException = _ServiceError . hasCode "NoSuchEntityException"

-- | Prism for OperationInProgressException' errors.
_OperationInProgressException :: AsError a => Getting (First ServiceError) a ServiceError
_OperationInProgressException =
    _ServiceError . hasCode "OperationInProgressException"

-- | Prism for InvalidCrossAccountRoleException' errors.
_InvalidCrossAccountRoleException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidCrossAccountRoleException =
    _ServiceError . hasCode "InvalidCrossAccountRoleException"

-- | Prism for InvalidInputException' errors.
_InvalidInputException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInputException = _ServiceError . hasCode "InvalidInputException"

-- | Prism for InternalException' errors.
_InternalException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalException = _ServiceError . hasCode "InternalException"

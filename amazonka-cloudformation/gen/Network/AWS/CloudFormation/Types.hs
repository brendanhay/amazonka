{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types
    (
    -- * Service Configuration
      cloudFormation

    -- * Errors
    , _ChangeSetNotFoundException
    , _InvalidChangeSetStatusException
    , _InsufficientCapabilitiesException
    , _AlreadyExistsException
    , _LimitExceededException

    -- * Capability
    , Capability (..)

    -- * ChangeAction
    , ChangeAction (..)

    -- * ChangeSetStatus
    , ChangeSetStatus (..)

    -- * ChangeSource
    , ChangeSource (..)

    -- * ChangeType
    , ChangeType (..)

    -- * EvaluationType
    , EvaluationType (..)

    -- * OnFailure
    , OnFailure (..)

    -- * Replacement
    , Replacement (..)

    -- * RequiresRecreation
    , RequiresRecreation (..)

    -- * ResourceAttribute
    , ResourceAttribute (..)

    -- * ResourceSignalStatus
    , ResourceSignalStatus (..)

    -- * ResourceStatus
    , ResourceStatus (..)

    -- * StackStatus
    , StackStatus (..)

    -- * AccountLimit
    , AccountLimit
    , accountLimit
    , alValue
    , alName

    -- * Change
    , Change
    , change
    , cResourceChange
    , cType

    -- * ChangeSetSummary
    , ChangeSetSummary
    , changeSetSummary
    , cssCreationTime
    , cssStatus
    , cssChangeSetName
    , cssChangeSetId
    , cssStatusReason
    , cssStackId
    , cssDescription
    , cssStackName

    -- * Output
    , Output
    , output
    , oOutputValue
    , oOutputKey
    , oDescription

    -- * Parameter
    , Parameter
    , parameter
    , pParameterValue
    , pParameterKey
    , pUsePreviousValue

    -- * ParameterConstraints
    , ParameterConstraints
    , parameterConstraints
    , pcAllowedValues

    -- * ParameterDeclaration
    , ParameterDeclaration
    , parameterDeclaration
    , pdParameterKey
    , pdParameterType
    , pdParameterConstraints
    , pdDefaultValue
    , pdNoEcho
    , pdDescription

    -- * ResourceChange
    , ResourceChange
    , resourceChange
    , rcLogicalResourceId
    , rcPhysicalResourceId
    , rcResourceType
    , rcAction
    , rcScope
    , rcDetails
    , rcReplacement

    -- * ResourceChangeDetail
    , ResourceChangeDetail
    , resourceChangeDetail
    , rcdCausingEntity
    , rcdChangeSource
    , rcdEvaluation
    , rcdTarget

    -- * ResourceTargetDefinition
    , ResourceTargetDefinition
    , resourceTargetDefinition
    , rtdAttribute
    , rtdRequiresRecreation
    , rtdName

    -- * Stack
    , Stack
    , stack
    , sDisableRollback
    , sLastUpdatedTime
    , sNotificationARNs
    , sStackStatusReason
    , sOutputs
    , sParameters
    , sStackId
    , sDescription
    , sCapabilities
    , sTags
    , sTimeoutInMinutes
    , sStackName
    , sCreationTime
    , sStackStatus

    -- * StackEvent
    , StackEvent
    , stackEvent
    , seLogicalResourceId
    , sePhysicalResourceId
    , seResourceType
    , seResourceStatusReason
    , seResourceProperties
    , seResourceStatus
    , seStackId
    , seEventId
    , seStackName
    , seTimestamp

    -- * StackResource
    , StackResource
    , stackResource
    , srPhysicalResourceId
    , srResourceStatusReason
    , srStackId
    , srDescription
    , srStackName
    , srLogicalResourceId
    , srResourceType
    , srTimestamp
    , srResourceStatus

    -- * StackResourceDetail
    , StackResourceDetail
    , stackResourceDetail
    , srdPhysicalResourceId
    , srdResourceStatusReason
    , srdMetadata
    , srdStackId
    , srdDescription
    , srdStackName
    , srdLogicalResourceId
    , srdResourceType
    , srdLastUpdatedTimestamp
    , srdResourceStatus

    -- * StackResourceSummary
    , StackResourceSummary
    , stackResourceSummary
    , srsPhysicalResourceId
    , srsResourceStatusReason
    , srsLogicalResourceId
    , srsResourceType
    , srsLastUpdatedTimestamp
    , srsResourceStatus

    -- * StackSummary
    , StackSummary
    , stackSummary
    , ssLastUpdatedTime
    , ssStackStatusReason
    , ssTemplateDescription
    , ssDeletionTime
    , ssStackId
    , ssStackName
    , ssCreationTime
    , ssStackStatus

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- * TemplateParameter
    , TemplateParameter
    , templateParameter
    , tpParameterKey
    , tpDefaultValue
    , tpNoEcho
    , tpDescription
    ) where

import           Network.AWS.CloudFormation.Types.Product
import           Network.AWS.CloudFormation.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version '2010-05-15' of the Amazon CloudFormation SDK configuration.
cloudFormation :: Service
cloudFormation =
    Service
    { _svcAbbrev = "CloudFormation"
    , _svcSigner = v4
    , _svcPrefix = "cloudformation"
    , _svcVersion = "2010-05-15"
    , _svcEndpoint = defaultEndpoint cloudFormation
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseXMLError
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
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | The specified change set name or ID doesn\'t exit. To view valid change
-- sets for a stack, use the 'ListChangeSets' action.
_ChangeSetNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ChangeSetNotFoundException =
    _ServiceError . hasStatus 404 . hasCode "ChangeSetNotFound"

-- | The specified change set cannot be used to update the stack. For
-- example, the change set status might be 'CREATE_IN_PROGRESS' or the
-- stack status might be 'UPDATE_IN_PROGRESS'.
_InvalidChangeSetStatusException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidChangeSetStatusException =
    _ServiceError . hasStatus 400 . hasCode "InvalidChangeSetStatus"

-- | The template contains resources with capabilities that were not
-- specified in the Capabilities parameter.
_InsufficientCapabilitiesException :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientCapabilitiesException =
    _ServiceError . hasStatus 400 . hasCode "InsufficientCapabilitiesException"

-- | Resource with the name requested already exists.
_AlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_AlreadyExistsException =
    _ServiceError . hasStatus 400 . hasCode "AlreadyExistsException"

-- | Quota for the resource has already been reached.
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
    _ServiceError . hasStatus 400 . hasCode "LimitExceededException"

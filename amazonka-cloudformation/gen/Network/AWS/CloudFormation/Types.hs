{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types
    (
    -- * Service Configuration
      cloudFormation

    -- * Errors
    , _CreatedButModifiedException
    , _ChangeSetNotFoundException
    , _OperationInProgressException
    , _InvalidChangeSetStatusException
    , _OperationNotFoundException
    , _OperationIdAlreadyExistsException
    , _InsufficientCapabilitiesException
    , _TokenAlreadyExistsException
    , _StackSetNotFoundException
    , _StackInstanceNotFoundException
    , _StackSetNotEmptyException
    , _InvalidOperationException
    , _NameAlreadyExistsException
    , _StaleRequestException
    , _AlreadyExistsException
    , _LimitExceededException

    -- * AccountGateStatus
    , AccountGateStatus (..)

    -- * Capability
    , Capability (..)

    -- * ChangeAction
    , ChangeAction (..)

    -- * ChangeSetStatus
    , ChangeSetStatus (..)

    -- * ChangeSetType
    , ChangeSetType (..)

    -- * ChangeSource
    , ChangeSource (..)

    -- * ChangeType
    , ChangeType (..)

    -- * EvaluationType
    , EvaluationType (..)

    -- * ExecutionStatus
    , ExecutionStatus (..)

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

    -- * StackInstanceStatus
    , StackInstanceStatus (..)

    -- * StackSetOperationAction
    , StackSetOperationAction (..)

    -- * StackSetOperationResultStatus
    , StackSetOperationResultStatus (..)

    -- * StackSetOperationStatus
    , StackSetOperationStatus (..)

    -- * StackSetStatus
    , StackSetStatus (..)

    -- * StackStatus
    , StackStatus (..)

    -- * TemplateStage
    , TemplateStage (..)

    -- * AccountGateResult
    , AccountGateResult
    , accountGateResult
    , agrStatus
    , agrStatusReason

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
    , cCreationTime
    , cStatus
    , cChangeSetName
    , cExecutionStatus
    , cChangeSetId
    , cStatusReason
    , cStackId
    , cDescription
    , cStackName

    -- * Export
    , Export
    , export'
    , eValue
    , eExportingStackId
    , eName

    -- * Output
    , Output
    , output
    , oOutputValue
    , oOutputKey
    , oExportName
    , oDescription

    -- * Parameter
    , Parameter
    , parameter
    , pParameterValue
    , pResolvedValue
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

    -- * RollbackConfiguration
    , RollbackConfiguration
    , rollbackConfiguration
    , rcRollbackTriggers
    , rcMonitoringTimeInMinutes

    -- * RollbackTrigger
    , RollbackTrigger
    , rollbackTrigger
    , rtARN
    , rtType

    -- * Stack
    , Stack
    , stack
    , sDisableRollback
    , sLastUpdatedTime
    , sRootId
    , sNotificationARNs
    , sStackStatusReason
    , sEnableTerminationProtection
    , sChangeSetId
    , sDeletionTime
    , sOutputs
    , sParameters
    , sStackId
    , sDescription
    , sCapabilities
    , sRollbackConfiguration
    , sTags
    , sTimeoutInMinutes
    , sParentId
    , sRoleARN
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
    , seClientRequestToken
    , seStackId
    , seEventId
    , seStackName
    , seTimestamp

    -- * StackInstance
    , StackInstance
    , stackInstance
    , siStatus
    , siAccount
    , siRegion
    , siStatusReason
    , siStackId
    , siParameterOverrides
    , siStackSetId

    -- * StackInstanceSummary
    , StackInstanceSummary
    , stackInstanceSummary
    , sisStatus
    , sisAccount
    , sisRegion
    , sisStatusReason
    , sisStackId
    , sisStackSetId

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

    -- * StackSet
    , StackSet
    , stackSet
    , ssStatus
    , ssAdministrationRoleARN
    , ssStackSetARN
    , ssParameters
    , ssTemplateBody
    , ssStackSetName
    , ssDescription
    , ssCapabilities
    , ssTags
    , ssStackSetId

    -- * StackSetOperation
    , StackSetOperation
    , stackSetOperation
    , ssoStatus
    , ssoAdministrationRoleARN
    , ssoAction
    , ssoEndTimestamp
    , ssoCreationTimestamp
    , ssoOperationPreferences
    , ssoOperationId
    , ssoRetainStacks
    , ssoStackSetId

    -- * StackSetOperationPreferences
    , StackSetOperationPreferences
    , stackSetOperationPreferences
    , ssopRegionOrder
    , ssopMaxConcurrentCount
    , ssopMaxConcurrentPercentage
    , ssopFailureToleranceCount
    , ssopFailureTolerancePercentage

    -- * StackSetOperationResultSummary
    , StackSetOperationResultSummary
    , stackSetOperationResultSummary
    , ssorsStatus
    , ssorsAccount
    , ssorsAccountGateResult
    , ssorsRegion
    , ssorsStatusReason

    -- * StackSetOperationSummary
    , StackSetOperationSummary
    , stackSetOperationSummary
    , ssosStatus
    , ssosAction
    , ssosEndTimestamp
    , ssosCreationTimestamp
    , ssosOperationId

    -- * StackSetSummary
    , StackSetSummary
    , stackSetSummary
    , sssStatus
    , sssStackSetName
    , sssDescription
    , sssStackSetId

    -- * StackSummary
    , StackSummary
    , stackSummary
    , ssLastUpdatedTime
    , ssRootId
    , ssStackStatusReason
    , ssTemplateDescription
    , ssDeletionTime
    , ssStackId
    , ssParentId
    , ssStackName
    , ssCreationTime
    , ssStackStatus

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- * TemplateParameter
    , TemplateParameter
    , templateParameter
    , tpParameterKey
    , tpDefaultValue
    , tpNoEcho
    , tpDescription
    ) where

import Network.AWS.CloudFormation.Types.Product
import Network.AWS.CloudFormation.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2010-05-15@ of the Amazon CloudFormation SDK configuration.
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
    , _svcError = parseXMLError "CloudFormation"
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


-- | The specified resource exists, but has been changed.
--
--
_CreatedButModifiedException :: AsError a => Getting (First ServiceError) a ServiceError
_CreatedButModifiedException =
  _MatchServiceError cloudFormation "CreatedButModifiedException" .
  hasStatus 409


-- | The specified change set name or ID doesn't exit. To view valid change sets for a stack, use the @ListChangeSets@ action.
--
--
_ChangeSetNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ChangeSetNotFoundException =
  _MatchServiceError cloudFormation "ChangeSetNotFound" . hasStatus 404


-- | Another operation is currently in progress for this stack set. Only one operation can be performed for a stack set at a given time.
--
--
_OperationInProgressException :: AsError a => Getting (First ServiceError) a ServiceError
_OperationInProgressException =
  _MatchServiceError cloudFormation "OperationInProgressException" .
  hasStatus 409


-- | The specified change set can't be used to update the stack. For example, the change set status might be @CREATE_IN_PROGRESS@ , or the stack status might be @UPDATE_IN_PROGRESS@ .
--
--
_InvalidChangeSetStatusException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidChangeSetStatusException =
  _MatchServiceError cloudFormation "InvalidChangeSetStatus" . hasStatus 400


-- | The specified ID refers to an operation that doesn't exist.
--
--
_OperationNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_OperationNotFoundException =
  _MatchServiceError cloudFormation "OperationNotFoundException" . hasStatus 404


-- | The specified operation ID already exists.
--
--
_OperationIdAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_OperationIdAlreadyExistsException =
  _MatchServiceError cloudFormation "OperationIdAlreadyExistsException" .
  hasStatus 409


-- | The template contains resources with capabilities that weren't specified in the Capabilities parameter.
--
--
_InsufficientCapabilitiesException :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientCapabilitiesException =
  _MatchServiceError cloudFormation "InsufficientCapabilitiesException" .
  hasStatus 400


-- | A client request token already exists.
--
--
_TokenAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_TokenAlreadyExistsException =
  _MatchServiceError cloudFormation "TokenAlreadyExistsException" .
  hasStatus 400


-- | The specified stack set doesn't exist.
--
--
_StackSetNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_StackSetNotFoundException =
  _MatchServiceError cloudFormation "StackSetNotFoundException" . hasStatus 404


-- | The specified stack instance doesn't exist.
--
--
_StackInstanceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_StackInstanceNotFoundException =
  _MatchServiceError cloudFormation "StackInstanceNotFoundException" .
  hasStatus 404


-- | You can't yet delete this stack set, because it still contains one or more stack instances. Delete all stack instances from the stack set before deleting the stack set.
--
--
_StackSetNotEmptyException :: AsError a => Getting (First ServiceError) a ServiceError
_StackSetNotEmptyException =
  _MatchServiceError cloudFormation "StackSetNotEmptyException" . hasStatus 409


-- | The specified operation isn't valid.
--
--
_InvalidOperationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidOperationException =
  _MatchServiceError cloudFormation "InvalidOperationException" . hasStatus 400


-- | The specified name is already in use.
--
--
_NameAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_NameAlreadyExistsException =
  _MatchServiceError cloudFormation "NameAlreadyExistsException" . hasStatus 409


-- | Another operation has been performed on this stack set since the specified operation was performed.
--
--
_StaleRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_StaleRequestException =
  _MatchServiceError cloudFormation "StaleRequestException" . hasStatus 409


-- | The resource with the name requested already exists.
--
--
_AlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_AlreadyExistsException =
  _MatchServiceError cloudFormation "AlreadyExistsException" . hasStatus 400


-- | The quota for the resource has already been reached.
--
--
-- For information on stack set limitations, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-limitations.html Limitations of StackSets> .
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
  _MatchServiceError cloudFormation "LimitExceededException" . hasStatus 400


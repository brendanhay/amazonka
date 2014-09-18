{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EMR.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Elastic MapReduce (Amazon EMR) is a web service that makes it easy
-- to process large amounts of data efficiently. Amazon EMR uses Hadoop
-- processing combined with several AWS products to do such tasks as web
-- indexing, data mining, log file analysis, machine learning, scientific
-- simulation, and data warehousing.
module Network.AWS.EMR.Types
    (
    -- * Service
      EMR
    -- ** Errors
    , EMRError (..)
    , _EMRClient
    , _EMRSerializer
    , _EMRService
    , _InternalServerError
    , _InternalServerException
    , _InvalidRequestException
    -- * ActionOnFailure
    , ActionOnFailure (..)

    -- * ClusterState
    , ClusterState (..)

    -- * ClusterStateChangeReasonCode
    , ClusterStateChangeReasonCode (..)

    -- * InstanceGroupState
    , InstanceGroupState (..)

    -- * InstanceGroupStateChangeReasonCode
    , InstanceGroupStateChangeReasonCode (..)

    -- * InstanceGroupType
    , InstanceGroupType (..)

    -- * InstanceRoleType
    , InstanceRoleType (..)

    -- * InstanceState
    , InstanceState (..)

    -- * InstanceStateChangeReasonCode
    , InstanceStateChangeReasonCode (..)

    -- * JobFlowExecutionState
    , JobFlowExecutionState (..)

    -- * MarketType
    , MarketType (..)

    -- * StepExecutionState
    , StepExecutionState (..)

    -- * StepState
    , StepState (..)

    -- * StepStateChangeReasonCode
    , StepStateChangeReasonCode (..)

    -- * BootstrapActionDetail
    , BootstrapActionDetail
    , mkBootstrapActionDetail
    , badBootstrapActionConfig

    -- * PlacementType
    , PlacementType
    , mkPlacementType
    , ptAvailabilityZone

    -- * Application
    , Application
    , mkApplication
    , aName
    , aVersion
    , aArgs
    , aAdditionalInfo

    -- * BootstrapActionConfig
    , BootstrapActionConfig
    , mkBootstrapActionConfig
    , bacName
    , bacScriptBootstrapAction

    -- * Cluster
    , Cluster
    , mkCluster
    , cId
    , cName
    , cStatus
    , cEc2InstanceAttributes
    , cLogUri
    , cRequestedAmiVersion
    , cRunningAmiVersion
    , cAutoTerminate
    , cTerminationProtected
    , cVisibleToAllUsers
    , cApplications
    , cTags
    , cServiceRole

    -- * ClusterStateChangeReason
    , ClusterStateChangeReason
    , mkClusterStateChangeReason
    , cscrCode
    , cscrMessage

    -- * ClusterStatus
    , ClusterStatus
    , mkClusterStatus
    , csState
    , csStateChangeReason
    , csTimeline

    -- * ClusterSummary
    , ClusterSummary
    , mkClusterSummary
    , csrId
    , csrName
    , csrStatus

    -- * ClusterTimeline
    , ClusterTimeline
    , mkClusterTimeline
    , ctCreationDateTime
    , ctReadyDateTime
    , ctEndDateTime

    -- * Command
    , Command
    , mkCommand
    , crName
    , crScriptPath
    , crArgs

    -- * Ec2InstanceAttributes
    , Ec2InstanceAttributes
    , mkEc2InstanceAttributes
    , eiaEc2KeyName
    , eiaEc2SubnetId
    , eiaEc2AvailabilityZone
    , eiaIamInstanceProfile

    -- * HadoopJarStepConfig
    , HadoopJarStepConfig
    , mkHadoopJarStepConfig
    , hjscProperties
    , hjscJar
    , hjscMainClass
    , hjscArgs

    -- * HadoopStepConfig
    , HadoopStepConfig
    , mkHadoopStepConfig
    , hscJar
    , hscProperties
    , hscMainClass
    , hscArgs

    -- * Instance
    , Instance
    , mkInstance
    , iId
    , iEc2InstanceId
    , iPublicDnsName
    , iPublicIpAddress
    , iPrivateDnsName
    , iPrivateIpAddress
    , iStatus

    -- * InstanceGroup
    , InstanceGroup
    , mkInstanceGroup
    , igId
    , igName
    , igMarket
    , igInstanceGroupType
    , igBidPrice
    , igInstanceType
    , igRequestedInstanceCount
    , igRunningInstanceCount
    , igStatus

    -- * InstanceGroupConfig
    , InstanceGroupConfig
    , mkInstanceGroupConfig
    , igcName
    , igcMarket
    , igcInstanceRole
    , igcBidPrice
    , igcInstanceType
    , igcInstanceCount

    -- * InstanceGroupDetail
    , InstanceGroupDetail
    , mkInstanceGroupDetail
    , igdInstanceGroupId
    , igdName
    , igdMarket
    , igdInstanceRole
    , igdBidPrice
    , igdInstanceType
    , igdInstanceRequestCount
    , igdInstanceRunningCount
    , igdState
    , igdLastStateChangeReason
    , igdCreationDateTime
    , igdStartDateTime
    , igdReadyDateTime
    , igdEndDateTime

    -- * InstanceGroupModifyConfig
    , InstanceGroupModifyConfig
    , mkInstanceGroupModifyConfig
    , igmcInstanceGroupId
    , igmcInstanceCount
    , igmcEC2InstanceIdsToTerminate

    -- * InstanceGroupStateChangeReason
    , InstanceGroupStateChangeReason
    , mkInstanceGroupStateChangeReason
    , igscrCode
    , igscrMessage

    -- * InstanceGroupStatus
    , InstanceGroupStatus
    , mkInstanceGroupStatus
    , igsState
    , igsStateChangeReason
    , igsTimeline

    -- * InstanceGroupTimeline
    , InstanceGroupTimeline
    , mkInstanceGroupTimeline
    , igtCreationDateTime
    , igtReadyDateTime
    , igtEndDateTime

    -- * InstanceStateChangeReason
    , InstanceStateChangeReason
    , mkInstanceStateChangeReason
    , iscrCode
    , iscrMessage

    -- * InstanceStatus
    , InstanceStatus
    , mkInstanceStatus
    , isState
    , isStateChangeReason
    , isTimeline

    -- * InstanceTimeline
    , InstanceTimeline
    , mkInstanceTimeline
    , itCreationDateTime
    , itReadyDateTime
    , itEndDateTime

    -- * JobFlowDetail
    , JobFlowDetail
    , mkJobFlowDetail
    , jfdJobFlowId
    , jfdName
    , jfdLogUri
    , jfdAmiVersion
    , jfdExecutionStatusDetail
    , jfdInstances
    , jfdSteps
    , jfdBootstrapActions
    , jfdSupportedProducts
    , jfdVisibleToAllUsers
    , jfdJobFlowRole
    , jfdServiceRole

    -- * JobFlowExecutionStatusDetail
    , JobFlowExecutionStatusDetail
    , mkJobFlowExecutionStatusDetail
    , jfesdState
    , jfesdCreationDateTime
    , jfesdStartDateTime
    , jfesdReadyDateTime
    , jfesdEndDateTime
    , jfesdLastStateChangeReason

    -- * JobFlowInstancesConfig
    , JobFlowInstancesConfig
    , mkJobFlowInstancesConfig
    , jficMasterInstanceType
    , jficSlaveInstanceType
    , jficInstanceCount
    , jficInstanceGroups
    , jficEc2KeyName
    , jficPlacement
    , jficKeepJobFlowAliveWhenNoSteps
    , jficTerminationProtected
    , jficHadoopVersion
    , jficEc2SubnetId

    -- * JobFlowInstancesDetail
    , JobFlowInstancesDetail
    , mkJobFlowInstancesDetail
    , jfidMasterInstanceType
    , jfidMasterPublicDnsName
    , jfidMasterInstanceId
    , jfidSlaveInstanceType
    , jfidInstanceCount
    , jfidInstanceGroups
    , jfidNormalizedInstanceHours
    , jfidEc2KeyName
    , jfidEc2SubnetId
    , jfidPlacement
    , jfidKeepJobFlowAliveWhenNoSteps
    , jfidTerminationProtected
    , jfidHadoopVersion

    -- * KeyValue
    , KeyValue
    , mkKeyValue
    , kvKey
    , kvValue

    -- * ScriptBootstrapActionConfig
    , ScriptBootstrapActionConfig
    , mkScriptBootstrapActionConfig
    , sbacPath
    , sbacArgs

    -- * Step
    , Step
    , mkStep
    , sId
    , sName
    , sConfig
    , sActionOnFailure
    , sStatus

    -- * StepConfig
    , StepConfig
    , mkStepConfig
    , scName
    , scActionOnFailure
    , scHadoopJarStep

    -- * StepDetail
    , StepDetail
    , mkStepDetail
    , sdStepConfig
    , sdExecutionStatusDetail

    -- * StepExecutionStatusDetail
    , StepExecutionStatusDetail
    , mkStepExecutionStatusDetail
    , sesdState
    , sesdCreationDateTime
    , sesdStartDateTime
    , sesdEndDateTime
    , sesdLastStateChangeReason

    -- * StepStateChangeReason
    , StepStateChangeReason
    , mkStepStateChangeReason
    , sscrCode
    , sscrMessage

    -- * StepStatus
    , StepStatus
    , mkStepStatus
    , ssState
    , ssStateChangeReason
    , ssTimeline

    -- * StepSummary
    , StepSummary
    , mkStepSummary
    , ssrId
    , ssrName
    , ssrStatus

    -- * StepTimeline
    , StepTimeline
    , mkStepTimeline
    , stCreationDateTime
    , stStartDateTime
    , stEndDateTime

    -- * SupportedProductConfig
    , SupportedProductConfig
    , mkSupportedProductConfig
    , spcName
    , spcArgs

    -- * Tag
    , Tag
    , mkTag
    , tKey
    , tValue
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2009-03-31@) of the
-- @Amazon Elastic MapReduce@ service.
data EMR deriving (Typeable)

instance AWSService EMR where
    type Sg EMR = V4
    type Er EMR = EMRError

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "elasticmapreduce"
        , _svcVersion  = "2009-03-31"
        , _svcTarget   = Nothing
        }

-- | A sum type representing possible errors returned by the 'EMR' service.
--
-- These typically include 'HTTPException's thrown by the underlying HTTP
-- mechanisms, serialisation errors, and typed errors as specified by the
-- service description where applicable.
data EMRError
    = EMRClient HttpException
    | EMRSerializer Text
    | EMRService Text
      -- | Indicates that an error occurred while processing the request and
      -- that the request was not completed.
    | InternalServerError
      -- | This exception occurs when there is an internal failure in the
      -- EMR service.
    | InternalServerException
        { _iseMessage :: Maybe Text
        }
      -- | This exception occurs when there is something wrong with user
      -- input.
    | InvalidRequestException
        { _ireErrorCode :: Maybe Text
        , _ireMessage :: Maybe Text
        }
    deriving (Show, Generic)

instance AWSError EMRError where
    awsError = const "EMRError"

instance AWSServiceError EMRError where
    serviceError    = EMRService
    clientError     = EMRClient
    serializerError = EMRSerializer

instance Exception EMRError

-- | See: 'EMRClient'
_EMRClient :: Prism' EMRError HttpException
_EMRClient = prism'
    EMRClient
    (\case
        EMRClient p1 -> Right p1
        x -> Left x)

-- | See: 'EMRSerializer'
_EMRSerializer :: Prism' EMRError Text
_EMRSerializer = prism'
    EMRSerializer
    (\case
        EMRSerializer p1 -> Right p1
        x -> Left x)

-- | See: 'EMRService'
_EMRService :: Prism' EMRError Text
_EMRService = prism'
    EMRService
    (\case
        EMRService p1 -> Right p1
        x -> Left x)

-- | Indicates that an error occurred while processing the request and that the
-- request was not completed.
--
-- See: 'InternalServerError'
_InternalServerError :: Prism' EMRError ()
_InternalServerError = prism'
    (const InternalServerError)
    (\case
        InternalServerError -> Right ()
        x -> Left x)

-- | This exception occurs when there is an internal failure in the EMR service.
--
-- See: 'InternalServerException'
_InternalServerException :: Prism' EMRError (Maybe Text)
_InternalServerException = prism'
    InternalServerException
    (\case
        InternalServerException p1 -> Right p1
        x -> Left x)

-- | This exception occurs when there is something wrong with user input.
--
-- See: 'InvalidRequestException'
_InvalidRequestException :: Prism' EMRError (Maybe Text, Maybe Text)
_InvalidRequestException = prism'
FIXME: Oh noes!

data ActionOnFailure
    = ActionOnFailureCancelAndWait -- ^ CANCEL_AND_WAIT
    | ActionOnFailureContinue -- ^ CONTINUE
    | ActionOnFailureTerminateCluster -- ^ TERMINATE_CLUSTER
    | ActionOnFailureTerminateJobFlow -- ^ TERMINATE_JOB_FLOW
      deriving (Eq, Show, Generic)

instance Hashable ActionOnFailure

instance FromText ActionOnFailure where
    parser = match "CANCEL_AND_WAIT" ActionOnFailureCancelAndWait
         <|> match "CONTINUE" ActionOnFailureContinue
         <|> match "TERMINATE_CLUSTER" ActionOnFailureTerminateCluster
         <|> match "TERMINATE_JOB_FLOW" ActionOnFailureTerminateJobFlow

instance ToText ActionOnFailure where
    toText ActionOnFailureCancelAndWait = "CANCEL_AND_WAIT"
    toText ActionOnFailureContinue = "CONTINUE"
    toText ActionOnFailureTerminateCluster = "TERMINATE_CLUSTER"
    toText ActionOnFailureTerminateJobFlow = "TERMINATE_JOB_FLOW"

instance ToByteString ActionOnFailure where
    toBS ActionOnFailureCancelAndWait = "CANCEL_AND_WAIT"
    toBS ActionOnFailureContinue = "CONTINUE"
    toBS ActionOnFailureTerminateCluster = "TERMINATE_CLUSTER"
    toBS ActionOnFailureTerminateJobFlow = "TERMINATE_JOB_FLOW"

instance ToHeader ActionOnFailure where
    toHeader k = toHeader k . toBS

instance ToQuery ActionOnFailure where
    toQuery = toQuery . toBS

instance FromJSON ActionOnFailure

instance ToJSON ActionOnFailure

data ClusterState
    = ClusterStateBootstrapping -- ^ BOOTSTRAPPING
    | ClusterStateRunning -- ^ RUNNING
    | ClusterStateStarting -- ^ STARTING
    | ClusterStateTerminated -- ^ TERMINATED
    | ClusterStateTerminatedWithErrors -- ^ TERMINATED_WITH_ERRORS
    | ClusterStateTerminating -- ^ TERMINATING
    | ClusterStateWaiting -- ^ WAITING
      deriving (Eq, Show, Generic)

instance Hashable ClusterState

instance FromText ClusterState where
    parser = match "BOOTSTRAPPING" ClusterStateBootstrapping
         <|> match "RUNNING" ClusterStateRunning
         <|> match "STARTING" ClusterStateStarting
         <|> match "TERMINATED" ClusterStateTerminated
         <|> match "TERMINATED_WITH_ERRORS" ClusterStateTerminatedWithErrors
         <|> match "TERMINATING" ClusterStateTerminating
         <|> match "WAITING" ClusterStateWaiting

instance ToText ClusterState where
    toText ClusterStateBootstrapping = "BOOTSTRAPPING"
    toText ClusterStateRunning = "RUNNING"
    toText ClusterStateStarting = "STARTING"
    toText ClusterStateTerminated = "TERMINATED"
    toText ClusterStateTerminatedWithErrors = "TERMINATED_WITH_ERRORS"
    toText ClusterStateTerminating = "TERMINATING"
    toText ClusterStateWaiting = "WAITING"

instance ToByteString ClusterState where
    toBS ClusterStateBootstrapping = "BOOTSTRAPPING"
    toBS ClusterStateRunning = "RUNNING"
    toBS ClusterStateStarting = "STARTING"
    toBS ClusterStateTerminated = "TERMINATED"
    toBS ClusterStateTerminatedWithErrors = "TERMINATED_WITH_ERRORS"
    toBS ClusterStateTerminating = "TERMINATING"
    toBS ClusterStateWaiting = "WAITING"

instance ToHeader ClusterState where
    toHeader k = toHeader k . toBS

instance ToQuery ClusterState where
    toQuery = toQuery . toBS

instance FromJSON ClusterState

instance ToJSON ClusterState

data ClusterStateChangeReasonCode
    = ClusterStateChangeReasonCodeAllStepsCompleted -- ^ ALL_STEPS_COMPLETED
    | ClusterStateChangeReasonCodeBootstrapFailure -- ^ BOOTSTRAP_FAILURE
    | ClusterStateChangeReasonCodeInstanceFailure -- ^ INSTANCE_FAILURE
    | ClusterStateChangeReasonCodeInternalError -- ^ INTERNAL_ERROR
    | ClusterStateChangeReasonCodeStepFailure -- ^ STEP_FAILURE
    | ClusterStateChangeReasonCodeUserRequest -- ^ USER_REQUEST
    | ClusterStateChangeReasonCodeValidationError -- ^ VALIDATION_ERROR
      deriving (Eq, Show, Generic)

instance Hashable ClusterStateChangeReasonCode

instance FromText ClusterStateChangeReasonCode where
    parser = match "ALL_STEPS_COMPLETED" ClusterStateChangeReasonCodeAllStepsCompleted
         <|> match "BOOTSTRAP_FAILURE" ClusterStateChangeReasonCodeBootstrapFailure
         <|> match "INSTANCE_FAILURE" ClusterStateChangeReasonCodeInstanceFailure
         <|> match "INTERNAL_ERROR" ClusterStateChangeReasonCodeInternalError
         <|> match "STEP_FAILURE" ClusterStateChangeReasonCodeStepFailure
         <|> match "USER_REQUEST" ClusterStateChangeReasonCodeUserRequest
         <|> match "VALIDATION_ERROR" ClusterStateChangeReasonCodeValidationError

instance ToText ClusterStateChangeReasonCode where
    toText ClusterStateChangeReasonCodeAllStepsCompleted = "ALL_STEPS_COMPLETED"
    toText ClusterStateChangeReasonCodeBootstrapFailure = "BOOTSTRAP_FAILURE"
    toText ClusterStateChangeReasonCodeInstanceFailure = "INSTANCE_FAILURE"
    toText ClusterStateChangeReasonCodeInternalError = "INTERNAL_ERROR"
    toText ClusterStateChangeReasonCodeStepFailure = "STEP_FAILURE"
    toText ClusterStateChangeReasonCodeUserRequest = "USER_REQUEST"
    toText ClusterStateChangeReasonCodeValidationError = "VALIDATION_ERROR"

instance ToByteString ClusterStateChangeReasonCode where
    toBS ClusterStateChangeReasonCodeAllStepsCompleted = "ALL_STEPS_COMPLETED"
    toBS ClusterStateChangeReasonCodeBootstrapFailure = "BOOTSTRAP_FAILURE"
    toBS ClusterStateChangeReasonCodeInstanceFailure = "INSTANCE_FAILURE"
    toBS ClusterStateChangeReasonCodeInternalError = "INTERNAL_ERROR"
    toBS ClusterStateChangeReasonCodeStepFailure = "STEP_FAILURE"
    toBS ClusterStateChangeReasonCodeUserRequest = "USER_REQUEST"
    toBS ClusterStateChangeReasonCodeValidationError = "VALIDATION_ERROR"

instance ToHeader ClusterStateChangeReasonCode where
    toHeader k = toHeader k . toBS

instance ToQuery ClusterStateChangeReasonCode where
    toQuery = toQuery . toBS

instance FromJSON ClusterStateChangeReasonCode

instance ToJSON ClusterStateChangeReasonCode

data InstanceGroupState
    = InstanceGroupStateArrested -- ^ ARRESTED
    | InstanceGroupStateBootstrapping -- ^ BOOTSTRAPPING
    | InstanceGroupStateEnded -- ^ ENDED
    | InstanceGroupStateProvisioning -- ^ PROVISIONING
    | InstanceGroupStateResizing -- ^ RESIZING
    | InstanceGroupStateRunning -- ^ RUNNING
    | InstanceGroupStateShuttingDown -- ^ SHUTTING_DOWN
    | InstanceGroupStateSuspended -- ^ SUSPENDED
    | InstanceGroupStateTerminated -- ^ TERMINATED
    | InstanceGroupStateTerminating -- ^ TERMINATING
      deriving (Eq, Show, Generic)

instance Hashable InstanceGroupState

instance FromText InstanceGroupState where
    parser = match "ARRESTED" InstanceGroupStateArrested
         <|> match "BOOTSTRAPPING" InstanceGroupStateBootstrapping
         <|> match "ENDED" InstanceGroupStateEnded
         <|> match "PROVISIONING" InstanceGroupStateProvisioning
         <|> match "RESIZING" InstanceGroupStateResizing
         <|> match "RUNNING" InstanceGroupStateRunning
         <|> match "SHUTTING_DOWN" InstanceGroupStateShuttingDown
         <|> match "SUSPENDED" InstanceGroupStateSuspended
         <|> match "TERMINATED" InstanceGroupStateTerminated
         <|> match "TERMINATING" InstanceGroupStateTerminating

instance ToText InstanceGroupState where
    toText InstanceGroupStateArrested = "ARRESTED"
    toText InstanceGroupStateBootstrapping = "BOOTSTRAPPING"
    toText InstanceGroupStateEnded = "ENDED"
    toText InstanceGroupStateProvisioning = "PROVISIONING"
    toText InstanceGroupStateResizing = "RESIZING"
    toText InstanceGroupStateRunning = "RUNNING"
    toText InstanceGroupStateShuttingDown = "SHUTTING_DOWN"
    toText InstanceGroupStateSuspended = "SUSPENDED"
    toText InstanceGroupStateTerminated = "TERMINATED"
    toText InstanceGroupStateTerminating = "TERMINATING"

instance ToByteString InstanceGroupState where
    toBS InstanceGroupStateArrested = "ARRESTED"
    toBS InstanceGroupStateBootstrapping = "BOOTSTRAPPING"
    toBS InstanceGroupStateEnded = "ENDED"
    toBS InstanceGroupStateProvisioning = "PROVISIONING"
    toBS InstanceGroupStateResizing = "RESIZING"
    toBS InstanceGroupStateRunning = "RUNNING"
    toBS InstanceGroupStateShuttingDown = "SHUTTING_DOWN"
    toBS InstanceGroupStateSuspended = "SUSPENDED"
    toBS InstanceGroupStateTerminated = "TERMINATED"
    toBS InstanceGroupStateTerminating = "TERMINATING"

instance ToHeader InstanceGroupState where
    toHeader k = toHeader k . toBS

instance ToQuery InstanceGroupState where
    toQuery = toQuery . toBS

instance FromJSON InstanceGroupState

instance ToJSON InstanceGroupState

data InstanceGroupStateChangeReasonCode
    = InstanceGroupStateChangeReasonCodeClusterTerminated -- ^ CLUSTER_TERMINATED
    | InstanceGroupStateChangeReasonCodeInstanceFailure -- ^ INSTANCE_FAILURE
    | InstanceGroupStateChangeReasonCodeInternalError -- ^ INTERNAL_ERROR
    | InstanceGroupStateChangeReasonCodeValidationError -- ^ VALIDATION_ERROR
      deriving (Eq, Show, Generic)

instance Hashable InstanceGroupStateChangeReasonCode

instance FromText InstanceGroupStateChangeReasonCode where
    parser = match "CLUSTER_TERMINATED" InstanceGroupStateChangeReasonCodeClusterTerminated
         <|> match "INSTANCE_FAILURE" InstanceGroupStateChangeReasonCodeInstanceFailure
         <|> match "INTERNAL_ERROR" InstanceGroupStateChangeReasonCodeInternalError
         <|> match "VALIDATION_ERROR" InstanceGroupStateChangeReasonCodeValidationError

instance ToText InstanceGroupStateChangeReasonCode where
    toText InstanceGroupStateChangeReasonCodeClusterTerminated = "CLUSTER_TERMINATED"
    toText InstanceGroupStateChangeReasonCodeInstanceFailure = "INSTANCE_FAILURE"
    toText InstanceGroupStateChangeReasonCodeInternalError = "INTERNAL_ERROR"
    toText InstanceGroupStateChangeReasonCodeValidationError = "VALIDATION_ERROR"

instance ToByteString InstanceGroupStateChangeReasonCode where
    toBS InstanceGroupStateChangeReasonCodeClusterTerminated = "CLUSTER_TERMINATED"
    toBS InstanceGroupStateChangeReasonCodeInstanceFailure = "INSTANCE_FAILURE"
    toBS InstanceGroupStateChangeReasonCodeInternalError = "INTERNAL_ERROR"
    toBS InstanceGroupStateChangeReasonCodeValidationError = "VALIDATION_ERROR"

instance ToHeader InstanceGroupStateChangeReasonCode where
    toHeader k = toHeader k . toBS

instance ToQuery InstanceGroupStateChangeReasonCode where
    toQuery = toQuery . toBS

instance FromJSON InstanceGroupStateChangeReasonCode

instance ToJSON InstanceGroupStateChangeReasonCode

data InstanceGroupType
    = InstanceGroupTypeCore -- ^ CORE
    | InstanceGroupTypeMaster -- ^ MASTER
    | InstanceGroupTypeTask -- ^ TASK
      deriving (Eq, Show, Generic)

instance Hashable InstanceGroupType

instance FromText InstanceGroupType where
    parser = match "CORE" InstanceGroupTypeCore
         <|> match "MASTER" InstanceGroupTypeMaster
         <|> match "TASK" InstanceGroupTypeTask

instance ToText InstanceGroupType where
    toText InstanceGroupTypeCore = "CORE"
    toText InstanceGroupTypeMaster = "MASTER"
    toText InstanceGroupTypeTask = "TASK"

instance ToByteString InstanceGroupType where
    toBS InstanceGroupTypeCore = "CORE"
    toBS InstanceGroupTypeMaster = "MASTER"
    toBS InstanceGroupTypeTask = "TASK"

instance ToHeader InstanceGroupType where
    toHeader k = toHeader k . toBS

instance ToQuery InstanceGroupType where
    toQuery = toQuery . toBS

instance FromJSON InstanceGroupType

instance ToJSON InstanceGroupType

data InstanceRoleType
    = InstanceRoleTypeCore -- ^ CORE
    | InstanceRoleTypeMaster -- ^ MASTER
    | InstanceRoleTypeTask -- ^ TASK
      deriving (Eq, Show, Generic)

instance Hashable InstanceRoleType

instance FromText InstanceRoleType where
    parser = match "CORE" InstanceRoleTypeCore
         <|> match "MASTER" InstanceRoleTypeMaster
         <|> match "TASK" InstanceRoleTypeTask

instance ToText InstanceRoleType where
    toText InstanceRoleTypeCore = "CORE"
    toText InstanceRoleTypeMaster = "MASTER"
    toText InstanceRoleTypeTask = "TASK"

instance ToByteString InstanceRoleType where
    toBS InstanceRoleTypeCore = "CORE"
    toBS InstanceRoleTypeMaster = "MASTER"
    toBS InstanceRoleTypeTask = "TASK"

instance ToHeader InstanceRoleType where
    toHeader k = toHeader k . toBS

instance ToQuery InstanceRoleType where
    toQuery = toQuery . toBS

instance FromJSON InstanceRoleType

instance ToJSON InstanceRoleType

data InstanceState
    = InstanceStateAwaitingFulfillment -- ^ AWAITING_FULFILLMENT
    | InstanceStateBootstrapping -- ^ BOOTSTRAPPING
    | InstanceStateProvisioning -- ^ PROVISIONING
    | InstanceStateRunning -- ^ RUNNING
    | InstanceStateTerminated -- ^ TERMINATED
      deriving (Eq, Show, Generic)

instance Hashable InstanceState

instance FromText InstanceState where
    parser = match "AWAITING_FULFILLMENT" InstanceStateAwaitingFulfillment
         <|> match "BOOTSTRAPPING" InstanceStateBootstrapping
         <|> match "PROVISIONING" InstanceStateProvisioning
         <|> match "RUNNING" InstanceStateRunning
         <|> match "TERMINATED" InstanceStateTerminated

instance ToText InstanceState where
    toText InstanceStateAwaitingFulfillment = "AWAITING_FULFILLMENT"
    toText InstanceStateBootstrapping = "BOOTSTRAPPING"
    toText InstanceStateProvisioning = "PROVISIONING"
    toText InstanceStateRunning = "RUNNING"
    toText InstanceStateTerminated = "TERMINATED"

instance ToByteString InstanceState where
    toBS InstanceStateAwaitingFulfillment = "AWAITING_FULFILLMENT"
    toBS InstanceStateBootstrapping = "BOOTSTRAPPING"
    toBS InstanceStateProvisioning = "PROVISIONING"
    toBS InstanceStateRunning = "RUNNING"
    toBS InstanceStateTerminated = "TERMINATED"

instance ToHeader InstanceState where
    toHeader k = toHeader k . toBS

instance ToQuery InstanceState where
    toQuery = toQuery . toBS

instance FromJSON InstanceState

instance ToJSON InstanceState

data InstanceStateChangeReasonCode
    = InstanceStateChangeReasonCodeBootstrapFailure -- ^ BOOTSTRAP_FAILURE
    | InstanceStateChangeReasonCodeClusterTerminated -- ^ CLUSTER_TERMINATED
    | InstanceStateChangeReasonCodeInstanceFailure -- ^ INSTANCE_FAILURE
    | InstanceStateChangeReasonCodeInternalError -- ^ INTERNAL_ERROR
    | InstanceStateChangeReasonCodeValidationError -- ^ VALIDATION_ERROR
      deriving (Eq, Show, Generic)

instance Hashable InstanceStateChangeReasonCode

instance FromText InstanceStateChangeReasonCode where
    parser = match "BOOTSTRAP_FAILURE" InstanceStateChangeReasonCodeBootstrapFailure
         <|> match "CLUSTER_TERMINATED" InstanceStateChangeReasonCodeClusterTerminated
         <|> match "INSTANCE_FAILURE" InstanceStateChangeReasonCodeInstanceFailure
         <|> match "INTERNAL_ERROR" InstanceStateChangeReasonCodeInternalError
         <|> match "VALIDATION_ERROR" InstanceStateChangeReasonCodeValidationError

instance ToText InstanceStateChangeReasonCode where
    toText InstanceStateChangeReasonCodeBootstrapFailure = "BOOTSTRAP_FAILURE"
    toText InstanceStateChangeReasonCodeClusterTerminated = "CLUSTER_TERMINATED"
    toText InstanceStateChangeReasonCodeInstanceFailure = "INSTANCE_FAILURE"
    toText InstanceStateChangeReasonCodeInternalError = "INTERNAL_ERROR"
    toText InstanceStateChangeReasonCodeValidationError = "VALIDATION_ERROR"

instance ToByteString InstanceStateChangeReasonCode where
    toBS InstanceStateChangeReasonCodeBootstrapFailure = "BOOTSTRAP_FAILURE"
    toBS InstanceStateChangeReasonCodeClusterTerminated = "CLUSTER_TERMINATED"
    toBS InstanceStateChangeReasonCodeInstanceFailure = "INSTANCE_FAILURE"
    toBS InstanceStateChangeReasonCodeInternalError = "INTERNAL_ERROR"
    toBS InstanceStateChangeReasonCodeValidationError = "VALIDATION_ERROR"

instance ToHeader InstanceStateChangeReasonCode where
    toHeader k = toHeader k . toBS

instance ToQuery InstanceStateChangeReasonCode where
    toQuery = toQuery . toBS

instance FromJSON InstanceStateChangeReasonCode

instance ToJSON InstanceStateChangeReasonCode

data JobFlowExecutionState
    = JobFlowExecutionStateBootstrapping -- ^ BOOTSTRAPPING
    | JobFlowExecutionStateCompleted -- ^ COMPLETED
    | JobFlowExecutionStateFailed -- ^ FAILED
    | JobFlowExecutionStateRunning -- ^ RUNNING
    | JobFlowExecutionStateShuttingDown -- ^ SHUTTING_DOWN
    | JobFlowExecutionStateStarting -- ^ STARTING
    | JobFlowExecutionStateTerminated -- ^ TERMINATED
    | JobFlowExecutionStateWaiting -- ^ WAITING
      deriving (Eq, Show, Generic)

instance Hashable JobFlowExecutionState

instance FromText JobFlowExecutionState where
    parser = match "BOOTSTRAPPING" JobFlowExecutionStateBootstrapping
         <|> match "COMPLETED" JobFlowExecutionStateCompleted
         <|> match "FAILED" JobFlowExecutionStateFailed
         <|> match "RUNNING" JobFlowExecutionStateRunning
         <|> match "SHUTTING_DOWN" JobFlowExecutionStateShuttingDown
         <|> match "STARTING" JobFlowExecutionStateStarting
         <|> match "TERMINATED" JobFlowExecutionStateTerminated
         <|> match "WAITING" JobFlowExecutionStateWaiting

instance ToText JobFlowExecutionState where
    toText JobFlowExecutionStateBootstrapping = "BOOTSTRAPPING"
    toText JobFlowExecutionStateCompleted = "COMPLETED"
    toText JobFlowExecutionStateFailed = "FAILED"
    toText JobFlowExecutionStateRunning = "RUNNING"
    toText JobFlowExecutionStateShuttingDown = "SHUTTING_DOWN"
    toText JobFlowExecutionStateStarting = "STARTING"
    toText JobFlowExecutionStateTerminated = "TERMINATED"
    toText JobFlowExecutionStateWaiting = "WAITING"

instance ToByteString JobFlowExecutionState where
    toBS JobFlowExecutionStateBootstrapping = "BOOTSTRAPPING"
    toBS JobFlowExecutionStateCompleted = "COMPLETED"
    toBS JobFlowExecutionStateFailed = "FAILED"
    toBS JobFlowExecutionStateRunning = "RUNNING"
    toBS JobFlowExecutionStateShuttingDown = "SHUTTING_DOWN"
    toBS JobFlowExecutionStateStarting = "STARTING"
    toBS JobFlowExecutionStateTerminated = "TERMINATED"
    toBS JobFlowExecutionStateWaiting = "WAITING"

instance ToHeader JobFlowExecutionState where
    toHeader k = toHeader k . toBS

instance ToQuery JobFlowExecutionState where
    toQuery = toQuery . toBS

instance FromJSON JobFlowExecutionState

instance ToJSON JobFlowExecutionState

data MarketType
    = MarketTypeOnDemand -- ^ ON_DEMAND
    | MarketTypeSpot -- ^ SPOT
      deriving (Eq, Show, Generic)

instance Hashable MarketType

instance FromText MarketType where
    parser = match "ON_DEMAND" MarketTypeOnDemand
         <|> match "SPOT" MarketTypeSpot

instance ToText MarketType where
    toText MarketTypeOnDemand = "ON_DEMAND"
    toText MarketTypeSpot = "SPOT"

instance ToByteString MarketType where
    toBS MarketTypeOnDemand = "ON_DEMAND"
    toBS MarketTypeSpot = "SPOT"

instance ToHeader MarketType where
    toHeader k = toHeader k . toBS

instance ToQuery MarketType where
    toQuery = toQuery . toBS

instance FromJSON MarketType

instance ToJSON MarketType

data StepExecutionState
    = StepExecutionStateCancelled -- ^ CANCELLED
    | StepExecutionStateCompleted -- ^ COMPLETED
    | StepExecutionStateContinue -- ^ CONTINUE
    | StepExecutionStateFailed -- ^ FAILED
    | StepExecutionStateInterrupted -- ^ INTERRUPTED
    | StepExecutionStatePending -- ^ PENDING
    | StepExecutionStateRunning -- ^ RUNNING
      deriving (Eq, Show, Generic)

instance Hashable StepExecutionState

instance FromText StepExecutionState where
    parser = match "CANCELLED" StepExecutionStateCancelled
         <|> match "COMPLETED" StepExecutionStateCompleted
         <|> match "CONTINUE" StepExecutionStateContinue
         <|> match "FAILED" StepExecutionStateFailed
         <|> match "INTERRUPTED" StepExecutionStateInterrupted
         <|> match "PENDING" StepExecutionStatePending
         <|> match "RUNNING" StepExecutionStateRunning

instance ToText StepExecutionState where
    toText StepExecutionStateCancelled = "CANCELLED"
    toText StepExecutionStateCompleted = "COMPLETED"
    toText StepExecutionStateContinue = "CONTINUE"
    toText StepExecutionStateFailed = "FAILED"
    toText StepExecutionStateInterrupted = "INTERRUPTED"
    toText StepExecutionStatePending = "PENDING"
    toText StepExecutionStateRunning = "RUNNING"

instance ToByteString StepExecutionState where
    toBS StepExecutionStateCancelled = "CANCELLED"
    toBS StepExecutionStateCompleted = "COMPLETED"
    toBS StepExecutionStateContinue = "CONTINUE"
    toBS StepExecutionStateFailed = "FAILED"
    toBS StepExecutionStateInterrupted = "INTERRUPTED"
    toBS StepExecutionStatePending = "PENDING"
    toBS StepExecutionStateRunning = "RUNNING"

instance ToHeader StepExecutionState where
    toHeader k = toHeader k . toBS

instance ToQuery StepExecutionState where
    toQuery = toQuery . toBS

instance FromJSON StepExecutionState

instance ToJSON StepExecutionState

data StepState
    = StepStateCancelled -- ^ CANCELLED
    | StepStateCompleted -- ^ COMPLETED
    | StepStateFailed -- ^ FAILED
    | StepStateInterrupted -- ^ INTERRUPTED
    | StepStatePending -- ^ PENDING
    | StepStateRunning -- ^ RUNNING
      deriving (Eq, Show, Generic)

instance Hashable StepState

instance FromText StepState where
    parser = match "CANCELLED" StepStateCancelled
         <|> match "COMPLETED" StepStateCompleted
         <|> match "FAILED" StepStateFailed
         <|> match "INTERRUPTED" StepStateInterrupted
         <|> match "PENDING" StepStatePending
         <|> match "RUNNING" StepStateRunning

instance ToText StepState where
    toText StepStateCancelled = "CANCELLED"
    toText StepStateCompleted = "COMPLETED"
    toText StepStateFailed = "FAILED"
    toText StepStateInterrupted = "INTERRUPTED"
    toText StepStatePending = "PENDING"
    toText StepStateRunning = "RUNNING"

instance ToByteString StepState where
    toBS StepStateCancelled = "CANCELLED"
    toBS StepStateCompleted = "COMPLETED"
    toBS StepStateFailed = "FAILED"
    toBS StepStateInterrupted = "INTERRUPTED"
    toBS StepStatePending = "PENDING"
    toBS StepStateRunning = "RUNNING"

instance ToHeader StepState where
    toHeader k = toHeader k . toBS

instance ToQuery StepState where
    toQuery = toQuery . toBS

instance FromJSON StepState

instance ToJSON StepState

data StepStateChangeReasonCode
    = StepStateChangeReasonCodeNone -- ^ NONE
      deriving (Eq, Show, Generic)

instance Hashable StepStateChangeReasonCode

instance FromText StepStateChangeReasonCode where
    parser = match "NONE" StepStateChangeReasonCodeNone

instance ToText StepStateChangeReasonCode where
    toText StepStateChangeReasonCodeNone = "NONE"

instance ToByteString StepStateChangeReasonCode where
    toBS StepStateChangeReasonCodeNone = "NONE"

instance ToHeader StepStateChangeReasonCode where
    toHeader k = toHeader k . toBS

instance ToQuery StepStateChangeReasonCode where
    toQuery = toQuery . toBS

instance FromJSON StepStateChangeReasonCode

instance ToJSON StepStateChangeReasonCode

-- | Reports the configuration of a bootstrap action in a job flow.
newtype BootstrapActionDetail = BootstrapActionDetail
    { _badBootstrapActionConfig :: Maybe BootstrapActionConfig
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'BootstrapActionDetail' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @BootstrapActionConfig ::@ @Maybe BootstrapActionConfig@
--
mkBootstrapActionDetail :: BootstrapActionDetail
mkBootstrapActionDetail = BootstrapActionDetail
    { _badBootstrapActionConfig = Nothing
    }

-- | A description of the bootstrap action.
badBootstrapActionConfig :: Lens' BootstrapActionDetail (Maybe BootstrapActionConfig)
badBootstrapActionConfig =
    lens _badBootstrapActionConfig
         (\s a -> s { _badBootstrapActionConfig = a })

instance FromJSON BootstrapActionDetail

instance ToJSON BootstrapActionDetail

-- | The Amazon EC2 Availability Zone for the job flow.
newtype PlacementType = PlacementType
    { _ptAvailabilityZone :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PlacementType' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AvailabilityZone ::@ @Text@
--
mkPlacementType :: Text -- ^ 'ptAvailabilityZone'
                -> PlacementType
mkPlacementType p1 = PlacementType
    { _ptAvailabilityZone = p1
    }

-- | The Amazon EC2 Availability Zone for the job flow.
ptAvailabilityZone :: Lens' PlacementType Text
ptAvailabilityZone =
    lens _ptAvailabilityZone (\s a -> s { _ptAvailabilityZone = a })

instance FromJSON PlacementType

instance ToJSON PlacementType

-- | An application is any Amazon or third-party software that you can add to
-- the cluster. This structure contains a list of strings that indicates the
-- software to use with the cluster and accepts a user argument list. Amazon
-- EMR accepts and forwards the argument list to the corresponding
-- installation script as bootstrap action argument. For more information, see
-- Launch a Job Flow on the MapR Distribution for Hadoop. Currently supported
-- values are: "mapr-m3" - launch the job flow using MapR M3 Edition.
-- "mapr-m5" - launch the job flow using MapR M5 Edition. "mapr" with the user
-- arguments specifying "--edition,m3" or "--edition,m5" - launch the job flow
-- using MapR M3 or M5 Edition, respectively.
data Application = Application
    { _aName :: Maybe Text
    , _aVersion :: Maybe Text
    , _aArgs :: [Text]
    , _aAdditionalInfo :: Map Text Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Application' data type.
--
-- 'Application' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Maybe Text@
--
-- * @Version ::@ @Maybe Text@
--
-- * @Args ::@ @[Text]@
--
-- * @AdditionalInfo ::@ @Map Text Text@
--
mkApplication :: Application
mkApplication = Application
    { _aName = Nothing
    , _aVersion = Nothing
    , _aArgs = mempty
    , _aAdditionalInfo = mempty
    }

-- | The name of the application.
aName :: Lens' Application (Maybe Text)
aName = lens _aName (\s a -> s { _aName = a })

-- | The version of the application.
aVersion :: Lens' Application (Maybe Text)
aVersion = lens _aVersion (\s a -> s { _aVersion = a })

-- | Arguments for Amazon EMR to pass to the application.
aArgs :: Lens' Application [Text]
aArgs = lens _aArgs (\s a -> s { _aArgs = a })

-- | This option is for advanced users only. This is meta information about
-- third-party applications that third-party vendors use for testing purposes.
aAdditionalInfo :: Lens' Application (Map Text Text)
aAdditionalInfo = lens _aAdditionalInfo (\s a -> s { _aAdditionalInfo = a })

instance FromJSON Application

-- | A description of the bootstrap action.
data BootstrapActionConfig = BootstrapActionConfig
    { _bacName :: Text
    , _bacScriptBootstrapAction :: ScriptBootstrapActionConfig
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'BootstrapActionConfig' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Text@
--
-- * @ScriptBootstrapAction ::@ @ScriptBootstrapActionConfig@
--
mkBootstrapActionConfig :: Text -- ^ 'bacName'
                        -> ScriptBootstrapActionConfig -- ^ 'bacScriptBootstrapAction'
                        -> BootstrapActionConfig
mkBootstrapActionConfig p1 p2 = BootstrapActionConfig
    { _bacName = p1
    , _bacScriptBootstrapAction = p2
    }

-- | The name of the bootstrap action.
bacName :: Lens' BootstrapActionConfig Text
bacName = lens _bacName (\s a -> s { _bacName = a })

-- | The script run by the bootstrap action.
bacScriptBootstrapAction :: Lens' BootstrapActionConfig ScriptBootstrapActionConfig
bacScriptBootstrapAction =
    lens _bacScriptBootstrapAction
         (\s a -> s { _bacScriptBootstrapAction = a })

instance FromJSON BootstrapActionConfig

instance ToJSON BootstrapActionConfig

-- | This output contains the details for the requested cluster.
data Cluster = Cluster
    { _cId :: Maybe Text
    , _cName :: Maybe Text
    , _cStatus :: Maybe ClusterStatus
    , _cEc2InstanceAttributes :: Maybe Ec2InstanceAttributes
    , _cLogUri :: Maybe Text
    , _cRequestedAmiVersion :: Maybe Text
    , _cRunningAmiVersion :: Maybe Text
    , _cAutoTerminate :: Maybe Bool
    , _cTerminationProtected :: Maybe Bool
    , _cVisibleToAllUsers :: Maybe Bool
    , _cApplications :: [Application]
    , _cTags :: [Tag]
    , _cServiceRole :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Cluster' data type.
--
-- 'Cluster' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Id ::@ @Maybe Text@
--
-- * @Name ::@ @Maybe Text@
--
-- * @Status ::@ @Maybe ClusterStatus@
--
-- * @Ec2InstanceAttributes ::@ @Maybe Ec2InstanceAttributes@
--
-- * @LogUri ::@ @Maybe Text@
--
-- * @RequestedAmiVersion ::@ @Maybe Text@
--
-- * @RunningAmiVersion ::@ @Maybe Text@
--
-- * @AutoTerminate ::@ @Maybe Bool@
--
-- * @TerminationProtected ::@ @Maybe Bool@
--
-- * @VisibleToAllUsers ::@ @Maybe Bool@
--
-- * @Applications ::@ @[Application]@
--
-- * @Tags ::@ @[Tag]@
--
-- * @ServiceRole ::@ @Maybe Text@
--
mkCluster :: Cluster
mkCluster = Cluster
    { _cId = Nothing
    , _cName = Nothing
    , _cStatus = Nothing
    , _cEc2InstanceAttributes = Nothing
    , _cLogUri = Nothing
    , _cRequestedAmiVersion = Nothing
    , _cRunningAmiVersion = Nothing
    , _cAutoTerminate = Nothing
    , _cTerminationProtected = Nothing
    , _cVisibleToAllUsers = Nothing
    , _cApplications = mempty
    , _cTags = mempty
    , _cServiceRole = Nothing
    }

-- | The unique identifier for the cluster.
cId :: Lens' Cluster (Maybe Text)
cId = lens _cId (\s a -> s { _cId = a })

-- | The name of the cluster.
cName :: Lens' Cluster (Maybe Text)
cName = lens _cName (\s a -> s { _cName = a })

-- | The current status details about the cluster.
cStatus :: Lens' Cluster (Maybe ClusterStatus)
cStatus = lens _cStatus (\s a -> s { _cStatus = a })

-- | Provides information about the EC2 instances in a cluster grouped by
-- category. For example, key name, subnet ID, IAM instance profile, and so
-- on.
cEc2InstanceAttributes :: Lens' Cluster (Maybe Ec2InstanceAttributes)
cEc2InstanceAttributes =
    lens _cEc2InstanceAttributes (\s a -> s { _cEc2InstanceAttributes = a })

-- | The path to the Amazon S3 location where logs for this cluster are stored.
cLogUri :: Lens' Cluster (Maybe Text)
cLogUri = lens _cLogUri (\s a -> s { _cLogUri = a })

-- | The AMI version requested for this cluster.JobFlowDetail$AmiVersion.-->.
cRequestedAmiVersion :: Lens' Cluster (Maybe Text)
cRequestedAmiVersion =
    lens _cRequestedAmiVersion (\s a -> s { _cRequestedAmiVersion = a })

-- | The AMI version running on this cluster. This differs from the requested
-- version only if the requested version is a meta version, such as "latest".
-- JobFlowDetail$AmiVersion.-->.
cRunningAmiVersion :: Lens' Cluster (Maybe Text)
cRunningAmiVersion =
    lens _cRunningAmiVersion (\s a -> s { _cRunningAmiVersion = a })

-- | Specifies whether the cluster should terminate after completing all steps.
cAutoTerminate :: Lens' Cluster (Maybe Bool)
cAutoTerminate = lens _cAutoTerminate (\s a -> s { _cAutoTerminate = a })

-- | Indicates whether Amazon EMR will lock the cluster to prevent the EC2
-- instances from being terminated by an API call or user intervention, or in
-- the event of a cluster error.
cTerminationProtected :: Lens' Cluster (Maybe Bool)
cTerminationProtected =
    lens _cTerminationProtected (\s a -> s { _cTerminationProtected = a })

-- | Indicates whether the job flow is visible to all IAM users of the AWS
-- account associated with the job flow. If this value is set to true, all IAM
-- users of that AWS account can view and manage the job flow if they have the
-- proper policy permissions set. If this value is false, only the IAM user
-- that created the cluster can view and manage it. This value can be changed
-- using the SetVisibleToAllUsers action.
cVisibleToAllUsers :: Lens' Cluster (Maybe Bool)
cVisibleToAllUsers =
    lens _cVisibleToAllUsers (\s a -> s { _cVisibleToAllUsers = a })

-- | The applications installed on this cluster.
cApplications :: Lens' Cluster [Application]
cApplications = lens _cApplications (\s a -> s { _cApplications = a })

-- | A list of tags associated with a cluster.
cTags :: Lens' Cluster [Tag]
cTags = lens _cTags (\s a -> s { _cTags = a })

-- | The IAM role that will be assumed by the Amazon EMR service to access AWS
-- resources on your behalf.
cServiceRole :: Lens' Cluster (Maybe Text)
cServiceRole = lens _cServiceRole (\s a -> s { _cServiceRole = a })

instance FromJSON Cluster

-- | The reason for the cluster status change.
data ClusterStateChangeReason = ClusterStateChangeReason
    { _cscrCode :: Maybe ClusterStateChangeReasonCode
    , _cscrMessage :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ClusterStateChangeReason' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Code ::@ @Maybe ClusterStateChangeReasonCode@
--
-- * @Message ::@ @Maybe Text@
--
mkClusterStateChangeReason :: ClusterStateChangeReason
mkClusterStateChangeReason = ClusterStateChangeReason
    { _cscrCode = Nothing
    , _cscrMessage = Nothing
    }

-- | The programmatic code for the state change reason.
cscrCode :: Lens' ClusterStateChangeReason (Maybe ClusterStateChangeReasonCode)
cscrCode = lens _cscrCode (\s a -> s { _cscrCode = a })

-- | The descriptive message for the state change reason.
cscrMessage :: Lens' ClusterStateChangeReason (Maybe Text)
cscrMessage = lens _cscrMessage (\s a -> s { _cscrMessage = a })

instance FromJSON ClusterStateChangeReason

instance ToJSON ClusterStateChangeReason

-- | The current status details about the cluster.
data ClusterStatus = ClusterStatus
    { _csState :: Maybe ClusterState
    , _csStateChangeReason :: Maybe ClusterStateChangeReason
    , _csTimeline :: Maybe ClusterTimeline
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ClusterStatus' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @State ::@ @Maybe ClusterState@
--
-- * @StateChangeReason ::@ @Maybe ClusterStateChangeReason@
--
-- * @Timeline ::@ @Maybe ClusterTimeline@
--
mkClusterStatus :: ClusterStatus
mkClusterStatus = ClusterStatus
    { _csState = Nothing
    , _csStateChangeReason = Nothing
    , _csTimeline = Nothing
    }

-- | The current state of the cluster.
csState :: Lens' ClusterStatus (Maybe ClusterState)
csState = lens _csState (\s a -> s { _csState = a })

-- | The reason for the cluster status change.
csStateChangeReason :: Lens' ClusterStatus (Maybe ClusterStateChangeReason)
csStateChangeReason =
    lens _csStateChangeReason (\s a -> s { _csStateChangeReason = a })

-- | A timeline that represents the status of a cluster over the lifetime of the
-- cluster.
csTimeline :: Lens' ClusterStatus (Maybe ClusterTimeline)
csTimeline = lens _csTimeline (\s a -> s { _csTimeline = a })

instance FromJSON ClusterStatus

instance ToJSON ClusterStatus

-- | The summary description of the cluster.
data ClusterSummary = ClusterSummary
    { _csrId :: Maybe Text
    , _csrName :: Maybe Text
    , _csrStatus :: Maybe ClusterStatus
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ClusterSummary' data type.
--
-- 'ClusterSummary' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Id ::@ @Maybe Text@
--
-- * @Name ::@ @Maybe Text@
--
-- * @Status ::@ @Maybe ClusterStatus@
--
mkClusterSummary :: ClusterSummary
mkClusterSummary = ClusterSummary
    { _csrId = Nothing
    , _csrName = Nothing
    , _csrStatus = Nothing
    }

-- | The unique identifier for the cluster.
csrId :: Lens' ClusterSummary (Maybe Text)
csrId = lens _csrId (\s a -> s { _csrId = a })

-- | The name of the cluster.
csrName :: Lens' ClusterSummary (Maybe Text)
csrName = lens _csrName (\s a -> s { _csrName = a })

-- | The details about the current status of the cluster.
csrStatus :: Lens' ClusterSummary (Maybe ClusterStatus)
csrStatus = lens _csrStatus (\s a -> s { _csrStatus = a })

instance FromJSON ClusterSummary

-- | A timeline that represents the status of a cluster over the lifetime of the
-- cluster.
data ClusterTimeline = ClusterTimeline
    { _ctCreationDateTime :: Maybe POSIX
    , _ctReadyDateTime :: Maybe POSIX
    , _ctEndDateTime :: Maybe POSIX
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ClusterTimeline' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CreationDateTime ::@ @Maybe POSIX@
--
-- * @ReadyDateTime ::@ @Maybe POSIX@
--
-- * @EndDateTime ::@ @Maybe POSIX@
--
mkClusterTimeline :: ClusterTimeline
mkClusterTimeline = ClusterTimeline
    { _ctCreationDateTime = Nothing
    , _ctReadyDateTime = Nothing
    , _ctEndDateTime = Nothing
    }

-- | The creation date and time of the cluster.
ctCreationDateTime :: Lens' ClusterTimeline (Maybe POSIX)
ctCreationDateTime =
    lens _ctCreationDateTime (\s a -> s { _ctCreationDateTime = a })

-- | The date and time when the cluster was ready to execute steps.
ctReadyDateTime :: Lens' ClusterTimeline (Maybe POSIX)
ctReadyDateTime = lens _ctReadyDateTime (\s a -> s { _ctReadyDateTime = a })

-- | The date and time when the cluster was terminated.
ctEndDateTime :: Lens' ClusterTimeline (Maybe POSIX)
ctEndDateTime = lens _ctEndDateTime (\s a -> s { _ctEndDateTime = a })

instance FromJSON ClusterTimeline

instance ToJSON ClusterTimeline

-- | An entity describing an executable that runs on a cluster.
data Command = Command
    { _crName :: Maybe Text
    , _crScriptPath :: Maybe Text
    , _crArgs :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Command' data type.
--
-- 'Command' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Maybe Text@
--
-- * @ScriptPath ::@ @Maybe Text@
--
-- * @Args ::@ @[Text]@
--
mkCommand :: Command
mkCommand = Command
    { _crName = Nothing
    , _crScriptPath = Nothing
    , _crArgs = mempty
    }

-- | The name of the command.
crName :: Lens' Command (Maybe Text)
crName = lens _crName (\s a -> s { _crName = a })

-- | The Amazon S3 location of the command script.
crScriptPath :: Lens' Command (Maybe Text)
crScriptPath = lens _crScriptPath (\s a -> s { _crScriptPath = a })

-- | Arguments for Amazon EMR to pass to the command for execution.
crArgs :: Lens' Command [Text]
crArgs = lens _crArgs (\s a -> s { _crArgs = a })

instance FromJSON Command

-- | Provides information about the EC2 instances in a cluster grouped by
-- category. For example, key name, subnet ID, IAM instance profile, and so
-- on.
data Ec2InstanceAttributes = Ec2InstanceAttributes
    { _eiaEc2KeyName :: Maybe Text
    , _eiaEc2SubnetId :: Maybe Text
    , _eiaEc2AvailabilityZone :: Maybe Text
    , _eiaIamInstanceProfile :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Ec2InstanceAttributes' data type.
--
-- 'Ec2InstanceAttributes' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Ec2KeyName ::@ @Maybe Text@
--
-- * @Ec2SubnetId ::@ @Maybe Text@
--
-- * @Ec2AvailabilityZone ::@ @Maybe Text@
--
-- * @IamInstanceProfile ::@ @Maybe Text@
--
mkEc2InstanceAttributes :: Ec2InstanceAttributes
mkEc2InstanceAttributes = Ec2InstanceAttributes
    { _eiaEc2KeyName = Nothing
    , _eiaEc2SubnetId = Nothing
    , _eiaEc2AvailabilityZone = Nothing
    , _eiaIamInstanceProfile = Nothing
    }

-- | The name of the Amazon EC2 key pair to use when connecting with SSH into
-- the master node as a user named "hadoop".
eiaEc2KeyName :: Lens' Ec2InstanceAttributes (Maybe Text)
eiaEc2KeyName = lens _eiaEc2KeyName (\s a -> s { _eiaEc2KeyName = a })

-- | To launch the job flow in Amazon VPC, set this parameter to the identifier
-- of the Amazon VPC subnet where you want the job flow to launch. If you do
-- not specify this value, the job flow is launched in the normal AWS cloud,
-- outside of a VPC. Amazon VPC currently does not support cluster compute
-- quadruple extra large (cc1.4xlarge) instances. Thus, you cannot specify the
-- cc1.4xlarge instance type for nodes of a job flow launched in a VPC.
eiaEc2SubnetId :: Lens' Ec2InstanceAttributes (Maybe Text)
eiaEc2SubnetId = lens _eiaEc2SubnetId (\s a -> s { _eiaEc2SubnetId = a })

-- | The Availability Zone in which the cluster will run.
eiaEc2AvailabilityZone :: Lens' Ec2InstanceAttributes (Maybe Text)
eiaEc2AvailabilityZone =
    lens _eiaEc2AvailabilityZone (\s a -> s { _eiaEc2AvailabilityZone = a })

-- | The IAM role that was specified when the job flow was launched. The EC2
-- instances of the job flow assume this role.
eiaIamInstanceProfile :: Lens' Ec2InstanceAttributes (Maybe Text)
eiaIamInstanceProfile =
    lens _eiaIamInstanceProfile (\s a -> s { _eiaIamInstanceProfile = a })

instance FromJSON Ec2InstanceAttributes

-- | The JAR file used for the job flow step.
data HadoopJarStepConfig = HadoopJarStepConfig
    { _hjscProperties :: [KeyValue]
    , _hjscJar :: Text
    , _hjscMainClass :: Maybe Text
    , _hjscArgs :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'HadoopJarStepConfig' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Properties ::@ @[KeyValue]@
--
-- * @Jar ::@ @Text@
--
-- * @MainClass ::@ @Maybe Text@
--
-- * @Args ::@ @[Text]@
--
mkHadoopJarStepConfig :: Text -- ^ 'hjscJar'
                      -> HadoopJarStepConfig
mkHadoopJarStepConfig p2 = HadoopJarStepConfig
    { _hjscProperties = mempty
    , _hjscJar = p2
    , _hjscMainClass = Nothing
    , _hjscArgs = mempty
    }

-- | A list of Java properties that are set when the step runs. You can use
-- these properties to pass key value pairs to your main function.
hjscProperties :: Lens' HadoopJarStepConfig [KeyValue]
hjscProperties = lens _hjscProperties (\s a -> s { _hjscProperties = a })

-- | A path to a JAR file run during the step.
hjscJar :: Lens' HadoopJarStepConfig Text
hjscJar = lens _hjscJar (\s a -> s { _hjscJar = a })

-- | The name of the main class in the specified Java file. If not specified,
-- the JAR file should specify a Main-Class in its manifest file.
hjscMainClass :: Lens' HadoopJarStepConfig (Maybe Text)
hjscMainClass = lens _hjscMainClass (\s a -> s { _hjscMainClass = a })

-- | A list of command line arguments passed to the JAR file's main function
-- when executed.
hjscArgs :: Lens' HadoopJarStepConfig [Text]
hjscArgs = lens _hjscArgs (\s a -> s { _hjscArgs = a })

instance FromJSON HadoopJarStepConfig

instance ToJSON HadoopJarStepConfig

-- | The Hadoop job configuration of the cluster step.
data HadoopStepConfig = HadoopStepConfig
    { _hscJar :: Maybe Text
    , _hscProperties :: Map Text Text
    , _hscMainClass :: Maybe Text
    , _hscArgs :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'HadoopStepConfig' data type.
--
-- 'HadoopStepConfig' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Jar ::@ @Maybe Text@
--
-- * @Properties ::@ @Map Text Text@
--
-- * @MainClass ::@ @Maybe Text@
--
-- * @Args ::@ @[Text]@
--
mkHadoopStepConfig :: HadoopStepConfig
mkHadoopStepConfig = HadoopStepConfig
    { _hscJar = Nothing
    , _hscProperties = mempty
    , _hscMainClass = Nothing
    , _hscArgs = mempty
    }

-- | The path to the JAR file that runs during the step.
hscJar :: Lens' HadoopStepConfig (Maybe Text)
hscJar = lens _hscJar (\s a -> s { _hscJar = a })

-- | The list of Java properties that are set when the step runs. You can use
-- these properties to pass key value pairs to your main function.
hscProperties :: Lens' HadoopStepConfig (Map Text Text)
hscProperties = lens _hscProperties (\s a -> s { _hscProperties = a })

-- | The name of the main class in the specified Java file. If not specified,
-- the JAR file should specify a main class in its manifest file.
hscMainClass :: Lens' HadoopStepConfig (Maybe Text)
hscMainClass = lens _hscMainClass (\s a -> s { _hscMainClass = a })

-- | The list of command line arguments to pass to the JAR file's main function
-- for execution.
hscArgs :: Lens' HadoopStepConfig [Text]
hscArgs = lens _hscArgs (\s a -> s { _hscArgs = a })

instance FromJSON HadoopStepConfig

-- | Represents an EC2 instance provisioned as part of cluster.
data Instance = Instance
    { _iId :: Maybe Text
    , _iEc2InstanceId :: Maybe Text
    , _iPublicDnsName :: Maybe Text
    , _iPublicIpAddress :: Maybe Text
    , _iPrivateDnsName :: Maybe Text
    , _iPrivateIpAddress :: Maybe Text
    , _iStatus :: Maybe InstanceStatus
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Instance' data type.
--
-- 'Instance' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Id ::@ @Maybe Text@
--
-- * @Ec2InstanceId ::@ @Maybe Text@
--
-- * @PublicDnsName ::@ @Maybe Text@
--
-- * @PublicIpAddress ::@ @Maybe Text@
--
-- * @PrivateDnsName ::@ @Maybe Text@
--
-- * @PrivateIpAddress ::@ @Maybe Text@
--
-- * @Status ::@ @Maybe InstanceStatus@
--
mkInstance :: Instance
mkInstance = Instance
    { _iId = Nothing
    , _iEc2InstanceId = Nothing
    , _iPublicDnsName = Nothing
    , _iPublicIpAddress = Nothing
    , _iPrivateDnsName = Nothing
    , _iPrivateIpAddress = Nothing
    , _iStatus = Nothing
    }

-- | The unique identifier for the instance in Amazon EMR.
iId :: Lens' Instance (Maybe Text)
iId = lens _iId (\s a -> s { _iId = a })

-- | The unique identifier of the instance in Amazon EC2.
iEc2InstanceId :: Lens' Instance (Maybe Text)
iEc2InstanceId = lens _iEc2InstanceId (\s a -> s { _iEc2InstanceId = a })

-- | The public DNS name of the instance.
iPublicDnsName :: Lens' Instance (Maybe Text)
iPublicDnsName = lens _iPublicDnsName (\s a -> s { _iPublicDnsName = a })

-- | The public IP address of the instance.
iPublicIpAddress :: Lens' Instance (Maybe Text)
iPublicIpAddress =
    lens _iPublicIpAddress (\s a -> s { _iPublicIpAddress = a })

-- | The private DNS name of the instance.
iPrivateDnsName :: Lens' Instance (Maybe Text)
iPrivateDnsName = lens _iPrivateDnsName (\s a -> s { _iPrivateDnsName = a })

-- | The private IP address of the instance.
iPrivateIpAddress :: Lens' Instance (Maybe Text)
iPrivateIpAddress =
    lens _iPrivateIpAddress (\s a -> s { _iPrivateIpAddress = a })

-- | The current status of the instance.
iStatus :: Lens' Instance (Maybe InstanceStatus)
iStatus = lens _iStatus (\s a -> s { _iStatus = a })

instance FromJSON Instance

-- | This entity represents an instance group, which is a group of instances
-- that have common purpose. For example, CORE instance group is used for
-- HDFS.
data InstanceGroup = InstanceGroup
    { _igId :: Maybe Text
    , _igName :: Maybe Text
    , _igMarket :: Maybe MarketType
    , _igInstanceGroupType :: Maybe InstanceGroupType
    , _igBidPrice :: Maybe Text
    , _igInstanceType :: Maybe Text
    , _igRequestedInstanceCount :: Maybe Integer
    , _igRunningInstanceCount :: Maybe Integer
    , _igStatus :: Maybe InstanceGroupStatus
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceGroup' data type.
--
-- 'InstanceGroup' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Id ::@ @Maybe Text@
--
-- * @Name ::@ @Maybe Text@
--
-- * @Market ::@ @Maybe MarketType@
--
-- * @InstanceGroupType ::@ @Maybe InstanceGroupType@
--
-- * @BidPrice ::@ @Maybe Text@
--
-- * @InstanceType ::@ @Maybe Text@
--
-- * @RequestedInstanceCount ::@ @Maybe Integer@
--
-- * @RunningInstanceCount ::@ @Maybe Integer@
--
-- * @Status ::@ @Maybe InstanceGroupStatus@
--
mkInstanceGroup :: InstanceGroup
mkInstanceGroup = InstanceGroup
    { _igId = Nothing
    , _igName = Nothing
    , _igMarket = Nothing
    , _igInstanceGroupType = Nothing
    , _igBidPrice = Nothing
    , _igInstanceType = Nothing
    , _igRequestedInstanceCount = Nothing
    , _igRunningInstanceCount = Nothing
    , _igStatus = Nothing
    }

-- | The identifier of the instance group.
igId :: Lens' InstanceGroup (Maybe Text)
igId = lens _igId (\s a -> s { _igId = a })

-- | The name of the instance group.
igName :: Lens' InstanceGroup (Maybe Text)
igName = lens _igName (\s a -> s { _igName = a })

-- | The marketplace to provision instances for this group. Valid values are
-- ON_DEMAND or SPOT.
igMarket :: Lens' InstanceGroup (Maybe MarketType)
igMarket = lens _igMarket (\s a -> s { _igMarket = a })

-- | The type of the instance group. Valid values are MASTER, CORE or TASK.
igInstanceGroupType :: Lens' InstanceGroup (Maybe InstanceGroupType)
igInstanceGroupType =
    lens _igInstanceGroupType (\s a -> s { _igInstanceGroupType = a })

-- | The bid price for each EC2 instance in the instance group when launching
-- nodes as Spot Instances, expressed in USD.
igBidPrice :: Lens' InstanceGroup (Maybe Text)
igBidPrice = lens _igBidPrice (\s a -> s { _igBidPrice = a })

-- | The EC2 instance type for all instances in the instance group.
igInstanceType :: Lens' InstanceGroup (Maybe Text)
igInstanceType = lens _igInstanceType (\s a -> s { _igInstanceType = a })

-- | The target number of instances for the instance group.
igRequestedInstanceCount :: Lens' InstanceGroup (Maybe Integer)
igRequestedInstanceCount =
    lens _igRequestedInstanceCount
         (\s a -> s { _igRequestedInstanceCount = a })

-- | The number of instances currently running in this instance group.
igRunningInstanceCount :: Lens' InstanceGroup (Maybe Integer)
igRunningInstanceCount =
    lens _igRunningInstanceCount (\s a -> s { _igRunningInstanceCount = a })

-- | The current status of the instance group.
igStatus :: Lens' InstanceGroup (Maybe InstanceGroupStatus)
igStatus = lens _igStatus (\s a -> s { _igStatus = a })

instance FromJSON InstanceGroup

-- | Configuration defining a new instance group.
data InstanceGroupConfig = InstanceGroupConfig
    { _igcName :: Maybe Text
    , _igcMarket :: Maybe MarketType
    , _igcInstanceRole :: InstanceRoleType
    , _igcBidPrice :: Maybe Text
    , _igcInstanceType :: Text
    , _igcInstanceCount :: !Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceGroupConfig' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Maybe Text@
--
-- * @Market ::@ @Maybe MarketType@
--
-- * @InstanceRole ::@ @InstanceRoleType@
--
-- * @BidPrice ::@ @Maybe Text@
--
-- * @InstanceType ::@ @Text@
--
-- * @InstanceCount ::@ @Integer@
--
mkInstanceGroupConfig :: InstanceRoleType -- ^ 'igcInstanceRole'
                      -> Text -- ^ 'igcInstanceType'
                      -> Integer -- ^ 'igcInstanceCount'
                      -> InstanceGroupConfig
mkInstanceGroupConfig p3 p5 p6 = InstanceGroupConfig
    { _igcName = Nothing
    , _igcMarket = Nothing
    , _igcInstanceRole = p3
    , _igcBidPrice = Nothing
    , _igcInstanceType = p5
    , _igcInstanceCount = p6
    }

-- | Friendly name given to the instance group.
igcName :: Lens' InstanceGroupConfig (Maybe Text)
igcName = lens _igcName (\s a -> s { _igcName = a })

-- | Market type of the Amazon EC2 instances used to create a cluster node.
igcMarket :: Lens' InstanceGroupConfig (Maybe MarketType)
igcMarket = lens _igcMarket (\s a -> s { _igcMarket = a })

-- | The role of the instance group in the cluster.
igcInstanceRole :: Lens' InstanceGroupConfig InstanceRoleType
igcInstanceRole = lens _igcInstanceRole (\s a -> s { _igcInstanceRole = a })

-- | Bid price for each Amazon EC2 instance in the instance group when launching
-- nodes as Spot Instances, expressed in USD.
igcBidPrice :: Lens' InstanceGroupConfig (Maybe Text)
igcBidPrice = lens _igcBidPrice (\s a -> s { _igcBidPrice = a })

-- | The Amazon EC2 instance type for all instances in the instance group.
igcInstanceType :: Lens' InstanceGroupConfig Text
igcInstanceType = lens _igcInstanceType (\s a -> s { _igcInstanceType = a })

-- | Target number of instances for the instance group.
igcInstanceCount :: Lens' InstanceGroupConfig Integer
igcInstanceCount =
    lens _igcInstanceCount (\s a -> s { _igcInstanceCount = a })

instance ToJSON InstanceGroupConfig

-- | Detailed information about an instance group.
data InstanceGroupDetail = InstanceGroupDetail
    { _igdInstanceGroupId :: Maybe Text
    , _igdName :: Maybe Text
    , _igdMarket :: MarketType
    , _igdInstanceRole :: InstanceRoleType
    , _igdBidPrice :: Maybe Text
    , _igdInstanceType :: Text
    , _igdInstanceRequestCount :: !Integer
    , _igdInstanceRunningCount :: !Integer
    , _igdState :: InstanceGroupState
    , _igdLastStateChangeReason :: Maybe Text
    , _igdCreationDateTime :: POSIX
    , _igdStartDateTime :: Maybe POSIX
    , _igdReadyDateTime :: Maybe POSIX
    , _igdEndDateTime :: Maybe POSIX
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceGroupDetail' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceGroupId ::@ @Maybe Text@
--
-- * @Name ::@ @Maybe Text@
--
-- * @Market ::@ @MarketType@
--
-- * @InstanceRole ::@ @InstanceRoleType@
--
-- * @BidPrice ::@ @Maybe Text@
--
-- * @InstanceType ::@ @Text@
--
-- * @InstanceRequestCount ::@ @Integer@
--
-- * @InstanceRunningCount ::@ @Integer@
--
-- * @State ::@ @InstanceGroupState@
--
-- * @LastStateChangeReason ::@ @Maybe Text@
--
-- * @CreationDateTime ::@ @POSIX@
--
-- * @StartDateTime ::@ @Maybe POSIX@
--
-- * @ReadyDateTime ::@ @Maybe POSIX@
--
-- * @EndDateTime ::@ @Maybe POSIX@
--
mkInstanceGroupDetail :: POSIX -- ^ 'igdCreationDateTime'
                      -> MarketType -- ^ 'igdMarket'
                      -> InstanceRoleType -- ^ 'igdInstanceRole'
                      -> Text -- ^ 'igdInstanceType'
                      -> Integer -- ^ 'igdInstanceRequestCount'
                      -> Integer -- ^ 'igdInstanceRunningCount'
                      -> InstanceGroupState -- ^ 'igdState'
                      -> InstanceGroupDetail
mkInstanceGroupDetail p11 p3 p4 p6 p7 p8 p9 = InstanceGroupDetail
    { _igdInstanceGroupId = Nothing
    , _igdName = Nothing
    , _igdMarket = p3
    , _igdInstanceRole = p4
    , _igdBidPrice = Nothing
    , _igdInstanceType = p6
    , _igdInstanceRequestCount = p7
    , _igdInstanceRunningCount = p8
    , _igdState = p9
    , _igdLastStateChangeReason = Nothing
    , _igdCreationDateTime = p11
    , _igdStartDateTime = Nothing
    , _igdReadyDateTime = Nothing
    , _igdEndDateTime = Nothing
    }

-- | Unique identifier for the instance group.
igdInstanceGroupId :: Lens' InstanceGroupDetail (Maybe Text)
igdInstanceGroupId =
    lens _igdInstanceGroupId (\s a -> s { _igdInstanceGroupId = a })

-- | Friendly name for the instance group.
igdName :: Lens' InstanceGroupDetail (Maybe Text)
igdName = lens _igdName (\s a -> s { _igdName = a })

-- | Market type of the Amazon EC2 instances used to create a cluster node.
igdMarket :: Lens' InstanceGroupDetail MarketType
igdMarket = lens _igdMarket (\s a -> s { _igdMarket = a })

-- | Instance group role in the cluster.
igdInstanceRole :: Lens' InstanceGroupDetail InstanceRoleType
igdInstanceRole = lens _igdInstanceRole (\s a -> s { _igdInstanceRole = a })

-- | Bid price for EC2 Instances when launching nodes as Spot Instances,
-- expressed in USD.
igdBidPrice :: Lens' InstanceGroupDetail (Maybe Text)
igdBidPrice = lens _igdBidPrice (\s a -> s { _igdBidPrice = a })

-- | Amazon EC2 Instance type.
igdInstanceType :: Lens' InstanceGroupDetail Text
igdInstanceType = lens _igdInstanceType (\s a -> s { _igdInstanceType = a })

-- | Target number of instances to run in the instance group.
igdInstanceRequestCount :: Lens' InstanceGroupDetail Integer
igdInstanceRequestCount =
    lens _igdInstanceRequestCount
         (\s a -> s { _igdInstanceRequestCount = a })

-- | Actual count of running instances.
igdInstanceRunningCount :: Lens' InstanceGroupDetail Integer
igdInstanceRunningCount =
    lens _igdInstanceRunningCount
         (\s a -> s { _igdInstanceRunningCount = a })

-- | State of instance group. The following values are deprecated: STARTING,
-- TERMINATED, and FAILED.
igdState :: Lens' InstanceGroupDetail InstanceGroupState
igdState = lens _igdState (\s a -> s { _igdState = a })

-- | Details regarding the state of the instance group.
igdLastStateChangeReason :: Lens' InstanceGroupDetail (Maybe Text)
igdLastStateChangeReason =
    lens _igdLastStateChangeReason
         (\s a -> s { _igdLastStateChangeReason = a })

-- | The date/time the instance group was created.
igdCreationDateTime :: Lens' InstanceGroupDetail POSIX
igdCreationDateTime =
    lens _igdCreationDateTime (\s a -> s { _igdCreationDateTime = a })

-- | The date/time the instance group was started.
igdStartDateTime :: Lens' InstanceGroupDetail (Maybe POSIX)
igdStartDateTime =
    lens _igdStartDateTime (\s a -> s { _igdStartDateTime = a })

-- | The date/time the instance group was available to the cluster.
igdReadyDateTime :: Lens' InstanceGroupDetail (Maybe POSIX)
igdReadyDateTime =
    lens _igdReadyDateTime (\s a -> s { _igdReadyDateTime = a })

-- | The date/time the instance group was terminated.
igdEndDateTime :: Lens' InstanceGroupDetail (Maybe POSIX)
igdEndDateTime = lens _igdEndDateTime (\s a -> s { _igdEndDateTime = a })

instance FromJSON InstanceGroupDetail

instance ToJSON InstanceGroupDetail

-- | Modify an instance group size.
data InstanceGroupModifyConfig = InstanceGroupModifyConfig
    { _igmcInstanceGroupId :: Text
    , _igmcInstanceCount :: Maybe Integer
    , _igmcEC2InstanceIdsToTerminate :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceGroupModifyConfig' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceGroupId ::@ @Text@
--
-- * @InstanceCount ::@ @Maybe Integer@
--
-- * @EC2InstanceIdsToTerminate ::@ @[Text]@
--
mkInstanceGroupModifyConfig :: Text -- ^ 'igmcInstanceGroupId'
                            -> InstanceGroupModifyConfig
mkInstanceGroupModifyConfig p1 = InstanceGroupModifyConfig
    { _igmcInstanceGroupId = p1
    , _igmcInstanceCount = Nothing
    , _igmcEC2InstanceIdsToTerminate = mempty
    }

-- | Unique ID of the instance group to expand or shrink.
igmcInstanceGroupId :: Lens' InstanceGroupModifyConfig Text
igmcInstanceGroupId =
    lens _igmcInstanceGroupId (\s a -> s { _igmcInstanceGroupId = a })

-- | Target size for the instance group.
igmcInstanceCount :: Lens' InstanceGroupModifyConfig (Maybe Integer)
igmcInstanceCount =
    lens _igmcInstanceCount (\s a -> s { _igmcInstanceCount = a })

-- | The EC2 InstanceIds to terminate. For advanced users only. Once you
-- terminate the instances, the instance group will not return to its original
-- requested size.
igmcEC2InstanceIdsToTerminate :: Lens' InstanceGroupModifyConfig [Text]
igmcEC2InstanceIdsToTerminate =
    lens _igmcEC2InstanceIdsToTerminate
         (\s a -> s { _igmcEC2InstanceIdsToTerminate = a })

instance ToJSON InstanceGroupModifyConfig

-- | The status change reason details for the instance group.
data InstanceGroupStateChangeReason = InstanceGroupStateChangeReason
    { _igscrCode :: Maybe InstanceGroupStateChangeReasonCode
    , _igscrMessage :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceGroupStateChangeReason' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Code ::@ @Maybe InstanceGroupStateChangeReasonCode@
--
-- * @Message ::@ @Maybe Text@
--
mkInstanceGroupStateChangeReason :: InstanceGroupStateChangeReason
mkInstanceGroupStateChangeReason = InstanceGroupStateChangeReason
    { _igscrCode = Nothing
    , _igscrMessage = Nothing
    }

-- | The programmable code for the state change reason.
igscrCode :: Lens' InstanceGroupStateChangeReason (Maybe InstanceGroupStateChangeReasonCode)
igscrCode = lens _igscrCode (\s a -> s { _igscrCode = a })

-- | The status change reason description.
igscrMessage :: Lens' InstanceGroupStateChangeReason (Maybe Text)
igscrMessage = lens _igscrMessage (\s a -> s { _igscrMessage = a })

instance FromJSON InstanceGroupStateChangeReason

instance ToJSON InstanceGroupStateChangeReason

-- | The current status of the instance group.
data InstanceGroupStatus = InstanceGroupStatus
    { _igsState :: Maybe InstanceGroupState
    , _igsStateChangeReason :: Maybe InstanceGroupStateChangeReason
    , _igsTimeline :: Maybe InstanceGroupTimeline
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceGroupStatus' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @State ::@ @Maybe InstanceGroupState@
--
-- * @StateChangeReason ::@ @Maybe InstanceGroupStateChangeReason@
--
-- * @Timeline ::@ @Maybe InstanceGroupTimeline@
--
mkInstanceGroupStatus :: InstanceGroupStatus
mkInstanceGroupStatus = InstanceGroupStatus
    { _igsState = Nothing
    , _igsStateChangeReason = Nothing
    , _igsTimeline = Nothing
    }

-- | The current state of the instance group.
igsState :: Lens' InstanceGroupStatus (Maybe InstanceGroupState)
igsState = lens _igsState (\s a -> s { _igsState = a })

-- | The status change reason details for the instance group.
igsStateChangeReason :: Lens' InstanceGroupStatus (Maybe InstanceGroupStateChangeReason)
igsStateChangeReason =
    lens _igsStateChangeReason (\s a -> s { _igsStateChangeReason = a })

-- | The timeline of the instance group status over time.
igsTimeline :: Lens' InstanceGroupStatus (Maybe InstanceGroupTimeline)
igsTimeline = lens _igsTimeline (\s a -> s { _igsTimeline = a })

instance FromJSON InstanceGroupStatus

instance ToJSON InstanceGroupStatus

-- | The timeline of the instance group status over time.
data InstanceGroupTimeline = InstanceGroupTimeline
    { _igtCreationDateTime :: Maybe POSIX
    , _igtReadyDateTime :: Maybe POSIX
    , _igtEndDateTime :: Maybe POSIX
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceGroupTimeline' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CreationDateTime ::@ @Maybe POSIX@
--
-- * @ReadyDateTime ::@ @Maybe POSIX@
--
-- * @EndDateTime ::@ @Maybe POSIX@
--
mkInstanceGroupTimeline :: InstanceGroupTimeline
mkInstanceGroupTimeline = InstanceGroupTimeline
    { _igtCreationDateTime = Nothing
    , _igtReadyDateTime = Nothing
    , _igtEndDateTime = Nothing
    }

-- | The creation date and time of the instance group.
igtCreationDateTime :: Lens' InstanceGroupTimeline (Maybe POSIX)
igtCreationDateTime =
    lens _igtCreationDateTime (\s a -> s { _igtCreationDateTime = a })

-- | The date and time when the instance group became ready to perform tasks.
igtReadyDateTime :: Lens' InstanceGroupTimeline (Maybe POSIX)
igtReadyDateTime =
    lens _igtReadyDateTime (\s a -> s { _igtReadyDateTime = a })

-- | The date and time when the instance group terminated.
igtEndDateTime :: Lens' InstanceGroupTimeline (Maybe POSIX)
igtEndDateTime = lens _igtEndDateTime (\s a -> s { _igtEndDateTime = a })

instance FromJSON InstanceGroupTimeline

instance ToJSON InstanceGroupTimeline

-- | The details of the status change reason for the instance.
data InstanceStateChangeReason = InstanceStateChangeReason
    { _iscrCode :: Maybe InstanceStateChangeReasonCode
    , _iscrMessage :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceStateChangeReason' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Code ::@ @Maybe InstanceStateChangeReasonCode@
--
-- * @Message ::@ @Maybe Text@
--
mkInstanceStateChangeReason :: InstanceStateChangeReason
mkInstanceStateChangeReason = InstanceStateChangeReason
    { _iscrCode = Nothing
    , _iscrMessage = Nothing
    }

-- | The programmable code for the state change reason.
iscrCode :: Lens' InstanceStateChangeReason (Maybe InstanceStateChangeReasonCode)
iscrCode = lens _iscrCode (\s a -> s { _iscrCode = a })

-- | The status change reason description.
iscrMessage :: Lens' InstanceStateChangeReason (Maybe Text)
iscrMessage = lens _iscrMessage (\s a -> s { _iscrMessage = a })

instance FromJSON InstanceStateChangeReason

instance ToJSON InstanceStateChangeReason

-- | The current status of the instance.
data InstanceStatus = InstanceStatus
    { _isState :: Maybe InstanceState
    , _isStateChangeReason :: Maybe InstanceStateChangeReason
    , _isTimeline :: Maybe InstanceTimeline
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceStatus' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @State ::@ @Maybe InstanceState@
--
-- * @StateChangeReason ::@ @Maybe InstanceStateChangeReason@
--
-- * @Timeline ::@ @Maybe InstanceTimeline@
--
mkInstanceStatus :: InstanceStatus
mkInstanceStatus = InstanceStatus
    { _isState = Nothing
    , _isStateChangeReason = Nothing
    , _isTimeline = Nothing
    }

-- | The current state of the instance.
isState :: Lens' InstanceStatus (Maybe InstanceState)
isState = lens _isState (\s a -> s { _isState = a })

-- | The details of the status change reason for the instance.
isStateChangeReason :: Lens' InstanceStatus (Maybe InstanceStateChangeReason)
isStateChangeReason =
    lens _isStateChangeReason (\s a -> s { _isStateChangeReason = a })

-- | The timeline of the instance status over time.
isTimeline :: Lens' InstanceStatus (Maybe InstanceTimeline)
isTimeline = lens _isTimeline (\s a -> s { _isTimeline = a })

instance FromJSON InstanceStatus

instance ToJSON InstanceStatus

-- | The timeline of the instance status over time.
data InstanceTimeline = InstanceTimeline
    { _itCreationDateTime :: Maybe POSIX
    , _itReadyDateTime :: Maybe POSIX
    , _itEndDateTime :: Maybe POSIX
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceTimeline' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CreationDateTime ::@ @Maybe POSIX@
--
-- * @ReadyDateTime ::@ @Maybe POSIX@
--
-- * @EndDateTime ::@ @Maybe POSIX@
--
mkInstanceTimeline :: InstanceTimeline
mkInstanceTimeline = InstanceTimeline
    { _itCreationDateTime = Nothing
    , _itReadyDateTime = Nothing
    , _itEndDateTime = Nothing
    }

-- | The creation date and time of the instance.
itCreationDateTime :: Lens' InstanceTimeline (Maybe POSIX)
itCreationDateTime =
    lens _itCreationDateTime (\s a -> s { _itCreationDateTime = a })

-- | The date and time when the instance was ready to perform tasks.
itReadyDateTime :: Lens' InstanceTimeline (Maybe POSIX)
itReadyDateTime = lens _itReadyDateTime (\s a -> s { _itReadyDateTime = a })

-- | The date and time when the instance was terminated.
itEndDateTime :: Lens' InstanceTimeline (Maybe POSIX)
itEndDateTime = lens _itEndDateTime (\s a -> s { _itEndDateTime = a })

instance FromJSON InstanceTimeline

instance ToJSON InstanceTimeline

-- | A description of a job flow.
data JobFlowDetail = JobFlowDetail
    { _jfdJobFlowId :: Text
    , _jfdName :: Text
    , _jfdLogUri :: Maybe Text
    , _jfdAmiVersion :: Maybe Text
    , _jfdExecutionStatusDetail :: JobFlowExecutionStatusDetail
    , _jfdInstances :: JobFlowInstancesDetail
    , _jfdSteps :: [StepDetail]
    , _jfdBootstrapActions :: [BootstrapActionDetail]
    , _jfdSupportedProducts :: [Text]
    , _jfdVisibleToAllUsers :: Maybe Bool
    , _jfdJobFlowRole :: Maybe Text
    , _jfdServiceRole :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'JobFlowDetail' data type.
--
-- 'JobFlowDetail' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @JobFlowId ::@ @Text@
--
-- * @Name ::@ @Text@
--
-- * @LogUri ::@ @Maybe Text@
--
-- * @AmiVersion ::@ @Maybe Text@
--
-- * @ExecutionStatusDetail ::@ @JobFlowExecutionStatusDetail@
--
-- * @Instances ::@ @JobFlowInstancesDetail@
--
-- * @Steps ::@ @[StepDetail]@
--
-- * @BootstrapActions ::@ @[BootstrapActionDetail]@
--
-- * @SupportedProducts ::@ @[Text]@
--
-- * @VisibleToAllUsers ::@ @Maybe Bool@
--
-- * @JobFlowRole ::@ @Maybe Text@
--
-- * @ServiceRole ::@ @Maybe Text@
--
mkJobFlowDetail :: Text -- ^ 'jfdJobFlowId'
                -> Text -- ^ 'jfdName'
                -> JobFlowExecutionStatusDetail -- ^ 'jfdExecutionStatusDetail'
                -> JobFlowInstancesDetail -- ^ 'jfdInstances'
                -> JobFlowDetail
mkJobFlowDetail p1 p2 p5 p6 = JobFlowDetail
    { _jfdJobFlowId = p1
    , _jfdName = p2
    , _jfdLogUri = Nothing
    , _jfdAmiVersion = Nothing
    , _jfdExecutionStatusDetail = p5
    , _jfdInstances = p6
    , _jfdSteps = mempty
    , _jfdBootstrapActions = mempty
    , _jfdSupportedProducts = mempty
    , _jfdVisibleToAllUsers = Nothing
    , _jfdJobFlowRole = Nothing
    , _jfdServiceRole = Nothing
    }

-- | The job flow identifier.
jfdJobFlowId :: Lens' JobFlowDetail Text
jfdJobFlowId = lens _jfdJobFlowId (\s a -> s { _jfdJobFlowId = a })

-- | The name of the job flow.
jfdName :: Lens' JobFlowDetail Text
jfdName = lens _jfdName (\s a -> s { _jfdName = a })

-- | The location in Amazon S3 where log files for the job are stored.
jfdLogUri :: Lens' JobFlowDetail (Maybe Text)
jfdLogUri = lens _jfdLogUri (\s a -> s { _jfdLogUri = a })

-- | The version of the AMI used to initialize Amazon EC2 instances in the job
-- flow. For a list of AMI versions currently supported by Amazon
-- ElasticMapReduce, go to AMI Versions Supported in Elastic MapReduce in the
-- Amazon Elastic MapReduce Developer's Guide.
jfdAmiVersion :: Lens' JobFlowDetail (Maybe Text)
jfdAmiVersion = lens _jfdAmiVersion (\s a -> s { _jfdAmiVersion = a })

-- | Describes the execution status of the job flow.
jfdExecutionStatusDetail :: Lens' JobFlowDetail JobFlowExecutionStatusDetail
jfdExecutionStatusDetail =
    lens _jfdExecutionStatusDetail
         (\s a -> s { _jfdExecutionStatusDetail = a })

-- | Describes the Amazon EC2 instances of the job flow.
jfdInstances :: Lens' JobFlowDetail JobFlowInstancesDetail
jfdInstances = lens _jfdInstances (\s a -> s { _jfdInstances = a })

-- | A list of steps run by the job flow.
jfdSteps :: Lens' JobFlowDetail [StepDetail]
jfdSteps = lens _jfdSteps (\s a -> s { _jfdSteps = a })

-- | A list of the bootstrap actions run by the job flow.
jfdBootstrapActions :: Lens' JobFlowDetail [BootstrapActionDetail]
jfdBootstrapActions =
    lens _jfdBootstrapActions (\s a -> s { _jfdBootstrapActions = a })

-- | A list of strings set by third party software when the job flow is
-- launched. If you are not using third party software to manage the job flow
-- this value is empty.
jfdSupportedProducts :: Lens' JobFlowDetail [Text]
jfdSupportedProducts =
    lens _jfdSupportedProducts (\s a -> s { _jfdSupportedProducts = a })

-- | Specifies whether the job flow is visible to all IAM users of the AWS
-- account associated with the job flow. If this value is set to true, all IAM
-- users of that AWS account can view and (if they have the proper policy
-- permissions set) manage the job flow. If it is set to false, only the IAM
-- user that created the job flow can view and manage it. This value can be
-- changed using the SetVisibleToAllUsers action.
jfdVisibleToAllUsers :: Lens' JobFlowDetail (Maybe Bool)
jfdVisibleToAllUsers =
    lens _jfdVisibleToAllUsers (\s a -> s { _jfdVisibleToAllUsers = a })

-- | The IAM role that was specified when the job flow was launched. The EC2
-- instances of the job flow assume this role.
jfdJobFlowRole :: Lens' JobFlowDetail (Maybe Text)
jfdJobFlowRole = lens _jfdJobFlowRole (\s a -> s { _jfdJobFlowRole = a })

-- | The IAM role that will be assumed by the Amazon EMR service to access AWS
-- resources on your behalf.
jfdServiceRole :: Lens' JobFlowDetail (Maybe Text)
jfdServiceRole = lens _jfdServiceRole (\s a -> s { _jfdServiceRole = a })

instance FromJSON JobFlowDetail

-- | Describes the execution status of the job flow.
data JobFlowExecutionStatusDetail = JobFlowExecutionStatusDetail
    { _jfesdState :: JobFlowExecutionState
    , _jfesdCreationDateTime :: POSIX
    , _jfesdStartDateTime :: Maybe POSIX
    , _jfesdReadyDateTime :: Maybe POSIX
    , _jfesdEndDateTime :: Maybe POSIX
    , _jfesdLastStateChangeReason :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'JobFlowExecutionStatusDetail' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @State ::@ @JobFlowExecutionState@
--
-- * @CreationDateTime ::@ @POSIX@
--
-- * @StartDateTime ::@ @Maybe POSIX@
--
-- * @ReadyDateTime ::@ @Maybe POSIX@
--
-- * @EndDateTime ::@ @Maybe POSIX@
--
-- * @LastStateChangeReason ::@ @Maybe Text@
--
mkJobFlowExecutionStatusDetail :: JobFlowExecutionState -- ^ 'jfesdState'
                               -> POSIX -- ^ 'jfesdCreationDateTime'
                               -> JobFlowExecutionStatusDetail
mkJobFlowExecutionStatusDetail p1 p2 = JobFlowExecutionStatusDetail
    { _jfesdState = p1
    , _jfesdCreationDateTime = p2
    , _jfesdStartDateTime = Nothing
    , _jfesdReadyDateTime = Nothing
    , _jfesdEndDateTime = Nothing
    , _jfesdLastStateChangeReason = Nothing
    }

-- | The state of the job flow.
jfesdState :: Lens' JobFlowExecutionStatusDetail JobFlowExecutionState
jfesdState = lens _jfesdState (\s a -> s { _jfesdState = a })

-- | The creation date and time of the job flow.
jfesdCreationDateTime :: Lens' JobFlowExecutionStatusDetail POSIX
jfesdCreationDateTime =
    lens _jfesdCreationDateTime (\s a -> s { _jfesdCreationDateTime = a })

-- | The start date and time of the job flow.
jfesdStartDateTime :: Lens' JobFlowExecutionStatusDetail (Maybe POSIX)
jfesdStartDateTime =
    lens _jfesdStartDateTime (\s a -> s { _jfesdStartDateTime = a })

-- | The date and time when the job flow was ready to start running bootstrap
-- actions.
jfesdReadyDateTime :: Lens' JobFlowExecutionStatusDetail (Maybe POSIX)
jfesdReadyDateTime =
    lens _jfesdReadyDateTime (\s a -> s { _jfesdReadyDateTime = a })

-- | The completion date and time of the job flow.
jfesdEndDateTime :: Lens' JobFlowExecutionStatusDetail (Maybe POSIX)
jfesdEndDateTime =
    lens _jfesdEndDateTime (\s a -> s { _jfesdEndDateTime = a })

-- | Description of the job flow last changed state.
jfesdLastStateChangeReason :: Lens' JobFlowExecutionStatusDetail (Maybe Text)
jfesdLastStateChangeReason =
    lens _jfesdLastStateChangeReason
         (\s a -> s { _jfesdLastStateChangeReason = a })

instance FromJSON JobFlowExecutionStatusDetail

instance ToJSON JobFlowExecutionStatusDetail

-- | A specification of the number and type of Amazon EC2 instances on which to
-- run the job flow.
data JobFlowInstancesConfig = JobFlowInstancesConfig
    { _jficMasterInstanceType :: Maybe Text
    , _jficSlaveInstanceType :: Maybe Text
    , _jficInstanceCount :: Maybe Integer
    , _jficInstanceGroups :: [InstanceGroupConfig]
    , _jficEc2KeyName :: Maybe Text
    , _jficPlacement :: Maybe PlacementType
    , _jficKeepJobFlowAliveWhenNoSteps :: Maybe Bool
    , _jficTerminationProtected :: Maybe Bool
    , _jficHadoopVersion :: Maybe Text
    , _jficEc2SubnetId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'JobFlowInstancesConfig' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @MasterInstanceType ::@ @Maybe Text@
--
-- * @SlaveInstanceType ::@ @Maybe Text@
--
-- * @InstanceCount ::@ @Maybe Integer@
--
-- * @InstanceGroups ::@ @[InstanceGroupConfig]@
--
-- * @Ec2KeyName ::@ @Maybe Text@
--
-- * @Placement ::@ @Maybe PlacementType@
--
-- * @KeepJobFlowAliveWhenNoSteps ::@ @Maybe Bool@
--
-- * @TerminationProtected ::@ @Maybe Bool@
--
-- * @HadoopVersion ::@ @Maybe Text@
--
-- * @Ec2SubnetId ::@ @Maybe Text@
--
mkJobFlowInstancesConfig :: JobFlowInstancesConfig
mkJobFlowInstancesConfig = JobFlowInstancesConfig
    { _jficMasterInstanceType = Nothing
    , _jficSlaveInstanceType = Nothing
    , _jficInstanceCount = Nothing
    , _jficInstanceGroups = mempty
    , _jficEc2KeyName = Nothing
    , _jficPlacement = Nothing
    , _jficKeepJobFlowAliveWhenNoSteps = Nothing
    , _jficTerminationProtected = Nothing
    , _jficHadoopVersion = Nothing
    , _jficEc2SubnetId = Nothing
    }

-- | The EC2 instance type of the master node.
jficMasterInstanceType :: Lens' JobFlowInstancesConfig (Maybe Text)
jficMasterInstanceType =
    lens _jficMasterInstanceType (\s a -> s { _jficMasterInstanceType = a })

-- | The EC2 instance type of the slave nodes.
jficSlaveInstanceType :: Lens' JobFlowInstancesConfig (Maybe Text)
jficSlaveInstanceType =
    lens _jficSlaveInstanceType (\s a -> s { _jficSlaveInstanceType = a })

-- | The number of Amazon EC2 instances used to execute the job flow.
jficInstanceCount :: Lens' JobFlowInstancesConfig (Maybe Integer)
jficInstanceCount =
    lens _jficInstanceCount (\s a -> s { _jficInstanceCount = a })

-- | Configuration for the job flow's instance groups.
jficInstanceGroups :: Lens' JobFlowInstancesConfig [InstanceGroupConfig]
jficInstanceGroups =
    lens _jficInstanceGroups (\s a -> s { _jficInstanceGroups = a })

-- | The name of the Amazon EC2 key pair that can be used to ssh to the master
-- node as the user called "hadoop.".
jficEc2KeyName :: Lens' JobFlowInstancesConfig (Maybe Text)
jficEc2KeyName = lens _jficEc2KeyName (\s a -> s { _jficEc2KeyName = a })

-- | The Availability Zone the job flow will run in.
jficPlacement :: Lens' JobFlowInstancesConfig (Maybe PlacementType)
jficPlacement = lens _jficPlacement (\s a -> s { _jficPlacement = a })

-- | Specifies whether the job flow should terminate after completing all steps.
jficKeepJobFlowAliveWhenNoSteps :: Lens' JobFlowInstancesConfig (Maybe Bool)
jficKeepJobFlowAliveWhenNoSteps =
    lens _jficKeepJobFlowAliveWhenNoSteps
         (\s a -> s { _jficKeepJobFlowAliveWhenNoSteps = a })

-- | Specifies whether to lock the job flow to prevent the Amazon EC2 instances
-- from being terminated by API call, user intervention, or in the event of a
-- job flow error.
jficTerminationProtected :: Lens' JobFlowInstancesConfig (Maybe Bool)
jficTerminationProtected =
    lens _jficTerminationProtected
         (\s a -> s { _jficTerminationProtected = a })

-- | The Hadoop version for the job flow. Valid inputs are "0.18", "0.20", or
-- "0.20.205". If you do not set this value, the default of 0.18 is used,
-- unless the AmiVersion parameter is set in the RunJobFlow call, in which
-- case the default version of Hadoop for that AMI version is used.
jficHadoopVersion :: Lens' JobFlowInstancesConfig (Maybe Text)
jficHadoopVersion =
    lens _jficHadoopVersion (\s a -> s { _jficHadoopVersion = a })

-- | To launch the job flow in Amazon Virtual Private Cloud (Amazon VPC), set
-- this parameter to the identifier of the Amazon VPC subnet where you want
-- the job flow to launch. If you do not specify this value, the job flow is
-- launched in the normal Amazon Web Services cloud, outside of an Amazon VPC.
-- Amazon VPC currently does not support cluster compute quadruple extra large
-- (cc1.4xlarge) instances. Thus you cannot specify the cc1.4xlarge instance
-- type for nodes of a job flow launched in a Amazon VPC.
jficEc2SubnetId :: Lens' JobFlowInstancesConfig (Maybe Text)
jficEc2SubnetId = lens _jficEc2SubnetId (\s a -> s { _jficEc2SubnetId = a })

instance ToJSON JobFlowInstancesConfig

-- | Describes the Amazon EC2 instances of the job flow.
data JobFlowInstancesDetail = JobFlowInstancesDetail
    { _jfidMasterInstanceType :: Text
    , _jfidMasterPublicDnsName :: Maybe Text
    , _jfidMasterInstanceId :: Maybe Text
    , _jfidSlaveInstanceType :: Text
    , _jfidInstanceCount :: !Integer
    , _jfidInstanceGroups :: [InstanceGroupDetail]
    , _jfidNormalizedInstanceHours :: Maybe Integer
    , _jfidEc2KeyName :: Maybe Text
    , _jfidEc2SubnetId :: Maybe Text
    , _jfidPlacement :: Maybe PlacementType
    , _jfidKeepJobFlowAliveWhenNoSteps :: Maybe Bool
    , _jfidTerminationProtected :: Maybe Bool
    , _jfidHadoopVersion :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'JobFlowInstancesDetail' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @MasterInstanceType ::@ @Text@
--
-- * @MasterPublicDnsName ::@ @Maybe Text@
--
-- * @MasterInstanceId ::@ @Maybe Text@
--
-- * @SlaveInstanceType ::@ @Text@
--
-- * @InstanceCount ::@ @Integer@
--
-- * @InstanceGroups ::@ @[InstanceGroupDetail]@
--
-- * @NormalizedInstanceHours ::@ @Maybe Integer@
--
-- * @Ec2KeyName ::@ @Maybe Text@
--
-- * @Ec2SubnetId ::@ @Maybe Text@
--
-- * @Placement ::@ @Maybe PlacementType@
--
-- * @KeepJobFlowAliveWhenNoSteps ::@ @Maybe Bool@
--
-- * @TerminationProtected ::@ @Maybe Bool@
--
-- * @HadoopVersion ::@ @Maybe Text@
--
mkJobFlowInstancesDetail :: Text -- ^ 'jfidMasterInstanceType'
                         -> Text -- ^ 'jfidSlaveInstanceType'
                         -> Integer -- ^ 'jfidInstanceCount'
                         -> JobFlowInstancesDetail
mkJobFlowInstancesDetail p1 p4 p5 = JobFlowInstancesDetail
    { _jfidMasterInstanceType = p1
    , _jfidMasterPublicDnsName = Nothing
    , _jfidMasterInstanceId = Nothing
    , _jfidSlaveInstanceType = p4
    , _jfidInstanceCount = p5
    , _jfidInstanceGroups = mempty
    , _jfidNormalizedInstanceHours = Nothing
    , _jfidEc2KeyName = Nothing
    , _jfidEc2SubnetId = Nothing
    , _jfidPlacement = Nothing
    , _jfidKeepJobFlowAliveWhenNoSteps = Nothing
    , _jfidTerminationProtected = Nothing
    , _jfidHadoopVersion = Nothing
    }

-- | The Amazon EC2 master node instance type.
jfidMasterInstanceType :: Lens' JobFlowInstancesDetail Text
jfidMasterInstanceType =
    lens _jfidMasterInstanceType (\s a -> s { _jfidMasterInstanceType = a })

-- | The DNS name of the master node.
jfidMasterPublicDnsName :: Lens' JobFlowInstancesDetail (Maybe Text)
jfidMasterPublicDnsName =
    lens _jfidMasterPublicDnsName
         (\s a -> s { _jfidMasterPublicDnsName = a })

-- | The Amazon EC2 instance identifier of the master node.
jfidMasterInstanceId :: Lens' JobFlowInstancesDetail (Maybe Text)
jfidMasterInstanceId =
    lens _jfidMasterInstanceId (\s a -> s { _jfidMasterInstanceId = a })

-- | The Amazon EC2 slave node instance type.
jfidSlaveInstanceType :: Lens' JobFlowInstancesDetail Text
jfidSlaveInstanceType =
    lens _jfidSlaveInstanceType (\s a -> s { _jfidSlaveInstanceType = a })

-- | The number of Amazon EC2 instances in the cluster. If the value is 1, the
-- same instance serves as both the master and slave node. If the value is
-- greater than 1, one instance is the master node and all others are slave
-- nodes.
jfidInstanceCount :: Lens' JobFlowInstancesDetail Integer
jfidInstanceCount =
    lens _jfidInstanceCount (\s a -> s { _jfidInstanceCount = a })

-- | Details about the job flow's instance groups.
jfidInstanceGroups :: Lens' JobFlowInstancesDetail [InstanceGroupDetail]
jfidInstanceGroups =
    lens _jfidInstanceGroups (\s a -> s { _jfidInstanceGroups = a })

-- | An approximation of the cost of the job flow, represented in
-- m1.small/hours. This value is incremented once for every hour an m1.small
-- runs. Larger instances are weighted more, so an Amazon EC2 instance that is
-- roughly four times more expensive would result in the normalized instance
-- hours being incremented by four. This result is only an approximation and
-- does not reflect the actual billing rate.
jfidNormalizedInstanceHours :: Lens' JobFlowInstancesDetail (Maybe Integer)
jfidNormalizedInstanceHours =
    lens _jfidNormalizedInstanceHours
         (\s a -> s { _jfidNormalizedInstanceHours = a })

-- | The name of an Amazon EC2 key pair that can be used to ssh to the master
-- node of job flow.
jfidEc2KeyName :: Lens' JobFlowInstancesDetail (Maybe Text)
jfidEc2KeyName = lens _jfidEc2KeyName (\s a -> s { _jfidEc2KeyName = a })

-- | For job flows launched within Amazon Virtual Private Cloud, this value
-- specifies the identifier of the subnet where the job flow was launched.
jfidEc2SubnetId :: Lens' JobFlowInstancesDetail (Maybe Text)
jfidEc2SubnetId = lens _jfidEc2SubnetId (\s a -> s { _jfidEc2SubnetId = a })

-- | The Amazon EC2 Availability Zone for the job flow.
jfidPlacement :: Lens' JobFlowInstancesDetail (Maybe PlacementType)
jfidPlacement = lens _jfidPlacement (\s a -> s { _jfidPlacement = a })

-- | Specifies whether the job flow should terminate after completing all steps.
jfidKeepJobFlowAliveWhenNoSteps :: Lens' JobFlowInstancesDetail (Maybe Bool)
jfidKeepJobFlowAliveWhenNoSteps =
    lens _jfidKeepJobFlowAliveWhenNoSteps
         (\s a -> s { _jfidKeepJobFlowAliveWhenNoSteps = a })

-- | Specifies whether the Amazon EC2 instances in the cluster are protected
-- from termination by API calls, user intervention, or in the event of a job
-- flow error.
jfidTerminationProtected :: Lens' JobFlowInstancesDetail (Maybe Bool)
jfidTerminationProtected =
    lens _jfidTerminationProtected
         (\s a -> s { _jfidTerminationProtected = a })

-- | The Hadoop version for the job flow.
jfidHadoopVersion :: Lens' JobFlowInstancesDetail (Maybe Text)
jfidHadoopVersion =
    lens _jfidHadoopVersion (\s a -> s { _jfidHadoopVersion = a })

instance FromJSON JobFlowInstancesDetail

instance ToJSON JobFlowInstancesDetail

-- | A key value pair.
data KeyValue = KeyValue
    { _kvKey :: Maybe Text
    , _kvValue :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'KeyValue' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Key ::@ @Maybe Text@
--
-- * @Value ::@ @Maybe Text@
--
mkKeyValue :: KeyValue
mkKeyValue = KeyValue
    { _kvKey = Nothing
    , _kvValue = Nothing
    }

-- | The unique identifier of a key value pair.
kvKey :: Lens' KeyValue (Maybe Text)
kvKey = lens _kvKey (\s a -> s { _kvKey = a })

-- | The value part of the identified key.
kvValue :: Lens' KeyValue (Maybe Text)
kvValue = lens _kvValue (\s a -> s { _kvValue = a })

instance FromJSON KeyValue

instance ToJSON KeyValue

-- | The script run by the bootstrap action.
data ScriptBootstrapActionConfig = ScriptBootstrapActionConfig
    { _sbacPath :: Text
    , _sbacArgs :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ScriptBootstrapActionConfig' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Path ::@ @Text@
--
-- * @Args ::@ @[Text]@
--
mkScriptBootstrapActionConfig :: Text -- ^ 'sbacPath'
                              -> ScriptBootstrapActionConfig
mkScriptBootstrapActionConfig p1 = ScriptBootstrapActionConfig
    { _sbacPath = p1
    , _sbacArgs = mempty
    }

-- | Location of the script to run during a bootstrap action. Can be either a
-- location in Amazon S3 or on a local file system.
sbacPath :: Lens' ScriptBootstrapActionConfig Text
sbacPath = lens _sbacPath (\s a -> s { _sbacPath = a })

-- | A list of command line arguments to pass to the bootstrap action script.
sbacArgs :: Lens' ScriptBootstrapActionConfig [Text]
sbacArgs = lens _sbacArgs (\s a -> s { _sbacArgs = a })

instance FromJSON ScriptBootstrapActionConfig

instance ToJSON ScriptBootstrapActionConfig

-- | The step details for the requested step identifier.
data Step = Step
    { _sId :: Maybe Text
    , _sName :: Maybe Text
    , _sConfig :: Maybe HadoopStepConfig
    , _sActionOnFailure :: Maybe ActionOnFailure
    , _sStatus :: Maybe StepStatus
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Step' data type.
--
-- 'Step' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Id ::@ @Maybe Text@
--
-- * @Name ::@ @Maybe Text@
--
-- * @Config ::@ @Maybe HadoopStepConfig@
--
-- * @ActionOnFailure ::@ @Maybe ActionOnFailure@
--
-- * @Status ::@ @Maybe StepStatus@
--
mkStep :: Step
mkStep = Step
    { _sId = Nothing
    , _sName = Nothing
    , _sConfig = Nothing
    , _sActionOnFailure = Nothing
    , _sStatus = Nothing
    }

-- | The identifier of the cluster step.
sId :: Lens' Step (Maybe Text)
sId = lens _sId (\s a -> s { _sId = a })

-- | The name of the cluster step.
sName :: Lens' Step (Maybe Text)
sName = lens _sName (\s a -> s { _sName = a })

-- | The Hadoop job configuration of the cluster step.
sConfig :: Lens' Step (Maybe HadoopStepConfig)
sConfig = lens _sConfig (\s a -> s { _sConfig = a })

-- | This specifies what action to take when the cluster step fails. Possible
-- values are TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE.
sActionOnFailure :: Lens' Step (Maybe ActionOnFailure)
sActionOnFailure =
    lens _sActionOnFailure (\s a -> s { _sActionOnFailure = a })

-- | The current execution status details of the cluster step.
sStatus :: Lens' Step (Maybe StepStatus)
sStatus = lens _sStatus (\s a -> s { _sStatus = a })

instance FromJSON Step

-- | Specification of a job flow step.
data StepConfig = StepConfig
    { _scName :: Text
    , _scActionOnFailure :: Maybe ActionOnFailure
    , _scHadoopJarStep :: HadoopJarStepConfig
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'StepConfig' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Text@
--
-- * @ActionOnFailure ::@ @Maybe ActionOnFailure@
--
-- * @HadoopJarStep ::@ @HadoopJarStepConfig@
--
mkStepConfig :: Text -- ^ 'scName'
             -> HadoopJarStepConfig -- ^ 'scHadoopJarStep'
             -> StepConfig
mkStepConfig p1 p3 = StepConfig
    { _scName = p1
    , _scActionOnFailure = Nothing
    , _scHadoopJarStep = p3
    }

-- | The name of the job flow step.
scName :: Lens' StepConfig Text
scName = lens _scName (\s a -> s { _scName = a })

-- | The action to take if the job flow step fails.
scActionOnFailure :: Lens' StepConfig (Maybe ActionOnFailure)
scActionOnFailure =
    lens _scActionOnFailure (\s a -> s { _scActionOnFailure = a })

-- | The JAR file used for the job flow step.
scHadoopJarStep :: Lens' StepConfig HadoopJarStepConfig
scHadoopJarStep = lens _scHadoopJarStep (\s a -> s { _scHadoopJarStep = a })

instance FromJSON StepConfig

instance ToJSON StepConfig

-- | Combines the execution state and configuration of a step.
data StepDetail = StepDetail
    { _sdStepConfig :: StepConfig
    , _sdExecutionStatusDetail :: StepExecutionStatusDetail
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'StepDetail' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StepConfig ::@ @StepConfig@
--
-- * @ExecutionStatusDetail ::@ @StepExecutionStatusDetail@
--
mkStepDetail :: StepConfig -- ^ 'sdStepConfig'
             -> StepExecutionStatusDetail -- ^ 'sdExecutionStatusDetail'
             -> StepDetail
mkStepDetail p1 p2 = StepDetail
    { _sdStepConfig = p1
    , _sdExecutionStatusDetail = p2
    }

-- | The step configuration.
sdStepConfig :: Lens' StepDetail StepConfig
sdStepConfig = lens _sdStepConfig (\s a -> s { _sdStepConfig = a })

-- | The description of the step status.
sdExecutionStatusDetail :: Lens' StepDetail StepExecutionStatusDetail
sdExecutionStatusDetail =
    lens _sdExecutionStatusDetail
         (\s a -> s { _sdExecutionStatusDetail = a })

instance FromJSON StepDetail

instance ToJSON StepDetail

-- | The description of the step status.
data StepExecutionStatusDetail = StepExecutionStatusDetail
    { _sesdState :: StepExecutionState
    , _sesdCreationDateTime :: POSIX
    , _sesdStartDateTime :: Maybe POSIX
    , _sesdEndDateTime :: Maybe POSIX
    , _sesdLastStateChangeReason :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'StepExecutionStatusDetail' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @State ::@ @StepExecutionState@
--
-- * @CreationDateTime ::@ @POSIX@
--
-- * @StartDateTime ::@ @Maybe POSIX@
--
-- * @EndDateTime ::@ @Maybe POSIX@
--
-- * @LastStateChangeReason ::@ @Maybe Text@
--
mkStepExecutionStatusDetail :: StepExecutionState -- ^ 'sesdState'
                            -> POSIX -- ^ 'sesdCreationDateTime'
                            -> StepExecutionStatusDetail
mkStepExecutionStatusDetail p1 p2 = StepExecutionStatusDetail
    { _sesdState = p1
    , _sesdCreationDateTime = p2
    , _sesdStartDateTime = Nothing
    , _sesdEndDateTime = Nothing
    , _sesdLastStateChangeReason = Nothing
    }

-- | The state of the job flow step.
sesdState :: Lens' StepExecutionStatusDetail StepExecutionState
sesdState = lens _sesdState (\s a -> s { _sesdState = a })

-- | The creation date and time of the step.
sesdCreationDateTime :: Lens' StepExecutionStatusDetail POSIX
sesdCreationDateTime =
    lens _sesdCreationDateTime (\s a -> s { _sesdCreationDateTime = a })

-- | The start date and time of the step.
sesdStartDateTime :: Lens' StepExecutionStatusDetail (Maybe POSIX)
sesdStartDateTime =
    lens _sesdStartDateTime (\s a -> s { _sesdStartDateTime = a })

-- | The completion date and time of the step.
sesdEndDateTime :: Lens' StepExecutionStatusDetail (Maybe POSIX)
sesdEndDateTime = lens _sesdEndDateTime (\s a -> s { _sesdEndDateTime = a })

-- | A description of the step's current state.
sesdLastStateChangeReason :: Lens' StepExecutionStatusDetail (Maybe Text)
sesdLastStateChangeReason =
    lens _sesdLastStateChangeReason
         (\s a -> s { _sesdLastStateChangeReason = a })

instance FromJSON StepExecutionStatusDetail

instance ToJSON StepExecutionStatusDetail

-- | The reason for the step execution status change.
data StepStateChangeReason = StepStateChangeReason
    { _sscrCode :: Maybe StepStateChangeReasonCode
    , _sscrMessage :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'StepStateChangeReason' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Code ::@ @Maybe StepStateChangeReasonCode@
--
-- * @Message ::@ @Maybe Text@
--
mkStepStateChangeReason :: StepStateChangeReason
mkStepStateChangeReason = StepStateChangeReason
    { _sscrCode = Nothing
    , _sscrMessage = Nothing
    }

-- | The programmable code for the state change reason.
sscrCode :: Lens' StepStateChangeReason (Maybe StepStateChangeReasonCode)
sscrCode = lens _sscrCode (\s a -> s { _sscrCode = a })

-- | The descriptive message for the state change reason.
sscrMessage :: Lens' StepStateChangeReason (Maybe Text)
sscrMessage = lens _sscrMessage (\s a -> s { _sscrMessage = a })

instance FromJSON StepStateChangeReason

instance ToJSON StepStateChangeReason

-- | The current execution status details of the cluster step.
data StepStatus = StepStatus
    { _ssState :: Maybe StepState
    , _ssStateChangeReason :: Maybe StepStateChangeReason
    , _ssTimeline :: Maybe StepTimeline
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'StepStatus' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @State ::@ @Maybe StepState@
--
-- * @StateChangeReason ::@ @Maybe StepStateChangeReason@
--
-- * @Timeline ::@ @Maybe StepTimeline@
--
mkStepStatus :: StepStatus
mkStepStatus = StepStatus
    { _ssState = Nothing
    , _ssStateChangeReason = Nothing
    , _ssTimeline = Nothing
    }

-- | The execution state of the cluster step.
ssState :: Lens' StepStatus (Maybe StepState)
ssState = lens _ssState (\s a -> s { _ssState = a })

-- | The reason for the step execution status change.
ssStateChangeReason :: Lens' StepStatus (Maybe StepStateChangeReason)
ssStateChangeReason =
    lens _ssStateChangeReason (\s a -> s { _ssStateChangeReason = a })

-- | The timeline of the cluster step status over time.
ssTimeline :: Lens' StepStatus (Maybe StepTimeline)
ssTimeline = lens _ssTimeline (\s a -> s { _ssTimeline = a })

instance FromJSON StepStatus

instance ToJSON StepStatus

-- | The summary of the cluster step.
data StepSummary = StepSummary
    { _ssrId :: Maybe Text
    , _ssrName :: Maybe Text
    , _ssrStatus :: Maybe StepStatus
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'StepSummary' data type.
--
-- 'StepSummary' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Id ::@ @Maybe Text@
--
-- * @Name ::@ @Maybe Text@
--
-- * @Status ::@ @Maybe StepStatus@
--
mkStepSummary :: StepSummary
mkStepSummary = StepSummary
    { _ssrId = Nothing
    , _ssrName = Nothing
    , _ssrStatus = Nothing
    }

-- | The identifier of the cluster step.
ssrId :: Lens' StepSummary (Maybe Text)
ssrId = lens _ssrId (\s a -> s { _ssrId = a })

-- | The name of the cluster step.
ssrName :: Lens' StepSummary (Maybe Text)
ssrName = lens _ssrName (\s a -> s { _ssrName = a })

-- | The current execution status details of the cluster step.
ssrStatus :: Lens' StepSummary (Maybe StepStatus)
ssrStatus = lens _ssrStatus (\s a -> s { _ssrStatus = a })

instance FromJSON StepSummary

-- | The timeline of the cluster step status over time.
data StepTimeline = StepTimeline
    { _stCreationDateTime :: Maybe POSIX
    , _stStartDateTime :: Maybe POSIX
    , _stEndDateTime :: Maybe POSIX
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'StepTimeline' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CreationDateTime ::@ @Maybe POSIX@
--
-- * @StartDateTime ::@ @Maybe POSIX@
--
-- * @EndDateTime ::@ @Maybe POSIX@
--
mkStepTimeline :: StepTimeline
mkStepTimeline = StepTimeline
    { _stCreationDateTime = Nothing
    , _stStartDateTime = Nothing
    , _stEndDateTime = Nothing
    }

-- | The date and time when the cluster step was created.
stCreationDateTime :: Lens' StepTimeline (Maybe POSIX)
stCreationDateTime =
    lens _stCreationDateTime (\s a -> s { _stCreationDateTime = a })

-- | The date and time when the cluster step execution started.
stStartDateTime :: Lens' StepTimeline (Maybe POSIX)
stStartDateTime = lens _stStartDateTime (\s a -> s { _stStartDateTime = a })

-- | The date and time when the cluster step execution completed or failed.
stEndDateTime :: Lens' StepTimeline (Maybe POSIX)
stEndDateTime = lens _stEndDateTime (\s a -> s { _stEndDateTime = a })

instance FromJSON StepTimeline

instance ToJSON StepTimeline

-- | The list of supported product configurations which allow user-supplied
-- arguments. EMR accepts these arguments and forwards them to the
-- corresponding installation script as bootstrap action arguments.
data SupportedProductConfig = SupportedProductConfig
    { _spcName :: Maybe Text
    , _spcArgs :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'SupportedProductConfig' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Maybe Text@
--
-- * @Args ::@ @[Text]@
--
mkSupportedProductConfig :: SupportedProductConfig
mkSupportedProductConfig = SupportedProductConfig
    { _spcName = Nothing
    , _spcArgs = mempty
    }

-- | The name of the product configuration.
spcName :: Lens' SupportedProductConfig (Maybe Text)
spcName = lens _spcName (\s a -> s { _spcName = a })

-- | The list of user-supplied arguments.
spcArgs :: Lens' SupportedProductConfig [Text]
spcArgs = lens _spcArgs (\s a -> s { _spcArgs = a })

instance ToJSON SupportedProductConfig

-- | A key/value pair containing user-defined metadata that you can associate
-- with an Amazon EMR resource. Tags make it easier to associate clusters in
-- various ways, such as grouping clu\ sters to track your Amazon EMR resource
-- allocation costs. For more information, see Tagging Amazon EMR Resources.
data Tag = Tag
    { _tKey :: Maybe Text
    , _tValue :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Tag' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Key ::@ @Maybe Text@
--
-- * @Value ::@ @Maybe Text@
--
mkTag :: Tag
mkTag = Tag
    { _tKey = Nothing
    , _tValue = Nothing
    }

-- | A user-defined key, which is the minimum required information for a valid
-- tag. For more information, see Tagging Amazon EMR Resources.
tKey :: Lens' Tag (Maybe Text)
tKey = lens _tKey (\s a -> s { _tKey = a })

-- | A user-defined value, which is optional in a tag. For more information, see
-- Tagging Amazon EMR Resources.
tValue :: Lens' Tag (Maybe Text)
tValue = lens _tValue (\s a -> s { _tValue = a })

instance FromJSON Tag

instance ToJSON Tag

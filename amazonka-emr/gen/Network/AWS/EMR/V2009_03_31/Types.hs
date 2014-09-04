{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EMR.V2009_03_31.Types
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
module Network.AWS.EMR.V2009_03_31.Types
    (
    -- * Service
      EMR
    -- ** Errors
    , Er (..)
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
    , cwId
    , cwName
    , cwStatus

    -- * ClusterTimeline
    , ClusterTimeline
    , mkClusterTimeline
    , cuCreationDateTime
    , cuReadyDateTime
    , cuEndDateTime

    -- * Command
    , Command
    , cdName
    , cdScriptPath
    , cdArgs

    -- * Ec2InstanceAttributes
    , Ec2InstanceAttributes
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
    , hscJar
    , hscProperties
    , hscMainClass
    , hscArgs

    -- * Instance
    , Instance
    , ieId
    , ieEc2InstanceId
    , iePublicDnsName
    , iePublicIpAddress
    , iePrivateDnsName
    , iePrivateIpAddress
    , ieStatus

    -- * InstanceGroup
    , InstanceGroup
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
    , iguState
    , iguStateChangeReason
    , iguTimeline

    -- * InstanceGroupTimeline
    , InstanceGroupTimeline
    , mkInstanceGroupTimeline
    , igwCreationDateTime
    , igwReadyDateTime
    , igwEndDateTime

    -- * InstanceStateChangeReason
    , InstanceStateChangeReason
    , mkInstanceStateChangeReason
    , iscrCode
    , iscrMessage

    -- * InstanceStatus
    , InstanceStatus
    , mkInstanceStatus
    , izState
    , izStateChangeReason
    , izTimeline

    -- * InstanceTimeline
    , InstanceTimeline
    , mkInstanceTimeline
    , iifCreationDateTime
    , iifReadyDateTime
    , iifEndDateTime

    -- * JobFlowDetail
    , JobFlowDetail
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
    , svId
    , svName
    , svConfig
    , svActionOnFailure
    , svStatus

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
    , sssState
    , sssStateChangeReason
    , sssTimeline

    -- * StepSummary
    , StepSummary
    , sssyId
    , sssyName
    , sssyStatus

    -- * StepTimeline
    , StepTimeline
    , mkStepTimeline
    , ssfCreationDateTime
    , ssfStartDateTime
    , ssfEndDateTime

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
    data Er EMR
        = EMRClient HttpException
        | EMRSerializer String
        | EMRService String
        | InternalServerError
        | InternalServerException
            { _iseMessage :: Maybe Text
            }
        | InvalidRequestException
            { _ireErrorCode :: Maybe Text
            , _ireMessage :: Maybe Text
            }

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "elasticmapreduce"
        , _svcVersion  = "2009-03-31"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er EMR)
deriving instance Generic (Er EMR)

instance AWSError (Er EMR) where
    awsError = const "EMRError"

instance AWSServiceError (Er EMR) where
    serviceError    = EMRService
    clientError     = EMRClient
    serializerError = EMRSerializer

instance Exception (Er EMR)

-- | The action to take if the job flow step fails.
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

-- | The current state of the cluster.
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

-- | The programmatic code for the state change reason.
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

-- | State of instance group. The following values are deprecated: STARTING,
-- TERMINATED, and FAILED.
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

-- | The programmable code for the state change reason.
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

-- | The type of the instance group. Valid values are MASTER, CORE or TASK.
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

-- | The role of the instance group in the cluster.
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

-- | The current state of the instance.
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

-- | The programmable code for the state change reason.
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

-- | The type of instance. A small instance A large instance.
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

-- | Market type of the Amazon EC2 instances used to create a cluster node.
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

-- | The state of the job flow step.
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

-- | The execution state of the cluster step.
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

-- | The programmable code for the state change reason.
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
      -- ^ A description of the bootstrap action.
    } deriving (Show, Generic)

-- | A description of the bootstrap action.
badBootstrapActionConfig :: Lens' BootstrapActionDetail (Maybe BootstrapActionConfig)
badBootstrapActionConfig = lens _badBootstrapActionConfig (\s a -> s { _badBootstrapActionConfig = a })
{-# INLINE badBootstrapActionConfig #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'BootstrapActionDetail' data type to populate a request.
mkBootstrapActionDetail :: BootstrapActionDetail
mkBootstrapActionDetail = BootstrapActionDetail
    { _badBootstrapActionConfig = Nothing
    }
{-# INLINE mkBootstrapActionDetail #-}

instance FromJSON BootstrapActionDetail

instance ToJSON BootstrapActionDetail

-- | The Amazon EC2 Availability Zone for the job flow.
newtype PlacementType = PlacementType
    { _ptAvailabilityZone :: Text
      -- ^ The Amazon EC2 Availability Zone for the job flow.
    } deriving (Show, Generic)

-- | The Amazon EC2 Availability Zone for the job flow.
ptAvailabilityZone :: Lens' PlacementType (Text)
ptAvailabilityZone = lens _ptAvailabilityZone (\s a -> s { _ptAvailabilityZone = a })
{-# INLINE ptAvailabilityZone #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PlacementType' data type to populate a request.
mkPlacementType :: Text -- ^ 'ptAvailabilityZone'
                -> PlacementType
mkPlacementType p1 = PlacementType
    { _ptAvailabilityZone = p1
    }
{-# INLINE mkPlacementType #-}

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
      -- ^ The name of the application.
    , _aVersion :: Maybe Text
      -- ^ The version of the application.
    , _aArgs :: [Text]
      -- ^ Arguments for Amazon EMR to pass to the application.
    , _aAdditionalInfo :: Map Text Text
      -- ^ This option is for advanced users only. This is meta information
      -- about third-party applications that third-party vendors use for
      -- testing purposes.
    } deriving (Show, Generic)

-- | The name of the application.
aName :: Lens' Application (Maybe Text)
aName = lens _aName (\s a -> s { _aName = a })
{-# INLINE aName #-}

-- | The version of the application.
aVersion :: Lens' Application (Maybe Text)
aVersion = lens _aVersion (\s a -> s { _aVersion = a })
{-# INLINE aVersion #-}

-- | Arguments for Amazon EMR to pass to the application.
aArgs :: Lens' Application ([Text])
aArgs = lens _aArgs (\s a -> s { _aArgs = a })
{-# INLINE aArgs #-}

-- | This option is for advanced users only. This is meta information about
-- third-party applications that third-party vendors use for testing purposes.
aAdditionalInfo :: Lens' Application (Map Text Text)
aAdditionalInfo = lens _aAdditionalInfo (\s a -> s { _aAdditionalInfo = a })
{-# INLINE aAdditionalInfo #-}

instance FromJSON Application

-- | A description of the bootstrap action.
data BootstrapActionConfig = BootstrapActionConfig
    { _bacName :: Text
      -- ^ The name of the bootstrap action.
    , _bacScriptBootstrapAction :: ScriptBootstrapActionConfig
      -- ^ The script run by the bootstrap action.
    } deriving (Show, Generic)

-- | The name of the bootstrap action.
bacName :: Lens' BootstrapActionConfig (Text)
bacName = lens _bacName (\s a -> s { _bacName = a })
{-# INLINE bacName #-}

-- | The script run by the bootstrap action.
bacScriptBootstrapAction :: Lens' BootstrapActionConfig (ScriptBootstrapActionConfig)
bacScriptBootstrapAction = lens _bacScriptBootstrapAction (\s a -> s { _bacScriptBootstrapAction = a })
{-# INLINE bacScriptBootstrapAction #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'BootstrapActionConfig' data type to populate a request.
mkBootstrapActionConfig :: Text -- ^ 'bacName'
                        -> ScriptBootstrapActionConfig -- ^ 'bacScriptBootstrapAction'
                        -> BootstrapActionConfig
mkBootstrapActionConfig p1 p2 = BootstrapActionConfig
    { _bacName = p1
    , _bacScriptBootstrapAction = p2
    }
{-# INLINE mkBootstrapActionConfig #-}

instance FromJSON BootstrapActionConfig

instance ToJSON BootstrapActionConfig

-- | This output contains the details for the requested cluster.
data Cluster = Cluster
    { _cId :: Maybe Text
      -- ^ The unique identifier for the cluster.
    , _cName :: Maybe Text
      -- ^ The name of the cluster.
    , _cStatus :: Maybe ClusterStatus
      -- ^ The current status details about the cluster.
    , _cEc2InstanceAttributes :: Maybe Ec2InstanceAttributes
      -- ^ Provides information about the EC2 instances in a cluster grouped
      -- by category. For example, key name, subnet ID, IAM instance
      -- profile, and so on.
    , _cLogUri :: Maybe Text
      -- ^ The path to the Amazon S3 location where logs for this cluster
      -- are stored.
    , _cRequestedAmiVersion :: Maybe Text
      -- ^ The AMI version requested for this
      -- cluster.JobFlowDetail$AmiVersion.-->.
    , _cRunningAmiVersion :: Maybe Text
      -- ^ The AMI version running on this cluster. This differs from the
      -- requested version only if the requested version is a meta
      -- version, such as "latest". JobFlowDetail$AmiVersion.-->.
    , _cAutoTerminate :: Maybe Bool
      -- ^ Specifies whether the cluster should terminate after completing
      -- all steps.
    , _cTerminationProtected :: Maybe Bool
      -- ^ Indicates whether Amazon EMR will lock the cluster to prevent the
      -- EC2 instances from being terminated by an API call or user
      -- intervention, or in the event of a cluster error.
    , _cVisibleToAllUsers :: Maybe Bool
      -- ^ Indicates whether the job flow is visible to all IAM users of the
      -- AWS account associated with the job flow. If this value is set to
      -- true, all IAM users of that AWS account can view and manage the
      -- job flow if they have the proper policy permissions set. If this
      -- value is false, only the IAM user that created the cluster can
      -- view and manage it. This value can be changed using the
      -- SetVisibleToAllUsers action.
    , _cApplications :: [Application]
      -- ^ The applications installed on this cluster.
    , _cTags :: [Tag]
      -- ^ A list of tags associated with a cluster.
    , _cServiceRole :: Maybe Text
      -- ^ The IAM role that will be assumed by the Amazon EMR service to
      -- access AWS resources on your behalf.
    } deriving (Show, Generic)

-- | The unique identifier for the cluster.
cId :: Lens' Cluster (Maybe Text)
cId = lens _cId (\s a -> s { _cId = a })
{-# INLINE cId #-}

-- | The name of the cluster.
cName :: Lens' Cluster (Maybe Text)
cName = lens _cName (\s a -> s { _cName = a })
{-# INLINE cName #-}

-- | The current status details about the cluster.
cStatus :: Lens' Cluster (Maybe ClusterStatus)
cStatus = lens _cStatus (\s a -> s { _cStatus = a })
{-# INLINE cStatus #-}

-- | Provides information about the EC2 instances in a cluster grouped by
-- category. For example, key name, subnet ID, IAM instance profile, and so
-- on.
cEc2InstanceAttributes :: Lens' Cluster (Maybe Ec2InstanceAttributes)
cEc2InstanceAttributes = lens _cEc2InstanceAttributes (\s a -> s { _cEc2InstanceAttributes = a })
{-# INLINE cEc2InstanceAttributes #-}

-- | The path to the Amazon S3 location where logs for this cluster are stored.
cLogUri :: Lens' Cluster (Maybe Text)
cLogUri = lens _cLogUri (\s a -> s { _cLogUri = a })
{-# INLINE cLogUri #-}

-- | The AMI version requested for this cluster.JobFlowDetail$AmiVersion.-->.
cRequestedAmiVersion :: Lens' Cluster (Maybe Text)
cRequestedAmiVersion = lens _cRequestedAmiVersion (\s a -> s { _cRequestedAmiVersion = a })
{-# INLINE cRequestedAmiVersion #-}

-- | The AMI version running on this cluster. This differs from the requested
-- version only if the requested version is a meta version, such as "latest".
-- JobFlowDetail$AmiVersion.-->.
cRunningAmiVersion :: Lens' Cluster (Maybe Text)
cRunningAmiVersion = lens _cRunningAmiVersion (\s a -> s { _cRunningAmiVersion = a })
{-# INLINE cRunningAmiVersion #-}

-- | Specifies whether the cluster should terminate after completing all steps.
cAutoTerminate :: Lens' Cluster (Maybe Bool)
cAutoTerminate = lens _cAutoTerminate (\s a -> s { _cAutoTerminate = a })
{-# INLINE cAutoTerminate #-}

-- | Indicates whether Amazon EMR will lock the cluster to prevent the EC2
-- instances from being terminated by an API call or user intervention, or in
-- the event of a cluster error.
cTerminationProtected :: Lens' Cluster (Maybe Bool)
cTerminationProtected = lens _cTerminationProtected (\s a -> s { _cTerminationProtected = a })
{-# INLINE cTerminationProtected #-}

-- | Indicates whether the job flow is visible to all IAM users of the AWS
-- account associated with the job flow. If this value is set to true, all IAM
-- users of that AWS account can view and manage the job flow if they have the
-- proper policy permissions set. If this value is false, only the IAM user
-- that created the cluster can view and manage it. This value can be changed
-- using the SetVisibleToAllUsers action.
cVisibleToAllUsers :: Lens' Cluster (Maybe Bool)
cVisibleToAllUsers = lens _cVisibleToAllUsers (\s a -> s { _cVisibleToAllUsers = a })
{-# INLINE cVisibleToAllUsers #-}

-- | The applications installed on this cluster.
cApplications :: Lens' Cluster ([Application])
cApplications = lens _cApplications (\s a -> s { _cApplications = a })
{-# INLINE cApplications #-}

-- | A list of tags associated with a cluster.
cTags :: Lens' Cluster ([Tag])
cTags = lens _cTags (\s a -> s { _cTags = a })
{-# INLINE cTags #-}

-- | The IAM role that will be assumed by the Amazon EMR service to access AWS
-- resources on your behalf.
cServiceRole :: Lens' Cluster (Maybe Text)
cServiceRole = lens _cServiceRole (\s a -> s { _cServiceRole = a })
{-# INLINE cServiceRole #-}

instance FromJSON Cluster

-- | The reason for the cluster status change.
data ClusterStateChangeReason = ClusterStateChangeReason
    { _cscrCode :: Maybe ClusterStateChangeReasonCode
      -- ^ The programmatic code for the state change reason.
    , _cscrMessage :: Maybe Text
      -- ^ The descriptive message for the state change reason.
    } deriving (Show, Generic)

-- | The programmatic code for the state change reason.
cscrCode :: Lens' ClusterStateChangeReason (Maybe ClusterStateChangeReasonCode)
cscrCode = lens _cscrCode (\s a -> s { _cscrCode = a })
{-# INLINE cscrCode #-}

-- | The descriptive message for the state change reason.
cscrMessage :: Lens' ClusterStateChangeReason (Maybe Text)
cscrMessage = lens _cscrMessage (\s a -> s { _cscrMessage = a })
{-# INLINE cscrMessage #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ClusterStateChangeReason' data type to populate a request.
mkClusterStateChangeReason :: ClusterStateChangeReason
mkClusterStateChangeReason = ClusterStateChangeReason
    { _cscrCode = Nothing
    , _cscrMessage = Nothing
    }
{-# INLINE mkClusterStateChangeReason #-}

instance FromJSON ClusterStateChangeReason

instance ToJSON ClusterStateChangeReason

-- | The current status details about the cluster.
data ClusterStatus = ClusterStatus
    { _csState :: Maybe ClusterState
      -- ^ The current state of the cluster.
    , _csStateChangeReason :: Maybe ClusterStateChangeReason
      -- ^ The reason for the cluster status change.
    , _csTimeline :: Maybe ClusterTimeline
      -- ^ A timeline that represents the status of a cluster over the
      -- lifetime of the cluster.
    } deriving (Show, Generic)

-- | The current state of the cluster.
csState :: Lens' ClusterStatus (Maybe ClusterState)
csState = lens _csState (\s a -> s { _csState = a })
{-# INLINE csState #-}

-- | The reason for the cluster status change.
csStateChangeReason :: Lens' ClusterStatus (Maybe ClusterStateChangeReason)
csStateChangeReason = lens _csStateChangeReason (\s a -> s { _csStateChangeReason = a })
{-# INLINE csStateChangeReason #-}

-- | A timeline that represents the status of a cluster over the lifetime of the
-- cluster.
csTimeline :: Lens' ClusterStatus (Maybe ClusterTimeline)
csTimeline = lens _csTimeline (\s a -> s { _csTimeline = a })
{-# INLINE csTimeline #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ClusterStatus' data type to populate a request.
mkClusterStatus :: ClusterStatus
mkClusterStatus = ClusterStatus
    { _csState = Nothing
    , _csStateChangeReason = Nothing
    , _csTimeline = Nothing
    }
{-# INLINE mkClusterStatus #-}

instance FromJSON ClusterStatus

instance ToJSON ClusterStatus

-- | The summary description of the cluster.
data ClusterSummary = ClusterSummary
    { _cwId :: Maybe Text
      -- ^ The unique identifier for the cluster.
    , _cwName :: Maybe Text
      -- ^ The name of the cluster.
    , _cwStatus :: Maybe ClusterStatus
      -- ^ The details about the current status of the cluster.
    } deriving (Show, Generic)

-- | The unique identifier for the cluster.
cwId :: Lens' ClusterSummary (Maybe Text)
cwId = lens _cwId (\s a -> s { _cwId = a })
{-# INLINE cwId #-}

-- | The name of the cluster.
cwName :: Lens' ClusterSummary (Maybe Text)
cwName = lens _cwName (\s a -> s { _cwName = a })
{-# INLINE cwName #-}

-- | The details about the current status of the cluster.
cwStatus :: Lens' ClusterSummary (Maybe ClusterStatus)
cwStatus = lens _cwStatus (\s a -> s { _cwStatus = a })
{-# INLINE cwStatus #-}

instance FromJSON ClusterSummary

-- | A timeline that represents the status of a cluster over the lifetime of the
-- cluster.
data ClusterTimeline = ClusterTimeline
    { _cuCreationDateTime :: Maybe POSIX
      -- ^ The creation date and time of the cluster.
    , _cuReadyDateTime :: Maybe POSIX
      -- ^ The date and time when the cluster was ready to execute steps.
    , _cuEndDateTime :: Maybe POSIX
      -- ^ The date and time when the cluster was terminated.
    } deriving (Show, Generic)

-- | The creation date and time of the cluster.
cuCreationDateTime :: Lens' ClusterTimeline (Maybe POSIX)
cuCreationDateTime = lens _cuCreationDateTime (\s a -> s { _cuCreationDateTime = a })
{-# INLINE cuCreationDateTime #-}

-- | The date and time when the cluster was ready to execute steps.
cuReadyDateTime :: Lens' ClusterTimeline (Maybe POSIX)
cuReadyDateTime = lens _cuReadyDateTime (\s a -> s { _cuReadyDateTime = a })
{-# INLINE cuReadyDateTime #-}

-- | The date and time when the cluster was terminated.
cuEndDateTime :: Lens' ClusterTimeline (Maybe POSIX)
cuEndDateTime = lens _cuEndDateTime (\s a -> s { _cuEndDateTime = a })
{-# INLINE cuEndDateTime #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ClusterTimeline' data type to populate a request.
mkClusterTimeline :: ClusterTimeline
mkClusterTimeline = ClusterTimeline
    { _cuCreationDateTime = Nothing
    , _cuReadyDateTime = Nothing
    , _cuEndDateTime = Nothing
    }
{-# INLINE mkClusterTimeline #-}

instance FromJSON ClusterTimeline

instance ToJSON ClusterTimeline

-- | An entity describing an executable that runs on a cluster.
data Command = Command
    { _cdName :: Maybe Text
      -- ^ The name of the command.
    , _cdScriptPath :: Maybe Text
      -- ^ The Amazon S3 location of the command script.
    , _cdArgs :: [Text]
      -- ^ Arguments for Amazon EMR to pass to the command for execution.
    } deriving (Show, Generic)

-- | The name of the command.
cdName :: Lens' Command (Maybe Text)
cdName = lens _cdName (\s a -> s { _cdName = a })
{-# INLINE cdName #-}

-- | The Amazon S3 location of the command script.
cdScriptPath :: Lens' Command (Maybe Text)
cdScriptPath = lens _cdScriptPath (\s a -> s { _cdScriptPath = a })
{-# INLINE cdScriptPath #-}

-- | Arguments for Amazon EMR to pass to the command for execution.
cdArgs :: Lens' Command ([Text])
cdArgs = lens _cdArgs (\s a -> s { _cdArgs = a })
{-# INLINE cdArgs #-}

instance FromJSON Command

-- | Provides information about the EC2 instances in a cluster grouped by
-- category. For example, key name, subnet ID, IAM instance profile, and so
-- on.
data Ec2InstanceAttributes = Ec2InstanceAttributes
    { _eiaEc2KeyName :: Maybe Text
      -- ^ The name of the Amazon EC2 key pair to use when connecting with
      -- SSH into the master node as a user named "hadoop".
    , _eiaEc2SubnetId :: Maybe Text
      -- ^ To launch the job flow in Amazon VPC, set this parameter to the
      -- identifier of the Amazon VPC subnet where you want the job flow
      -- to launch. If you do not specify this value, the job flow is
      -- launched in the normal AWS cloud, outside of a VPC. Amazon VPC
      -- currently does not support cluster compute quadruple extra large
      -- (cc1.4xlarge) instances. Thus, you cannot specify the cc1.4xlarge
      -- instance type for nodes of a job flow launched in a VPC.
    , _eiaEc2AvailabilityZone :: Maybe Text
      -- ^ The Availability Zone in which the cluster will run.
    , _eiaIamInstanceProfile :: Maybe Text
      -- ^ The IAM role that was specified when the job flow was launched.
      -- The EC2 instances of the job flow assume this role.
    } deriving (Show, Generic)

-- | The name of the Amazon EC2 key pair to use when connecting with SSH into
-- the master node as a user named "hadoop".
eiaEc2KeyName :: Lens' Ec2InstanceAttributes (Maybe Text)
eiaEc2KeyName = lens _eiaEc2KeyName (\s a -> s { _eiaEc2KeyName = a })
{-# INLINE eiaEc2KeyName #-}

-- | To launch the job flow in Amazon VPC, set this parameter to the identifier
-- of the Amazon VPC subnet where you want the job flow to launch. If you do
-- not specify this value, the job flow is launched in the normal AWS cloud,
-- outside of a VPC. Amazon VPC currently does not support cluster compute
-- quadruple extra large (cc1.4xlarge) instances. Thus, you cannot specify the
-- cc1.4xlarge instance type for nodes of a job flow launched in a VPC.
eiaEc2SubnetId :: Lens' Ec2InstanceAttributes (Maybe Text)
eiaEc2SubnetId = lens _eiaEc2SubnetId (\s a -> s { _eiaEc2SubnetId = a })
{-# INLINE eiaEc2SubnetId #-}

-- | The Availability Zone in which the cluster will run.
eiaEc2AvailabilityZone :: Lens' Ec2InstanceAttributes (Maybe Text)
eiaEc2AvailabilityZone = lens _eiaEc2AvailabilityZone (\s a -> s { _eiaEc2AvailabilityZone = a })
{-# INLINE eiaEc2AvailabilityZone #-}

-- | The IAM role that was specified when the job flow was launched. The EC2
-- instances of the job flow assume this role.
eiaIamInstanceProfile :: Lens' Ec2InstanceAttributes (Maybe Text)
eiaIamInstanceProfile = lens _eiaIamInstanceProfile (\s a -> s { _eiaIamInstanceProfile = a })
{-# INLINE eiaIamInstanceProfile #-}

instance FromJSON Ec2InstanceAttributes

-- | The JAR file used for the job flow step.
data HadoopJarStepConfig = HadoopJarStepConfig
    { _hjscProperties :: [KeyValue]
      -- ^ A list of Java properties that are set when the step runs. You
      -- can use these properties to pass key value pairs to your main
      -- function.
    , _hjscJar :: Text
      -- ^ A path to a JAR file run during the step.
    , _hjscMainClass :: Maybe Text
      -- ^ The name of the main class in the specified Java file. If not
      -- specified, the JAR file should specify a Main-Class in its
      -- manifest file.
    , _hjscArgs :: [Text]
      -- ^ A list of command line arguments passed to the JAR file's main
      -- function when executed.
    } deriving (Show, Generic)

-- | A list of Java properties that are set when the step runs. You can use
-- these properties to pass key value pairs to your main function.
hjscProperties :: Lens' HadoopJarStepConfig ([KeyValue])
hjscProperties = lens _hjscProperties (\s a -> s { _hjscProperties = a })
{-# INLINE hjscProperties #-}

-- | A path to a JAR file run during the step.
hjscJar :: Lens' HadoopJarStepConfig (Text)
hjscJar = lens _hjscJar (\s a -> s { _hjscJar = a })
{-# INLINE hjscJar #-}

-- | The name of the main class in the specified Java file. If not specified,
-- the JAR file should specify a Main-Class in its manifest file.
hjscMainClass :: Lens' HadoopJarStepConfig (Maybe Text)
hjscMainClass = lens _hjscMainClass (\s a -> s { _hjscMainClass = a })
{-# INLINE hjscMainClass #-}

-- | A list of command line arguments passed to the JAR file's main function
-- when executed.
hjscArgs :: Lens' HadoopJarStepConfig ([Text])
hjscArgs = lens _hjscArgs (\s a -> s { _hjscArgs = a })
{-# INLINE hjscArgs #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'HadoopJarStepConfig' data type to populate a request.
mkHadoopJarStepConfig :: Text -- ^ 'hjscJar'
                      -> HadoopJarStepConfig
mkHadoopJarStepConfig p1 = HadoopJarStepConfig
    { _hjscProperties = mempty
    , _hjscJar = p2
    , _hjscMainClass = Nothing
    , _hjscArgs = mempty
    }
{-# INLINE mkHadoopJarStepConfig #-}

instance FromJSON HadoopJarStepConfig

instance ToJSON HadoopJarStepConfig

-- | The Hadoop job configuration of the cluster step.
data HadoopStepConfig = HadoopStepConfig
    { _hscJar :: Maybe Text
      -- ^ The path to the JAR file that runs during the step.
    , _hscProperties :: Map Text Text
      -- ^ The list of Java properties that are set when the step runs. You
      -- can use these properties to pass key value pairs to your main
      -- function.
    , _hscMainClass :: Maybe Text
      -- ^ The name of the main class in the specified Java file. If not
      -- specified, the JAR file should specify a main class in its
      -- manifest file.
    , _hscArgs :: [Text]
      -- ^ The list of command line arguments to pass to the JAR file's main
      -- function for execution.
    } deriving (Show, Generic)

-- | The path to the JAR file that runs during the step.
hscJar :: Lens' HadoopStepConfig (Maybe Text)
hscJar = lens _hscJar (\s a -> s { _hscJar = a })
{-# INLINE hscJar #-}

-- | The list of Java properties that are set when the step runs. You can use
-- these properties to pass key value pairs to your main function.
hscProperties :: Lens' HadoopStepConfig (Map Text Text)
hscProperties = lens _hscProperties (\s a -> s { _hscProperties = a })
{-# INLINE hscProperties #-}

-- | The name of the main class in the specified Java file. If not specified,
-- the JAR file should specify a main class in its manifest file.
hscMainClass :: Lens' HadoopStepConfig (Maybe Text)
hscMainClass = lens _hscMainClass (\s a -> s { _hscMainClass = a })
{-# INLINE hscMainClass #-}

-- | The list of command line arguments to pass to the JAR file's main function
-- for execution.
hscArgs :: Lens' HadoopStepConfig ([Text])
hscArgs = lens _hscArgs (\s a -> s { _hscArgs = a })
{-# INLINE hscArgs #-}

instance FromJSON HadoopStepConfig

-- | Represents an EC2 instance provisioned as part of cluster.
data Instance = Instance
    { _ieId :: Maybe Text
      -- ^ The unique identifier for the instance in Amazon EMR.
    , _ieEc2InstanceId :: Maybe Text
      -- ^ The unique identifier of the instance in Amazon EC2.
    , _iePublicDnsName :: Maybe Text
      -- ^ The public DNS name of the instance.
    , _iePublicIpAddress :: Maybe Text
      -- ^ The public IP address of the instance.
    , _iePrivateDnsName :: Maybe Text
      -- ^ The private DNS name of the instance.
    , _iePrivateIpAddress :: Maybe Text
      -- ^ The private IP address of the instance.
    , _ieStatus :: Maybe InstanceStatus
      -- ^ The current status of the instance.
    } deriving (Show, Generic)

-- | The unique identifier for the instance in Amazon EMR.
ieId :: Lens' Instance (Maybe Text)
ieId = lens _ieId (\s a -> s { _ieId = a })
{-# INLINE ieId #-}

-- | The unique identifier of the instance in Amazon EC2.
ieEc2InstanceId :: Lens' Instance (Maybe Text)
ieEc2InstanceId = lens _ieEc2InstanceId (\s a -> s { _ieEc2InstanceId = a })
{-# INLINE ieEc2InstanceId #-}

-- | The public DNS name of the instance.
iePublicDnsName :: Lens' Instance (Maybe Text)
iePublicDnsName = lens _iePublicDnsName (\s a -> s { _iePublicDnsName = a })
{-# INLINE iePublicDnsName #-}

-- | The public IP address of the instance.
iePublicIpAddress :: Lens' Instance (Maybe Text)
iePublicIpAddress = lens _iePublicIpAddress (\s a -> s { _iePublicIpAddress = a })
{-# INLINE iePublicIpAddress #-}

-- | The private DNS name of the instance.
iePrivateDnsName :: Lens' Instance (Maybe Text)
iePrivateDnsName = lens _iePrivateDnsName (\s a -> s { _iePrivateDnsName = a })
{-# INLINE iePrivateDnsName #-}

-- | The private IP address of the instance.
iePrivateIpAddress :: Lens' Instance (Maybe Text)
iePrivateIpAddress = lens _iePrivateIpAddress (\s a -> s { _iePrivateIpAddress = a })
{-# INLINE iePrivateIpAddress #-}

-- | The current status of the instance.
ieStatus :: Lens' Instance (Maybe InstanceStatus)
ieStatus = lens _ieStatus (\s a -> s { _ieStatus = a })
{-# INLINE ieStatus #-}

instance FromJSON Instance

-- | This entity represents an instance group, which is a group of instances
-- that have common purpose. For example, CORE instance group is used for
-- HDFS.
data InstanceGroup = InstanceGroup
    { _igId :: Maybe Text
      -- ^ The identifier of the instance group.
    , _igName :: Maybe Text
      -- ^ The name of the instance group.
    , _igMarket :: Maybe MarketType
      -- ^ The marketplace to provision instances for this group. Valid
      -- values are ON_DEMAND or SPOT.
    , _igInstanceGroupType :: Maybe InstanceGroupType
      -- ^ The type of the instance group. Valid values are MASTER, CORE or
      -- TASK.
    , _igBidPrice :: Maybe Text
      -- ^ The bid price for each EC2 instance in the instance group when
      -- launching nodes as Spot Instances, expressed in USD.
    , _igInstanceType :: Maybe Text
      -- ^ The EC2 instance type for all instances in the instance group.
    , _igRequestedInstanceCount :: Maybe Integer
      -- ^ The target number of instances for the instance group.
    , _igRunningInstanceCount :: Maybe Integer
      -- ^ The number of instances currently running in this instance group.
    , _igStatus :: Maybe InstanceGroupStatus
      -- ^ The current status of the instance group.
    } deriving (Show, Generic)

-- | The identifier of the instance group.
igId :: Lens' InstanceGroup (Maybe Text)
igId = lens _igId (\s a -> s { _igId = a })
{-# INLINE igId #-}

-- | The name of the instance group.
igName :: Lens' InstanceGroup (Maybe Text)
igName = lens _igName (\s a -> s { _igName = a })
{-# INLINE igName #-}

-- | The marketplace to provision instances for this group. Valid values are
-- ON_DEMAND or SPOT.
igMarket :: Lens' InstanceGroup (Maybe MarketType)
igMarket = lens _igMarket (\s a -> s { _igMarket = a })
{-# INLINE igMarket #-}

-- | The type of the instance group. Valid values are MASTER, CORE or TASK.
igInstanceGroupType :: Lens' InstanceGroup (Maybe InstanceGroupType)
igInstanceGroupType = lens _igInstanceGroupType (\s a -> s { _igInstanceGroupType = a })
{-# INLINE igInstanceGroupType #-}

-- | The bid price for each EC2 instance in the instance group when launching
-- nodes as Spot Instances, expressed in USD.
igBidPrice :: Lens' InstanceGroup (Maybe Text)
igBidPrice = lens _igBidPrice (\s a -> s { _igBidPrice = a })
{-# INLINE igBidPrice #-}

-- | The EC2 instance type for all instances in the instance group.
igInstanceType :: Lens' InstanceGroup (Maybe Text)
igInstanceType = lens _igInstanceType (\s a -> s { _igInstanceType = a })
{-# INLINE igInstanceType #-}

-- | The target number of instances for the instance group.
igRequestedInstanceCount :: Lens' InstanceGroup (Maybe Integer)
igRequestedInstanceCount = lens _igRequestedInstanceCount (\s a -> s { _igRequestedInstanceCount = a })
{-# INLINE igRequestedInstanceCount #-}

-- | The number of instances currently running in this instance group.
igRunningInstanceCount :: Lens' InstanceGroup (Maybe Integer)
igRunningInstanceCount = lens _igRunningInstanceCount (\s a -> s { _igRunningInstanceCount = a })
{-# INLINE igRunningInstanceCount #-}

-- | The current status of the instance group.
igStatus :: Lens' InstanceGroup (Maybe InstanceGroupStatus)
igStatus = lens _igStatus (\s a -> s { _igStatus = a })
{-# INLINE igStatus #-}

instance FromJSON InstanceGroup

-- | Configuration defining a new instance group.
data InstanceGroupConfig = InstanceGroupConfig
    { _igcName :: Maybe Text
      -- ^ Friendly name given to the instance group.
    , _igcMarket :: Maybe MarketType
      -- ^ Market type of the Amazon EC2 instances used to create a cluster
      -- node.
    , _igcInstanceRole :: InstanceRoleType
      -- ^ The role of the instance group in the cluster.
    , _igcBidPrice :: Maybe Text
      -- ^ Bid price for each Amazon EC2 instance in the instance group when
      -- launching nodes as Spot Instances, expressed in USD.
    , _igcInstanceType :: Text
      -- ^ The Amazon EC2 instance type for all instances in the instance
      -- group.
    , _igcInstanceCount :: Integer
      -- ^ Target number of instances for the instance group.
    } deriving (Show, Generic)

-- | Friendly name given to the instance group.
igcName :: Lens' InstanceGroupConfig (Maybe Text)
igcName = lens _igcName (\s a -> s { _igcName = a })
{-# INLINE igcName #-}

-- | Market type of the Amazon EC2 instances used to create a cluster node.
igcMarket :: Lens' InstanceGroupConfig (Maybe MarketType)
igcMarket = lens _igcMarket (\s a -> s { _igcMarket = a })
{-# INLINE igcMarket #-}

-- | The role of the instance group in the cluster.
igcInstanceRole :: Lens' InstanceGroupConfig (InstanceRoleType)
igcInstanceRole = lens _igcInstanceRole (\s a -> s { _igcInstanceRole = a })
{-# INLINE igcInstanceRole #-}

-- | Bid price for each Amazon EC2 instance in the instance group when launching
-- nodes as Spot Instances, expressed in USD.
igcBidPrice :: Lens' InstanceGroupConfig (Maybe Text)
igcBidPrice = lens _igcBidPrice (\s a -> s { _igcBidPrice = a })
{-# INLINE igcBidPrice #-}

-- | The Amazon EC2 instance type for all instances in the instance group.
igcInstanceType :: Lens' InstanceGroupConfig (Text)
igcInstanceType = lens _igcInstanceType (\s a -> s { _igcInstanceType = a })
{-# INLINE igcInstanceType #-}

-- | Target number of instances for the instance group.
igcInstanceCount :: Lens' InstanceGroupConfig (Integer)
igcInstanceCount = lens _igcInstanceCount (\s a -> s { _igcInstanceCount = a })
{-# INLINE igcInstanceCount #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceGroupConfig' data type to populate a request.
mkInstanceGroupConfig :: InstanceRoleType -- ^ 'igcInstanceRole'
                      -> Text -- ^ 'igcInstanceType'
                      -> Integer -- ^ 'igcInstanceCount'
                      -> InstanceGroupConfig
mkInstanceGroupConfig p1 p2 p3 = InstanceGroupConfig
    { _igcName = Nothing
    , _igcMarket = Nothing
    , _igcInstanceRole = p3
    , _igcBidPrice = Nothing
    , _igcInstanceType = p5
    , _igcInstanceCount = p6
    }
{-# INLINE mkInstanceGroupConfig #-}

instance ToJSON InstanceGroupConfig

-- | Detailed information about an instance group.
data InstanceGroupDetail = InstanceGroupDetail
    { _igdInstanceGroupId :: Maybe Text
      -- ^ Unique identifier for the instance group.
    , _igdName :: Maybe Text
      -- ^ Friendly name for the instance group.
    , _igdMarket :: MarketType
      -- ^ Market type of the Amazon EC2 instances used to create a cluster
      -- node.
    , _igdInstanceRole :: InstanceRoleType
      -- ^ Instance group role in the cluster.
    , _igdBidPrice :: Maybe Text
      -- ^ Bid price for EC2 Instances when launching nodes as Spot
      -- Instances, expressed in USD.
    , _igdInstanceType :: Text
      -- ^ Amazon EC2 Instance type.
    , _igdInstanceRequestCount :: Integer
      -- ^ Target number of instances to run in the instance group.
    , _igdInstanceRunningCount :: Integer
      -- ^ Actual count of running instances.
    , _igdState :: InstanceGroupState
      -- ^ State of instance group. The following values are deprecated:
      -- STARTING, TERMINATED, and FAILED.
    , _igdLastStateChangeReason :: Maybe Text
      -- ^ Details regarding the state of the instance group.
    , _igdCreationDateTime :: POSIX
      -- ^ The date/time the instance group was created.
    , _igdStartDateTime :: Maybe POSIX
      -- ^ The date/time the instance group was started.
    , _igdReadyDateTime :: Maybe POSIX
      -- ^ The date/time the instance group was available to the cluster.
    , _igdEndDateTime :: Maybe POSIX
      -- ^ The date/time the instance group was terminated.
    } deriving (Show, Generic)

-- | Unique identifier for the instance group.
igdInstanceGroupId :: Lens' InstanceGroupDetail (Maybe Text)
igdInstanceGroupId = lens _igdInstanceGroupId (\s a -> s { _igdInstanceGroupId = a })
{-# INLINE igdInstanceGroupId #-}

-- | Friendly name for the instance group.
igdName :: Lens' InstanceGroupDetail (Maybe Text)
igdName = lens _igdName (\s a -> s { _igdName = a })
{-# INLINE igdName #-}

-- | Market type of the Amazon EC2 instances used to create a cluster node.
igdMarket :: Lens' InstanceGroupDetail (MarketType)
igdMarket = lens _igdMarket (\s a -> s { _igdMarket = a })
{-# INLINE igdMarket #-}

-- | Instance group role in the cluster.
igdInstanceRole :: Lens' InstanceGroupDetail (InstanceRoleType)
igdInstanceRole = lens _igdInstanceRole (\s a -> s { _igdInstanceRole = a })
{-# INLINE igdInstanceRole #-}

-- | Bid price for EC2 Instances when launching nodes as Spot Instances,
-- expressed in USD.
igdBidPrice :: Lens' InstanceGroupDetail (Maybe Text)
igdBidPrice = lens _igdBidPrice (\s a -> s { _igdBidPrice = a })
{-# INLINE igdBidPrice #-}

-- | Amazon EC2 Instance type.
igdInstanceType :: Lens' InstanceGroupDetail (Text)
igdInstanceType = lens _igdInstanceType (\s a -> s { _igdInstanceType = a })
{-# INLINE igdInstanceType #-}

-- | Target number of instances to run in the instance group.
igdInstanceRequestCount :: Lens' InstanceGroupDetail (Integer)
igdInstanceRequestCount = lens _igdInstanceRequestCount (\s a -> s { _igdInstanceRequestCount = a })
{-# INLINE igdInstanceRequestCount #-}

-- | Actual count of running instances.
igdInstanceRunningCount :: Lens' InstanceGroupDetail (Integer)
igdInstanceRunningCount = lens _igdInstanceRunningCount (\s a -> s { _igdInstanceRunningCount = a })
{-# INLINE igdInstanceRunningCount #-}

-- | State of instance group. The following values are deprecated: STARTING,
-- TERMINATED, and FAILED.
igdState :: Lens' InstanceGroupDetail (InstanceGroupState)
igdState = lens _igdState (\s a -> s { _igdState = a })
{-# INLINE igdState #-}

-- | Details regarding the state of the instance group.
igdLastStateChangeReason :: Lens' InstanceGroupDetail (Maybe Text)
igdLastStateChangeReason = lens _igdLastStateChangeReason (\s a -> s { _igdLastStateChangeReason = a })
{-# INLINE igdLastStateChangeReason #-}

-- | The date/time the instance group was created.
igdCreationDateTime :: Lens' InstanceGroupDetail (POSIX)
igdCreationDateTime = lens _igdCreationDateTime (\s a -> s { _igdCreationDateTime = a })
{-# INLINE igdCreationDateTime #-}

-- | The date/time the instance group was started.
igdStartDateTime :: Lens' InstanceGroupDetail (Maybe POSIX)
igdStartDateTime = lens _igdStartDateTime (\s a -> s { _igdStartDateTime = a })
{-# INLINE igdStartDateTime #-}

-- | The date/time the instance group was available to the cluster.
igdReadyDateTime :: Lens' InstanceGroupDetail (Maybe POSIX)
igdReadyDateTime = lens _igdReadyDateTime (\s a -> s { _igdReadyDateTime = a })
{-# INLINE igdReadyDateTime #-}

-- | The date/time the instance group was terminated.
igdEndDateTime :: Lens' InstanceGroupDetail (Maybe POSIX)
igdEndDateTime = lens _igdEndDateTime (\s a -> s { _igdEndDateTime = a })
{-# INLINE igdEndDateTime #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceGroupDetail' data type to populate a request.
mkInstanceGroupDetail :: MarketType -- ^ 'igdMarket'
                      -> InstanceRoleType -- ^ 'igdInstanceRole'
                      -> Text -- ^ 'igdInstanceType'
                      -> Integer -- ^ 'igdInstanceRequestCount'
                      -> Integer -- ^ 'igdInstanceRunningCount'
                      -> InstanceGroupState -- ^ 'igdState'
                      -> POSIX -- ^ 'igdCreationDateTime'
                      -> InstanceGroupDetail
mkInstanceGroupDetail p1 p2 p3 p4 p5 p6 p7 = InstanceGroupDetail
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
{-# INLINE mkInstanceGroupDetail #-}

instance FromJSON InstanceGroupDetail

instance ToJSON InstanceGroupDetail

-- | Modify an instance group size.
data InstanceGroupModifyConfig = InstanceGroupModifyConfig
    { _igmcInstanceGroupId :: Text
      -- ^ Unique ID of the instance group to expand or shrink.
    , _igmcInstanceCount :: Maybe Integer
      -- ^ Target size for the instance group.
    , _igmcEC2InstanceIdsToTerminate :: [Text]
      -- ^ The EC2 InstanceIds to terminate. For advanced users only. Once
      -- you terminate the instances, the instance group will not return
      -- to its original requested size.
    } deriving (Show, Generic)

-- | Unique ID of the instance group to expand or shrink.
igmcInstanceGroupId :: Lens' InstanceGroupModifyConfig (Text)
igmcInstanceGroupId = lens _igmcInstanceGroupId (\s a -> s { _igmcInstanceGroupId = a })
{-# INLINE igmcInstanceGroupId #-}

-- | Target size for the instance group.
igmcInstanceCount :: Lens' InstanceGroupModifyConfig (Maybe Integer)
igmcInstanceCount = lens _igmcInstanceCount (\s a -> s { _igmcInstanceCount = a })
{-# INLINE igmcInstanceCount #-}

-- | The EC2 InstanceIds to terminate. For advanced users only. Once you
-- terminate the instances, the instance group will not return to its original
-- requested size.
igmcEC2InstanceIdsToTerminate :: Lens' InstanceGroupModifyConfig ([Text])
igmcEC2InstanceIdsToTerminate = lens _igmcEC2InstanceIdsToTerminate (\s a -> s { _igmcEC2InstanceIdsToTerminate = a })
{-# INLINE igmcEC2InstanceIdsToTerminate #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceGroupModifyConfig' data type to populate a request.
mkInstanceGroupModifyConfig :: Text -- ^ 'igmcInstanceGroupId'
                            -> InstanceGroupModifyConfig
mkInstanceGroupModifyConfig p1 = InstanceGroupModifyConfig
    { _igmcInstanceGroupId = p1
    , _igmcInstanceCount = Nothing
    , _igmcEC2InstanceIdsToTerminate = mempty
    }
{-# INLINE mkInstanceGroupModifyConfig #-}

instance ToJSON InstanceGroupModifyConfig

-- | The status change reason details for the instance group.
data InstanceGroupStateChangeReason = InstanceGroupStateChangeReason
    { _igscrCode :: Maybe InstanceGroupStateChangeReasonCode
      -- ^ The programmable code for the state change reason.
    , _igscrMessage :: Maybe Text
      -- ^ The status change reason description.
    } deriving (Show, Generic)

-- | The programmable code for the state change reason.
igscrCode :: Lens' InstanceGroupStateChangeReason (Maybe InstanceGroupStateChangeReasonCode)
igscrCode = lens _igscrCode (\s a -> s { _igscrCode = a })
{-# INLINE igscrCode #-}

-- | The status change reason description.
igscrMessage :: Lens' InstanceGroupStateChangeReason (Maybe Text)
igscrMessage = lens _igscrMessage (\s a -> s { _igscrMessage = a })
{-# INLINE igscrMessage #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceGroupStateChangeReason' data type to populate a request.
mkInstanceGroupStateChangeReason :: InstanceGroupStateChangeReason
mkInstanceGroupStateChangeReason = InstanceGroupStateChangeReason
    { _igscrCode = Nothing
    , _igscrMessage = Nothing
    }
{-# INLINE mkInstanceGroupStateChangeReason #-}

instance FromJSON InstanceGroupStateChangeReason

instance ToJSON InstanceGroupStateChangeReason

-- | The current status of the instance group.
data InstanceGroupStatus = InstanceGroupStatus
    { _iguState :: Maybe InstanceGroupState
      -- ^ The current state of the instance group.
    , _iguStateChangeReason :: Maybe InstanceGroupStateChangeReason
      -- ^ The status change reason details for the instance group.
    , _iguTimeline :: Maybe InstanceGroupTimeline
      -- ^ The timeline of the instance group status over time.
    } deriving (Show, Generic)

-- | The current state of the instance group.
iguState :: Lens' InstanceGroupStatus (Maybe InstanceGroupState)
iguState = lens _iguState (\s a -> s { _iguState = a })
{-# INLINE iguState #-}

-- | The status change reason details for the instance group.
iguStateChangeReason :: Lens' InstanceGroupStatus (Maybe InstanceGroupStateChangeReason)
iguStateChangeReason = lens _iguStateChangeReason (\s a -> s { _iguStateChangeReason = a })
{-# INLINE iguStateChangeReason #-}

-- | The timeline of the instance group status over time.
iguTimeline :: Lens' InstanceGroupStatus (Maybe InstanceGroupTimeline)
iguTimeline = lens _iguTimeline (\s a -> s { _iguTimeline = a })
{-# INLINE iguTimeline #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceGroupStatus' data type to populate a request.
mkInstanceGroupStatus :: InstanceGroupStatus
mkInstanceGroupStatus = InstanceGroupStatus
    { _iguState = Nothing
    , _iguStateChangeReason = Nothing
    , _iguTimeline = Nothing
    }
{-# INLINE mkInstanceGroupStatus #-}

instance FromJSON InstanceGroupStatus

instance ToJSON InstanceGroupStatus

-- | The timeline of the instance group status over time.
data InstanceGroupTimeline = InstanceGroupTimeline
    { _igwCreationDateTime :: Maybe POSIX
      -- ^ The creation date and time of the instance group.
    , _igwReadyDateTime :: Maybe POSIX
      -- ^ The date and time when the instance group became ready to perform
      -- tasks.
    , _igwEndDateTime :: Maybe POSIX
      -- ^ The date and time when the instance group terminated.
    } deriving (Show, Generic)

-- | The creation date and time of the instance group.
igwCreationDateTime :: Lens' InstanceGroupTimeline (Maybe POSIX)
igwCreationDateTime = lens _igwCreationDateTime (\s a -> s { _igwCreationDateTime = a })
{-# INLINE igwCreationDateTime #-}

-- | The date and time when the instance group became ready to perform tasks.
igwReadyDateTime :: Lens' InstanceGroupTimeline (Maybe POSIX)
igwReadyDateTime = lens _igwReadyDateTime (\s a -> s { _igwReadyDateTime = a })
{-# INLINE igwReadyDateTime #-}

-- | The date and time when the instance group terminated.
igwEndDateTime :: Lens' InstanceGroupTimeline (Maybe POSIX)
igwEndDateTime = lens _igwEndDateTime (\s a -> s { _igwEndDateTime = a })
{-# INLINE igwEndDateTime #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceGroupTimeline' data type to populate a request.
mkInstanceGroupTimeline :: InstanceGroupTimeline
mkInstanceGroupTimeline = InstanceGroupTimeline
    { _igwCreationDateTime = Nothing
    , _igwReadyDateTime = Nothing
    , _igwEndDateTime = Nothing
    }
{-# INLINE mkInstanceGroupTimeline #-}

instance FromJSON InstanceGroupTimeline

instance ToJSON InstanceGroupTimeline

-- | The details of the status change reason for the instance.
data InstanceStateChangeReason = InstanceStateChangeReason
    { _iscrCode :: Maybe InstanceStateChangeReasonCode
      -- ^ The programmable code for the state change reason.
    , _iscrMessage :: Maybe Text
      -- ^ The status change reason description.
    } deriving (Show, Generic)

-- | The programmable code for the state change reason.
iscrCode :: Lens' InstanceStateChangeReason (Maybe InstanceStateChangeReasonCode)
iscrCode = lens _iscrCode (\s a -> s { _iscrCode = a })
{-# INLINE iscrCode #-}

-- | The status change reason description.
iscrMessage :: Lens' InstanceStateChangeReason (Maybe Text)
iscrMessage = lens _iscrMessage (\s a -> s { _iscrMessage = a })
{-# INLINE iscrMessage #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceStateChangeReason' data type to populate a request.
mkInstanceStateChangeReason :: InstanceStateChangeReason
mkInstanceStateChangeReason = InstanceStateChangeReason
    { _iscrCode = Nothing
    , _iscrMessage = Nothing
    }
{-# INLINE mkInstanceStateChangeReason #-}

instance FromJSON InstanceStateChangeReason

instance ToJSON InstanceStateChangeReason

-- | The current status of the instance.
data InstanceStatus = InstanceStatus
    { _izState :: Maybe InstanceState
      -- ^ The current state of the instance.
    , _izStateChangeReason :: Maybe InstanceStateChangeReason
      -- ^ The details of the status change reason for the instance.
    , _izTimeline :: Maybe InstanceTimeline
      -- ^ The timeline of the instance status over time.
    } deriving (Show, Generic)

-- | The current state of the instance.
izState :: Lens' InstanceStatus (Maybe InstanceState)
izState = lens _izState (\s a -> s { _izState = a })
{-# INLINE izState #-}

-- | The details of the status change reason for the instance.
izStateChangeReason :: Lens' InstanceStatus (Maybe InstanceStateChangeReason)
izStateChangeReason = lens _izStateChangeReason (\s a -> s { _izStateChangeReason = a })
{-# INLINE izStateChangeReason #-}

-- | The timeline of the instance status over time.
izTimeline :: Lens' InstanceStatus (Maybe InstanceTimeline)
izTimeline = lens _izTimeline (\s a -> s { _izTimeline = a })
{-# INLINE izTimeline #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceStatus' data type to populate a request.
mkInstanceStatus :: InstanceStatus
mkInstanceStatus = InstanceStatus
    { _izState = Nothing
    , _izStateChangeReason = Nothing
    , _izTimeline = Nothing
    }
{-# INLINE mkInstanceStatus #-}

instance FromJSON InstanceStatus

instance ToJSON InstanceStatus

-- | The timeline of the instance status over time.
data InstanceTimeline = InstanceTimeline
    { _iifCreationDateTime :: Maybe POSIX
      -- ^ The creation date and time of the instance.
    , _iifReadyDateTime :: Maybe POSIX
      -- ^ The date and time when the instance was ready to perform tasks.
    , _iifEndDateTime :: Maybe POSIX
      -- ^ The date and time when the instance was terminated.
    } deriving (Show, Generic)

-- | The creation date and time of the instance.
iifCreationDateTime :: Lens' InstanceTimeline (Maybe POSIX)
iifCreationDateTime = lens _iifCreationDateTime (\s a -> s { _iifCreationDateTime = a })
{-# INLINE iifCreationDateTime #-}

-- | The date and time when the instance was ready to perform tasks.
iifReadyDateTime :: Lens' InstanceTimeline (Maybe POSIX)
iifReadyDateTime = lens _iifReadyDateTime (\s a -> s { _iifReadyDateTime = a })
{-# INLINE iifReadyDateTime #-}

-- | The date and time when the instance was terminated.
iifEndDateTime :: Lens' InstanceTimeline (Maybe POSIX)
iifEndDateTime = lens _iifEndDateTime (\s a -> s { _iifEndDateTime = a })
{-# INLINE iifEndDateTime #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceTimeline' data type to populate a request.
mkInstanceTimeline :: InstanceTimeline
mkInstanceTimeline = InstanceTimeline
    { _iifCreationDateTime = Nothing
    , _iifReadyDateTime = Nothing
    , _iifEndDateTime = Nothing
    }
{-# INLINE mkInstanceTimeline #-}

instance FromJSON InstanceTimeline

instance ToJSON InstanceTimeline

-- | A description of a job flow.
data JobFlowDetail = JobFlowDetail
    { _jfdJobFlowId :: Text
      -- ^ The job flow identifier.
    , _jfdName :: Text
      -- ^ The name of the job flow.
    , _jfdLogUri :: Maybe Text
      -- ^ The location in Amazon S3 where log files for the job are stored.
    , _jfdAmiVersion :: Maybe Text
      -- ^ The version of the AMI used to initialize Amazon EC2 instances in
      -- the job flow. For a list of AMI versions currently supported by
      -- Amazon ElasticMapReduce, go to AMI Versions Supported in Elastic
      -- MapReduce in the Amazon Elastic MapReduce Developer's Guide.
    , _jfdExecutionStatusDetail :: JobFlowExecutionStatusDetail
      -- ^ Describes the execution status of the job flow.
    , _jfdInstances :: JobFlowInstancesDetail
      -- ^ Describes the Amazon EC2 instances of the job flow.
    , _jfdSteps :: [StepDetail]
      -- ^ A list of steps run by the job flow.
    , _jfdBootstrapActions :: [BootstrapActionDetail]
      -- ^ A list of the bootstrap actions run by the job flow.
    , _jfdSupportedProducts :: [Text]
      -- ^ A list of strings set by third party software when the job flow
      -- is launched. If you are not using third party software to manage
      -- the job flow this value is empty.
    , _jfdVisibleToAllUsers :: Maybe Bool
      -- ^ Specifies whether the job flow is visible to all IAM users of the
      -- AWS account associated with the job flow. If this value is set to
      -- true, all IAM users of that AWS account can view and (if they
      -- have the proper policy permissions set) manage the job flow. If
      -- it is set to false, only the IAM user that created the job flow
      -- can view and manage it. This value can be changed using the
      -- SetVisibleToAllUsers action.
    , _jfdJobFlowRole :: Maybe Text
      -- ^ The IAM role that was specified when the job flow was launched.
      -- The EC2 instances of the job flow assume this role.
    , _jfdServiceRole :: Maybe Text
      -- ^ The IAM role that will be assumed by the Amazon EMR service to
      -- access AWS resources on your behalf.
    } deriving (Show, Generic)

-- | The job flow identifier.
jfdJobFlowId :: Lens' JobFlowDetail (Text)
jfdJobFlowId = lens _jfdJobFlowId (\s a -> s { _jfdJobFlowId = a })
{-# INLINE jfdJobFlowId #-}

-- | The name of the job flow.
jfdName :: Lens' JobFlowDetail (Text)
jfdName = lens _jfdName (\s a -> s { _jfdName = a })
{-# INLINE jfdName #-}

-- | The location in Amazon S3 where log files for the job are stored.
jfdLogUri :: Lens' JobFlowDetail (Maybe Text)
jfdLogUri = lens _jfdLogUri (\s a -> s { _jfdLogUri = a })
{-# INLINE jfdLogUri #-}

-- | The version of the AMI used to initialize Amazon EC2 instances in the job
-- flow. For a list of AMI versions currently supported by Amazon
-- ElasticMapReduce, go to AMI Versions Supported in Elastic MapReduce in the
-- Amazon Elastic MapReduce Developer's Guide.
jfdAmiVersion :: Lens' JobFlowDetail (Maybe Text)
jfdAmiVersion = lens _jfdAmiVersion (\s a -> s { _jfdAmiVersion = a })
{-# INLINE jfdAmiVersion #-}

-- | Describes the execution status of the job flow.
jfdExecutionStatusDetail :: Lens' JobFlowDetail (JobFlowExecutionStatusDetail)
jfdExecutionStatusDetail = lens _jfdExecutionStatusDetail (\s a -> s { _jfdExecutionStatusDetail = a })
{-# INLINE jfdExecutionStatusDetail #-}

-- | Describes the Amazon EC2 instances of the job flow.
jfdInstances :: Lens' JobFlowDetail (JobFlowInstancesDetail)
jfdInstances = lens _jfdInstances (\s a -> s { _jfdInstances = a })
{-# INLINE jfdInstances #-}

-- | A list of steps run by the job flow.
jfdSteps :: Lens' JobFlowDetail ([StepDetail])
jfdSteps = lens _jfdSteps (\s a -> s { _jfdSteps = a })
{-# INLINE jfdSteps #-}

-- | A list of the bootstrap actions run by the job flow.
jfdBootstrapActions :: Lens' JobFlowDetail ([BootstrapActionDetail])
jfdBootstrapActions = lens _jfdBootstrapActions (\s a -> s { _jfdBootstrapActions = a })
{-# INLINE jfdBootstrapActions #-}

-- | A list of strings set by third party software when the job flow is
-- launched. If you are not using third party software to manage the job flow
-- this value is empty.
jfdSupportedProducts :: Lens' JobFlowDetail ([Text])
jfdSupportedProducts = lens _jfdSupportedProducts (\s a -> s { _jfdSupportedProducts = a })
{-# INLINE jfdSupportedProducts #-}

-- | Specifies whether the job flow is visible to all IAM users of the AWS
-- account associated with the job flow. If this value is set to true, all IAM
-- users of that AWS account can view and (if they have the proper policy
-- permissions set) manage the job flow. If it is set to false, only the IAM
-- user that created the job flow can view and manage it. This value can be
-- changed using the SetVisibleToAllUsers action.
jfdVisibleToAllUsers :: Lens' JobFlowDetail (Maybe Bool)
jfdVisibleToAllUsers = lens _jfdVisibleToAllUsers (\s a -> s { _jfdVisibleToAllUsers = a })
{-# INLINE jfdVisibleToAllUsers #-}

-- | The IAM role that was specified when the job flow was launched. The EC2
-- instances of the job flow assume this role.
jfdJobFlowRole :: Lens' JobFlowDetail (Maybe Text)
jfdJobFlowRole = lens _jfdJobFlowRole (\s a -> s { _jfdJobFlowRole = a })
{-# INLINE jfdJobFlowRole #-}

-- | The IAM role that will be assumed by the Amazon EMR service to access AWS
-- resources on your behalf.
jfdServiceRole :: Lens' JobFlowDetail (Maybe Text)
jfdServiceRole = lens _jfdServiceRole (\s a -> s { _jfdServiceRole = a })
{-# INLINE jfdServiceRole #-}

instance FromJSON JobFlowDetail

-- | Describes the execution status of the job flow.
data JobFlowExecutionStatusDetail = JobFlowExecutionStatusDetail
    { _jfesdState :: JobFlowExecutionState
      -- ^ The state of the job flow.
    , _jfesdCreationDateTime :: POSIX
      -- ^ The creation date and time of the job flow.
    , _jfesdStartDateTime :: Maybe POSIX
      -- ^ The start date and time of the job flow.
    , _jfesdReadyDateTime :: Maybe POSIX
      -- ^ The date and time when the job flow was ready to start running
      -- bootstrap actions.
    , _jfesdEndDateTime :: Maybe POSIX
      -- ^ The completion date and time of the job flow.
    , _jfesdLastStateChangeReason :: Maybe Text
      -- ^ Description of the job flow last changed state.
    } deriving (Show, Generic)

-- | The state of the job flow.
jfesdState :: Lens' JobFlowExecutionStatusDetail (JobFlowExecutionState)
jfesdState = lens _jfesdState (\s a -> s { _jfesdState = a })
{-# INLINE jfesdState #-}

-- | The creation date and time of the job flow.
jfesdCreationDateTime :: Lens' JobFlowExecutionStatusDetail (POSIX)
jfesdCreationDateTime = lens _jfesdCreationDateTime (\s a -> s { _jfesdCreationDateTime = a })
{-# INLINE jfesdCreationDateTime #-}

-- | The start date and time of the job flow.
jfesdStartDateTime :: Lens' JobFlowExecutionStatusDetail (Maybe POSIX)
jfesdStartDateTime = lens _jfesdStartDateTime (\s a -> s { _jfesdStartDateTime = a })
{-# INLINE jfesdStartDateTime #-}

-- | The date and time when the job flow was ready to start running bootstrap
-- actions.
jfesdReadyDateTime :: Lens' JobFlowExecutionStatusDetail (Maybe POSIX)
jfesdReadyDateTime = lens _jfesdReadyDateTime (\s a -> s { _jfesdReadyDateTime = a })
{-# INLINE jfesdReadyDateTime #-}

-- | The completion date and time of the job flow.
jfesdEndDateTime :: Lens' JobFlowExecutionStatusDetail (Maybe POSIX)
jfesdEndDateTime = lens _jfesdEndDateTime (\s a -> s { _jfesdEndDateTime = a })
{-# INLINE jfesdEndDateTime #-}

-- | Description of the job flow last changed state.
jfesdLastStateChangeReason :: Lens' JobFlowExecutionStatusDetail (Maybe Text)
jfesdLastStateChangeReason = lens _jfesdLastStateChangeReason (\s a -> s { _jfesdLastStateChangeReason = a })
{-# INLINE jfesdLastStateChangeReason #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'JobFlowExecutionStatusDetail' data type to populate a request.
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
{-# INLINE mkJobFlowExecutionStatusDetail #-}

instance FromJSON JobFlowExecutionStatusDetail

instance ToJSON JobFlowExecutionStatusDetail

-- | A specification of the number and type of Amazon EC2 instances on which to
-- run the job flow.
data JobFlowInstancesConfig = JobFlowInstancesConfig
    { _jficMasterInstanceType :: Maybe Text
      -- ^ The EC2 instance type of the master node.
    , _jficSlaveInstanceType :: Maybe Text
      -- ^ The EC2 instance type of the slave nodes.
    , _jficInstanceCount :: Maybe Integer
      -- ^ The number of Amazon EC2 instances used to execute the job flow.
    , _jficInstanceGroups :: [InstanceGroupConfig]
      -- ^ Configuration for the job flow's instance groups.
    , _jficEc2KeyName :: Maybe Text
      -- ^ The name of the Amazon EC2 key pair that can be used to ssh to
      -- the master node as the user called "hadoop.".
    , _jficPlacement :: Maybe PlacementType
      -- ^ The Availability Zone the job flow will run in.
    , _jficKeepJobFlowAliveWhenNoSteps :: Maybe Bool
      -- ^ Specifies whether the job flow should terminate after completing
      -- all steps.
    , _jficTerminationProtected :: Maybe Bool
      -- ^ Specifies whether to lock the job flow to prevent the Amazon EC2
      -- instances from being terminated by API call, user intervention,
      -- or in the event of a job flow error.
    , _jficHadoopVersion :: Maybe Text
      -- ^ The Hadoop version for the job flow. Valid inputs are "0.18",
      -- "0.20", or "0.20.205". If you do not set this value, the default
      -- of 0.18 is used, unless the AmiVersion parameter is set in the
      -- RunJobFlow call, in which case the default version of Hadoop for
      -- that AMI version is used.
    , _jficEc2SubnetId :: Maybe Text
      -- ^ To launch the job flow in Amazon Virtual Private Cloud (Amazon
      -- VPC), set this parameter to the identifier of the Amazon VPC
      -- subnet where you want the job flow to launch. If you do not
      -- specify this value, the job flow is launched in the normal Amazon
      -- Web Services cloud, outside of an Amazon VPC. Amazon VPC
      -- currently does not support cluster compute quadruple extra large
      -- (cc1.4xlarge) instances. Thus you cannot specify the cc1.4xlarge
      -- instance type for nodes of a job flow launched in a Amazon VPC.
    } deriving (Show, Generic)

-- | The EC2 instance type of the master node.
jficMasterInstanceType :: Lens' JobFlowInstancesConfig (Maybe Text)
jficMasterInstanceType = lens _jficMasterInstanceType (\s a -> s { _jficMasterInstanceType = a })
{-# INLINE jficMasterInstanceType #-}

-- | The EC2 instance type of the slave nodes.
jficSlaveInstanceType :: Lens' JobFlowInstancesConfig (Maybe Text)
jficSlaveInstanceType = lens _jficSlaveInstanceType (\s a -> s { _jficSlaveInstanceType = a })
{-# INLINE jficSlaveInstanceType #-}

-- | The number of Amazon EC2 instances used to execute the job flow.
jficInstanceCount :: Lens' JobFlowInstancesConfig (Maybe Integer)
jficInstanceCount = lens _jficInstanceCount (\s a -> s { _jficInstanceCount = a })
{-# INLINE jficInstanceCount #-}

-- | Configuration for the job flow's instance groups.
jficInstanceGroups :: Lens' JobFlowInstancesConfig ([InstanceGroupConfig])
jficInstanceGroups = lens _jficInstanceGroups (\s a -> s { _jficInstanceGroups = a })
{-# INLINE jficInstanceGroups #-}

-- | The name of the Amazon EC2 key pair that can be used to ssh to the master
-- node as the user called "hadoop.".
jficEc2KeyName :: Lens' JobFlowInstancesConfig (Maybe Text)
jficEc2KeyName = lens _jficEc2KeyName (\s a -> s { _jficEc2KeyName = a })
{-# INLINE jficEc2KeyName #-}

-- | The Availability Zone the job flow will run in.
jficPlacement :: Lens' JobFlowInstancesConfig (Maybe PlacementType)
jficPlacement = lens _jficPlacement (\s a -> s { _jficPlacement = a })
{-# INLINE jficPlacement #-}

-- | Specifies whether the job flow should terminate after completing all steps.
jficKeepJobFlowAliveWhenNoSteps :: Lens' JobFlowInstancesConfig (Maybe Bool)
jficKeepJobFlowAliveWhenNoSteps = lens _jficKeepJobFlowAliveWhenNoSteps (\s a -> s { _jficKeepJobFlowAliveWhenNoSteps = a })
{-# INLINE jficKeepJobFlowAliveWhenNoSteps #-}

-- | Specifies whether to lock the job flow to prevent the Amazon EC2 instances
-- from being terminated by API call, user intervention, or in the event of a
-- job flow error.
jficTerminationProtected :: Lens' JobFlowInstancesConfig (Maybe Bool)
jficTerminationProtected = lens _jficTerminationProtected (\s a -> s { _jficTerminationProtected = a })
{-# INLINE jficTerminationProtected #-}

-- | The Hadoop version for the job flow. Valid inputs are "0.18", "0.20", or
-- "0.20.205". If you do not set this value, the default of 0.18 is used,
-- unless the AmiVersion parameter is set in the RunJobFlow call, in which
-- case the default version of Hadoop for that AMI version is used.
jficHadoopVersion :: Lens' JobFlowInstancesConfig (Maybe Text)
jficHadoopVersion = lens _jficHadoopVersion (\s a -> s { _jficHadoopVersion = a })
{-# INLINE jficHadoopVersion #-}

-- | To launch the job flow in Amazon Virtual Private Cloud (Amazon VPC), set
-- this parameter to the identifier of the Amazon VPC subnet where you want
-- the job flow to launch. If you do not specify this value, the job flow is
-- launched in the normal Amazon Web Services cloud, outside of an Amazon VPC.
-- Amazon VPC currently does not support cluster compute quadruple extra large
-- (cc1.4xlarge) instances. Thus you cannot specify the cc1.4xlarge instance
-- type for nodes of a job flow launched in a Amazon VPC.
jficEc2SubnetId :: Lens' JobFlowInstancesConfig (Maybe Text)
jficEc2SubnetId = lens _jficEc2SubnetId (\s a -> s { _jficEc2SubnetId = a })
{-# INLINE jficEc2SubnetId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'JobFlowInstancesConfig' data type to populate a request.
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
{-# INLINE mkJobFlowInstancesConfig #-}

instance ToJSON JobFlowInstancesConfig

-- | Describes the Amazon EC2 instances of the job flow.
data JobFlowInstancesDetail = JobFlowInstancesDetail
    { _jfidMasterInstanceType :: Text
      -- ^ The Amazon EC2 master node instance type.
    , _jfidMasterPublicDnsName :: Maybe Text
      -- ^ The DNS name of the master node.
    , _jfidMasterInstanceId :: Maybe Text
      -- ^ The Amazon EC2 instance identifier of the master node.
    , _jfidSlaveInstanceType :: Text
      -- ^ The Amazon EC2 slave node instance type.
    , _jfidInstanceCount :: Integer
      -- ^ The number of Amazon EC2 instances in the cluster. If the value
      -- is 1, the same instance serves as both the master and slave node.
      -- If the value is greater than 1, one instance is the master node
      -- and all others are slave nodes.
    , _jfidInstanceGroups :: [InstanceGroupDetail]
      -- ^ Details about the job flow's instance groups.
    , _jfidNormalizedInstanceHours :: Maybe Integer
      -- ^ An approximation of the cost of the job flow, represented in
      -- m1.small/hours. This value is incremented once for every hour an
      -- m1.small runs. Larger instances are weighted more, so an Amazon
      -- EC2 instance that is roughly four times more expensive would
      -- result in the normalized instance hours being incremented by
      -- four. This result is only an approximation and does not reflect
      -- the actual billing rate.
    , _jfidEc2KeyName :: Maybe Text
      -- ^ The name of an Amazon EC2 key pair that can be used to ssh to the
      -- master node of job flow.
    , _jfidEc2SubnetId :: Maybe Text
      -- ^ For job flows launched within Amazon Virtual Private Cloud, this
      -- value specifies the identifier of the subnet where the job flow
      -- was launched.
    , _jfidPlacement :: Maybe PlacementType
      -- ^ The Amazon EC2 Availability Zone for the job flow.
    , _jfidKeepJobFlowAliveWhenNoSteps :: Maybe Bool
      -- ^ Specifies whether the job flow should terminate after completing
      -- all steps.
    , _jfidTerminationProtected :: Maybe Bool
      -- ^ Specifies whether the Amazon EC2 instances in the cluster are
      -- protected from termination by API calls, user intervention, or in
      -- the event of a job flow error.
    , _jfidHadoopVersion :: Maybe Text
      -- ^ The Hadoop version for the job flow.
    } deriving (Show, Generic)

-- | The Amazon EC2 master node instance type.
jfidMasterInstanceType :: Lens' JobFlowInstancesDetail (Text)
jfidMasterInstanceType = lens _jfidMasterInstanceType (\s a -> s { _jfidMasterInstanceType = a })
{-# INLINE jfidMasterInstanceType #-}

-- | The DNS name of the master node.
jfidMasterPublicDnsName :: Lens' JobFlowInstancesDetail (Maybe Text)
jfidMasterPublicDnsName = lens _jfidMasterPublicDnsName (\s a -> s { _jfidMasterPublicDnsName = a })
{-# INLINE jfidMasterPublicDnsName #-}

-- | The Amazon EC2 instance identifier of the master node.
jfidMasterInstanceId :: Lens' JobFlowInstancesDetail (Maybe Text)
jfidMasterInstanceId = lens _jfidMasterInstanceId (\s a -> s { _jfidMasterInstanceId = a })
{-# INLINE jfidMasterInstanceId #-}

-- | The Amazon EC2 slave node instance type.
jfidSlaveInstanceType :: Lens' JobFlowInstancesDetail (Text)
jfidSlaveInstanceType = lens _jfidSlaveInstanceType (\s a -> s { _jfidSlaveInstanceType = a })
{-# INLINE jfidSlaveInstanceType #-}

-- | The number of Amazon EC2 instances in the cluster. If the value is 1, the
-- same instance serves as both the master and slave node. If the value is
-- greater than 1, one instance is the master node and all others are slave
-- nodes.
jfidInstanceCount :: Lens' JobFlowInstancesDetail (Integer)
jfidInstanceCount = lens _jfidInstanceCount (\s a -> s { _jfidInstanceCount = a })
{-# INLINE jfidInstanceCount #-}

-- | Details about the job flow's instance groups.
jfidInstanceGroups :: Lens' JobFlowInstancesDetail ([InstanceGroupDetail])
jfidInstanceGroups = lens _jfidInstanceGroups (\s a -> s { _jfidInstanceGroups = a })
{-# INLINE jfidInstanceGroups #-}

-- | An approximation of the cost of the job flow, represented in
-- m1.small/hours. This value is incremented once for every hour an m1.small
-- runs. Larger instances are weighted more, so an Amazon EC2 instance that is
-- roughly four times more expensive would result in the normalized instance
-- hours being incremented by four. This result is only an approximation and
-- does not reflect the actual billing rate.
jfidNormalizedInstanceHours :: Lens' JobFlowInstancesDetail (Maybe Integer)
jfidNormalizedInstanceHours = lens _jfidNormalizedInstanceHours (\s a -> s { _jfidNormalizedInstanceHours = a })
{-# INLINE jfidNormalizedInstanceHours #-}

-- | The name of an Amazon EC2 key pair that can be used to ssh to the master
-- node of job flow.
jfidEc2KeyName :: Lens' JobFlowInstancesDetail (Maybe Text)
jfidEc2KeyName = lens _jfidEc2KeyName (\s a -> s { _jfidEc2KeyName = a })
{-# INLINE jfidEc2KeyName #-}

-- | For job flows launched within Amazon Virtual Private Cloud, this value
-- specifies the identifier of the subnet where the job flow was launched.
jfidEc2SubnetId :: Lens' JobFlowInstancesDetail (Maybe Text)
jfidEc2SubnetId = lens _jfidEc2SubnetId (\s a -> s { _jfidEc2SubnetId = a })
{-# INLINE jfidEc2SubnetId #-}

-- | The Amazon EC2 Availability Zone for the job flow.
jfidPlacement :: Lens' JobFlowInstancesDetail (Maybe PlacementType)
jfidPlacement = lens _jfidPlacement (\s a -> s { _jfidPlacement = a })
{-# INLINE jfidPlacement #-}

-- | Specifies whether the job flow should terminate after completing all steps.
jfidKeepJobFlowAliveWhenNoSteps :: Lens' JobFlowInstancesDetail (Maybe Bool)
jfidKeepJobFlowAliveWhenNoSteps = lens _jfidKeepJobFlowAliveWhenNoSteps (\s a -> s { _jfidKeepJobFlowAliveWhenNoSteps = a })
{-# INLINE jfidKeepJobFlowAliveWhenNoSteps #-}

-- | Specifies whether the Amazon EC2 instances in the cluster are protected
-- from termination by API calls, user intervention, or in the event of a job
-- flow error.
jfidTerminationProtected :: Lens' JobFlowInstancesDetail (Maybe Bool)
jfidTerminationProtected = lens _jfidTerminationProtected (\s a -> s { _jfidTerminationProtected = a })
{-# INLINE jfidTerminationProtected #-}

-- | The Hadoop version for the job flow.
jfidHadoopVersion :: Lens' JobFlowInstancesDetail (Maybe Text)
jfidHadoopVersion = lens _jfidHadoopVersion (\s a -> s { _jfidHadoopVersion = a })
{-# INLINE jfidHadoopVersion #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'JobFlowInstancesDetail' data type to populate a request.
mkJobFlowInstancesDetail :: Text -- ^ 'jfidMasterInstanceType'
                         -> Text -- ^ 'jfidSlaveInstanceType'
                         -> Integer -- ^ 'jfidInstanceCount'
                         -> JobFlowInstancesDetail
mkJobFlowInstancesDetail p1 p2 p3 = JobFlowInstancesDetail
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
{-# INLINE mkJobFlowInstancesDetail #-}

instance FromJSON JobFlowInstancesDetail

instance ToJSON JobFlowInstancesDetail

-- | A key value pair.
data KeyValue = KeyValue
    { _kvKey :: Maybe Text
      -- ^ The unique identifier of a key value pair.
    , _kvValue :: Maybe Text
      -- ^ The value part of the identified key.
    } deriving (Show, Generic)

-- | The unique identifier of a key value pair.
kvKey :: Lens' KeyValue (Maybe Text)
kvKey = lens _kvKey (\s a -> s { _kvKey = a })
{-# INLINE kvKey #-}

-- | The value part of the identified key.
kvValue :: Lens' KeyValue (Maybe Text)
kvValue = lens _kvValue (\s a -> s { _kvValue = a })
{-# INLINE kvValue #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'KeyValue' data type to populate a request.
mkKeyValue :: KeyValue
mkKeyValue = KeyValue
    { _kvKey = Nothing
    , _kvValue = Nothing
    }
{-# INLINE mkKeyValue #-}

instance FromJSON KeyValue

instance ToJSON KeyValue

-- | The script run by the bootstrap action.
data ScriptBootstrapActionConfig = ScriptBootstrapActionConfig
    { _sbacPath :: Text
      -- ^ Location of the script to run during a bootstrap action. Can be
      -- either a location in Amazon S3 or on a local file system.
    , _sbacArgs :: [Text]
      -- ^ A list of command line arguments to pass to the bootstrap action
      -- script.
    } deriving (Show, Generic)

-- | Location of the script to run during a bootstrap action. Can be either a
-- location in Amazon S3 or on a local file system.
sbacPath :: Lens' ScriptBootstrapActionConfig (Text)
sbacPath = lens _sbacPath (\s a -> s { _sbacPath = a })
{-# INLINE sbacPath #-}

-- | A list of command line arguments to pass to the bootstrap action script.
sbacArgs :: Lens' ScriptBootstrapActionConfig ([Text])
sbacArgs = lens _sbacArgs (\s a -> s { _sbacArgs = a })
{-# INLINE sbacArgs #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ScriptBootstrapActionConfig' data type to populate a request.
mkScriptBootstrapActionConfig :: Text -- ^ 'sbacPath'
                              -> ScriptBootstrapActionConfig
mkScriptBootstrapActionConfig p1 = ScriptBootstrapActionConfig
    { _sbacPath = p1
    , _sbacArgs = mempty
    }
{-# INLINE mkScriptBootstrapActionConfig #-}

instance FromJSON ScriptBootstrapActionConfig

instance ToJSON ScriptBootstrapActionConfig

-- | The step details for the requested step identifier.
data Step = Step
    { _svId :: Maybe Text
      -- ^ The identifier of the cluster step.
    , _svName :: Maybe Text
      -- ^ The name of the cluster step.
    , _svConfig :: Maybe HadoopStepConfig
      -- ^ The Hadoop job configuration of the cluster step.
    , _svActionOnFailure :: Maybe ActionOnFailure
      -- ^ This specifies what action to take when the cluster step fails.
      -- Possible values are TERMINATE_CLUSTER, CANCEL_AND_WAIT, and
      -- CONTINUE.
    , _svStatus :: Maybe StepStatus
      -- ^ The current execution status details of the cluster step.
    } deriving (Show, Generic)

-- | The identifier of the cluster step.
svId :: Lens' Step (Maybe Text)
svId = lens _svId (\s a -> s { _svId = a })
{-# INLINE svId #-}

-- | The name of the cluster step.
svName :: Lens' Step (Maybe Text)
svName = lens _svName (\s a -> s { _svName = a })
{-# INLINE svName #-}

-- | The Hadoop job configuration of the cluster step.
svConfig :: Lens' Step (Maybe HadoopStepConfig)
svConfig = lens _svConfig (\s a -> s { _svConfig = a })
{-# INLINE svConfig #-}

-- | This specifies what action to take when the cluster step fails. Possible
-- values are TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE.
svActionOnFailure :: Lens' Step (Maybe ActionOnFailure)
svActionOnFailure = lens _svActionOnFailure (\s a -> s { _svActionOnFailure = a })
{-# INLINE svActionOnFailure #-}

-- | The current execution status details of the cluster step.
svStatus :: Lens' Step (Maybe StepStatus)
svStatus = lens _svStatus (\s a -> s { _svStatus = a })
{-# INLINE svStatus #-}

instance FromJSON Step

-- | Specification of a job flow step.
data StepConfig = StepConfig
    { _scName :: Text
      -- ^ The name of the job flow step.
    , _scActionOnFailure :: Maybe ActionOnFailure
      -- ^ The action to take if the job flow step fails.
    , _scHadoopJarStep :: HadoopJarStepConfig
      -- ^ The JAR file used for the job flow step.
    } deriving (Show, Generic)

-- | The name of the job flow step.
scName :: Lens' StepConfig (Text)
scName = lens _scName (\s a -> s { _scName = a })
{-# INLINE scName #-}

-- | The action to take if the job flow step fails.
scActionOnFailure :: Lens' StepConfig (Maybe ActionOnFailure)
scActionOnFailure = lens _scActionOnFailure (\s a -> s { _scActionOnFailure = a })
{-# INLINE scActionOnFailure #-}

-- | The JAR file used for the job flow step.
scHadoopJarStep :: Lens' StepConfig (HadoopJarStepConfig)
scHadoopJarStep = lens _scHadoopJarStep (\s a -> s { _scHadoopJarStep = a })
{-# INLINE scHadoopJarStep #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'StepConfig' data type to populate a request.
mkStepConfig :: Text -- ^ 'scName'
             -> HadoopJarStepConfig -- ^ 'scHadoopJarStep'
             -> StepConfig
mkStepConfig p1 p2 = StepConfig
    { _scName = p1
    , _scActionOnFailure = Nothing
    , _scHadoopJarStep = p3
    }
{-# INLINE mkStepConfig #-}

instance FromJSON StepConfig

instance ToJSON StepConfig

-- | Combines the execution state and configuration of a step.
data StepDetail = StepDetail
    { _sdStepConfig :: StepConfig
      -- ^ The step configuration.
    , _sdExecutionStatusDetail :: StepExecutionStatusDetail
      -- ^ The description of the step status.
    } deriving (Show, Generic)

-- | The step configuration.
sdStepConfig :: Lens' StepDetail (StepConfig)
sdStepConfig = lens _sdStepConfig (\s a -> s { _sdStepConfig = a })
{-# INLINE sdStepConfig #-}

-- | The description of the step status.
sdExecutionStatusDetail :: Lens' StepDetail (StepExecutionStatusDetail)
sdExecutionStatusDetail = lens _sdExecutionStatusDetail (\s a -> s { _sdExecutionStatusDetail = a })
{-# INLINE sdExecutionStatusDetail #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'StepDetail' data type to populate a request.
mkStepDetail :: StepConfig -- ^ 'sdStepConfig'
             -> StepExecutionStatusDetail -- ^ 'sdExecutionStatusDetail'
             -> StepDetail
mkStepDetail p1 p2 = StepDetail
    { _sdStepConfig = p1
    , _sdExecutionStatusDetail = p2
    }
{-# INLINE mkStepDetail #-}

instance FromJSON StepDetail

instance ToJSON StepDetail

-- | The description of the step status.
data StepExecutionStatusDetail = StepExecutionStatusDetail
    { _sesdState :: StepExecutionState
      -- ^ The state of the job flow step.
    , _sesdCreationDateTime :: POSIX
      -- ^ The creation date and time of the step.
    , _sesdStartDateTime :: Maybe POSIX
      -- ^ The start date and time of the step.
    , _sesdEndDateTime :: Maybe POSIX
      -- ^ The completion date and time of the step.
    , _sesdLastStateChangeReason :: Maybe Text
      -- ^ A description of the step's current state.
    } deriving (Show, Generic)

-- | The state of the job flow step.
sesdState :: Lens' StepExecutionStatusDetail (StepExecutionState)
sesdState = lens _sesdState (\s a -> s { _sesdState = a })
{-# INLINE sesdState #-}

-- | The creation date and time of the step.
sesdCreationDateTime :: Lens' StepExecutionStatusDetail (POSIX)
sesdCreationDateTime = lens _sesdCreationDateTime (\s a -> s { _sesdCreationDateTime = a })
{-# INLINE sesdCreationDateTime #-}

-- | The start date and time of the step.
sesdStartDateTime :: Lens' StepExecutionStatusDetail (Maybe POSIX)
sesdStartDateTime = lens _sesdStartDateTime (\s a -> s { _sesdStartDateTime = a })
{-# INLINE sesdStartDateTime #-}

-- | The completion date and time of the step.
sesdEndDateTime :: Lens' StepExecutionStatusDetail (Maybe POSIX)
sesdEndDateTime = lens _sesdEndDateTime (\s a -> s { _sesdEndDateTime = a })
{-# INLINE sesdEndDateTime #-}

-- | A description of the step's current state.
sesdLastStateChangeReason :: Lens' StepExecutionStatusDetail (Maybe Text)
sesdLastStateChangeReason = lens _sesdLastStateChangeReason (\s a -> s { _sesdLastStateChangeReason = a })
{-# INLINE sesdLastStateChangeReason #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'StepExecutionStatusDetail' data type to populate a request.
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
{-# INLINE mkStepExecutionStatusDetail #-}

instance FromJSON StepExecutionStatusDetail

instance ToJSON StepExecutionStatusDetail

-- | The reason for the step execution status change.
data StepStateChangeReason = StepStateChangeReason
    { _sscrCode :: Maybe StepStateChangeReasonCode
      -- ^ The programmable code for the state change reason.
    , _sscrMessage :: Maybe Text
      -- ^ The descriptive message for the state change reason.
    } deriving (Show, Generic)

-- | The programmable code for the state change reason.
sscrCode :: Lens' StepStateChangeReason (Maybe StepStateChangeReasonCode)
sscrCode = lens _sscrCode (\s a -> s { _sscrCode = a })
{-# INLINE sscrCode #-}

-- | The descriptive message for the state change reason.
sscrMessage :: Lens' StepStateChangeReason (Maybe Text)
sscrMessage = lens _sscrMessage (\s a -> s { _sscrMessage = a })
{-# INLINE sscrMessage #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'StepStateChangeReason' data type to populate a request.
mkStepStateChangeReason :: StepStateChangeReason
mkStepStateChangeReason = StepStateChangeReason
    { _sscrCode = Nothing
    , _sscrMessage = Nothing
    }
{-# INLINE mkStepStateChangeReason #-}

instance FromJSON StepStateChangeReason

instance ToJSON StepStateChangeReason

-- | The current execution status details of the cluster step.
data StepStatus = StepStatus
    { _sssState :: Maybe StepState
      -- ^ The execution state of the cluster step.
    , _sssStateChangeReason :: Maybe StepStateChangeReason
      -- ^ The reason for the step execution status change.
    , _sssTimeline :: Maybe StepTimeline
      -- ^ The timeline of the cluster step status over time.
    } deriving (Show, Generic)

-- | The execution state of the cluster step.
sssState :: Lens' StepStatus (Maybe StepState)
sssState = lens _sssState (\s a -> s { _sssState = a })
{-# INLINE sssState #-}

-- | The reason for the step execution status change.
sssStateChangeReason :: Lens' StepStatus (Maybe StepStateChangeReason)
sssStateChangeReason = lens _sssStateChangeReason (\s a -> s { _sssStateChangeReason = a })
{-# INLINE sssStateChangeReason #-}

-- | The timeline of the cluster step status over time.
sssTimeline :: Lens' StepStatus (Maybe StepTimeline)
sssTimeline = lens _sssTimeline (\s a -> s { _sssTimeline = a })
{-# INLINE sssTimeline #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'StepStatus' data type to populate a request.
mkStepStatus :: StepStatus
mkStepStatus = StepStatus
    { _sssState = Nothing
    , _sssStateChangeReason = Nothing
    , _sssTimeline = Nothing
    }
{-# INLINE mkStepStatus #-}

instance FromJSON StepStatus

instance ToJSON StepStatus

-- | The summary of the cluster step.
data StepSummary = StepSummary
    { _sssyId :: Maybe Text
      -- ^ The identifier of the cluster step.
    , _sssyName :: Maybe Text
      -- ^ The name of the cluster step.
    , _sssyStatus :: Maybe StepStatus
      -- ^ The current execution status details of the cluster step.
    } deriving (Show, Generic)

-- | The identifier of the cluster step.
sssyId :: Lens' StepSummary (Maybe Text)
sssyId = lens _sssyId (\s a -> s { _sssyId = a })
{-# INLINE sssyId #-}

-- | The name of the cluster step.
sssyName :: Lens' StepSummary (Maybe Text)
sssyName = lens _sssyName (\s a -> s { _sssyName = a })
{-# INLINE sssyName #-}

-- | The current execution status details of the cluster step.
sssyStatus :: Lens' StepSummary (Maybe StepStatus)
sssyStatus = lens _sssyStatus (\s a -> s { _sssyStatus = a })
{-# INLINE sssyStatus #-}

instance FromJSON StepSummary

-- | The timeline of the cluster step status over time.
data StepTimeline = StepTimeline
    { _ssfCreationDateTime :: Maybe POSIX
      -- ^ The date and time when the cluster step was created.
    , _ssfStartDateTime :: Maybe POSIX
      -- ^ The date and time when the cluster step execution started.
    , _ssfEndDateTime :: Maybe POSIX
      -- ^ The date and time when the cluster step execution completed or
      -- failed.
    } deriving (Show, Generic)

-- | The date and time when the cluster step was created.
ssfCreationDateTime :: Lens' StepTimeline (Maybe POSIX)
ssfCreationDateTime = lens _ssfCreationDateTime (\s a -> s { _ssfCreationDateTime = a })
{-# INLINE ssfCreationDateTime #-}

-- | The date and time when the cluster step execution started.
ssfStartDateTime :: Lens' StepTimeline (Maybe POSIX)
ssfStartDateTime = lens _ssfStartDateTime (\s a -> s { _ssfStartDateTime = a })
{-# INLINE ssfStartDateTime #-}

-- | The date and time when the cluster step execution completed or failed.
ssfEndDateTime :: Lens' StepTimeline (Maybe POSIX)
ssfEndDateTime = lens _ssfEndDateTime (\s a -> s { _ssfEndDateTime = a })
{-# INLINE ssfEndDateTime #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'StepTimeline' data type to populate a request.
mkStepTimeline :: StepTimeline
mkStepTimeline = StepTimeline
    { _ssfCreationDateTime = Nothing
    , _ssfStartDateTime = Nothing
    , _ssfEndDateTime = Nothing
    }
{-# INLINE mkStepTimeline #-}

instance FromJSON StepTimeline

instance ToJSON StepTimeline

-- | The list of supported product configurations which allow user-supplied
-- arguments. EMR accepts these arguments and forwards them to the
-- corresponding installation script as bootstrap action arguments.
data SupportedProductConfig = SupportedProductConfig
    { _spcName :: Maybe Text
      -- ^ The name of the product configuration.
    , _spcArgs :: [Text]
      -- ^ The list of user-supplied arguments.
    } deriving (Show, Generic)

-- | The name of the product configuration.
spcName :: Lens' SupportedProductConfig (Maybe Text)
spcName = lens _spcName (\s a -> s { _spcName = a })
{-# INLINE spcName #-}

-- | The list of user-supplied arguments.
spcArgs :: Lens' SupportedProductConfig ([Text])
spcArgs = lens _spcArgs (\s a -> s { _spcArgs = a })
{-# INLINE spcArgs #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'SupportedProductConfig' data type to populate a request.
mkSupportedProductConfig :: SupportedProductConfig
mkSupportedProductConfig = SupportedProductConfig
    { _spcName = Nothing
    , _spcArgs = mempty
    }
{-# INLINE mkSupportedProductConfig #-}

instance ToJSON SupportedProductConfig

-- | A key/value pair containing user-defined metadata that you can associate
-- with an Amazon EMR resource. Tags make it easier to associate clusters in
-- various ways, such as grouping clu\ sters to track your Amazon EMR resource
-- allocation costs. For more information, see Tagging Amazon EMR Resources.
data Tag = Tag
    { _tKey :: Maybe Text
      -- ^ A user-defined key, which is the minimum required information for
      -- a valid tag. For more information, see Tagging Amazon EMR
      -- Resources.
    , _tValue :: Maybe Text
      -- ^ A user-defined value, which is optional in a tag. For more
      -- information, see Tagging Amazon EMR Resources.
    } deriving (Show, Generic)

-- | A user-defined key, which is the minimum required information for a valid
-- tag. For more information, see Tagging Amazon EMR Resources.
tKey :: Lens' Tag (Maybe Text)
tKey = lens _tKey (\s a -> s { _tKey = a })
{-# INLINE tKey #-}

-- | A user-defined value, which is optional in a tag. For more information, see
-- Tagging Amazon EMR Resources.
tValue :: Lens' Tag (Maybe Text)
tValue = lens _tValue (\s a -> s { _tValue = a })
{-# INLINE tValue #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Tag' data type to populate a request.
mkTag :: Tag
mkTag = Tag
    { _tKey = Nothing
    , _tValue = Nothing
    }
{-# INLINE mkTag #-}

instance FromJSON Tag

instance ToJSON Tag

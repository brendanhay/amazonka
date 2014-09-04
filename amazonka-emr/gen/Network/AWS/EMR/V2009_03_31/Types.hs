{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
    , BootstrapActionDetail (..)
    , badBootstrapActionConfig

    -- * PlacementType
    , PlacementType (..)
    , ptAvailabilityZone

    -- * Application
    , Application (..)
    , aName
    , aVersion
    , aArgs
    , aAdditionalInfo

    -- * BootstrapActionConfig
    , BootstrapActionConfig (..)
    , bacName
    , bacScriptBootstrapAction

    -- * Cluster
    , Cluster (..)
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
    , ClusterStateChangeReason (..)
    , cscrCode
    , cscrMessage

    -- * ClusterStatus
    , ClusterStatus (..)
    , csState
    , csStateChangeReason
    , csTimeline

    -- * ClusterSummary
    , ClusterSummary (..)
    , cwId
    , cwName
    , cwStatus

    -- * ClusterTimeline
    , ClusterTimeline (..)
    , cuCreationDateTime
    , cuReadyDateTime
    , cuEndDateTime

    -- * Command
    , Command (..)
    , cdName
    , cdScriptPath
    , cdArgs

    -- * Ec2InstanceAttributes
    , Ec2InstanceAttributes (..)
    , eiaEc2KeyName
    , eiaEc2SubnetId
    , eiaEc2AvailabilityZone
    , eiaIamInstanceProfile

    -- * HadoopJarStepConfig
    , HadoopJarStepConfig (..)
    , hjscProperties
    , hjscJar
    , hjscMainClass
    , hjscArgs

    -- * HadoopStepConfig
    , HadoopStepConfig (..)
    , hscJar
    , hscProperties
    , hscMainClass
    , hscArgs

    -- * Instance
    , Instance (..)
    , ieId
    , ieEc2InstanceId
    , iePublicDnsName
    , iePublicIpAddress
    , iePrivateDnsName
    , iePrivateIpAddress
    , ieStatus

    -- * InstanceGroup
    , InstanceGroup (..)
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
    , InstanceGroupConfig (..)
    , igcName
    , igcMarket
    , igcInstanceRole
    , igcBidPrice
    , igcInstanceType
    , igcInstanceCount

    -- * InstanceGroupDetail
    , InstanceGroupDetail (..)
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
    , InstanceGroupModifyConfig (..)
    , igmcInstanceGroupId
    , igmcInstanceCount
    , igmcEC2InstanceIdsToTerminate

    -- * InstanceGroupStateChangeReason
    , InstanceGroupStateChangeReason (..)
    , igscrCode
    , igscrMessage

    -- * InstanceGroupStatus
    , InstanceGroupStatus (..)
    , iguState
    , iguStateChangeReason
    , iguTimeline

    -- * InstanceGroupTimeline
    , InstanceGroupTimeline (..)
    , igwCreationDateTime
    , igwReadyDateTime
    , igwEndDateTime

    -- * InstanceStateChangeReason
    , InstanceStateChangeReason (..)
    , iscrCode
    , iscrMessage

    -- * InstanceStatus
    , InstanceStatus (..)
    , izState
    , izStateChangeReason
    , izTimeline

    -- * InstanceTimeline
    , InstanceTimeline (..)
    , iifCreationDateTime
    , iifReadyDateTime
    , iifEndDateTime

    -- * JobFlowDetail
    , JobFlowDetail (..)
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
    , JobFlowExecutionStatusDetail (..)
    , jfesdState
    , jfesdCreationDateTime
    , jfesdStartDateTime
    , jfesdReadyDateTime
    , jfesdEndDateTime
    , jfesdLastStateChangeReason

    -- * JobFlowInstancesConfig
    , JobFlowInstancesConfig (..)
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
    , JobFlowInstancesDetail (..)
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
    , KeyValue (..)
    , kvKey
    , kvValue

    -- * ScriptBootstrapActionConfig
    , ScriptBootstrapActionConfig (..)
    , sbacPath
    , sbacArgs

    -- * Step
    , Step (..)
    , svId
    , svName
    , svConfig
    , svActionOnFailure
    , svStatus

    -- * StepConfig
    , StepConfig (..)
    , scName
    , scActionOnFailure
    , scHadoopJarStep

    -- * StepDetail
    , StepDetail (..)
    , sdStepConfig
    , sdExecutionStatusDetail

    -- * StepExecutionStatusDetail
    , StepExecutionStatusDetail (..)
    , sesdState
    , sesdCreationDateTime
    , sesdStartDateTime
    , sesdEndDateTime
    , sesdLastStateChangeReason

    -- * StepStateChangeReason
    , StepStateChangeReason (..)
    , sscrCode
    , sscrMessage

    -- * StepStatus
    , StepStatus (..)
    , sssState
    , sssStateChangeReason
    , sssTimeline

    -- * StepSummary
    , StepSummary (..)
    , sssyId
    , sssyName
    , sssyStatus

    -- * StepTimeline
    , StepTimeline (..)
    , ssfCreationDateTime
    , ssfStartDateTime
    , ssfEndDateTime

    -- * SupportedProductConfig
    , SupportedProductConfig (..)
    , spcName
    , spcArgs

    -- * Tag
    , Tag (..)
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
badBootstrapActionConfig f x =
    f (_badBootstrapActionConfig x)
        <&> \y -> x { _badBootstrapActionConfig = y }
{-# INLINE badBootstrapActionConfig #-}

instance FromJSON BootstrapActionDetail

instance ToJSON BootstrapActionDetail

-- | The Amazon EC2 Availability Zone for the job flow.
newtype PlacementType = PlacementType
    { _ptAvailabilityZone :: Text
      -- ^ The Amazon EC2 Availability Zone for the job flow.
    } deriving (Show, Generic)

-- | The Amazon EC2 Availability Zone for the job flow.
ptAvailabilityZone :: Lens' PlacementType (Text)
ptAvailabilityZone f x =
    f (_ptAvailabilityZone x)
        <&> \y -> x { _ptAvailabilityZone = y }
{-# INLINE ptAvailabilityZone #-}

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
aName f x =
    f (_aName x)
        <&> \y -> x { _aName = y }
{-# INLINE aName #-}

-- | The version of the application.
aVersion :: Lens' Application (Maybe Text)
aVersion f x =
    f (_aVersion x)
        <&> \y -> x { _aVersion = y }
{-# INLINE aVersion #-}

-- | Arguments for Amazon EMR to pass to the application.
aArgs :: Lens' Application ([Text])
aArgs f x =
    f (_aArgs x)
        <&> \y -> x { _aArgs = y }
{-# INLINE aArgs #-}

-- | This option is for advanced users only. This is meta information about
-- third-party applications that third-party vendors use for testing purposes.
aAdditionalInfo :: Lens' Application (Map Text Text)
aAdditionalInfo f x =
    f (_aAdditionalInfo x)
        <&> \y -> x { _aAdditionalInfo = y }
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
bacName f x =
    f (_bacName x)
        <&> \y -> x { _bacName = y }
{-# INLINE bacName #-}

-- | The script run by the bootstrap action.
bacScriptBootstrapAction :: Lens' BootstrapActionConfig (ScriptBootstrapActionConfig)
bacScriptBootstrapAction f x =
    f (_bacScriptBootstrapAction x)
        <&> \y -> x { _bacScriptBootstrapAction = y }
{-# INLINE bacScriptBootstrapAction #-}

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
cId f x =
    f (_cId x)
        <&> \y -> x { _cId = y }
{-# INLINE cId #-}

-- | The name of the cluster.
cName :: Lens' Cluster (Maybe Text)
cName f x =
    f (_cName x)
        <&> \y -> x { _cName = y }
{-# INLINE cName #-}

-- | The current status details about the cluster.
cStatus :: Lens' Cluster (Maybe ClusterStatus)
cStatus f x =
    f (_cStatus x)
        <&> \y -> x { _cStatus = y }
{-# INLINE cStatus #-}

-- | Provides information about the EC2 instances in a cluster grouped by
-- category. For example, key name, subnet ID, IAM instance profile, and so
-- on.
cEc2InstanceAttributes :: Lens' Cluster (Maybe Ec2InstanceAttributes)
cEc2InstanceAttributes f x =
    f (_cEc2InstanceAttributes x)
        <&> \y -> x { _cEc2InstanceAttributes = y }
{-# INLINE cEc2InstanceAttributes #-}

-- | The path to the Amazon S3 location where logs for this cluster are stored.
cLogUri :: Lens' Cluster (Maybe Text)
cLogUri f x =
    f (_cLogUri x)
        <&> \y -> x { _cLogUri = y }
{-# INLINE cLogUri #-}

-- | The AMI version requested for this cluster.JobFlowDetail$AmiVersion.-->.
cRequestedAmiVersion :: Lens' Cluster (Maybe Text)
cRequestedAmiVersion f x =
    f (_cRequestedAmiVersion x)
        <&> \y -> x { _cRequestedAmiVersion = y }
{-# INLINE cRequestedAmiVersion #-}

-- | The AMI version running on this cluster. This differs from the requested
-- version only if the requested version is a meta version, such as "latest".
-- JobFlowDetail$AmiVersion.-->.
cRunningAmiVersion :: Lens' Cluster (Maybe Text)
cRunningAmiVersion f x =
    f (_cRunningAmiVersion x)
        <&> \y -> x { _cRunningAmiVersion = y }
{-# INLINE cRunningAmiVersion #-}

-- | Specifies whether the cluster should terminate after completing all steps.
cAutoTerminate :: Lens' Cluster (Maybe Bool)
cAutoTerminate f x =
    f (_cAutoTerminate x)
        <&> \y -> x { _cAutoTerminate = y }
{-# INLINE cAutoTerminate #-}

-- | Indicates whether Amazon EMR will lock the cluster to prevent the EC2
-- instances from being terminated by an API call or user intervention, or in
-- the event of a cluster error.
cTerminationProtected :: Lens' Cluster (Maybe Bool)
cTerminationProtected f x =
    f (_cTerminationProtected x)
        <&> \y -> x { _cTerminationProtected = y }
{-# INLINE cTerminationProtected #-}

-- | Indicates whether the job flow is visible to all IAM users of the AWS
-- account associated with the job flow. If this value is set to true, all IAM
-- users of that AWS account can view and manage the job flow if they have the
-- proper policy permissions set. If this value is false, only the IAM user
-- that created the cluster can view and manage it. This value can be changed
-- using the SetVisibleToAllUsers action.
cVisibleToAllUsers :: Lens' Cluster (Maybe Bool)
cVisibleToAllUsers f x =
    f (_cVisibleToAllUsers x)
        <&> \y -> x { _cVisibleToAllUsers = y }
{-# INLINE cVisibleToAllUsers #-}

-- | The applications installed on this cluster.
cApplications :: Lens' Cluster ([Application])
cApplications f x =
    f (_cApplications x)
        <&> \y -> x { _cApplications = y }
{-# INLINE cApplications #-}

-- | A list of tags associated with a cluster.
cTags :: Lens' Cluster ([Tag])
cTags f x =
    f (_cTags x)
        <&> \y -> x { _cTags = y }
{-# INLINE cTags #-}

-- | The IAM role that will be assumed by the Amazon EMR service to access AWS
-- resources on your behalf.
cServiceRole :: Lens' Cluster (Maybe Text)
cServiceRole f x =
    f (_cServiceRole x)
        <&> \y -> x { _cServiceRole = y }
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
cscrCode f x =
    f (_cscrCode x)
        <&> \y -> x { _cscrCode = y }
{-# INLINE cscrCode #-}

-- | The descriptive message for the state change reason.
cscrMessage :: Lens' ClusterStateChangeReason (Maybe Text)
cscrMessage f x =
    f (_cscrMessage x)
        <&> \y -> x { _cscrMessage = y }
{-# INLINE cscrMessage #-}

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
csState f x =
    f (_csState x)
        <&> \y -> x { _csState = y }
{-# INLINE csState #-}

-- | The reason for the cluster status change.
csStateChangeReason :: Lens' ClusterStatus (Maybe ClusterStateChangeReason)
csStateChangeReason f x =
    f (_csStateChangeReason x)
        <&> \y -> x { _csStateChangeReason = y }
{-# INLINE csStateChangeReason #-}

-- | A timeline that represents the status of a cluster over the lifetime of the
-- cluster.
csTimeline :: Lens' ClusterStatus (Maybe ClusterTimeline)
csTimeline f x =
    f (_csTimeline x)
        <&> \y -> x { _csTimeline = y }
{-# INLINE csTimeline #-}

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
cwId f x =
    f (_cwId x)
        <&> \y -> x { _cwId = y }
{-# INLINE cwId #-}

-- | The name of the cluster.
cwName :: Lens' ClusterSummary (Maybe Text)
cwName f x =
    f (_cwName x)
        <&> \y -> x { _cwName = y }
{-# INLINE cwName #-}

-- | The details about the current status of the cluster.
cwStatus :: Lens' ClusterSummary (Maybe ClusterStatus)
cwStatus f x =
    f (_cwStatus x)
        <&> \y -> x { _cwStatus = y }
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
cuCreationDateTime f x =
    f (_cuCreationDateTime x)
        <&> \y -> x { _cuCreationDateTime = y }
{-# INLINE cuCreationDateTime #-}

-- | The date and time when the cluster was ready to execute steps.
cuReadyDateTime :: Lens' ClusterTimeline (Maybe POSIX)
cuReadyDateTime f x =
    f (_cuReadyDateTime x)
        <&> \y -> x { _cuReadyDateTime = y }
{-# INLINE cuReadyDateTime #-}

-- | The date and time when the cluster was terminated.
cuEndDateTime :: Lens' ClusterTimeline (Maybe POSIX)
cuEndDateTime f x =
    f (_cuEndDateTime x)
        <&> \y -> x { _cuEndDateTime = y }
{-# INLINE cuEndDateTime #-}

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
cdName f x =
    f (_cdName x)
        <&> \y -> x { _cdName = y }
{-# INLINE cdName #-}

-- | The Amazon S3 location of the command script.
cdScriptPath :: Lens' Command (Maybe Text)
cdScriptPath f x =
    f (_cdScriptPath x)
        <&> \y -> x { _cdScriptPath = y }
{-# INLINE cdScriptPath #-}

-- | Arguments for Amazon EMR to pass to the command for execution.
cdArgs :: Lens' Command ([Text])
cdArgs f x =
    f (_cdArgs x)
        <&> \y -> x { _cdArgs = y }
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
eiaEc2KeyName f x =
    f (_eiaEc2KeyName x)
        <&> \y -> x { _eiaEc2KeyName = y }
{-# INLINE eiaEc2KeyName #-}

-- | To launch the job flow in Amazon VPC, set this parameter to the identifier
-- of the Amazon VPC subnet where you want the job flow to launch. If you do
-- not specify this value, the job flow is launched in the normal AWS cloud,
-- outside of a VPC. Amazon VPC currently does not support cluster compute
-- quadruple extra large (cc1.4xlarge) instances. Thus, you cannot specify the
-- cc1.4xlarge instance type for nodes of a job flow launched in a VPC.
eiaEc2SubnetId :: Lens' Ec2InstanceAttributes (Maybe Text)
eiaEc2SubnetId f x =
    f (_eiaEc2SubnetId x)
        <&> \y -> x { _eiaEc2SubnetId = y }
{-# INLINE eiaEc2SubnetId #-}

-- | The Availability Zone in which the cluster will run.
eiaEc2AvailabilityZone :: Lens' Ec2InstanceAttributes (Maybe Text)
eiaEc2AvailabilityZone f x =
    f (_eiaEc2AvailabilityZone x)
        <&> \y -> x { _eiaEc2AvailabilityZone = y }
{-# INLINE eiaEc2AvailabilityZone #-}

-- | The IAM role that was specified when the job flow was launched. The EC2
-- instances of the job flow assume this role.
eiaIamInstanceProfile :: Lens' Ec2InstanceAttributes (Maybe Text)
eiaIamInstanceProfile f x =
    f (_eiaIamInstanceProfile x)
        <&> \y -> x { _eiaIamInstanceProfile = y }
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
hjscProperties f x =
    f (_hjscProperties x)
        <&> \y -> x { _hjscProperties = y }
{-# INLINE hjscProperties #-}

-- | A path to a JAR file run during the step.
hjscJar :: Lens' HadoopJarStepConfig (Text)
hjscJar f x =
    f (_hjscJar x)
        <&> \y -> x { _hjscJar = y }
{-# INLINE hjscJar #-}

-- | The name of the main class in the specified Java file. If not specified,
-- the JAR file should specify a Main-Class in its manifest file.
hjscMainClass :: Lens' HadoopJarStepConfig (Maybe Text)
hjscMainClass f x =
    f (_hjscMainClass x)
        <&> \y -> x { _hjscMainClass = y }
{-# INLINE hjscMainClass #-}

-- | A list of command line arguments passed to the JAR file's main function
-- when executed.
hjscArgs :: Lens' HadoopJarStepConfig ([Text])
hjscArgs f x =
    f (_hjscArgs x)
        <&> \y -> x { _hjscArgs = y }
{-# INLINE hjscArgs #-}

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
hscJar f x =
    f (_hscJar x)
        <&> \y -> x { _hscJar = y }
{-# INLINE hscJar #-}

-- | The list of Java properties that are set when the step runs. You can use
-- these properties to pass key value pairs to your main function.
hscProperties :: Lens' HadoopStepConfig (Map Text Text)
hscProperties f x =
    f (_hscProperties x)
        <&> \y -> x { _hscProperties = y }
{-# INLINE hscProperties #-}

-- | The name of the main class in the specified Java file. If not specified,
-- the JAR file should specify a main class in its manifest file.
hscMainClass :: Lens' HadoopStepConfig (Maybe Text)
hscMainClass f x =
    f (_hscMainClass x)
        <&> \y -> x { _hscMainClass = y }
{-# INLINE hscMainClass #-}

-- | The list of command line arguments to pass to the JAR file's main function
-- for execution.
hscArgs :: Lens' HadoopStepConfig ([Text])
hscArgs f x =
    f (_hscArgs x)
        <&> \y -> x { _hscArgs = y }
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
ieId f x =
    f (_ieId x)
        <&> \y -> x { _ieId = y }
{-# INLINE ieId #-}

-- | The unique identifier of the instance in Amazon EC2.
ieEc2InstanceId :: Lens' Instance (Maybe Text)
ieEc2InstanceId f x =
    f (_ieEc2InstanceId x)
        <&> \y -> x { _ieEc2InstanceId = y }
{-# INLINE ieEc2InstanceId #-}

-- | The public DNS name of the instance.
iePublicDnsName :: Lens' Instance (Maybe Text)
iePublicDnsName f x =
    f (_iePublicDnsName x)
        <&> \y -> x { _iePublicDnsName = y }
{-# INLINE iePublicDnsName #-}

-- | The public IP address of the instance.
iePublicIpAddress :: Lens' Instance (Maybe Text)
iePublicIpAddress f x =
    f (_iePublicIpAddress x)
        <&> \y -> x { _iePublicIpAddress = y }
{-# INLINE iePublicIpAddress #-}

-- | The private DNS name of the instance.
iePrivateDnsName :: Lens' Instance (Maybe Text)
iePrivateDnsName f x =
    f (_iePrivateDnsName x)
        <&> \y -> x { _iePrivateDnsName = y }
{-# INLINE iePrivateDnsName #-}

-- | The private IP address of the instance.
iePrivateIpAddress :: Lens' Instance (Maybe Text)
iePrivateIpAddress f x =
    f (_iePrivateIpAddress x)
        <&> \y -> x { _iePrivateIpAddress = y }
{-# INLINE iePrivateIpAddress #-}

-- | The current status of the instance.
ieStatus :: Lens' Instance (Maybe InstanceStatus)
ieStatus f x =
    f (_ieStatus x)
        <&> \y -> x { _ieStatus = y }
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
igId f x =
    f (_igId x)
        <&> \y -> x { _igId = y }
{-# INLINE igId #-}

-- | The name of the instance group.
igName :: Lens' InstanceGroup (Maybe Text)
igName f x =
    f (_igName x)
        <&> \y -> x { _igName = y }
{-# INLINE igName #-}

-- | The marketplace to provision instances for this group. Valid values are
-- ON_DEMAND or SPOT.
igMarket :: Lens' InstanceGroup (Maybe MarketType)
igMarket f x =
    f (_igMarket x)
        <&> \y -> x { _igMarket = y }
{-# INLINE igMarket #-}

-- | The type of the instance group. Valid values are MASTER, CORE or TASK.
igInstanceGroupType :: Lens' InstanceGroup (Maybe InstanceGroupType)
igInstanceGroupType f x =
    f (_igInstanceGroupType x)
        <&> \y -> x { _igInstanceGroupType = y }
{-# INLINE igInstanceGroupType #-}

-- | The bid price for each EC2 instance in the instance group when launching
-- nodes as Spot Instances, expressed in USD.
igBidPrice :: Lens' InstanceGroup (Maybe Text)
igBidPrice f x =
    f (_igBidPrice x)
        <&> \y -> x { _igBidPrice = y }
{-# INLINE igBidPrice #-}

-- | The EC2 instance type for all instances in the instance group.
igInstanceType :: Lens' InstanceGroup (Maybe Text)
igInstanceType f x =
    f (_igInstanceType x)
        <&> \y -> x { _igInstanceType = y }
{-# INLINE igInstanceType #-}

-- | The target number of instances for the instance group.
igRequestedInstanceCount :: Lens' InstanceGroup (Maybe Integer)
igRequestedInstanceCount f x =
    f (_igRequestedInstanceCount x)
        <&> \y -> x { _igRequestedInstanceCount = y }
{-# INLINE igRequestedInstanceCount #-}

-- | The number of instances currently running in this instance group.
igRunningInstanceCount :: Lens' InstanceGroup (Maybe Integer)
igRunningInstanceCount f x =
    f (_igRunningInstanceCount x)
        <&> \y -> x { _igRunningInstanceCount = y }
{-# INLINE igRunningInstanceCount #-}

-- | The current status of the instance group.
igStatus :: Lens' InstanceGroup (Maybe InstanceGroupStatus)
igStatus f x =
    f (_igStatus x)
        <&> \y -> x { _igStatus = y }
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
igcName f x =
    f (_igcName x)
        <&> \y -> x { _igcName = y }
{-# INLINE igcName #-}

-- | Market type of the Amazon EC2 instances used to create a cluster node.
igcMarket :: Lens' InstanceGroupConfig (Maybe MarketType)
igcMarket f x =
    f (_igcMarket x)
        <&> \y -> x { _igcMarket = y }
{-# INLINE igcMarket #-}

-- | The role of the instance group in the cluster.
igcInstanceRole :: Lens' InstanceGroupConfig (InstanceRoleType)
igcInstanceRole f x =
    f (_igcInstanceRole x)
        <&> \y -> x { _igcInstanceRole = y }
{-# INLINE igcInstanceRole #-}

-- | Bid price for each Amazon EC2 instance in the instance group when launching
-- nodes as Spot Instances, expressed in USD.
igcBidPrice :: Lens' InstanceGroupConfig (Maybe Text)
igcBidPrice f x =
    f (_igcBidPrice x)
        <&> \y -> x { _igcBidPrice = y }
{-# INLINE igcBidPrice #-}

-- | The Amazon EC2 instance type for all instances in the instance group.
igcInstanceType :: Lens' InstanceGroupConfig (Text)
igcInstanceType f x =
    f (_igcInstanceType x)
        <&> \y -> x { _igcInstanceType = y }
{-# INLINE igcInstanceType #-}

-- | Target number of instances for the instance group.
igcInstanceCount :: Lens' InstanceGroupConfig (Integer)
igcInstanceCount f x =
    f (_igcInstanceCount x)
        <&> \y -> x { _igcInstanceCount = y }
{-# INLINE igcInstanceCount #-}

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
igdInstanceGroupId f x =
    f (_igdInstanceGroupId x)
        <&> \y -> x { _igdInstanceGroupId = y }
{-# INLINE igdInstanceGroupId #-}

-- | Friendly name for the instance group.
igdName :: Lens' InstanceGroupDetail (Maybe Text)
igdName f x =
    f (_igdName x)
        <&> \y -> x { _igdName = y }
{-# INLINE igdName #-}

-- | Market type of the Amazon EC2 instances used to create a cluster node.
igdMarket :: Lens' InstanceGroupDetail (MarketType)
igdMarket f x =
    f (_igdMarket x)
        <&> \y -> x { _igdMarket = y }
{-# INLINE igdMarket #-}

-- | Instance group role in the cluster.
igdInstanceRole :: Lens' InstanceGroupDetail (InstanceRoleType)
igdInstanceRole f x =
    f (_igdInstanceRole x)
        <&> \y -> x { _igdInstanceRole = y }
{-# INLINE igdInstanceRole #-}

-- | Bid price for EC2 Instances when launching nodes as Spot Instances,
-- expressed in USD.
igdBidPrice :: Lens' InstanceGroupDetail (Maybe Text)
igdBidPrice f x =
    f (_igdBidPrice x)
        <&> \y -> x { _igdBidPrice = y }
{-# INLINE igdBidPrice #-}

-- | Amazon EC2 Instance type.
igdInstanceType :: Lens' InstanceGroupDetail (Text)
igdInstanceType f x =
    f (_igdInstanceType x)
        <&> \y -> x { _igdInstanceType = y }
{-# INLINE igdInstanceType #-}

-- | Target number of instances to run in the instance group.
igdInstanceRequestCount :: Lens' InstanceGroupDetail (Integer)
igdInstanceRequestCount f x =
    f (_igdInstanceRequestCount x)
        <&> \y -> x { _igdInstanceRequestCount = y }
{-# INLINE igdInstanceRequestCount #-}

-- | Actual count of running instances.
igdInstanceRunningCount :: Lens' InstanceGroupDetail (Integer)
igdInstanceRunningCount f x =
    f (_igdInstanceRunningCount x)
        <&> \y -> x { _igdInstanceRunningCount = y }
{-# INLINE igdInstanceRunningCount #-}

-- | State of instance group. The following values are deprecated: STARTING,
-- TERMINATED, and FAILED.
igdState :: Lens' InstanceGroupDetail (InstanceGroupState)
igdState f x =
    f (_igdState x)
        <&> \y -> x { _igdState = y }
{-# INLINE igdState #-}

-- | Details regarding the state of the instance group.
igdLastStateChangeReason :: Lens' InstanceGroupDetail (Maybe Text)
igdLastStateChangeReason f x =
    f (_igdLastStateChangeReason x)
        <&> \y -> x { _igdLastStateChangeReason = y }
{-# INLINE igdLastStateChangeReason #-}

-- | The date/time the instance group was created.
igdCreationDateTime :: Lens' InstanceGroupDetail (POSIX)
igdCreationDateTime f x =
    f (_igdCreationDateTime x)
        <&> \y -> x { _igdCreationDateTime = y }
{-# INLINE igdCreationDateTime #-}

-- | The date/time the instance group was started.
igdStartDateTime :: Lens' InstanceGroupDetail (Maybe POSIX)
igdStartDateTime f x =
    f (_igdStartDateTime x)
        <&> \y -> x { _igdStartDateTime = y }
{-# INLINE igdStartDateTime #-}

-- | The date/time the instance group was available to the cluster.
igdReadyDateTime :: Lens' InstanceGroupDetail (Maybe POSIX)
igdReadyDateTime f x =
    f (_igdReadyDateTime x)
        <&> \y -> x { _igdReadyDateTime = y }
{-# INLINE igdReadyDateTime #-}

-- | The date/time the instance group was terminated.
igdEndDateTime :: Lens' InstanceGroupDetail (Maybe POSIX)
igdEndDateTime f x =
    f (_igdEndDateTime x)
        <&> \y -> x { _igdEndDateTime = y }
{-# INLINE igdEndDateTime #-}

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
igmcInstanceGroupId f x =
    f (_igmcInstanceGroupId x)
        <&> \y -> x { _igmcInstanceGroupId = y }
{-# INLINE igmcInstanceGroupId #-}

-- | Target size for the instance group.
igmcInstanceCount :: Lens' InstanceGroupModifyConfig (Maybe Integer)
igmcInstanceCount f x =
    f (_igmcInstanceCount x)
        <&> \y -> x { _igmcInstanceCount = y }
{-# INLINE igmcInstanceCount #-}

-- | The EC2 InstanceIds to terminate. For advanced users only. Once you
-- terminate the instances, the instance group will not return to its original
-- requested size.
igmcEC2InstanceIdsToTerminate :: Lens' InstanceGroupModifyConfig ([Text])
igmcEC2InstanceIdsToTerminate f x =
    f (_igmcEC2InstanceIdsToTerminate x)
        <&> \y -> x { _igmcEC2InstanceIdsToTerminate = y }
{-# INLINE igmcEC2InstanceIdsToTerminate #-}

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
igscrCode f x =
    f (_igscrCode x)
        <&> \y -> x { _igscrCode = y }
{-# INLINE igscrCode #-}

-- | The status change reason description.
igscrMessage :: Lens' InstanceGroupStateChangeReason (Maybe Text)
igscrMessage f x =
    f (_igscrMessage x)
        <&> \y -> x { _igscrMessage = y }
{-# INLINE igscrMessage #-}

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
iguState f x =
    f (_iguState x)
        <&> \y -> x { _iguState = y }
{-# INLINE iguState #-}

-- | The status change reason details for the instance group.
iguStateChangeReason :: Lens' InstanceGroupStatus (Maybe InstanceGroupStateChangeReason)
iguStateChangeReason f x =
    f (_iguStateChangeReason x)
        <&> \y -> x { _iguStateChangeReason = y }
{-# INLINE iguStateChangeReason #-}

-- | The timeline of the instance group status over time.
iguTimeline :: Lens' InstanceGroupStatus (Maybe InstanceGroupTimeline)
iguTimeline f x =
    f (_iguTimeline x)
        <&> \y -> x { _iguTimeline = y }
{-# INLINE iguTimeline #-}

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
igwCreationDateTime f x =
    f (_igwCreationDateTime x)
        <&> \y -> x { _igwCreationDateTime = y }
{-# INLINE igwCreationDateTime #-}

-- | The date and time when the instance group became ready to perform tasks.
igwReadyDateTime :: Lens' InstanceGroupTimeline (Maybe POSIX)
igwReadyDateTime f x =
    f (_igwReadyDateTime x)
        <&> \y -> x { _igwReadyDateTime = y }
{-# INLINE igwReadyDateTime #-}

-- | The date and time when the instance group terminated.
igwEndDateTime :: Lens' InstanceGroupTimeline (Maybe POSIX)
igwEndDateTime f x =
    f (_igwEndDateTime x)
        <&> \y -> x { _igwEndDateTime = y }
{-# INLINE igwEndDateTime #-}

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
iscrCode f x =
    f (_iscrCode x)
        <&> \y -> x { _iscrCode = y }
{-# INLINE iscrCode #-}

-- | The status change reason description.
iscrMessage :: Lens' InstanceStateChangeReason (Maybe Text)
iscrMessage f x =
    f (_iscrMessage x)
        <&> \y -> x { _iscrMessage = y }
{-# INLINE iscrMessage #-}

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
izState f x =
    f (_izState x)
        <&> \y -> x { _izState = y }
{-# INLINE izState #-}

-- | The details of the status change reason for the instance.
izStateChangeReason :: Lens' InstanceStatus (Maybe InstanceStateChangeReason)
izStateChangeReason f x =
    f (_izStateChangeReason x)
        <&> \y -> x { _izStateChangeReason = y }
{-# INLINE izStateChangeReason #-}

-- | The timeline of the instance status over time.
izTimeline :: Lens' InstanceStatus (Maybe InstanceTimeline)
izTimeline f x =
    f (_izTimeline x)
        <&> \y -> x { _izTimeline = y }
{-# INLINE izTimeline #-}

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
iifCreationDateTime f x =
    f (_iifCreationDateTime x)
        <&> \y -> x { _iifCreationDateTime = y }
{-# INLINE iifCreationDateTime #-}

-- | The date and time when the instance was ready to perform tasks.
iifReadyDateTime :: Lens' InstanceTimeline (Maybe POSIX)
iifReadyDateTime f x =
    f (_iifReadyDateTime x)
        <&> \y -> x { _iifReadyDateTime = y }
{-# INLINE iifReadyDateTime #-}

-- | The date and time when the instance was terminated.
iifEndDateTime :: Lens' InstanceTimeline (Maybe POSIX)
iifEndDateTime f x =
    f (_iifEndDateTime x)
        <&> \y -> x { _iifEndDateTime = y }
{-# INLINE iifEndDateTime #-}

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
jfdJobFlowId f x =
    f (_jfdJobFlowId x)
        <&> \y -> x { _jfdJobFlowId = y }
{-# INLINE jfdJobFlowId #-}

-- | The name of the job flow.
jfdName :: Lens' JobFlowDetail (Text)
jfdName f x =
    f (_jfdName x)
        <&> \y -> x { _jfdName = y }
{-# INLINE jfdName #-}

-- | The location in Amazon S3 where log files for the job are stored.
jfdLogUri :: Lens' JobFlowDetail (Maybe Text)
jfdLogUri f x =
    f (_jfdLogUri x)
        <&> \y -> x { _jfdLogUri = y }
{-# INLINE jfdLogUri #-}

-- | The version of the AMI used to initialize Amazon EC2 instances in the job
-- flow. For a list of AMI versions currently supported by Amazon
-- ElasticMapReduce, go to AMI Versions Supported in Elastic MapReduce in the
-- Amazon Elastic MapReduce Developer's Guide.
jfdAmiVersion :: Lens' JobFlowDetail (Maybe Text)
jfdAmiVersion f x =
    f (_jfdAmiVersion x)
        <&> \y -> x { _jfdAmiVersion = y }
{-# INLINE jfdAmiVersion #-}

-- | Describes the execution status of the job flow.
jfdExecutionStatusDetail :: Lens' JobFlowDetail (JobFlowExecutionStatusDetail)
jfdExecutionStatusDetail f x =
    f (_jfdExecutionStatusDetail x)
        <&> \y -> x { _jfdExecutionStatusDetail = y }
{-# INLINE jfdExecutionStatusDetail #-}

-- | Describes the Amazon EC2 instances of the job flow.
jfdInstances :: Lens' JobFlowDetail (JobFlowInstancesDetail)
jfdInstances f x =
    f (_jfdInstances x)
        <&> \y -> x { _jfdInstances = y }
{-# INLINE jfdInstances #-}

-- | A list of steps run by the job flow.
jfdSteps :: Lens' JobFlowDetail ([StepDetail])
jfdSteps f x =
    f (_jfdSteps x)
        <&> \y -> x { _jfdSteps = y }
{-# INLINE jfdSteps #-}

-- | A list of the bootstrap actions run by the job flow.
jfdBootstrapActions :: Lens' JobFlowDetail ([BootstrapActionDetail])
jfdBootstrapActions f x =
    f (_jfdBootstrapActions x)
        <&> \y -> x { _jfdBootstrapActions = y }
{-# INLINE jfdBootstrapActions #-}

-- | A list of strings set by third party software when the job flow is
-- launched. If you are not using third party software to manage the job flow
-- this value is empty.
jfdSupportedProducts :: Lens' JobFlowDetail ([Text])
jfdSupportedProducts f x =
    f (_jfdSupportedProducts x)
        <&> \y -> x { _jfdSupportedProducts = y }
{-# INLINE jfdSupportedProducts #-}

-- | Specifies whether the job flow is visible to all IAM users of the AWS
-- account associated with the job flow. If this value is set to true, all IAM
-- users of that AWS account can view and (if they have the proper policy
-- permissions set) manage the job flow. If it is set to false, only the IAM
-- user that created the job flow can view and manage it. This value can be
-- changed using the SetVisibleToAllUsers action.
jfdVisibleToAllUsers :: Lens' JobFlowDetail (Maybe Bool)
jfdVisibleToAllUsers f x =
    f (_jfdVisibleToAllUsers x)
        <&> \y -> x { _jfdVisibleToAllUsers = y }
{-# INLINE jfdVisibleToAllUsers #-}

-- | The IAM role that was specified when the job flow was launched. The EC2
-- instances of the job flow assume this role.
jfdJobFlowRole :: Lens' JobFlowDetail (Maybe Text)
jfdJobFlowRole f x =
    f (_jfdJobFlowRole x)
        <&> \y -> x { _jfdJobFlowRole = y }
{-# INLINE jfdJobFlowRole #-}

-- | The IAM role that will be assumed by the Amazon EMR service to access AWS
-- resources on your behalf.
jfdServiceRole :: Lens' JobFlowDetail (Maybe Text)
jfdServiceRole f x =
    f (_jfdServiceRole x)
        <&> \y -> x { _jfdServiceRole = y }
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
jfesdState f x =
    f (_jfesdState x)
        <&> \y -> x { _jfesdState = y }
{-# INLINE jfesdState #-}

-- | The creation date and time of the job flow.
jfesdCreationDateTime :: Lens' JobFlowExecutionStatusDetail (POSIX)
jfesdCreationDateTime f x =
    f (_jfesdCreationDateTime x)
        <&> \y -> x { _jfesdCreationDateTime = y }
{-# INLINE jfesdCreationDateTime #-}

-- | The start date and time of the job flow.
jfesdStartDateTime :: Lens' JobFlowExecutionStatusDetail (Maybe POSIX)
jfesdStartDateTime f x =
    f (_jfesdStartDateTime x)
        <&> \y -> x { _jfesdStartDateTime = y }
{-# INLINE jfesdStartDateTime #-}

-- | The date and time when the job flow was ready to start running bootstrap
-- actions.
jfesdReadyDateTime :: Lens' JobFlowExecutionStatusDetail (Maybe POSIX)
jfesdReadyDateTime f x =
    f (_jfesdReadyDateTime x)
        <&> \y -> x { _jfesdReadyDateTime = y }
{-# INLINE jfesdReadyDateTime #-}

-- | The completion date and time of the job flow.
jfesdEndDateTime :: Lens' JobFlowExecutionStatusDetail (Maybe POSIX)
jfesdEndDateTime f x =
    f (_jfesdEndDateTime x)
        <&> \y -> x { _jfesdEndDateTime = y }
{-# INLINE jfesdEndDateTime #-}

-- | Description of the job flow last changed state.
jfesdLastStateChangeReason :: Lens' JobFlowExecutionStatusDetail (Maybe Text)
jfesdLastStateChangeReason f x =
    f (_jfesdLastStateChangeReason x)
        <&> \y -> x { _jfesdLastStateChangeReason = y }
{-# INLINE jfesdLastStateChangeReason #-}

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
jficMasterInstanceType f x =
    f (_jficMasterInstanceType x)
        <&> \y -> x { _jficMasterInstanceType = y }
{-# INLINE jficMasterInstanceType #-}

-- | The EC2 instance type of the slave nodes.
jficSlaveInstanceType :: Lens' JobFlowInstancesConfig (Maybe Text)
jficSlaveInstanceType f x =
    f (_jficSlaveInstanceType x)
        <&> \y -> x { _jficSlaveInstanceType = y }
{-# INLINE jficSlaveInstanceType #-}

-- | The number of Amazon EC2 instances used to execute the job flow.
jficInstanceCount :: Lens' JobFlowInstancesConfig (Maybe Integer)
jficInstanceCount f x =
    f (_jficInstanceCount x)
        <&> \y -> x { _jficInstanceCount = y }
{-# INLINE jficInstanceCount #-}

-- | Configuration for the job flow's instance groups.
jficInstanceGroups :: Lens' JobFlowInstancesConfig ([InstanceGroupConfig])
jficInstanceGroups f x =
    f (_jficInstanceGroups x)
        <&> \y -> x { _jficInstanceGroups = y }
{-# INLINE jficInstanceGroups #-}

-- | The name of the Amazon EC2 key pair that can be used to ssh to the master
-- node as the user called "hadoop.".
jficEc2KeyName :: Lens' JobFlowInstancesConfig (Maybe Text)
jficEc2KeyName f x =
    f (_jficEc2KeyName x)
        <&> \y -> x { _jficEc2KeyName = y }
{-# INLINE jficEc2KeyName #-}

-- | The Availability Zone the job flow will run in.
jficPlacement :: Lens' JobFlowInstancesConfig (Maybe PlacementType)
jficPlacement f x =
    f (_jficPlacement x)
        <&> \y -> x { _jficPlacement = y }
{-# INLINE jficPlacement #-}

-- | Specifies whether the job flow should terminate after completing all steps.
jficKeepJobFlowAliveWhenNoSteps :: Lens' JobFlowInstancesConfig (Maybe Bool)
jficKeepJobFlowAliveWhenNoSteps f x =
    f (_jficKeepJobFlowAliveWhenNoSteps x)
        <&> \y -> x { _jficKeepJobFlowAliveWhenNoSteps = y }
{-# INLINE jficKeepJobFlowAliveWhenNoSteps #-}

-- | Specifies whether to lock the job flow to prevent the Amazon EC2 instances
-- from being terminated by API call, user intervention, or in the event of a
-- job flow error.
jficTerminationProtected :: Lens' JobFlowInstancesConfig (Maybe Bool)
jficTerminationProtected f x =
    f (_jficTerminationProtected x)
        <&> \y -> x { _jficTerminationProtected = y }
{-# INLINE jficTerminationProtected #-}

-- | The Hadoop version for the job flow. Valid inputs are "0.18", "0.20", or
-- "0.20.205". If you do not set this value, the default of 0.18 is used,
-- unless the AmiVersion parameter is set in the RunJobFlow call, in which
-- case the default version of Hadoop for that AMI version is used.
jficHadoopVersion :: Lens' JobFlowInstancesConfig (Maybe Text)
jficHadoopVersion f x =
    f (_jficHadoopVersion x)
        <&> \y -> x { _jficHadoopVersion = y }
{-# INLINE jficHadoopVersion #-}

-- | To launch the job flow in Amazon Virtual Private Cloud (Amazon VPC), set
-- this parameter to the identifier of the Amazon VPC subnet where you want
-- the job flow to launch. If you do not specify this value, the job flow is
-- launched in the normal Amazon Web Services cloud, outside of an Amazon VPC.
-- Amazon VPC currently does not support cluster compute quadruple extra large
-- (cc1.4xlarge) instances. Thus you cannot specify the cc1.4xlarge instance
-- type for nodes of a job flow launched in a Amazon VPC.
jficEc2SubnetId :: Lens' JobFlowInstancesConfig (Maybe Text)
jficEc2SubnetId f x =
    f (_jficEc2SubnetId x)
        <&> \y -> x { _jficEc2SubnetId = y }
{-# INLINE jficEc2SubnetId #-}

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
jfidMasterInstanceType f x =
    f (_jfidMasterInstanceType x)
        <&> \y -> x { _jfidMasterInstanceType = y }
{-# INLINE jfidMasterInstanceType #-}

-- | The DNS name of the master node.
jfidMasterPublicDnsName :: Lens' JobFlowInstancesDetail (Maybe Text)
jfidMasterPublicDnsName f x =
    f (_jfidMasterPublicDnsName x)
        <&> \y -> x { _jfidMasterPublicDnsName = y }
{-# INLINE jfidMasterPublicDnsName #-}

-- | The Amazon EC2 instance identifier of the master node.
jfidMasterInstanceId :: Lens' JobFlowInstancesDetail (Maybe Text)
jfidMasterInstanceId f x =
    f (_jfidMasterInstanceId x)
        <&> \y -> x { _jfidMasterInstanceId = y }
{-# INLINE jfidMasterInstanceId #-}

-- | The Amazon EC2 slave node instance type.
jfidSlaveInstanceType :: Lens' JobFlowInstancesDetail (Text)
jfidSlaveInstanceType f x =
    f (_jfidSlaveInstanceType x)
        <&> \y -> x { _jfidSlaveInstanceType = y }
{-# INLINE jfidSlaveInstanceType #-}

-- | The number of Amazon EC2 instances in the cluster. If the value is 1, the
-- same instance serves as both the master and slave node. If the value is
-- greater than 1, one instance is the master node and all others are slave
-- nodes.
jfidInstanceCount :: Lens' JobFlowInstancesDetail (Integer)
jfidInstanceCount f x =
    f (_jfidInstanceCount x)
        <&> \y -> x { _jfidInstanceCount = y }
{-# INLINE jfidInstanceCount #-}

-- | Details about the job flow's instance groups.
jfidInstanceGroups :: Lens' JobFlowInstancesDetail ([InstanceGroupDetail])
jfidInstanceGroups f x =
    f (_jfidInstanceGroups x)
        <&> \y -> x { _jfidInstanceGroups = y }
{-# INLINE jfidInstanceGroups #-}

-- | An approximation of the cost of the job flow, represented in
-- m1.small/hours. This value is incremented once for every hour an m1.small
-- runs. Larger instances are weighted more, so an Amazon EC2 instance that is
-- roughly four times more expensive would result in the normalized instance
-- hours being incremented by four. This result is only an approximation and
-- does not reflect the actual billing rate.
jfidNormalizedInstanceHours :: Lens' JobFlowInstancesDetail (Maybe Integer)
jfidNormalizedInstanceHours f x =
    f (_jfidNormalizedInstanceHours x)
        <&> \y -> x { _jfidNormalizedInstanceHours = y }
{-# INLINE jfidNormalizedInstanceHours #-}

-- | The name of an Amazon EC2 key pair that can be used to ssh to the master
-- node of job flow.
jfidEc2KeyName :: Lens' JobFlowInstancesDetail (Maybe Text)
jfidEc2KeyName f x =
    f (_jfidEc2KeyName x)
        <&> \y -> x { _jfidEc2KeyName = y }
{-# INLINE jfidEc2KeyName #-}

-- | For job flows launched within Amazon Virtual Private Cloud, this value
-- specifies the identifier of the subnet where the job flow was launched.
jfidEc2SubnetId :: Lens' JobFlowInstancesDetail (Maybe Text)
jfidEc2SubnetId f x =
    f (_jfidEc2SubnetId x)
        <&> \y -> x { _jfidEc2SubnetId = y }
{-# INLINE jfidEc2SubnetId #-}

-- | The Amazon EC2 Availability Zone for the job flow.
jfidPlacement :: Lens' JobFlowInstancesDetail (Maybe PlacementType)
jfidPlacement f x =
    f (_jfidPlacement x)
        <&> \y -> x { _jfidPlacement = y }
{-# INLINE jfidPlacement #-}

-- | Specifies whether the job flow should terminate after completing all steps.
jfidKeepJobFlowAliveWhenNoSteps :: Lens' JobFlowInstancesDetail (Maybe Bool)
jfidKeepJobFlowAliveWhenNoSteps f x =
    f (_jfidKeepJobFlowAliveWhenNoSteps x)
        <&> \y -> x { _jfidKeepJobFlowAliveWhenNoSteps = y }
{-# INLINE jfidKeepJobFlowAliveWhenNoSteps #-}

-- | Specifies whether the Amazon EC2 instances in the cluster are protected
-- from termination by API calls, user intervention, or in the event of a job
-- flow error.
jfidTerminationProtected :: Lens' JobFlowInstancesDetail (Maybe Bool)
jfidTerminationProtected f x =
    f (_jfidTerminationProtected x)
        <&> \y -> x { _jfidTerminationProtected = y }
{-# INLINE jfidTerminationProtected #-}

-- | The Hadoop version for the job flow.
jfidHadoopVersion :: Lens' JobFlowInstancesDetail (Maybe Text)
jfidHadoopVersion f x =
    f (_jfidHadoopVersion x)
        <&> \y -> x { _jfidHadoopVersion = y }
{-# INLINE jfidHadoopVersion #-}

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
kvKey f x =
    f (_kvKey x)
        <&> \y -> x { _kvKey = y }
{-# INLINE kvKey #-}

-- | The value part of the identified key.
kvValue :: Lens' KeyValue (Maybe Text)
kvValue f x =
    f (_kvValue x)
        <&> \y -> x { _kvValue = y }
{-# INLINE kvValue #-}

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
sbacPath f x =
    f (_sbacPath x)
        <&> \y -> x { _sbacPath = y }
{-# INLINE sbacPath #-}

-- | A list of command line arguments to pass to the bootstrap action script.
sbacArgs :: Lens' ScriptBootstrapActionConfig ([Text])
sbacArgs f x =
    f (_sbacArgs x)
        <&> \y -> x { _sbacArgs = y }
{-# INLINE sbacArgs #-}

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
svId f x =
    f (_svId x)
        <&> \y -> x { _svId = y }
{-# INLINE svId #-}

-- | The name of the cluster step.
svName :: Lens' Step (Maybe Text)
svName f x =
    f (_svName x)
        <&> \y -> x { _svName = y }
{-# INLINE svName #-}

-- | The Hadoop job configuration of the cluster step.
svConfig :: Lens' Step (Maybe HadoopStepConfig)
svConfig f x =
    f (_svConfig x)
        <&> \y -> x { _svConfig = y }
{-# INLINE svConfig #-}

-- | This specifies what action to take when the cluster step fails. Possible
-- values are TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE.
svActionOnFailure :: Lens' Step (Maybe ActionOnFailure)
svActionOnFailure f x =
    f (_svActionOnFailure x)
        <&> \y -> x { _svActionOnFailure = y }
{-# INLINE svActionOnFailure #-}

-- | The current execution status details of the cluster step.
svStatus :: Lens' Step (Maybe StepStatus)
svStatus f x =
    f (_svStatus x)
        <&> \y -> x { _svStatus = y }
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
scName f x =
    f (_scName x)
        <&> \y -> x { _scName = y }
{-# INLINE scName #-}

-- | The action to take if the job flow step fails.
scActionOnFailure :: Lens' StepConfig (Maybe ActionOnFailure)
scActionOnFailure f x =
    f (_scActionOnFailure x)
        <&> \y -> x { _scActionOnFailure = y }
{-# INLINE scActionOnFailure #-}

-- | The JAR file used for the job flow step.
scHadoopJarStep :: Lens' StepConfig (HadoopJarStepConfig)
scHadoopJarStep f x =
    f (_scHadoopJarStep x)
        <&> \y -> x { _scHadoopJarStep = y }
{-# INLINE scHadoopJarStep #-}

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
sdStepConfig f x =
    f (_sdStepConfig x)
        <&> \y -> x { _sdStepConfig = y }
{-# INLINE sdStepConfig #-}

-- | The description of the step status.
sdExecutionStatusDetail :: Lens' StepDetail (StepExecutionStatusDetail)
sdExecutionStatusDetail f x =
    f (_sdExecutionStatusDetail x)
        <&> \y -> x { _sdExecutionStatusDetail = y }
{-# INLINE sdExecutionStatusDetail #-}

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
sesdState f x =
    f (_sesdState x)
        <&> \y -> x { _sesdState = y }
{-# INLINE sesdState #-}

-- | The creation date and time of the step.
sesdCreationDateTime :: Lens' StepExecutionStatusDetail (POSIX)
sesdCreationDateTime f x =
    f (_sesdCreationDateTime x)
        <&> \y -> x { _sesdCreationDateTime = y }
{-# INLINE sesdCreationDateTime #-}

-- | The start date and time of the step.
sesdStartDateTime :: Lens' StepExecutionStatusDetail (Maybe POSIX)
sesdStartDateTime f x =
    f (_sesdStartDateTime x)
        <&> \y -> x { _sesdStartDateTime = y }
{-# INLINE sesdStartDateTime #-}

-- | The completion date and time of the step.
sesdEndDateTime :: Lens' StepExecutionStatusDetail (Maybe POSIX)
sesdEndDateTime f x =
    f (_sesdEndDateTime x)
        <&> \y -> x { _sesdEndDateTime = y }
{-# INLINE sesdEndDateTime #-}

-- | A description of the step's current state.
sesdLastStateChangeReason :: Lens' StepExecutionStatusDetail (Maybe Text)
sesdLastStateChangeReason f x =
    f (_sesdLastStateChangeReason x)
        <&> \y -> x { _sesdLastStateChangeReason = y }
{-# INLINE sesdLastStateChangeReason #-}

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
sscrCode f x =
    f (_sscrCode x)
        <&> \y -> x { _sscrCode = y }
{-# INLINE sscrCode #-}

-- | The descriptive message for the state change reason.
sscrMessage :: Lens' StepStateChangeReason (Maybe Text)
sscrMessage f x =
    f (_sscrMessage x)
        <&> \y -> x { _sscrMessage = y }
{-# INLINE sscrMessage #-}

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
sssState f x =
    f (_sssState x)
        <&> \y -> x { _sssState = y }
{-# INLINE sssState #-}

-- | The reason for the step execution status change.
sssStateChangeReason :: Lens' StepStatus (Maybe StepStateChangeReason)
sssStateChangeReason f x =
    f (_sssStateChangeReason x)
        <&> \y -> x { _sssStateChangeReason = y }
{-# INLINE sssStateChangeReason #-}

-- | The timeline of the cluster step status over time.
sssTimeline :: Lens' StepStatus (Maybe StepTimeline)
sssTimeline f x =
    f (_sssTimeline x)
        <&> \y -> x { _sssTimeline = y }
{-# INLINE sssTimeline #-}

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
sssyId f x =
    f (_sssyId x)
        <&> \y -> x { _sssyId = y }
{-# INLINE sssyId #-}

-- | The name of the cluster step.
sssyName :: Lens' StepSummary (Maybe Text)
sssyName f x =
    f (_sssyName x)
        <&> \y -> x { _sssyName = y }
{-# INLINE sssyName #-}

-- | The current execution status details of the cluster step.
sssyStatus :: Lens' StepSummary (Maybe StepStatus)
sssyStatus f x =
    f (_sssyStatus x)
        <&> \y -> x { _sssyStatus = y }
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
ssfCreationDateTime f x =
    f (_ssfCreationDateTime x)
        <&> \y -> x { _ssfCreationDateTime = y }
{-# INLINE ssfCreationDateTime #-}

-- | The date and time when the cluster step execution started.
ssfStartDateTime :: Lens' StepTimeline (Maybe POSIX)
ssfStartDateTime f x =
    f (_ssfStartDateTime x)
        <&> \y -> x { _ssfStartDateTime = y }
{-# INLINE ssfStartDateTime #-}

-- | The date and time when the cluster step execution completed or failed.
ssfEndDateTime :: Lens' StepTimeline (Maybe POSIX)
ssfEndDateTime f x =
    f (_ssfEndDateTime x)
        <&> \y -> x { _ssfEndDateTime = y }
{-# INLINE ssfEndDateTime #-}

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
spcName f x =
    f (_spcName x)
        <&> \y -> x { _spcName = y }
{-# INLINE spcName #-}

-- | The list of user-supplied arguments.
spcArgs :: Lens' SupportedProductConfig ([Text])
spcArgs f x =
    f (_spcArgs x)
        <&> \y -> x { _spcArgs = y }
{-# INLINE spcArgs #-}

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
tKey f x =
    f (_tKey x)
        <&> \y -> x { _tKey = y }
{-# INLINE tKey #-}

-- | A user-defined value, which is optional in a tag. For more information, see
-- Tagging Amazon EMR Resources.
tValue :: Lens' Tag (Maybe Text)
tValue f x =
    f (_tValue x)
        <&> \y -> x { _tValue = y }
{-# INLINE tValue #-}

instance FromJSON Tag

instance ToJSON Tag

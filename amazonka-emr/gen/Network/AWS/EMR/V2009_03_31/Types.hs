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
badBootstrapActionConfig
    :: Functor f
    => (Maybe BootstrapActionConfig
    -> f (Maybe BootstrapActionConfig))
    -> BootstrapActionDetail
    -> f BootstrapActionDetail
badBootstrapActionConfig f x =
    (\y -> x { _badBootstrapActionConfig = y })
       <$> f (_badBootstrapActionConfig x)
{-# INLINE badBootstrapActionConfig #-}

instance FromJSON BootstrapActionDetail

instance ToJSON BootstrapActionDetail

-- | The Amazon EC2 Availability Zone for the job flow.
newtype PlacementType = PlacementType
    { _ptAvailabilityZone :: Text
      -- ^ The Amazon EC2 Availability Zone for the job flow.
    } deriving (Show, Generic)

-- | The Amazon EC2 Availability Zone for the job flow.
ptAvailabilityZone
    :: Functor f
    => (Text
    -> f (Text))
    -> PlacementType
    -> f PlacementType
ptAvailabilityZone f x =
    (\y -> x { _ptAvailabilityZone = y })
       <$> f (_ptAvailabilityZone x)
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
aName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Application
    -> f Application
aName f x =
    (\y -> x { _aName = y })
       <$> f (_aName x)
{-# INLINE aName #-}

-- | The version of the application.
aVersion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Application
    -> f Application
aVersion f x =
    (\y -> x { _aVersion = y })
       <$> f (_aVersion x)
{-# INLINE aVersion #-}

-- | Arguments for Amazon EMR to pass to the application.
aArgs
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> Application
    -> f Application
aArgs f x =
    (\y -> x { _aArgs = y })
       <$> f (_aArgs x)
{-# INLINE aArgs #-}

-- | This option is for advanced users only. This is meta information about
-- third-party applications that third-party vendors use for testing purposes.
aAdditionalInfo
    :: Functor f
    => (Map Text Text
    -> f (Map Text Text))
    -> Application
    -> f Application
aAdditionalInfo f x =
    (\y -> x { _aAdditionalInfo = y })
       <$> f (_aAdditionalInfo x)
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
bacName
    :: Functor f
    => (Text
    -> f (Text))
    -> BootstrapActionConfig
    -> f BootstrapActionConfig
bacName f x =
    (\y -> x { _bacName = y })
       <$> f (_bacName x)
{-# INLINE bacName #-}

-- | The script run by the bootstrap action.
bacScriptBootstrapAction
    :: Functor f
    => (ScriptBootstrapActionConfig
    -> f (ScriptBootstrapActionConfig))
    -> BootstrapActionConfig
    -> f BootstrapActionConfig
bacScriptBootstrapAction f x =
    (\y -> x { _bacScriptBootstrapAction = y })
       <$> f (_bacScriptBootstrapAction x)
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
cId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Cluster
    -> f Cluster
cId f x =
    (\y -> x { _cId = y })
       <$> f (_cId x)
{-# INLINE cId #-}

-- | The name of the cluster.
cName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Cluster
    -> f Cluster
cName f x =
    (\y -> x { _cName = y })
       <$> f (_cName x)
{-# INLINE cName #-}

-- | The current status details about the cluster.
cStatus
    :: Functor f
    => (Maybe ClusterStatus
    -> f (Maybe ClusterStatus))
    -> Cluster
    -> f Cluster
cStatus f x =
    (\y -> x { _cStatus = y })
       <$> f (_cStatus x)
{-# INLINE cStatus #-}

-- | Provides information about the EC2 instances in a cluster grouped by
-- category. For example, key name, subnet ID, IAM instance profile, and so
-- on.
cEc2InstanceAttributes
    :: Functor f
    => (Maybe Ec2InstanceAttributes
    -> f (Maybe Ec2InstanceAttributes))
    -> Cluster
    -> f Cluster
cEc2InstanceAttributes f x =
    (\y -> x { _cEc2InstanceAttributes = y })
       <$> f (_cEc2InstanceAttributes x)
{-# INLINE cEc2InstanceAttributes #-}

-- | The path to the Amazon S3 location where logs for this cluster are stored.
cLogUri
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Cluster
    -> f Cluster
cLogUri f x =
    (\y -> x { _cLogUri = y })
       <$> f (_cLogUri x)
{-# INLINE cLogUri #-}

-- | The AMI version requested for this cluster.JobFlowDetail$AmiVersion.-->.
cRequestedAmiVersion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Cluster
    -> f Cluster
cRequestedAmiVersion f x =
    (\y -> x { _cRequestedAmiVersion = y })
       <$> f (_cRequestedAmiVersion x)
{-# INLINE cRequestedAmiVersion #-}

-- | The AMI version running on this cluster. This differs from the requested
-- version only if the requested version is a meta version, such as "latest".
-- JobFlowDetail$AmiVersion.-->.
cRunningAmiVersion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Cluster
    -> f Cluster
cRunningAmiVersion f x =
    (\y -> x { _cRunningAmiVersion = y })
       <$> f (_cRunningAmiVersion x)
{-# INLINE cRunningAmiVersion #-}

-- | Specifies whether the cluster should terminate after completing all steps.
cAutoTerminate
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> Cluster
    -> f Cluster
cAutoTerminate f x =
    (\y -> x { _cAutoTerminate = y })
       <$> f (_cAutoTerminate x)
{-# INLINE cAutoTerminate #-}

-- | Indicates whether Amazon EMR will lock the cluster to prevent the EC2
-- instances from being terminated by an API call or user intervention, or in
-- the event of a cluster error.
cTerminationProtected
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> Cluster
    -> f Cluster
cTerminationProtected f x =
    (\y -> x { _cTerminationProtected = y })
       <$> f (_cTerminationProtected x)
{-# INLINE cTerminationProtected #-}

-- | Indicates whether the job flow is visible to all IAM users of the AWS
-- account associated with the job flow. If this value is set to true, all IAM
-- users of that AWS account can view and manage the job flow if they have the
-- proper policy permissions set. If this value is false, only the IAM user
-- that created the cluster can view and manage it. This value can be changed
-- using the SetVisibleToAllUsers action.
cVisibleToAllUsers
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> Cluster
    -> f Cluster
cVisibleToAllUsers f x =
    (\y -> x { _cVisibleToAllUsers = y })
       <$> f (_cVisibleToAllUsers x)
{-# INLINE cVisibleToAllUsers #-}

-- | The applications installed on this cluster.
cApplications
    :: Functor f
    => ([Application]
    -> f ([Application]))
    -> Cluster
    -> f Cluster
cApplications f x =
    (\y -> x { _cApplications = y })
       <$> f (_cApplications x)
{-# INLINE cApplications #-}

-- | A list of tags associated with a cluster.
cTags
    :: Functor f
    => ([Tag]
    -> f ([Tag]))
    -> Cluster
    -> f Cluster
cTags f x =
    (\y -> x { _cTags = y })
       <$> f (_cTags x)
{-# INLINE cTags #-}

-- | The IAM role that will be assumed by the Amazon EMR service to access AWS
-- resources on your behalf.
cServiceRole
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Cluster
    -> f Cluster
cServiceRole f x =
    (\y -> x { _cServiceRole = y })
       <$> f (_cServiceRole x)
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
cscrCode
    :: Functor f
    => (Maybe ClusterStateChangeReasonCode
    -> f (Maybe ClusterStateChangeReasonCode))
    -> ClusterStateChangeReason
    -> f ClusterStateChangeReason
cscrCode f x =
    (\y -> x { _cscrCode = y })
       <$> f (_cscrCode x)
{-# INLINE cscrCode #-}

-- | The descriptive message for the state change reason.
cscrMessage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ClusterStateChangeReason
    -> f ClusterStateChangeReason
cscrMessage f x =
    (\y -> x { _cscrMessage = y })
       <$> f (_cscrMessage x)
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
csState
    :: Functor f
    => (Maybe ClusterState
    -> f (Maybe ClusterState))
    -> ClusterStatus
    -> f ClusterStatus
csState f x =
    (\y -> x { _csState = y })
       <$> f (_csState x)
{-# INLINE csState #-}

-- | The reason for the cluster status change.
csStateChangeReason
    :: Functor f
    => (Maybe ClusterStateChangeReason
    -> f (Maybe ClusterStateChangeReason))
    -> ClusterStatus
    -> f ClusterStatus
csStateChangeReason f x =
    (\y -> x { _csStateChangeReason = y })
       <$> f (_csStateChangeReason x)
{-# INLINE csStateChangeReason #-}

-- | A timeline that represents the status of a cluster over the lifetime of the
-- cluster.
csTimeline
    :: Functor f
    => (Maybe ClusterTimeline
    -> f (Maybe ClusterTimeline))
    -> ClusterStatus
    -> f ClusterStatus
csTimeline f x =
    (\y -> x { _csTimeline = y })
       <$> f (_csTimeline x)
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
cwId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ClusterSummary
    -> f ClusterSummary
cwId f x =
    (\y -> x { _cwId = y })
       <$> f (_cwId x)
{-# INLINE cwId #-}

-- | The name of the cluster.
cwName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ClusterSummary
    -> f ClusterSummary
cwName f x =
    (\y -> x { _cwName = y })
       <$> f (_cwName x)
{-# INLINE cwName #-}

-- | The details about the current status of the cluster.
cwStatus
    :: Functor f
    => (Maybe ClusterStatus
    -> f (Maybe ClusterStatus))
    -> ClusterSummary
    -> f ClusterSummary
cwStatus f x =
    (\y -> x { _cwStatus = y })
       <$> f (_cwStatus x)
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
cuCreationDateTime
    :: Functor f
    => (Maybe POSIX
    -> f (Maybe POSIX))
    -> ClusterTimeline
    -> f ClusterTimeline
cuCreationDateTime f x =
    (\y -> x { _cuCreationDateTime = y })
       <$> f (_cuCreationDateTime x)
{-# INLINE cuCreationDateTime #-}

-- | The date and time when the cluster was ready to execute steps.
cuReadyDateTime
    :: Functor f
    => (Maybe POSIX
    -> f (Maybe POSIX))
    -> ClusterTimeline
    -> f ClusterTimeline
cuReadyDateTime f x =
    (\y -> x { _cuReadyDateTime = y })
       <$> f (_cuReadyDateTime x)
{-# INLINE cuReadyDateTime #-}

-- | The date and time when the cluster was terminated.
cuEndDateTime
    :: Functor f
    => (Maybe POSIX
    -> f (Maybe POSIX))
    -> ClusterTimeline
    -> f ClusterTimeline
cuEndDateTime f x =
    (\y -> x { _cuEndDateTime = y })
       <$> f (_cuEndDateTime x)
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
cdName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Command
    -> f Command
cdName f x =
    (\y -> x { _cdName = y })
       <$> f (_cdName x)
{-# INLINE cdName #-}

-- | The Amazon S3 location of the command script.
cdScriptPath
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Command
    -> f Command
cdScriptPath f x =
    (\y -> x { _cdScriptPath = y })
       <$> f (_cdScriptPath x)
{-# INLINE cdScriptPath #-}

-- | Arguments for Amazon EMR to pass to the command for execution.
cdArgs
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> Command
    -> f Command
cdArgs f x =
    (\y -> x { _cdArgs = y })
       <$> f (_cdArgs x)
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
eiaEc2KeyName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Ec2InstanceAttributes
    -> f Ec2InstanceAttributes
eiaEc2KeyName f x =
    (\y -> x { _eiaEc2KeyName = y })
       <$> f (_eiaEc2KeyName x)
{-# INLINE eiaEc2KeyName #-}

-- | To launch the job flow in Amazon VPC, set this parameter to the identifier
-- of the Amazon VPC subnet where you want the job flow to launch. If you do
-- not specify this value, the job flow is launched in the normal AWS cloud,
-- outside of a VPC. Amazon VPC currently does not support cluster compute
-- quadruple extra large (cc1.4xlarge) instances. Thus, you cannot specify the
-- cc1.4xlarge instance type for nodes of a job flow launched in a VPC.
eiaEc2SubnetId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Ec2InstanceAttributes
    -> f Ec2InstanceAttributes
eiaEc2SubnetId f x =
    (\y -> x { _eiaEc2SubnetId = y })
       <$> f (_eiaEc2SubnetId x)
{-# INLINE eiaEc2SubnetId #-}

-- | The Availability Zone in which the cluster will run.
eiaEc2AvailabilityZone
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Ec2InstanceAttributes
    -> f Ec2InstanceAttributes
eiaEc2AvailabilityZone f x =
    (\y -> x { _eiaEc2AvailabilityZone = y })
       <$> f (_eiaEc2AvailabilityZone x)
{-# INLINE eiaEc2AvailabilityZone #-}

-- | The IAM role that was specified when the job flow was launched. The EC2
-- instances of the job flow assume this role.
eiaIamInstanceProfile
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Ec2InstanceAttributes
    -> f Ec2InstanceAttributes
eiaIamInstanceProfile f x =
    (\y -> x { _eiaIamInstanceProfile = y })
       <$> f (_eiaIamInstanceProfile x)
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
hjscProperties
    :: Functor f
    => ([KeyValue]
    -> f ([KeyValue]))
    -> HadoopJarStepConfig
    -> f HadoopJarStepConfig
hjscProperties f x =
    (\y -> x { _hjscProperties = y })
       <$> f (_hjscProperties x)
{-# INLINE hjscProperties #-}

-- | A path to a JAR file run during the step.
hjscJar
    :: Functor f
    => (Text
    -> f (Text))
    -> HadoopJarStepConfig
    -> f HadoopJarStepConfig
hjscJar f x =
    (\y -> x { _hjscJar = y })
       <$> f (_hjscJar x)
{-# INLINE hjscJar #-}

-- | The name of the main class in the specified Java file. If not specified,
-- the JAR file should specify a Main-Class in its manifest file.
hjscMainClass
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> HadoopJarStepConfig
    -> f HadoopJarStepConfig
hjscMainClass f x =
    (\y -> x { _hjscMainClass = y })
       <$> f (_hjscMainClass x)
{-# INLINE hjscMainClass #-}

-- | A list of command line arguments passed to the JAR file's main function
-- when executed.
hjscArgs
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> HadoopJarStepConfig
    -> f HadoopJarStepConfig
hjscArgs f x =
    (\y -> x { _hjscArgs = y })
       <$> f (_hjscArgs x)
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
hscJar
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> HadoopStepConfig
    -> f HadoopStepConfig
hscJar f x =
    (\y -> x { _hscJar = y })
       <$> f (_hscJar x)
{-# INLINE hscJar #-}

-- | The list of Java properties that are set when the step runs. You can use
-- these properties to pass key value pairs to your main function.
hscProperties
    :: Functor f
    => (Map Text Text
    -> f (Map Text Text))
    -> HadoopStepConfig
    -> f HadoopStepConfig
hscProperties f x =
    (\y -> x { _hscProperties = y })
       <$> f (_hscProperties x)
{-# INLINE hscProperties #-}

-- | The name of the main class in the specified Java file. If not specified,
-- the JAR file should specify a main class in its manifest file.
hscMainClass
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> HadoopStepConfig
    -> f HadoopStepConfig
hscMainClass f x =
    (\y -> x { _hscMainClass = y })
       <$> f (_hscMainClass x)
{-# INLINE hscMainClass #-}

-- | The list of command line arguments to pass to the JAR file's main function
-- for execution.
hscArgs
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> HadoopStepConfig
    -> f HadoopStepConfig
hscArgs f x =
    (\y -> x { _hscArgs = y })
       <$> f (_hscArgs x)
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
ieId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
ieId f x =
    (\y -> x { _ieId = y })
       <$> f (_ieId x)
{-# INLINE ieId #-}

-- | The unique identifier of the instance in Amazon EC2.
ieEc2InstanceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
ieEc2InstanceId f x =
    (\y -> x { _ieEc2InstanceId = y })
       <$> f (_ieEc2InstanceId x)
{-# INLINE ieEc2InstanceId #-}

-- | The public DNS name of the instance.
iePublicDnsName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
iePublicDnsName f x =
    (\y -> x { _iePublicDnsName = y })
       <$> f (_iePublicDnsName x)
{-# INLINE iePublicDnsName #-}

-- | The public IP address of the instance.
iePublicIpAddress
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
iePublicIpAddress f x =
    (\y -> x { _iePublicIpAddress = y })
       <$> f (_iePublicIpAddress x)
{-# INLINE iePublicIpAddress #-}

-- | The private DNS name of the instance.
iePrivateDnsName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
iePrivateDnsName f x =
    (\y -> x { _iePrivateDnsName = y })
       <$> f (_iePrivateDnsName x)
{-# INLINE iePrivateDnsName #-}

-- | The private IP address of the instance.
iePrivateIpAddress
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
iePrivateIpAddress f x =
    (\y -> x { _iePrivateIpAddress = y })
       <$> f (_iePrivateIpAddress x)
{-# INLINE iePrivateIpAddress #-}

-- | The current status of the instance.
ieStatus
    :: Functor f
    => (Maybe InstanceStatus
    -> f (Maybe InstanceStatus))
    -> Instance
    -> f Instance
ieStatus f x =
    (\y -> x { _ieStatus = y })
       <$> f (_ieStatus x)
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
igId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstanceGroup
    -> f InstanceGroup
igId f x =
    (\y -> x { _igId = y })
       <$> f (_igId x)
{-# INLINE igId #-}

-- | The name of the instance group.
igName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstanceGroup
    -> f InstanceGroup
igName f x =
    (\y -> x { _igName = y })
       <$> f (_igName x)
{-# INLINE igName #-}

-- | The marketplace to provision instances for this group. Valid values are
-- ON_DEMAND or SPOT.
igMarket
    :: Functor f
    => (Maybe MarketType
    -> f (Maybe MarketType))
    -> InstanceGroup
    -> f InstanceGroup
igMarket f x =
    (\y -> x { _igMarket = y })
       <$> f (_igMarket x)
{-# INLINE igMarket #-}

-- | The type of the instance group. Valid values are MASTER, CORE or TASK.
igInstanceGroupType
    :: Functor f
    => (Maybe InstanceGroupType
    -> f (Maybe InstanceGroupType))
    -> InstanceGroup
    -> f InstanceGroup
igInstanceGroupType f x =
    (\y -> x { _igInstanceGroupType = y })
       <$> f (_igInstanceGroupType x)
{-# INLINE igInstanceGroupType #-}

-- | The bid price for each EC2 instance in the instance group when launching
-- nodes as Spot Instances, expressed in USD.
igBidPrice
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstanceGroup
    -> f InstanceGroup
igBidPrice f x =
    (\y -> x { _igBidPrice = y })
       <$> f (_igBidPrice x)
{-# INLINE igBidPrice #-}

-- | The EC2 instance type for all instances in the instance group.
igInstanceType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstanceGroup
    -> f InstanceGroup
igInstanceType f x =
    (\y -> x { _igInstanceType = y })
       <$> f (_igInstanceType x)
{-# INLINE igInstanceType #-}

-- | The target number of instances for the instance group.
igRequestedInstanceCount
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> InstanceGroup
    -> f InstanceGroup
igRequestedInstanceCount f x =
    (\y -> x { _igRequestedInstanceCount = y })
       <$> f (_igRequestedInstanceCount x)
{-# INLINE igRequestedInstanceCount #-}

-- | The number of instances currently running in this instance group.
igRunningInstanceCount
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> InstanceGroup
    -> f InstanceGroup
igRunningInstanceCount f x =
    (\y -> x { _igRunningInstanceCount = y })
       <$> f (_igRunningInstanceCount x)
{-# INLINE igRunningInstanceCount #-}

-- | The current status of the instance group.
igStatus
    :: Functor f
    => (Maybe InstanceGroupStatus
    -> f (Maybe InstanceGroupStatus))
    -> InstanceGroup
    -> f InstanceGroup
igStatus f x =
    (\y -> x { _igStatus = y })
       <$> f (_igStatus x)
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
igcName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstanceGroupConfig
    -> f InstanceGroupConfig
igcName f x =
    (\y -> x { _igcName = y })
       <$> f (_igcName x)
{-# INLINE igcName #-}

-- | Market type of the Amazon EC2 instances used to create a cluster node.
igcMarket
    :: Functor f
    => (Maybe MarketType
    -> f (Maybe MarketType))
    -> InstanceGroupConfig
    -> f InstanceGroupConfig
igcMarket f x =
    (\y -> x { _igcMarket = y })
       <$> f (_igcMarket x)
{-# INLINE igcMarket #-}

-- | The role of the instance group in the cluster.
igcInstanceRole
    :: Functor f
    => (InstanceRoleType
    -> f (InstanceRoleType))
    -> InstanceGroupConfig
    -> f InstanceGroupConfig
igcInstanceRole f x =
    (\y -> x { _igcInstanceRole = y })
       <$> f (_igcInstanceRole x)
{-# INLINE igcInstanceRole #-}

-- | Bid price for each Amazon EC2 instance in the instance group when launching
-- nodes as Spot Instances, expressed in USD.
igcBidPrice
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstanceGroupConfig
    -> f InstanceGroupConfig
igcBidPrice f x =
    (\y -> x { _igcBidPrice = y })
       <$> f (_igcBidPrice x)
{-# INLINE igcBidPrice #-}

-- | The Amazon EC2 instance type for all instances in the instance group.
igcInstanceType
    :: Functor f
    => (Text
    -> f (Text))
    -> InstanceGroupConfig
    -> f InstanceGroupConfig
igcInstanceType f x =
    (\y -> x { _igcInstanceType = y })
       <$> f (_igcInstanceType x)
{-# INLINE igcInstanceType #-}

-- | Target number of instances for the instance group.
igcInstanceCount
    :: Functor f
    => (Integer
    -> f (Integer))
    -> InstanceGroupConfig
    -> f InstanceGroupConfig
igcInstanceCount f x =
    (\y -> x { _igcInstanceCount = y })
       <$> f (_igcInstanceCount x)
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
igdInstanceGroupId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstanceGroupDetail
    -> f InstanceGroupDetail
igdInstanceGroupId f x =
    (\y -> x { _igdInstanceGroupId = y })
       <$> f (_igdInstanceGroupId x)
{-# INLINE igdInstanceGroupId #-}

-- | Friendly name for the instance group.
igdName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstanceGroupDetail
    -> f InstanceGroupDetail
igdName f x =
    (\y -> x { _igdName = y })
       <$> f (_igdName x)
{-# INLINE igdName #-}

-- | Market type of the Amazon EC2 instances used to create a cluster node.
igdMarket
    :: Functor f
    => (MarketType
    -> f (MarketType))
    -> InstanceGroupDetail
    -> f InstanceGroupDetail
igdMarket f x =
    (\y -> x { _igdMarket = y })
       <$> f (_igdMarket x)
{-# INLINE igdMarket #-}

-- | Instance group role in the cluster.
igdInstanceRole
    :: Functor f
    => (InstanceRoleType
    -> f (InstanceRoleType))
    -> InstanceGroupDetail
    -> f InstanceGroupDetail
igdInstanceRole f x =
    (\y -> x { _igdInstanceRole = y })
       <$> f (_igdInstanceRole x)
{-# INLINE igdInstanceRole #-}

-- | Bid price for EC2 Instances when launching nodes as Spot Instances,
-- expressed in USD.
igdBidPrice
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstanceGroupDetail
    -> f InstanceGroupDetail
igdBidPrice f x =
    (\y -> x { _igdBidPrice = y })
       <$> f (_igdBidPrice x)
{-# INLINE igdBidPrice #-}

-- | Amazon EC2 Instance type.
igdInstanceType
    :: Functor f
    => (Text
    -> f (Text))
    -> InstanceGroupDetail
    -> f InstanceGroupDetail
igdInstanceType f x =
    (\y -> x { _igdInstanceType = y })
       <$> f (_igdInstanceType x)
{-# INLINE igdInstanceType #-}

-- | Target number of instances to run in the instance group.
igdInstanceRequestCount
    :: Functor f
    => (Integer
    -> f (Integer))
    -> InstanceGroupDetail
    -> f InstanceGroupDetail
igdInstanceRequestCount f x =
    (\y -> x { _igdInstanceRequestCount = y })
       <$> f (_igdInstanceRequestCount x)
{-# INLINE igdInstanceRequestCount #-}

-- | Actual count of running instances.
igdInstanceRunningCount
    :: Functor f
    => (Integer
    -> f (Integer))
    -> InstanceGroupDetail
    -> f InstanceGroupDetail
igdInstanceRunningCount f x =
    (\y -> x { _igdInstanceRunningCount = y })
       <$> f (_igdInstanceRunningCount x)
{-# INLINE igdInstanceRunningCount #-}

-- | State of instance group. The following values are deprecated: STARTING,
-- TERMINATED, and FAILED.
igdState
    :: Functor f
    => (InstanceGroupState
    -> f (InstanceGroupState))
    -> InstanceGroupDetail
    -> f InstanceGroupDetail
igdState f x =
    (\y -> x { _igdState = y })
       <$> f (_igdState x)
{-# INLINE igdState #-}

-- | Details regarding the state of the instance group.
igdLastStateChangeReason
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstanceGroupDetail
    -> f InstanceGroupDetail
igdLastStateChangeReason f x =
    (\y -> x { _igdLastStateChangeReason = y })
       <$> f (_igdLastStateChangeReason x)
{-# INLINE igdLastStateChangeReason #-}

-- | The date/time the instance group was created.
igdCreationDateTime
    :: Functor f
    => (POSIX
    -> f (POSIX))
    -> InstanceGroupDetail
    -> f InstanceGroupDetail
igdCreationDateTime f x =
    (\y -> x { _igdCreationDateTime = y })
       <$> f (_igdCreationDateTime x)
{-# INLINE igdCreationDateTime #-}

-- | The date/time the instance group was started.
igdStartDateTime
    :: Functor f
    => (Maybe POSIX
    -> f (Maybe POSIX))
    -> InstanceGroupDetail
    -> f InstanceGroupDetail
igdStartDateTime f x =
    (\y -> x { _igdStartDateTime = y })
       <$> f (_igdStartDateTime x)
{-# INLINE igdStartDateTime #-}

-- | The date/time the instance group was available to the cluster.
igdReadyDateTime
    :: Functor f
    => (Maybe POSIX
    -> f (Maybe POSIX))
    -> InstanceGroupDetail
    -> f InstanceGroupDetail
igdReadyDateTime f x =
    (\y -> x { _igdReadyDateTime = y })
       <$> f (_igdReadyDateTime x)
{-# INLINE igdReadyDateTime #-}

-- | The date/time the instance group was terminated.
igdEndDateTime
    :: Functor f
    => (Maybe POSIX
    -> f (Maybe POSIX))
    -> InstanceGroupDetail
    -> f InstanceGroupDetail
igdEndDateTime f x =
    (\y -> x { _igdEndDateTime = y })
       <$> f (_igdEndDateTime x)
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
igmcInstanceGroupId
    :: Functor f
    => (Text
    -> f (Text))
    -> InstanceGroupModifyConfig
    -> f InstanceGroupModifyConfig
igmcInstanceGroupId f x =
    (\y -> x { _igmcInstanceGroupId = y })
       <$> f (_igmcInstanceGroupId x)
{-# INLINE igmcInstanceGroupId #-}

-- | Target size for the instance group.
igmcInstanceCount
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> InstanceGroupModifyConfig
    -> f InstanceGroupModifyConfig
igmcInstanceCount f x =
    (\y -> x { _igmcInstanceCount = y })
       <$> f (_igmcInstanceCount x)
{-# INLINE igmcInstanceCount #-}

-- | The EC2 InstanceIds to terminate. For advanced users only. Once you
-- terminate the instances, the instance group will not return to its original
-- requested size.
igmcEC2InstanceIdsToTerminate
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> InstanceGroupModifyConfig
    -> f InstanceGroupModifyConfig
igmcEC2InstanceIdsToTerminate f x =
    (\y -> x { _igmcEC2InstanceIdsToTerminate = y })
       <$> f (_igmcEC2InstanceIdsToTerminate x)
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
igscrCode
    :: Functor f
    => (Maybe InstanceGroupStateChangeReasonCode
    -> f (Maybe InstanceGroupStateChangeReasonCode))
    -> InstanceGroupStateChangeReason
    -> f InstanceGroupStateChangeReason
igscrCode f x =
    (\y -> x { _igscrCode = y })
       <$> f (_igscrCode x)
{-# INLINE igscrCode #-}

-- | The status change reason description.
igscrMessage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstanceGroupStateChangeReason
    -> f InstanceGroupStateChangeReason
igscrMessage f x =
    (\y -> x { _igscrMessage = y })
       <$> f (_igscrMessage x)
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
iguState
    :: Functor f
    => (Maybe InstanceGroupState
    -> f (Maybe InstanceGroupState))
    -> InstanceGroupStatus
    -> f InstanceGroupStatus
iguState f x =
    (\y -> x { _iguState = y })
       <$> f (_iguState x)
{-# INLINE iguState #-}

-- | The status change reason details for the instance group.
iguStateChangeReason
    :: Functor f
    => (Maybe InstanceGroupStateChangeReason
    -> f (Maybe InstanceGroupStateChangeReason))
    -> InstanceGroupStatus
    -> f InstanceGroupStatus
iguStateChangeReason f x =
    (\y -> x { _iguStateChangeReason = y })
       <$> f (_iguStateChangeReason x)
{-# INLINE iguStateChangeReason #-}

-- | The timeline of the instance group status over time.
iguTimeline
    :: Functor f
    => (Maybe InstanceGroupTimeline
    -> f (Maybe InstanceGroupTimeline))
    -> InstanceGroupStatus
    -> f InstanceGroupStatus
iguTimeline f x =
    (\y -> x { _iguTimeline = y })
       <$> f (_iguTimeline x)
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
igwCreationDateTime
    :: Functor f
    => (Maybe POSIX
    -> f (Maybe POSIX))
    -> InstanceGroupTimeline
    -> f InstanceGroupTimeline
igwCreationDateTime f x =
    (\y -> x { _igwCreationDateTime = y })
       <$> f (_igwCreationDateTime x)
{-# INLINE igwCreationDateTime #-}

-- | The date and time when the instance group became ready to perform tasks.
igwReadyDateTime
    :: Functor f
    => (Maybe POSIX
    -> f (Maybe POSIX))
    -> InstanceGroupTimeline
    -> f InstanceGroupTimeline
igwReadyDateTime f x =
    (\y -> x { _igwReadyDateTime = y })
       <$> f (_igwReadyDateTime x)
{-# INLINE igwReadyDateTime #-}

-- | The date and time when the instance group terminated.
igwEndDateTime
    :: Functor f
    => (Maybe POSIX
    -> f (Maybe POSIX))
    -> InstanceGroupTimeline
    -> f InstanceGroupTimeline
igwEndDateTime f x =
    (\y -> x { _igwEndDateTime = y })
       <$> f (_igwEndDateTime x)
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
iscrCode
    :: Functor f
    => (Maybe InstanceStateChangeReasonCode
    -> f (Maybe InstanceStateChangeReasonCode))
    -> InstanceStateChangeReason
    -> f InstanceStateChangeReason
iscrCode f x =
    (\y -> x { _iscrCode = y })
       <$> f (_iscrCode x)
{-# INLINE iscrCode #-}

-- | The status change reason description.
iscrMessage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstanceStateChangeReason
    -> f InstanceStateChangeReason
iscrMessage f x =
    (\y -> x { _iscrMessage = y })
       <$> f (_iscrMessage x)
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
izState
    :: Functor f
    => (Maybe InstanceState
    -> f (Maybe InstanceState))
    -> InstanceStatus
    -> f InstanceStatus
izState f x =
    (\y -> x { _izState = y })
       <$> f (_izState x)
{-# INLINE izState #-}

-- | The details of the status change reason for the instance.
izStateChangeReason
    :: Functor f
    => (Maybe InstanceStateChangeReason
    -> f (Maybe InstanceStateChangeReason))
    -> InstanceStatus
    -> f InstanceStatus
izStateChangeReason f x =
    (\y -> x { _izStateChangeReason = y })
       <$> f (_izStateChangeReason x)
{-# INLINE izStateChangeReason #-}

-- | The timeline of the instance status over time.
izTimeline
    :: Functor f
    => (Maybe InstanceTimeline
    -> f (Maybe InstanceTimeline))
    -> InstanceStatus
    -> f InstanceStatus
izTimeline f x =
    (\y -> x { _izTimeline = y })
       <$> f (_izTimeline x)
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
iifCreationDateTime
    :: Functor f
    => (Maybe POSIX
    -> f (Maybe POSIX))
    -> InstanceTimeline
    -> f InstanceTimeline
iifCreationDateTime f x =
    (\y -> x { _iifCreationDateTime = y })
       <$> f (_iifCreationDateTime x)
{-# INLINE iifCreationDateTime #-}

-- | The date and time when the instance was ready to perform tasks.
iifReadyDateTime
    :: Functor f
    => (Maybe POSIX
    -> f (Maybe POSIX))
    -> InstanceTimeline
    -> f InstanceTimeline
iifReadyDateTime f x =
    (\y -> x { _iifReadyDateTime = y })
       <$> f (_iifReadyDateTime x)
{-# INLINE iifReadyDateTime #-}

-- | The date and time when the instance was terminated.
iifEndDateTime
    :: Functor f
    => (Maybe POSIX
    -> f (Maybe POSIX))
    -> InstanceTimeline
    -> f InstanceTimeline
iifEndDateTime f x =
    (\y -> x { _iifEndDateTime = y })
       <$> f (_iifEndDateTime x)
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
jfdJobFlowId
    :: Functor f
    => (Text
    -> f (Text))
    -> JobFlowDetail
    -> f JobFlowDetail
jfdJobFlowId f x =
    (\y -> x { _jfdJobFlowId = y })
       <$> f (_jfdJobFlowId x)
{-# INLINE jfdJobFlowId #-}

-- | The name of the job flow.
jfdName
    :: Functor f
    => (Text
    -> f (Text))
    -> JobFlowDetail
    -> f JobFlowDetail
jfdName f x =
    (\y -> x { _jfdName = y })
       <$> f (_jfdName x)
{-# INLINE jfdName #-}

-- | The location in Amazon S3 where log files for the job are stored.
jfdLogUri
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> JobFlowDetail
    -> f JobFlowDetail
jfdLogUri f x =
    (\y -> x { _jfdLogUri = y })
       <$> f (_jfdLogUri x)
{-# INLINE jfdLogUri #-}

-- | The version of the AMI used to initialize Amazon EC2 instances in the job
-- flow. For a list of AMI versions currently supported by Amazon
-- ElasticMapReduce, go to AMI Versions Supported in Elastic MapReduce in the
-- Amazon Elastic MapReduce Developer's Guide.
jfdAmiVersion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> JobFlowDetail
    -> f JobFlowDetail
jfdAmiVersion f x =
    (\y -> x { _jfdAmiVersion = y })
       <$> f (_jfdAmiVersion x)
{-# INLINE jfdAmiVersion #-}

-- | Describes the execution status of the job flow.
jfdExecutionStatusDetail
    :: Functor f
    => (JobFlowExecutionStatusDetail
    -> f (JobFlowExecutionStatusDetail))
    -> JobFlowDetail
    -> f JobFlowDetail
jfdExecutionStatusDetail f x =
    (\y -> x { _jfdExecutionStatusDetail = y })
       <$> f (_jfdExecutionStatusDetail x)
{-# INLINE jfdExecutionStatusDetail #-}

-- | Describes the Amazon EC2 instances of the job flow.
jfdInstances
    :: Functor f
    => (JobFlowInstancesDetail
    -> f (JobFlowInstancesDetail))
    -> JobFlowDetail
    -> f JobFlowDetail
jfdInstances f x =
    (\y -> x { _jfdInstances = y })
       <$> f (_jfdInstances x)
{-# INLINE jfdInstances #-}

-- | A list of steps run by the job flow.
jfdSteps
    :: Functor f
    => ([StepDetail]
    -> f ([StepDetail]))
    -> JobFlowDetail
    -> f JobFlowDetail
jfdSteps f x =
    (\y -> x { _jfdSteps = y })
       <$> f (_jfdSteps x)
{-# INLINE jfdSteps #-}

-- | A list of the bootstrap actions run by the job flow.
jfdBootstrapActions
    :: Functor f
    => ([BootstrapActionDetail]
    -> f ([BootstrapActionDetail]))
    -> JobFlowDetail
    -> f JobFlowDetail
jfdBootstrapActions f x =
    (\y -> x { _jfdBootstrapActions = y })
       <$> f (_jfdBootstrapActions x)
{-# INLINE jfdBootstrapActions #-}

-- | A list of strings set by third party software when the job flow is
-- launched. If you are not using third party software to manage the job flow
-- this value is empty.
jfdSupportedProducts
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> JobFlowDetail
    -> f JobFlowDetail
jfdSupportedProducts f x =
    (\y -> x { _jfdSupportedProducts = y })
       <$> f (_jfdSupportedProducts x)
{-# INLINE jfdSupportedProducts #-}

-- | Specifies whether the job flow is visible to all IAM users of the AWS
-- account associated with the job flow. If this value is set to true, all IAM
-- users of that AWS account can view and (if they have the proper policy
-- permissions set) manage the job flow. If it is set to false, only the IAM
-- user that created the job flow can view and manage it. This value can be
-- changed using the SetVisibleToAllUsers action.
jfdVisibleToAllUsers
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> JobFlowDetail
    -> f JobFlowDetail
jfdVisibleToAllUsers f x =
    (\y -> x { _jfdVisibleToAllUsers = y })
       <$> f (_jfdVisibleToAllUsers x)
{-# INLINE jfdVisibleToAllUsers #-}

-- | The IAM role that was specified when the job flow was launched. The EC2
-- instances of the job flow assume this role.
jfdJobFlowRole
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> JobFlowDetail
    -> f JobFlowDetail
jfdJobFlowRole f x =
    (\y -> x { _jfdJobFlowRole = y })
       <$> f (_jfdJobFlowRole x)
{-# INLINE jfdJobFlowRole #-}

-- | The IAM role that will be assumed by the Amazon EMR service to access AWS
-- resources on your behalf.
jfdServiceRole
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> JobFlowDetail
    -> f JobFlowDetail
jfdServiceRole f x =
    (\y -> x { _jfdServiceRole = y })
       <$> f (_jfdServiceRole x)
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
jfesdState
    :: Functor f
    => (JobFlowExecutionState
    -> f (JobFlowExecutionState))
    -> JobFlowExecutionStatusDetail
    -> f JobFlowExecutionStatusDetail
jfesdState f x =
    (\y -> x { _jfesdState = y })
       <$> f (_jfesdState x)
{-# INLINE jfesdState #-}

-- | The creation date and time of the job flow.
jfesdCreationDateTime
    :: Functor f
    => (POSIX
    -> f (POSIX))
    -> JobFlowExecutionStatusDetail
    -> f JobFlowExecutionStatusDetail
jfesdCreationDateTime f x =
    (\y -> x { _jfesdCreationDateTime = y })
       <$> f (_jfesdCreationDateTime x)
{-# INLINE jfesdCreationDateTime #-}

-- | The start date and time of the job flow.
jfesdStartDateTime
    :: Functor f
    => (Maybe POSIX
    -> f (Maybe POSIX))
    -> JobFlowExecutionStatusDetail
    -> f JobFlowExecutionStatusDetail
jfesdStartDateTime f x =
    (\y -> x { _jfesdStartDateTime = y })
       <$> f (_jfesdStartDateTime x)
{-# INLINE jfesdStartDateTime #-}

-- | The date and time when the job flow was ready to start running bootstrap
-- actions.
jfesdReadyDateTime
    :: Functor f
    => (Maybe POSIX
    -> f (Maybe POSIX))
    -> JobFlowExecutionStatusDetail
    -> f JobFlowExecutionStatusDetail
jfesdReadyDateTime f x =
    (\y -> x { _jfesdReadyDateTime = y })
       <$> f (_jfesdReadyDateTime x)
{-# INLINE jfesdReadyDateTime #-}

-- | The completion date and time of the job flow.
jfesdEndDateTime
    :: Functor f
    => (Maybe POSIX
    -> f (Maybe POSIX))
    -> JobFlowExecutionStatusDetail
    -> f JobFlowExecutionStatusDetail
jfesdEndDateTime f x =
    (\y -> x { _jfesdEndDateTime = y })
       <$> f (_jfesdEndDateTime x)
{-# INLINE jfesdEndDateTime #-}

-- | Description of the job flow last changed state.
jfesdLastStateChangeReason
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> JobFlowExecutionStatusDetail
    -> f JobFlowExecutionStatusDetail
jfesdLastStateChangeReason f x =
    (\y -> x { _jfesdLastStateChangeReason = y })
       <$> f (_jfesdLastStateChangeReason x)
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
jficMasterInstanceType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> JobFlowInstancesConfig
    -> f JobFlowInstancesConfig
jficMasterInstanceType f x =
    (\y -> x { _jficMasterInstanceType = y })
       <$> f (_jficMasterInstanceType x)
{-# INLINE jficMasterInstanceType #-}

-- | The EC2 instance type of the slave nodes.
jficSlaveInstanceType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> JobFlowInstancesConfig
    -> f JobFlowInstancesConfig
jficSlaveInstanceType f x =
    (\y -> x { _jficSlaveInstanceType = y })
       <$> f (_jficSlaveInstanceType x)
{-# INLINE jficSlaveInstanceType #-}

-- | The number of Amazon EC2 instances used to execute the job flow.
jficInstanceCount
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> JobFlowInstancesConfig
    -> f JobFlowInstancesConfig
jficInstanceCount f x =
    (\y -> x { _jficInstanceCount = y })
       <$> f (_jficInstanceCount x)
{-# INLINE jficInstanceCount #-}

-- | Configuration for the job flow's instance groups.
jficInstanceGroups
    :: Functor f
    => ([InstanceGroupConfig]
    -> f ([InstanceGroupConfig]))
    -> JobFlowInstancesConfig
    -> f JobFlowInstancesConfig
jficInstanceGroups f x =
    (\y -> x { _jficInstanceGroups = y })
       <$> f (_jficInstanceGroups x)
{-# INLINE jficInstanceGroups #-}

-- | The name of the Amazon EC2 key pair that can be used to ssh to the master
-- node as the user called "hadoop.".
jficEc2KeyName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> JobFlowInstancesConfig
    -> f JobFlowInstancesConfig
jficEc2KeyName f x =
    (\y -> x { _jficEc2KeyName = y })
       <$> f (_jficEc2KeyName x)
{-# INLINE jficEc2KeyName #-}

-- | The Availability Zone the job flow will run in.
jficPlacement
    :: Functor f
    => (Maybe PlacementType
    -> f (Maybe PlacementType))
    -> JobFlowInstancesConfig
    -> f JobFlowInstancesConfig
jficPlacement f x =
    (\y -> x { _jficPlacement = y })
       <$> f (_jficPlacement x)
{-# INLINE jficPlacement #-}

-- | Specifies whether the job flow should terminate after completing all steps.
jficKeepJobFlowAliveWhenNoSteps
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> JobFlowInstancesConfig
    -> f JobFlowInstancesConfig
jficKeepJobFlowAliveWhenNoSteps f x =
    (\y -> x { _jficKeepJobFlowAliveWhenNoSteps = y })
       <$> f (_jficKeepJobFlowAliveWhenNoSteps x)
{-# INLINE jficKeepJobFlowAliveWhenNoSteps #-}

-- | Specifies whether to lock the job flow to prevent the Amazon EC2 instances
-- from being terminated by API call, user intervention, or in the event of a
-- job flow error.
jficTerminationProtected
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> JobFlowInstancesConfig
    -> f JobFlowInstancesConfig
jficTerminationProtected f x =
    (\y -> x { _jficTerminationProtected = y })
       <$> f (_jficTerminationProtected x)
{-# INLINE jficTerminationProtected #-}

-- | The Hadoop version for the job flow. Valid inputs are "0.18", "0.20", or
-- "0.20.205". If you do not set this value, the default of 0.18 is used,
-- unless the AmiVersion parameter is set in the RunJobFlow call, in which
-- case the default version of Hadoop for that AMI version is used.
jficHadoopVersion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> JobFlowInstancesConfig
    -> f JobFlowInstancesConfig
jficHadoopVersion f x =
    (\y -> x { _jficHadoopVersion = y })
       <$> f (_jficHadoopVersion x)
{-# INLINE jficHadoopVersion #-}

-- | To launch the job flow in Amazon Virtual Private Cloud (Amazon VPC), set
-- this parameter to the identifier of the Amazon VPC subnet where you want
-- the job flow to launch. If you do not specify this value, the job flow is
-- launched in the normal Amazon Web Services cloud, outside of an Amazon VPC.
-- Amazon VPC currently does not support cluster compute quadruple extra large
-- (cc1.4xlarge) instances. Thus you cannot specify the cc1.4xlarge instance
-- type for nodes of a job flow launched in a Amazon VPC.
jficEc2SubnetId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> JobFlowInstancesConfig
    -> f JobFlowInstancesConfig
jficEc2SubnetId f x =
    (\y -> x { _jficEc2SubnetId = y })
       <$> f (_jficEc2SubnetId x)
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
jfidMasterInstanceType
    :: Functor f
    => (Text
    -> f (Text))
    -> JobFlowInstancesDetail
    -> f JobFlowInstancesDetail
jfidMasterInstanceType f x =
    (\y -> x { _jfidMasterInstanceType = y })
       <$> f (_jfidMasterInstanceType x)
{-# INLINE jfidMasterInstanceType #-}

-- | The DNS name of the master node.
jfidMasterPublicDnsName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> JobFlowInstancesDetail
    -> f JobFlowInstancesDetail
jfidMasterPublicDnsName f x =
    (\y -> x { _jfidMasterPublicDnsName = y })
       <$> f (_jfidMasterPublicDnsName x)
{-# INLINE jfidMasterPublicDnsName #-}

-- | The Amazon EC2 instance identifier of the master node.
jfidMasterInstanceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> JobFlowInstancesDetail
    -> f JobFlowInstancesDetail
jfidMasterInstanceId f x =
    (\y -> x { _jfidMasterInstanceId = y })
       <$> f (_jfidMasterInstanceId x)
{-# INLINE jfidMasterInstanceId #-}

-- | The Amazon EC2 slave node instance type.
jfidSlaveInstanceType
    :: Functor f
    => (Text
    -> f (Text))
    -> JobFlowInstancesDetail
    -> f JobFlowInstancesDetail
jfidSlaveInstanceType f x =
    (\y -> x { _jfidSlaveInstanceType = y })
       <$> f (_jfidSlaveInstanceType x)
{-# INLINE jfidSlaveInstanceType #-}

-- | The number of Amazon EC2 instances in the cluster. If the value is 1, the
-- same instance serves as both the master and slave node. If the value is
-- greater than 1, one instance is the master node and all others are slave
-- nodes.
jfidInstanceCount
    :: Functor f
    => (Integer
    -> f (Integer))
    -> JobFlowInstancesDetail
    -> f JobFlowInstancesDetail
jfidInstanceCount f x =
    (\y -> x { _jfidInstanceCount = y })
       <$> f (_jfidInstanceCount x)
{-# INLINE jfidInstanceCount #-}

-- | Details about the job flow's instance groups.
jfidInstanceGroups
    :: Functor f
    => ([InstanceGroupDetail]
    -> f ([InstanceGroupDetail]))
    -> JobFlowInstancesDetail
    -> f JobFlowInstancesDetail
jfidInstanceGroups f x =
    (\y -> x { _jfidInstanceGroups = y })
       <$> f (_jfidInstanceGroups x)
{-# INLINE jfidInstanceGroups #-}

-- | An approximation of the cost of the job flow, represented in
-- m1.small/hours. This value is incremented once for every hour an m1.small
-- runs. Larger instances are weighted more, so an Amazon EC2 instance that is
-- roughly four times more expensive would result in the normalized instance
-- hours being incremented by four. This result is only an approximation and
-- does not reflect the actual billing rate.
jfidNormalizedInstanceHours
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> JobFlowInstancesDetail
    -> f JobFlowInstancesDetail
jfidNormalizedInstanceHours f x =
    (\y -> x { _jfidNormalizedInstanceHours = y })
       <$> f (_jfidNormalizedInstanceHours x)
{-# INLINE jfidNormalizedInstanceHours #-}

-- | The name of an Amazon EC2 key pair that can be used to ssh to the master
-- node of job flow.
jfidEc2KeyName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> JobFlowInstancesDetail
    -> f JobFlowInstancesDetail
jfidEc2KeyName f x =
    (\y -> x { _jfidEc2KeyName = y })
       <$> f (_jfidEc2KeyName x)
{-# INLINE jfidEc2KeyName #-}

-- | For job flows launched within Amazon Virtual Private Cloud, this value
-- specifies the identifier of the subnet where the job flow was launched.
jfidEc2SubnetId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> JobFlowInstancesDetail
    -> f JobFlowInstancesDetail
jfidEc2SubnetId f x =
    (\y -> x { _jfidEc2SubnetId = y })
       <$> f (_jfidEc2SubnetId x)
{-# INLINE jfidEc2SubnetId #-}

-- | The Amazon EC2 Availability Zone for the job flow.
jfidPlacement
    :: Functor f
    => (Maybe PlacementType
    -> f (Maybe PlacementType))
    -> JobFlowInstancesDetail
    -> f JobFlowInstancesDetail
jfidPlacement f x =
    (\y -> x { _jfidPlacement = y })
       <$> f (_jfidPlacement x)
{-# INLINE jfidPlacement #-}

-- | Specifies whether the job flow should terminate after completing all steps.
jfidKeepJobFlowAliveWhenNoSteps
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> JobFlowInstancesDetail
    -> f JobFlowInstancesDetail
jfidKeepJobFlowAliveWhenNoSteps f x =
    (\y -> x { _jfidKeepJobFlowAliveWhenNoSteps = y })
       <$> f (_jfidKeepJobFlowAliveWhenNoSteps x)
{-# INLINE jfidKeepJobFlowAliveWhenNoSteps #-}

-- | Specifies whether the Amazon EC2 instances in the cluster are protected
-- from termination by API calls, user intervention, or in the event of a job
-- flow error.
jfidTerminationProtected
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> JobFlowInstancesDetail
    -> f JobFlowInstancesDetail
jfidTerminationProtected f x =
    (\y -> x { _jfidTerminationProtected = y })
       <$> f (_jfidTerminationProtected x)
{-# INLINE jfidTerminationProtected #-}

-- | The Hadoop version for the job flow.
jfidHadoopVersion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> JobFlowInstancesDetail
    -> f JobFlowInstancesDetail
jfidHadoopVersion f x =
    (\y -> x { _jfidHadoopVersion = y })
       <$> f (_jfidHadoopVersion x)
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
kvKey
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> KeyValue
    -> f KeyValue
kvKey f x =
    (\y -> x { _kvKey = y })
       <$> f (_kvKey x)
{-# INLINE kvKey #-}

-- | The value part of the identified key.
kvValue
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> KeyValue
    -> f KeyValue
kvValue f x =
    (\y -> x { _kvValue = y })
       <$> f (_kvValue x)
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
sbacPath
    :: Functor f
    => (Text
    -> f (Text))
    -> ScriptBootstrapActionConfig
    -> f ScriptBootstrapActionConfig
sbacPath f x =
    (\y -> x { _sbacPath = y })
       <$> f (_sbacPath x)
{-# INLINE sbacPath #-}

-- | A list of command line arguments to pass to the bootstrap action script.
sbacArgs
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> ScriptBootstrapActionConfig
    -> f ScriptBootstrapActionConfig
sbacArgs f x =
    (\y -> x { _sbacArgs = y })
       <$> f (_sbacArgs x)
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
svId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Step
    -> f Step
svId f x =
    (\y -> x { _svId = y })
       <$> f (_svId x)
{-# INLINE svId #-}

-- | The name of the cluster step.
svName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Step
    -> f Step
svName f x =
    (\y -> x { _svName = y })
       <$> f (_svName x)
{-# INLINE svName #-}

-- | The Hadoop job configuration of the cluster step.
svConfig
    :: Functor f
    => (Maybe HadoopStepConfig
    -> f (Maybe HadoopStepConfig))
    -> Step
    -> f Step
svConfig f x =
    (\y -> x { _svConfig = y })
       <$> f (_svConfig x)
{-# INLINE svConfig #-}

-- | This specifies what action to take when the cluster step fails. Possible
-- values are TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE.
svActionOnFailure
    :: Functor f
    => (Maybe ActionOnFailure
    -> f (Maybe ActionOnFailure))
    -> Step
    -> f Step
svActionOnFailure f x =
    (\y -> x { _svActionOnFailure = y })
       <$> f (_svActionOnFailure x)
{-# INLINE svActionOnFailure #-}

-- | The current execution status details of the cluster step.
svStatus
    :: Functor f
    => (Maybe StepStatus
    -> f (Maybe StepStatus))
    -> Step
    -> f Step
svStatus f x =
    (\y -> x { _svStatus = y })
       <$> f (_svStatus x)
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
scName
    :: Functor f
    => (Text
    -> f (Text))
    -> StepConfig
    -> f StepConfig
scName f x =
    (\y -> x { _scName = y })
       <$> f (_scName x)
{-# INLINE scName #-}

-- | The action to take if the job flow step fails.
scActionOnFailure
    :: Functor f
    => (Maybe ActionOnFailure
    -> f (Maybe ActionOnFailure))
    -> StepConfig
    -> f StepConfig
scActionOnFailure f x =
    (\y -> x { _scActionOnFailure = y })
       <$> f (_scActionOnFailure x)
{-# INLINE scActionOnFailure #-}

-- | The JAR file used for the job flow step.
scHadoopJarStep
    :: Functor f
    => (HadoopJarStepConfig
    -> f (HadoopJarStepConfig))
    -> StepConfig
    -> f StepConfig
scHadoopJarStep f x =
    (\y -> x { _scHadoopJarStep = y })
       <$> f (_scHadoopJarStep x)
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
sdStepConfig
    :: Functor f
    => (StepConfig
    -> f (StepConfig))
    -> StepDetail
    -> f StepDetail
sdStepConfig f x =
    (\y -> x { _sdStepConfig = y })
       <$> f (_sdStepConfig x)
{-# INLINE sdStepConfig #-}

-- | The description of the step status.
sdExecutionStatusDetail
    :: Functor f
    => (StepExecutionStatusDetail
    -> f (StepExecutionStatusDetail))
    -> StepDetail
    -> f StepDetail
sdExecutionStatusDetail f x =
    (\y -> x { _sdExecutionStatusDetail = y })
       <$> f (_sdExecutionStatusDetail x)
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
sesdState
    :: Functor f
    => (StepExecutionState
    -> f (StepExecutionState))
    -> StepExecutionStatusDetail
    -> f StepExecutionStatusDetail
sesdState f x =
    (\y -> x { _sesdState = y })
       <$> f (_sesdState x)
{-# INLINE sesdState #-}

-- | The creation date and time of the step.
sesdCreationDateTime
    :: Functor f
    => (POSIX
    -> f (POSIX))
    -> StepExecutionStatusDetail
    -> f StepExecutionStatusDetail
sesdCreationDateTime f x =
    (\y -> x { _sesdCreationDateTime = y })
       <$> f (_sesdCreationDateTime x)
{-# INLINE sesdCreationDateTime #-}

-- | The start date and time of the step.
sesdStartDateTime
    :: Functor f
    => (Maybe POSIX
    -> f (Maybe POSIX))
    -> StepExecutionStatusDetail
    -> f StepExecutionStatusDetail
sesdStartDateTime f x =
    (\y -> x { _sesdStartDateTime = y })
       <$> f (_sesdStartDateTime x)
{-# INLINE sesdStartDateTime #-}

-- | The completion date and time of the step.
sesdEndDateTime
    :: Functor f
    => (Maybe POSIX
    -> f (Maybe POSIX))
    -> StepExecutionStatusDetail
    -> f StepExecutionStatusDetail
sesdEndDateTime f x =
    (\y -> x { _sesdEndDateTime = y })
       <$> f (_sesdEndDateTime x)
{-# INLINE sesdEndDateTime #-}

-- | A description of the step's current state.
sesdLastStateChangeReason
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StepExecutionStatusDetail
    -> f StepExecutionStatusDetail
sesdLastStateChangeReason f x =
    (\y -> x { _sesdLastStateChangeReason = y })
       <$> f (_sesdLastStateChangeReason x)
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
sscrCode
    :: Functor f
    => (Maybe StepStateChangeReasonCode
    -> f (Maybe StepStateChangeReasonCode))
    -> StepStateChangeReason
    -> f StepStateChangeReason
sscrCode f x =
    (\y -> x { _sscrCode = y })
       <$> f (_sscrCode x)
{-# INLINE sscrCode #-}

-- | The descriptive message for the state change reason.
sscrMessage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StepStateChangeReason
    -> f StepStateChangeReason
sscrMessage f x =
    (\y -> x { _sscrMessage = y })
       <$> f (_sscrMessage x)
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
sssState
    :: Functor f
    => (Maybe StepState
    -> f (Maybe StepState))
    -> StepStatus
    -> f StepStatus
sssState f x =
    (\y -> x { _sssState = y })
       <$> f (_sssState x)
{-# INLINE sssState #-}

-- | The reason for the step execution status change.
sssStateChangeReason
    :: Functor f
    => (Maybe StepStateChangeReason
    -> f (Maybe StepStateChangeReason))
    -> StepStatus
    -> f StepStatus
sssStateChangeReason f x =
    (\y -> x { _sssStateChangeReason = y })
       <$> f (_sssStateChangeReason x)
{-# INLINE sssStateChangeReason #-}

-- | The timeline of the cluster step status over time.
sssTimeline
    :: Functor f
    => (Maybe StepTimeline
    -> f (Maybe StepTimeline))
    -> StepStatus
    -> f StepStatus
sssTimeline f x =
    (\y -> x { _sssTimeline = y })
       <$> f (_sssTimeline x)
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
sssyId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StepSummary
    -> f StepSummary
sssyId f x =
    (\y -> x { _sssyId = y })
       <$> f (_sssyId x)
{-# INLINE sssyId #-}

-- | The name of the cluster step.
sssyName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StepSummary
    -> f StepSummary
sssyName f x =
    (\y -> x { _sssyName = y })
       <$> f (_sssyName x)
{-# INLINE sssyName #-}

-- | The current execution status details of the cluster step.
sssyStatus
    :: Functor f
    => (Maybe StepStatus
    -> f (Maybe StepStatus))
    -> StepSummary
    -> f StepSummary
sssyStatus f x =
    (\y -> x { _sssyStatus = y })
       <$> f (_sssyStatus x)
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
ssfCreationDateTime
    :: Functor f
    => (Maybe POSIX
    -> f (Maybe POSIX))
    -> StepTimeline
    -> f StepTimeline
ssfCreationDateTime f x =
    (\y -> x { _ssfCreationDateTime = y })
       <$> f (_ssfCreationDateTime x)
{-# INLINE ssfCreationDateTime #-}

-- | The date and time when the cluster step execution started.
ssfStartDateTime
    :: Functor f
    => (Maybe POSIX
    -> f (Maybe POSIX))
    -> StepTimeline
    -> f StepTimeline
ssfStartDateTime f x =
    (\y -> x { _ssfStartDateTime = y })
       <$> f (_ssfStartDateTime x)
{-# INLINE ssfStartDateTime #-}

-- | The date and time when the cluster step execution completed or failed.
ssfEndDateTime
    :: Functor f
    => (Maybe POSIX
    -> f (Maybe POSIX))
    -> StepTimeline
    -> f StepTimeline
ssfEndDateTime f x =
    (\y -> x { _ssfEndDateTime = y })
       <$> f (_ssfEndDateTime x)
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
spcName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SupportedProductConfig
    -> f SupportedProductConfig
spcName f x =
    (\y -> x { _spcName = y })
       <$> f (_spcName x)
{-# INLINE spcName #-}

-- | The list of user-supplied arguments.
spcArgs
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> SupportedProductConfig
    -> f SupportedProductConfig
spcArgs f x =
    (\y -> x { _spcArgs = y })
       <$> f (_spcArgs x)
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
tKey
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Tag
    -> f Tag
tKey f x =
    (\y -> x { _tKey = y })
       <$> f (_tKey x)
{-# INLINE tKey #-}

-- | A user-defined value, which is optional in a tag. For more information, see
-- Tagging Amazon EMR Resources.
tValue
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Tag
    -> f Tag
tValue f x =
    (\y -> x { _tValue = y })
       <$> f (_tValue x)
{-# INLINE tValue #-}

instance FromJSON Tag

instance ToJSON Tag

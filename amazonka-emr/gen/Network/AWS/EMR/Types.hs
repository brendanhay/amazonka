{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
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

module Network.AWS.EMR.Types
    (
    -- * Service
      EMR
    -- ** Error
    , JSONError
    -- ** JSON
    , jsonOptions

    -- * MarketType
    , MarketType (..)

    -- * InstanceGroupConfig
    , InstanceGroupConfig
    , instanceGroupConfig
    , igcBidPrice
    , igcInstanceCount
    , igcInstanceRole
    , igcInstanceType
    , igcMarket
    , igcName

    -- * InstanceStateChangeReason
    , InstanceStateChangeReason
    , instanceStateChangeReason
    , iscrCode
    , iscrMessage

    -- * JobFlowDetail
    , JobFlowDetail
    , jobFlowDetail
    , jfdAmiVersion
    , jfdBootstrapActions
    , jfdExecutionStatusDetail
    , jfdInstances
    , jfdJobFlowId
    , jfdJobFlowRole
    , jfdLogUri
    , jfdName
    , jfdServiceRole
    , jfdSteps
    , jfdSupportedProducts
    , jfdVisibleToAllUsers

    -- * KeyValue
    , KeyValue
    , keyValue
    , kvKey
    , kvValue

    -- * SupportedProductConfig
    , SupportedProductConfig
    , supportedProductConfig
    , spcArgs
    , spcName

    -- * Command
    , Command
    , command
    , cArgs
    , cName
    , cScriptPath

    -- * StepExecutionState
    , StepExecutionState (..)

    -- * ActionOnFailure
    , ActionOnFailure (..)

    -- * ClusterStateChangeReason
    , ClusterStateChangeReason
    , clusterStateChangeReason
    , cscrCode
    , cscrMessage

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- * Application
    , Application
    , application
    , aAdditionalInfo
    , aArgs
    , aName
    , aVersion

    -- * JobFlowExecutionStatusDetail
    , JobFlowExecutionStatusDetail
    , jobFlowExecutionStatusDetail
    , jfesdCreationDateTime
    , jfesdEndDateTime
    , jfesdLastStateChangeReason
    , jfesdReadyDateTime
    , jfesdStartDateTime
    , jfesdState

    -- * InstanceGroupStatus
    , InstanceGroupStatus
    , instanceGroupStatus
    , igsState
    , igsStateChangeReason
    , igsTimeline

    -- * Cluster
    , Cluster
    , cluster
    , c1Applications
    , c1AutoTerminate
    , c1Ec2InstanceAttributes
    , c1Id
    , c1LogUri
    , c1Name
    , c1RequestedAmiVersion
    , c1RunningAmiVersion
    , c1ServiceRole
    , c1Status
    , c1Tags
    , c1TerminationProtected
    , c1VisibleToAllUsers

    -- * InstanceTimeline
    , InstanceTimeline
    , instanceTimeline
    , itCreationDateTime
    , itEndDateTime
    , itReadyDateTime

    -- * Ec2InstanceAttributes
    , Ec2InstanceAttributes
    , ec2InstanceAttributes
    , eiaEc2AvailabilityZone
    , eiaEc2KeyName
    , eiaEc2SubnetId
    , eiaIamInstanceProfile

    -- * StepStateChangeReasonCode
    , StepStateChangeReasonCode (..)

    -- * ClusterState
    , ClusterState (..)

    -- * HadoopStepConfig
    , HadoopStepConfig
    , hadoopStepConfig
    , hscArgs
    , hscJar
    , hscMainClass
    , hscProperties

    -- * JobFlowExecutionState
    , JobFlowExecutionState (..)

    -- * StepDetail
    , StepDetail
    , stepDetail
    , sdExecutionStatusDetail
    , sdStepConfig

    -- * InstanceGroupStateChangeReason
    , InstanceGroupStateChangeReason
    , instanceGroupStateChangeReason
    , igscrCode
    , igscrMessage

    -- * InstanceGroupType
    , InstanceGroupType (..)

    -- * InstanceGroupStateChangeReasonCode
    , InstanceGroupStateChangeReasonCode (..)

    -- * StepStatus
    , StepStatus
    , stepStatus
    , ssState
    , ssStateChangeReason
    , ssTimeline

    -- * StepSummary
    , StepSummary
    , stepSummary
    , ssId
    , ssName
    , ssStatus

    -- * InstanceGroupState
    , InstanceGroupState (..)

    -- * StepTimeline
    , StepTimeline
    , stepTimeline
    , stCreationDateTime
    , stEndDateTime
    , stStartDateTime

    -- * PlacementType
    , PlacementType
    , placementType
    , ptAvailabilityZone

    -- * HadoopJarStepConfig
    , HadoopJarStepConfig
    , hadoopJarStepConfig
    , hjscArgs
    , hjscJar
    , hjscMainClass
    , hjscProperties

    -- * InstanceGroupModifyConfig
    , InstanceGroupModifyConfig
    , instanceGroupModifyConfig
    , igmcEC2InstanceIdsToTerminate
    , igmcInstanceCount
    , igmcInstanceGroupId

    -- * InstanceGroupDetail
    , InstanceGroupDetail
    , instanceGroupDetail
    , igdBidPrice
    , igdCreationDateTime
    , igdEndDateTime
    , igdInstanceGroupId
    , igdInstanceRequestCount
    , igdInstanceRole
    , igdInstanceRunningCount
    , igdInstanceType
    , igdLastStateChangeReason
    , igdMarket
    , igdName
    , igdReadyDateTime
    , igdStartDateTime
    , igdState

    -- * StepStateChangeReason
    , StepStateChangeReason
    , stepStateChangeReason
    , sscrCode
    , sscrMessage

    -- * ClusterStateChangeReasonCode
    , ClusterStateChangeReasonCode (..)

    -- * Step
    , Step
    , step
    , sActionOnFailure
    , sConfig
    , sId
    , sName
    , sStatus

    -- * StepState
    , StepState (..)

    -- * InstanceGroupTimeline
    , InstanceGroupTimeline
    , instanceGroupTimeline
    , igtCreationDateTime
    , igtEndDateTime
    , igtReadyDateTime

    -- * BootstrapActionDetail
    , BootstrapActionDetail
    , bootstrapActionDetail
    , badBootstrapActionConfig

    -- * StepExecutionStatusDetail
    , StepExecutionStatusDetail
    , stepExecutionStatusDetail
    , sesdCreationDateTime
    , sesdEndDateTime
    , sesdLastStateChangeReason
    , sesdStartDateTime
    , sesdState

    -- * InstanceStatus
    , InstanceStatus
    , instanceStatus
    , isState
    , isStateChangeReason
    , isTimeline

    -- * InstanceRoleType
    , InstanceRoleType (..)

    -- * JobFlowInstancesConfig
    , JobFlowInstancesConfig
    , jobFlowInstancesConfig
    , jficEc2KeyName
    , jficEc2SubnetId
    , jficHadoopVersion
    , jficInstanceCount
    , jficInstanceGroups
    , jficKeepJobFlowAliveWhenNoSteps
    , jficMasterInstanceType
    , jficPlacement
    , jficSlaveInstanceType
    , jficTerminationProtected

    -- * StepConfig
    , StepConfig
    , stepConfig
    , scActionOnFailure
    , scHadoopJarStep
    , scName

    -- * InstanceGroup
    , InstanceGroup
    , instanceGroup
    , igBidPrice
    , igId
    , igInstanceGroupType
    , igInstanceType
    , igMarket
    , igName
    , igRequestedInstanceCount
    , igRunningInstanceCount
    , igStatus

    -- * BootstrapActionConfig
    , BootstrapActionConfig
    , bootstrapActionConfig
    , bacName
    , bacScriptBootstrapAction

    -- * ClusterSummary
    , ClusterSummary
    , clusterSummary
    , csId
    , csName
    , csStatus

    -- * JobFlowInstancesDetail
    , JobFlowInstancesDetail
    , jobFlowInstancesDetail
    , jfidEc2KeyName
    , jfidEc2SubnetId
    , jfidHadoopVersion
    , jfidInstanceCount
    , jfidInstanceGroups
    , jfidKeepJobFlowAliveWhenNoSteps
    , jfidMasterInstanceId
    , jfidMasterInstanceType
    , jfidMasterPublicDnsName
    , jfidNormalizedInstanceHours
    , jfidPlacement
    , jfidSlaveInstanceType
    , jfidTerminationProtected

    -- * ClusterStatus
    , ClusterStatus
    , clusterStatus
    , csState
    , csStateChangeReason
    , csTimeline

    -- * InstanceState
    , InstanceState (..)

    -- * ClusterTimeline
    , ClusterTimeline
    , clusterTimeline
    , ctCreationDateTime
    , ctEndDateTime
    , ctReadyDateTime

    -- * InstanceStateChangeReasonCode
    , InstanceStateChangeReasonCode (..)

    -- * Instance
    , Instance
    , instance'
    , iEc2InstanceId
    , iId
    , iPrivateDnsName
    , iPrivateIpAddress
    , iPublicDnsName
    , iPublicIpAddress
    , iStatus

    -- * ScriptBootstrapActionConfig
    , ScriptBootstrapActionConfig
    , scriptBootstrapActionConfig
    , sbacArgs
    , sbacPath
    ) where

import Data.Char (isUpper)
import Network.AWS.Error
import Network.AWS.Prelude
import Network.AWS.Signing.V4
import qualified GHC.Exts

-- | Supported version (@2009-03-31@) of the Amazon Elastic MapReduce.
data EMR deriving (Typeable)

instance AWSService EMR where
    type Sg EMR = V4
    type Er EMR = JSONError

    service = Service
        { _svcEndpoint = regional
        , _svcAbbrev   = "EMR"
        , _svcPrefix   = "elasticmapreduce"
        , _svcVersion  = "2009-03-31"
        , _svcTarget   = Nothing
        }

    handle = jsonError alwaysFail

jsonOptions :: AesonOptions
jsonOptions = defaultOptions
    { fieldLabelModifier = dropWhile (not . isUpper)
    }

data MarketType
    = OnDemand -- ^ ON_DEMAND
    | Spot     -- ^ SPOT
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable MarketType

instance FromText MarketType where
    parser = match "ON_DEMAND" OnDemand
         <|> match "SPOT"      Spot

instance ToText MarketType where
    toText = \case
        OnDemand -> "ON_DEMAND"
        Spot     -> "SPOT"

instance FromJSON MarketType where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON MarketType where
    toJSON = genericToJSON jsonOptions

data InstanceGroupConfig = InstanceGroupConfig
    { _igcBidPrice      :: Maybe Text
    , _igcInstanceCount :: Int
    , _igcInstanceRole  :: Text
    , _igcInstanceType  :: Text
    , _igcMarket        :: Maybe Text
    , _igcName          :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'InstanceGroupConfig' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'igcBidPrice' @::@ 'Maybe' 'Text'
--
-- * 'igcInstanceCount' @::@ 'Int'
--
-- * 'igcInstanceRole' @::@ 'Text'
--
-- * 'igcInstanceType' @::@ 'Text'
--
-- * 'igcMarket' @::@ 'Maybe' 'Text'
--
-- * 'igcName' @::@ 'Maybe' 'Text'
--
instanceGroupConfig :: Text -- ^ 'igcInstanceRole'
                    -> Text -- ^ 'igcInstanceType'
                    -> Int -- ^ 'igcInstanceCount'
                    -> InstanceGroupConfig
instanceGroupConfig p1 p2 p3 = InstanceGroupConfig
    { _igcInstanceRole  = p1
    , _igcInstanceType  = p2
    , _igcInstanceCount = p3
    , _igcName          = Nothing
    , _igcMarket        = Nothing
    , _igcBidPrice      = Nothing
    }

-- | Bid price for each Amazon EC2 instance in the instance group when
-- launching nodes as Spot Instances, expressed in USD.
igcBidPrice :: Lens' InstanceGroupConfig (Maybe Text)
igcBidPrice = lens _igcBidPrice (\s a -> s { _igcBidPrice = a })

-- | Target number of instances for the instance group.
igcInstanceCount :: Lens' InstanceGroupConfig Int
igcInstanceCount = lens _igcInstanceCount (\s a -> s { _igcInstanceCount = a })

-- | The role of the instance group in the cluster.
igcInstanceRole :: Lens' InstanceGroupConfig Text
igcInstanceRole = lens _igcInstanceRole (\s a -> s { _igcInstanceRole = a })

-- | The Amazon EC2 instance type for all instances in the instance group.
igcInstanceType :: Lens' InstanceGroupConfig Text
igcInstanceType = lens _igcInstanceType (\s a -> s { _igcInstanceType = a })

-- | Market type of the Amazon EC2 instances used to create a cluster node.
igcMarket :: Lens' InstanceGroupConfig (Maybe Text)
igcMarket = lens _igcMarket (\s a -> s { _igcMarket = a })

-- | Friendly name given to the instance group.
igcName :: Lens' InstanceGroupConfig (Maybe Text)
igcName = lens _igcName (\s a -> s { _igcName = a })

instance FromJSON InstanceGroupConfig where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON InstanceGroupConfig where
    toJSON = genericToJSON jsonOptions

data InstanceStateChangeReason = InstanceStateChangeReason
    { _iscrCode    :: Maybe Text
    , _iscrMessage :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'InstanceStateChangeReason' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iscrCode' @::@ 'Maybe' 'Text'
--
-- * 'iscrMessage' @::@ 'Maybe' 'Text'
--
instanceStateChangeReason :: InstanceStateChangeReason
instanceStateChangeReason = InstanceStateChangeReason
    { _iscrCode    = Nothing
    , _iscrMessage = Nothing
    }

-- | The programmable code for the state change reason.
iscrCode :: Lens' InstanceStateChangeReason (Maybe Text)
iscrCode = lens _iscrCode (\s a -> s { _iscrCode = a })

-- | The status change reason description.
iscrMessage :: Lens' InstanceStateChangeReason (Maybe Text)
iscrMessage = lens _iscrMessage (\s a -> s { _iscrMessage = a })

instance FromJSON InstanceStateChangeReason where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON InstanceStateChangeReason where
    toJSON = genericToJSON jsonOptions

data JobFlowDetail = JobFlowDetail
    { _jfdAmiVersion            :: Maybe Text
    , _jfdBootstrapActions      :: [BootstrapActionDetail]
    , _jfdExecutionStatusDetail :: JobFlowExecutionStatusDetail
    , _jfdInstances             :: JobFlowInstancesDetail
    , _jfdJobFlowId             :: Text
    , _jfdJobFlowRole           :: Maybe Text
    , _jfdLogUri                :: Maybe Text
    , _jfdName                  :: Text
    , _jfdServiceRole           :: Maybe Text
    , _jfdSteps                 :: [StepDetail]
    , _jfdSupportedProducts     :: [Text]
    , _jfdVisibleToAllUsers     :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | 'JobFlowDetail' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'jfdAmiVersion' @::@ 'Maybe' 'Text'
--
-- * 'jfdBootstrapActions' @::@ ['BootstrapActionDetail']
--
-- * 'jfdExecutionStatusDetail' @::@ 'JobFlowExecutionStatusDetail'
--
-- * 'jfdInstances' @::@ 'JobFlowInstancesDetail'
--
-- * 'jfdJobFlowId' @::@ 'Text'
--
-- * 'jfdJobFlowRole' @::@ 'Maybe' 'Text'
--
-- * 'jfdLogUri' @::@ 'Maybe' 'Text'
--
-- * 'jfdName' @::@ 'Text'
--
-- * 'jfdServiceRole' @::@ 'Maybe' 'Text'
--
-- * 'jfdSteps' @::@ ['StepDetail']
--
-- * 'jfdSupportedProducts' @::@ ['Text']
--
-- * 'jfdVisibleToAllUsers' @::@ 'Maybe' 'Bool'
--
jobFlowDetail :: Text -- ^ 'jfdJobFlowId'
              -> Text -- ^ 'jfdName'
              -> JobFlowExecutionStatusDetail -- ^ 'jfdExecutionStatusDetail'
              -> JobFlowInstancesDetail -- ^ 'jfdInstances'
              -> JobFlowDetail
jobFlowDetail p1 p2 p3 p4 = JobFlowDetail
    { _jfdJobFlowId             = p1
    , _jfdName                  = p2
    , _jfdExecutionStatusDetail = p3
    , _jfdInstances             = p4
    , _jfdLogUri                = Nothing
    , _jfdAmiVersion            = Nothing
    , _jfdSteps                 = mempty
    , _jfdBootstrapActions      = mempty
    , _jfdSupportedProducts     = mempty
    , _jfdVisibleToAllUsers     = Nothing
    , _jfdJobFlowRole           = Nothing
    , _jfdServiceRole           = Nothing
    }

-- | The version of the AMI used to initialize Amazon EC2 instances in the job
-- flow. For a list of AMI versions currently supported by Amazon
-- ElasticMapReduce, go to AMI Versions Supported in Elastic MapReduce in
-- the Amazon Elastic MapReduce Developer's Guide.
jfdAmiVersion :: Lens' JobFlowDetail (Maybe Text)
jfdAmiVersion = lens _jfdAmiVersion (\s a -> s { _jfdAmiVersion = a })

-- | A list of the bootstrap actions run by the job flow.
jfdBootstrapActions :: Lens' JobFlowDetail [BootstrapActionDetail]
jfdBootstrapActions =
    lens _jfdBootstrapActions (\s a -> s { _jfdBootstrapActions = a })

-- | Describes the execution status of the job flow.
jfdExecutionStatusDetail :: Lens' JobFlowDetail JobFlowExecutionStatusDetail
jfdExecutionStatusDetail =
    lens _jfdExecutionStatusDetail
        (\s a -> s { _jfdExecutionStatusDetail = a })

-- | Describes the Amazon EC2 instances of the job flow.
jfdInstances :: Lens' JobFlowDetail JobFlowInstancesDetail
jfdInstances = lens _jfdInstances (\s a -> s { _jfdInstances = a })

-- | The job flow identifier.
jfdJobFlowId :: Lens' JobFlowDetail Text
jfdJobFlowId = lens _jfdJobFlowId (\s a -> s { _jfdJobFlowId = a })

-- | The IAM role that was specified when the job flow was launched. The EC2
-- instances of the job flow assume this role.
jfdJobFlowRole :: Lens' JobFlowDetail (Maybe Text)
jfdJobFlowRole = lens _jfdJobFlowRole (\s a -> s { _jfdJobFlowRole = a })

-- | The location in Amazon S3 where log files for the job are stored.
jfdLogUri :: Lens' JobFlowDetail (Maybe Text)
jfdLogUri = lens _jfdLogUri (\s a -> s { _jfdLogUri = a })

-- | The name of the job flow.
jfdName :: Lens' JobFlowDetail Text
jfdName = lens _jfdName (\s a -> s { _jfdName = a })

-- | The IAM role that will be assumed by the Amazon EMR service to access AWS
-- resources on your behalf.
jfdServiceRole :: Lens' JobFlowDetail (Maybe Text)
jfdServiceRole = lens _jfdServiceRole (\s a -> s { _jfdServiceRole = a })

-- | A list of steps run by the job flow.
jfdSteps :: Lens' JobFlowDetail [StepDetail]
jfdSteps = lens _jfdSteps (\s a -> s { _jfdSteps = a })

-- | A list of strings set by third party software when the job flow is
-- launched. If you are not using third party software to manage the job
-- flow this value is empty.
jfdSupportedProducts :: Lens' JobFlowDetail [Text]
jfdSupportedProducts =
    lens _jfdSupportedProducts (\s a -> s { _jfdSupportedProducts = a })

-- | Specifies whether the job flow is visible to all IAM users of the AWS
-- account associated with the job flow. If this value is set to true, all
-- IAM users of that AWS account can view and (if they have the proper
-- policy permissions set) manage the job flow. If it is set to false, only
-- the IAM user that created the job flow can view and manage it. This value
-- can be changed using the SetVisibleToAllUsers action.
jfdVisibleToAllUsers :: Lens' JobFlowDetail (Maybe Bool)
jfdVisibleToAllUsers =
    lens _jfdVisibleToAllUsers (\s a -> s { _jfdVisibleToAllUsers = a })

instance FromJSON JobFlowDetail where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON JobFlowDetail where
    toJSON = genericToJSON jsonOptions

data KeyValue = KeyValue
    { _kvKey   :: Maybe Text
    , _kvValue :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'KeyValue' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'kvKey' @::@ 'Maybe' 'Text'
--
-- * 'kvValue' @::@ 'Maybe' 'Text'
--
keyValue :: KeyValue
keyValue = KeyValue
    { _kvKey   = Nothing
    , _kvValue = Nothing
    }

-- | The unique identifier of a key value pair.
kvKey :: Lens' KeyValue (Maybe Text)
kvKey = lens _kvKey (\s a -> s { _kvKey = a })

-- | The value part of the identified key.
kvValue :: Lens' KeyValue (Maybe Text)
kvValue = lens _kvValue (\s a -> s { _kvValue = a })

instance FromJSON KeyValue where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON KeyValue where
    toJSON = genericToJSON jsonOptions

data SupportedProductConfig = SupportedProductConfig
    { _spcArgs :: [Text]
    , _spcName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'SupportedProductConfig' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'spcArgs' @::@ ['Text']
--
-- * 'spcName' @::@ 'Maybe' 'Text'
--
supportedProductConfig :: SupportedProductConfig
supportedProductConfig = SupportedProductConfig
    { _spcName = Nothing
    , _spcArgs = mempty
    }

-- | The list of user-supplied arguments.
spcArgs :: Lens' SupportedProductConfig [Text]
spcArgs = lens _spcArgs (\s a -> s { _spcArgs = a })

-- | The name of the product configuration.
spcName :: Lens' SupportedProductConfig (Maybe Text)
spcName = lens _spcName (\s a -> s { _spcName = a })

instance FromJSON SupportedProductConfig where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON SupportedProductConfig where
    toJSON = genericToJSON jsonOptions

data Command = Command
    { _cArgs       :: [Text]
    , _cName       :: Maybe Text
    , _cScriptPath :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Command' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cArgs' @::@ ['Text']
--
-- * 'cName' @::@ 'Maybe' 'Text'
--
-- * 'cScriptPath' @::@ 'Maybe' 'Text'
--
command :: Command
command = Command
    { _cName       = Nothing
    , _cScriptPath = Nothing
    , _cArgs       = mempty
    }

-- | Arguments for Amazon EMR to pass to the command for execution.
cArgs :: Lens' Command [Text]
cArgs = lens _cArgs (\s a -> s { _cArgs = a })

-- | The name of the command.
cName :: Lens' Command (Maybe Text)
cName = lens _cName (\s a -> s { _cName = a })

-- | The Amazon S3 location of the command script.
cScriptPath :: Lens' Command (Maybe Text)
cScriptPath = lens _cScriptPath (\s a -> s { _cScriptPath = a })

instance FromJSON Command where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON Command where
    toJSON = genericToJSON jsonOptions

data StepExecutionState
    = Cancelled   -- ^ CANCELLED
    | Completed   -- ^ COMPLETED
    | Continue    -- ^ CONTINUE
    | Failed      -- ^ FAILED
    | Interrupted -- ^ INTERRUPTED
    | Pending     -- ^ PENDING
    | Running     -- ^ RUNNING
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable StepExecutionState

instance FromText StepExecutionState where
    parser = match "CANCELLED"   Cancelled
         <|> match "COMPLETED"   Completed
         <|> match "CONTINUE"    Continue
         <|> match "FAILED"      Failed
         <|> match "INTERRUPTED" Interrupted
         <|> match "PENDING"     Pending
         <|> match "RUNNING"     Running

instance ToText StepExecutionState where
    toText = \case
        Cancelled   -> "CANCELLED"
        Completed   -> "COMPLETED"
        Continue    -> "CONTINUE"
        Failed      -> "FAILED"
        Interrupted -> "INTERRUPTED"
        Pending     -> "PENDING"
        Running     -> "RUNNING"

instance FromJSON StepExecutionState where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON StepExecutionState where
    toJSON = genericToJSON jsonOptions

data ActionOnFailure
    = AOFCancelAndWait    -- ^ CANCEL_AND_WAIT
    | AOFContinue         -- ^ CONTINUE
    | AOFTerminateCluster -- ^ TERMINATE_CLUSTER
    | AOFTerminateJobFlow -- ^ TERMINATE_JOB_FLOW
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ActionOnFailure

instance FromText ActionOnFailure where
    parser = match "CANCEL_AND_WAIT"    AOFCancelAndWait
         <|> match "CONTINUE"           AOFContinue
         <|> match "TERMINATE_CLUSTER"  AOFTerminateCluster
         <|> match "TERMINATE_JOB_FLOW" AOFTerminateJobFlow

instance ToText ActionOnFailure where
    toText = \case
        AOFCancelAndWait    -> "CANCEL_AND_WAIT"
        AOFContinue         -> "CONTINUE"
        AOFTerminateCluster -> "TERMINATE_CLUSTER"
        AOFTerminateJobFlow -> "TERMINATE_JOB_FLOW"

instance FromJSON ActionOnFailure where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON ActionOnFailure where
    toJSON = genericToJSON jsonOptions

data ClusterStateChangeReason = ClusterStateChangeReason
    { _cscrCode    :: Maybe Text
    , _cscrMessage :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ClusterStateChangeReason' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cscrCode' @::@ 'Maybe' 'Text'
--
-- * 'cscrMessage' @::@ 'Maybe' 'Text'
--
clusterStateChangeReason :: ClusterStateChangeReason
clusterStateChangeReason = ClusterStateChangeReason
    { _cscrCode    = Nothing
    , _cscrMessage = Nothing
    }

-- | The programmatic code for the state change reason.
cscrCode :: Lens' ClusterStateChangeReason (Maybe Text)
cscrCode = lens _cscrCode (\s a -> s { _cscrCode = a })

-- | The descriptive message for the state change reason.
cscrMessage :: Lens' ClusterStateChangeReason (Maybe Text)
cscrMessage = lens _cscrMessage (\s a -> s { _cscrMessage = a })

instance FromJSON ClusterStateChangeReason where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON ClusterStateChangeReason where
    toJSON = genericToJSON jsonOptions

data Tag = Tag
    { _tagKey   :: Maybe Text
    , _tagValue :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Tag' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tagKey' @::@ 'Maybe' 'Text'
--
-- * 'tagValue' @::@ 'Maybe' 'Text'
--
tag :: Tag
tag = Tag
    { _tagKey   = Nothing
    , _tagValue = Nothing
    }

-- | A user-defined key, which is the minimum required information for a valid
-- tag. For more information, see Tagging Amazon EMR Resources.
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\s a -> s { _tagKey = a })

-- | A user-defined value, which is optional in a tag. For more information,
-- see Tagging Amazon EMR Resources.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\s a -> s { _tagValue = a })

instance FromJSON Tag where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON Tag where
    toJSON = genericToJSON jsonOptions

data Application = Application
    { _aAdditionalInfo :: Map Text Text
    , _aArgs           :: [Text]
    , _aName           :: Maybe Text
    , _aVersion        :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'Application' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aAdditionalInfo' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'aArgs' @::@ ['Text']
--
-- * 'aName' @::@ 'Maybe' 'Text'
--
-- * 'aVersion' @::@ 'Maybe' 'Text'
--
application :: Application
application = Application
    { _aName           = Nothing
    , _aVersion        = Nothing
    , _aArgs           = mempty
    , _aAdditionalInfo = mempty
    }

-- | This option is for advanced users only. This is meta information about
-- third-party applications that third-party vendors use for testing
-- purposes.
aAdditionalInfo :: Lens' Application (HashMap Text Text)
aAdditionalInfo = lens _aAdditionalInfo (\s a -> s { _aAdditionalInfo = a })
    . _Map

-- | Arguments for Amazon EMR to pass to the application.
aArgs :: Lens' Application [Text]
aArgs = lens _aArgs (\s a -> s { _aArgs = a })

-- | The name of the application.
aName :: Lens' Application (Maybe Text)
aName = lens _aName (\s a -> s { _aName = a })

-- | The version of the application.
aVersion :: Lens' Application (Maybe Text)
aVersion = lens _aVersion (\s a -> s { _aVersion = a })

instance FromJSON Application where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON Application where
    toJSON = genericToJSON jsonOptions

data JobFlowExecutionStatusDetail = JobFlowExecutionStatusDetail
    { _jfesdCreationDateTime      :: RFC822
    , _jfesdEndDateTime           :: Maybe RFC822
    , _jfesdLastStateChangeReason :: Maybe Text
    , _jfesdReadyDateTime         :: Maybe RFC822
    , _jfesdStartDateTime         :: Maybe RFC822
    , _jfesdState                 :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'JobFlowExecutionStatusDetail' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'jfesdCreationDateTime' @::@ 'UTCTime'
--
-- * 'jfesdEndDateTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'jfesdLastStateChangeReason' @::@ 'Maybe' 'Text'
--
-- * 'jfesdReadyDateTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'jfesdStartDateTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'jfesdState' @::@ 'Text'
--
jobFlowExecutionStatusDetail :: Text -- ^ 'jfesdState'
                             -> UTCTime -- ^ 'jfesdCreationDateTime'
                             -> JobFlowExecutionStatusDetail
jobFlowExecutionStatusDetail p1 p2 = JobFlowExecutionStatusDetail
    { _jfesdState                 = p1
    , _jfesdCreationDateTime      = withIso _Time (const id) p2
    , _jfesdStartDateTime         = Nothing
    , _jfesdReadyDateTime         = Nothing
    , _jfesdEndDateTime           = Nothing
    , _jfesdLastStateChangeReason = Nothing
    }

-- | The creation date and time of the job flow.
jfesdCreationDateTime :: Lens' JobFlowExecutionStatusDetail UTCTime
jfesdCreationDateTime =
    lens _jfesdCreationDateTime (\s a -> s { _jfesdCreationDateTime = a })
        . _Time

-- | The completion date and time of the job flow.
jfesdEndDateTime :: Lens' JobFlowExecutionStatusDetail (Maybe UTCTime)
jfesdEndDateTime = lens _jfesdEndDateTime (\s a -> s { _jfesdEndDateTime = a })
    . mapping _Time

-- | Description of the job flow last changed state.
jfesdLastStateChangeReason :: Lens' JobFlowExecutionStatusDetail (Maybe Text)
jfesdLastStateChangeReason =
    lens _jfesdLastStateChangeReason
        (\s a -> s { _jfesdLastStateChangeReason = a })

-- | The date and time when the job flow was ready to start running bootstrap
-- actions.
jfesdReadyDateTime :: Lens' JobFlowExecutionStatusDetail (Maybe UTCTime)
jfesdReadyDateTime =
    lens _jfesdReadyDateTime (\s a -> s { _jfesdReadyDateTime = a })
        . mapping _Time

-- | The start date and time of the job flow.
jfesdStartDateTime :: Lens' JobFlowExecutionStatusDetail (Maybe UTCTime)
jfesdStartDateTime =
    lens _jfesdStartDateTime (\s a -> s { _jfesdStartDateTime = a })
        . mapping _Time

-- | The state of the job flow.
jfesdState :: Lens' JobFlowExecutionStatusDetail Text
jfesdState = lens _jfesdState (\s a -> s { _jfesdState = a })

instance FromJSON JobFlowExecutionStatusDetail where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON JobFlowExecutionStatusDetail where
    toJSON = genericToJSON jsonOptions

data InstanceGroupStatus = InstanceGroupStatus
    { _igsState             :: Maybe Text
    , _igsStateChangeReason :: Maybe InstanceGroupStateChangeReason
    , _igsTimeline          :: Maybe InstanceGroupTimeline
    } deriving (Eq, Show, Generic)

-- | 'InstanceGroupStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'igsState' @::@ 'Maybe' 'Text'
--
-- * 'igsStateChangeReason' @::@ 'Maybe' 'InstanceGroupStateChangeReason'
--
-- * 'igsTimeline' @::@ 'Maybe' 'InstanceGroupTimeline'
--
instanceGroupStatus :: InstanceGroupStatus
instanceGroupStatus = InstanceGroupStatus
    { _igsState             = Nothing
    , _igsStateChangeReason = Nothing
    , _igsTimeline          = Nothing
    }

-- | The current state of the instance group.
igsState :: Lens' InstanceGroupStatus (Maybe Text)
igsState = lens _igsState (\s a -> s { _igsState = a })

-- | The status change reason details for the instance group.
igsStateChangeReason :: Lens' InstanceGroupStatus (Maybe InstanceGroupStateChangeReason)
igsStateChangeReason =
    lens _igsStateChangeReason (\s a -> s { _igsStateChangeReason = a })

-- | The timeline of the instance group status over time.
igsTimeline :: Lens' InstanceGroupStatus (Maybe InstanceGroupTimeline)
igsTimeline = lens _igsTimeline (\s a -> s { _igsTimeline = a })

instance FromJSON InstanceGroupStatus where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON InstanceGroupStatus where
    toJSON = genericToJSON jsonOptions

data Cluster = Cluster
    { _c1Applications          :: [Application]
    , _c1AutoTerminate         :: Maybe Bool
    , _c1Ec2InstanceAttributes :: Maybe Ec2InstanceAttributes
    , _c1Id                    :: Maybe Text
    , _c1LogUri                :: Maybe Text
    , _c1Name                  :: Maybe Text
    , _c1RequestedAmiVersion   :: Maybe Text
    , _c1RunningAmiVersion     :: Maybe Text
    , _c1ServiceRole           :: Maybe Text
    , _c1Status                :: Maybe ClusterStatus
    , _c1Tags                  :: [Tag]
    , _c1TerminationProtected  :: Maybe Bool
    , _c1VisibleToAllUsers     :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | 'Cluster' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'c1Applications' @::@ ['Application']
--
-- * 'c1AutoTerminate' @::@ 'Maybe' 'Bool'
--
-- * 'c1Ec2InstanceAttributes' @::@ 'Maybe' 'Ec2InstanceAttributes'
--
-- * 'c1Id' @::@ 'Maybe' 'Text'
--
-- * 'c1LogUri' @::@ 'Maybe' 'Text'
--
-- * 'c1Name' @::@ 'Maybe' 'Text'
--
-- * 'c1RequestedAmiVersion' @::@ 'Maybe' 'Text'
--
-- * 'c1RunningAmiVersion' @::@ 'Maybe' 'Text'
--
-- * 'c1ServiceRole' @::@ 'Maybe' 'Text'
--
-- * 'c1Status' @::@ 'Maybe' 'ClusterStatus'
--
-- * 'c1Tags' @::@ ['Tag']
--
-- * 'c1TerminationProtected' @::@ 'Maybe' 'Bool'
--
-- * 'c1VisibleToAllUsers' @::@ 'Maybe' 'Bool'
--
cluster :: Cluster
cluster = Cluster
    { _c1Id                    = Nothing
    , _c1Name                  = Nothing
    , _c1Status                = Nothing
    , _c1Ec2InstanceAttributes = Nothing
    , _c1LogUri                = Nothing
    , _c1RequestedAmiVersion   = Nothing
    , _c1RunningAmiVersion     = Nothing
    , _c1AutoTerminate         = Nothing
    , _c1TerminationProtected  = Nothing
    , _c1VisibleToAllUsers     = Nothing
    , _c1Applications          = mempty
    , _c1Tags                  = mempty
    , _c1ServiceRole           = Nothing
    }

-- | The applications installed on this cluster.
c1Applications :: Lens' Cluster [Application]
c1Applications = lens _c1Applications (\s a -> s { _c1Applications = a })

-- | Specifies whether the cluster should terminate after completing all
-- steps.
c1AutoTerminate :: Lens' Cluster (Maybe Bool)
c1AutoTerminate = lens _c1AutoTerminate (\s a -> s { _c1AutoTerminate = a })

c1Ec2InstanceAttributes :: Lens' Cluster (Maybe Ec2InstanceAttributes)
c1Ec2InstanceAttributes =
    lens _c1Ec2InstanceAttributes (\s a -> s { _c1Ec2InstanceAttributes = a })

-- | The unique identifier for the cluster.
c1Id :: Lens' Cluster (Maybe Text)
c1Id = lens _c1Id (\s a -> s { _c1Id = a })

-- | The path to the Amazon S3 location where logs for this cluster are
-- stored.
c1LogUri :: Lens' Cluster (Maybe Text)
c1LogUri = lens _c1LogUri (\s a -> s { _c1LogUri = a })

-- | The name of the cluster.
c1Name :: Lens' Cluster (Maybe Text)
c1Name = lens _c1Name (\s a -> s { _c1Name = a })

-- | The AMI version requested for this cluster.
c1RequestedAmiVersion :: Lens' Cluster (Maybe Text)
c1RequestedAmiVersion =
    lens _c1RequestedAmiVersion (\s a -> s { _c1RequestedAmiVersion = a })

-- | The AMI version running on this cluster. This differs from the requested
-- version only if the requested version is a meta version, such as
-- "latest".
c1RunningAmiVersion :: Lens' Cluster (Maybe Text)
c1RunningAmiVersion =
    lens _c1RunningAmiVersion (\s a -> s { _c1RunningAmiVersion = a })

-- | The IAM role that will be assumed by the Amazon EMR service to access AWS
-- resources on your behalf.
c1ServiceRole :: Lens' Cluster (Maybe Text)
c1ServiceRole = lens _c1ServiceRole (\s a -> s { _c1ServiceRole = a })

-- | The current status details about the cluster.
c1Status :: Lens' Cluster (Maybe ClusterStatus)
c1Status = lens _c1Status (\s a -> s { _c1Status = a })

-- | A list of tags associated with a cluster.
c1Tags :: Lens' Cluster [Tag]
c1Tags = lens _c1Tags (\s a -> s { _c1Tags = a })

-- | Indicates whether Amazon EMR will lock the cluster to prevent the EC2
-- instances from being terminated by an API call or user intervention, or
-- in the event of a cluster error.
c1TerminationProtected :: Lens' Cluster (Maybe Bool)
c1TerminationProtected =
    lens _c1TerminationProtected (\s a -> s { _c1TerminationProtected = a })

-- | Indicates whether the job flow is visible to all IAM users of the AWS
-- account associated with the job flow. If this value is set to true, all
-- IAM users of that AWS account can view and manage the job flow if they
-- have the proper policy permissions set. If this value is false, only the
-- IAM user that created the cluster can view and manage it. This value can
-- be changed using the SetVisibleToAllUsers action.
c1VisibleToAllUsers :: Lens' Cluster (Maybe Bool)
c1VisibleToAllUsers =
    lens _c1VisibleToAllUsers (\s a -> s { _c1VisibleToAllUsers = a })

instance FromJSON Cluster where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON Cluster where
    toJSON = genericToJSON jsonOptions

data InstanceTimeline = InstanceTimeline
    { _itCreationDateTime :: Maybe RFC822
    , _itEndDateTime      :: Maybe RFC822
    , _itReadyDateTime    :: Maybe RFC822
    } deriving (Eq, Ord, Show, Generic)

-- | 'InstanceTimeline' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'itCreationDateTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'itEndDateTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'itReadyDateTime' @::@ 'Maybe' 'UTCTime'
--
instanceTimeline :: InstanceTimeline
instanceTimeline = InstanceTimeline
    { _itCreationDateTime = Nothing
    , _itReadyDateTime    = Nothing
    , _itEndDateTime      = Nothing
    }

-- | The creation date and time of the instance.
itCreationDateTime :: Lens' InstanceTimeline (Maybe UTCTime)
itCreationDateTime =
    lens _itCreationDateTime (\s a -> s { _itCreationDateTime = a })
        . mapping _Time

-- | The date and time when the instance was terminated.
itEndDateTime :: Lens' InstanceTimeline (Maybe UTCTime)
itEndDateTime = lens _itEndDateTime (\s a -> s { _itEndDateTime = a })
    . mapping _Time

-- | The date and time when the instance was ready to perform tasks.
itReadyDateTime :: Lens' InstanceTimeline (Maybe UTCTime)
itReadyDateTime = lens _itReadyDateTime (\s a -> s { _itReadyDateTime = a })
    . mapping _Time

instance FromJSON InstanceTimeline where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON InstanceTimeline where
    toJSON = genericToJSON jsonOptions

data Ec2InstanceAttributes = Ec2InstanceAttributes
    { _eiaEc2AvailabilityZone :: Maybe Text
    , _eiaEc2KeyName          :: Maybe Text
    , _eiaEc2SubnetId         :: Maybe Text
    , _eiaIamInstanceProfile  :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Ec2InstanceAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eiaEc2AvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'eiaEc2KeyName' @::@ 'Maybe' 'Text'
--
-- * 'eiaEc2SubnetId' @::@ 'Maybe' 'Text'
--
-- * 'eiaIamInstanceProfile' @::@ 'Maybe' 'Text'
--
ec2InstanceAttributes :: Ec2InstanceAttributes
ec2InstanceAttributes = Ec2InstanceAttributes
    { _eiaEc2KeyName          = Nothing
    , _eiaEc2SubnetId         = Nothing
    , _eiaEc2AvailabilityZone = Nothing
    , _eiaIamInstanceProfile  = Nothing
    }

-- | The Availability Zone in which the cluster will run.
eiaEc2AvailabilityZone :: Lens' Ec2InstanceAttributes (Maybe Text)
eiaEc2AvailabilityZone =
    lens _eiaEc2AvailabilityZone (\s a -> s { _eiaEc2AvailabilityZone = a })

-- | The name of the Amazon EC2 key pair to use when connecting with SSH into
-- the master node as a user named "hadoop".
eiaEc2KeyName :: Lens' Ec2InstanceAttributes (Maybe Text)
eiaEc2KeyName = lens _eiaEc2KeyName (\s a -> s { _eiaEc2KeyName = a })

-- | To launch the job flow in Amazon VPC, set this parameter to the
-- identifier of the Amazon VPC subnet where you want the job flow to
-- launch. If you do not specify this value, the job flow is launched in the
-- normal AWS cloud, outside of a VPC. Amazon VPC currently does not support
-- cluster compute quadruple extra large (cc1.4xlarge) instances. Thus, you
-- cannot specify the cc1.4xlarge instance type for nodes of a job flow
-- launched in a VPC.
eiaEc2SubnetId :: Lens' Ec2InstanceAttributes (Maybe Text)
eiaEc2SubnetId = lens _eiaEc2SubnetId (\s a -> s { _eiaEc2SubnetId = a })

-- | The IAM role that was specified when the job flow was launched. The EC2
-- instances of the job flow assume this role.
eiaIamInstanceProfile :: Lens' Ec2InstanceAttributes (Maybe Text)
eiaIamInstanceProfile =
    lens _eiaIamInstanceProfile (\s a -> s { _eiaIamInstanceProfile = a })

instance FromJSON Ec2InstanceAttributes where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON Ec2InstanceAttributes where
    toJSON = genericToJSON jsonOptions

data StepStateChangeReasonCode
    = None -- ^ NONE
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable StepStateChangeReasonCode

instance FromText StepStateChangeReasonCode where
    parser = match "NONE" None

instance ToText StepStateChangeReasonCode where
    toText None = "NONE"

instance FromJSON StepStateChangeReasonCode where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON StepStateChangeReasonCode where
    toJSON = genericToJSON jsonOptions

data ClusterState
    = CSBootstrapping        -- ^ BOOTSTRAPPING
    | CSRunning              -- ^ RUNNING
    | CSStarting             -- ^ STARTING
    | CSTerminated           -- ^ TERMINATED
    | CSTerminatedWithErrors -- ^ TERMINATED_WITH_ERRORS
    | CSTerminating          -- ^ TERMINATING
    | CSWaiting              -- ^ WAITING
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ClusterState

instance FromText ClusterState where
    parser = match "BOOTSTRAPPING"          CSBootstrapping
         <|> match "RUNNING"                CSRunning
         <|> match "STARTING"               CSStarting
         <|> match "TERMINATED"             CSTerminated
         <|> match "TERMINATED_WITH_ERRORS" CSTerminatedWithErrors
         <|> match "TERMINATING"            CSTerminating
         <|> match "WAITING"                CSWaiting

instance ToText ClusterState where
    toText = \case
        CSBootstrapping        -> "BOOTSTRAPPING"
        CSRunning              -> "RUNNING"
        CSStarting             -> "STARTING"
        CSTerminated           -> "TERMINATED"
        CSTerminatedWithErrors -> "TERMINATED_WITH_ERRORS"
        CSTerminating          -> "TERMINATING"
        CSWaiting              -> "WAITING"

instance FromJSON ClusterState where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON ClusterState where
    toJSON = genericToJSON jsonOptions

data HadoopStepConfig = HadoopStepConfig
    { _hscArgs       :: [Text]
    , _hscJar        :: Maybe Text
    , _hscMainClass  :: Maybe Text
    , _hscProperties :: Map Text Text
    } deriving (Eq, Show, Generic)

-- | 'HadoopStepConfig' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hscArgs' @::@ ['Text']
--
-- * 'hscJar' @::@ 'Maybe' 'Text'
--
-- * 'hscMainClass' @::@ 'Maybe' 'Text'
--
-- * 'hscProperties' @::@ 'HashMap' 'Text' 'Text'
--
hadoopStepConfig :: HadoopStepConfig
hadoopStepConfig = HadoopStepConfig
    { _hscJar        = Nothing
    , _hscProperties = mempty
    , _hscMainClass  = Nothing
    , _hscArgs       = mempty
    }

-- | The list of command line arguments to pass to the JAR file's main
-- function for execution.
hscArgs :: Lens' HadoopStepConfig [Text]
hscArgs = lens _hscArgs (\s a -> s { _hscArgs = a })

-- | The path to the JAR file that runs during the step.
hscJar :: Lens' HadoopStepConfig (Maybe Text)
hscJar = lens _hscJar (\s a -> s { _hscJar = a })

-- | The name of the main class in the specified Java file. If not specified,
-- the JAR file should specify a main class in its manifest file.
hscMainClass :: Lens' HadoopStepConfig (Maybe Text)
hscMainClass = lens _hscMainClass (\s a -> s { _hscMainClass = a })

-- | The list of Java properties that are set when the step runs. You can use
-- these properties to pass key value pairs to your main function.
hscProperties :: Lens' HadoopStepConfig (HashMap Text Text)
hscProperties = lens _hscProperties (\s a -> s { _hscProperties = a })
    . _Map

instance FromJSON HadoopStepConfig where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON HadoopStepConfig where
    toJSON = genericToJSON jsonOptions

data JobFlowExecutionState
    = JFESBootstrapping -- ^ BOOTSTRAPPING
    | JFESCompleted     -- ^ COMPLETED
    | JFESFailed        -- ^ FAILED
    | JFESRunning       -- ^ RUNNING
    | JFESShuttingDown  -- ^ SHUTTING_DOWN
    | JFESStarting      -- ^ STARTING
    | JFESTerminated    -- ^ TERMINATED
    | JFESWaiting       -- ^ WAITING
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable JobFlowExecutionState

instance FromText JobFlowExecutionState where
    parser = match "BOOTSTRAPPING" JFESBootstrapping
         <|> match "COMPLETED"     JFESCompleted
         <|> match "FAILED"        JFESFailed
         <|> match "RUNNING"       JFESRunning
         <|> match "SHUTTING_DOWN" JFESShuttingDown
         <|> match "STARTING"      JFESStarting
         <|> match "TERMINATED"    JFESTerminated
         <|> match "WAITING"       JFESWaiting

instance ToText JobFlowExecutionState where
    toText = \case
        JFESBootstrapping -> "BOOTSTRAPPING"
        JFESCompleted     -> "COMPLETED"
        JFESFailed        -> "FAILED"
        JFESRunning       -> "RUNNING"
        JFESShuttingDown  -> "SHUTTING_DOWN"
        JFESStarting      -> "STARTING"
        JFESTerminated    -> "TERMINATED"
        JFESWaiting       -> "WAITING"

instance FromJSON JobFlowExecutionState where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON JobFlowExecutionState where
    toJSON = genericToJSON jsonOptions

data StepDetail = StepDetail
    { _sdExecutionStatusDetail :: StepExecutionStatusDetail
    , _sdStepConfig            :: StepConfig
    } deriving (Eq, Show, Generic)

-- | 'StepDetail' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sdExecutionStatusDetail' @::@ 'StepExecutionStatusDetail'
--
-- * 'sdStepConfig' @::@ 'StepConfig'
--
stepDetail :: StepConfig -- ^ 'sdStepConfig'
           -> StepExecutionStatusDetail -- ^ 'sdExecutionStatusDetail'
           -> StepDetail
stepDetail p1 p2 = StepDetail
    { _sdStepConfig            = p1
    , _sdExecutionStatusDetail = p2
    }

-- | The description of the step status.
sdExecutionStatusDetail :: Lens' StepDetail StepExecutionStatusDetail
sdExecutionStatusDetail =
    lens _sdExecutionStatusDetail (\s a -> s { _sdExecutionStatusDetail = a })

-- | The step configuration.
sdStepConfig :: Lens' StepDetail StepConfig
sdStepConfig = lens _sdStepConfig (\s a -> s { _sdStepConfig = a })

instance FromJSON StepDetail where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON StepDetail where
    toJSON = genericToJSON jsonOptions

data InstanceGroupStateChangeReason = InstanceGroupStateChangeReason
    { _igscrCode    :: Maybe Text
    , _igscrMessage :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'InstanceGroupStateChangeReason' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'igscrCode' @::@ 'Maybe' 'Text'
--
-- * 'igscrMessage' @::@ 'Maybe' 'Text'
--
instanceGroupStateChangeReason :: InstanceGroupStateChangeReason
instanceGroupStateChangeReason = InstanceGroupStateChangeReason
    { _igscrCode    = Nothing
    , _igscrMessage = Nothing
    }

-- | The programmable code for the state change reason.
igscrCode :: Lens' InstanceGroupStateChangeReason (Maybe Text)
igscrCode = lens _igscrCode (\s a -> s { _igscrCode = a })

-- | The status change reason description.
igscrMessage :: Lens' InstanceGroupStateChangeReason (Maybe Text)
igscrMessage = lens _igscrMessage (\s a -> s { _igscrMessage = a })

instance FromJSON InstanceGroupStateChangeReason where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON InstanceGroupStateChangeReason where
    toJSON = genericToJSON jsonOptions

data InstanceGroupType
    = Core   -- ^ CORE
    | Master -- ^ MASTER
    | Task   -- ^ TASK
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable InstanceGroupType

instance FromText InstanceGroupType where
    parser = match "CORE"   Core
         <|> match "MASTER" Master
         <|> match "TASK"   Task

instance ToText InstanceGroupType where
    toText = \case
        Core   -> "CORE"
        Master -> "MASTER"
        Task   -> "TASK"

instance FromJSON InstanceGroupType where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON InstanceGroupType where
    toJSON = genericToJSON jsonOptions

data InstanceGroupStateChangeReasonCode
    = ClusterTerminated -- ^ CLUSTER_TERMINATED
    | InstanceFailure   -- ^ INSTANCE_FAILURE
    | InternalError     -- ^ INTERNAL_ERROR
    | ValidationError   -- ^ VALIDATION_ERROR
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable InstanceGroupStateChangeReasonCode

instance FromText InstanceGroupStateChangeReasonCode where
    parser = match "CLUSTER_TERMINATED" ClusterTerminated
         <|> match "INSTANCE_FAILURE"   InstanceFailure
         <|> match "INTERNAL_ERROR"     InternalError
         <|> match "VALIDATION_ERROR"   ValidationError

instance ToText InstanceGroupStateChangeReasonCode where
    toText = \case
        ClusterTerminated -> "CLUSTER_TERMINATED"
        InstanceFailure   -> "INSTANCE_FAILURE"
        InternalError     -> "INTERNAL_ERROR"
        ValidationError   -> "VALIDATION_ERROR"

instance FromJSON InstanceGroupStateChangeReasonCode where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON InstanceGroupStateChangeReasonCode where
    toJSON = genericToJSON jsonOptions

data StepStatus = StepStatus
    { _ssState             :: Maybe Text
    , _ssStateChangeReason :: Maybe StepStateChangeReason
    , _ssTimeline          :: Maybe StepTimeline
    } deriving (Eq, Show, Generic)

-- | 'StepStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ssState' @::@ 'Maybe' 'Text'
--
-- * 'ssStateChangeReason' @::@ 'Maybe' 'StepStateChangeReason'
--
-- * 'ssTimeline' @::@ 'Maybe' 'StepTimeline'
--
stepStatus :: StepStatus
stepStatus = StepStatus
    { _ssState             = Nothing
    , _ssStateChangeReason = Nothing
    , _ssTimeline          = Nothing
    }

-- | The execution state of the cluster step.
ssState :: Lens' StepStatus (Maybe Text)
ssState = lens _ssState (\s a -> s { _ssState = a })

-- | The reason for the step execution status change.
ssStateChangeReason :: Lens' StepStatus (Maybe StepStateChangeReason)
ssStateChangeReason =
    lens _ssStateChangeReason (\s a -> s { _ssStateChangeReason = a })

-- | The timeline of the cluster step status over time.
ssTimeline :: Lens' StepStatus (Maybe StepTimeline)
ssTimeline = lens _ssTimeline (\s a -> s { _ssTimeline = a })

instance FromJSON StepStatus where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON StepStatus where
    toJSON = genericToJSON jsonOptions

data StepSummary = StepSummary
    { _ssId     :: Maybe Text
    , _ssName   :: Maybe Text
    , _ssStatus :: Maybe StepStatus
    } deriving (Eq, Show, Generic)

-- | 'StepSummary' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ssId' @::@ 'Maybe' 'Text'
--
-- * 'ssName' @::@ 'Maybe' 'Text'
--
-- * 'ssStatus' @::@ 'Maybe' 'StepStatus'
--
stepSummary :: StepSummary
stepSummary = StepSummary
    { _ssId     = Nothing
    , _ssName   = Nothing
    , _ssStatus = Nothing
    }

-- | The identifier of the cluster step.
ssId :: Lens' StepSummary (Maybe Text)
ssId = lens _ssId (\s a -> s { _ssId = a })

-- | The name of the cluster step.
ssName :: Lens' StepSummary (Maybe Text)
ssName = lens _ssName (\s a -> s { _ssName = a })

-- | The current execution status details of the cluster step.
ssStatus :: Lens' StepSummary (Maybe StepStatus)
ssStatus = lens _ssStatus (\s a -> s { _ssStatus = a })

instance FromJSON StepSummary where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON StepSummary where
    toJSON = genericToJSON jsonOptions

data InstanceGroupState
    = IGSArrested      -- ^ ARRESTED
    | IGSBootstrapping -- ^ BOOTSTRAPPING
    | IGSEnded         -- ^ ENDED
    | IGSProvisioning  -- ^ PROVISIONING
    | IGSResizing      -- ^ RESIZING
    | IGSRunning       -- ^ RUNNING
    | IGSShuttingDown  -- ^ SHUTTING_DOWN
    | IGSSuspended     -- ^ SUSPENDED
    | IGSTerminated    -- ^ TERMINATED
    | IGSTerminating   -- ^ TERMINATING
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable InstanceGroupState

instance FromText InstanceGroupState where
    parser = match "ARRESTED"      IGSArrested
         <|> match "BOOTSTRAPPING" IGSBootstrapping
         <|> match "ENDED"         IGSEnded
         <|> match "PROVISIONING"  IGSProvisioning
         <|> match "RESIZING"      IGSResizing
         <|> match "RUNNING"       IGSRunning
         <|> match "SHUTTING_DOWN" IGSShuttingDown
         <|> match "SUSPENDED"     IGSSuspended
         <|> match "TERMINATED"    IGSTerminated
         <|> match "TERMINATING"   IGSTerminating

instance ToText InstanceGroupState where
    toText = \case
        IGSArrested      -> "ARRESTED"
        IGSBootstrapping -> "BOOTSTRAPPING"
        IGSEnded         -> "ENDED"
        IGSProvisioning  -> "PROVISIONING"
        IGSResizing      -> "RESIZING"
        IGSRunning       -> "RUNNING"
        IGSShuttingDown  -> "SHUTTING_DOWN"
        IGSSuspended     -> "SUSPENDED"
        IGSTerminated    -> "TERMINATED"
        IGSTerminating   -> "TERMINATING"

instance FromJSON InstanceGroupState where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON InstanceGroupState where
    toJSON = genericToJSON jsonOptions

data StepTimeline = StepTimeline
    { _stCreationDateTime :: Maybe RFC822
    , _stEndDateTime      :: Maybe RFC822
    , _stStartDateTime    :: Maybe RFC822
    } deriving (Eq, Ord, Show, Generic)

-- | 'StepTimeline' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'stCreationDateTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'stEndDateTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'stStartDateTime' @::@ 'Maybe' 'UTCTime'
--
stepTimeline :: StepTimeline
stepTimeline = StepTimeline
    { _stCreationDateTime = Nothing
    , _stStartDateTime    = Nothing
    , _stEndDateTime      = Nothing
    }

-- | The date and time when the cluster step was created.
stCreationDateTime :: Lens' StepTimeline (Maybe UTCTime)
stCreationDateTime =
    lens _stCreationDateTime (\s a -> s { _stCreationDateTime = a })
        . mapping _Time

-- | The date and time when the cluster step execution completed or failed.
stEndDateTime :: Lens' StepTimeline (Maybe UTCTime)
stEndDateTime = lens _stEndDateTime (\s a -> s { _stEndDateTime = a })
    . mapping _Time

-- | The date and time when the cluster step execution started.
stStartDateTime :: Lens' StepTimeline (Maybe UTCTime)
stStartDateTime = lens _stStartDateTime (\s a -> s { _stStartDateTime = a })
    . mapping _Time

instance FromJSON StepTimeline where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON StepTimeline where
    toJSON = genericToJSON jsonOptions

newtype PlacementType = PlacementType
    { _ptAvailabilityZone :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'PlacementType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ptAvailabilityZone' @::@ 'Text'
--
placementType :: Text -- ^ 'ptAvailabilityZone'
              -> PlacementType
placementType p1 = PlacementType
    { _ptAvailabilityZone = p1
    }

-- | The Amazon EC2 Availability Zone for the job flow.
ptAvailabilityZone :: Lens' PlacementType Text
ptAvailabilityZone =
    lens _ptAvailabilityZone (\s a -> s { _ptAvailabilityZone = a })

instance FromJSON PlacementType where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON PlacementType where
    toJSON = genericToJSON jsonOptions

data HadoopJarStepConfig = HadoopJarStepConfig
    { _hjscArgs       :: [Text]
    , _hjscJar        :: Text
    , _hjscMainClass  :: Maybe Text
    , _hjscProperties :: [KeyValue]
    } deriving (Eq, Show, Generic)

-- | 'HadoopJarStepConfig' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hjscArgs' @::@ ['Text']
--
-- * 'hjscJar' @::@ 'Text'
--
-- * 'hjscMainClass' @::@ 'Maybe' 'Text'
--
-- * 'hjscProperties' @::@ ['KeyValue']
--
hadoopJarStepConfig :: Text -- ^ 'hjscJar'
                    -> HadoopJarStepConfig
hadoopJarStepConfig p1 = HadoopJarStepConfig
    { _hjscJar        = p1
    , _hjscProperties = mempty
    , _hjscMainClass  = Nothing
    , _hjscArgs       = mempty
    }

-- | A list of command line arguments passed to the JAR file's main function
-- when executed.
hjscArgs :: Lens' HadoopJarStepConfig [Text]
hjscArgs = lens _hjscArgs (\s a -> s { _hjscArgs = a })

-- | A path to a JAR file run during the step.
hjscJar :: Lens' HadoopJarStepConfig Text
hjscJar = lens _hjscJar (\s a -> s { _hjscJar = a })

-- | The name of the main class in the specified Java file. If not specified,
-- the JAR file should specify a Main-Class in its manifest file.
hjscMainClass :: Lens' HadoopJarStepConfig (Maybe Text)
hjscMainClass = lens _hjscMainClass (\s a -> s { _hjscMainClass = a })

-- | A list of Java properties that are set when the step runs. You can use
-- these properties to pass key value pairs to your main function.
hjscProperties :: Lens' HadoopJarStepConfig [KeyValue]
hjscProperties = lens _hjscProperties (\s a -> s { _hjscProperties = a })

instance FromJSON HadoopJarStepConfig where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON HadoopJarStepConfig where
    toJSON = genericToJSON jsonOptions

data InstanceGroupModifyConfig = InstanceGroupModifyConfig
    { _igmcEC2InstanceIdsToTerminate :: [Text]
    , _igmcInstanceCount             :: Maybe Int
    , _igmcInstanceGroupId           :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'InstanceGroupModifyConfig' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'igmcEC2InstanceIdsToTerminate' @::@ ['Text']
--
-- * 'igmcInstanceCount' @::@ 'Maybe' 'Int'
--
-- * 'igmcInstanceGroupId' @::@ 'Text'
--
instanceGroupModifyConfig :: Text -- ^ 'igmcInstanceGroupId'
                          -> InstanceGroupModifyConfig
instanceGroupModifyConfig p1 = InstanceGroupModifyConfig
    { _igmcInstanceGroupId           = p1
    , _igmcInstanceCount             = Nothing
    , _igmcEC2InstanceIdsToTerminate = mempty
    }

-- | The EC2 InstanceIds to terminate. For advanced users only. Once you
-- terminate the instances, the instance group will not return to its
-- original requested size.
igmcEC2InstanceIdsToTerminate :: Lens' InstanceGroupModifyConfig [Text]
igmcEC2InstanceIdsToTerminate =
    lens _igmcEC2InstanceIdsToTerminate
        (\s a -> s { _igmcEC2InstanceIdsToTerminate = a })

-- | Target size for the instance group.
igmcInstanceCount :: Lens' InstanceGroupModifyConfig (Maybe Int)
igmcInstanceCount =
    lens _igmcInstanceCount (\s a -> s { _igmcInstanceCount = a })

-- | Unique ID of the instance group to expand or shrink.
igmcInstanceGroupId :: Lens' InstanceGroupModifyConfig Text
igmcInstanceGroupId =
    lens _igmcInstanceGroupId (\s a -> s { _igmcInstanceGroupId = a })

instance FromJSON InstanceGroupModifyConfig where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON InstanceGroupModifyConfig where
    toJSON = genericToJSON jsonOptions

data InstanceGroupDetail = InstanceGroupDetail
    { _igdBidPrice              :: Maybe Text
    , _igdCreationDateTime      :: RFC822
    , _igdEndDateTime           :: Maybe RFC822
    , _igdInstanceGroupId       :: Maybe Text
    , _igdInstanceRequestCount  :: Int
    , _igdInstanceRole          :: Text
    , _igdInstanceRunningCount  :: Int
    , _igdInstanceType          :: Text
    , _igdLastStateChangeReason :: Maybe Text
    , _igdMarket                :: Text
    , _igdName                  :: Maybe Text
    , _igdReadyDateTime         :: Maybe RFC822
    , _igdStartDateTime         :: Maybe RFC822
    , _igdState                 :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'InstanceGroupDetail' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'igdBidPrice' @::@ 'Maybe' 'Text'
--
-- * 'igdCreationDateTime' @::@ 'UTCTime'
--
-- * 'igdEndDateTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'igdInstanceGroupId' @::@ 'Maybe' 'Text'
--
-- * 'igdInstanceRequestCount' @::@ 'Int'
--
-- * 'igdInstanceRole' @::@ 'Text'
--
-- * 'igdInstanceRunningCount' @::@ 'Int'
--
-- * 'igdInstanceType' @::@ 'Text'
--
-- * 'igdLastStateChangeReason' @::@ 'Maybe' 'Text'
--
-- * 'igdMarket' @::@ 'Text'
--
-- * 'igdName' @::@ 'Maybe' 'Text'
--
-- * 'igdReadyDateTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'igdStartDateTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'igdState' @::@ 'Text'
--
instanceGroupDetail :: Text -- ^ 'igdMarket'
                    -> Text -- ^ 'igdInstanceRole'
                    -> Text -- ^ 'igdInstanceType'
                    -> Int -- ^ 'igdInstanceRequestCount'
                    -> Int -- ^ 'igdInstanceRunningCount'
                    -> Text -- ^ 'igdState'
                    -> UTCTime -- ^ 'igdCreationDateTime'
                    -> InstanceGroupDetail
instanceGroupDetail p1 p2 p3 p4 p5 p6 p7 = InstanceGroupDetail
    { _igdMarket                = p1
    , _igdInstanceRole          = p2
    , _igdInstanceType          = p3
    , _igdInstanceRequestCount  = p4
    , _igdInstanceRunningCount  = p5
    , _igdState                 = p6
    , _igdCreationDateTime      = withIso _Time (const id) p7
    , _igdInstanceGroupId       = Nothing
    , _igdName                  = Nothing
    , _igdBidPrice              = Nothing
    , _igdLastStateChangeReason = Nothing
    , _igdStartDateTime         = Nothing
    , _igdReadyDateTime         = Nothing
    , _igdEndDateTime           = Nothing
    }

-- | Bid price for EC2 Instances when launching nodes as Spot Instances,
-- expressed in USD.
igdBidPrice :: Lens' InstanceGroupDetail (Maybe Text)
igdBidPrice = lens _igdBidPrice (\s a -> s { _igdBidPrice = a })

-- | The date/time the instance group was created.
igdCreationDateTime :: Lens' InstanceGroupDetail UTCTime
igdCreationDateTime =
    lens _igdCreationDateTime (\s a -> s { _igdCreationDateTime = a })
        . _Time

-- | The date/time the instance group was terminated.
igdEndDateTime :: Lens' InstanceGroupDetail (Maybe UTCTime)
igdEndDateTime = lens _igdEndDateTime (\s a -> s { _igdEndDateTime = a })
    . mapping _Time

-- | Unique identifier for the instance group.
igdInstanceGroupId :: Lens' InstanceGroupDetail (Maybe Text)
igdInstanceGroupId =
    lens _igdInstanceGroupId (\s a -> s { _igdInstanceGroupId = a })

-- | Target number of instances to run in the instance group.
igdInstanceRequestCount :: Lens' InstanceGroupDetail Int
igdInstanceRequestCount =
    lens _igdInstanceRequestCount (\s a -> s { _igdInstanceRequestCount = a })

-- | Instance group role in the cluster.
igdInstanceRole :: Lens' InstanceGroupDetail Text
igdInstanceRole = lens _igdInstanceRole (\s a -> s { _igdInstanceRole = a })

-- | Actual count of running instances.
igdInstanceRunningCount :: Lens' InstanceGroupDetail Int
igdInstanceRunningCount =
    lens _igdInstanceRunningCount (\s a -> s { _igdInstanceRunningCount = a })

-- | Amazon EC2 Instance type.
igdInstanceType :: Lens' InstanceGroupDetail Text
igdInstanceType = lens _igdInstanceType (\s a -> s { _igdInstanceType = a })

-- | Details regarding the state of the instance group.
igdLastStateChangeReason :: Lens' InstanceGroupDetail (Maybe Text)
igdLastStateChangeReason =
    lens _igdLastStateChangeReason
        (\s a -> s { _igdLastStateChangeReason = a })

-- | Market type of the Amazon EC2 instances used to create a cluster node.
igdMarket :: Lens' InstanceGroupDetail Text
igdMarket = lens _igdMarket (\s a -> s { _igdMarket = a })

-- | Friendly name for the instance group.
igdName :: Lens' InstanceGroupDetail (Maybe Text)
igdName = lens _igdName (\s a -> s { _igdName = a })

-- | The date/time the instance group was available to the cluster.
igdReadyDateTime :: Lens' InstanceGroupDetail (Maybe UTCTime)
igdReadyDateTime = lens _igdReadyDateTime (\s a -> s { _igdReadyDateTime = a })
    . mapping _Time

-- | The date/time the instance group was started.
igdStartDateTime :: Lens' InstanceGroupDetail (Maybe UTCTime)
igdStartDateTime = lens _igdStartDateTime (\s a -> s { _igdStartDateTime = a })
    . mapping _Time

-- | State of instance group. The following values are deprecated: STARTING,
-- TERMINATED, and FAILED.
igdState :: Lens' InstanceGroupDetail Text
igdState = lens _igdState (\s a -> s { _igdState = a })

instance FromJSON InstanceGroupDetail where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON InstanceGroupDetail where
    toJSON = genericToJSON jsonOptions

data StepStateChangeReason = StepStateChangeReason
    { _sscrCode    :: Maybe Text
    , _sscrMessage :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'StepStateChangeReason' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sscrCode' @::@ 'Maybe' 'Text'
--
-- * 'sscrMessage' @::@ 'Maybe' 'Text'
--
stepStateChangeReason :: StepStateChangeReason
stepStateChangeReason = StepStateChangeReason
    { _sscrCode    = Nothing
    , _sscrMessage = Nothing
    }

-- | The programmable code for the state change reason.
sscrCode :: Lens' StepStateChangeReason (Maybe Text)
sscrCode = lens _sscrCode (\s a -> s { _sscrCode = a })

-- | The descriptive message for the state change reason.
sscrMessage :: Lens' StepStateChangeReason (Maybe Text)
sscrMessage = lens _sscrMessage (\s a -> s { _sscrMessage = a })

instance FromJSON StepStateChangeReason where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON StepStateChangeReason where
    toJSON = genericToJSON jsonOptions

data ClusterStateChangeReasonCode
    = CSCRCAllStepsCompleted -- ^ ALL_STEPS_COMPLETED
    | CSCRCBootstrapFailure  -- ^ BOOTSTRAP_FAILURE
    | CSCRCInstanceFailure   -- ^ INSTANCE_FAILURE
    | CSCRCInternalError     -- ^ INTERNAL_ERROR
    | CSCRCStepFailure       -- ^ STEP_FAILURE
    | CSCRCUserRequest       -- ^ USER_REQUEST
    | CSCRCValidationError   -- ^ VALIDATION_ERROR
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ClusterStateChangeReasonCode

instance FromText ClusterStateChangeReasonCode where
    parser = match "ALL_STEPS_COMPLETED" CSCRCAllStepsCompleted
         <|> match "BOOTSTRAP_FAILURE"   CSCRCBootstrapFailure
         <|> match "INSTANCE_FAILURE"    CSCRCInstanceFailure
         <|> match "INTERNAL_ERROR"      CSCRCInternalError
         <|> match "STEP_FAILURE"        CSCRCStepFailure
         <|> match "USER_REQUEST"        CSCRCUserRequest
         <|> match "VALIDATION_ERROR"    CSCRCValidationError

instance ToText ClusterStateChangeReasonCode where
    toText = \case
        CSCRCAllStepsCompleted -> "ALL_STEPS_COMPLETED"
        CSCRCBootstrapFailure  -> "BOOTSTRAP_FAILURE"
        CSCRCInstanceFailure   -> "INSTANCE_FAILURE"
        CSCRCInternalError     -> "INTERNAL_ERROR"
        CSCRCStepFailure       -> "STEP_FAILURE"
        CSCRCUserRequest       -> "USER_REQUEST"
        CSCRCValidationError   -> "VALIDATION_ERROR"

instance FromJSON ClusterStateChangeReasonCode where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON ClusterStateChangeReasonCode where
    toJSON = genericToJSON jsonOptions

data Step = Step
    { _sActionOnFailure :: Maybe Text
    , _sConfig          :: Maybe HadoopStepConfig
    , _sId              :: Maybe Text
    , _sName            :: Maybe Text
    , _sStatus          :: Maybe StepStatus
    } deriving (Eq, Show, Generic)

-- | 'Step' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sActionOnFailure' @::@ 'Maybe' 'Text'
--
-- * 'sConfig' @::@ 'Maybe' 'HadoopStepConfig'
--
-- * 'sId' @::@ 'Maybe' 'Text'
--
-- * 'sName' @::@ 'Maybe' 'Text'
--
-- * 'sStatus' @::@ 'Maybe' 'StepStatus'
--
step :: Step
step = Step
    { _sId              = Nothing
    , _sName            = Nothing
    , _sConfig          = Nothing
    , _sActionOnFailure = Nothing
    , _sStatus          = Nothing
    }

-- | This specifies what action to take when the cluster step fails. Possible
-- values are TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE.
sActionOnFailure :: Lens' Step (Maybe Text)
sActionOnFailure = lens _sActionOnFailure (\s a -> s { _sActionOnFailure = a })

-- | The Hadoop job configuration of the cluster step.
sConfig :: Lens' Step (Maybe HadoopStepConfig)
sConfig = lens _sConfig (\s a -> s { _sConfig = a })

-- | The identifier of the cluster step.
sId :: Lens' Step (Maybe Text)
sId = lens _sId (\s a -> s { _sId = a })

-- | The name of the cluster step.
sName :: Lens' Step (Maybe Text)
sName = lens _sName (\s a -> s { _sName = a })

-- | The current execution status details of the cluster step.
sStatus :: Lens' Step (Maybe StepStatus)
sStatus = lens _sStatus (\s a -> s { _sStatus = a })

instance FromJSON Step where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON Step where
    toJSON = genericToJSON jsonOptions

data StepState
    = SSCancelled   -- ^ CANCELLED
    | SSCompleted   -- ^ COMPLETED
    | SSFailed      -- ^ FAILED
    | SSInterrupted -- ^ INTERRUPTED
    | SSPending     -- ^ PENDING
    | SSRunning     -- ^ RUNNING
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable StepState

instance FromText StepState where
    parser = match "CANCELLED"   SSCancelled
         <|> match "COMPLETED"   SSCompleted
         <|> match "FAILED"      SSFailed
         <|> match "INTERRUPTED" SSInterrupted
         <|> match "PENDING"     SSPending
         <|> match "RUNNING"     SSRunning

instance ToText StepState where
    toText = \case
        SSCancelled   -> "CANCELLED"
        SSCompleted   -> "COMPLETED"
        SSFailed      -> "FAILED"
        SSInterrupted -> "INTERRUPTED"
        SSPending     -> "PENDING"
        SSRunning     -> "RUNNING"

instance FromJSON StepState where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON StepState where
    toJSON = genericToJSON jsonOptions

data InstanceGroupTimeline = InstanceGroupTimeline
    { _igtCreationDateTime :: Maybe RFC822
    , _igtEndDateTime      :: Maybe RFC822
    , _igtReadyDateTime    :: Maybe RFC822
    } deriving (Eq, Ord, Show, Generic)

-- | 'InstanceGroupTimeline' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'igtCreationDateTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'igtEndDateTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'igtReadyDateTime' @::@ 'Maybe' 'UTCTime'
--
instanceGroupTimeline :: InstanceGroupTimeline
instanceGroupTimeline = InstanceGroupTimeline
    { _igtCreationDateTime = Nothing
    , _igtReadyDateTime    = Nothing
    , _igtEndDateTime      = Nothing
    }

-- | The creation date and time of the instance group.
igtCreationDateTime :: Lens' InstanceGroupTimeline (Maybe UTCTime)
igtCreationDateTime =
    lens _igtCreationDateTime (\s a -> s { _igtCreationDateTime = a })
        . mapping _Time

-- | The date and time when the instance group terminated.
igtEndDateTime :: Lens' InstanceGroupTimeline (Maybe UTCTime)
igtEndDateTime = lens _igtEndDateTime (\s a -> s { _igtEndDateTime = a })
    . mapping _Time

-- | The date and time when the instance group became ready to perform tasks.
igtReadyDateTime :: Lens' InstanceGroupTimeline (Maybe UTCTime)
igtReadyDateTime = lens _igtReadyDateTime (\s a -> s { _igtReadyDateTime = a })
    . mapping _Time

instance FromJSON InstanceGroupTimeline where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON InstanceGroupTimeline where
    toJSON = genericToJSON jsonOptions

newtype BootstrapActionDetail = BootstrapActionDetail
    { _badBootstrapActionConfig :: Maybe BootstrapActionConfig
    } deriving (Eq, Show, Generic)

-- | 'BootstrapActionDetail' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'badBootstrapActionConfig' @::@ 'Maybe' 'BootstrapActionConfig'
--
bootstrapActionDetail :: BootstrapActionDetail
bootstrapActionDetail = BootstrapActionDetail
    { _badBootstrapActionConfig = Nothing
    }

-- | A description of the bootstrap action.
badBootstrapActionConfig :: Lens' BootstrapActionDetail (Maybe BootstrapActionConfig)
badBootstrapActionConfig =
    lens _badBootstrapActionConfig
        (\s a -> s { _badBootstrapActionConfig = a })

instance FromJSON BootstrapActionDetail where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON BootstrapActionDetail where
    toJSON = genericToJSON jsonOptions

data StepExecutionStatusDetail = StepExecutionStatusDetail
    { _sesdCreationDateTime      :: RFC822
    , _sesdEndDateTime           :: Maybe RFC822
    , _sesdLastStateChangeReason :: Maybe Text
    , _sesdStartDateTime         :: Maybe RFC822
    , _sesdState                 :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'StepExecutionStatusDetail' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sesdCreationDateTime' @::@ 'UTCTime'
--
-- * 'sesdEndDateTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'sesdLastStateChangeReason' @::@ 'Maybe' 'Text'
--
-- * 'sesdStartDateTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'sesdState' @::@ 'Text'
--
stepExecutionStatusDetail :: Text -- ^ 'sesdState'
                          -> UTCTime -- ^ 'sesdCreationDateTime'
                          -> StepExecutionStatusDetail
stepExecutionStatusDetail p1 p2 = StepExecutionStatusDetail
    { _sesdState                 = p1
    , _sesdCreationDateTime      = withIso _Time (const id) p2
    , _sesdStartDateTime         = Nothing
    , _sesdEndDateTime           = Nothing
    , _sesdLastStateChangeReason = Nothing
    }

-- | The creation date and time of the step.
sesdCreationDateTime :: Lens' StepExecutionStatusDetail UTCTime
sesdCreationDateTime =
    lens _sesdCreationDateTime (\s a -> s { _sesdCreationDateTime = a })
        . _Time

-- | The completion date and time of the step.
sesdEndDateTime :: Lens' StepExecutionStatusDetail (Maybe UTCTime)
sesdEndDateTime = lens _sesdEndDateTime (\s a -> s { _sesdEndDateTime = a })
    . mapping _Time

-- | A description of the step's current state.
sesdLastStateChangeReason :: Lens' StepExecutionStatusDetail (Maybe Text)
sesdLastStateChangeReason =
    lens _sesdLastStateChangeReason
        (\s a -> s { _sesdLastStateChangeReason = a })

-- | The start date and time of the step.
sesdStartDateTime :: Lens' StepExecutionStatusDetail (Maybe UTCTime)
sesdStartDateTime =
    lens _sesdStartDateTime (\s a -> s { _sesdStartDateTime = a })
        . mapping _Time

-- | The state of the job flow step.
sesdState :: Lens' StepExecutionStatusDetail Text
sesdState = lens _sesdState (\s a -> s { _sesdState = a })

instance FromJSON StepExecutionStatusDetail where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON StepExecutionStatusDetail where
    toJSON = genericToJSON jsonOptions

data InstanceStatus = InstanceStatus
    { _isState             :: Maybe Text
    , _isStateChangeReason :: Maybe InstanceStateChangeReason
    , _isTimeline          :: Maybe InstanceTimeline
    } deriving (Eq, Show, Generic)

-- | 'InstanceStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'isState' @::@ 'Maybe' 'Text'
--
-- * 'isStateChangeReason' @::@ 'Maybe' 'InstanceStateChangeReason'
--
-- * 'isTimeline' @::@ 'Maybe' 'InstanceTimeline'
--
instanceStatus :: InstanceStatus
instanceStatus = InstanceStatus
    { _isState             = Nothing
    , _isStateChangeReason = Nothing
    , _isTimeline          = Nothing
    }

-- | The current state of the instance.
isState :: Lens' InstanceStatus (Maybe Text)
isState = lens _isState (\s a -> s { _isState = a })

-- | The details of the status change reason for the instance.
isStateChangeReason :: Lens' InstanceStatus (Maybe InstanceStateChangeReason)
isStateChangeReason =
    lens _isStateChangeReason (\s a -> s { _isStateChangeReason = a })

-- | The timeline of the instance status over time.
isTimeline :: Lens' InstanceStatus (Maybe InstanceTimeline)
isTimeline = lens _isTimeline (\s a -> s { _isTimeline = a })

instance FromJSON InstanceStatus where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON InstanceStatus where
    toJSON = genericToJSON jsonOptions

data InstanceRoleType
    = IRTCore   -- ^ CORE
    | IRTMaster -- ^ MASTER
    | IRTTask   -- ^ TASK
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable InstanceRoleType

instance FromText InstanceRoleType where
    parser = match "CORE"   IRTCore
         <|> match "MASTER" IRTMaster
         <|> match "TASK"   IRTTask

instance ToText InstanceRoleType where
    toText = \case
        IRTCore   -> "CORE"
        IRTMaster -> "MASTER"
        IRTTask   -> "TASK"

instance FromJSON InstanceRoleType where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON InstanceRoleType where
    toJSON = genericToJSON jsonOptions

data JobFlowInstancesConfig = JobFlowInstancesConfig
    { _jficEc2KeyName                  :: Maybe Text
    , _jficEc2SubnetId                 :: Maybe Text
    , _jficHadoopVersion               :: Maybe Text
    , _jficInstanceCount               :: Maybe Int
    , _jficInstanceGroups              :: [InstanceGroupConfig]
    , _jficKeepJobFlowAliveWhenNoSteps :: Maybe Bool
    , _jficMasterInstanceType          :: Maybe Text
    , _jficPlacement                   :: Maybe PlacementType
    , _jficSlaveInstanceType           :: Maybe Text
    , _jficTerminationProtected        :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | 'JobFlowInstancesConfig' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'jficEc2KeyName' @::@ 'Maybe' 'Text'
--
-- * 'jficEc2SubnetId' @::@ 'Maybe' 'Text'
--
-- * 'jficHadoopVersion' @::@ 'Maybe' 'Text'
--
-- * 'jficInstanceCount' @::@ 'Maybe' 'Int'
--
-- * 'jficInstanceGroups' @::@ ['InstanceGroupConfig']
--
-- * 'jficKeepJobFlowAliveWhenNoSteps' @::@ 'Maybe' 'Bool'
--
-- * 'jficMasterInstanceType' @::@ 'Maybe' 'Text'
--
-- * 'jficPlacement' @::@ 'Maybe' 'PlacementType'
--
-- * 'jficSlaveInstanceType' @::@ 'Maybe' 'Text'
--
-- * 'jficTerminationProtected' @::@ 'Maybe' 'Bool'
--
jobFlowInstancesConfig :: JobFlowInstancesConfig
jobFlowInstancesConfig = JobFlowInstancesConfig
    { _jficMasterInstanceType          = Nothing
    , _jficSlaveInstanceType           = Nothing
    , _jficInstanceCount               = Nothing
    , _jficInstanceGroups              = mempty
    , _jficEc2KeyName                  = Nothing
    , _jficPlacement                   = Nothing
    , _jficKeepJobFlowAliveWhenNoSteps = Nothing
    , _jficTerminationProtected        = Nothing
    , _jficHadoopVersion               = Nothing
    , _jficEc2SubnetId                 = Nothing
    }

-- | The name of the Amazon EC2 key pair that can be used to ssh to the master
-- node as the user called "hadoop.".
jficEc2KeyName :: Lens' JobFlowInstancesConfig (Maybe Text)
jficEc2KeyName = lens _jficEc2KeyName (\s a -> s { _jficEc2KeyName = a })

-- | To launch the job flow in Amazon Virtual Private Cloud (Amazon VPC), set
-- this parameter to the identifier of the Amazon VPC subnet where you want
-- the job flow to launch. If you do not specify this value, the job flow is
-- launched in the normal Amazon Web Services cloud, outside of an Amazon
-- VPC. Amazon VPC currently does not support cluster compute quadruple
-- extra large (cc1.4xlarge) instances. Thus you cannot specify the
-- cc1.4xlarge instance type for nodes of a job flow launched in a Amazon
-- VPC.
jficEc2SubnetId :: Lens' JobFlowInstancesConfig (Maybe Text)
jficEc2SubnetId = lens _jficEc2SubnetId (\s a -> s { _jficEc2SubnetId = a })

-- | The Hadoop version for the job flow. Valid inputs are "0.18", "0.20", or
-- "0.20.205". If you do not set this value, the default of 0.18 is used,
-- unless the AmiVersion parameter is set in the RunJobFlow call, in which
-- case the default version of Hadoop for that AMI version is used.
jficHadoopVersion :: Lens' JobFlowInstancesConfig (Maybe Text)
jficHadoopVersion =
    lens _jficHadoopVersion (\s a -> s { _jficHadoopVersion = a })

-- | The number of Amazon EC2 instances used to execute the job flow.
jficInstanceCount :: Lens' JobFlowInstancesConfig (Maybe Int)
jficInstanceCount =
    lens _jficInstanceCount (\s a -> s { _jficInstanceCount = a })

-- | Configuration for the job flow's instance groups.
jficInstanceGroups :: Lens' JobFlowInstancesConfig [InstanceGroupConfig]
jficInstanceGroups =
    lens _jficInstanceGroups (\s a -> s { _jficInstanceGroups = a })

-- | Specifies whether the job flow should terminate after completing all
-- steps.
jficKeepJobFlowAliveWhenNoSteps :: Lens' JobFlowInstancesConfig (Maybe Bool)
jficKeepJobFlowAliveWhenNoSteps =
    lens _jficKeepJobFlowAliveWhenNoSteps
        (\s a -> s { _jficKeepJobFlowAliveWhenNoSteps = a })

-- | The EC2 instance type of the master node.
jficMasterInstanceType :: Lens' JobFlowInstancesConfig (Maybe Text)
jficMasterInstanceType =
    lens _jficMasterInstanceType (\s a -> s { _jficMasterInstanceType = a })

-- | The Availability Zone the job flow will run in.
jficPlacement :: Lens' JobFlowInstancesConfig (Maybe PlacementType)
jficPlacement = lens _jficPlacement (\s a -> s { _jficPlacement = a })

-- | The EC2 instance type of the slave nodes.
jficSlaveInstanceType :: Lens' JobFlowInstancesConfig (Maybe Text)
jficSlaveInstanceType =
    lens _jficSlaveInstanceType (\s a -> s { _jficSlaveInstanceType = a })

-- | Specifies whether to lock the job flow to prevent the Amazon EC2
-- instances from being terminated by API call, user intervention, or in the
-- event of a job flow error.
jficTerminationProtected :: Lens' JobFlowInstancesConfig (Maybe Bool)
jficTerminationProtected =
    lens _jficTerminationProtected
        (\s a -> s { _jficTerminationProtected = a })

instance FromJSON JobFlowInstancesConfig where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON JobFlowInstancesConfig where
    toJSON = genericToJSON jsonOptions

data StepConfig = StepConfig
    { _scActionOnFailure :: Maybe Text
    , _scHadoopJarStep   :: HadoopJarStepConfig
    , _scName            :: Text
    } deriving (Eq, Show, Generic)

-- | 'StepConfig' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'scActionOnFailure' @::@ 'Maybe' 'Text'
--
-- * 'scHadoopJarStep' @::@ 'HadoopJarStepConfig'
--
-- * 'scName' @::@ 'Text'
--
stepConfig :: Text -- ^ 'scName'
           -> HadoopJarStepConfig -- ^ 'scHadoopJarStep'
           -> StepConfig
stepConfig p1 p2 = StepConfig
    { _scName            = p1
    , _scHadoopJarStep   = p2
    , _scActionOnFailure = Nothing
    }

-- | The action to take if the job flow step fails.
scActionOnFailure :: Lens' StepConfig (Maybe Text)
scActionOnFailure =
    lens _scActionOnFailure (\s a -> s { _scActionOnFailure = a })

-- | The JAR file used for the job flow step.
scHadoopJarStep :: Lens' StepConfig HadoopJarStepConfig
scHadoopJarStep = lens _scHadoopJarStep (\s a -> s { _scHadoopJarStep = a })

-- | The name of the job flow step.
scName :: Lens' StepConfig Text
scName = lens _scName (\s a -> s { _scName = a })

instance FromJSON StepConfig where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON StepConfig where
    toJSON = genericToJSON jsonOptions

data InstanceGroup = InstanceGroup
    { _igBidPrice               :: Maybe Text
    , _igId                     :: Maybe Text
    , _igInstanceGroupType      :: Maybe Text
    , _igInstanceType           :: Maybe Text
    , _igMarket                 :: Maybe Text
    , _igName                   :: Maybe Text
    , _igRequestedInstanceCount :: Maybe Int
    , _igRunningInstanceCount   :: Maybe Int
    , _igStatus                 :: Maybe InstanceGroupStatus
    } deriving (Eq, Show, Generic)

-- | 'InstanceGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'igBidPrice' @::@ 'Maybe' 'Text'
--
-- * 'igId' @::@ 'Maybe' 'Text'
--
-- * 'igInstanceGroupType' @::@ 'Maybe' 'Text'
--
-- * 'igInstanceType' @::@ 'Maybe' 'Text'
--
-- * 'igMarket' @::@ 'Maybe' 'Text'
--
-- * 'igName' @::@ 'Maybe' 'Text'
--
-- * 'igRequestedInstanceCount' @::@ 'Maybe' 'Int'
--
-- * 'igRunningInstanceCount' @::@ 'Maybe' 'Int'
--
-- * 'igStatus' @::@ 'Maybe' 'InstanceGroupStatus'
--
instanceGroup :: InstanceGroup
instanceGroup = InstanceGroup
    { _igId                     = Nothing
    , _igName                   = Nothing
    , _igMarket                 = Nothing
    , _igInstanceGroupType      = Nothing
    , _igBidPrice               = Nothing
    , _igInstanceType           = Nothing
    , _igRequestedInstanceCount = Nothing
    , _igRunningInstanceCount   = Nothing
    , _igStatus                 = Nothing
    }

-- | The bid price for each EC2 instance in the instance group when launching
-- nodes as Spot Instances, expressed in USD.
igBidPrice :: Lens' InstanceGroup (Maybe Text)
igBidPrice = lens _igBidPrice (\s a -> s { _igBidPrice = a })

-- | The identifier of the instance group.
igId :: Lens' InstanceGroup (Maybe Text)
igId = lens _igId (\s a -> s { _igId = a })

-- | The type of the instance group. Valid values are MASTER, CORE or TASK.
igInstanceGroupType :: Lens' InstanceGroup (Maybe Text)
igInstanceGroupType =
    lens _igInstanceGroupType (\s a -> s { _igInstanceGroupType = a })

-- | The EC2 instance type for all instances in the instance group.
igInstanceType :: Lens' InstanceGroup (Maybe Text)
igInstanceType = lens _igInstanceType (\s a -> s { _igInstanceType = a })

-- | The marketplace to provision instances for this group. Valid values are
-- ON_DEMAND or SPOT.
igMarket :: Lens' InstanceGroup (Maybe Text)
igMarket = lens _igMarket (\s a -> s { _igMarket = a })

-- | The name of the instance group.
igName :: Lens' InstanceGroup (Maybe Text)
igName = lens _igName (\s a -> s { _igName = a })

-- | The target number of instances for the instance group.
igRequestedInstanceCount :: Lens' InstanceGroup (Maybe Int)
igRequestedInstanceCount =
    lens _igRequestedInstanceCount
        (\s a -> s { _igRequestedInstanceCount = a })

-- | The number of instances currently running in this instance group.
igRunningInstanceCount :: Lens' InstanceGroup (Maybe Int)
igRunningInstanceCount =
    lens _igRunningInstanceCount (\s a -> s { _igRunningInstanceCount = a })

-- | The current status of the instance group.
igStatus :: Lens' InstanceGroup (Maybe InstanceGroupStatus)
igStatus = lens _igStatus (\s a -> s { _igStatus = a })

instance FromJSON InstanceGroup where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON InstanceGroup where
    toJSON = genericToJSON jsonOptions

data BootstrapActionConfig = BootstrapActionConfig
    { _bacName                  :: Text
    , _bacScriptBootstrapAction :: ScriptBootstrapActionConfig
    } deriving (Eq, Show, Generic)

-- | 'BootstrapActionConfig' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bacName' @::@ 'Text'
--
-- * 'bacScriptBootstrapAction' @::@ 'ScriptBootstrapActionConfig'
--
bootstrapActionConfig :: Text -- ^ 'bacName'
                      -> ScriptBootstrapActionConfig -- ^ 'bacScriptBootstrapAction'
                      -> BootstrapActionConfig
bootstrapActionConfig p1 p2 = BootstrapActionConfig
    { _bacName                  = p1
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

instance FromJSON BootstrapActionConfig where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON BootstrapActionConfig where
    toJSON = genericToJSON jsonOptions

data ClusterSummary = ClusterSummary
    { _csId     :: Maybe Text
    , _csName   :: Maybe Text
    , _csStatus :: Maybe ClusterStatus
    } deriving (Eq, Show, Generic)

-- | 'ClusterSummary' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csId' @::@ 'Maybe' 'Text'
--
-- * 'csName' @::@ 'Maybe' 'Text'
--
-- * 'csStatus' @::@ 'Maybe' 'ClusterStatus'
--
clusterSummary :: ClusterSummary
clusterSummary = ClusterSummary
    { _csId     = Nothing
    , _csName   = Nothing
    , _csStatus = Nothing
    }

-- | The unique identifier for the cluster.
csId :: Lens' ClusterSummary (Maybe Text)
csId = lens _csId (\s a -> s { _csId = a })

-- | The name of the cluster.
csName :: Lens' ClusterSummary (Maybe Text)
csName = lens _csName (\s a -> s { _csName = a })

-- | The details about the current status of the cluster.
csStatus :: Lens' ClusterSummary (Maybe ClusterStatus)
csStatus = lens _csStatus (\s a -> s { _csStatus = a })

instance FromJSON ClusterSummary where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON ClusterSummary where
    toJSON = genericToJSON jsonOptions

data JobFlowInstancesDetail = JobFlowInstancesDetail
    { _jfidEc2KeyName                  :: Maybe Text
    , _jfidEc2SubnetId                 :: Maybe Text
    , _jfidHadoopVersion               :: Maybe Text
    , _jfidInstanceCount               :: Int
    , _jfidInstanceGroups              :: [InstanceGroupDetail]
    , _jfidKeepJobFlowAliveWhenNoSteps :: Maybe Bool
    , _jfidMasterInstanceId            :: Maybe Text
    , _jfidMasterInstanceType          :: Text
    , _jfidMasterPublicDnsName         :: Maybe Text
    , _jfidNormalizedInstanceHours     :: Maybe Int
    , _jfidPlacement                   :: Maybe PlacementType
    , _jfidSlaveInstanceType           :: Text
    , _jfidTerminationProtected        :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | 'JobFlowInstancesDetail' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'jfidEc2KeyName' @::@ 'Maybe' 'Text'
--
-- * 'jfidEc2SubnetId' @::@ 'Maybe' 'Text'
--
-- * 'jfidHadoopVersion' @::@ 'Maybe' 'Text'
--
-- * 'jfidInstanceCount' @::@ 'Int'
--
-- * 'jfidInstanceGroups' @::@ ['InstanceGroupDetail']
--
-- * 'jfidKeepJobFlowAliveWhenNoSteps' @::@ 'Maybe' 'Bool'
--
-- * 'jfidMasterInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'jfidMasterInstanceType' @::@ 'Text'
--
-- * 'jfidMasterPublicDnsName' @::@ 'Maybe' 'Text'
--
-- * 'jfidNormalizedInstanceHours' @::@ 'Maybe' 'Int'
--
-- * 'jfidPlacement' @::@ 'Maybe' 'PlacementType'
--
-- * 'jfidSlaveInstanceType' @::@ 'Text'
--
-- * 'jfidTerminationProtected' @::@ 'Maybe' 'Bool'
--
jobFlowInstancesDetail :: Text -- ^ 'jfidMasterInstanceType'
                       -> Text -- ^ 'jfidSlaveInstanceType'
                       -> Int -- ^ 'jfidInstanceCount'
                       -> JobFlowInstancesDetail
jobFlowInstancesDetail p1 p2 p3 = JobFlowInstancesDetail
    { _jfidMasterInstanceType          = p1
    , _jfidSlaveInstanceType           = p2
    , _jfidInstanceCount               = p3
    , _jfidMasterPublicDnsName         = Nothing
    , _jfidMasterInstanceId            = Nothing
    , _jfidInstanceGroups              = mempty
    , _jfidNormalizedInstanceHours     = Nothing
    , _jfidEc2KeyName                  = Nothing
    , _jfidEc2SubnetId                 = Nothing
    , _jfidPlacement                   = Nothing
    , _jfidKeepJobFlowAliveWhenNoSteps = Nothing
    , _jfidTerminationProtected        = Nothing
    , _jfidHadoopVersion               = Nothing
    }

-- | The name of an Amazon EC2 key pair that can be used to ssh to the master
-- node of job flow.
jfidEc2KeyName :: Lens' JobFlowInstancesDetail (Maybe Text)
jfidEc2KeyName = lens _jfidEc2KeyName (\s a -> s { _jfidEc2KeyName = a })

-- | For job flows launched within Amazon Virtual Private Cloud, this value
-- specifies the identifier of the subnet where the job flow was launched.
jfidEc2SubnetId :: Lens' JobFlowInstancesDetail (Maybe Text)
jfidEc2SubnetId = lens _jfidEc2SubnetId (\s a -> s { _jfidEc2SubnetId = a })

-- | The Hadoop version for the job flow.
jfidHadoopVersion :: Lens' JobFlowInstancesDetail (Maybe Text)
jfidHadoopVersion =
    lens _jfidHadoopVersion (\s a -> s { _jfidHadoopVersion = a })

-- | The number of Amazon EC2 instances in the cluster. If the value is 1, the
-- same instance serves as both the master and slave node. If the value is
-- greater than 1, one instance is the master node and all others are slave
-- nodes.
jfidInstanceCount :: Lens' JobFlowInstancesDetail Int
jfidInstanceCount =
    lens _jfidInstanceCount (\s a -> s { _jfidInstanceCount = a })

-- | Details about the job flow's instance groups.
jfidInstanceGroups :: Lens' JobFlowInstancesDetail [InstanceGroupDetail]
jfidInstanceGroups =
    lens _jfidInstanceGroups (\s a -> s { _jfidInstanceGroups = a })

-- | Specifies whether the job flow should terminate after completing all
-- steps.
jfidKeepJobFlowAliveWhenNoSteps :: Lens' JobFlowInstancesDetail (Maybe Bool)
jfidKeepJobFlowAliveWhenNoSteps =
    lens _jfidKeepJobFlowAliveWhenNoSteps
        (\s a -> s { _jfidKeepJobFlowAliveWhenNoSteps = a })

-- | The Amazon EC2 instance identifier of the master node.
jfidMasterInstanceId :: Lens' JobFlowInstancesDetail (Maybe Text)
jfidMasterInstanceId =
    lens _jfidMasterInstanceId (\s a -> s { _jfidMasterInstanceId = a })

-- | The Amazon EC2 master node instance type.
jfidMasterInstanceType :: Lens' JobFlowInstancesDetail Text
jfidMasterInstanceType =
    lens _jfidMasterInstanceType (\s a -> s { _jfidMasterInstanceType = a })

-- | The DNS name of the master node.
jfidMasterPublicDnsName :: Lens' JobFlowInstancesDetail (Maybe Text)
jfidMasterPublicDnsName =
    lens _jfidMasterPublicDnsName (\s a -> s { _jfidMasterPublicDnsName = a })

-- | An approximation of the cost of the job flow, represented in
-- m1.small/hours. This value is incremented once for every hour an m1.small
-- runs. Larger instances are weighted more, so an Amazon EC2 instance that
-- is roughly four times more expensive would result in the normalized
-- instance hours being incremented by four. This result is only an
-- approximation and does not reflect the actual billing rate.
jfidNormalizedInstanceHours :: Lens' JobFlowInstancesDetail (Maybe Int)
jfidNormalizedInstanceHours =
    lens _jfidNormalizedInstanceHours
        (\s a -> s { _jfidNormalizedInstanceHours = a })

-- | The Amazon EC2 Availability Zone for the job flow.
jfidPlacement :: Lens' JobFlowInstancesDetail (Maybe PlacementType)
jfidPlacement = lens _jfidPlacement (\s a -> s { _jfidPlacement = a })

-- | The Amazon EC2 slave node instance type.
jfidSlaveInstanceType :: Lens' JobFlowInstancesDetail Text
jfidSlaveInstanceType =
    lens _jfidSlaveInstanceType (\s a -> s { _jfidSlaveInstanceType = a })

-- | Specifies whether the Amazon EC2 instances in the cluster are protected
-- from termination by API calls, user intervention, or in the event of a
-- job flow error.
jfidTerminationProtected :: Lens' JobFlowInstancesDetail (Maybe Bool)
jfidTerminationProtected =
    lens _jfidTerminationProtected
        (\s a -> s { _jfidTerminationProtected = a })

instance FromJSON JobFlowInstancesDetail where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON JobFlowInstancesDetail where
    toJSON = genericToJSON jsonOptions

data ClusterStatus = ClusterStatus
    { _csState             :: Maybe Text
    , _csStateChangeReason :: Maybe ClusterStateChangeReason
    , _csTimeline          :: Maybe ClusterTimeline
    } deriving (Eq, Show, Generic)

-- | 'ClusterStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csState' @::@ 'Maybe' 'Text'
--
-- * 'csStateChangeReason' @::@ 'Maybe' 'ClusterStateChangeReason'
--
-- * 'csTimeline' @::@ 'Maybe' 'ClusterTimeline'
--
clusterStatus :: ClusterStatus
clusterStatus = ClusterStatus
    { _csState             = Nothing
    , _csStateChangeReason = Nothing
    , _csTimeline          = Nothing
    }

-- | The current state of the cluster.
csState :: Lens' ClusterStatus (Maybe Text)
csState = lens _csState (\s a -> s { _csState = a })

-- | The reason for the cluster status change.
csStateChangeReason :: Lens' ClusterStatus (Maybe ClusterStateChangeReason)
csStateChangeReason =
    lens _csStateChangeReason (\s a -> s { _csStateChangeReason = a })

-- | A timeline that represents the status of a cluster over the lifetime of
-- the cluster.
csTimeline :: Lens' ClusterStatus (Maybe ClusterTimeline)
csTimeline = lens _csTimeline (\s a -> s { _csTimeline = a })

instance FromJSON ClusterStatus where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON ClusterStatus where
    toJSON = genericToJSON jsonOptions

data InstanceState
    = ISAwaitingFulfillment -- ^ AWAITING_FULFILLMENT
    | ISBootstrapping       -- ^ BOOTSTRAPPING
    | ISProvisioning        -- ^ PROVISIONING
    | ISRunning             -- ^ RUNNING
    | ISTerminated          -- ^ TERMINATED
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable InstanceState

instance FromText InstanceState where
    parser = match "AWAITING_FULFILLMENT" ISAwaitingFulfillment
         <|> match "BOOTSTRAPPING"        ISBootstrapping
         <|> match "PROVISIONING"         ISProvisioning
         <|> match "RUNNING"              ISRunning
         <|> match "TERMINATED"           ISTerminated

instance ToText InstanceState where
    toText = \case
        ISAwaitingFulfillment -> "AWAITING_FULFILLMENT"
        ISBootstrapping       -> "BOOTSTRAPPING"
        ISProvisioning        -> "PROVISIONING"
        ISRunning             -> "RUNNING"
        ISTerminated          -> "TERMINATED"

instance FromJSON InstanceState where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON InstanceState where
    toJSON = genericToJSON jsonOptions

data ClusterTimeline = ClusterTimeline
    { _ctCreationDateTime :: Maybe RFC822
    , _ctEndDateTime      :: Maybe RFC822
    , _ctReadyDateTime    :: Maybe RFC822
    } deriving (Eq, Ord, Show, Generic)

-- | 'ClusterTimeline' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ctCreationDateTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'ctEndDateTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'ctReadyDateTime' @::@ 'Maybe' 'UTCTime'
--
clusterTimeline :: ClusterTimeline
clusterTimeline = ClusterTimeline
    { _ctCreationDateTime = Nothing
    , _ctReadyDateTime    = Nothing
    , _ctEndDateTime      = Nothing
    }

-- | The creation date and time of the cluster.
ctCreationDateTime :: Lens' ClusterTimeline (Maybe UTCTime)
ctCreationDateTime =
    lens _ctCreationDateTime (\s a -> s { _ctCreationDateTime = a })
        . mapping _Time

-- | The date and time when the cluster was terminated.
ctEndDateTime :: Lens' ClusterTimeline (Maybe UTCTime)
ctEndDateTime = lens _ctEndDateTime (\s a -> s { _ctEndDateTime = a })
    . mapping _Time

-- | The date and time when the cluster was ready to execute steps.
ctReadyDateTime :: Lens' ClusterTimeline (Maybe UTCTime)
ctReadyDateTime = lens _ctReadyDateTime (\s a -> s { _ctReadyDateTime = a })
    . mapping _Time

instance FromJSON ClusterTimeline where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON ClusterTimeline where
    toJSON = genericToJSON jsonOptions

data InstanceStateChangeReasonCode
    = ISCRCBootstrapFailure  -- ^ BOOTSTRAP_FAILURE
    | ISCRCClusterTerminated -- ^ CLUSTER_TERMINATED
    | ISCRCInstanceFailure   -- ^ INSTANCE_FAILURE
    | ISCRCInternalError     -- ^ INTERNAL_ERROR
    | ISCRCValidationError   -- ^ VALIDATION_ERROR
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable InstanceStateChangeReasonCode

instance FromText InstanceStateChangeReasonCode where
    parser = match "BOOTSTRAP_FAILURE"  ISCRCBootstrapFailure
         <|> match "CLUSTER_TERMINATED" ISCRCClusterTerminated
         <|> match "INSTANCE_FAILURE"   ISCRCInstanceFailure
         <|> match "INTERNAL_ERROR"     ISCRCInternalError
         <|> match "VALIDATION_ERROR"   ISCRCValidationError

instance ToText InstanceStateChangeReasonCode where
    toText = \case
        ISCRCBootstrapFailure  -> "BOOTSTRAP_FAILURE"
        ISCRCClusterTerminated -> "CLUSTER_TERMINATED"
        ISCRCInstanceFailure   -> "INSTANCE_FAILURE"
        ISCRCInternalError     -> "INTERNAL_ERROR"
        ISCRCValidationError   -> "VALIDATION_ERROR"

instance FromJSON InstanceStateChangeReasonCode where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON InstanceStateChangeReasonCode where
    toJSON = genericToJSON jsonOptions

data Instance = Instance
    { _iEc2InstanceId    :: Maybe Text
    , _iId               :: Maybe Text
    , _iPrivateDnsName   :: Maybe Text
    , _iPrivateIpAddress :: Maybe Text
    , _iPublicDnsName    :: Maybe Text
    , _iPublicIpAddress  :: Maybe Text
    , _iStatus           :: Maybe InstanceStatus
    } deriving (Eq, Show, Generic)

-- | 'Instance' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iEc2InstanceId' @::@ 'Maybe' 'Text'
--
-- * 'iId' @::@ 'Maybe' 'Text'
--
-- * 'iPrivateDnsName' @::@ 'Maybe' 'Text'
--
-- * 'iPrivateIpAddress' @::@ 'Maybe' 'Text'
--
-- * 'iPublicDnsName' @::@ 'Maybe' 'Text'
--
-- * 'iPublicIpAddress' @::@ 'Maybe' 'Text'
--
-- * 'iStatus' @::@ 'Maybe' 'InstanceStatus'
--
instance' :: Instance
instance' = Instance
    { _iId               = Nothing
    , _iEc2InstanceId    = Nothing
    , _iPublicDnsName    = Nothing
    , _iPublicIpAddress  = Nothing
    , _iPrivateDnsName   = Nothing
    , _iPrivateIpAddress = Nothing
    , _iStatus           = Nothing
    }

-- | The unique identifier of the instance in Amazon EC2.
iEc2InstanceId :: Lens' Instance (Maybe Text)
iEc2InstanceId = lens _iEc2InstanceId (\s a -> s { _iEc2InstanceId = a })

-- | The unique identifier for the instance in Amazon EMR.
iId :: Lens' Instance (Maybe Text)
iId = lens _iId (\s a -> s { _iId = a })

-- | The private DNS name of the instance.
iPrivateDnsName :: Lens' Instance (Maybe Text)
iPrivateDnsName = lens _iPrivateDnsName (\s a -> s { _iPrivateDnsName = a })

-- | The private IP address of the instance.
iPrivateIpAddress :: Lens' Instance (Maybe Text)
iPrivateIpAddress =
    lens _iPrivateIpAddress (\s a -> s { _iPrivateIpAddress = a })

-- | The public DNS name of the instance.
iPublicDnsName :: Lens' Instance (Maybe Text)
iPublicDnsName = lens _iPublicDnsName (\s a -> s { _iPublicDnsName = a })

-- | The public IP address of the instance.
iPublicIpAddress :: Lens' Instance (Maybe Text)
iPublicIpAddress = lens _iPublicIpAddress (\s a -> s { _iPublicIpAddress = a })

-- | The current status of the instance.
iStatus :: Lens' Instance (Maybe InstanceStatus)
iStatus = lens _iStatus (\s a -> s { _iStatus = a })

instance FromJSON Instance where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON Instance where
    toJSON = genericToJSON jsonOptions

data ScriptBootstrapActionConfig = ScriptBootstrapActionConfig
    { _sbacArgs :: [Text]
    , _sbacPath :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ScriptBootstrapActionConfig' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sbacArgs' @::@ ['Text']
--
-- * 'sbacPath' @::@ 'Text'
--
scriptBootstrapActionConfig :: Text -- ^ 'sbacPath'
                            -> ScriptBootstrapActionConfig
scriptBootstrapActionConfig p1 = ScriptBootstrapActionConfig
    { _sbacPath = p1
    , _sbacArgs = mempty
    }

-- | A list of command line arguments to pass to the bootstrap action script.
sbacArgs :: Lens' ScriptBootstrapActionConfig [Text]
sbacArgs = lens _sbacArgs (\s a -> s { _sbacArgs = a })

-- | Location of the script to run during a bootstrap action. Can be either a
-- location in Amazon S3 or on a local file system.
sbacPath :: Lens' ScriptBootstrapActionConfig Text
sbacPath = lens _sbacPath (\s a -> s { _sbacPath = a })

instance FromJSON ScriptBootstrapActionConfig where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON ScriptBootstrapActionConfig where
    toJSON = genericToJSON jsonOptions

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Batch.Types.Product where

import           Network.AWS.Batch.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude

-- | An object representing the details of a container that is part of a job attempt.
--
--
--
-- /See:/ 'attemptContainerDetail' smart constructor.
data AttemptContainerDetail = AttemptContainerDetail'
    { _acdTaskARN              :: !(Maybe Text)
    , _acdContainerInstanceARN :: !(Maybe Text)
    , _acdReason               :: !(Maybe Text)
    , _acdLogStreamName        :: !(Maybe Text)
    , _acdExitCode             :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AttemptContainerDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acdTaskARN' - The Amazon Resource Name (ARN) of the Amazon ECS task that is associated with the job attempt.
--
-- * 'acdContainerInstanceARN' - The Amazon Resource Name (ARN) of the Amazon ECS container instance that hosts the job attempt.
--
-- * 'acdReason' - A short (255 max characters) human-readable string to provide additional details about a running or stopped container.
--
-- * 'acdLogStreamName' - Undocumented member.
--
-- * 'acdExitCode' - The exit code for the job attempt. A non-zero exit code is considered a failure.
attemptContainerDetail
    :: AttemptContainerDetail
attemptContainerDetail =
    AttemptContainerDetail'
    { _acdTaskARN = Nothing
    , _acdContainerInstanceARN = Nothing
    , _acdReason = Nothing
    , _acdLogStreamName = Nothing
    , _acdExitCode = Nothing
    }

-- | The Amazon Resource Name (ARN) of the Amazon ECS task that is associated with the job attempt.
acdTaskARN :: Lens' AttemptContainerDetail (Maybe Text)
acdTaskARN = lens _acdTaskARN (\ s a -> s{_acdTaskARN = a});

-- | The Amazon Resource Name (ARN) of the Amazon ECS container instance that hosts the job attempt.
acdContainerInstanceARN :: Lens' AttemptContainerDetail (Maybe Text)
acdContainerInstanceARN = lens _acdContainerInstanceARN (\ s a -> s{_acdContainerInstanceARN = a});

-- | A short (255 max characters) human-readable string to provide additional details about a running or stopped container.
acdReason :: Lens' AttemptContainerDetail (Maybe Text)
acdReason = lens _acdReason (\ s a -> s{_acdReason = a});

-- | Undocumented member.
acdLogStreamName :: Lens' AttemptContainerDetail (Maybe Text)
acdLogStreamName = lens _acdLogStreamName (\ s a -> s{_acdLogStreamName = a});

-- | The exit code for the job attempt. A non-zero exit code is considered a failure.
acdExitCode :: Lens' AttemptContainerDetail (Maybe Int)
acdExitCode = lens _acdExitCode (\ s a -> s{_acdExitCode = a});

instance FromJSON AttemptContainerDetail where
        parseJSON
          = withObject "AttemptContainerDetail"
              (\ x ->
                 AttemptContainerDetail' <$>
                   (x .:? "taskArn") <*> (x .:? "containerInstanceArn")
                     <*> (x .:? "reason")
                     <*> (x .:? "logStreamName")
                     <*> (x .:? "exitCode"))

instance Hashable AttemptContainerDetail

instance NFData AttemptContainerDetail

-- | An object representing a job attempt.
--
--
--
-- /See:/ 'attemptDetail' smart constructor.
data AttemptDetail = AttemptDetail'
    { _adStoppedAt    :: !(Maybe Integer)
    , _adStartedAt    :: !(Maybe Integer)
    , _adContainer    :: !(Maybe AttemptContainerDetail)
    , _adStatusReason :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AttemptDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adStoppedAt' - The Unix timestamp for when the attempt was stopped (when the task transitioned from the @RUNNING@ state to the @STOPPED@ state).
--
-- * 'adStartedAt' - The Unix timestamp for when the attempt was started (when the task transitioned from the @PENDING@ state to the @RUNNING@ state).
--
-- * 'adContainer' - Details about the container in this job attempt.
--
-- * 'adStatusReason' - A short, human-readable string to provide additional details about the current status of the job attempt.
attemptDetail
    :: AttemptDetail
attemptDetail =
    AttemptDetail'
    { _adStoppedAt = Nothing
    , _adStartedAt = Nothing
    , _adContainer = Nothing
    , _adStatusReason = Nothing
    }

-- | The Unix timestamp for when the attempt was stopped (when the task transitioned from the @RUNNING@ state to the @STOPPED@ state).
adStoppedAt :: Lens' AttemptDetail (Maybe Integer)
adStoppedAt = lens _adStoppedAt (\ s a -> s{_adStoppedAt = a});

-- | The Unix timestamp for when the attempt was started (when the task transitioned from the @PENDING@ state to the @RUNNING@ state).
adStartedAt :: Lens' AttemptDetail (Maybe Integer)
adStartedAt = lens _adStartedAt (\ s a -> s{_adStartedAt = a});

-- | Details about the container in this job attempt.
adContainer :: Lens' AttemptDetail (Maybe AttemptContainerDetail)
adContainer = lens _adContainer (\ s a -> s{_adContainer = a});

-- | A short, human-readable string to provide additional details about the current status of the job attempt.
adStatusReason :: Lens' AttemptDetail (Maybe Text)
adStatusReason = lens _adStatusReason (\ s a -> s{_adStatusReason = a});

instance FromJSON AttemptDetail where
        parseJSON
          = withObject "AttemptDetail"
              (\ x ->
                 AttemptDetail' <$>
                   (x .:? "stoppedAt") <*> (x .:? "startedAt") <*>
                     (x .:? "container")
                     <*> (x .:? "statusReason"))

instance Hashable AttemptDetail

instance NFData AttemptDetail

-- | An object representing an AWS Batch compute environment.
--
--
--
-- /See:/ 'computeEnvironmentDetail' smart constructor.
data ComputeEnvironmentDetail = ComputeEnvironmentDetail'
    { _cedStatus                 :: !(Maybe CEStatus)
    , _cedState                  :: !(Maybe CEState)
    , _cedComputeResources       :: !(Maybe ComputeResource)
    , _cedStatusReason           :: !(Maybe Text)
    , _cedType                   :: !(Maybe CEType)
    , _cedServiceRole            :: !(Maybe Text)
    , _cedComputeEnvironmentName :: !Text
    , _cedComputeEnvironmentARN  :: !Text
    , _cedEcsClusterARN          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ComputeEnvironmentDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cedStatus' - The current status of the compute environment (for example, @CREATING@ or @VALID@ ).
--
-- * 'cedState' - The state of the compute environment. The valid values are @ENABLED@ or @DISABLED@ . An @ENABLED@ state indicates that you can register instances with the compute environment and that the associated instances can accept jobs.
--
-- * 'cedComputeResources' - The compute resources defined for the compute environment.
--
-- * 'cedStatusReason' - A short, human-readable string to provide additional details about the current status of the compute environment.
--
-- * 'cedType' - The type of the compute environment.
--
-- * 'cedServiceRole' - The service role associated with the compute environment that allows AWS Batch to make calls to AWS API operations on your behalf.
--
-- * 'cedComputeEnvironmentName' - The name of the compute environment.
--
-- * 'cedComputeEnvironmentARN' - The Amazon Resource Name (ARN) of the compute environment.
--
-- * 'cedEcsClusterARN' - The Amazon Resource Name (ARN) of the underlying Amazon ECS cluster used by the compute environment.
computeEnvironmentDetail
    :: Text -- ^ 'cedComputeEnvironmentName'
    -> Text -- ^ 'cedComputeEnvironmentARN'
    -> Text -- ^ 'cedEcsClusterARN'
    -> ComputeEnvironmentDetail
computeEnvironmentDetail pComputeEnvironmentName_ pComputeEnvironmentARN_ pEcsClusterARN_ =
    ComputeEnvironmentDetail'
    { _cedStatus = Nothing
    , _cedState = Nothing
    , _cedComputeResources = Nothing
    , _cedStatusReason = Nothing
    , _cedType = Nothing
    , _cedServiceRole = Nothing
    , _cedComputeEnvironmentName = pComputeEnvironmentName_
    , _cedComputeEnvironmentARN = pComputeEnvironmentARN_
    , _cedEcsClusterARN = pEcsClusterARN_
    }

-- | The current status of the compute environment (for example, @CREATING@ or @VALID@ ).
cedStatus :: Lens' ComputeEnvironmentDetail (Maybe CEStatus)
cedStatus = lens _cedStatus (\ s a -> s{_cedStatus = a});

-- | The state of the compute environment. The valid values are @ENABLED@ or @DISABLED@ . An @ENABLED@ state indicates that you can register instances with the compute environment and that the associated instances can accept jobs.
cedState :: Lens' ComputeEnvironmentDetail (Maybe CEState)
cedState = lens _cedState (\ s a -> s{_cedState = a});

-- | The compute resources defined for the compute environment.
cedComputeResources :: Lens' ComputeEnvironmentDetail (Maybe ComputeResource)
cedComputeResources = lens _cedComputeResources (\ s a -> s{_cedComputeResources = a});

-- | A short, human-readable string to provide additional details about the current status of the compute environment.
cedStatusReason :: Lens' ComputeEnvironmentDetail (Maybe Text)
cedStatusReason = lens _cedStatusReason (\ s a -> s{_cedStatusReason = a});

-- | The type of the compute environment.
cedType :: Lens' ComputeEnvironmentDetail (Maybe CEType)
cedType = lens _cedType (\ s a -> s{_cedType = a});

-- | The service role associated with the compute environment that allows AWS Batch to make calls to AWS API operations on your behalf.
cedServiceRole :: Lens' ComputeEnvironmentDetail (Maybe Text)
cedServiceRole = lens _cedServiceRole (\ s a -> s{_cedServiceRole = a});

-- | The name of the compute environment.
cedComputeEnvironmentName :: Lens' ComputeEnvironmentDetail Text
cedComputeEnvironmentName = lens _cedComputeEnvironmentName (\ s a -> s{_cedComputeEnvironmentName = a});

-- | The Amazon Resource Name (ARN) of the compute environment.
cedComputeEnvironmentARN :: Lens' ComputeEnvironmentDetail Text
cedComputeEnvironmentARN = lens _cedComputeEnvironmentARN (\ s a -> s{_cedComputeEnvironmentARN = a});

-- | The Amazon Resource Name (ARN) of the underlying Amazon ECS cluster used by the compute environment.
cedEcsClusterARN :: Lens' ComputeEnvironmentDetail Text
cedEcsClusterARN = lens _cedEcsClusterARN (\ s a -> s{_cedEcsClusterARN = a});

instance FromJSON ComputeEnvironmentDetail where
        parseJSON
          = withObject "ComputeEnvironmentDetail"
              (\ x ->
                 ComputeEnvironmentDetail' <$>
                   (x .:? "status") <*> (x .:? "state") <*>
                     (x .:? "computeResources")
                     <*> (x .:? "statusReason")
                     <*> (x .:? "type")
                     <*> (x .:? "serviceRole")
                     <*> (x .: "computeEnvironmentName")
                     <*> (x .: "computeEnvironmentArn")
                     <*> (x .: "ecsClusterArn"))

instance Hashable ComputeEnvironmentDetail

instance NFData ComputeEnvironmentDetail

-- | The order in which compute environments are tried for job placement within a queue. Compute environments are tried in ascending order. For example, if two compute environments are associated with a job queue, the compute environment with a lower order integer value is tried for job placement first.
--
--
--
-- /See:/ 'computeEnvironmentOrder' smart constructor.
data ComputeEnvironmentOrder = ComputeEnvironmentOrder'
    { _ceoOrder              :: !Int
    , _ceoComputeEnvironment :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ComputeEnvironmentOrder' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ceoOrder' - The order of the compute environment.
--
-- * 'ceoComputeEnvironment' - The Amazon Resource Name (ARN) of the compute environment.
computeEnvironmentOrder
    :: Int -- ^ 'ceoOrder'
    -> Text -- ^ 'ceoComputeEnvironment'
    -> ComputeEnvironmentOrder
computeEnvironmentOrder pOrder_ pComputeEnvironment_ =
    ComputeEnvironmentOrder'
    { _ceoOrder = pOrder_
    , _ceoComputeEnvironment = pComputeEnvironment_
    }

-- | The order of the compute environment.
ceoOrder :: Lens' ComputeEnvironmentOrder Int
ceoOrder = lens _ceoOrder (\ s a -> s{_ceoOrder = a});

-- | The Amazon Resource Name (ARN) of the compute environment.
ceoComputeEnvironment :: Lens' ComputeEnvironmentOrder Text
ceoComputeEnvironment = lens _ceoComputeEnvironment (\ s a -> s{_ceoComputeEnvironment = a});

instance FromJSON ComputeEnvironmentOrder where
        parseJSON
          = withObject "ComputeEnvironmentOrder"
              (\ x ->
                 ComputeEnvironmentOrder' <$>
                   (x .: "order") <*> (x .: "computeEnvironment"))

instance Hashable ComputeEnvironmentOrder

instance NFData ComputeEnvironmentOrder

instance ToJSON ComputeEnvironmentOrder where
        toJSON ComputeEnvironmentOrder'{..}
          = object
              (catMaybes
                 [Just ("order" .= _ceoOrder),
                  Just
                    ("computeEnvironment" .= _ceoComputeEnvironment)])

-- | An object representing an AWS Batch compute resource.
--
--
--
-- /See:/ 'computeResource' smart constructor.
data ComputeResource = ComputeResource'
    { _crEc2KeyPair       :: !(Maybe Text)
    , _crBidPercentage    :: !(Maybe Int)
    , _crSpotIAMFleetRole :: !(Maybe Text)
    , _crImageId          :: !(Maybe Text)
    , _crDesiredvCPUs     :: !(Maybe Int)
    , _crTags             :: !(Maybe (Map Text Text))
    , _crType             :: !CRType
    , _crMinvCPUs         :: !Int
    , _crMaxvCPUs         :: !Int
    , _crInstanceTypes    :: ![Text]
    , _crSubnets          :: ![Text]
    , _crSecurityGroupIds :: ![Text]
    , _crInstanceRole     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ComputeResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crEc2KeyPair' - The EC2 key pair that is used for instances launched in the compute environment.
--
-- * 'crBidPercentage' - The minimum percentage that a Spot Instance price must be when compared with the On-Demand price for that instance type before instances are launched. For example, if your bid percentage is 20%, then the Spot price must be below 20% of the current On-Demand price for that EC2 instance.
--
-- * 'crSpotIAMFleetRole' - The Amazon Resource Name (ARN) of the Amazon EC2 Spot Fleet IAM role applied to a @SPOT@ compute environment.
--
-- * 'crImageId' - The Amazon Machine Image (AMI) ID used for instances launched in the compute environment.
--
-- * 'crDesiredvCPUs' - The desired number of EC2 vCPUS in the compute environment.
--
-- * 'crTags' - Key-value pair tags to be applied to resources that are launched in the compute environment.
--
-- * 'crType' - The type of compute environment.
--
-- * 'crMinvCPUs' - The minimum number of EC2 vCPUs that an environment should maintain.
--
-- * 'crMaxvCPUs' - The maximum number of EC2 vCPUs that an environment can reach.
--
-- * 'crInstanceTypes' - The instances types that may launched.
--
-- * 'crSubnets' - The VPC subnets into which the compute resources are launched.
--
-- * 'crSecurityGroupIds' - The EC2 security group that is associated with instances launched in the compute environment.
--
-- * 'crInstanceRole' - The Amazon ECS instance role applied to Amazon EC2 instances in a compute environment.
computeResource
    :: CRType -- ^ 'crType'
    -> Int -- ^ 'crMinvCPUs'
    -> Int -- ^ 'crMaxvCPUs'
    -> Text -- ^ 'crInstanceRole'
    -> ComputeResource
computeResource pType_ pMinvCPUs_ pMaxvCPUs_ pInstanceRole_ =
    ComputeResource'
    { _crEc2KeyPair = Nothing
    , _crBidPercentage = Nothing
    , _crSpotIAMFleetRole = Nothing
    , _crImageId = Nothing
    , _crDesiredvCPUs = Nothing
    , _crTags = Nothing
    , _crType = pType_
    , _crMinvCPUs = pMinvCPUs_
    , _crMaxvCPUs = pMaxvCPUs_
    , _crInstanceTypes = mempty
    , _crSubnets = mempty
    , _crSecurityGroupIds = mempty
    , _crInstanceRole = pInstanceRole_
    }

-- | The EC2 key pair that is used for instances launched in the compute environment.
crEc2KeyPair :: Lens' ComputeResource (Maybe Text)
crEc2KeyPair = lens _crEc2KeyPair (\ s a -> s{_crEc2KeyPair = a});

-- | The minimum percentage that a Spot Instance price must be when compared with the On-Demand price for that instance type before instances are launched. For example, if your bid percentage is 20%, then the Spot price must be below 20% of the current On-Demand price for that EC2 instance.
crBidPercentage :: Lens' ComputeResource (Maybe Int)
crBidPercentage = lens _crBidPercentage (\ s a -> s{_crBidPercentage = a});

-- | The Amazon Resource Name (ARN) of the Amazon EC2 Spot Fleet IAM role applied to a @SPOT@ compute environment.
crSpotIAMFleetRole :: Lens' ComputeResource (Maybe Text)
crSpotIAMFleetRole = lens _crSpotIAMFleetRole (\ s a -> s{_crSpotIAMFleetRole = a});

-- | The Amazon Machine Image (AMI) ID used for instances launched in the compute environment.
crImageId :: Lens' ComputeResource (Maybe Text)
crImageId = lens _crImageId (\ s a -> s{_crImageId = a});

-- | The desired number of EC2 vCPUS in the compute environment.
crDesiredvCPUs :: Lens' ComputeResource (Maybe Int)
crDesiredvCPUs = lens _crDesiredvCPUs (\ s a -> s{_crDesiredvCPUs = a});

-- | Key-value pair tags to be applied to resources that are launched in the compute environment.
crTags :: Lens' ComputeResource (HashMap Text Text)
crTags = lens _crTags (\ s a -> s{_crTags = a}) . _Default . _Map;

-- | The type of compute environment.
crType :: Lens' ComputeResource CRType
crType = lens _crType (\ s a -> s{_crType = a});

-- | The minimum number of EC2 vCPUs that an environment should maintain.
crMinvCPUs :: Lens' ComputeResource Int
crMinvCPUs = lens _crMinvCPUs (\ s a -> s{_crMinvCPUs = a});

-- | The maximum number of EC2 vCPUs that an environment can reach.
crMaxvCPUs :: Lens' ComputeResource Int
crMaxvCPUs = lens _crMaxvCPUs (\ s a -> s{_crMaxvCPUs = a});

-- | The instances types that may launched.
crInstanceTypes :: Lens' ComputeResource [Text]
crInstanceTypes = lens _crInstanceTypes (\ s a -> s{_crInstanceTypes = a}) . _Coerce;

-- | The VPC subnets into which the compute resources are launched.
crSubnets :: Lens' ComputeResource [Text]
crSubnets = lens _crSubnets (\ s a -> s{_crSubnets = a}) . _Coerce;

-- | The EC2 security group that is associated with instances launched in the compute environment.
crSecurityGroupIds :: Lens' ComputeResource [Text]
crSecurityGroupIds = lens _crSecurityGroupIds (\ s a -> s{_crSecurityGroupIds = a}) . _Coerce;

-- | The Amazon ECS instance role applied to Amazon EC2 instances in a compute environment.
crInstanceRole :: Lens' ComputeResource Text
crInstanceRole = lens _crInstanceRole (\ s a -> s{_crInstanceRole = a});

instance FromJSON ComputeResource where
        parseJSON
          = withObject "ComputeResource"
              (\ x ->
                 ComputeResource' <$>
                   (x .:? "ec2KeyPair") <*> (x .:? "bidPercentage") <*>
                     (x .:? "spotIamFleetRole")
                     <*> (x .:? "imageId")
                     <*> (x .:? "desiredvCpus")
                     <*> (x .:? "tags" .!= mempty)
                     <*> (x .: "type")
                     <*> (x .: "minvCpus")
                     <*> (x .: "maxvCpus")
                     <*> (x .:? "instanceTypes" .!= mempty)
                     <*> (x .:? "subnets" .!= mempty)
                     <*> (x .:? "securityGroupIds" .!= mempty)
                     <*> (x .: "instanceRole"))

instance Hashable ComputeResource

instance NFData ComputeResource

instance ToJSON ComputeResource where
        toJSON ComputeResource'{..}
          = object
              (catMaybes
                 [("ec2KeyPair" .=) <$> _crEc2KeyPair,
                  ("bidPercentage" .=) <$> _crBidPercentage,
                  ("spotIamFleetRole" .=) <$> _crSpotIAMFleetRole,
                  ("imageId" .=) <$> _crImageId,
                  ("desiredvCpus" .=) <$> _crDesiredvCPUs,
                  ("tags" .=) <$> _crTags, Just ("type" .= _crType),
                  Just ("minvCpus" .= _crMinvCPUs),
                  Just ("maxvCpus" .= _crMaxvCPUs),
                  Just ("instanceTypes" .= _crInstanceTypes),
                  Just ("subnets" .= _crSubnets),
                  Just ("securityGroupIds" .= _crSecurityGroupIds),
                  Just ("instanceRole" .= _crInstanceRole)])

-- | An object representing the attributes of a compute environment that can be updated.
--
--
--
-- /See:/ 'computeResourceUpdate' smart constructor.
data ComputeResourceUpdate = ComputeResourceUpdate'
    { _cruMinvCPUs     :: !(Maybe Int)
    , _cruMaxvCPUs     :: !(Maybe Int)
    , _cruDesiredvCPUs :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ComputeResourceUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cruMinvCPUs' - The minimum number of EC2 vCPUs that an environment should maintain.
--
-- * 'cruMaxvCPUs' - The maximum number of EC2 vCPUs that an environment can reach.
--
-- * 'cruDesiredvCPUs' - The desired number of EC2 vCPUS in the compute environment.
computeResourceUpdate
    :: ComputeResourceUpdate
computeResourceUpdate =
    ComputeResourceUpdate'
    { _cruMinvCPUs = Nothing
    , _cruMaxvCPUs = Nothing
    , _cruDesiredvCPUs = Nothing
    }

-- | The minimum number of EC2 vCPUs that an environment should maintain.
cruMinvCPUs :: Lens' ComputeResourceUpdate (Maybe Int)
cruMinvCPUs = lens _cruMinvCPUs (\ s a -> s{_cruMinvCPUs = a});

-- | The maximum number of EC2 vCPUs that an environment can reach.
cruMaxvCPUs :: Lens' ComputeResourceUpdate (Maybe Int)
cruMaxvCPUs = lens _cruMaxvCPUs (\ s a -> s{_cruMaxvCPUs = a});

-- | The desired number of EC2 vCPUS in the compute environment.
cruDesiredvCPUs :: Lens' ComputeResourceUpdate (Maybe Int)
cruDesiredvCPUs = lens _cruDesiredvCPUs (\ s a -> s{_cruDesiredvCPUs = a});

instance Hashable ComputeResourceUpdate

instance NFData ComputeResourceUpdate

instance ToJSON ComputeResourceUpdate where
        toJSON ComputeResourceUpdate'{..}
          = object
              (catMaybes
                 [("minvCpus" .=) <$> _cruMinvCPUs,
                  ("maxvCpus" .=) <$> _cruMaxvCPUs,
                  ("desiredvCpus" .=) <$> _cruDesiredvCPUs])

-- | An object representing the details of a container that is part of a job.
--
--
--
-- /See:/ 'containerDetail' smart constructor.
data ContainerDetail = ContainerDetail'
    { _cdImage                  :: !(Maybe Text)
    , _cdCommand                :: !(Maybe [Text])
    , _cdEnvironment            :: !(Maybe [KeyValuePair])
    , _cdTaskARN                :: !(Maybe Text)
    , _cdUlimits                :: !(Maybe [Ulimit])
    , _cdContainerInstanceARN   :: !(Maybe Text)
    , _cdPrivileged             :: !(Maybe Bool)
    , _cdJobRoleARN             :: !(Maybe Text)
    , _cdMemory                 :: !(Maybe Int)
    , _cdUser                   :: !(Maybe Text)
    , _cdReason                 :: !(Maybe Text)
    , _cdLogStreamName          :: !(Maybe Text)
    , _cdMountPoints            :: !(Maybe [MountPoint])
    , _cdExitCode               :: !(Maybe Int)
    , _cdVcpus                  :: !(Maybe Int)
    , _cdReadonlyRootFilesystem :: !(Maybe Bool)
    , _cdVolumes                :: !(Maybe [Volume])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ContainerDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdImage' - The image used to start the container.
--
-- * 'cdCommand' - The command that is passed to the container.
--
-- * 'cdEnvironment' - The environment variables to pass to a container.
--
-- * 'cdTaskARN' - The Amazon Resource Name (ARN) of the Amazon ECS task that is associated with the container job.
--
-- * 'cdUlimits' - A list of @ulimit@ values to set in the container.
--
-- * 'cdContainerInstanceARN' - The Amazon Resource Name (ARN) of the container instance on which the container is running.
--
-- * 'cdPrivileged' - When this parameter is true, the container is given elevated privileges on the host container instance (similar to the @root@ user).
--
-- * 'cdJobRoleARN' - The Amazon Resource Name (ARN) associated with the job upon execution.
--
-- * 'cdMemory' - The number of MiB of memory reserved for the job.
--
-- * 'cdUser' - The user name to use inside the container.
--
-- * 'cdReason' - A short (255 max characters) human-readable string to provide additional details about a running or stopped container.
--
-- * 'cdLogStreamName' - Undocumented member.
--
-- * 'cdMountPoints' - The mount points for data volumes in your container.
--
-- * 'cdExitCode' - The exit code to return upon completion.
--
-- * 'cdVcpus' - The number of VCPUs allocated for the job.
--
-- * 'cdReadonlyRootFilesystem' - When this parameter is true, the container is given read-only access to its root file system.
--
-- * 'cdVolumes' - A list of volumes associated with the job.
containerDetail
    :: ContainerDetail
containerDetail =
    ContainerDetail'
    { _cdImage = Nothing
    , _cdCommand = Nothing
    , _cdEnvironment = Nothing
    , _cdTaskARN = Nothing
    , _cdUlimits = Nothing
    , _cdContainerInstanceARN = Nothing
    , _cdPrivileged = Nothing
    , _cdJobRoleARN = Nothing
    , _cdMemory = Nothing
    , _cdUser = Nothing
    , _cdReason = Nothing
    , _cdLogStreamName = Nothing
    , _cdMountPoints = Nothing
    , _cdExitCode = Nothing
    , _cdVcpus = Nothing
    , _cdReadonlyRootFilesystem = Nothing
    , _cdVolumes = Nothing
    }

-- | The image used to start the container.
cdImage :: Lens' ContainerDetail (Maybe Text)
cdImage = lens _cdImage (\ s a -> s{_cdImage = a});

-- | The command that is passed to the container.
cdCommand :: Lens' ContainerDetail [Text]
cdCommand = lens _cdCommand (\ s a -> s{_cdCommand = a}) . _Default . _Coerce;

-- | The environment variables to pass to a container.
cdEnvironment :: Lens' ContainerDetail [KeyValuePair]
cdEnvironment = lens _cdEnvironment (\ s a -> s{_cdEnvironment = a}) . _Default . _Coerce;

-- | The Amazon Resource Name (ARN) of the Amazon ECS task that is associated with the container job.
cdTaskARN :: Lens' ContainerDetail (Maybe Text)
cdTaskARN = lens _cdTaskARN (\ s a -> s{_cdTaskARN = a});

-- | A list of @ulimit@ values to set in the container.
cdUlimits :: Lens' ContainerDetail [Ulimit]
cdUlimits = lens _cdUlimits (\ s a -> s{_cdUlimits = a}) . _Default . _Coerce;

-- | The Amazon Resource Name (ARN) of the container instance on which the container is running.
cdContainerInstanceARN :: Lens' ContainerDetail (Maybe Text)
cdContainerInstanceARN = lens _cdContainerInstanceARN (\ s a -> s{_cdContainerInstanceARN = a});

-- | When this parameter is true, the container is given elevated privileges on the host container instance (similar to the @root@ user).
cdPrivileged :: Lens' ContainerDetail (Maybe Bool)
cdPrivileged = lens _cdPrivileged (\ s a -> s{_cdPrivileged = a});

-- | The Amazon Resource Name (ARN) associated with the job upon execution.
cdJobRoleARN :: Lens' ContainerDetail (Maybe Text)
cdJobRoleARN = lens _cdJobRoleARN (\ s a -> s{_cdJobRoleARN = a});

-- | The number of MiB of memory reserved for the job.
cdMemory :: Lens' ContainerDetail (Maybe Int)
cdMemory = lens _cdMemory (\ s a -> s{_cdMemory = a});

-- | The user name to use inside the container.
cdUser :: Lens' ContainerDetail (Maybe Text)
cdUser = lens _cdUser (\ s a -> s{_cdUser = a});

-- | A short (255 max characters) human-readable string to provide additional details about a running or stopped container.
cdReason :: Lens' ContainerDetail (Maybe Text)
cdReason = lens _cdReason (\ s a -> s{_cdReason = a});

-- | Undocumented member.
cdLogStreamName :: Lens' ContainerDetail (Maybe Text)
cdLogStreamName = lens _cdLogStreamName (\ s a -> s{_cdLogStreamName = a});

-- | The mount points for data volumes in your container.
cdMountPoints :: Lens' ContainerDetail [MountPoint]
cdMountPoints = lens _cdMountPoints (\ s a -> s{_cdMountPoints = a}) . _Default . _Coerce;

-- | The exit code to return upon completion.
cdExitCode :: Lens' ContainerDetail (Maybe Int)
cdExitCode = lens _cdExitCode (\ s a -> s{_cdExitCode = a});

-- | The number of VCPUs allocated for the job.
cdVcpus :: Lens' ContainerDetail (Maybe Int)
cdVcpus = lens _cdVcpus (\ s a -> s{_cdVcpus = a});

-- | When this parameter is true, the container is given read-only access to its root file system.
cdReadonlyRootFilesystem :: Lens' ContainerDetail (Maybe Bool)
cdReadonlyRootFilesystem = lens _cdReadonlyRootFilesystem (\ s a -> s{_cdReadonlyRootFilesystem = a});

-- | A list of volumes associated with the job.
cdVolumes :: Lens' ContainerDetail [Volume]
cdVolumes = lens _cdVolumes (\ s a -> s{_cdVolumes = a}) . _Default . _Coerce;

instance FromJSON ContainerDetail where
        parseJSON
          = withObject "ContainerDetail"
              (\ x ->
                 ContainerDetail' <$>
                   (x .:? "image") <*> (x .:? "command" .!= mempty) <*>
                     (x .:? "environment" .!= mempty)
                     <*> (x .:? "taskArn")
                     <*> (x .:? "ulimits" .!= mempty)
                     <*> (x .:? "containerInstanceArn")
                     <*> (x .:? "privileged")
                     <*> (x .:? "jobRoleArn")
                     <*> (x .:? "memory")
                     <*> (x .:? "user")
                     <*> (x .:? "reason")
                     <*> (x .:? "logStreamName")
                     <*> (x .:? "mountPoints" .!= mempty)
                     <*> (x .:? "exitCode")
                     <*> (x .:? "vcpus")
                     <*> (x .:? "readonlyRootFilesystem")
                     <*> (x .:? "volumes" .!= mempty))

instance Hashable ContainerDetail

instance NFData ContainerDetail

-- | The overrides that should be sent to a container.
--
--
--
-- /See:/ 'containerOverrides' smart constructor.
data ContainerOverrides = ContainerOverrides'
    { _coCommand     :: !(Maybe [Text])
    , _coEnvironment :: !(Maybe [KeyValuePair])
    , _coMemory      :: !(Maybe Int)
    , _coVcpus       :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ContainerOverrides' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'coCommand' - The command to send to the container that overrides the default command from the Docker image or the job definition.
--
-- * 'coEnvironment' - The environment variables to send to the container. You can add new environment variables, which are added to the container at launch, or you can override the existing environment variables from the Docker image or the job definition.
--
-- * 'coMemory' - The number of MiB of memory reserved for the job. This value overrides the value set in the job definition.
--
-- * 'coVcpus' - The number of vCPUs to reserve for the container. This value overrides the value set in the job definition.
containerOverrides
    :: ContainerOverrides
containerOverrides =
    ContainerOverrides'
    { _coCommand = Nothing
    , _coEnvironment = Nothing
    , _coMemory = Nothing
    , _coVcpus = Nothing
    }

-- | The command to send to the container that overrides the default command from the Docker image or the job definition.
coCommand :: Lens' ContainerOverrides [Text]
coCommand = lens _coCommand (\ s a -> s{_coCommand = a}) . _Default . _Coerce;

-- | The environment variables to send to the container. You can add new environment variables, which are added to the container at launch, or you can override the existing environment variables from the Docker image or the job definition.
coEnvironment :: Lens' ContainerOverrides [KeyValuePair]
coEnvironment = lens _coEnvironment (\ s a -> s{_coEnvironment = a}) . _Default . _Coerce;

-- | The number of MiB of memory reserved for the job. This value overrides the value set in the job definition.
coMemory :: Lens' ContainerOverrides (Maybe Int)
coMemory = lens _coMemory (\ s a -> s{_coMemory = a});

-- | The number of vCPUs to reserve for the container. This value overrides the value set in the job definition.
coVcpus :: Lens' ContainerOverrides (Maybe Int)
coVcpus = lens _coVcpus (\ s a -> s{_coVcpus = a});

instance Hashable ContainerOverrides

instance NFData ContainerOverrides

instance ToJSON ContainerOverrides where
        toJSON ContainerOverrides'{..}
          = object
              (catMaybes
                 [("command" .=) <$> _coCommand,
                  ("environment" .=) <$> _coEnvironment,
                  ("memory" .=) <$> _coMemory,
                  ("vcpus" .=) <$> _coVcpus])

-- | Container properties are used in job definitions to describe the container that is launched as part of a job.
--
--
--
-- /See:/ 'containerProperties' smart constructor.
data ContainerProperties = ContainerProperties'
    { _cpCommand                :: !(Maybe [Text])
    , _cpEnvironment            :: !(Maybe [KeyValuePair])
    , _cpUlimits                :: !(Maybe [Ulimit])
    , _cpPrivileged             :: !(Maybe Bool)
    , _cpJobRoleARN             :: !(Maybe Text)
    , _cpUser                   :: !(Maybe Text)
    , _cpMountPoints            :: !(Maybe [MountPoint])
    , _cpReadonlyRootFilesystem :: !(Maybe Bool)
    , _cpVolumes                :: !(Maybe [Volume])
    , _cpImage                  :: !Text
    , _cpVcpus                  :: !Int
    , _cpMemory                 :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ContainerProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpCommand' - The command that is passed to the container. This parameter maps to @Cmd@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @COMMAND@ parameter to <https://docs.docker.com/engine/reference/run/ docker run> . For more information, see <https://docs.docker.com/engine/reference/builder/#cmd https://docs.docker.com/engine/reference/builder/#cmd> .
--
-- * 'cpEnvironment' - The environment variables to pass to a container. This parameter maps to @Env@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--env@ option to <https://docs.docker.com/engine/reference/run/ docker run> . /Important:/ We do not recommend using plain text environment variables for sensitive information, such as credential data.
--
-- * 'cpUlimits' - A list of @ulimits@ to set in the container. This parameter maps to @Ulimits@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--ulimit@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- * 'cpPrivileged' - When this parameter is true, the container is given elevated privileges on the host container instance (similar to the @root@ user). This parameter maps to @Privileged@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--privileged@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- * 'cpJobRoleARN' - The Amazon Resource Name (ARN) of the IAM role that the container can assume for AWS permissions.
--
-- * 'cpUser' - The user name to use inside the container. This parameter maps to @User@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--user@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- * 'cpMountPoints' - The mount points for data volumes in your container. This parameter maps to @Volumes@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--volume@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- * 'cpReadonlyRootFilesystem' - When this parameter is true, the container is given read-only access to its root file system. This parameter maps to @ReadonlyRootfs@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--read-only@ option to @docker run@ .
--
-- * 'cpVolumes' - A list of data volumes used in a job.
--
-- * 'cpImage' - The image used to start a container. This string is passed directly to the Docker daemon. Images in the Docker Hub registry are available by default. Other repositories are specified with @/repository-url/ //image/ :/tag/ @ . Up to 255 letters (uppercase and lowercase), numbers, hyphens, underscores, colons, periods, forward slashes, and number signs are allowed. This parameter maps to @Image@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @IMAGE@ parameter of <https://docs.docker.com/engine/reference/run/ docker run> .     * Images in Amazon ECR repositories use the full registry and repository URI (for example, @012345678910.dkr.ecr.<region-name>.amazonaws.com/<repository-name>@ ).      * Images in official repositories on Docker Hub use a single name (for example, @ubuntu@ or @mongo@ ).     * Images in other repositories on Docker Hub are qualified with an organization name (for example, @amazon/amazon-ecs-agent@ ).     * Images in other online repositories are qualified further by a domain name (for example, @quay.io/assemblyline/ubuntu@ ).
--
-- * 'cpVcpus' - The number of vCPUs reserved for the container. This parameter maps to @CpuShares@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--cpu-shares@ option to <https://docs.docker.com/engine/reference/run/ docker run> . Each vCPU is equivalent to 1,024 CPU shares.
--
-- * 'cpMemory' - The hard limit (in MiB) of memory to present to the container. If your container attempts to exceed the memory specified here, the container is killed. This parameter maps to @Memory@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--memory@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
containerProperties
    :: Text -- ^ 'cpImage'
    -> Int -- ^ 'cpVcpus'
    -> Int -- ^ 'cpMemory'
    -> ContainerProperties
containerProperties pImage_ pVcpus_ pMemory_ =
    ContainerProperties'
    { _cpCommand = Nothing
    , _cpEnvironment = Nothing
    , _cpUlimits = Nothing
    , _cpPrivileged = Nothing
    , _cpJobRoleARN = Nothing
    , _cpUser = Nothing
    , _cpMountPoints = Nothing
    , _cpReadonlyRootFilesystem = Nothing
    , _cpVolumes = Nothing
    , _cpImage = pImage_
    , _cpVcpus = pVcpus_
    , _cpMemory = pMemory_
    }

-- | The command that is passed to the container. This parameter maps to @Cmd@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @COMMAND@ parameter to <https://docs.docker.com/engine/reference/run/ docker run> . For more information, see <https://docs.docker.com/engine/reference/builder/#cmd https://docs.docker.com/engine/reference/builder/#cmd> .
cpCommand :: Lens' ContainerProperties [Text]
cpCommand = lens _cpCommand (\ s a -> s{_cpCommand = a}) . _Default . _Coerce;

-- | The environment variables to pass to a container. This parameter maps to @Env@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--env@ option to <https://docs.docker.com/engine/reference/run/ docker run> . /Important:/ We do not recommend using plain text environment variables for sensitive information, such as credential data.
cpEnvironment :: Lens' ContainerProperties [KeyValuePair]
cpEnvironment = lens _cpEnvironment (\ s a -> s{_cpEnvironment = a}) . _Default . _Coerce;

-- | A list of @ulimits@ to set in the container. This parameter maps to @Ulimits@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--ulimit@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
cpUlimits :: Lens' ContainerProperties [Ulimit]
cpUlimits = lens _cpUlimits (\ s a -> s{_cpUlimits = a}) . _Default . _Coerce;

-- | When this parameter is true, the container is given elevated privileges on the host container instance (similar to the @root@ user). This parameter maps to @Privileged@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--privileged@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
cpPrivileged :: Lens' ContainerProperties (Maybe Bool)
cpPrivileged = lens _cpPrivileged (\ s a -> s{_cpPrivileged = a});

-- | The Amazon Resource Name (ARN) of the IAM role that the container can assume for AWS permissions.
cpJobRoleARN :: Lens' ContainerProperties (Maybe Text)
cpJobRoleARN = lens _cpJobRoleARN (\ s a -> s{_cpJobRoleARN = a});

-- | The user name to use inside the container. This parameter maps to @User@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--user@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
cpUser :: Lens' ContainerProperties (Maybe Text)
cpUser = lens _cpUser (\ s a -> s{_cpUser = a});

-- | The mount points for data volumes in your container. This parameter maps to @Volumes@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--volume@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
cpMountPoints :: Lens' ContainerProperties [MountPoint]
cpMountPoints = lens _cpMountPoints (\ s a -> s{_cpMountPoints = a}) . _Default . _Coerce;

-- | When this parameter is true, the container is given read-only access to its root file system. This parameter maps to @ReadonlyRootfs@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--read-only@ option to @docker run@ .
cpReadonlyRootFilesystem :: Lens' ContainerProperties (Maybe Bool)
cpReadonlyRootFilesystem = lens _cpReadonlyRootFilesystem (\ s a -> s{_cpReadonlyRootFilesystem = a});

-- | A list of data volumes used in a job.
cpVolumes :: Lens' ContainerProperties [Volume]
cpVolumes = lens _cpVolumes (\ s a -> s{_cpVolumes = a}) . _Default . _Coerce;

-- | The image used to start a container. This string is passed directly to the Docker daemon. Images in the Docker Hub registry are available by default. Other repositories are specified with @/repository-url/ //image/ :/tag/ @ . Up to 255 letters (uppercase and lowercase), numbers, hyphens, underscores, colons, periods, forward slashes, and number signs are allowed. This parameter maps to @Image@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @IMAGE@ parameter of <https://docs.docker.com/engine/reference/run/ docker run> .     * Images in Amazon ECR repositories use the full registry and repository URI (for example, @012345678910.dkr.ecr.<region-name>.amazonaws.com/<repository-name>@ ).      * Images in official repositories on Docker Hub use a single name (for example, @ubuntu@ or @mongo@ ).     * Images in other repositories on Docker Hub are qualified with an organization name (for example, @amazon/amazon-ecs-agent@ ).     * Images in other online repositories are qualified further by a domain name (for example, @quay.io/assemblyline/ubuntu@ ).
cpImage :: Lens' ContainerProperties Text
cpImage = lens _cpImage (\ s a -> s{_cpImage = a});

-- | The number of vCPUs reserved for the container. This parameter maps to @CpuShares@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--cpu-shares@ option to <https://docs.docker.com/engine/reference/run/ docker run> . Each vCPU is equivalent to 1,024 CPU shares.
cpVcpus :: Lens' ContainerProperties Int
cpVcpus = lens _cpVcpus (\ s a -> s{_cpVcpus = a});

-- | The hard limit (in MiB) of memory to present to the container. If your container attempts to exceed the memory specified here, the container is killed. This parameter maps to @Memory@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--memory@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
cpMemory :: Lens' ContainerProperties Int
cpMemory = lens _cpMemory (\ s a -> s{_cpMemory = a});

instance FromJSON ContainerProperties where
        parseJSON
          = withObject "ContainerProperties"
              (\ x ->
                 ContainerProperties' <$>
                   (x .:? "command" .!= mempty) <*>
                     (x .:? "environment" .!= mempty)
                     <*> (x .:? "ulimits" .!= mempty)
                     <*> (x .:? "privileged")
                     <*> (x .:? "jobRoleArn")
                     <*> (x .:? "user")
                     <*> (x .:? "mountPoints" .!= mempty)
                     <*> (x .:? "readonlyRootFilesystem")
                     <*> (x .:? "volumes" .!= mempty)
                     <*> (x .: "image")
                     <*> (x .: "vcpus")
                     <*> (x .: "memory"))

instance Hashable ContainerProperties

instance NFData ContainerProperties

instance ToJSON ContainerProperties where
        toJSON ContainerProperties'{..}
          = object
              (catMaybes
                 [("command" .=) <$> _cpCommand,
                  ("environment" .=) <$> _cpEnvironment,
                  ("ulimits" .=) <$> _cpUlimits,
                  ("privileged" .=) <$> _cpPrivileged,
                  ("jobRoleArn" .=) <$> _cpJobRoleARN,
                  ("user" .=) <$> _cpUser,
                  ("mountPoints" .=) <$> _cpMountPoints,
                  ("readonlyRootFilesystem" .=) <$>
                    _cpReadonlyRootFilesystem,
                  ("volumes" .=) <$> _cpVolumes,
                  Just ("image" .= _cpImage),
                  Just ("vcpus" .= _cpVcpus),
                  Just ("memory" .= _cpMemory)])

-- | The contents of the @host@ parameter determine whether your data volume persists on the host container instance and where it is stored. If the host parameter is empty, then the Docker daemon assigns a host path for your data volume, but the data is not guaranteed to persist after the containers associated with it stop running.
--
--
--
-- /See:/ 'host' smart constructor.
newtype Host = Host'
    { _hSourcePath :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Host' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hSourcePath' - The path on the host container instance that is presented to the container. If this parameter is empty, then the Docker daemon has assigned a host path for you. If the @host@ parameter contains a @sourcePath@ file location, then the data volume persists at the specified location on the host container instance until you delete it manually. If the @sourcePath@ value does not exist on the host container instance, the Docker daemon creates it. If the location does exist, the contents of the source path folder are exported.
host
    :: Host
host =
    Host'
    { _hSourcePath = Nothing
    }

-- | The path on the host container instance that is presented to the container. If this parameter is empty, then the Docker daemon has assigned a host path for you. If the @host@ parameter contains a @sourcePath@ file location, then the data volume persists at the specified location on the host container instance until you delete it manually. If the @sourcePath@ value does not exist on the host container instance, the Docker daemon creates it. If the location does exist, the contents of the source path folder are exported.
hSourcePath :: Lens' Host (Maybe Text)
hSourcePath = lens _hSourcePath (\ s a -> s{_hSourcePath = a});

instance FromJSON Host where
        parseJSON
          = withObject "Host"
              (\ x -> Host' <$> (x .:? "sourcePath"))

instance Hashable Host

instance NFData Host

instance ToJSON Host where
        toJSON Host'{..}
          = object
              (catMaybes [("sourcePath" .=) <$> _hSourcePath])

-- | An object representing an AWS Batch job definition.
--
--
--
-- /See:/ 'jobDefinition' smart constructor.
data JobDefinition = JobDefinition'
    { _jStatus              :: !(Maybe Text)
    , _jRetryStrategy       :: !(Maybe RetryStrategy)
    , _jParameters          :: !(Maybe (Map Text Text))
    , _jContainerProperties :: !(Maybe ContainerProperties)
    , _jJobDefinitionName   :: !Text
    , _jJobDefinitionARN    :: !Text
    , _jRevision            :: !Int
    , _jType                :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'JobDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jStatus' - The status of the job definition.
--
-- * 'jRetryStrategy' - The retry strategy to use for failed jobs that are submitted with this job definition.
--
-- * 'jParameters' - Default parameters or parameter substitution placeholders that are set in the job definition. Parameters are specified as a key-value pair mapping. Parameters in a @SubmitJob@ request override any corresponding parameter defaults from the job definition.
--
-- * 'jContainerProperties' - An object with various properties specific to container-based jobs.
--
-- * 'jJobDefinitionName' - The name of the job definition.
--
-- * 'jJobDefinitionARN' - The Amazon Resource Name (ARN) for the job definition.
--
-- * 'jRevision' - The revision of the job definition.
--
-- * 'jType' - The type of job definition.
jobDefinition
    :: Text -- ^ 'jJobDefinitionName'
    -> Text -- ^ 'jJobDefinitionARN'
    -> Int -- ^ 'jRevision'
    -> Text -- ^ 'jType'
    -> JobDefinition
jobDefinition pJobDefinitionName_ pJobDefinitionARN_ pRevision_ pType_ =
    JobDefinition'
    { _jStatus = Nothing
    , _jRetryStrategy = Nothing
    , _jParameters = Nothing
    , _jContainerProperties = Nothing
    , _jJobDefinitionName = pJobDefinitionName_
    , _jJobDefinitionARN = pJobDefinitionARN_
    , _jRevision = pRevision_
    , _jType = pType_
    }

-- | The status of the job definition.
jStatus :: Lens' JobDefinition (Maybe Text)
jStatus = lens _jStatus (\ s a -> s{_jStatus = a});

-- | The retry strategy to use for failed jobs that are submitted with this job definition.
jRetryStrategy :: Lens' JobDefinition (Maybe RetryStrategy)
jRetryStrategy = lens _jRetryStrategy (\ s a -> s{_jRetryStrategy = a});

-- | Default parameters or parameter substitution placeholders that are set in the job definition. Parameters are specified as a key-value pair mapping. Parameters in a @SubmitJob@ request override any corresponding parameter defaults from the job definition.
jParameters :: Lens' JobDefinition (HashMap Text Text)
jParameters = lens _jParameters (\ s a -> s{_jParameters = a}) . _Default . _Map;

-- | An object with various properties specific to container-based jobs.
jContainerProperties :: Lens' JobDefinition (Maybe ContainerProperties)
jContainerProperties = lens _jContainerProperties (\ s a -> s{_jContainerProperties = a});

-- | The name of the job definition.
jJobDefinitionName :: Lens' JobDefinition Text
jJobDefinitionName = lens _jJobDefinitionName (\ s a -> s{_jJobDefinitionName = a});

-- | The Amazon Resource Name (ARN) for the job definition.
jJobDefinitionARN :: Lens' JobDefinition Text
jJobDefinitionARN = lens _jJobDefinitionARN (\ s a -> s{_jJobDefinitionARN = a});

-- | The revision of the job definition.
jRevision :: Lens' JobDefinition Int
jRevision = lens _jRevision (\ s a -> s{_jRevision = a});

-- | The type of job definition.
jType :: Lens' JobDefinition Text
jType = lens _jType (\ s a -> s{_jType = a});

instance FromJSON JobDefinition where
        parseJSON
          = withObject "JobDefinition"
              (\ x ->
                 JobDefinition' <$>
                   (x .:? "status") <*> (x .:? "retryStrategy") <*>
                     (x .:? "parameters" .!= mempty)
                     <*> (x .:? "containerProperties")
                     <*> (x .: "jobDefinitionName")
                     <*> (x .: "jobDefinitionArn")
                     <*> (x .: "revision")
                     <*> (x .: "type"))

instance Hashable JobDefinition

instance NFData JobDefinition

-- | An object representing an AWS Batch job dependency.
--
--
--
-- /See:/ 'jobDependency' smart constructor.
newtype JobDependency = JobDependency'
    { _jJobId :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'JobDependency' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jJobId' - The job ID of the AWS Batch job associated with this dependency.
jobDependency
    :: JobDependency
jobDependency =
    JobDependency'
    { _jJobId = Nothing
    }

-- | The job ID of the AWS Batch job associated with this dependency.
jJobId :: Lens' JobDependency (Maybe Text)
jJobId = lens _jJobId (\ s a -> s{_jJobId = a});

instance FromJSON JobDependency where
        parseJSON
          = withObject "JobDependency"
              (\ x -> JobDependency' <$> (x .:? "jobId"))

instance Hashable JobDependency

instance NFData JobDependency

instance ToJSON JobDependency where
        toJSON JobDependency'{..}
          = object (catMaybes [("jobId" .=) <$> _jJobId])

-- | An object representing an AWS Batch job.
--
--
--
-- /See:/ 'jobDetail' smart constructor.
data JobDetail = JobDetail'
    { _jdStoppedAt     :: !(Maybe Integer)
    , _jdCreatedAt     :: !(Maybe Integer)
    , _jdRetryStrategy :: !(Maybe RetryStrategy)
    , _jdAttempts      :: !(Maybe [AttemptDetail])
    , _jdDependsOn     :: !(Maybe [JobDependency])
    , _jdContainer     :: !(Maybe ContainerDetail)
    , _jdParameters    :: !(Maybe (Map Text Text))
    , _jdStatusReason  :: !(Maybe Text)
    , _jdJobName       :: !Text
    , _jdJobId         :: !Text
    , _jdJobQueue      :: !Text
    , _jdStatus        :: !JobStatus
    , _jdStartedAt     :: !Integer
    , _jdJobDefinition :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'JobDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jdStoppedAt' - The Unix timestamp for when the job was stopped (when the task transitioned from the @RUNNING@ state to the @STOPPED@ state).
--
-- * 'jdCreatedAt' - The Unix timestamp for when the job was created (when the task entered the @PENDING@ state).
--
-- * 'jdRetryStrategy' - The retry strategy to use for this job if an attempt fails.
--
-- * 'jdAttempts' - A list of job attempts associated with this job.
--
-- * 'jdDependsOn' - A list of job names or IDs on which this job depends.
--
-- * 'jdContainer' - An object representing the details of the container that is associated with the job.
--
-- * 'jdParameters' - Additional parameters passed to the job that replace parameter substitution placeholders or override any corresponding parameter defaults from the job definition.
--
-- * 'jdStatusReason' - A short, human-readable string to provide additional details about the current status of the job.
--
-- * 'jdJobName' - The name of the job.
--
-- * 'jdJobId' - The ID for the job.
--
-- * 'jdJobQueue' - The Amazon Resource Name (ARN) of the job queue with which the job is associated.
--
-- * 'jdStatus' - The current status for the job.
--
-- * 'jdStartedAt' - The Unix timestamp for when the job was started (when the task transitioned from the @PENDING@ state to the @RUNNING@ state).
--
-- * 'jdJobDefinition' - The job definition that is used by this job.
jobDetail
    :: Text -- ^ 'jdJobName'
    -> Text -- ^ 'jdJobId'
    -> Text -- ^ 'jdJobQueue'
    -> JobStatus -- ^ 'jdStatus'
    -> Integer -- ^ 'jdStartedAt'
    -> Text -- ^ 'jdJobDefinition'
    -> JobDetail
jobDetail pJobName_ pJobId_ pJobQueue_ pStatus_ pStartedAt_ pJobDefinition_ =
    JobDetail'
    { _jdStoppedAt = Nothing
    , _jdCreatedAt = Nothing
    , _jdRetryStrategy = Nothing
    , _jdAttempts = Nothing
    , _jdDependsOn = Nothing
    , _jdContainer = Nothing
    , _jdParameters = Nothing
    , _jdStatusReason = Nothing
    , _jdJobName = pJobName_
    , _jdJobId = pJobId_
    , _jdJobQueue = pJobQueue_
    , _jdStatus = pStatus_
    , _jdStartedAt = pStartedAt_
    , _jdJobDefinition = pJobDefinition_
    }

-- | The Unix timestamp for when the job was stopped (when the task transitioned from the @RUNNING@ state to the @STOPPED@ state).
jdStoppedAt :: Lens' JobDetail (Maybe Integer)
jdStoppedAt = lens _jdStoppedAt (\ s a -> s{_jdStoppedAt = a});

-- | The Unix timestamp for when the job was created (when the task entered the @PENDING@ state).
jdCreatedAt :: Lens' JobDetail (Maybe Integer)
jdCreatedAt = lens _jdCreatedAt (\ s a -> s{_jdCreatedAt = a});

-- | The retry strategy to use for this job if an attempt fails.
jdRetryStrategy :: Lens' JobDetail (Maybe RetryStrategy)
jdRetryStrategy = lens _jdRetryStrategy (\ s a -> s{_jdRetryStrategy = a});

-- | A list of job attempts associated with this job.
jdAttempts :: Lens' JobDetail [AttemptDetail]
jdAttempts = lens _jdAttempts (\ s a -> s{_jdAttempts = a}) . _Default . _Coerce;

-- | A list of job names or IDs on which this job depends.
jdDependsOn :: Lens' JobDetail [JobDependency]
jdDependsOn = lens _jdDependsOn (\ s a -> s{_jdDependsOn = a}) . _Default . _Coerce;

-- | An object representing the details of the container that is associated with the job.
jdContainer :: Lens' JobDetail (Maybe ContainerDetail)
jdContainer = lens _jdContainer (\ s a -> s{_jdContainer = a});

-- | Additional parameters passed to the job that replace parameter substitution placeholders or override any corresponding parameter defaults from the job definition.
jdParameters :: Lens' JobDetail (HashMap Text Text)
jdParameters = lens _jdParameters (\ s a -> s{_jdParameters = a}) . _Default . _Map;

-- | A short, human-readable string to provide additional details about the current status of the job.
jdStatusReason :: Lens' JobDetail (Maybe Text)
jdStatusReason = lens _jdStatusReason (\ s a -> s{_jdStatusReason = a});

-- | The name of the job.
jdJobName :: Lens' JobDetail Text
jdJobName = lens _jdJobName (\ s a -> s{_jdJobName = a});

-- | The ID for the job.
jdJobId :: Lens' JobDetail Text
jdJobId = lens _jdJobId (\ s a -> s{_jdJobId = a});

-- | The Amazon Resource Name (ARN) of the job queue with which the job is associated.
jdJobQueue :: Lens' JobDetail Text
jdJobQueue = lens _jdJobQueue (\ s a -> s{_jdJobQueue = a});

-- | The current status for the job.
jdStatus :: Lens' JobDetail JobStatus
jdStatus = lens _jdStatus (\ s a -> s{_jdStatus = a});

-- | The Unix timestamp for when the job was started (when the task transitioned from the @PENDING@ state to the @RUNNING@ state).
jdStartedAt :: Lens' JobDetail Integer
jdStartedAt = lens _jdStartedAt (\ s a -> s{_jdStartedAt = a});

-- | The job definition that is used by this job.
jdJobDefinition :: Lens' JobDetail Text
jdJobDefinition = lens _jdJobDefinition (\ s a -> s{_jdJobDefinition = a});

instance FromJSON JobDetail where
        parseJSON
          = withObject "JobDetail"
              (\ x ->
                 JobDetail' <$>
                   (x .:? "stoppedAt") <*> (x .:? "createdAt") <*>
                     (x .:? "retryStrategy")
                     <*> (x .:? "attempts" .!= mempty)
                     <*> (x .:? "dependsOn" .!= mempty)
                     <*> (x .:? "container")
                     <*> (x .:? "parameters" .!= mempty)
                     <*> (x .:? "statusReason")
                     <*> (x .: "jobName")
                     <*> (x .: "jobId")
                     <*> (x .: "jobQueue")
                     <*> (x .: "status")
                     <*> (x .: "startedAt")
                     <*> (x .: "jobDefinition"))

instance Hashable JobDetail

instance NFData JobDetail

-- | An object representing the details of an AWS Batch job queue.
--
--
--
-- /See:/ 'jobQueueDetail' smart constructor.
data JobQueueDetail = JobQueueDetail'
    { _jqdStatus                  :: !(Maybe JQStatus)
    , _jqdStatusReason            :: !(Maybe Text)
    , _jqdJobQueueName            :: !Text
    , _jqdJobQueueARN             :: !Text
    , _jqdState                   :: !JQState
    , _jqdPriority                :: !Int
    , _jqdComputeEnvironmentOrder :: ![ComputeEnvironmentOrder]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'JobQueueDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jqdStatus' - The status of the job queue (for example, @CREATING@ or @VALID@ ).
--
-- * 'jqdStatusReason' - A short, human-readable string to provide additional details about the current status of the job queue.
--
-- * 'jqdJobQueueName' - The name of the job queue.
--
-- * 'jqdJobQueueARN' - The Amazon Resource Name (ARN) of the job queue.
--
-- * 'jqdState' - Describes the ability of the queue to accept new jobs.
--
-- * 'jqdPriority' - The priority of the job queue.
--
-- * 'jqdComputeEnvironmentOrder' - The compute environments that are attached to the job queue and the order in which job placement is preferred. Compute environments are selected for job placement in ascending order.
jobQueueDetail
    :: Text -- ^ 'jqdJobQueueName'
    -> Text -- ^ 'jqdJobQueueARN'
    -> JQState -- ^ 'jqdState'
    -> Int -- ^ 'jqdPriority'
    -> JobQueueDetail
jobQueueDetail pJobQueueName_ pJobQueueARN_ pState_ pPriority_ =
    JobQueueDetail'
    { _jqdStatus = Nothing
    , _jqdStatusReason = Nothing
    , _jqdJobQueueName = pJobQueueName_
    , _jqdJobQueueARN = pJobQueueARN_
    , _jqdState = pState_
    , _jqdPriority = pPriority_
    , _jqdComputeEnvironmentOrder = mempty
    }

-- | The status of the job queue (for example, @CREATING@ or @VALID@ ).
jqdStatus :: Lens' JobQueueDetail (Maybe JQStatus)
jqdStatus = lens _jqdStatus (\ s a -> s{_jqdStatus = a});

-- | A short, human-readable string to provide additional details about the current status of the job queue.
jqdStatusReason :: Lens' JobQueueDetail (Maybe Text)
jqdStatusReason = lens _jqdStatusReason (\ s a -> s{_jqdStatusReason = a});

-- | The name of the job queue.
jqdJobQueueName :: Lens' JobQueueDetail Text
jqdJobQueueName = lens _jqdJobQueueName (\ s a -> s{_jqdJobQueueName = a});

-- | The Amazon Resource Name (ARN) of the job queue.
jqdJobQueueARN :: Lens' JobQueueDetail Text
jqdJobQueueARN = lens _jqdJobQueueARN (\ s a -> s{_jqdJobQueueARN = a});

-- | Describes the ability of the queue to accept new jobs.
jqdState :: Lens' JobQueueDetail JQState
jqdState = lens _jqdState (\ s a -> s{_jqdState = a});

-- | The priority of the job queue.
jqdPriority :: Lens' JobQueueDetail Int
jqdPriority = lens _jqdPriority (\ s a -> s{_jqdPriority = a});

-- | The compute environments that are attached to the job queue and the order in which job placement is preferred. Compute environments are selected for job placement in ascending order.
jqdComputeEnvironmentOrder :: Lens' JobQueueDetail [ComputeEnvironmentOrder]
jqdComputeEnvironmentOrder = lens _jqdComputeEnvironmentOrder (\ s a -> s{_jqdComputeEnvironmentOrder = a}) . _Coerce;

instance FromJSON JobQueueDetail where
        parseJSON
          = withObject "JobQueueDetail"
              (\ x ->
                 JobQueueDetail' <$>
                   (x .:? "status") <*> (x .:? "statusReason") <*>
                     (x .: "jobQueueName")
                     <*> (x .: "jobQueueArn")
                     <*> (x .: "state")
                     <*> (x .: "priority")
                     <*> (x .:? "computeEnvironmentOrder" .!= mempty))

instance Hashable JobQueueDetail

instance NFData JobQueueDetail

-- | An object representing summary details of a job.
--
--
--
-- /See:/ 'jobSummary' smart constructor.
data JobSummary = JobSummary'
    { _jsJobId   :: !Text
    , _jsJobName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'JobSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jsJobId' - The ID of the job.
--
-- * 'jsJobName' - The name of the job.
jobSummary
    :: Text -- ^ 'jsJobId'
    -> Text -- ^ 'jsJobName'
    -> JobSummary
jobSummary pJobId_ pJobName_ =
    JobSummary'
    { _jsJobId = pJobId_
    , _jsJobName = pJobName_
    }

-- | The ID of the job.
jsJobId :: Lens' JobSummary Text
jsJobId = lens _jsJobId (\ s a -> s{_jsJobId = a});

-- | The name of the job.
jsJobName :: Lens' JobSummary Text
jsJobName = lens _jsJobName (\ s a -> s{_jsJobName = a});

instance FromJSON JobSummary where
        parseJSON
          = withObject "JobSummary"
              (\ x ->
                 JobSummary' <$> (x .: "jobId") <*> (x .: "jobName"))

instance Hashable JobSummary

instance NFData JobSummary

-- | A key-value pair object.
--
--
--
-- /See:/ 'keyValuePair' smart constructor.
data KeyValuePair = KeyValuePair'
    { _kvpValue :: !(Maybe Text)
    , _kvpName  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'KeyValuePair' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kvpValue' - The value of the key value pair. For environment variables, this is the value of the environment variable.
--
-- * 'kvpName' - The name of the key value pair. For environment variables, this is the name of the environment variable.
keyValuePair
    :: KeyValuePair
keyValuePair =
    KeyValuePair'
    { _kvpValue = Nothing
    , _kvpName = Nothing
    }

-- | The value of the key value pair. For environment variables, this is the value of the environment variable.
kvpValue :: Lens' KeyValuePair (Maybe Text)
kvpValue = lens _kvpValue (\ s a -> s{_kvpValue = a});

-- | The name of the key value pair. For environment variables, this is the name of the environment variable.
kvpName :: Lens' KeyValuePair (Maybe Text)
kvpName = lens _kvpName (\ s a -> s{_kvpName = a});

instance FromJSON KeyValuePair where
        parseJSON
          = withObject "KeyValuePair"
              (\ x ->
                 KeyValuePair' <$> (x .:? "value") <*> (x .:? "name"))

instance Hashable KeyValuePair

instance NFData KeyValuePair

instance ToJSON KeyValuePair where
        toJSON KeyValuePair'{..}
          = object
              (catMaybes
                 [("value" .=) <$> _kvpValue,
                  ("name" .=) <$> _kvpName])

-- | Details on a Docker volume mount point that is used in a job's container properties.
--
--
--
-- /See:/ 'mountPoint' smart constructor.
data MountPoint = MountPoint'
    { _mpContainerPath :: !(Maybe Text)
    , _mpSourceVolume  :: !(Maybe Text)
    , _mpReadOnly      :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MountPoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpContainerPath' - The path on the container at which to mount the host volume.
--
-- * 'mpSourceVolume' - The name of the volume to mount.
--
-- * 'mpReadOnly' - If this value is @true@ , the container has read-only access to the volume; otherwise, the container can write to the volume. The default value is @false@ .
mountPoint
    :: MountPoint
mountPoint =
    MountPoint'
    { _mpContainerPath = Nothing
    , _mpSourceVolume = Nothing
    , _mpReadOnly = Nothing
    }

-- | The path on the container at which to mount the host volume.
mpContainerPath :: Lens' MountPoint (Maybe Text)
mpContainerPath = lens _mpContainerPath (\ s a -> s{_mpContainerPath = a});

-- | The name of the volume to mount.
mpSourceVolume :: Lens' MountPoint (Maybe Text)
mpSourceVolume = lens _mpSourceVolume (\ s a -> s{_mpSourceVolume = a});

-- | If this value is @true@ , the container has read-only access to the volume; otherwise, the container can write to the volume. The default value is @false@ .
mpReadOnly :: Lens' MountPoint (Maybe Bool)
mpReadOnly = lens _mpReadOnly (\ s a -> s{_mpReadOnly = a});

instance FromJSON MountPoint where
        parseJSON
          = withObject "MountPoint"
              (\ x ->
                 MountPoint' <$>
                   (x .:? "containerPath") <*> (x .:? "sourceVolume")
                     <*> (x .:? "readOnly"))

instance Hashable MountPoint

instance NFData MountPoint

instance ToJSON MountPoint where
        toJSON MountPoint'{..}
          = object
              (catMaybes
                 [("containerPath" .=) <$> _mpContainerPath,
                  ("sourceVolume" .=) <$> _mpSourceVolume,
                  ("readOnly" .=) <$> _mpReadOnly])

-- | The retry strategy associated with a job.
--
--
--
-- /See:/ 'retryStrategy' smart constructor.
newtype RetryStrategy = RetryStrategy'
    { _rsAttempts :: Maybe Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RetryStrategy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsAttempts' - The number of times to move a job to the @RUNNABLE@ status. You may specify between 1 and 10 attempts. If @attempts@ is greater than one, the job is retried if it fails until it has moved to @RUNNABLE@ that many times.
retryStrategy
    :: RetryStrategy
retryStrategy =
    RetryStrategy'
    { _rsAttempts = Nothing
    }

-- | The number of times to move a job to the @RUNNABLE@ status. You may specify between 1 and 10 attempts. If @attempts@ is greater than one, the job is retried if it fails until it has moved to @RUNNABLE@ that many times.
rsAttempts :: Lens' RetryStrategy (Maybe Int)
rsAttempts = lens _rsAttempts (\ s a -> s{_rsAttempts = a});

instance FromJSON RetryStrategy where
        parseJSON
          = withObject "RetryStrategy"
              (\ x -> RetryStrategy' <$> (x .:? "attempts"))

instance Hashable RetryStrategy

instance NFData RetryStrategy

instance ToJSON RetryStrategy where
        toJSON RetryStrategy'{..}
          = object
              (catMaybes [("attempts" .=) <$> _rsAttempts])

-- | The @ulimit@ settings to pass to the container.
--
--
--
-- /See:/ 'ulimit' smart constructor.
data Ulimit = Ulimit'
    { _uHardLimit :: !Int
    , _uName      :: !Text
    , _uSoftLimit :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Ulimit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uHardLimit' - The hard limit for the @ulimit@ type.
--
-- * 'uName' - The @type@ of the @ulimit@ .
--
-- * 'uSoftLimit' - The soft limit for the @ulimit@ type.
ulimit
    :: Int -- ^ 'uHardLimit'
    -> Text -- ^ 'uName'
    -> Int -- ^ 'uSoftLimit'
    -> Ulimit
ulimit pHardLimit_ pName_ pSoftLimit_ =
    Ulimit'
    { _uHardLimit = pHardLimit_
    , _uName = pName_
    , _uSoftLimit = pSoftLimit_
    }

-- | The hard limit for the @ulimit@ type.
uHardLimit :: Lens' Ulimit Int
uHardLimit = lens _uHardLimit (\ s a -> s{_uHardLimit = a});

-- | The @type@ of the @ulimit@ .
uName :: Lens' Ulimit Text
uName = lens _uName (\ s a -> s{_uName = a});

-- | The soft limit for the @ulimit@ type.
uSoftLimit :: Lens' Ulimit Int
uSoftLimit = lens _uSoftLimit (\ s a -> s{_uSoftLimit = a});

instance FromJSON Ulimit where
        parseJSON
          = withObject "Ulimit"
              (\ x ->
                 Ulimit' <$>
                   (x .: "hardLimit") <*> (x .: "name") <*>
                     (x .: "softLimit"))

instance Hashable Ulimit

instance NFData Ulimit

instance ToJSON Ulimit where
        toJSON Ulimit'{..}
          = object
              (catMaybes
                 [Just ("hardLimit" .= _uHardLimit),
                  Just ("name" .= _uName),
                  Just ("softLimit" .= _uSoftLimit)])

-- | A data volume used in a job's container properties.
--
--
--
-- /See:/ 'volume' smart constructor.
data Volume = Volume'
    { _vName :: !(Maybe Text)
    , _vHost :: !(Maybe Host)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Volume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vName' - The name of the volume. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed. This name is referenced in the @sourceVolume@ parameter of container definition @mountPoints@ .
--
-- * 'vHost' - The contents of the @host@ parameter determine whether your data volume persists on the host container instance and where it is stored. If the host parameter is empty, then the Docker daemon assigns a host path for your data volume, but the data is not guaranteed to persist after the containers associated with it stop running.
volume
    :: Volume
volume =
    Volume'
    { _vName = Nothing
    , _vHost = Nothing
    }

-- | The name of the volume. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed. This name is referenced in the @sourceVolume@ parameter of container definition @mountPoints@ .
vName :: Lens' Volume (Maybe Text)
vName = lens _vName (\ s a -> s{_vName = a});

-- | The contents of the @host@ parameter determine whether your data volume persists on the host container instance and where it is stored. If the host parameter is empty, then the Docker daemon assigns a host path for your data volume, but the data is not guaranteed to persist after the containers associated with it stop running.
vHost :: Lens' Volume (Maybe Host)
vHost = lens _vHost (\ s a -> s{_vHost = a});

instance FromJSON Volume where
        parseJSON
          = withObject "Volume"
              (\ x ->
                 Volume' <$> (x .:? "name") <*> (x .:? "host"))

instance Hashable Volume

instance NFData Volume

instance ToJSON Volume where
        toJSON Volume'{..}
          = object
              (catMaybes
                 [("name" .=) <$> _vName, ("host" .=) <$> _vHost])

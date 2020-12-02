{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.JobDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.JobDetail where

import Network.AWS.Batch.Types.ArrayPropertiesDetail
import Network.AWS.Batch.Types.AttemptDetail
import Network.AWS.Batch.Types.ContainerDetail
import Network.AWS.Batch.Types.JobDependency
import Network.AWS.Batch.Types.JobStatus
import Network.AWS.Batch.Types.JobTimeout
import Network.AWS.Batch.Types.NodeDetails
import Network.AWS.Batch.Types.NodeProperties
import Network.AWS.Batch.Types.RetryStrategy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object representing an AWS Batch job.
--
--
--
-- /See:/ 'jobDetail' smart constructor.
data JobDetail = JobDetail'
  { _jdStoppedAt :: !(Maybe Integer),
    _jdJobARN :: !(Maybe Text),
    _jdCreatedAt :: !(Maybe Integer),
    _jdRetryStrategy :: !(Maybe RetryStrategy),
    _jdAttempts :: !(Maybe [AttemptDetail]),
    _jdStartedAt :: !(Maybe Integer),
    _jdDependsOn :: !(Maybe [JobDependency]),
    _jdContainer :: !(Maybe ContainerDetail),
    _jdNodeDetails :: !(Maybe NodeDetails),
    _jdParameters :: !(Maybe (Map Text (Text))),
    _jdStatusReason :: !(Maybe Text),
    _jdArrayProperties :: !(Maybe ArrayPropertiesDetail),
    _jdTimeout :: !(Maybe JobTimeout),
    _jdNodeProperties :: !(Maybe NodeProperties),
    _jdTags :: !(Maybe (Map Text (Text))),
    _jdJobName :: !Text,
    _jdJobId :: !Text,
    _jdJobQueue :: !Text,
    _jdStatus :: !JobStatus,
    _jdJobDefinition :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JobDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jdStoppedAt' - The Unix timestamp (in milliseconds) for when the job was stopped (when the job transitioned from the @RUNNING@ state to a terminal state, such as @SUCCEEDED@ or @FAILED@ ).
--
-- * 'jdJobARN' - The Amazon Resource Name (ARN) of the job.
--
-- * 'jdCreatedAt' - The Unix timestamp (in milliseconds) for when the job was created. For non-array jobs and parent array jobs, this is when the job entered the @SUBMITTED@ state (at the time 'SubmitJob' was called). For array child jobs, this is when the child job was spawned by its parent and entered the @PENDING@ state.
--
-- * 'jdRetryStrategy' - The retry strategy to use for this job if an attempt fails.
--
-- * 'jdAttempts' - A list of job attempts associated with this job.
--
-- * 'jdStartedAt' - The Unix timestamp (in milliseconds) for when the job was started (when the job transitioned from the @STARTING@ state to the @RUNNING@ state). This parameter is not provided for child jobs of array jobs or multi-node parallel jobs.
--
-- * 'jdDependsOn' - A list of job IDs on which this job depends.
--
-- * 'jdContainer' - An object representing the details of the container that is associated with the job.
--
-- * 'jdNodeDetails' - An object representing the details of a node that is associated with a multi-node parallel job.
--
-- * 'jdParameters' - Additional parameters passed to the job that replace parameter substitution placeholders or override any corresponding parameter defaults from the job definition.
--
-- * 'jdStatusReason' - A short, human-readable string to provide additional details about the current status of the job.
--
-- * 'jdArrayProperties' - The array properties of the job, if it is an array job.
--
-- * 'jdTimeout' - The timeout configuration for the job.
--
-- * 'jdNodeProperties' - An object representing the node properties of a multi-node parallel job.
--
-- * 'jdTags' - The tags applied to the job.
--
-- * 'jdJobName' - The name of the job.
--
-- * 'jdJobId' - The ID for the job.
--
-- * 'jdJobQueue' - The Amazon Resource Name (ARN) of the job queue with which the job is associated.
--
-- * 'jdStatus' - The current status for the job.
--
-- * 'jdJobDefinition' - The job definition that is used by this job.
jobDetail ::
  -- | 'jdJobName'
  Text ->
  -- | 'jdJobId'
  Text ->
  -- | 'jdJobQueue'
  Text ->
  -- | 'jdStatus'
  JobStatus ->
  -- | 'jdJobDefinition'
  Text ->
  JobDetail
jobDetail pJobName_ pJobId_ pJobQueue_ pStatus_ pJobDefinition_ =
  JobDetail'
    { _jdStoppedAt = Nothing,
      _jdJobARN = Nothing,
      _jdCreatedAt = Nothing,
      _jdRetryStrategy = Nothing,
      _jdAttempts = Nothing,
      _jdStartedAt = Nothing,
      _jdDependsOn = Nothing,
      _jdContainer = Nothing,
      _jdNodeDetails = Nothing,
      _jdParameters = Nothing,
      _jdStatusReason = Nothing,
      _jdArrayProperties = Nothing,
      _jdTimeout = Nothing,
      _jdNodeProperties = Nothing,
      _jdTags = Nothing,
      _jdJobName = pJobName_,
      _jdJobId = pJobId_,
      _jdJobQueue = pJobQueue_,
      _jdStatus = pStatus_,
      _jdJobDefinition = pJobDefinition_
    }

-- | The Unix timestamp (in milliseconds) for when the job was stopped (when the job transitioned from the @RUNNING@ state to a terminal state, such as @SUCCEEDED@ or @FAILED@ ).
jdStoppedAt :: Lens' JobDetail (Maybe Integer)
jdStoppedAt = lens _jdStoppedAt (\s a -> s {_jdStoppedAt = a})

-- | The Amazon Resource Name (ARN) of the job.
jdJobARN :: Lens' JobDetail (Maybe Text)
jdJobARN = lens _jdJobARN (\s a -> s {_jdJobARN = a})

-- | The Unix timestamp (in milliseconds) for when the job was created. For non-array jobs and parent array jobs, this is when the job entered the @SUBMITTED@ state (at the time 'SubmitJob' was called). For array child jobs, this is when the child job was spawned by its parent and entered the @PENDING@ state.
jdCreatedAt :: Lens' JobDetail (Maybe Integer)
jdCreatedAt = lens _jdCreatedAt (\s a -> s {_jdCreatedAt = a})

-- | The retry strategy to use for this job if an attempt fails.
jdRetryStrategy :: Lens' JobDetail (Maybe RetryStrategy)
jdRetryStrategy = lens _jdRetryStrategy (\s a -> s {_jdRetryStrategy = a})

-- | A list of job attempts associated with this job.
jdAttempts :: Lens' JobDetail [AttemptDetail]
jdAttempts = lens _jdAttempts (\s a -> s {_jdAttempts = a}) . _Default . _Coerce

-- | The Unix timestamp (in milliseconds) for when the job was started (when the job transitioned from the @STARTING@ state to the @RUNNING@ state). This parameter is not provided for child jobs of array jobs or multi-node parallel jobs.
jdStartedAt :: Lens' JobDetail (Maybe Integer)
jdStartedAt = lens _jdStartedAt (\s a -> s {_jdStartedAt = a})

-- | A list of job IDs on which this job depends.
jdDependsOn :: Lens' JobDetail [JobDependency]
jdDependsOn = lens _jdDependsOn (\s a -> s {_jdDependsOn = a}) . _Default . _Coerce

-- | An object representing the details of the container that is associated with the job.
jdContainer :: Lens' JobDetail (Maybe ContainerDetail)
jdContainer = lens _jdContainer (\s a -> s {_jdContainer = a})

-- | An object representing the details of a node that is associated with a multi-node parallel job.
jdNodeDetails :: Lens' JobDetail (Maybe NodeDetails)
jdNodeDetails = lens _jdNodeDetails (\s a -> s {_jdNodeDetails = a})

-- | Additional parameters passed to the job that replace parameter substitution placeholders or override any corresponding parameter defaults from the job definition.
jdParameters :: Lens' JobDetail (HashMap Text (Text))
jdParameters = lens _jdParameters (\s a -> s {_jdParameters = a}) . _Default . _Map

-- | A short, human-readable string to provide additional details about the current status of the job.
jdStatusReason :: Lens' JobDetail (Maybe Text)
jdStatusReason = lens _jdStatusReason (\s a -> s {_jdStatusReason = a})

-- | The array properties of the job, if it is an array job.
jdArrayProperties :: Lens' JobDetail (Maybe ArrayPropertiesDetail)
jdArrayProperties = lens _jdArrayProperties (\s a -> s {_jdArrayProperties = a})

-- | The timeout configuration for the job.
jdTimeout :: Lens' JobDetail (Maybe JobTimeout)
jdTimeout = lens _jdTimeout (\s a -> s {_jdTimeout = a})

-- | An object representing the node properties of a multi-node parallel job.
jdNodeProperties :: Lens' JobDetail (Maybe NodeProperties)
jdNodeProperties = lens _jdNodeProperties (\s a -> s {_jdNodeProperties = a})

-- | The tags applied to the job.
jdTags :: Lens' JobDetail (HashMap Text (Text))
jdTags = lens _jdTags (\s a -> s {_jdTags = a}) . _Default . _Map

-- | The name of the job.
jdJobName :: Lens' JobDetail Text
jdJobName = lens _jdJobName (\s a -> s {_jdJobName = a})

-- | The ID for the job.
jdJobId :: Lens' JobDetail Text
jdJobId = lens _jdJobId (\s a -> s {_jdJobId = a})

-- | The Amazon Resource Name (ARN) of the job queue with which the job is associated.
jdJobQueue :: Lens' JobDetail Text
jdJobQueue = lens _jdJobQueue (\s a -> s {_jdJobQueue = a})

-- | The current status for the job.
jdStatus :: Lens' JobDetail JobStatus
jdStatus = lens _jdStatus (\s a -> s {_jdStatus = a})

-- | The job definition that is used by this job.
jdJobDefinition :: Lens' JobDetail Text
jdJobDefinition = lens _jdJobDefinition (\s a -> s {_jdJobDefinition = a})

instance FromJSON JobDetail where
  parseJSON =
    withObject
      "JobDetail"
      ( \x ->
          JobDetail'
            <$> (x .:? "stoppedAt")
            <*> (x .:? "jobArn")
            <*> (x .:? "createdAt")
            <*> (x .:? "retryStrategy")
            <*> (x .:? "attempts" .!= mempty)
            <*> (x .:? "startedAt")
            <*> (x .:? "dependsOn" .!= mempty)
            <*> (x .:? "container")
            <*> (x .:? "nodeDetails")
            <*> (x .:? "parameters" .!= mempty)
            <*> (x .:? "statusReason")
            <*> (x .:? "arrayProperties")
            <*> (x .:? "timeout")
            <*> (x .:? "nodeProperties")
            <*> (x .:? "tags" .!= mempty)
            <*> (x .: "jobName")
            <*> (x .: "jobId")
            <*> (x .: "jobQueue")
            <*> (x .: "status")
            <*> (x .: "jobDefinition")
      )

instance Hashable JobDetail

instance NFData JobDetail

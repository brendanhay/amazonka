{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.SubmitJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Submits an AWS Batch job from a job definition. Parameters specified during 'SubmitJob' override parameters defined in the job definition.
--
--
module Network.AWS.Batch.SubmitJob
    (
    -- * Creating a Request
      submitJob
    , SubmitJob
    -- * Request Lenses
    , sjContainerOverrides
    , sjRetryStrategy
    , sjDependsOn
    , sjParameters
    , sjArrayProperties
    , sjTimeout
    , sjJobName
    , sjJobQueue
    , sjJobDefinition

    -- * Destructuring the Response
    , submitJobResponse
    , SubmitJobResponse
    -- * Response Lenses
    , sjrsResponseStatus
    , sjrsJobName
    , sjrsJobId
    ) where

import Network.AWS.Batch.Types
import Network.AWS.Batch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'submitJob' smart constructor.
data SubmitJob = SubmitJob'
  { _sjContainerOverrides :: !(Maybe ContainerOverrides)
  , _sjRetryStrategy      :: !(Maybe RetryStrategy)
  , _sjDependsOn          :: !(Maybe [JobDependency])
  , _sjParameters         :: !(Maybe (Map Text Text))
  , _sjArrayProperties    :: !(Maybe ArrayProperties)
  , _sjTimeout            :: !(Maybe JobTimeout)
  , _sjJobName            :: !Text
  , _sjJobQueue           :: !Text
  , _sjJobDefinition      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SubmitJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sjContainerOverrides' - A list of container overrides in JSON format that specify the name of a container in the specified job definition and the overrides it should receive. You can override the default command for a container (that is specified in the job definition or the Docker image) with a @command@ override. You can also override existing environment variables (that are specified in the job definition or Docker image) on a container or add new environment variables to it with an @environment@ override.
--
-- * 'sjRetryStrategy' - The retry strategy to use for failed jobs from this 'SubmitJob' operation. When a retry strategy is specified here, it overrides the retry strategy defined in the job definition.
--
-- * 'sjDependsOn' - A list of dependencies for the job. A job can depend upon a maximum of 20 jobs. You can specify a @SEQUENTIAL@ type dependency without specifying a job ID for array jobs so that each child array job completes sequentially, starting at index 0. You can also specify an @N_TO_N@ type dependency with a job ID for array jobs so that each index child of this job must wait for the corresponding index child of each dependency to complete before it can begin.
--
-- * 'sjParameters' - Additional parameters passed to the job that replace parameter substitution placeholders that are set in the job definition. Parameters are specified as a key and value pair mapping. Parameters in a @SubmitJob@ request override any corresponding parameter defaults from the job definition.
--
-- * 'sjArrayProperties' - The array properties for the submitted job, such as the size of the array. The array size can be between 2 and 10,000. If you specify array properties for a job, it becomes an array job. For more information, see <http://docs.aws.amazon.com/batch/latest/userguide/array_jobs.html Array Jobs> in the /AWS Batch User Guide/ .
--
-- * 'sjTimeout' - The timeout configuration for this 'SubmitJob' operation. You can specify a timeout duration after which AWS Batch terminates your jobs if they have not finished. If a job is terminated due to a timeout, it is not retried. The minimum value for the timeout is 60 seconds. This configuration overrides any timeout configuration specified in the job definition. For array jobs, child jobs have the same timeout configuration as the parent job. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/job_timeouts.html Job Timeouts> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'sjJobName' - The name of the job. The first character must be alphanumeric, and up to 128 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
--
-- * 'sjJobQueue' - The job queue into which the job is submitted. You can specify either the name or the Amazon Resource Name (ARN) of the queue.
--
-- * 'sjJobDefinition' - The job definition used by this job. This value can be either a @name:revision@ or the Amazon Resource Name (ARN) for the job definition.
submitJob
    :: Text -- ^ 'sjJobName'
    -> Text -- ^ 'sjJobQueue'
    -> Text -- ^ 'sjJobDefinition'
    -> SubmitJob
submitJob pJobName_ pJobQueue_ pJobDefinition_ =
  SubmitJob'
    { _sjContainerOverrides = Nothing
    , _sjRetryStrategy = Nothing
    , _sjDependsOn = Nothing
    , _sjParameters = Nothing
    , _sjArrayProperties = Nothing
    , _sjTimeout = Nothing
    , _sjJobName = pJobName_
    , _sjJobQueue = pJobQueue_
    , _sjJobDefinition = pJobDefinition_
    }


-- | A list of container overrides in JSON format that specify the name of a container in the specified job definition and the overrides it should receive. You can override the default command for a container (that is specified in the job definition or the Docker image) with a @command@ override. You can also override existing environment variables (that are specified in the job definition or Docker image) on a container or add new environment variables to it with an @environment@ override.
sjContainerOverrides :: Lens' SubmitJob (Maybe ContainerOverrides)
sjContainerOverrides = lens _sjContainerOverrides (\ s a -> s{_sjContainerOverrides = a})

-- | The retry strategy to use for failed jobs from this 'SubmitJob' operation. When a retry strategy is specified here, it overrides the retry strategy defined in the job definition.
sjRetryStrategy :: Lens' SubmitJob (Maybe RetryStrategy)
sjRetryStrategy = lens _sjRetryStrategy (\ s a -> s{_sjRetryStrategy = a})

-- | A list of dependencies for the job. A job can depend upon a maximum of 20 jobs. You can specify a @SEQUENTIAL@ type dependency without specifying a job ID for array jobs so that each child array job completes sequentially, starting at index 0. You can also specify an @N_TO_N@ type dependency with a job ID for array jobs so that each index child of this job must wait for the corresponding index child of each dependency to complete before it can begin.
sjDependsOn :: Lens' SubmitJob [JobDependency]
sjDependsOn = lens _sjDependsOn (\ s a -> s{_sjDependsOn = a}) . _Default . _Coerce

-- | Additional parameters passed to the job that replace parameter substitution placeholders that are set in the job definition. Parameters are specified as a key and value pair mapping. Parameters in a @SubmitJob@ request override any corresponding parameter defaults from the job definition.
sjParameters :: Lens' SubmitJob (HashMap Text Text)
sjParameters = lens _sjParameters (\ s a -> s{_sjParameters = a}) . _Default . _Map

-- | The array properties for the submitted job, such as the size of the array. The array size can be between 2 and 10,000. If you specify array properties for a job, it becomes an array job. For more information, see <http://docs.aws.amazon.com/batch/latest/userguide/array_jobs.html Array Jobs> in the /AWS Batch User Guide/ .
sjArrayProperties :: Lens' SubmitJob (Maybe ArrayProperties)
sjArrayProperties = lens _sjArrayProperties (\ s a -> s{_sjArrayProperties = a})

-- | The timeout configuration for this 'SubmitJob' operation. You can specify a timeout duration after which AWS Batch terminates your jobs if they have not finished. If a job is terminated due to a timeout, it is not retried. The minimum value for the timeout is 60 seconds. This configuration overrides any timeout configuration specified in the job definition. For array jobs, child jobs have the same timeout configuration as the parent job. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/job_timeouts.html Job Timeouts> in the /Amazon Elastic Container Service Developer Guide/ .
sjTimeout :: Lens' SubmitJob (Maybe JobTimeout)
sjTimeout = lens _sjTimeout (\ s a -> s{_sjTimeout = a})

-- | The name of the job. The first character must be alphanumeric, and up to 128 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
sjJobName :: Lens' SubmitJob Text
sjJobName = lens _sjJobName (\ s a -> s{_sjJobName = a})

-- | The job queue into which the job is submitted. You can specify either the name or the Amazon Resource Name (ARN) of the queue.
sjJobQueue :: Lens' SubmitJob Text
sjJobQueue = lens _sjJobQueue (\ s a -> s{_sjJobQueue = a})

-- | The job definition used by this job. This value can be either a @name:revision@ or the Amazon Resource Name (ARN) for the job definition.
sjJobDefinition :: Lens' SubmitJob Text
sjJobDefinition = lens _sjJobDefinition (\ s a -> s{_sjJobDefinition = a})

instance AWSRequest SubmitJob where
        type Rs SubmitJob = SubmitJobResponse
        request = postJSON batch
        response
          = receiveJSON
              (\ s h x ->
                 SubmitJobResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "jobName") <*>
                     (x .:> "jobId"))

instance Hashable SubmitJob where

instance NFData SubmitJob where

instance ToHeaders SubmitJob where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SubmitJob where
        toJSON SubmitJob'{..}
          = object
              (catMaybes
                 [("containerOverrides" .=) <$> _sjContainerOverrides,
                  ("retryStrategy" .=) <$> _sjRetryStrategy,
                  ("dependsOn" .=) <$> _sjDependsOn,
                  ("parameters" .=) <$> _sjParameters,
                  ("arrayProperties" .=) <$> _sjArrayProperties,
                  ("timeout" .=) <$> _sjTimeout,
                  Just ("jobName" .= _sjJobName),
                  Just ("jobQueue" .= _sjJobQueue),
                  Just ("jobDefinition" .= _sjJobDefinition)])

instance ToPath SubmitJob where
        toPath = const "/v1/submitjob"

instance ToQuery SubmitJob where
        toQuery = const mempty

-- | /See:/ 'submitJobResponse' smart constructor.
data SubmitJobResponse = SubmitJobResponse'
  { _sjrsResponseStatus :: !Int
  , _sjrsJobName        :: !Text
  , _sjrsJobId          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SubmitJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sjrsResponseStatus' - -- | The response status code.
--
-- * 'sjrsJobName' - The name of the job.
--
-- * 'sjrsJobId' - The unique identifier for the job.
submitJobResponse
    :: Int -- ^ 'sjrsResponseStatus'
    -> Text -- ^ 'sjrsJobName'
    -> Text -- ^ 'sjrsJobId'
    -> SubmitJobResponse
submitJobResponse pResponseStatus_ pJobName_ pJobId_ =
  SubmitJobResponse'
    { _sjrsResponseStatus = pResponseStatus_
    , _sjrsJobName = pJobName_
    , _sjrsJobId = pJobId_
    }


-- | -- | The response status code.
sjrsResponseStatus :: Lens' SubmitJobResponse Int
sjrsResponseStatus = lens _sjrsResponseStatus (\ s a -> s{_sjrsResponseStatus = a})

-- | The name of the job.
sjrsJobName :: Lens' SubmitJobResponse Text
sjrsJobName = lens _sjrsJobName (\ s a -> s{_sjrsJobName = a})

-- | The unique identifier for the job.
sjrsJobId :: Lens' SubmitJobResponse Text
sjrsJobId = lens _sjrsJobId (\ s a -> s{_sjrsJobId = a})

instance NFData SubmitJobResponse where

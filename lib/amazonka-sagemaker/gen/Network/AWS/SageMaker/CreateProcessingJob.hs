{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateProcessingJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a processing job.
module Network.AWS.SageMaker.CreateProcessingJob
  ( -- * Creating a Request
    createProcessingJob,
    CreateProcessingJob,

    -- * Request Lenses
    cpjEnvironment,
    cpjStoppingCondition,
    cpjExperimentConfig,
    cpjProcessingInputs,
    cpjNetworkConfig,
    cpjProcessingOutputConfig,
    cpjTags,
    cpjProcessingJobName,
    cpjProcessingResources,
    cpjAppSpecification,
    cpjRoleARN,

    -- * Destructuring the Response
    createProcessingJobResponse,
    CreateProcessingJobResponse,

    -- * Response Lenses
    cpjrsResponseStatus,
    cpjrsProcessingJobARN,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'createProcessingJob' smart constructor.
data CreateProcessingJob = CreateProcessingJob'
  { _cpjEnvironment ::
      !(Maybe (Map Text (Text))),
    _cpjStoppingCondition ::
      !(Maybe ProcessingStoppingCondition),
    _cpjExperimentConfig :: !(Maybe ExperimentConfig),
    _cpjProcessingInputs :: !(Maybe [ProcessingInput]),
    _cpjNetworkConfig :: !(Maybe NetworkConfig),
    _cpjProcessingOutputConfig ::
      !(Maybe ProcessingOutputConfig),
    _cpjTags :: !(Maybe [Tag]),
    _cpjProcessingJobName :: !Text,
    _cpjProcessingResources :: !ProcessingResources,
    _cpjAppSpecification :: !AppSpecification,
    _cpjRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateProcessingJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpjEnvironment' - Sets the environment variables in the Docker container.
--
-- * 'cpjStoppingCondition' - The time limit for how long the processing job is allowed to run.
--
-- * 'cpjExperimentConfig' - Undocumented member.
--
-- * 'cpjProcessingInputs' - For each input, data is downloaded from S3 into the processing container before the processing job begins running if "S3InputMode" is set to @File@ .
--
-- * 'cpjNetworkConfig' - Networking options for a processing job.
--
-- * 'cpjProcessingOutputConfig' - Output configuration for the processing job.
--
-- * 'cpjTags' - (Optional) An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- * 'cpjProcessingJobName' - The name of the processing job. The name must be unique within an AWS Region in the AWS account.
--
-- * 'cpjProcessingResources' - Identifies the resources, ML compute instances, and ML storage volumes to deploy for a processing job. In distributed training, you specify more than one instance.
--
-- * 'cpjAppSpecification' - Configures the processing job to run a specified Docker container image.
--
-- * 'cpjRoleARN' - The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can assume to perform tasks on your behalf.
createProcessingJob ::
  -- | 'cpjProcessingJobName'
  Text ->
  -- | 'cpjProcessingResources'
  ProcessingResources ->
  -- | 'cpjAppSpecification'
  AppSpecification ->
  -- | 'cpjRoleARN'
  Text ->
  CreateProcessingJob
createProcessingJob
  pProcessingJobName_
  pProcessingResources_
  pAppSpecification_
  pRoleARN_ =
    CreateProcessingJob'
      { _cpjEnvironment = Nothing,
        _cpjStoppingCondition = Nothing,
        _cpjExperimentConfig = Nothing,
        _cpjProcessingInputs = Nothing,
        _cpjNetworkConfig = Nothing,
        _cpjProcessingOutputConfig = Nothing,
        _cpjTags = Nothing,
        _cpjProcessingJobName = pProcessingJobName_,
        _cpjProcessingResources = pProcessingResources_,
        _cpjAppSpecification = pAppSpecification_,
        _cpjRoleARN = pRoleARN_
      }

-- | Sets the environment variables in the Docker container.
cpjEnvironment :: Lens' CreateProcessingJob (HashMap Text (Text))
cpjEnvironment = lens _cpjEnvironment (\s a -> s {_cpjEnvironment = a}) . _Default . _Map

-- | The time limit for how long the processing job is allowed to run.
cpjStoppingCondition :: Lens' CreateProcessingJob (Maybe ProcessingStoppingCondition)
cpjStoppingCondition = lens _cpjStoppingCondition (\s a -> s {_cpjStoppingCondition = a})

-- | Undocumented member.
cpjExperimentConfig :: Lens' CreateProcessingJob (Maybe ExperimentConfig)
cpjExperimentConfig = lens _cpjExperimentConfig (\s a -> s {_cpjExperimentConfig = a})

-- | For each input, data is downloaded from S3 into the processing container before the processing job begins running if "S3InputMode" is set to @File@ .
cpjProcessingInputs :: Lens' CreateProcessingJob [ProcessingInput]
cpjProcessingInputs = lens _cpjProcessingInputs (\s a -> s {_cpjProcessingInputs = a}) . _Default . _Coerce

-- | Networking options for a processing job.
cpjNetworkConfig :: Lens' CreateProcessingJob (Maybe NetworkConfig)
cpjNetworkConfig = lens _cpjNetworkConfig (\s a -> s {_cpjNetworkConfig = a})

-- | Output configuration for the processing job.
cpjProcessingOutputConfig :: Lens' CreateProcessingJob (Maybe ProcessingOutputConfig)
cpjProcessingOutputConfig = lens _cpjProcessingOutputConfig (\s a -> s {_cpjProcessingOutputConfig = a})

-- | (Optional) An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
cpjTags :: Lens' CreateProcessingJob [Tag]
cpjTags = lens _cpjTags (\s a -> s {_cpjTags = a}) . _Default . _Coerce

-- | The name of the processing job. The name must be unique within an AWS Region in the AWS account.
cpjProcessingJobName :: Lens' CreateProcessingJob Text
cpjProcessingJobName = lens _cpjProcessingJobName (\s a -> s {_cpjProcessingJobName = a})

-- | Identifies the resources, ML compute instances, and ML storage volumes to deploy for a processing job. In distributed training, you specify more than one instance.
cpjProcessingResources :: Lens' CreateProcessingJob ProcessingResources
cpjProcessingResources = lens _cpjProcessingResources (\s a -> s {_cpjProcessingResources = a})

-- | Configures the processing job to run a specified Docker container image.
cpjAppSpecification :: Lens' CreateProcessingJob AppSpecification
cpjAppSpecification = lens _cpjAppSpecification (\s a -> s {_cpjAppSpecification = a})

-- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can assume to perform tasks on your behalf.
cpjRoleARN :: Lens' CreateProcessingJob Text
cpjRoleARN = lens _cpjRoleARN (\s a -> s {_cpjRoleARN = a})

instance AWSRequest CreateProcessingJob where
  type Rs CreateProcessingJob = CreateProcessingJobResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          CreateProcessingJobResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "ProcessingJobArn")
      )

instance Hashable CreateProcessingJob

instance NFData CreateProcessingJob

instance ToHeaders CreateProcessingJob where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.CreateProcessingJob" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateProcessingJob where
  toJSON CreateProcessingJob' {..} =
    object
      ( catMaybes
          [ ("Environment" .=) <$> _cpjEnvironment,
            ("StoppingCondition" .=) <$> _cpjStoppingCondition,
            ("ExperimentConfig" .=) <$> _cpjExperimentConfig,
            ("ProcessingInputs" .=) <$> _cpjProcessingInputs,
            ("NetworkConfig" .=) <$> _cpjNetworkConfig,
            ("ProcessingOutputConfig" .=) <$> _cpjProcessingOutputConfig,
            ("Tags" .=) <$> _cpjTags,
            Just ("ProcessingJobName" .= _cpjProcessingJobName),
            Just ("ProcessingResources" .= _cpjProcessingResources),
            Just ("AppSpecification" .= _cpjAppSpecification),
            Just ("RoleArn" .= _cpjRoleARN)
          ]
      )

instance ToPath CreateProcessingJob where
  toPath = const "/"

instance ToQuery CreateProcessingJob where
  toQuery = const mempty

-- | /See:/ 'createProcessingJobResponse' smart constructor.
data CreateProcessingJobResponse = CreateProcessingJobResponse'
  { _cpjrsResponseStatus ::
      !Int,
    _cpjrsProcessingJobARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateProcessingJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpjrsResponseStatus' - -- | The response status code.
--
-- * 'cpjrsProcessingJobARN' - The Amazon Resource Name (ARN) of the processing job.
createProcessingJobResponse ::
  -- | 'cpjrsResponseStatus'
  Int ->
  -- | 'cpjrsProcessingJobARN'
  Text ->
  CreateProcessingJobResponse
createProcessingJobResponse pResponseStatus_ pProcessingJobARN_ =
  CreateProcessingJobResponse'
    { _cpjrsResponseStatus =
        pResponseStatus_,
      _cpjrsProcessingJobARN = pProcessingJobARN_
    }

-- | -- | The response status code.
cpjrsResponseStatus :: Lens' CreateProcessingJobResponse Int
cpjrsResponseStatus = lens _cpjrsResponseStatus (\s a -> s {_cpjrsResponseStatus = a})

-- | The Amazon Resource Name (ARN) of the processing job.
cpjrsProcessingJobARN :: Lens' CreateProcessingJobResponse Text
cpjrsProcessingJobARN = lens _cpjrsProcessingJobARN (\s a -> s {_cpjrsProcessingJobARN = a})

instance NFData CreateProcessingJobResponse

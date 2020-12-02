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
-- Module      : Network.AWS.SageMaker.CreateTransformJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a transform job. A transform job uses a trained model to get inferences on a dataset and saves these results to an Amazon S3 location that you specify.
--
--
-- To perform batch transformations, you create a transform job and use the data that you have readily available.
--
-- In the request body, you provide the following:
--
--     * @TransformJobName@ - Identifies the transform job. The name must be unique within an AWS Region in an AWS account.
--
--     * @ModelName@ - Identifies the model to use. @ModelName@ must be the name of an existing Amazon SageMaker model in the same AWS Region and AWS account. For information on creating a model, see 'CreateModel' .
--
--     * @TransformInput@ - Describes the dataset to be transformed and the Amazon S3 location where it is stored.
--
--     * @TransformOutput@ - Identifies the Amazon S3 location where you want Amazon SageMaker to save the results from the transform job.
--
--     * @TransformResources@ - Identifies the ML compute instances for the transform job.
--
--
--
-- For more information about how batch transformation works, see <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-transform.html Batch Transform> .
module Network.AWS.SageMaker.CreateTransformJob
  ( -- * Creating a Request
    createTransformJob,
    CreateTransformJob,

    -- * Request Lenses
    ctjModelClientConfig,
    ctjBatchStrategy,
    ctjMaxPayloadInMB,
    ctjEnvironment,
    ctjExperimentConfig,
    ctjMaxConcurrentTransforms,
    ctjDataProcessing,
    ctjTags,
    ctjTransformJobName,
    ctjModelName,
    ctjTransformInput,
    ctjTransformOutput,
    ctjTransformResources,

    -- * Destructuring the Response
    createTransformJobResponse,
    CreateTransformJobResponse,

    -- * Response Lenses
    ctjrsResponseStatus,
    ctjrsTransformJobARN,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'createTransformJob' smart constructor.
data CreateTransformJob = CreateTransformJob'
  { _ctjModelClientConfig ::
      !(Maybe ModelClientConfig),
    _ctjBatchStrategy :: !(Maybe BatchStrategy),
    _ctjMaxPayloadInMB :: !(Maybe Nat),
    _ctjEnvironment :: !(Maybe (Map Text (Text))),
    _ctjExperimentConfig :: !(Maybe ExperimentConfig),
    _ctjMaxConcurrentTransforms :: !(Maybe Nat),
    _ctjDataProcessing :: !(Maybe DataProcessing),
    _ctjTags :: !(Maybe [Tag]),
    _ctjTransformJobName :: !Text,
    _ctjModelName :: !Text,
    _ctjTransformInput :: !TransformInput,
    _ctjTransformOutput :: !TransformOutput,
    _ctjTransformResources :: !TransformResources
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTransformJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctjModelClientConfig' - Configures the timeout and maximum number of retries for processing a transform job invocation.
--
-- * 'ctjBatchStrategy' - Specifies the number of records to include in a mini-batch for an HTTP inference request. A /record/ // is a single unit of input data that inference can be made on. For example, a single line in a CSV file is a record.  To enable the batch strategy, you must set the @SplitType@ property to @Line@ , @RecordIO@ , or @TFRecord@ . To use only one record when making an HTTP invocation request to a container, set @BatchStrategy@ to @SingleRecord@ and @SplitType@ to @Line@ . To fit as many records in a mini-batch as can fit within the @MaxPayloadInMB@ limit, set @BatchStrategy@ to @MultiRecord@ and @SplitType@ to @Line@ .
--
-- * 'ctjMaxPayloadInMB' - The maximum allowed size of the payload, in MB. A /payload/ is the data portion of a record (without metadata). The value in @MaxPayloadInMB@ must be greater than, or equal to, the size of a single record. To estimate the size of a record in MB, divide the size of your dataset by the number of records. To ensure that the records fit within the maximum payload size, we recommend using a slightly larger value. The default value is @6@ MB.  For cases where the payload might be arbitrarily large and is transmitted using HTTP chunked encoding, set the value to @0@ . This feature works only in supported algorithms. Currently, Amazon SageMaker built-in algorithms do not support HTTP chunked encoding.
--
-- * 'ctjEnvironment' - The environment variables to set in the Docker container. We support up to 16 key and values entries in the map.
--
-- * 'ctjExperimentConfig' - Undocumented member.
--
-- * 'ctjMaxConcurrentTransforms' - The maximum number of parallel requests that can be sent to each instance in a transform job. If @MaxConcurrentTransforms@ is set to @0@ or left unset, Amazon SageMaker checks the optional execution-parameters to determine the settings for your chosen algorithm. If the execution-parameters endpoint is not enabled, the default value is @1@ . For more information on execution-parameters, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms-batch-code.html#your-algorithms-batch-code-how-containe-serves-requests How Containers Serve Requests> . For built-in algorithms, you don't need to set a value for @MaxConcurrentTransforms@ .
--
-- * 'ctjDataProcessing' - The data structure used to specify the data to be used for inference in a batch transform job and to associate the data that is relevant to the prediction results in the output. The input filter provided allows you to exclude input data that is not needed for inference in a batch transform job. The output filter provided allows you to include input data relevant to interpreting the predictions in the output from the job. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-transform-data-processing.html Associate Prediction Results with their Corresponding Input Records> .
--
-- * 'ctjTags' - (Optional) An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- * 'ctjTransformJobName' - The name of the transform job. The name must be unique within an AWS Region in an AWS account.
--
-- * 'ctjModelName' - The name of the model that you want to use for the transform job. @ModelName@ must be the name of an existing Amazon SageMaker model within an AWS Region in an AWS account.
--
-- * 'ctjTransformInput' - Describes the input source and the way the transform job consumes it.
--
-- * 'ctjTransformOutput' - Describes the results of the transform job.
--
-- * 'ctjTransformResources' - Describes the resources, including ML instance types and ML instance count, to use for the transform job.
createTransformJob ::
  -- | 'ctjTransformJobName'
  Text ->
  -- | 'ctjModelName'
  Text ->
  -- | 'ctjTransformInput'
  TransformInput ->
  -- | 'ctjTransformOutput'
  TransformOutput ->
  -- | 'ctjTransformResources'
  TransformResources ->
  CreateTransformJob
createTransformJob
  pTransformJobName_
  pModelName_
  pTransformInput_
  pTransformOutput_
  pTransformResources_ =
    CreateTransformJob'
      { _ctjModelClientConfig = Nothing,
        _ctjBatchStrategy = Nothing,
        _ctjMaxPayloadInMB = Nothing,
        _ctjEnvironment = Nothing,
        _ctjExperimentConfig = Nothing,
        _ctjMaxConcurrentTransforms = Nothing,
        _ctjDataProcessing = Nothing,
        _ctjTags = Nothing,
        _ctjTransformJobName = pTransformJobName_,
        _ctjModelName = pModelName_,
        _ctjTransformInput = pTransformInput_,
        _ctjTransformOutput = pTransformOutput_,
        _ctjTransformResources = pTransformResources_
      }

-- | Configures the timeout and maximum number of retries for processing a transform job invocation.
ctjModelClientConfig :: Lens' CreateTransformJob (Maybe ModelClientConfig)
ctjModelClientConfig = lens _ctjModelClientConfig (\s a -> s {_ctjModelClientConfig = a})

-- | Specifies the number of records to include in a mini-batch for an HTTP inference request. A /record/ // is a single unit of input data that inference can be made on. For example, a single line in a CSV file is a record.  To enable the batch strategy, you must set the @SplitType@ property to @Line@ , @RecordIO@ , or @TFRecord@ . To use only one record when making an HTTP invocation request to a container, set @BatchStrategy@ to @SingleRecord@ and @SplitType@ to @Line@ . To fit as many records in a mini-batch as can fit within the @MaxPayloadInMB@ limit, set @BatchStrategy@ to @MultiRecord@ and @SplitType@ to @Line@ .
ctjBatchStrategy :: Lens' CreateTransformJob (Maybe BatchStrategy)
ctjBatchStrategy = lens _ctjBatchStrategy (\s a -> s {_ctjBatchStrategy = a})

-- | The maximum allowed size of the payload, in MB. A /payload/ is the data portion of a record (without metadata). The value in @MaxPayloadInMB@ must be greater than, or equal to, the size of a single record. To estimate the size of a record in MB, divide the size of your dataset by the number of records. To ensure that the records fit within the maximum payload size, we recommend using a slightly larger value. The default value is @6@ MB.  For cases where the payload might be arbitrarily large and is transmitted using HTTP chunked encoding, set the value to @0@ . This feature works only in supported algorithms. Currently, Amazon SageMaker built-in algorithms do not support HTTP chunked encoding.
ctjMaxPayloadInMB :: Lens' CreateTransformJob (Maybe Natural)
ctjMaxPayloadInMB = lens _ctjMaxPayloadInMB (\s a -> s {_ctjMaxPayloadInMB = a}) . mapping _Nat

-- | The environment variables to set in the Docker container. We support up to 16 key and values entries in the map.
ctjEnvironment :: Lens' CreateTransformJob (HashMap Text (Text))
ctjEnvironment = lens _ctjEnvironment (\s a -> s {_ctjEnvironment = a}) . _Default . _Map

-- | Undocumented member.
ctjExperimentConfig :: Lens' CreateTransformJob (Maybe ExperimentConfig)
ctjExperimentConfig = lens _ctjExperimentConfig (\s a -> s {_ctjExperimentConfig = a})

-- | The maximum number of parallel requests that can be sent to each instance in a transform job. If @MaxConcurrentTransforms@ is set to @0@ or left unset, Amazon SageMaker checks the optional execution-parameters to determine the settings for your chosen algorithm. If the execution-parameters endpoint is not enabled, the default value is @1@ . For more information on execution-parameters, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms-batch-code.html#your-algorithms-batch-code-how-containe-serves-requests How Containers Serve Requests> . For built-in algorithms, you don't need to set a value for @MaxConcurrentTransforms@ .
ctjMaxConcurrentTransforms :: Lens' CreateTransformJob (Maybe Natural)
ctjMaxConcurrentTransforms = lens _ctjMaxConcurrentTransforms (\s a -> s {_ctjMaxConcurrentTransforms = a}) . mapping _Nat

-- | The data structure used to specify the data to be used for inference in a batch transform job and to associate the data that is relevant to the prediction results in the output. The input filter provided allows you to exclude input data that is not needed for inference in a batch transform job. The output filter provided allows you to include input data relevant to interpreting the predictions in the output from the job. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-transform-data-processing.html Associate Prediction Results with their Corresponding Input Records> .
ctjDataProcessing :: Lens' CreateTransformJob (Maybe DataProcessing)
ctjDataProcessing = lens _ctjDataProcessing (\s a -> s {_ctjDataProcessing = a})

-- | (Optional) An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
ctjTags :: Lens' CreateTransformJob [Tag]
ctjTags = lens _ctjTags (\s a -> s {_ctjTags = a}) . _Default . _Coerce

-- | The name of the transform job. The name must be unique within an AWS Region in an AWS account.
ctjTransformJobName :: Lens' CreateTransformJob Text
ctjTransformJobName = lens _ctjTransformJobName (\s a -> s {_ctjTransformJobName = a})

-- | The name of the model that you want to use for the transform job. @ModelName@ must be the name of an existing Amazon SageMaker model within an AWS Region in an AWS account.
ctjModelName :: Lens' CreateTransformJob Text
ctjModelName = lens _ctjModelName (\s a -> s {_ctjModelName = a})

-- | Describes the input source and the way the transform job consumes it.
ctjTransformInput :: Lens' CreateTransformJob TransformInput
ctjTransformInput = lens _ctjTransformInput (\s a -> s {_ctjTransformInput = a})

-- | Describes the results of the transform job.
ctjTransformOutput :: Lens' CreateTransformJob TransformOutput
ctjTransformOutput = lens _ctjTransformOutput (\s a -> s {_ctjTransformOutput = a})

-- | Describes the resources, including ML instance types and ML instance count, to use for the transform job.
ctjTransformResources :: Lens' CreateTransformJob TransformResources
ctjTransformResources = lens _ctjTransformResources (\s a -> s {_ctjTransformResources = a})

instance AWSRequest CreateTransformJob where
  type Rs CreateTransformJob = CreateTransformJobResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          CreateTransformJobResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "TransformJobArn")
      )

instance Hashable CreateTransformJob

instance NFData CreateTransformJob

instance ToHeaders CreateTransformJob where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.CreateTransformJob" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateTransformJob where
  toJSON CreateTransformJob' {..} =
    object
      ( catMaybes
          [ ("ModelClientConfig" .=) <$> _ctjModelClientConfig,
            ("BatchStrategy" .=) <$> _ctjBatchStrategy,
            ("MaxPayloadInMB" .=) <$> _ctjMaxPayloadInMB,
            ("Environment" .=) <$> _ctjEnvironment,
            ("ExperimentConfig" .=) <$> _ctjExperimentConfig,
            ("MaxConcurrentTransforms" .=) <$> _ctjMaxConcurrentTransforms,
            ("DataProcessing" .=) <$> _ctjDataProcessing,
            ("Tags" .=) <$> _ctjTags,
            Just ("TransformJobName" .= _ctjTransformJobName),
            Just ("ModelName" .= _ctjModelName),
            Just ("TransformInput" .= _ctjTransformInput),
            Just ("TransformOutput" .= _ctjTransformOutput),
            Just ("TransformResources" .= _ctjTransformResources)
          ]
      )

instance ToPath CreateTransformJob where
  toPath = const "/"

instance ToQuery CreateTransformJob where
  toQuery = const mempty

-- | /See:/ 'createTransformJobResponse' smart constructor.
data CreateTransformJobResponse = CreateTransformJobResponse'
  { _ctjrsResponseStatus ::
      !Int,
    _ctjrsTransformJobARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTransformJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctjrsResponseStatus' - -- | The response status code.
--
-- * 'ctjrsTransformJobARN' - The Amazon Resource Name (ARN) of the transform job.
createTransformJobResponse ::
  -- | 'ctjrsResponseStatus'
  Int ->
  -- | 'ctjrsTransformJobARN'
  Text ->
  CreateTransformJobResponse
createTransformJobResponse pResponseStatus_ pTransformJobARN_ =
  CreateTransformJobResponse'
    { _ctjrsResponseStatus =
        pResponseStatus_,
      _ctjrsTransformJobARN = pTransformJobARN_
    }

-- | -- | The response status code.
ctjrsResponseStatus :: Lens' CreateTransformJobResponse Int
ctjrsResponseStatus = lens _ctjrsResponseStatus (\s a -> s {_ctjrsResponseStatus = a})

-- | The Amazon Resource Name (ARN) of the transform job.
ctjrsTransformJobARN :: Lens' CreateTransformJobResponse Text
ctjrsTransformJobARN = lens _ctjrsTransformJobARN (\s a -> s {_ctjrsTransformJobARN = a})

instance NFData CreateTransformJobResponse

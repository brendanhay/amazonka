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
-- Module      : Network.AWS.SageMaker.CreateCompilationJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a model compilation job. After the model has been compiled, Amazon SageMaker saves the resulting model artifacts to an Amazon Simple Storage Service (Amazon S3) bucket that you specify.
--
--
-- If you choose to host your model using Amazon SageMaker hosting services, you can use the resulting model artifacts as part of the model. You can also use the artifacts with AWS IoT Greengrass. In that case, deploy them as an ML resource.
--
-- In the request body, you provide the following:
--
--     * A name for the compilation job
--
--     * Information about the input model artifacts
--
--     * The output location for the compiled model and the device (target) that the model runs on
--
--     * @The Amazon Resource Name (ARN) of the IAM role that Amazon SageMaker assumes to perform the model compilation job@
--
--
--
-- You can also provide a @Tag@ to track the model compilation job's resource use and costs. The response body contains the @CompilationJobArn@ for the compiled job.
--
-- To stop a model compilation job, use 'StopCompilationJob' . To get information about a particular model compilation job, use 'DescribeCompilationJob' . To get information about multiple model compilation jobs, use 'ListCompilationJobs' .
--
module Network.AWS.SageMaker.CreateCompilationJob
    (
    -- * Creating a Request
      createCompilationJob
    , CreateCompilationJob
    -- * Request Lenses
    , ccjCompilationJobName
    , ccjRoleARN
    , ccjInputConfig
    , ccjOutputConfig
    , ccjStoppingCondition

    -- * Destructuring the Response
    , createCompilationJobResponse
    , CreateCompilationJobResponse
    -- * Response Lenses
    , ccjrsResponseStatus
    , ccjrsCompilationJobARN
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'createCompilationJob' smart constructor.
data CreateCompilationJob = CreateCompilationJob'
  { _ccjCompilationJobName :: !Text
  , _ccjRoleARN            :: !Text
  , _ccjInputConfig        :: !InputConfig
  , _ccjOutputConfig       :: !OutputConfig
  , _ccjStoppingCondition  :: !StoppingCondition
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCompilationJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccjCompilationJobName' - A name for the model compilation job. The name must be unique within the AWS Region and within your AWS account.
--
-- * 'ccjRoleARN' - The Amazon Resource Name (ARN) of an IIAMAM role that enables Amazon SageMaker to perform tasks on your behalf.  During model compilation, Amazon SageMaker needs your permission to:     * Read input data from an S3 bucket     * Write model artifacts to an S3 bucket     * Write logs to Amazon CloudWatch Logs     * Publish metrics to Amazon CloudWatch You grant permissions for all of these tasks to an IAM role. To pass this role to Amazon SageMaker, the caller of this API must have the @iam:PassRole@ permission. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles.>
--
-- * 'ccjInputConfig' - Provides information about the location of input model artifacts, the name and shape of the expected data inputs, and the framework in which the model was trained.
--
-- * 'ccjOutputConfig' - Provides information about the output location for the compiled model and the target device the model runs on.
--
-- * 'ccjStoppingCondition' - The duration allowed for model compilation.
createCompilationJob
    :: Text -- ^ 'ccjCompilationJobName'
    -> Text -- ^ 'ccjRoleARN'
    -> InputConfig -- ^ 'ccjInputConfig'
    -> OutputConfig -- ^ 'ccjOutputConfig'
    -> StoppingCondition -- ^ 'ccjStoppingCondition'
    -> CreateCompilationJob
createCompilationJob pCompilationJobName_ pRoleARN_ pInputConfig_ pOutputConfig_ pStoppingCondition_ =
  CreateCompilationJob'
    { _ccjCompilationJobName = pCompilationJobName_
    , _ccjRoleARN = pRoleARN_
    , _ccjInputConfig = pInputConfig_
    , _ccjOutputConfig = pOutputConfig_
    , _ccjStoppingCondition = pStoppingCondition_
    }


-- | A name for the model compilation job. The name must be unique within the AWS Region and within your AWS account.
ccjCompilationJobName :: Lens' CreateCompilationJob Text
ccjCompilationJobName = lens _ccjCompilationJobName (\ s a -> s{_ccjCompilationJobName = a})

-- | The Amazon Resource Name (ARN) of an IIAMAM role that enables Amazon SageMaker to perform tasks on your behalf.  During model compilation, Amazon SageMaker needs your permission to:     * Read input data from an S3 bucket     * Write model artifacts to an S3 bucket     * Write logs to Amazon CloudWatch Logs     * Publish metrics to Amazon CloudWatch You grant permissions for all of these tasks to an IAM role. To pass this role to Amazon SageMaker, the caller of this API must have the @iam:PassRole@ permission. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles.>
ccjRoleARN :: Lens' CreateCompilationJob Text
ccjRoleARN = lens _ccjRoleARN (\ s a -> s{_ccjRoleARN = a})

-- | Provides information about the location of input model artifacts, the name and shape of the expected data inputs, and the framework in which the model was trained.
ccjInputConfig :: Lens' CreateCompilationJob InputConfig
ccjInputConfig = lens _ccjInputConfig (\ s a -> s{_ccjInputConfig = a})

-- | Provides information about the output location for the compiled model and the target device the model runs on.
ccjOutputConfig :: Lens' CreateCompilationJob OutputConfig
ccjOutputConfig = lens _ccjOutputConfig (\ s a -> s{_ccjOutputConfig = a})

-- | The duration allowed for model compilation.
ccjStoppingCondition :: Lens' CreateCompilationJob StoppingCondition
ccjStoppingCondition = lens _ccjStoppingCondition (\ s a -> s{_ccjStoppingCondition = a})

instance AWSRequest CreateCompilationJob where
        type Rs CreateCompilationJob =
             CreateCompilationJobResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 CreateCompilationJobResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "CompilationJobArn"))

instance Hashable CreateCompilationJob where

instance NFData CreateCompilationJob where

instance ToHeaders CreateCompilationJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.CreateCompilationJob" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateCompilationJob where
        toJSON CreateCompilationJob'{..}
          = object
              (catMaybes
                 [Just
                    ("CompilationJobName" .= _ccjCompilationJobName),
                  Just ("RoleArn" .= _ccjRoleARN),
                  Just ("InputConfig" .= _ccjInputConfig),
                  Just ("OutputConfig" .= _ccjOutputConfig),
                  Just ("StoppingCondition" .= _ccjStoppingCondition)])

instance ToPath CreateCompilationJob where
        toPath = const "/"

instance ToQuery CreateCompilationJob where
        toQuery = const mempty

-- | /See:/ 'createCompilationJobResponse' smart constructor.
data CreateCompilationJobResponse = CreateCompilationJobResponse'
  { _ccjrsResponseStatus    :: !Int
  , _ccjrsCompilationJobARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCompilationJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccjrsResponseStatus' - -- | The response status code.
--
-- * 'ccjrsCompilationJobARN' - If the action is successful, the service sends back an HTTP 200 response. Amazon SageMaker returns the following data in JSON format:     * @CompilationJobArn@ : The Amazon Resource Name (ARN) of the compiled job.
createCompilationJobResponse
    :: Int -- ^ 'ccjrsResponseStatus'
    -> Text -- ^ 'ccjrsCompilationJobARN'
    -> CreateCompilationJobResponse
createCompilationJobResponse pResponseStatus_ pCompilationJobARN_ =
  CreateCompilationJobResponse'
    { _ccjrsResponseStatus = pResponseStatus_
    , _ccjrsCompilationJobARN = pCompilationJobARN_
    }


-- | -- | The response status code.
ccjrsResponseStatus :: Lens' CreateCompilationJobResponse Int
ccjrsResponseStatus = lens _ccjrsResponseStatus (\ s a -> s{_ccjrsResponseStatus = a})

-- | If the action is successful, the service sends back an HTTP 200 response. Amazon SageMaker returns the following data in JSON format:     * @CompilationJobArn@ : The Amazon Resource Name (ARN) of the compiled job.
ccjrsCompilationJobARN :: Lens' CreateCompilationJobResponse Text
ccjrsCompilationJobARN = lens _ccjrsCompilationJobARN (\ s a -> s{_ccjrsCompilationJobARN = a})

instance NFData CreateCompilationJobResponse where

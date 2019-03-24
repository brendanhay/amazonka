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
-- Module      : Network.AWS.SageMaker.CreateLabelingJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a job that uses workers to label the data objects in your input dataset. You can use the labeled data to train machine learning models.
--
--
-- You can select your workforce from one of three providers:
--
--     * A private workforce that you create. It can include employees, contractors, and outside experts. Use a private workforce when want the data to stay within your organization or when a specific set of skills is required.
--
--     * One or more vendors that you select from the AWS Marketplace. Vendors provide expertise in specific areas.
--
--     * The Amazon Mechanical Turk workforce. This is the largest workforce, but it should only be used for public data or data that has been stripped of any personally identifiable information.
--
--
--
-- You can also use /automated data labeling/ to reduce the number of data objects that need to be labeled by a human. Automated data labeling uses /active learning/ to determine if a data object can be labeled by machine or if it needs to be sent to a human worker. For more information, see <http://docs.aws.amazon.com/sagemaker/latest/dg/sms-automated-labeling.html Using Automated Data Labeling> .
--
-- The data objects to be labeled are contained in an Amazon S3 bucket. You create a /manifest file/ that describes the location of each object. For more information, see <http://docs.aws.amazon.com/sagemaker/latest/dg/sms-data.html Using Input and Output Data> .
--
-- The output can be used as the manifest file for another labeling job or as training data for your machine learning models.
--
module Network.AWS.SageMaker.CreateLabelingJob
    (
    -- * Creating a Request
      createLabelingJob
    , CreateLabelingJob
    -- * Request Lenses
    , cljLabelingJobAlgorithmsConfig
    , cljLabelCategoryConfigS3URI
    , cljStoppingConditions
    , cljTags
    , cljLabelingJobName
    , cljLabelAttributeName
    , cljInputConfig
    , cljOutputConfig
    , cljRoleARN
    , cljHumanTaskConfig

    -- * Destructuring the Response
    , createLabelingJobResponse
    , CreateLabelingJobResponse
    -- * Response Lenses
    , cljrsResponseStatus
    , cljrsLabelingJobARN
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'createLabelingJob' smart constructor.
data CreateLabelingJob = CreateLabelingJob'
  { _cljLabelingJobAlgorithmsConfig :: !(Maybe LabelingJobAlgorithmsConfig)
  , _cljLabelCategoryConfigS3URI    :: !(Maybe Text)
  , _cljStoppingConditions          :: !(Maybe LabelingJobStoppingConditions)
  , _cljTags                        :: !(Maybe [Tag])
  , _cljLabelingJobName             :: !Text
  , _cljLabelAttributeName          :: !Text
  , _cljInputConfig                 :: !LabelingJobInputConfig
  , _cljOutputConfig                :: !LabelingJobOutputConfig
  , _cljRoleARN                     :: !Text
  , _cljHumanTaskConfig             :: !HumanTaskConfig
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLabelingJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cljLabelingJobAlgorithmsConfig' - Configures the information required to perform automated data labeling.
--
-- * 'cljLabelCategoryConfigS3URI' - The S3 URL of the file that defines the categories used to label the data objects. The file is a JSON structure in the following format: @{@  @"document-version": "2018-11-28"@  @"labels": [@  @{@  @"label": "/label 1/ "@  @},@  @{@  @"label": "/label 2/ "@  @},@  @...@  @{@  @"label": "/label n/ "@  @}@  @]@  @}@
--
-- * 'cljStoppingConditions' - A set of conditions for stopping the labeling job. If any of the conditions are met, the job is automatically stopped. You can use these conditions to control the cost of data labeling.
--
-- * 'cljTags' - An array of key/value pairs. For more information, see <http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- * 'cljLabelingJobName' - The name of the labeling job. This name is used to identify the job in a list of labeling jobs.
--
-- * 'cljLabelAttributeName' - The attribute name to use for the label in the output manifest file. This is the key for the key/value pair formed with the label that a worker assigns to the object. The name can't end with "-metadata". If you are running a semantic segmentation labeling job, the attribute name must end with "-ref". If you are running any other kind of labeling job, the attribute name must not end with "-ref".
--
-- * 'cljInputConfig' - Input data for the labeling job, such as the Amazon S3 location of the data objects and the location of the manifest file that describes the data objects.
--
-- * 'cljOutputConfig' - The location of the output data and the AWS Key Management Service key ID for the key used to encrypt the output data, if any.
--
-- * 'cljRoleARN' - The Amazon Resource Number (ARN) that Amazon SageMaker assumes to perform tasks on your behalf during data labeling. You must grant this role the necessary permissions so that Amazon SageMaker can successfully complete data labeling.
--
-- * 'cljHumanTaskConfig' - Configures the information required for human workers to complete a labeling task.
createLabelingJob
    :: Text -- ^ 'cljLabelingJobName'
    -> Text -- ^ 'cljLabelAttributeName'
    -> LabelingJobInputConfig -- ^ 'cljInputConfig'
    -> LabelingJobOutputConfig -- ^ 'cljOutputConfig'
    -> Text -- ^ 'cljRoleARN'
    -> HumanTaskConfig -- ^ 'cljHumanTaskConfig'
    -> CreateLabelingJob
createLabelingJob pLabelingJobName_ pLabelAttributeName_ pInputConfig_ pOutputConfig_ pRoleARN_ pHumanTaskConfig_ =
  CreateLabelingJob'
    { _cljLabelingJobAlgorithmsConfig = Nothing
    , _cljLabelCategoryConfigS3URI = Nothing
    , _cljStoppingConditions = Nothing
    , _cljTags = Nothing
    , _cljLabelingJobName = pLabelingJobName_
    , _cljLabelAttributeName = pLabelAttributeName_
    , _cljInputConfig = pInputConfig_
    , _cljOutputConfig = pOutputConfig_
    , _cljRoleARN = pRoleARN_
    , _cljHumanTaskConfig = pHumanTaskConfig_
    }


-- | Configures the information required to perform automated data labeling.
cljLabelingJobAlgorithmsConfig :: Lens' CreateLabelingJob (Maybe LabelingJobAlgorithmsConfig)
cljLabelingJobAlgorithmsConfig = lens _cljLabelingJobAlgorithmsConfig (\ s a -> s{_cljLabelingJobAlgorithmsConfig = a})

-- | The S3 URL of the file that defines the categories used to label the data objects. The file is a JSON structure in the following format: @{@  @"document-version": "2018-11-28"@  @"labels": [@  @{@  @"label": "/label 1/ "@  @},@  @{@  @"label": "/label 2/ "@  @},@  @...@  @{@  @"label": "/label n/ "@  @}@  @]@  @}@
cljLabelCategoryConfigS3URI :: Lens' CreateLabelingJob (Maybe Text)
cljLabelCategoryConfigS3URI = lens _cljLabelCategoryConfigS3URI (\ s a -> s{_cljLabelCategoryConfigS3URI = a})

-- | A set of conditions for stopping the labeling job. If any of the conditions are met, the job is automatically stopped. You can use these conditions to control the cost of data labeling.
cljStoppingConditions :: Lens' CreateLabelingJob (Maybe LabelingJobStoppingConditions)
cljStoppingConditions = lens _cljStoppingConditions (\ s a -> s{_cljStoppingConditions = a})

-- | An array of key/value pairs. For more information, see <http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
cljTags :: Lens' CreateLabelingJob [Tag]
cljTags = lens _cljTags (\ s a -> s{_cljTags = a}) . _Default . _Coerce

-- | The name of the labeling job. This name is used to identify the job in a list of labeling jobs.
cljLabelingJobName :: Lens' CreateLabelingJob Text
cljLabelingJobName = lens _cljLabelingJobName (\ s a -> s{_cljLabelingJobName = a})

-- | The attribute name to use for the label in the output manifest file. This is the key for the key/value pair formed with the label that a worker assigns to the object. The name can't end with "-metadata". If you are running a semantic segmentation labeling job, the attribute name must end with "-ref". If you are running any other kind of labeling job, the attribute name must not end with "-ref".
cljLabelAttributeName :: Lens' CreateLabelingJob Text
cljLabelAttributeName = lens _cljLabelAttributeName (\ s a -> s{_cljLabelAttributeName = a})

-- | Input data for the labeling job, such as the Amazon S3 location of the data objects and the location of the manifest file that describes the data objects.
cljInputConfig :: Lens' CreateLabelingJob LabelingJobInputConfig
cljInputConfig = lens _cljInputConfig (\ s a -> s{_cljInputConfig = a})

-- | The location of the output data and the AWS Key Management Service key ID for the key used to encrypt the output data, if any.
cljOutputConfig :: Lens' CreateLabelingJob LabelingJobOutputConfig
cljOutputConfig = lens _cljOutputConfig (\ s a -> s{_cljOutputConfig = a})

-- | The Amazon Resource Number (ARN) that Amazon SageMaker assumes to perform tasks on your behalf during data labeling. You must grant this role the necessary permissions so that Amazon SageMaker can successfully complete data labeling.
cljRoleARN :: Lens' CreateLabelingJob Text
cljRoleARN = lens _cljRoleARN (\ s a -> s{_cljRoleARN = a})

-- | Configures the information required for human workers to complete a labeling task.
cljHumanTaskConfig :: Lens' CreateLabelingJob HumanTaskConfig
cljHumanTaskConfig = lens _cljHumanTaskConfig (\ s a -> s{_cljHumanTaskConfig = a})

instance AWSRequest CreateLabelingJob where
        type Rs CreateLabelingJob = CreateLabelingJobResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 CreateLabelingJobResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "LabelingJobArn"))

instance Hashable CreateLabelingJob where

instance NFData CreateLabelingJob where

instance ToHeaders CreateLabelingJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.CreateLabelingJob" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateLabelingJob where
        toJSON CreateLabelingJob'{..}
          = object
              (catMaybes
                 [("LabelingJobAlgorithmsConfig" .=) <$>
                    _cljLabelingJobAlgorithmsConfig,
                  ("LabelCategoryConfigS3Uri" .=) <$>
                    _cljLabelCategoryConfigS3URI,
                  ("StoppingConditions" .=) <$> _cljStoppingConditions,
                  ("Tags" .=) <$> _cljTags,
                  Just ("LabelingJobName" .= _cljLabelingJobName),
                  Just
                    ("LabelAttributeName" .= _cljLabelAttributeName),
                  Just ("InputConfig" .= _cljInputConfig),
                  Just ("OutputConfig" .= _cljOutputConfig),
                  Just ("RoleArn" .= _cljRoleARN),
                  Just ("HumanTaskConfig" .= _cljHumanTaskConfig)])

instance ToPath CreateLabelingJob where
        toPath = const "/"

instance ToQuery CreateLabelingJob where
        toQuery = const mempty

-- | /See:/ 'createLabelingJobResponse' smart constructor.
data CreateLabelingJobResponse = CreateLabelingJobResponse'
  { _cljrsResponseStatus :: !Int
  , _cljrsLabelingJobARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLabelingJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cljrsResponseStatus' - -- | The response status code.
--
-- * 'cljrsLabelingJobARN' - The Amazon Resource Name (ARN) of the labeling job. You use this ARN to identify the labeling job.
createLabelingJobResponse
    :: Int -- ^ 'cljrsResponseStatus'
    -> Text -- ^ 'cljrsLabelingJobARN'
    -> CreateLabelingJobResponse
createLabelingJobResponse pResponseStatus_ pLabelingJobARN_ =
  CreateLabelingJobResponse'
    { _cljrsResponseStatus = pResponseStatus_
    , _cljrsLabelingJobARN = pLabelingJobARN_
    }


-- | -- | The response status code.
cljrsResponseStatus :: Lens' CreateLabelingJobResponse Int
cljrsResponseStatus = lens _cljrsResponseStatus (\ s a -> s{_cljrsResponseStatus = a})

-- | The Amazon Resource Name (ARN) of the labeling job. You use this ARN to identify the labeling job.
cljrsLabelingJobARN :: Lens' CreateLabelingJobResponse Text
cljrsLabelingJobARN = lens _cljrsLabelingJobARN (\ s a -> s{_cljrsLabelingJobARN = a})

instance NFData CreateLabelingJobResponse where

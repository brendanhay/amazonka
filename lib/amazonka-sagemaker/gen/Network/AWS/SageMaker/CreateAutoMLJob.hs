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
-- Module      : Network.AWS.SageMaker.CreateAutoMLJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Autopilot job.
--
--
-- Find the best performing model after you run an Autopilot job by calling . Deploy that model by following the steps described in <https://docs.aws.amazon.com/sagemaker/latest/dg/ex1-deploy-model.html Step 6.1: Deploy the Model to Amazon SageMaker Hosting Services> .
--
-- For information about how to use Autopilot, see <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-automate-model-development.html Automate Model Development with Amazon SageMaker Autopilot> .
module Network.AWS.SageMaker.CreateAutoMLJob
  ( -- * Creating a Request
    createAutoMLJob,
    CreateAutoMLJob,

    -- * Request Lenses
    camljGenerateCandidateDefinitionsOnly,
    camljProblemType,
    camljAutoMLJobConfig,
    camljAutoMLJobObjective,
    camljTags,
    camljAutoMLJobName,
    camljInputDataConfig,
    camljOutputDataConfig,
    camljRoleARN,

    -- * Destructuring the Response
    createAutoMLJobResponse,
    CreateAutoMLJobResponse,

    -- * Response Lenses
    camljrsResponseStatus,
    camljrsAutoMLJobARN,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'createAutoMLJob' smart constructor.
data CreateAutoMLJob = CreateAutoMLJob'
  { _camljGenerateCandidateDefinitionsOnly ::
      !(Maybe Bool),
    _camljProblemType :: !(Maybe ProblemType),
    _camljAutoMLJobConfig :: !(Maybe AutoMLJobConfig),
    _camljAutoMLJobObjective :: !(Maybe AutoMLJobObjective),
    _camljTags :: !(Maybe [Tag]),
    _camljAutoMLJobName :: !Text,
    _camljInputDataConfig :: !(List1 AutoMLChannel),
    _camljOutputDataConfig :: !AutoMLOutputDataConfig,
    _camljRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateAutoMLJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'camljGenerateCandidateDefinitionsOnly' - Generates possible candidates without training a model. A candidate is a combination of data preprocessors, algorithms, and algorithm parameter settings.
--
-- * 'camljProblemType' - Defines the kind of preprocessing and algorithms intended for the candidates. Options include: BinaryClassification, MulticlassClassification, and Regression.
--
-- * 'camljAutoMLJobConfig' - Contains CompletionCriteria and SecurityConfig.
--
-- * 'camljAutoMLJobObjective' - Defines the objective of a an AutoML job. You provide a 'AutoMLJobObjective$MetricName' and Autopilot infers whether to minimize or maximize it. If a metric is not specified, the most commonly used ObjectiveMetric for problem type is automaically selected.
--
-- * 'camljTags' - Each tag consists of a key and an optional value. Tag keys must be unique per resource.
--
-- * 'camljAutoMLJobName' - Identifies an Autopilot job. Must be unique to your account and is case-insensitive.
--
-- * 'camljInputDataConfig' - Similar to InputDataConfig supported by Tuning. Format(s) supported: CSV. Minimum of 500 rows.
--
-- * 'camljOutputDataConfig' - Similar to OutputDataConfig supported by Tuning. Format(s) supported: CSV.
--
-- * 'camljRoleARN' - The ARN of the role that is used to access the data.
createAutoMLJob ::
  -- | 'camljAutoMLJobName'
  Text ->
  -- | 'camljInputDataConfig'
  NonEmpty AutoMLChannel ->
  -- | 'camljOutputDataConfig'
  AutoMLOutputDataConfig ->
  -- | 'camljRoleARN'
  Text ->
  CreateAutoMLJob
createAutoMLJob
  pAutoMLJobName_
  pInputDataConfig_
  pOutputDataConfig_
  pRoleARN_ =
    CreateAutoMLJob'
      { _camljGenerateCandidateDefinitionsOnly =
          Nothing,
        _camljProblemType = Nothing,
        _camljAutoMLJobConfig = Nothing,
        _camljAutoMLJobObjective = Nothing,
        _camljTags = Nothing,
        _camljAutoMLJobName = pAutoMLJobName_,
        _camljInputDataConfig = _List1 # pInputDataConfig_,
        _camljOutputDataConfig = pOutputDataConfig_,
        _camljRoleARN = pRoleARN_
      }

-- | Generates possible candidates without training a model. A candidate is a combination of data preprocessors, algorithms, and algorithm parameter settings.
camljGenerateCandidateDefinitionsOnly :: Lens' CreateAutoMLJob (Maybe Bool)
camljGenerateCandidateDefinitionsOnly = lens _camljGenerateCandidateDefinitionsOnly (\s a -> s {_camljGenerateCandidateDefinitionsOnly = a})

-- | Defines the kind of preprocessing and algorithms intended for the candidates. Options include: BinaryClassification, MulticlassClassification, and Regression.
camljProblemType :: Lens' CreateAutoMLJob (Maybe ProblemType)
camljProblemType = lens _camljProblemType (\s a -> s {_camljProblemType = a})

-- | Contains CompletionCriteria and SecurityConfig.
camljAutoMLJobConfig :: Lens' CreateAutoMLJob (Maybe AutoMLJobConfig)
camljAutoMLJobConfig = lens _camljAutoMLJobConfig (\s a -> s {_camljAutoMLJobConfig = a})

-- | Defines the objective of a an AutoML job. You provide a 'AutoMLJobObjective$MetricName' and Autopilot infers whether to minimize or maximize it. If a metric is not specified, the most commonly used ObjectiveMetric for problem type is automaically selected.
camljAutoMLJobObjective :: Lens' CreateAutoMLJob (Maybe AutoMLJobObjective)
camljAutoMLJobObjective = lens _camljAutoMLJobObjective (\s a -> s {_camljAutoMLJobObjective = a})

-- | Each tag consists of a key and an optional value. Tag keys must be unique per resource.
camljTags :: Lens' CreateAutoMLJob [Tag]
camljTags = lens _camljTags (\s a -> s {_camljTags = a}) . _Default . _Coerce

-- | Identifies an Autopilot job. Must be unique to your account and is case-insensitive.
camljAutoMLJobName :: Lens' CreateAutoMLJob Text
camljAutoMLJobName = lens _camljAutoMLJobName (\s a -> s {_camljAutoMLJobName = a})

-- | Similar to InputDataConfig supported by Tuning. Format(s) supported: CSV. Minimum of 500 rows.
camljInputDataConfig :: Lens' CreateAutoMLJob (NonEmpty AutoMLChannel)
camljInputDataConfig = lens _camljInputDataConfig (\s a -> s {_camljInputDataConfig = a}) . _List1

-- | Similar to OutputDataConfig supported by Tuning. Format(s) supported: CSV.
camljOutputDataConfig :: Lens' CreateAutoMLJob AutoMLOutputDataConfig
camljOutputDataConfig = lens _camljOutputDataConfig (\s a -> s {_camljOutputDataConfig = a})

-- | The ARN of the role that is used to access the data.
camljRoleARN :: Lens' CreateAutoMLJob Text
camljRoleARN = lens _camljRoleARN (\s a -> s {_camljRoleARN = a})

instance AWSRequest CreateAutoMLJob where
  type Rs CreateAutoMLJob = CreateAutoMLJobResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          CreateAutoMLJobResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "AutoMLJobArn")
      )

instance Hashable CreateAutoMLJob

instance NFData CreateAutoMLJob

instance ToHeaders CreateAutoMLJob where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.CreateAutoMLJob" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateAutoMLJob where
  toJSON CreateAutoMLJob' {..} =
    object
      ( catMaybes
          [ ("GenerateCandidateDefinitionsOnly" .=)
              <$> _camljGenerateCandidateDefinitionsOnly,
            ("ProblemType" .=) <$> _camljProblemType,
            ("AutoMLJobConfig" .=) <$> _camljAutoMLJobConfig,
            ("AutoMLJobObjective" .=) <$> _camljAutoMLJobObjective,
            ("Tags" .=) <$> _camljTags,
            Just ("AutoMLJobName" .= _camljAutoMLJobName),
            Just ("InputDataConfig" .= _camljInputDataConfig),
            Just ("OutputDataConfig" .= _camljOutputDataConfig),
            Just ("RoleArn" .= _camljRoleARN)
          ]
      )

instance ToPath CreateAutoMLJob where
  toPath = const "/"

instance ToQuery CreateAutoMLJob where
  toQuery = const mempty

-- | /See:/ 'createAutoMLJobResponse' smart constructor.
data CreateAutoMLJobResponse = CreateAutoMLJobResponse'
  { _camljrsResponseStatus ::
      !Int,
    _camljrsAutoMLJobARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateAutoMLJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'camljrsResponseStatus' - -- | The response status code.
--
-- * 'camljrsAutoMLJobARN' - When a job is created, it is assigned a unique ARN.
createAutoMLJobResponse ::
  -- | 'camljrsResponseStatus'
  Int ->
  -- | 'camljrsAutoMLJobARN'
  Text ->
  CreateAutoMLJobResponse
createAutoMLJobResponse pResponseStatus_ pAutoMLJobARN_ =
  CreateAutoMLJobResponse'
    { _camljrsResponseStatus =
        pResponseStatus_,
      _camljrsAutoMLJobARN = pAutoMLJobARN_
    }

-- | -- | The response status code.
camljrsResponseStatus :: Lens' CreateAutoMLJobResponse Int
camljrsResponseStatus = lens _camljrsResponseStatus (\s a -> s {_camljrsResponseStatus = a})

-- | When a job is created, it is assigned a unique ARN.
camljrsAutoMLJobARN :: Lens' CreateAutoMLJobResponse Text
camljrsAutoMLJobARN = lens _camljrsAutoMLJobARN (\s a -> s {_camljrsAutoMLJobARN = a})

instance NFData CreateAutoMLJobResponse

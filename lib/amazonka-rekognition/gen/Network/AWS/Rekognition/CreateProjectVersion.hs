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
-- Module      : Network.AWS.Rekognition.CreateProjectVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version of a model and begins training. Models are managed as part of an Amazon Rekognition Custom Labels project. You can specify one training dataset and one testing dataset. The response from @CreateProjectVersion@ is an Amazon Resource Name (ARN) for the version of the model.
--
--
-- Training takes a while to complete. You can get the current status by calling 'DescribeProjectVersions' .
--
-- Once training has successfully completed, call 'DescribeProjectVersions' to get the training results and evaluate the model.
--
-- After evaluating the model, you start the model by calling 'StartProjectVersion' .
--
-- This operation requires permissions to perform the @rekognition:CreateProjectVersion@ action.
module Network.AWS.Rekognition.CreateProjectVersion
  ( -- * Creating a Request
    createProjectVersion,
    CreateProjectVersion,

    -- * Request Lenses
    cpvProjectARN,
    cpvVersionName,
    cpvOutputConfig,
    cpvTrainingData,
    cpvTestingData,

    -- * Destructuring the Response
    createProjectVersionResponse,
    CreateProjectVersionResponse,

    -- * Response Lenses
    cpvrsProjectVersionARN,
    cpvrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createProjectVersion' smart constructor.
data CreateProjectVersion = CreateProjectVersion'
  { _cpvProjectARN ::
      !Text,
    _cpvVersionName :: !Text,
    _cpvOutputConfig :: !OutputConfig,
    _cpvTrainingData :: !TrainingData,
    _cpvTestingData :: !TestingData
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateProjectVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpvProjectARN' - The ARN of the Amazon Rekognition Custom Labels project that manages the model that you want to train.
--
-- * 'cpvVersionName' - A name for the version of the model. This value must be unique.
--
-- * 'cpvOutputConfig' - The Amazon S3 location to store the results of training.
--
-- * 'cpvTrainingData' - The dataset to use for training.
--
-- * 'cpvTestingData' - The dataset to use for testing.
createProjectVersion ::
  -- | 'cpvProjectARN'
  Text ->
  -- | 'cpvVersionName'
  Text ->
  -- | 'cpvOutputConfig'
  OutputConfig ->
  -- | 'cpvTrainingData'
  TrainingData ->
  -- | 'cpvTestingData'
  TestingData ->
  CreateProjectVersion
createProjectVersion
  pProjectARN_
  pVersionName_
  pOutputConfig_
  pTrainingData_
  pTestingData_ =
    CreateProjectVersion'
      { _cpvProjectARN = pProjectARN_,
        _cpvVersionName = pVersionName_,
        _cpvOutputConfig = pOutputConfig_,
        _cpvTrainingData = pTrainingData_,
        _cpvTestingData = pTestingData_
      }

-- | The ARN of the Amazon Rekognition Custom Labels project that manages the model that you want to train.
cpvProjectARN :: Lens' CreateProjectVersion Text
cpvProjectARN = lens _cpvProjectARN (\s a -> s {_cpvProjectARN = a})

-- | A name for the version of the model. This value must be unique.
cpvVersionName :: Lens' CreateProjectVersion Text
cpvVersionName = lens _cpvVersionName (\s a -> s {_cpvVersionName = a})

-- | The Amazon S3 location to store the results of training.
cpvOutputConfig :: Lens' CreateProjectVersion OutputConfig
cpvOutputConfig = lens _cpvOutputConfig (\s a -> s {_cpvOutputConfig = a})

-- | The dataset to use for training.
cpvTrainingData :: Lens' CreateProjectVersion TrainingData
cpvTrainingData = lens _cpvTrainingData (\s a -> s {_cpvTrainingData = a})

-- | The dataset to use for testing.
cpvTestingData :: Lens' CreateProjectVersion TestingData
cpvTestingData = lens _cpvTestingData (\s a -> s {_cpvTestingData = a})

instance AWSRequest CreateProjectVersion where
  type Rs CreateProjectVersion = CreateProjectVersionResponse
  request = postJSON rekognition
  response =
    receiveJSON
      ( \s h x ->
          CreateProjectVersionResponse'
            <$> (x .?> "ProjectVersionArn") <*> (pure (fromEnum s))
      )

instance Hashable CreateProjectVersion

instance NFData CreateProjectVersion

instance ToHeaders CreateProjectVersion where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("RekognitionService.CreateProjectVersion" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateProjectVersion where
  toJSON CreateProjectVersion' {..} =
    object
      ( catMaybes
          [ Just ("ProjectArn" .= _cpvProjectARN),
            Just ("VersionName" .= _cpvVersionName),
            Just ("OutputConfig" .= _cpvOutputConfig),
            Just ("TrainingData" .= _cpvTrainingData),
            Just ("TestingData" .= _cpvTestingData)
          ]
      )

instance ToPath CreateProjectVersion where
  toPath = const "/"

instance ToQuery CreateProjectVersion where
  toQuery = const mempty

-- | /See:/ 'createProjectVersionResponse' smart constructor.
data CreateProjectVersionResponse = CreateProjectVersionResponse'
  { _cpvrsProjectVersionARN ::
      !(Maybe Text),
    _cpvrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateProjectVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpvrsProjectVersionARN' - The ARN of the model version that was created. Use @DescribeProjectVersion@ to get the current status of the training operation.
--
-- * 'cpvrsResponseStatus' - -- | The response status code.
createProjectVersionResponse ::
  -- | 'cpvrsResponseStatus'
  Int ->
  CreateProjectVersionResponse
createProjectVersionResponse pResponseStatus_ =
  CreateProjectVersionResponse'
    { _cpvrsProjectVersionARN = Nothing,
      _cpvrsResponseStatus = pResponseStatus_
    }

-- | The ARN of the model version that was created. Use @DescribeProjectVersion@ to get the current status of the training operation.
cpvrsProjectVersionARN :: Lens' CreateProjectVersionResponse (Maybe Text)
cpvrsProjectVersionARN = lens _cpvrsProjectVersionARN (\s a -> s {_cpvrsProjectVersionARN = a})

-- | -- | The response status code.
cpvrsResponseStatus :: Lens' CreateProjectVersionResponse Int
cpvrsResponseStatus = lens _cpvrsResponseStatus (\s a -> s {_cpvrsResponseStatus = a})

instance NFData CreateProjectVersionResponse

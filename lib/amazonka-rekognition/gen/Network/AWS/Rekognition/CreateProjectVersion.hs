{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
-- Training takes a while to complete. You can get the current status by calling 'DescribeProjectVersions' .
-- Once training has successfully completed, call 'DescribeProjectVersions' to get the training results and evaluate the model.
-- After evaluating the model, you start the model by calling 'StartProjectVersion' .
-- This operation requires permissions to perform the @rekognition:CreateProjectVersion@ action.
module Network.AWS.Rekognition.CreateProjectVersion
  ( -- * Creating a request
    CreateProjectVersion (..),
    mkCreateProjectVersion,

    -- ** Request lenses
    cpvVersionName,
    cpvTestingData,
    cpvOutputConfig,
    cpvTrainingData,
    cpvProjectARN,

    -- * Destructuring the response
    CreateProjectVersionResponse (..),
    mkCreateProjectVersionResponse,

    -- ** Response lenses
    cpvrsProjectVersionARN,
    cpvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateProjectVersion' smart constructor.
data CreateProjectVersion = CreateProjectVersion'
  { -- | A name for the version of the model. This value must be unique.
    versionName :: Lude.Text,
    -- | The dataset to use for testing.
    testingData :: TestingData,
    -- | The Amazon S3 location to store the results of training.
    outputConfig :: OutputConfig,
    -- | The dataset to use for training.
    trainingData :: TrainingData,
    -- | The ARN of the Amazon Rekognition Custom Labels project that manages the model that you want to train.
    projectARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateProjectVersion' with the minimum fields required to make a request.
--
-- * 'versionName' - A name for the version of the model. This value must be unique.
-- * 'testingData' - The dataset to use for testing.
-- * 'outputConfig' - The Amazon S3 location to store the results of training.
-- * 'trainingData' - The dataset to use for training.
-- * 'projectARN' - The ARN of the Amazon Rekognition Custom Labels project that manages the model that you want to train.
mkCreateProjectVersion ::
  -- | 'versionName'
  Lude.Text ->
  -- | 'testingData'
  TestingData ->
  -- | 'outputConfig'
  OutputConfig ->
  -- | 'trainingData'
  TrainingData ->
  -- | 'projectARN'
  Lude.Text ->
  CreateProjectVersion
mkCreateProjectVersion
  pVersionName_
  pTestingData_
  pOutputConfig_
  pTrainingData_
  pProjectARN_ =
    CreateProjectVersion'
      { versionName = pVersionName_,
        testingData = pTestingData_,
        outputConfig = pOutputConfig_,
        trainingData = pTrainingData_,
        projectARN = pProjectARN_
      }

-- | A name for the version of the model. This value must be unique.
--
-- /Note:/ Consider using 'versionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvVersionName :: Lens.Lens' CreateProjectVersion Lude.Text
cpvVersionName = Lens.lens (versionName :: CreateProjectVersion -> Lude.Text) (\s a -> s {versionName = a} :: CreateProjectVersion)
{-# DEPRECATED cpvVersionName "Use generic-lens or generic-optics with 'versionName' instead." #-}

-- | The dataset to use for testing.
--
-- /Note:/ Consider using 'testingData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvTestingData :: Lens.Lens' CreateProjectVersion TestingData
cpvTestingData = Lens.lens (testingData :: CreateProjectVersion -> TestingData) (\s a -> s {testingData = a} :: CreateProjectVersion)
{-# DEPRECATED cpvTestingData "Use generic-lens or generic-optics with 'testingData' instead." #-}

-- | The Amazon S3 location to store the results of training.
--
-- /Note:/ Consider using 'outputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvOutputConfig :: Lens.Lens' CreateProjectVersion OutputConfig
cpvOutputConfig = Lens.lens (outputConfig :: CreateProjectVersion -> OutputConfig) (\s a -> s {outputConfig = a} :: CreateProjectVersion)
{-# DEPRECATED cpvOutputConfig "Use generic-lens or generic-optics with 'outputConfig' instead." #-}

-- | The dataset to use for training.
--
-- /Note:/ Consider using 'trainingData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvTrainingData :: Lens.Lens' CreateProjectVersion TrainingData
cpvTrainingData = Lens.lens (trainingData :: CreateProjectVersion -> TrainingData) (\s a -> s {trainingData = a} :: CreateProjectVersion)
{-# DEPRECATED cpvTrainingData "Use generic-lens or generic-optics with 'trainingData' instead." #-}

-- | The ARN of the Amazon Rekognition Custom Labels project that manages the model that you want to train.
--
-- /Note:/ Consider using 'projectARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvProjectARN :: Lens.Lens' CreateProjectVersion Lude.Text
cpvProjectARN = Lens.lens (projectARN :: CreateProjectVersion -> Lude.Text) (\s a -> s {projectARN = a} :: CreateProjectVersion)
{-# DEPRECATED cpvProjectARN "Use generic-lens or generic-optics with 'projectARN' instead." #-}

instance Lude.AWSRequest CreateProjectVersion where
  type Rs CreateProjectVersion = CreateProjectVersionResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateProjectVersionResponse'
            Lude.<$> (x Lude..?> "ProjectVersionArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateProjectVersion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.CreateProjectVersion" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateProjectVersion where
  toJSON CreateProjectVersion' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("VersionName" Lude..= versionName),
            Lude.Just ("TestingData" Lude..= testingData),
            Lude.Just ("OutputConfig" Lude..= outputConfig),
            Lude.Just ("TrainingData" Lude..= trainingData),
            Lude.Just ("ProjectArn" Lude..= projectARN)
          ]
      )

instance Lude.ToPath CreateProjectVersion where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateProjectVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateProjectVersionResponse' smart constructor.
data CreateProjectVersionResponse = CreateProjectVersionResponse'
  { -- | The ARN of the model version that was created. Use @DescribeProjectVersion@ to get the current status of the training operation.
    projectVersionARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateProjectVersionResponse' with the minimum fields required to make a request.
--
-- * 'projectVersionARN' - The ARN of the model version that was created. Use @DescribeProjectVersion@ to get the current status of the training operation.
-- * 'responseStatus' - The response status code.
mkCreateProjectVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateProjectVersionResponse
mkCreateProjectVersionResponse pResponseStatus_ =
  CreateProjectVersionResponse'
    { projectVersionARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the model version that was created. Use @DescribeProjectVersion@ to get the current status of the training operation.
--
-- /Note:/ Consider using 'projectVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvrsProjectVersionARN :: Lens.Lens' CreateProjectVersionResponse (Lude.Maybe Lude.Text)
cpvrsProjectVersionARN = Lens.lens (projectVersionARN :: CreateProjectVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {projectVersionARN = a} :: CreateProjectVersionResponse)
{-# DEPRECATED cpvrsProjectVersionARN "Use generic-lens or generic-optics with 'projectVersionARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvrsResponseStatus :: Lens.Lens' CreateProjectVersionResponse Lude.Int
cpvrsResponseStatus = Lens.lens (responseStatus :: CreateProjectVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateProjectVersionResponse)
{-# DEPRECATED cpvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

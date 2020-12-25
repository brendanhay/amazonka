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
    cpvProjectArn,
    cpvVersionName,
    cpvOutputConfig,
    cpvTrainingData,
    cpvTestingData,

    -- * Destructuring the response
    CreateProjectVersionResponse (..),
    mkCreateProjectVersionResponse,

    -- ** Response lenses
    cpvrrsProjectVersionArn,
    cpvrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateProjectVersion' smart constructor.
data CreateProjectVersion = CreateProjectVersion'
  { -- | The ARN of the Amazon Rekognition Custom Labels project that manages the model that you want to train.
    projectArn :: Types.ProjectArn,
    -- | A name for the version of the model. This value must be unique.
    versionName :: Types.VersionName,
    -- | The Amazon S3 location to store the results of training.
    outputConfig :: Types.OutputConfig,
    -- | The dataset to use for training.
    trainingData :: Types.TrainingData,
    -- | The dataset to use for testing.
    testingData :: Types.TestingData
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateProjectVersion' value with any optional fields omitted.
mkCreateProjectVersion ::
  -- | 'projectArn'
  Types.ProjectArn ->
  -- | 'versionName'
  Types.VersionName ->
  -- | 'outputConfig'
  Types.OutputConfig ->
  -- | 'trainingData'
  Types.TrainingData ->
  -- | 'testingData'
  Types.TestingData ->
  CreateProjectVersion
mkCreateProjectVersion
  projectArn
  versionName
  outputConfig
  trainingData
  testingData =
    CreateProjectVersion'
      { projectArn,
        versionName,
        outputConfig,
        trainingData,
        testingData
      }

-- | The ARN of the Amazon Rekognition Custom Labels project that manages the model that you want to train.
--
-- /Note:/ Consider using 'projectArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvProjectArn :: Lens.Lens' CreateProjectVersion Types.ProjectArn
cpvProjectArn = Lens.field @"projectArn"
{-# DEPRECATED cpvProjectArn "Use generic-lens or generic-optics with 'projectArn' instead." #-}

-- | A name for the version of the model. This value must be unique.
--
-- /Note:/ Consider using 'versionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvVersionName :: Lens.Lens' CreateProjectVersion Types.VersionName
cpvVersionName = Lens.field @"versionName"
{-# DEPRECATED cpvVersionName "Use generic-lens or generic-optics with 'versionName' instead." #-}

-- | The Amazon S3 location to store the results of training.
--
-- /Note:/ Consider using 'outputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvOutputConfig :: Lens.Lens' CreateProjectVersion Types.OutputConfig
cpvOutputConfig = Lens.field @"outputConfig"
{-# DEPRECATED cpvOutputConfig "Use generic-lens or generic-optics with 'outputConfig' instead." #-}

-- | The dataset to use for training.
--
-- /Note:/ Consider using 'trainingData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvTrainingData :: Lens.Lens' CreateProjectVersion Types.TrainingData
cpvTrainingData = Lens.field @"trainingData"
{-# DEPRECATED cpvTrainingData "Use generic-lens or generic-optics with 'trainingData' instead." #-}

-- | The dataset to use for testing.
--
-- /Note:/ Consider using 'testingData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvTestingData :: Lens.Lens' CreateProjectVersion Types.TestingData
cpvTestingData = Lens.field @"testingData"
{-# DEPRECATED cpvTestingData "Use generic-lens or generic-optics with 'testingData' instead." #-}

instance Core.FromJSON CreateProjectVersion where
  toJSON CreateProjectVersion {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ProjectArn" Core..= projectArn),
            Core.Just ("VersionName" Core..= versionName),
            Core.Just ("OutputConfig" Core..= outputConfig),
            Core.Just ("TrainingData" Core..= trainingData),
            Core.Just ("TestingData" Core..= testingData)
          ]
      )

instance Core.AWSRequest CreateProjectVersion where
  type Rs CreateProjectVersion = CreateProjectVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "RekognitionService.CreateProjectVersion")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProjectVersionResponse'
            Core.<$> (x Core..:? "ProjectVersionArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateProjectVersionResponse' smart constructor.
data CreateProjectVersionResponse = CreateProjectVersionResponse'
  { -- | The ARN of the model version that was created. Use @DescribeProjectVersion@ to get the current status of the training operation.
    projectVersionArn :: Core.Maybe Types.ProjectVersionArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateProjectVersionResponse' value with any optional fields omitted.
mkCreateProjectVersionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateProjectVersionResponse
mkCreateProjectVersionResponse responseStatus =
  CreateProjectVersionResponse'
    { projectVersionArn = Core.Nothing,
      responseStatus
    }

-- | The ARN of the model version that was created. Use @DescribeProjectVersion@ to get the current status of the training operation.
--
-- /Note:/ Consider using 'projectVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvrrsProjectVersionArn :: Lens.Lens' CreateProjectVersionResponse (Core.Maybe Types.ProjectVersionArn)
cpvrrsProjectVersionArn = Lens.field @"projectVersionArn"
{-# DEPRECATED cpvrrsProjectVersionArn "Use generic-lens or generic-optics with 'projectVersionArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvrrsResponseStatus :: Lens.Lens' CreateProjectVersionResponse Core.Int
cpvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cpvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

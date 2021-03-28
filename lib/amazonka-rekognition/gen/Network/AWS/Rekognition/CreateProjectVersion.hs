{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateProjectVersion (..)
    , mkCreateProjectVersion
    -- ** Request lenses
    , cpvProjectArn
    , cpvVersionName
    , cpvOutputConfig
    , cpvTrainingData
    , cpvTestingData

    -- * Destructuring the response
    , CreateProjectVersionResponse (..)
    , mkCreateProjectVersionResponse
    -- ** Response lenses
    , cpvrrsProjectVersionArn
    , cpvrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateProjectVersion' smart constructor.
data CreateProjectVersion = CreateProjectVersion'
  { projectArn :: Types.ProjectArn
    -- ^ The ARN of the Amazon Rekognition Custom Labels project that manages the model that you want to train.
  , versionName :: Types.VersionName
    -- ^ A name for the version of the model. This value must be unique.
  , outputConfig :: Types.OutputConfig
    -- ^ The Amazon S3 location to store the results of training.
  , trainingData :: Types.TrainingData
    -- ^ The dataset to use for training. 
  , testingData :: Types.TestingData
    -- ^ The dataset to use for testing.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateProjectVersion' value with any optional fields omitted.
mkCreateProjectVersion
    :: Types.ProjectArn -- ^ 'projectArn'
    -> Types.VersionName -- ^ 'versionName'
    -> Types.OutputConfig -- ^ 'outputConfig'
    -> Types.TrainingData -- ^ 'trainingData'
    -> Types.TestingData -- ^ 'testingData'
    -> CreateProjectVersion
mkCreateProjectVersion projectArn versionName outputConfig
  trainingData testingData
  = CreateProjectVersion'{projectArn, versionName, outputConfig,
                          trainingData, testingData}

-- | The ARN of the Amazon Rekognition Custom Labels project that manages the model that you want to train.
--
-- /Note:/ Consider using 'projectArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvProjectArn :: Lens.Lens' CreateProjectVersion Types.ProjectArn
cpvProjectArn = Lens.field @"projectArn"
{-# INLINEABLE cpvProjectArn #-}
{-# DEPRECATED projectArn "Use generic-lens or generic-optics with 'projectArn' instead"  #-}

-- | A name for the version of the model. This value must be unique.
--
-- /Note:/ Consider using 'versionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvVersionName :: Lens.Lens' CreateProjectVersion Types.VersionName
cpvVersionName = Lens.field @"versionName"
{-# INLINEABLE cpvVersionName #-}
{-# DEPRECATED versionName "Use generic-lens or generic-optics with 'versionName' instead"  #-}

-- | The Amazon S3 location to store the results of training.
--
-- /Note:/ Consider using 'outputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvOutputConfig :: Lens.Lens' CreateProjectVersion Types.OutputConfig
cpvOutputConfig = Lens.field @"outputConfig"
{-# INLINEABLE cpvOutputConfig #-}
{-# DEPRECATED outputConfig "Use generic-lens or generic-optics with 'outputConfig' instead"  #-}

-- | The dataset to use for training. 
--
-- /Note:/ Consider using 'trainingData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvTrainingData :: Lens.Lens' CreateProjectVersion Types.TrainingData
cpvTrainingData = Lens.field @"trainingData"
{-# INLINEABLE cpvTrainingData #-}
{-# DEPRECATED trainingData "Use generic-lens or generic-optics with 'trainingData' instead"  #-}

-- | The dataset to use for testing.
--
-- /Note:/ Consider using 'testingData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvTestingData :: Lens.Lens' CreateProjectVersion Types.TestingData
cpvTestingData = Lens.field @"testingData"
{-# INLINEABLE cpvTestingData #-}
{-# DEPRECATED testingData "Use generic-lens or generic-optics with 'testingData' instead"  #-}

instance Core.ToQuery CreateProjectVersion where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateProjectVersion where
        toHeaders CreateProjectVersion{..}
          = Core.pure
              ("X-Amz-Target", "RekognitionService.CreateProjectVersion")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateProjectVersion where
        toJSON CreateProjectVersion{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ProjectArn" Core..= projectArn),
                  Core.Just ("VersionName" Core..= versionName),
                  Core.Just ("OutputConfig" Core..= outputConfig),
                  Core.Just ("TrainingData" Core..= trainingData),
                  Core.Just ("TestingData" Core..= testingData)])

instance Core.AWSRequest CreateProjectVersion where
        type Rs CreateProjectVersion = CreateProjectVersionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateProjectVersionResponse' Core.<$>
                   (x Core..:? "ProjectVersionArn") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateProjectVersionResponse' smart constructor.
data CreateProjectVersionResponse = CreateProjectVersionResponse'
  { projectVersionArn :: Core.Maybe Types.ProjectVersionArn
    -- ^ The ARN of the model version that was created. Use @DescribeProjectVersion@ to get the current status of the training operation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateProjectVersionResponse' value with any optional fields omitted.
mkCreateProjectVersionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateProjectVersionResponse
mkCreateProjectVersionResponse responseStatus
  = CreateProjectVersionResponse'{projectVersionArn = Core.Nothing,
                                  responseStatus}

-- | The ARN of the model version that was created. Use @DescribeProjectVersion@ to get the current status of the training operation.
--
-- /Note:/ Consider using 'projectVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvrrsProjectVersionArn :: Lens.Lens' CreateProjectVersionResponse (Core.Maybe Types.ProjectVersionArn)
cpvrrsProjectVersionArn = Lens.field @"projectVersionArn"
{-# INLINEABLE cpvrrsProjectVersionArn #-}
{-# DEPRECATED projectVersionArn "Use generic-lens or generic-optics with 'projectVersionArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvrrsResponseStatus :: Lens.Lens' CreateProjectVersionResponse Core.Int
cpvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cpvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.CreateLanguageModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new custom language model. Use Amazon S3 prefixes to provide the location of your input files. The time it takes to create your model depends on the size of your training data.
module Network.AWS.Transcribe.CreateLanguageModel
    (
    -- * Creating a request
      CreateLanguageModel (..)
    , mkCreateLanguageModel
    -- ** Request lenses
    , clmLanguageCode
    , clmBaseModelName
    , clmModelName
    , clmInputDataConfig

    -- * Destructuring the response
    , CreateLanguageModelResponse (..)
    , mkCreateLanguageModelResponse
    -- ** Response lenses
    , clmrrsBaseModelName
    , clmrrsInputDataConfig
    , clmrrsLanguageCode
    , clmrrsModelName
    , clmrrsModelStatus
    , clmrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Transcribe.Types as Types

-- | /See:/ 'mkCreateLanguageModel' smart constructor.
data CreateLanguageModel = CreateLanguageModel'
  { languageCode :: Types.CLMLanguageCode
    -- ^ The language of the input text you're using to train your custom language model.
  , baseModelName :: Types.BaseModelName
    -- ^ The Amazon Transcribe standard language model, or base model used to create your custom language model.
--
-- If you want to use your custom language model to transcribe audio with a sample rate of 16 kHz or greater, choose @Wideband@ .
-- If you want to use your custom language model to transcribe audio with a sample rate that is less than 16 kHz, choose @Narrowband@ .
  , modelName :: Types.ModelName
    -- ^ The name you choose for your custom language model when you create it.
  , inputDataConfig :: Types.InputDataConfig
    -- ^ Contains the data access role and the Amazon S3 prefixes to read the required input files to create a custom language model.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLanguageModel' value with any optional fields omitted.
mkCreateLanguageModel
    :: Types.CLMLanguageCode -- ^ 'languageCode'
    -> Types.BaseModelName -- ^ 'baseModelName'
    -> Types.ModelName -- ^ 'modelName'
    -> Types.InputDataConfig -- ^ 'inputDataConfig'
    -> CreateLanguageModel
mkCreateLanguageModel languageCode baseModelName modelName
  inputDataConfig
  = CreateLanguageModel'{languageCode, baseModelName, modelName,
                         inputDataConfig}

-- | The language of the input text you're using to train your custom language model.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clmLanguageCode :: Lens.Lens' CreateLanguageModel Types.CLMLanguageCode
clmLanguageCode = Lens.field @"languageCode"
{-# INLINEABLE clmLanguageCode #-}
{-# DEPRECATED languageCode "Use generic-lens or generic-optics with 'languageCode' instead"  #-}

-- | The Amazon Transcribe standard language model, or base model used to create your custom language model.
--
-- If you want to use your custom language model to transcribe audio with a sample rate of 16 kHz or greater, choose @Wideband@ .
-- If you want to use your custom language model to transcribe audio with a sample rate that is less than 16 kHz, choose @Narrowband@ .
--
-- /Note:/ Consider using 'baseModelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clmBaseModelName :: Lens.Lens' CreateLanguageModel Types.BaseModelName
clmBaseModelName = Lens.field @"baseModelName"
{-# INLINEABLE clmBaseModelName #-}
{-# DEPRECATED baseModelName "Use generic-lens or generic-optics with 'baseModelName' instead"  #-}

-- | The name you choose for your custom language model when you create it.
--
-- /Note:/ Consider using 'modelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clmModelName :: Lens.Lens' CreateLanguageModel Types.ModelName
clmModelName = Lens.field @"modelName"
{-# INLINEABLE clmModelName #-}
{-# DEPRECATED modelName "Use generic-lens or generic-optics with 'modelName' instead"  #-}

-- | Contains the data access role and the Amazon S3 prefixes to read the required input files to create a custom language model.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clmInputDataConfig :: Lens.Lens' CreateLanguageModel Types.InputDataConfig
clmInputDataConfig = Lens.field @"inputDataConfig"
{-# INLINEABLE clmInputDataConfig #-}
{-# DEPRECATED inputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead"  #-}

instance Core.ToQuery CreateLanguageModel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateLanguageModel where
        toHeaders CreateLanguageModel{..}
          = Core.pure ("X-Amz-Target", "Transcribe.CreateLanguageModel")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateLanguageModel where
        toJSON CreateLanguageModel{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("LanguageCode" Core..= languageCode),
                  Core.Just ("BaseModelName" Core..= baseModelName),
                  Core.Just ("ModelName" Core..= modelName),
                  Core.Just ("InputDataConfig" Core..= inputDataConfig)])

instance Core.AWSRequest CreateLanguageModel where
        type Rs CreateLanguageModel = CreateLanguageModelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateLanguageModelResponse' Core.<$>
                   (x Core..:? "BaseModelName") Core.<*> x Core..:? "InputDataConfig"
                     Core.<*> x Core..:? "LanguageCode"
                     Core.<*> x Core..:? "ModelName"
                     Core.<*> x Core..:? "ModelStatus"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateLanguageModelResponse' smart constructor.
data CreateLanguageModelResponse = CreateLanguageModelResponse'
  { baseModelName :: Core.Maybe Types.BaseModelName
    -- ^ The Amazon Transcribe standard language model, or base model you've used to create a custom language model.
  , inputDataConfig :: Core.Maybe Types.InputDataConfig
    -- ^ The data access role and Amazon S3 prefixes you've chosen to create your custom language model.
  , languageCode :: Core.Maybe Types.CLMLanguageCode
    -- ^ The language code of the text you've used to create a custom language model.
  , modelName :: Core.Maybe Types.ModelName
    -- ^ The name you've chosen for your custom language model.
  , modelStatus :: Core.Maybe Types.ModelStatus
    -- ^ The status of the custom language model. When the status is @COMPLETED@ the model is ready to use.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLanguageModelResponse' value with any optional fields omitted.
mkCreateLanguageModelResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateLanguageModelResponse
mkCreateLanguageModelResponse responseStatus
  = CreateLanguageModelResponse'{baseModelName = Core.Nothing,
                                 inputDataConfig = Core.Nothing, languageCode = Core.Nothing,
                                 modelName = Core.Nothing, modelStatus = Core.Nothing,
                                 responseStatus}

-- | The Amazon Transcribe standard language model, or base model you've used to create a custom language model.
--
-- /Note:/ Consider using 'baseModelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clmrrsBaseModelName :: Lens.Lens' CreateLanguageModelResponse (Core.Maybe Types.BaseModelName)
clmrrsBaseModelName = Lens.field @"baseModelName"
{-# INLINEABLE clmrrsBaseModelName #-}
{-# DEPRECATED baseModelName "Use generic-lens or generic-optics with 'baseModelName' instead"  #-}

-- | The data access role and Amazon S3 prefixes you've chosen to create your custom language model.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clmrrsInputDataConfig :: Lens.Lens' CreateLanguageModelResponse (Core.Maybe Types.InputDataConfig)
clmrrsInputDataConfig = Lens.field @"inputDataConfig"
{-# INLINEABLE clmrrsInputDataConfig #-}
{-# DEPRECATED inputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead"  #-}

-- | The language code of the text you've used to create a custom language model.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clmrrsLanguageCode :: Lens.Lens' CreateLanguageModelResponse (Core.Maybe Types.CLMLanguageCode)
clmrrsLanguageCode = Lens.field @"languageCode"
{-# INLINEABLE clmrrsLanguageCode #-}
{-# DEPRECATED languageCode "Use generic-lens or generic-optics with 'languageCode' instead"  #-}

-- | The name you've chosen for your custom language model.
--
-- /Note:/ Consider using 'modelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clmrrsModelName :: Lens.Lens' CreateLanguageModelResponse (Core.Maybe Types.ModelName)
clmrrsModelName = Lens.field @"modelName"
{-# INLINEABLE clmrrsModelName #-}
{-# DEPRECATED modelName "Use generic-lens or generic-optics with 'modelName' instead"  #-}

-- | The status of the custom language model. When the status is @COMPLETED@ the model is ready to use.
--
-- /Note:/ Consider using 'modelStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clmrrsModelStatus :: Lens.Lens' CreateLanguageModelResponse (Core.Maybe Types.ModelStatus)
clmrrsModelStatus = Lens.field @"modelStatus"
{-# INLINEABLE clmrrsModelStatus #-}
{-# DEPRECATED modelStatus "Use generic-lens or generic-optics with 'modelStatus' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clmrrsResponseStatus :: Lens.Lens' CreateLanguageModelResponse Core.Int
clmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE clmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.DescribeLanguageModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a single custom language model. Use this information to see details about the language model in your AWS account. You can also see whether the base language model used to create your custom language model has been updated. If Amazon Transcribe has updated the base model, you can create a new custom language model using the updated base model. If the language model wasn't created, you can use this operation to understand why Amazon Transcribe couldn't create it. 
module Network.AWS.Transcribe.DescribeLanguageModel
    (
    -- * Creating a request
      DescribeLanguageModel (..)
    , mkDescribeLanguageModel
    -- ** Request lenses
    , dModelName

    -- * Destructuring the response
    , DescribeLanguageModelResponse (..)
    , mkDescribeLanguageModelResponse
    -- ** Response lenses
    , dlmrrsLanguageModel
    , dlmrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Transcribe.Types as Types

-- | /See:/ 'mkDescribeLanguageModel' smart constructor.
newtype DescribeLanguageModel = DescribeLanguageModel'
  { modelName :: Types.ModelName
    -- ^ The name of the custom language model you submit to get more information.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLanguageModel' value with any optional fields omitted.
mkDescribeLanguageModel
    :: Types.ModelName -- ^ 'modelName'
    -> DescribeLanguageModel
mkDescribeLanguageModel modelName
  = DescribeLanguageModel'{modelName}

-- | The name of the custom language model you submit to get more information.
--
-- /Note:/ Consider using 'modelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dModelName :: Lens.Lens' DescribeLanguageModel Types.ModelName
dModelName = Lens.field @"modelName"
{-# INLINEABLE dModelName #-}
{-# DEPRECATED modelName "Use generic-lens or generic-optics with 'modelName' instead"  #-}

instance Core.ToQuery DescribeLanguageModel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeLanguageModel where
        toHeaders DescribeLanguageModel{..}
          = Core.pure ("X-Amz-Target", "Transcribe.DescribeLanguageModel")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeLanguageModel where
        toJSON DescribeLanguageModel{..}
          = Core.object
              (Core.catMaybes [Core.Just ("ModelName" Core..= modelName)])

instance Core.AWSRequest DescribeLanguageModel where
        type Rs DescribeLanguageModel = DescribeLanguageModelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeLanguageModelResponse' Core.<$>
                   (x Core..:? "LanguageModel") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeLanguageModelResponse' smart constructor.
data DescribeLanguageModelResponse = DescribeLanguageModelResponse'
  { languageModel :: Core.Maybe Types.LanguageModel
    -- ^ The name of the custom language model you requested more information about.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeLanguageModelResponse' value with any optional fields omitted.
mkDescribeLanguageModelResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeLanguageModelResponse
mkDescribeLanguageModelResponse responseStatus
  = DescribeLanguageModelResponse'{languageModel = Core.Nothing,
                                   responseStatus}

-- | The name of the custom language model you requested more information about.
--
-- /Note:/ Consider using 'languageModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlmrrsLanguageModel :: Lens.Lens' DescribeLanguageModelResponse (Core.Maybe Types.LanguageModel)
dlmrrsLanguageModel = Lens.field @"languageModel"
{-# INLINEABLE dlmrrsLanguageModel #-}
{-# DEPRECATED languageModel "Use generic-lens or generic-optics with 'languageModel' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlmrrsResponseStatus :: Lens.Lens' DescribeLanguageModelResponse Core.Int
dlmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dlmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

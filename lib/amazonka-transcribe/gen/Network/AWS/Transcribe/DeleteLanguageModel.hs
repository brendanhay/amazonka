{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.DeleteLanguageModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a custom language model using its name.
module Network.AWS.Transcribe.DeleteLanguageModel
    (
    -- * Creating a request
      DeleteLanguageModel (..)
    , mkDeleteLanguageModel
    -- ** Request lenses
    , dlmModelName

    -- * Destructuring the response
    , DeleteLanguageModelResponse (..)
    , mkDeleteLanguageModelResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Transcribe.Types as Types

-- | /See:/ 'mkDeleteLanguageModel' smart constructor.
newtype DeleteLanguageModel = DeleteLanguageModel'
  { modelName :: Types.ModelName
    -- ^ The name of the model you're choosing to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLanguageModel' value with any optional fields omitted.
mkDeleteLanguageModel
    :: Types.ModelName -- ^ 'modelName'
    -> DeleteLanguageModel
mkDeleteLanguageModel modelName = DeleteLanguageModel'{modelName}

-- | The name of the model you're choosing to delete.
--
-- /Note:/ Consider using 'modelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlmModelName :: Lens.Lens' DeleteLanguageModel Types.ModelName
dlmModelName = Lens.field @"modelName"
{-# INLINEABLE dlmModelName #-}
{-# DEPRECATED modelName "Use generic-lens or generic-optics with 'modelName' instead"  #-}

instance Core.ToQuery DeleteLanguageModel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteLanguageModel where
        toHeaders DeleteLanguageModel{..}
          = Core.pure ("X-Amz-Target", "Transcribe.DeleteLanguageModel")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteLanguageModel where
        toJSON DeleteLanguageModel{..}
          = Core.object
              (Core.catMaybes [Core.Just ("ModelName" Core..= modelName)])

instance Core.AWSRequest DeleteLanguageModel where
        type Rs DeleteLanguageModel = DeleteLanguageModelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteLanguageModelResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteLanguageModelResponse' smart constructor.
data DeleteLanguageModelResponse = DeleteLanguageModelResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLanguageModelResponse' value with any optional fields omitted.
mkDeleteLanguageModelResponse
    :: DeleteLanguageModelResponse
mkDeleteLanguageModelResponse = DeleteLanguageModelResponse'

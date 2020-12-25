{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a model. The @DeleteModel@ API deletes only the model entry that was created in Amazon SageMaker when you called the 'CreateModel' API. It does not delete model artifacts, inference code, or the IAM role that you specified when creating the model.
module Network.AWS.SageMaker.DeleteModel
  ( -- * Creating a request
    DeleteModel (..),
    mkDeleteModel,

    -- ** Request lenses
    dmModelName,

    -- * Destructuring the response
    DeleteModelResponse (..),
    mkDeleteModelResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDeleteModel' smart constructor.
newtype DeleteModel = DeleteModel'
  { -- | The name of the model to delete.
    modelName :: Types.ModelName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteModel' value with any optional fields omitted.
mkDeleteModel ::
  -- | 'modelName'
  Types.ModelName ->
  DeleteModel
mkDeleteModel modelName = DeleteModel' {modelName}

-- | The name of the model to delete.
--
-- /Note:/ Consider using 'modelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmModelName :: Lens.Lens' DeleteModel Types.ModelName
dmModelName = Lens.field @"modelName"
{-# DEPRECATED dmModelName "Use generic-lens or generic-optics with 'modelName' instead." #-}

instance Core.FromJSON DeleteModel where
  toJSON DeleteModel {..} =
    Core.object
      (Core.catMaybes [Core.Just ("ModelName" Core..= modelName)])

instance Core.AWSRequest DeleteModel where
  type Rs DeleteModel = DeleteModelResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.DeleteModel")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DeleteModelResponse'

-- | /See:/ 'mkDeleteModelResponse' smart constructor.
data DeleteModelResponse = DeleteModelResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteModelResponse' value with any optional fields omitted.
mkDeleteModelResponse ::
  DeleteModelResponse
mkDeleteModelResponse = DeleteModelResponse'

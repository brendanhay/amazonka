{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteExperiment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon SageMaker experiment. All trials associated with the experiment must be deleted first. Use the 'ListTrials' API to get a list of the trials associated with the experiment.
module Network.AWS.SageMaker.DeleteExperiment
  ( -- * Creating a request
    DeleteExperiment (..),
    mkDeleteExperiment,

    -- ** Request lenses
    dExperimentName,

    -- * Destructuring the response
    DeleteExperimentResponse (..),
    mkDeleteExperimentResponse,

    -- ** Response lenses
    derfrsExperimentArn,
    derfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDeleteExperiment' smart constructor.
newtype DeleteExperiment = DeleteExperiment'
  { -- | The name of the experiment to delete.
    experimentName :: Types.ExperimentEntityName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteExperiment' value with any optional fields omitted.
mkDeleteExperiment ::
  -- | 'experimentName'
  Types.ExperimentEntityName ->
  DeleteExperiment
mkDeleteExperiment experimentName =
  DeleteExperiment' {experimentName}

-- | The name of the experiment to delete.
--
-- /Note:/ Consider using 'experimentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dExperimentName :: Lens.Lens' DeleteExperiment Types.ExperimentEntityName
dExperimentName = Lens.field @"experimentName"
{-# DEPRECATED dExperimentName "Use generic-lens or generic-optics with 'experimentName' instead." #-}

instance Core.FromJSON DeleteExperiment where
  toJSON DeleteExperiment {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ExperimentName" Core..= experimentName)]
      )

instance Core.AWSRequest DeleteExperiment where
  type Rs DeleteExperiment = DeleteExperimentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.DeleteExperiment")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteExperimentResponse'
            Core.<$> (x Core..:? "ExperimentArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteExperimentResponse' smart constructor.
data DeleteExperimentResponse = DeleteExperimentResponse'
  { -- | The Amazon Resource Name (ARN) of the experiment that is being deleted.
    experimentArn :: Core.Maybe Types.ExperimentArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteExperimentResponse' value with any optional fields omitted.
mkDeleteExperimentResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteExperimentResponse
mkDeleteExperimentResponse responseStatus =
  DeleteExperimentResponse'
    { experimentArn = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the experiment that is being deleted.
--
-- /Note:/ Consider using 'experimentArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derfrsExperimentArn :: Lens.Lens' DeleteExperimentResponse (Core.Maybe Types.ExperimentArn)
derfrsExperimentArn = Lens.field @"experimentArn"
{-# DEPRECATED derfrsExperimentArn "Use generic-lens or generic-optics with 'experimentArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derfrsResponseStatus :: Lens.Lens' DeleteExperimentResponse Core.Int
derfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED derfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

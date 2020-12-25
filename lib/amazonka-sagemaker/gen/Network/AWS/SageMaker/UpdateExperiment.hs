{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.UpdateExperiment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds, updates, or removes the description of an experiment. Updates the display name of an experiment.
module Network.AWS.SageMaker.UpdateExperiment
  ( -- * Creating a request
    UpdateExperiment (..),
    mkUpdateExperiment,

    -- ** Request lenses
    ueExperimentName,
    ueDescription,
    ueDisplayName,

    -- * Destructuring the response
    UpdateExperimentResponse (..),
    mkUpdateExperimentResponse,

    -- ** Response lenses
    uerrsExperimentArn,
    uerrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkUpdateExperiment' smart constructor.
data UpdateExperiment = UpdateExperiment'
  { -- | The name of the experiment to update.
    experimentName :: Types.ExperimentEntityName,
    -- | The description of the experiment.
    description :: Core.Maybe Types.Description,
    -- | The name of the experiment as displayed. The name doesn't need to be unique. If @DisplayName@ isn't specified, @ExperimentName@ is displayed.
    displayName :: Core.Maybe Types.ExperimentEntityName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateExperiment' value with any optional fields omitted.
mkUpdateExperiment ::
  -- | 'experimentName'
  Types.ExperimentEntityName ->
  UpdateExperiment
mkUpdateExperiment experimentName =
  UpdateExperiment'
    { experimentName,
      description = Core.Nothing,
      displayName = Core.Nothing
    }

-- | The name of the experiment to update.
--
-- /Note:/ Consider using 'experimentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueExperimentName :: Lens.Lens' UpdateExperiment Types.ExperimentEntityName
ueExperimentName = Lens.field @"experimentName"
{-# DEPRECATED ueExperimentName "Use generic-lens or generic-optics with 'experimentName' instead." #-}

-- | The description of the experiment.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueDescription :: Lens.Lens' UpdateExperiment (Core.Maybe Types.Description)
ueDescription = Lens.field @"description"
{-# DEPRECATED ueDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the experiment as displayed. The name doesn't need to be unique. If @DisplayName@ isn't specified, @ExperimentName@ is displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueDisplayName :: Lens.Lens' UpdateExperiment (Core.Maybe Types.ExperimentEntityName)
ueDisplayName = Lens.field @"displayName"
{-# DEPRECATED ueDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

instance Core.FromJSON UpdateExperiment where
  toJSON UpdateExperiment {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ExperimentName" Core..= experimentName),
            ("Description" Core..=) Core.<$> description,
            ("DisplayName" Core..=) Core.<$> displayName
          ]
      )

instance Core.AWSRequest UpdateExperiment where
  type Rs UpdateExperiment = UpdateExperimentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.UpdateExperiment")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateExperimentResponse'
            Core.<$> (x Core..:? "ExperimentArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateExperimentResponse' smart constructor.
data UpdateExperimentResponse = UpdateExperimentResponse'
  { -- | The Amazon Resource Name (ARN) of the experiment.
    experimentArn :: Core.Maybe Types.ExperimentArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateExperimentResponse' value with any optional fields omitted.
mkUpdateExperimentResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateExperimentResponse
mkUpdateExperimentResponse responseStatus =
  UpdateExperimentResponse'
    { experimentArn = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the experiment.
--
-- /Note:/ Consider using 'experimentArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uerrsExperimentArn :: Lens.Lens' UpdateExperimentResponse (Core.Maybe Types.ExperimentArn)
uerrsExperimentArn = Lens.field @"experimentArn"
{-# DEPRECATED uerrsExperimentArn "Use generic-lens or generic-optics with 'experimentArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uerrsResponseStatus :: Lens.Lens' UpdateExperimentResponse Core.Int
uerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

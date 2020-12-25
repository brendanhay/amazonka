{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.UpdateTrial
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the display name of a trial.
module Network.AWS.SageMaker.UpdateTrial
  ( -- * Creating a request
    UpdateTrial (..),
    mkUpdateTrial,

    -- ** Request lenses
    utTrialName,
    utDisplayName,

    -- * Destructuring the response
    UpdateTrialResponse (..),
    mkUpdateTrialResponse,

    -- ** Response lenses
    utrrsTrialArn,
    utrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkUpdateTrial' smart constructor.
data UpdateTrial = UpdateTrial'
  { -- | The name of the trial to update.
    trialName :: Types.TrialName,
    -- | The name of the trial as displayed. The name doesn't need to be unique. If @DisplayName@ isn't specified, @TrialName@ is displayed.
    displayName :: Core.Maybe Types.DisplayName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTrial' value with any optional fields omitted.
mkUpdateTrial ::
  -- | 'trialName'
  Types.TrialName ->
  UpdateTrial
mkUpdateTrial trialName =
  UpdateTrial' {trialName, displayName = Core.Nothing}

-- | The name of the trial to update.
--
-- /Note:/ Consider using 'trialName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utTrialName :: Lens.Lens' UpdateTrial Types.TrialName
utTrialName = Lens.field @"trialName"
{-# DEPRECATED utTrialName "Use generic-lens or generic-optics with 'trialName' instead." #-}

-- | The name of the trial as displayed. The name doesn't need to be unique. If @DisplayName@ isn't specified, @TrialName@ is displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utDisplayName :: Lens.Lens' UpdateTrial (Core.Maybe Types.DisplayName)
utDisplayName = Lens.field @"displayName"
{-# DEPRECATED utDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

instance Core.FromJSON UpdateTrial where
  toJSON UpdateTrial {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TrialName" Core..= trialName),
            ("DisplayName" Core..=) Core.<$> displayName
          ]
      )

instance Core.AWSRequest UpdateTrial where
  type Rs UpdateTrial = UpdateTrialResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.UpdateTrial")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTrialResponse'
            Core.<$> (x Core..:? "TrialArn") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateTrialResponse' smart constructor.
data UpdateTrialResponse = UpdateTrialResponse'
  { -- | The Amazon Resource Name (ARN) of the trial.
    trialArn :: Core.Maybe Types.TrialArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTrialResponse' value with any optional fields omitted.
mkUpdateTrialResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateTrialResponse
mkUpdateTrialResponse responseStatus =
  UpdateTrialResponse' {trialArn = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) of the trial.
--
-- /Note:/ Consider using 'trialArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrrsTrialArn :: Lens.Lens' UpdateTrialResponse (Core.Maybe Types.TrialArn)
utrrsTrialArn = Lens.field @"trialArn"
{-# DEPRECATED utrrsTrialArn "Use generic-lens or generic-optics with 'trialArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrrsResponseStatus :: Lens.Lens' UpdateTrialResponse Core.Int
utrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED utrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

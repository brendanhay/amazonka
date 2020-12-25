{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteTrialComponent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified trial component. A trial component must be disassociated from all trials before the trial component can be deleted. To disassociate a trial component from a trial, call the 'DisassociateTrialComponent' API.
module Network.AWS.SageMaker.DeleteTrialComponent
  ( -- * Creating a request
    DeleteTrialComponent (..),
    mkDeleteTrialComponent,

    -- ** Request lenses
    dTrialComponentName,

    -- * Destructuring the response
    DeleteTrialComponentResponse (..),
    mkDeleteTrialComponentResponse,

    -- ** Response lenses
    dtcrfrsTrialComponentArn,
    dtcrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDeleteTrialComponent' smart constructor.
newtype DeleteTrialComponent = DeleteTrialComponent'
  { -- | The name of the component to delete.
    trialComponentName :: Types.TrialComponentName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTrialComponent' value with any optional fields omitted.
mkDeleteTrialComponent ::
  -- | 'trialComponentName'
  Types.TrialComponentName ->
  DeleteTrialComponent
mkDeleteTrialComponent trialComponentName =
  DeleteTrialComponent' {trialComponentName}

-- | The name of the component to delete.
--
-- /Note:/ Consider using 'trialComponentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTrialComponentName :: Lens.Lens' DeleteTrialComponent Types.TrialComponentName
dTrialComponentName = Lens.field @"trialComponentName"
{-# DEPRECATED dTrialComponentName "Use generic-lens or generic-optics with 'trialComponentName' instead." #-}

instance Core.FromJSON DeleteTrialComponent where
  toJSON DeleteTrialComponent {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("TrialComponentName" Core..= trialComponentName)]
      )

instance Core.AWSRequest DeleteTrialComponent where
  type Rs DeleteTrialComponent = DeleteTrialComponentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.DeleteTrialComponent")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteTrialComponentResponse'
            Core.<$> (x Core..:? "TrialComponentArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteTrialComponentResponse' smart constructor.
data DeleteTrialComponentResponse = DeleteTrialComponentResponse'
  { -- | The Amazon Resource Name (ARN) of the component is being deleted.
    trialComponentArn :: Core.Maybe Types.TrialComponentArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTrialComponentResponse' value with any optional fields omitted.
mkDeleteTrialComponentResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteTrialComponentResponse
mkDeleteTrialComponentResponse responseStatus =
  DeleteTrialComponentResponse'
    { trialComponentArn = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the component is being deleted.
--
-- /Note:/ Consider using 'trialComponentArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcrfrsTrialComponentArn :: Lens.Lens' DeleteTrialComponentResponse (Core.Maybe Types.TrialComponentArn)
dtcrfrsTrialComponentArn = Lens.field @"trialComponentArn"
{-# DEPRECATED dtcrfrsTrialComponentArn "Use generic-lens or generic-optics with 'trialComponentArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcrfrsResponseStatus :: Lens.Lens' DeleteTrialComponentResponse Core.Int
dtcrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtcrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

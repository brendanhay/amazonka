{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteTrial
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified trial. All trial components that make up the trial must be deleted first. Use the 'DescribeTrialComponent' API to get the list of trial components.
module Network.AWS.SageMaker.DeleteTrial
  ( -- * Creating a request
    DeleteTrial (..),
    mkDeleteTrial,

    -- ** Request lenses
    dTrialName,

    -- * Destructuring the response
    DeleteTrialResponse (..),
    mkDeleteTrialResponse,

    -- ** Response lenses
    drsTrialArn,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDeleteTrial' smart constructor.
newtype DeleteTrial = DeleteTrial'
  { -- | The name of the trial to delete.
    trialName :: Types.TrialName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTrial' value with any optional fields omitted.
mkDeleteTrial ::
  -- | 'trialName'
  Types.TrialName ->
  DeleteTrial
mkDeleteTrial trialName = DeleteTrial' {trialName}

-- | The name of the trial to delete.
--
-- /Note:/ Consider using 'trialName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTrialName :: Lens.Lens' DeleteTrial Types.TrialName
dTrialName = Lens.field @"trialName"
{-# DEPRECATED dTrialName "Use generic-lens or generic-optics with 'trialName' instead." #-}

instance Core.FromJSON DeleteTrial where
  toJSON DeleteTrial {..} =
    Core.object
      (Core.catMaybes [Core.Just ("TrialName" Core..= trialName)])

instance Core.AWSRequest DeleteTrial where
  type Rs DeleteTrial = DeleteTrialResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.DeleteTrial")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteTrialResponse'
            Core.<$> (x Core..:? "TrialArn") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteTrialResponse' smart constructor.
data DeleteTrialResponse = DeleteTrialResponse'
  { -- | The Amazon Resource Name (ARN) of the trial that is being deleted.
    trialArn :: Core.Maybe Types.TrialArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTrialResponse' value with any optional fields omitted.
mkDeleteTrialResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteTrialResponse
mkDeleteTrialResponse responseStatus =
  DeleteTrialResponse' {trialArn = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) of the trial that is being deleted.
--
-- /Note:/ Consider using 'trialArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsTrialArn :: Lens.Lens' DeleteTrialResponse (Core.Maybe Types.TrialArn)
drsTrialArn = Lens.field @"trialArn"
{-# DEPRECATED drsTrialArn "Use generic-lens or generic-optics with 'trialArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteTrialResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.DeleteActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an activity.
module Network.AWS.StepFunctions.DeleteActivity
  ( -- * Creating a request
    DeleteActivity (..),
    mkDeleteActivity,

    -- ** Request lenses
    daActivityArn,

    -- * Destructuring the response
    DeleteActivityResponse (..),
    mkDeleteActivityResponse,

    -- ** Response lenses
    darrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StepFunctions.Types as Types

-- | /See:/ 'mkDeleteActivity' smart constructor.
newtype DeleteActivity = DeleteActivity'
  { -- | The Amazon Resource Name (ARN) of the activity to delete.
    activityArn :: Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteActivity' value with any optional fields omitted.
mkDeleteActivity ::
  -- | 'activityArn'
  Types.Arn ->
  DeleteActivity
mkDeleteActivity activityArn = DeleteActivity' {activityArn}

-- | The Amazon Resource Name (ARN) of the activity to delete.
--
-- /Note:/ Consider using 'activityArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daActivityArn :: Lens.Lens' DeleteActivity Types.Arn
daActivityArn = Lens.field @"activityArn"
{-# DEPRECATED daActivityArn "Use generic-lens or generic-optics with 'activityArn' instead." #-}

instance Core.FromJSON DeleteActivity where
  toJSON DeleteActivity {..} =
    Core.object
      (Core.catMaybes [Core.Just ("activityArn" Core..= activityArn)])

instance Core.AWSRequest DeleteActivity where
  type Rs DeleteActivity = DeleteActivityResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSStepFunctions.DeleteActivity")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteActivityResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteActivityResponse' smart constructor.
newtype DeleteActivityResponse = DeleteActivityResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteActivityResponse' value with any optional fields omitted.
mkDeleteActivityResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteActivityResponse
mkDeleteActivityResponse responseStatus =
  DeleteActivityResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsResponseStatus :: Lens.Lens' DeleteActivityResponse Core.Int
darrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED darrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

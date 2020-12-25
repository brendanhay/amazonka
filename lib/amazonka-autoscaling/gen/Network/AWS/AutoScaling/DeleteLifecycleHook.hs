{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DeleteLifecycleHook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified lifecycle hook.
--
-- If there are any outstanding lifecycle actions, they are completed first (@ABANDON@ for launching instances, @CONTINUE@ for terminating instances).
module Network.AWS.AutoScaling.DeleteLifecycleHook
  ( -- * Creating a request
    DeleteLifecycleHook (..),
    mkDeleteLifecycleHook,

    -- ** Request lenses
    dlhfLifecycleHookName,
    dlhfAutoScalingGroupName,

    -- * Destructuring the response
    DeleteLifecycleHookResponse (..),
    mkDeleteLifecycleHookResponse,

    -- ** Response lenses
    dlhrfrsResponseStatus,
  )
where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteLifecycleHook' smart constructor.
data DeleteLifecycleHook = DeleteLifecycleHook'
  { -- | The name of the lifecycle hook.
    lifecycleHookName :: Types.LifecycleHookName,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Types.AutoScalingGroupName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLifecycleHook' value with any optional fields omitted.
mkDeleteLifecycleHook ::
  -- | 'lifecycleHookName'
  Types.LifecycleHookName ->
  -- | 'autoScalingGroupName'
  Types.AutoScalingGroupName ->
  DeleteLifecycleHook
mkDeleteLifecycleHook lifecycleHookName autoScalingGroupName =
  DeleteLifecycleHook' {lifecycleHookName, autoScalingGroupName}

-- | The name of the lifecycle hook.
--
-- /Note:/ Consider using 'lifecycleHookName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlhfLifecycleHookName :: Lens.Lens' DeleteLifecycleHook Types.LifecycleHookName
dlhfLifecycleHookName = Lens.field @"lifecycleHookName"
{-# DEPRECATED dlhfLifecycleHookName "Use generic-lens or generic-optics with 'lifecycleHookName' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlhfAutoScalingGroupName :: Lens.Lens' DeleteLifecycleHook Types.AutoScalingGroupName
dlhfAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# DEPRECATED dlhfAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

instance Core.AWSRequest DeleteLifecycleHook where
  type Rs DeleteLifecycleHook = DeleteLifecycleHookResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DeleteLifecycleHook")
                Core.<> (Core.pure ("Version", "2011-01-01"))
                Core.<> (Core.toQueryValue "LifecycleHookName" lifecycleHookName)
                Core.<> (Core.toQueryValue "AutoScalingGroupName" autoScalingGroupName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DeleteLifecycleHookResult"
      ( \s h x ->
          DeleteLifecycleHookResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteLifecycleHookResponse' smart constructor.
newtype DeleteLifecycleHookResponse = DeleteLifecycleHookResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLifecycleHookResponse' value with any optional fields omitted.
mkDeleteLifecycleHookResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteLifecycleHookResponse
mkDeleteLifecycleHookResponse responseStatus =
  DeleteLifecycleHookResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlhrfrsResponseStatus :: Lens.Lens' DeleteLifecycleHookResponse Core.Int
dlhrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dlhrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

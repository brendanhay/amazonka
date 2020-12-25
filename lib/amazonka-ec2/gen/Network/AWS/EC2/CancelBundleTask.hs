{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CancelBundleTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a bundling operation for an instance store-backed Windows instance.
module Network.AWS.EC2.CancelBundleTask
  ( -- * Creating a request
    CancelBundleTask (..),
    mkCancelBundleTask,

    -- ** Request lenses
    cbtBundleId,
    cbtDryRun,

    -- * Destructuring the response
    CancelBundleTaskResponse (..),
    mkCancelBundleTaskResponse,

    -- ** Response lenses
    cbtrrsBundleTask,
    cbtrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for CancelBundleTask.
--
-- /See:/ 'mkCancelBundleTask' smart constructor.
data CancelBundleTask = CancelBundleTask'
  { -- | The ID of the bundle task.
    bundleId :: Types.BundleId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelBundleTask' value with any optional fields omitted.
mkCancelBundleTask ::
  -- | 'bundleId'
  Types.BundleId ->
  CancelBundleTask
mkCancelBundleTask bundleId =
  CancelBundleTask' {bundleId, dryRun = Core.Nothing}

-- | The ID of the bundle task.
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbtBundleId :: Lens.Lens' CancelBundleTask Types.BundleId
cbtBundleId = Lens.field @"bundleId"
{-# DEPRECATED cbtBundleId "Use generic-lens or generic-optics with 'bundleId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbtDryRun :: Lens.Lens' CancelBundleTask (Core.Maybe Core.Bool)
cbtDryRun = Lens.field @"dryRun"
{-# DEPRECATED cbtDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest CancelBundleTask where
  type Rs CancelBundleTask = CancelBundleTaskResponse
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
            ( Core.pure ("Action", "CancelBundleTask")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "BundleId" bundleId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CancelBundleTaskResponse'
            Core.<$> (x Core..@? "bundleInstanceTask")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of CancelBundleTask.
--
-- /See:/ 'mkCancelBundleTaskResponse' smart constructor.
data CancelBundleTaskResponse = CancelBundleTaskResponse'
  { -- | Information about the bundle task.
    bundleTask :: Core.Maybe Types.BundleTask,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CancelBundleTaskResponse' value with any optional fields omitted.
mkCancelBundleTaskResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CancelBundleTaskResponse
mkCancelBundleTaskResponse responseStatus =
  CancelBundleTaskResponse'
    { bundleTask = Core.Nothing,
      responseStatus
    }

-- | Information about the bundle task.
--
-- /Note:/ Consider using 'bundleTask' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbtrrsBundleTask :: Lens.Lens' CancelBundleTaskResponse (Core.Maybe Types.BundleTask)
cbtrrsBundleTask = Lens.field @"bundleTask"
{-# DEPRECATED cbtrrsBundleTask "Use generic-lens or generic-optics with 'bundleTask' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbtrrsResponseStatus :: Lens.Lens' CancelBundleTaskResponse Core.Int
cbtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cbtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

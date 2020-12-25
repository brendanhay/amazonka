{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DeleteTapePool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a custom tape pool. A custom tape pool can only be deleted if there are no tapes in the pool and if there are no automatic tape creation policies that reference the custom tape pool.
module Network.AWS.StorageGateway.DeleteTapePool
  ( -- * Creating a request
    DeleteTapePool (..),
    mkDeleteTapePool,

    -- ** Request lenses
    dtpPoolARN,

    -- * Destructuring the response
    DeleteTapePoolResponse (..),
    mkDeleteTapePoolResponse,

    -- ** Response lenses
    dtprrsPoolARN,
    dtprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | /See:/ 'mkDeleteTapePool' smart constructor.
newtype DeleteTapePool = DeleteTapePool'
  { -- | The Amazon Resource Name (ARN) of the custom tape pool to delete.
    poolARN :: Types.PoolARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTapePool' value with any optional fields omitted.
mkDeleteTapePool ::
  -- | 'poolARN'
  Types.PoolARN ->
  DeleteTapePool
mkDeleteTapePool poolARN = DeleteTapePool' {poolARN}

-- | The Amazon Resource Name (ARN) of the custom tape pool to delete.
--
-- /Note:/ Consider using 'poolARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtpPoolARN :: Lens.Lens' DeleteTapePool Types.PoolARN
dtpPoolARN = Lens.field @"poolARN"
{-# DEPRECATED dtpPoolARN "Use generic-lens or generic-optics with 'poolARN' instead." #-}

instance Core.FromJSON DeleteTapePool where
  toJSON DeleteTapePool {..} =
    Core.object
      (Core.catMaybes [Core.Just ("PoolARN" Core..= poolARN)])

instance Core.AWSRequest DeleteTapePool where
  type Rs DeleteTapePool = DeleteTapePoolResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StorageGateway_20130630.DeleteTapePool")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteTapePoolResponse'
            Core.<$> (x Core..:? "PoolARN") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteTapePoolResponse' smart constructor.
data DeleteTapePoolResponse = DeleteTapePoolResponse'
  { -- | The Amazon Resource Name (ARN) of the custom tape pool being deleted.
    poolARN :: Core.Maybe Types.PoolARN,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTapePoolResponse' value with any optional fields omitted.
mkDeleteTapePoolResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteTapePoolResponse
mkDeleteTapePoolResponse responseStatus =
  DeleteTapePoolResponse' {poolARN = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) of the custom tape pool being deleted.
--
-- /Note:/ Consider using 'poolARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtprrsPoolARN :: Lens.Lens' DeleteTapePoolResponse (Core.Maybe Types.PoolARN)
dtprrsPoolARN = Lens.field @"poolARN"
{-# DEPRECATED dtprrsPoolARN "Use generic-lens or generic-optics with 'poolARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtprrsResponseStatus :: Lens.Lens' DeleteTapePoolResponse Core.Int
dtprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.CancelCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a cluster job. You can only cancel a cluster job while it's in the @AwaitingQuorum@ status. You'll have at least an hour after creating a cluster job to cancel it.
module Network.AWS.Snowball.CancelCluster
  ( -- * Creating a request
    CancelCluster (..),
    mkCancelCluster,

    -- ** Request lenses
    ccClusterId,

    -- * Destructuring the response
    CancelClusterResponse (..),
    mkCancelClusterResponse,

    -- ** Response lenses
    ccrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Snowball.Types as Types

-- | /See:/ 'mkCancelCluster' smart constructor.
newtype CancelCluster = CancelCluster'
  { -- | The 39-character ID for the cluster that you want to cancel, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
    clusterId :: Types.ClusterId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelCluster' value with any optional fields omitted.
mkCancelCluster ::
  -- | 'clusterId'
  Types.ClusterId ->
  CancelCluster
mkCancelCluster clusterId = CancelCluster' {clusterId}

-- | The 39-character ID for the cluster that you want to cancel, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccClusterId :: Lens.Lens' CancelCluster Types.ClusterId
ccClusterId = Lens.field @"clusterId"
{-# DEPRECATED ccClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

instance Core.FromJSON CancelCluster where
  toJSON CancelCluster {..} =
    Core.object
      (Core.catMaybes [Core.Just ("ClusterId" Core..= clusterId)])

instance Core.AWSRequest CancelCluster where
  type Rs CancelCluster = CancelClusterResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSIESnowballJobManagementService.CancelCluster")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelClusterResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCancelClusterResponse' smart constructor.
newtype CancelClusterResponse = CancelClusterResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelClusterResponse' value with any optional fields omitted.
mkCancelClusterResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CancelClusterResponse
mkCancelClusterResponse responseStatus =
  CancelClusterResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsResponseStatus :: Lens.Lens' CancelClusterResponse Core.Int
ccrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ccrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

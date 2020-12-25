{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DeleteTrust
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing trust relationship between your AWS Managed Microsoft AD directory and an external domain.
module Network.AWS.DirectoryService.DeleteTrust
  ( -- * Creating a request
    DeleteTrust (..),
    mkDeleteTrust,

    -- ** Request lenses
    dtTrustId,
    dtDeleteAssociatedConditionalForwarder,

    -- * Destructuring the response
    DeleteTrustResponse (..),
    mkDeleteTrustResponse,

    -- ** Response lenses
    drsTrustId,
    drsResponseStatus,
  )
where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Deletes the local side of an existing trust relationship between the AWS Managed Microsoft AD directory and the external domain.
--
-- /See:/ 'mkDeleteTrust' smart constructor.
data DeleteTrust = DeleteTrust'
  { -- | The Trust ID of the trust relationship to be deleted.
    trustId :: Types.TrustId,
    -- | Delete a conditional forwarder as part of a DeleteTrustRequest.
    deleteAssociatedConditionalForwarder :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTrust' value with any optional fields omitted.
mkDeleteTrust ::
  -- | 'trustId'
  Types.TrustId ->
  DeleteTrust
mkDeleteTrust trustId =
  DeleteTrust'
    { trustId,
      deleteAssociatedConditionalForwarder = Core.Nothing
    }

-- | The Trust ID of the trust relationship to be deleted.
--
-- /Note:/ Consider using 'trustId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTrustId :: Lens.Lens' DeleteTrust Types.TrustId
dtTrustId = Lens.field @"trustId"
{-# DEPRECATED dtTrustId "Use generic-lens or generic-optics with 'trustId' instead." #-}

-- | Delete a conditional forwarder as part of a DeleteTrustRequest.
--
-- /Note:/ Consider using 'deleteAssociatedConditionalForwarder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtDeleteAssociatedConditionalForwarder :: Lens.Lens' DeleteTrust (Core.Maybe Core.Bool)
dtDeleteAssociatedConditionalForwarder = Lens.field @"deleteAssociatedConditionalForwarder"
{-# DEPRECATED dtDeleteAssociatedConditionalForwarder "Use generic-lens or generic-optics with 'deleteAssociatedConditionalForwarder' instead." #-}

instance Core.FromJSON DeleteTrust where
  toJSON DeleteTrust {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TrustId" Core..= trustId),
            ("DeleteAssociatedConditionalForwarder" Core..=)
              Core.<$> deleteAssociatedConditionalForwarder
          ]
      )

instance Core.AWSRequest DeleteTrust where
  type Rs DeleteTrust = DeleteTrustResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DirectoryService_20150416.DeleteTrust")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteTrustResponse'
            Core.<$> (x Core..:? "TrustId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The result of a DeleteTrust request.
--
-- /See:/ 'mkDeleteTrustResponse' smart constructor.
data DeleteTrustResponse = DeleteTrustResponse'
  { -- | The Trust ID of the trust relationship that was deleted.
    trustId :: Core.Maybe Types.TrustId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTrustResponse' value with any optional fields omitted.
mkDeleteTrustResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteTrustResponse
mkDeleteTrustResponse responseStatus =
  DeleteTrustResponse' {trustId = Core.Nothing, responseStatus}

-- | The Trust ID of the trust relationship that was deleted.
--
-- /Note:/ Consider using 'trustId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsTrustId :: Lens.Lens' DeleteTrustResponse (Core.Maybe Types.TrustId)
drsTrustId = Lens.field @"trustId"
{-# DEPRECATED drsTrustId "Use generic-lens or generic-optics with 'trustId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteTrustResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DeleteLogSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified log subscription.
module Network.AWS.DirectoryService.DeleteLogSubscription
  ( -- * Creating a request
    DeleteLogSubscription (..),
    mkDeleteLogSubscription,

    -- ** Request lenses
    dlsDirectoryId,

    -- * Destructuring the response
    DeleteLogSubscriptionResponse (..),
    mkDeleteLogSubscriptionResponse,

    -- ** Response lenses
    dlsrrsResponseStatus,
  )
where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteLogSubscription' smart constructor.
newtype DeleteLogSubscription = DeleteLogSubscription'
  { -- | Identifier of the directory whose log subscription you want to delete.
    directoryId :: Types.DirectoryId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLogSubscription' value with any optional fields omitted.
mkDeleteLogSubscription ::
  -- | 'directoryId'
  Types.DirectoryId ->
  DeleteLogSubscription
mkDeleteLogSubscription directoryId =
  DeleteLogSubscription' {directoryId}

-- | Identifier of the directory whose log subscription you want to delete.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlsDirectoryId :: Lens.Lens' DeleteLogSubscription Types.DirectoryId
dlsDirectoryId = Lens.field @"directoryId"
{-# DEPRECATED dlsDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

instance Core.FromJSON DeleteLogSubscription where
  toJSON DeleteLogSubscription {..} =
    Core.object
      (Core.catMaybes [Core.Just ("DirectoryId" Core..= directoryId)])

instance Core.AWSRequest DeleteLogSubscription where
  type Rs DeleteLogSubscription = DeleteLogSubscriptionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DirectoryService_20150416.DeleteLogSubscription")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteLogSubscriptionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteLogSubscriptionResponse' smart constructor.
newtype DeleteLogSubscriptionResponse = DeleteLogSubscriptionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLogSubscriptionResponse' value with any optional fields omitted.
mkDeleteLogSubscriptionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteLogSubscriptionResponse
mkDeleteLogSubscriptionResponse responseStatus =
  DeleteLogSubscriptionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlsrrsResponseStatus :: Lens.Lens' DeleteLogSubscriptionResponse Core.Int
dlsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dlsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

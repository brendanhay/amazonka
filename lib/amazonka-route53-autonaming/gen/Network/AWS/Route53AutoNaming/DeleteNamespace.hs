{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.DeleteNamespace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a namespace from the current account. If the namespace still contains one or more services, the request fails.
module Network.AWS.Route53AutoNaming.DeleteNamespace
  ( -- * Creating a request
    DeleteNamespace (..),
    mkDeleteNamespace,

    -- ** Request lenses
    dnId,

    -- * Destructuring the response
    DeleteNamespaceResponse (..),
    mkDeleteNamespaceResponse,

    -- ** Response lenses
    dnrrsOperationId,
    dnrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53AutoNaming.Types as Types

-- | /See:/ 'mkDeleteNamespace' smart constructor.
newtype DeleteNamespace = DeleteNamespace'
  { -- | The ID of the namespace that you want to delete.
    id :: Types.ResourceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNamespace' value with any optional fields omitted.
mkDeleteNamespace ::
  -- | 'id'
  Types.ResourceId ->
  DeleteNamespace
mkDeleteNamespace id = DeleteNamespace' {id}

-- | The ID of the namespace that you want to delete.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnId :: Lens.Lens' DeleteNamespace Types.ResourceId
dnId = Lens.field @"id"
{-# DEPRECATED dnId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.FromJSON DeleteNamespace where
  toJSON DeleteNamespace {..} =
    Core.object (Core.catMaybes [Core.Just ("Id" Core..= id)])

instance Core.AWSRequest DeleteNamespace where
  type Rs DeleteNamespace = DeleteNamespaceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Route53AutoNaming_v20170314.DeleteNamespace")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteNamespaceResponse'
            Core.<$> (x Core..:? "OperationId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteNamespaceResponse' smart constructor.
data DeleteNamespaceResponse = DeleteNamespaceResponse'
  { -- | A value that you can use to determine whether the request completed successfully. To get the status of the operation, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation> .
    operationId :: Core.Maybe Types.OperationId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNamespaceResponse' value with any optional fields omitted.
mkDeleteNamespaceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteNamespaceResponse
mkDeleteNamespaceResponse responseStatus =
  DeleteNamespaceResponse'
    { operationId = Core.Nothing,
      responseStatus
    }

-- | A value that you can use to determine whether the request completed successfully. To get the status of the operation, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation> .
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnrrsOperationId :: Lens.Lens' DeleteNamespaceResponse (Core.Maybe Types.OperationId)
dnrrsOperationId = Lens.field @"operationId"
{-# DEPRECATED dnrrsOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnrrsResponseStatus :: Lens.Lens' DeleteNamespaceResponse Core.Int
dnrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dnrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

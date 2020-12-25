{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetOperation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific operation. Operations include events such as when you create an instance, allocate a static IP, attach a static IP, and so on.
module Network.AWS.Lightsail.GetOperation
  ( -- * Creating a request
    GetOperation (..),
    mkGetOperation,

    -- ** Request lenses
    goOperationId,

    -- * Destructuring the response
    GetOperationResponse (..),
    mkGetOperationResponse,

    -- ** Response lenses
    gorfrsOperation,
    gorfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetOperation' smart constructor.
newtype GetOperation = GetOperation'
  { -- | A GUID used to identify the operation.
    operationId :: Types.NonEmptyString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetOperation' value with any optional fields omitted.
mkGetOperation ::
  -- | 'operationId'
  Types.NonEmptyString ->
  GetOperation
mkGetOperation operationId = GetOperation' {operationId}

-- | A GUID used to identify the operation.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goOperationId :: Lens.Lens' GetOperation Types.NonEmptyString
goOperationId = Lens.field @"operationId"
{-# DEPRECATED goOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

instance Core.FromJSON GetOperation where
  toJSON GetOperation {..} =
    Core.object
      (Core.catMaybes [Core.Just ("operationId" Core..= operationId)])

instance Core.AWSRequest GetOperation where
  type Rs GetOperation = GetOperationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Lightsail_20161128.GetOperation")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOperationResponse'
            Core.<$> (x Core..:? "operation") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetOperationResponse' smart constructor.
data GetOperationResponse = GetOperationResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operation :: Core.Maybe Types.Operation,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetOperationResponse' value with any optional fields omitted.
mkGetOperationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetOperationResponse
mkGetOperationResponse responseStatus =
  GetOperationResponse' {operation = Core.Nothing, responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorfrsOperation :: Lens.Lens' GetOperationResponse (Core.Maybe Types.Operation)
gorfrsOperation = Lens.field @"operation"
{-# DEPRECATED gorfrsOperation "Use generic-lens or generic-optics with 'operation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorfrsResponseStatus :: Lens.Lens' GetOperationResponse Core.Int
gorfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gorfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

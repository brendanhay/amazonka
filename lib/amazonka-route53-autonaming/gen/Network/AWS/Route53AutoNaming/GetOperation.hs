{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.GetOperation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about any operation that returns an operation ID in the response, such as a @CreateService@ request.
module Network.AWS.Route53AutoNaming.GetOperation
  ( -- * Creating a request
    GetOperation (..),
    mkGetOperation,

    -- ** Request lenses
    goOperationId,

    -- * Destructuring the response
    GetOperationResponse (..),
    mkGetOperationResponse,

    -- ** Response lenses
    gorrsOperation,
    gorrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53AutoNaming.Types as Types

-- | /See:/ 'mkGetOperation' smart constructor.
newtype GetOperation = GetOperation'
  { -- | The ID of the operation that you want to get more information about.
    operationId :: Types.ResourceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetOperation' value with any optional fields omitted.
mkGetOperation ::
  -- | 'operationId'
  Types.ResourceId ->
  GetOperation
mkGetOperation operationId = GetOperation' {operationId}

-- | The ID of the operation that you want to get more information about.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goOperationId :: Lens.Lens' GetOperation Types.ResourceId
goOperationId = Lens.field @"operationId"
{-# DEPRECATED goOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

instance Core.FromJSON GetOperation where
  toJSON GetOperation {..} =
    Core.object
      (Core.catMaybes [Core.Just ("OperationId" Core..= operationId)])

instance Core.AWSRequest GetOperation where
  type Rs GetOperation = GetOperationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Route53AutoNaming_v20170314.GetOperation")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOperationResponse'
            Core.<$> (x Core..:? "Operation") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetOperationResponse' smart constructor.
data GetOperationResponse = GetOperationResponse'
  { -- | A complex type that contains information about the operation.
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

-- | A complex type that contains information about the operation.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsOperation :: Lens.Lens' GetOperationResponse (Core.Maybe Types.Operation)
gorrsOperation = Lens.field @"operation"
{-# DEPRECATED gorrsOperation "Use generic-lens or generic-optics with 'operation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsResponseStatus :: Lens.Lens' GetOperationResponse Core.Int
gorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

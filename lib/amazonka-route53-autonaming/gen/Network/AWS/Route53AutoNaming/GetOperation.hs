{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetOperation (..)
    , mkGetOperation
    -- ** Request lenses
    , goOperationId

    -- * Destructuring the response
    , GetOperationResponse (..)
    , mkGetOperationResponse
    -- ** Response lenses
    , gorrsOperation
    , gorrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53AutoNaming.Types as Types

-- | /See:/ 'mkGetOperation' smart constructor.
newtype GetOperation = GetOperation'
  { operationId :: Types.ResourceId
    -- ^ The ID of the operation that you want to get more information about.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetOperation' value with any optional fields omitted.
mkGetOperation
    :: Types.ResourceId -- ^ 'operationId'
    -> GetOperation
mkGetOperation operationId = GetOperation'{operationId}

-- | The ID of the operation that you want to get more information about.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goOperationId :: Lens.Lens' GetOperation Types.ResourceId
goOperationId = Lens.field @"operationId"
{-# INLINEABLE goOperationId #-}
{-# DEPRECATED operationId "Use generic-lens or generic-optics with 'operationId' instead"  #-}

instance Core.ToQuery GetOperation where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetOperation where
        toHeaders GetOperation{..}
          = Core.pure
              ("X-Amz-Target", "Route53AutoNaming_v20170314.GetOperation")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetOperation where
        toJSON GetOperation{..}
          = Core.object
              (Core.catMaybes [Core.Just ("OperationId" Core..= operationId)])

instance Core.AWSRequest GetOperation where
        type Rs GetOperation = GetOperationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetOperationResponse' Core.<$>
                   (x Core..:? "Operation") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetOperationResponse' smart constructor.
data GetOperationResponse = GetOperationResponse'
  { operation :: Core.Maybe Types.Operation
    -- ^ A complex type that contains information about the operation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetOperationResponse' value with any optional fields omitted.
mkGetOperationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetOperationResponse
mkGetOperationResponse responseStatus
  = GetOperationResponse'{operation = Core.Nothing, responseStatus}

-- | A complex type that contains information about the operation.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsOperation :: Lens.Lens' GetOperationResponse (Core.Maybe Types.Operation)
gorrsOperation = Lens.field @"operation"
{-# INLINEABLE gorrsOperation #-}
{-# DEPRECATED operation "Use generic-lens or generic-optics with 'operation' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsResponseStatus :: Lens.Lens' GetOperationResponse Core.Int
gorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

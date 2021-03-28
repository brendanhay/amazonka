{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DescribeEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with a specific endpoint. Use this operation to get the status of an endpoint.
module Network.AWS.Comprehend.DescribeEndpoint
    (
    -- * Creating a request
      DescribeEndpoint (..)
    , mkDescribeEndpoint
    -- ** Request lenses
    , defEndpointArn

    -- * Destructuring the response
    , DescribeEndpointResponse (..)
    , mkDescribeEndpointResponse
    -- ** Response lenses
    , derrsEndpointProperties
    , derrsResponseStatus
    ) where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeEndpoint' smart constructor.
newtype DescribeEndpoint = DescribeEndpoint'
  { endpointArn :: Types.ComprehendEndpointArn
    -- ^ The Amazon Resource Number (ARN) of the endpoint being described.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEndpoint' value with any optional fields omitted.
mkDescribeEndpoint
    :: Types.ComprehendEndpointArn -- ^ 'endpointArn'
    -> DescribeEndpoint
mkDescribeEndpoint endpointArn = DescribeEndpoint'{endpointArn}

-- | The Amazon Resource Number (ARN) of the endpoint being described.
--
-- /Note:/ Consider using 'endpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
defEndpointArn :: Lens.Lens' DescribeEndpoint Types.ComprehendEndpointArn
defEndpointArn = Lens.field @"endpointArn"
{-# INLINEABLE defEndpointArn #-}
{-# DEPRECATED endpointArn "Use generic-lens or generic-optics with 'endpointArn' instead"  #-}

instance Core.ToQuery DescribeEndpoint where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeEndpoint where
        toHeaders DescribeEndpoint{..}
          = Core.pure
              ("X-Amz-Target", "Comprehend_20171127.DescribeEndpoint")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeEndpoint where
        toJSON DescribeEndpoint{..}
          = Core.object
              (Core.catMaybes [Core.Just ("EndpointArn" Core..= endpointArn)])

instance Core.AWSRequest DescribeEndpoint where
        type Rs DescribeEndpoint = DescribeEndpointResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeEndpointResponse' Core.<$>
                   (x Core..:? "EndpointProperties") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeEndpointResponse' smart constructor.
data DescribeEndpointResponse = DescribeEndpointResponse'
  { endpointProperties :: Core.Maybe Types.EndpointProperties
    -- ^ Describes information associated with the specific endpoint.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeEndpointResponse' value with any optional fields omitted.
mkDescribeEndpointResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeEndpointResponse
mkDescribeEndpointResponse responseStatus
  = DescribeEndpointResponse'{endpointProperties = Core.Nothing,
                              responseStatus}

-- | Describes information associated with the specific endpoint.
--
-- /Note:/ Consider using 'endpointProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsEndpointProperties :: Lens.Lens' DescribeEndpointResponse (Core.Maybe Types.EndpointProperties)
derrsEndpointProperties = Lens.field @"endpointProperties"
{-# INLINEABLE derrsEndpointProperties #-}
{-# DEPRECATED endpointProperties "Use generic-lens or generic-optics with 'endpointProperties' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsResponseStatus :: Lens.Lens' DescribeEndpointResponse Core.Int
derrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE derrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

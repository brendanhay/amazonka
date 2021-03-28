{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.IsVpcPeered
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a Boolean value indicating whether your Lightsail VPC is peered.
module Network.AWS.Lightsail.IsVpcPeered
    (
    -- * Creating a request
      IsVpcPeered (..)
    , mkIsVpcPeered

    -- * Destructuring the response
    , IsVpcPeeredResponse (..)
    , mkIsVpcPeeredResponse
    -- ** Response lenses
    , ivprrsIsPeered
    , ivprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkIsVpcPeered' smart constructor.
data IsVpcPeered = IsVpcPeered'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IsVpcPeered' value with any optional fields omitted.
mkIsVpcPeered
    :: IsVpcPeered
mkIsVpcPeered = IsVpcPeered'

instance Core.ToQuery IsVpcPeered where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders IsVpcPeered where
        toHeaders IsVpcPeered{..}
          = Core.pure ("X-Amz-Target", "Lightsail_20161128.IsVpcPeered")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON IsVpcPeered where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest IsVpcPeered where
        type Rs IsVpcPeered = IsVpcPeeredResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 IsVpcPeeredResponse' Core.<$>
                   (x Core..:? "isPeered") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkIsVpcPeeredResponse' smart constructor.
data IsVpcPeeredResponse = IsVpcPeeredResponse'
  { isPeered :: Core.Maybe Core.Bool
    -- ^ Returns @true@ if the Lightsail VPC is peered; otherwise, @false@ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IsVpcPeeredResponse' value with any optional fields omitted.
mkIsVpcPeeredResponse
    :: Core.Int -- ^ 'responseStatus'
    -> IsVpcPeeredResponse
mkIsVpcPeeredResponse responseStatus
  = IsVpcPeeredResponse'{isPeered = Core.Nothing, responseStatus}

-- | Returns @true@ if the Lightsail VPC is peered; otherwise, @false@ .
--
-- /Note:/ Consider using 'isPeered' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivprrsIsPeered :: Lens.Lens' IsVpcPeeredResponse (Core.Maybe Core.Bool)
ivprrsIsPeered = Lens.field @"isPeered"
{-# INLINEABLE ivprrsIsPeered #-}
{-# DEPRECATED isPeered "Use generic-lens or generic-optics with 'isPeered' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivprrsResponseStatus :: Lens.Lens' IsVpcPeeredResponse Core.Int
ivprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ivprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

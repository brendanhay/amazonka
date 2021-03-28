{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.DeleteNetworkProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a network profile.
module Network.AWS.DeviceFarm.DeleteNetworkProfile
    (
    -- * Creating a request
      DeleteNetworkProfile (..)
    , mkDeleteNetworkProfile
    -- ** Request lenses
    , dnpArn

    -- * Destructuring the response
    , DeleteNetworkProfileResponse (..)
    , mkDeleteNetworkProfileResponse
    -- ** Response lenses
    , dnprrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteNetworkProfile' smart constructor.
newtype DeleteNetworkProfile = DeleteNetworkProfile'
  { arn :: Types.Arn
    -- ^ The ARN of the network profile to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNetworkProfile' value with any optional fields omitted.
mkDeleteNetworkProfile
    :: Types.Arn -- ^ 'arn'
    -> DeleteNetworkProfile
mkDeleteNetworkProfile arn = DeleteNetworkProfile'{arn}

-- | The ARN of the network profile to delete.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnpArn :: Lens.Lens' DeleteNetworkProfile Types.Arn
dnpArn = Lens.field @"arn"
{-# INLINEABLE dnpArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

instance Core.ToQuery DeleteNetworkProfile where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteNetworkProfile where
        toHeaders DeleteNetworkProfile{..}
          = Core.pure
              ("X-Amz-Target", "DeviceFarm_20150623.DeleteNetworkProfile")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteNetworkProfile where
        toJSON DeleteNetworkProfile{..}
          = Core.object (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.AWSRequest DeleteNetworkProfile where
        type Rs DeleteNetworkProfile = DeleteNetworkProfileResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteNetworkProfileResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteNetworkProfileResponse' smart constructor.
newtype DeleteNetworkProfileResponse = DeleteNetworkProfileResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNetworkProfileResponse' value with any optional fields omitted.
mkDeleteNetworkProfileResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteNetworkProfileResponse
mkDeleteNetworkProfileResponse responseStatus
  = DeleteNetworkProfileResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnprrsResponseStatus :: Lens.Lens' DeleteNetworkProfileResponse Core.Int
dnprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dnprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.GetSnowballUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the Snow Family service limit for your account, and also the number of Snow devices your account has in use.
--
-- The default service limit for the number of Snow devices that you can have at one time is 1. If you want to increase your service limit, contact AWS Support.
module Network.AWS.Snowball.GetSnowballUsage
    (
    -- * Creating a request
      GetSnowballUsage (..)
    , mkGetSnowballUsage

    -- * Destructuring the response
    , GetSnowballUsageResponse (..)
    , mkGetSnowballUsageResponse
    -- ** Response lenses
    , gsurrsSnowballLimit
    , gsurrsSnowballsInUse
    , gsurrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Snowball.Types as Types

-- | /See:/ 'mkGetSnowballUsage' smart constructor.
data GetSnowballUsage = GetSnowballUsage'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSnowballUsage' value with any optional fields omitted.
mkGetSnowballUsage
    :: GetSnowballUsage
mkGetSnowballUsage = GetSnowballUsage'

instance Core.ToQuery GetSnowballUsage where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetSnowballUsage where
        toHeaders GetSnowballUsage{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSIESnowballJobManagementService.GetSnowballUsage")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetSnowballUsage where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest GetSnowballUsage where
        type Rs GetSnowballUsage = GetSnowballUsageResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetSnowballUsageResponse' Core.<$>
                   (x Core..:? "SnowballLimit") Core.<*> x Core..:? "SnowballsInUse"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetSnowballUsageResponse' smart constructor.
data GetSnowballUsageResponse = GetSnowballUsageResponse'
  { snowballLimit :: Core.Maybe Core.Int
    -- ^ The service limit for number of Snow devices this account can have at once. The default service limit is 1 (one).
  , snowballsInUse :: Core.Maybe Core.Int
    -- ^ The number of Snow devices that this account is currently using.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSnowballUsageResponse' value with any optional fields omitted.
mkGetSnowballUsageResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetSnowballUsageResponse
mkGetSnowballUsageResponse responseStatus
  = GetSnowballUsageResponse'{snowballLimit = Core.Nothing,
                              snowballsInUse = Core.Nothing, responseStatus}

-- | The service limit for number of Snow devices this account can have at once. The default service limit is 1 (one).
--
-- /Note:/ Consider using 'snowballLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsurrsSnowballLimit :: Lens.Lens' GetSnowballUsageResponse (Core.Maybe Core.Int)
gsurrsSnowballLimit = Lens.field @"snowballLimit"
{-# INLINEABLE gsurrsSnowballLimit #-}
{-# DEPRECATED snowballLimit "Use generic-lens or generic-optics with 'snowballLimit' instead"  #-}

-- | The number of Snow devices that this account is currently using.
--
-- /Note:/ Consider using 'snowballsInUse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsurrsSnowballsInUse :: Lens.Lens' GetSnowballUsageResponse (Core.Maybe Core.Int)
gsurrsSnowballsInUse = Lens.field @"snowballsInUse"
{-# INLINEABLE gsurrsSnowballsInUse #-}
{-# DEPRECATED snowballsInUse "Use generic-lens or generic-optics with 'snowballsInUse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsurrsResponseStatus :: Lens.Lens' GetSnowballUsageResponse Core.Int
gsurrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gsurrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

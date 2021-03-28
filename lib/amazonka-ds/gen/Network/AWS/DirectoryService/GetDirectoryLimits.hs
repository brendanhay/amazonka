{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.GetDirectoryLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Obtains directory limit information for the current Region.
module Network.AWS.DirectoryService.GetDirectoryLimits
    (
    -- * Creating a request
      GetDirectoryLimits (..)
    , mkGetDirectoryLimits

    -- * Destructuring the response
    , GetDirectoryLimitsResponse (..)
    , mkGetDirectoryLimitsResponse
    -- ** Response lenses
    , gdlrrsDirectoryLimits
    , gdlrrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the 'GetDirectoryLimits' operation.
--
-- /See:/ 'mkGetDirectoryLimits' smart constructor.
data GetDirectoryLimits = GetDirectoryLimits'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDirectoryLimits' value with any optional fields omitted.
mkGetDirectoryLimits
    :: GetDirectoryLimits
mkGetDirectoryLimits = GetDirectoryLimits'

instance Core.ToQuery GetDirectoryLimits where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetDirectoryLimits where
        toHeaders GetDirectoryLimits{..}
          = Core.pure
              ("X-Amz-Target", "DirectoryService_20150416.GetDirectoryLimits")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetDirectoryLimits where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest GetDirectoryLimits where
        type Rs GetDirectoryLimits = GetDirectoryLimitsResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetDirectoryLimitsResponse' Core.<$>
                   (x Core..:? "DirectoryLimits") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the results of the 'GetDirectoryLimits' operation.
--
-- /See:/ 'mkGetDirectoryLimitsResponse' smart constructor.
data GetDirectoryLimitsResponse = GetDirectoryLimitsResponse'
  { directoryLimits :: Core.Maybe Types.DirectoryLimits
    -- ^ A 'DirectoryLimits' object that contains the directory limits for the current rRegion.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDirectoryLimitsResponse' value with any optional fields omitted.
mkGetDirectoryLimitsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetDirectoryLimitsResponse
mkGetDirectoryLimitsResponse responseStatus
  = GetDirectoryLimitsResponse'{directoryLimits = Core.Nothing,
                                responseStatus}

-- | A 'DirectoryLimits' object that contains the directory limits for the current rRegion.
--
-- /Note:/ Consider using 'directoryLimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdlrrsDirectoryLimits :: Lens.Lens' GetDirectoryLimitsResponse (Core.Maybe Types.DirectoryLimits)
gdlrrsDirectoryLimits = Lens.field @"directoryLimits"
{-# INLINEABLE gdlrrsDirectoryLimits #-}
{-# DEPRECATED directoryLimits "Use generic-lens or generic-optics with 'directoryLimits' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdlrrsResponseStatus :: Lens.Lens' GetDirectoryLimitsResponse Core.Int
gdlrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gdlrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

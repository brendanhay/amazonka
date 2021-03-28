{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetSuite
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a suite.
module Network.AWS.DeviceFarm.GetSuite
    (
    -- * Creating a request
      GetSuite (..)
    , mkGetSuite
    -- ** Request lenses
    , gsArn

    -- * Destructuring the response
    , GetSuiteResponse (..)
    , mkGetSuiteResponse
    -- ** Response lenses
    , gsrrsSuite
    , gsrrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the get suite operation.
--
-- /See:/ 'mkGetSuite' smart constructor.
newtype GetSuite = GetSuite'
  { arn :: Types.Arn
    -- ^ The suite's ARN.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetSuite' value with any optional fields omitted.
mkGetSuite
    :: Types.Arn -- ^ 'arn'
    -> GetSuite
mkGetSuite arn = GetSuite'{arn}

-- | The suite's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsArn :: Lens.Lens' GetSuite Types.Arn
gsArn = Lens.field @"arn"
{-# INLINEABLE gsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

instance Core.ToQuery GetSuite where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetSuite where
        toHeaders GetSuite{..}
          = Core.pure ("X-Amz-Target", "DeviceFarm_20150623.GetSuite")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetSuite where
        toJSON GetSuite{..}
          = Core.object (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.AWSRequest GetSuite where
        type Rs GetSuite = GetSuiteResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetSuiteResponse' Core.<$>
                   (x Core..:? "suite") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the result of a get suite request.
--
-- /See:/ 'mkGetSuiteResponse' smart constructor.
data GetSuiteResponse = GetSuiteResponse'
  { suite :: Core.Maybe Types.Suite
    -- ^ A collection of one or more tests.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetSuiteResponse' value with any optional fields omitted.
mkGetSuiteResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetSuiteResponse
mkGetSuiteResponse responseStatus
  = GetSuiteResponse'{suite = Core.Nothing, responseStatus}

-- | A collection of one or more tests.
--
-- /Note:/ Consider using 'suite' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsSuite :: Lens.Lens' GetSuiteResponse (Core.Maybe Types.Suite)
gsrrsSuite = Lens.field @"suite"
{-# INLINEABLE gsrrsSuite #-}
{-# DEPRECATED suite "Use generic-lens or generic-optics with 'suite' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsResponseStatus :: Lens.Lens' GetSuiteResponse Core.Int
gsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

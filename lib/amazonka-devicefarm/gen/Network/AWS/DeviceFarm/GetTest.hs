{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetTest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a test.
module Network.AWS.DeviceFarm.GetTest
    (
    -- * Creating a request
      GetTest (..)
    , mkGetTest
    -- ** Request lenses
    , gtArn

    -- * Destructuring the response
    , GetTestResponse (..)
    , mkGetTestResponse
    -- ** Response lenses
    , gtrrsTest
    , gtrrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the get test operation.
--
-- /See:/ 'mkGetTest' smart constructor.
newtype GetTest = GetTest'
  { arn :: Types.AmazonResourceName
    -- ^ The test's ARN.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetTest' value with any optional fields omitted.
mkGetTest
    :: Types.AmazonResourceName -- ^ 'arn'
    -> GetTest
mkGetTest arn = GetTest'{arn}

-- | The test's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtArn :: Lens.Lens' GetTest Types.AmazonResourceName
gtArn = Lens.field @"arn"
{-# INLINEABLE gtArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

instance Core.ToQuery GetTest where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetTest where
        toHeaders GetTest{..}
          = Core.pure ("X-Amz-Target", "DeviceFarm_20150623.GetTest") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetTest where
        toJSON GetTest{..}
          = Core.object (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.AWSRequest GetTest where
        type Rs GetTest = GetTestResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetTestResponse' Core.<$>
                   (x Core..:? "test") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the result of a get test request.
--
-- /See:/ 'mkGetTestResponse' smart constructor.
data GetTestResponse = GetTestResponse'
  { test :: Core.Maybe Types.Test
    -- ^ A test condition that is evaluated.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetTestResponse' value with any optional fields omitted.
mkGetTestResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetTestResponse
mkGetTestResponse responseStatus
  = GetTestResponse'{test = Core.Nothing, responseStatus}

-- | A test condition that is evaluated.
--
-- /Note:/ Consider using 'test' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsTest :: Lens.Lens' GetTestResponse (Core.Maybe Types.Test)
gtrrsTest = Lens.field @"test"
{-# INLINEABLE gtrrsTest #-}
{-# DEPRECATED test "Use generic-lens or generic-optics with 'test' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsResponseStatus :: Lens.Lens' GetTestResponse Core.Int
gtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.CreateTestGridUrl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a signed, short-term URL that can be passed to a Selenium @RemoteWebDriver@ constructor.
module Network.AWS.DeviceFarm.CreateTestGridUrl
    (
    -- * Creating a request
      CreateTestGridUrl (..)
    , mkCreateTestGridUrl
    -- ** Request lenses
    , ctguProjectArn
    , ctguExpiresInSeconds

    -- * Destructuring the response
    , CreateTestGridUrlResponse (..)
    , mkCreateTestGridUrlResponse
    -- ** Response lenses
    , ctgurrsExpires
    , ctgurrsUrl
    , ctgurrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateTestGridUrl' smart constructor.
data CreateTestGridUrl = CreateTestGridUrl'
  { projectArn :: Types.DeviceFarmArn
    -- ^ ARN (from 'CreateTestGridProject' or 'ListTestGridProjects' ) to associate with the short-term URL. 
  , expiresInSeconds :: Core.Natural
    -- ^ Lifetime, in seconds, of the URL.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTestGridUrl' value with any optional fields omitted.
mkCreateTestGridUrl
    :: Types.DeviceFarmArn -- ^ 'projectArn'
    -> Core.Natural -- ^ 'expiresInSeconds'
    -> CreateTestGridUrl
mkCreateTestGridUrl projectArn expiresInSeconds
  = CreateTestGridUrl'{projectArn, expiresInSeconds}

-- | ARN (from 'CreateTestGridProject' or 'ListTestGridProjects' ) to associate with the short-term URL. 
--
-- /Note:/ Consider using 'projectArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctguProjectArn :: Lens.Lens' CreateTestGridUrl Types.DeviceFarmArn
ctguProjectArn = Lens.field @"projectArn"
{-# INLINEABLE ctguProjectArn #-}
{-# DEPRECATED projectArn "Use generic-lens or generic-optics with 'projectArn' instead"  #-}

-- | Lifetime, in seconds, of the URL.
--
-- /Note:/ Consider using 'expiresInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctguExpiresInSeconds :: Lens.Lens' CreateTestGridUrl Core.Natural
ctguExpiresInSeconds = Lens.field @"expiresInSeconds"
{-# INLINEABLE ctguExpiresInSeconds #-}
{-# DEPRECATED expiresInSeconds "Use generic-lens or generic-optics with 'expiresInSeconds' instead"  #-}

instance Core.ToQuery CreateTestGridUrl where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateTestGridUrl where
        toHeaders CreateTestGridUrl{..}
          = Core.pure
              ("X-Amz-Target", "DeviceFarm_20150623.CreateTestGridUrl")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateTestGridUrl where
        toJSON CreateTestGridUrl{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("projectArn" Core..= projectArn),
                  Core.Just ("expiresInSeconds" Core..= expiresInSeconds)])

instance Core.AWSRequest CreateTestGridUrl where
        type Rs CreateTestGridUrl = CreateTestGridUrlResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateTestGridUrlResponse' Core.<$>
                   (x Core..:? "expires") Core.<*> x Core..:? "url" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateTestGridUrlResponse' smart constructor.
data CreateTestGridUrlResponse = CreateTestGridUrlResponse'
  { expires :: Core.Maybe Core.NominalDiffTime
    -- ^ The number of seconds the URL from 'CreateTestGridUrlResult$url' stays active.
  , url :: Core.Maybe Core.Text
    -- ^ A signed URL, expiring in 'CreateTestGridUrlRequest$expiresInSeconds' seconds, to be passed to a @RemoteWebDriver@ . 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateTestGridUrlResponse' value with any optional fields omitted.
mkCreateTestGridUrlResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateTestGridUrlResponse
mkCreateTestGridUrlResponse responseStatus
  = CreateTestGridUrlResponse'{expires = Core.Nothing,
                               url = Core.Nothing, responseStatus}

-- | The number of seconds the URL from 'CreateTestGridUrlResult$url' stays active.
--
-- /Note:/ Consider using 'expires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgurrsExpires :: Lens.Lens' CreateTestGridUrlResponse (Core.Maybe Core.NominalDiffTime)
ctgurrsExpires = Lens.field @"expires"
{-# INLINEABLE ctgurrsExpires #-}
{-# DEPRECATED expires "Use generic-lens or generic-optics with 'expires' instead"  #-}

-- | A signed URL, expiring in 'CreateTestGridUrlRequest$expiresInSeconds' seconds, to be passed to a @RemoteWebDriver@ . 
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgurrsUrl :: Lens.Lens' CreateTestGridUrlResponse (Core.Maybe Core.Text)
ctgurrsUrl = Lens.field @"url"
{-# INLINEABLE ctgurrsUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgurrsResponseStatus :: Lens.Lens' CreateTestGridUrlResponse Core.Int
ctgurrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ctgurrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

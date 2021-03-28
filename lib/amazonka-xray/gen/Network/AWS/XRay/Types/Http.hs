{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.Http
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.XRay.Types.Http
  ( Http (..)
  -- * Smart constructor
  , mkHttp
  -- * Lenses
  , hClientIp
  , hHttpMethod
  , hHttpStatus
  , hHttpURL
  , hUserAgent
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about an HTTP request.
--
-- /See:/ 'mkHttp' smart constructor.
data Http = Http'
  { clientIp :: Core.Maybe Core.Text
    -- ^ The IP address of the requestor.
  , httpMethod :: Core.Maybe Core.Text
    -- ^ The request method.
  , httpStatus :: Core.Maybe Core.Int
    -- ^ The response status.
  , httpURL :: Core.Maybe Core.Text
    -- ^ The request URL.
  , userAgent :: Core.Maybe Core.Text
    -- ^ The request's user agent string.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Http' value with any optional fields omitted.
mkHttp
    :: Http
mkHttp
  = Http'{clientIp = Core.Nothing, httpMethod = Core.Nothing,
          httpStatus = Core.Nothing, httpURL = Core.Nothing,
          userAgent = Core.Nothing}

-- | The IP address of the requestor.
--
-- /Note:/ Consider using 'clientIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hClientIp :: Lens.Lens' Http (Core.Maybe Core.Text)
hClientIp = Lens.field @"clientIp"
{-# INLINEABLE hClientIp #-}
{-# DEPRECATED clientIp "Use generic-lens or generic-optics with 'clientIp' instead"  #-}

-- | The request method.
--
-- /Note:/ Consider using 'httpMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hHttpMethod :: Lens.Lens' Http (Core.Maybe Core.Text)
hHttpMethod = Lens.field @"httpMethod"
{-# INLINEABLE hHttpMethod #-}
{-# DEPRECATED httpMethod "Use generic-lens or generic-optics with 'httpMethod' instead"  #-}

-- | The response status.
--
-- /Note:/ Consider using 'httpStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hHttpStatus :: Lens.Lens' Http (Core.Maybe Core.Int)
hHttpStatus = Lens.field @"httpStatus"
{-# INLINEABLE hHttpStatus #-}
{-# DEPRECATED httpStatus "Use generic-lens or generic-optics with 'httpStatus' instead"  #-}

-- | The request URL.
--
-- /Note:/ Consider using 'httpURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hHttpURL :: Lens.Lens' Http (Core.Maybe Core.Text)
hHttpURL = Lens.field @"httpURL"
{-# INLINEABLE hHttpURL #-}
{-# DEPRECATED httpURL "Use generic-lens or generic-optics with 'httpURL' instead"  #-}

-- | The request's user agent string.
--
-- /Note:/ Consider using 'userAgent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hUserAgent :: Lens.Lens' Http (Core.Maybe Core.Text)
hUserAgent = Lens.field @"userAgent"
{-# INLINEABLE hUserAgent #-}
{-# DEPRECATED userAgent "Use generic-lens or generic-optics with 'userAgent' instead"  #-}

instance Core.FromJSON Http where
        parseJSON
          = Core.withObject "Http" Core.$
              \ x ->
                Http' Core.<$>
                  (x Core..:? "ClientIp") Core.<*> x Core..:? "HttpMethod" Core.<*>
                    x Core..:? "HttpStatus"
                    Core.<*> x Core..:? "HttpURL"
                    Core.<*> x Core..:? "UserAgent"

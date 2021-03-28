{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.HttpContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.HttpContext
  ( HttpContext (..)
  -- * Smart constructor
  , mkHttpContext
  -- * Lenses
  , hcHeaders
  , hcQueryString
  ) where

import qualified Network.AWS.IoT.Types.HttpHeaderName as Types
import qualified Network.AWS.IoT.Types.HttpHeaderValue as Types
import qualified Network.AWS.IoT.Types.HttpQueryString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the HTTP context to use for the test authorizer request.
--
-- /See:/ 'mkHttpContext' smart constructor.
data HttpContext = HttpContext'
  { headers :: Core.Maybe (Core.HashMap Types.HttpHeaderName Types.HttpHeaderValue)
    -- ^ The header keys and values in an HTTP authorization request.
  , queryString :: Core.Maybe Types.HttpQueryString
    -- ^ The query string keys and values in an HTTP authorization request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HttpContext' value with any optional fields omitted.
mkHttpContext
    :: HttpContext
mkHttpContext
  = HttpContext'{headers = Core.Nothing, queryString = Core.Nothing}

-- | The header keys and values in an HTTP authorization request.
--
-- /Note:/ Consider using 'headers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcHeaders :: Lens.Lens' HttpContext (Core.Maybe (Core.HashMap Types.HttpHeaderName Types.HttpHeaderValue))
hcHeaders = Lens.field @"headers"
{-# INLINEABLE hcHeaders #-}
{-# DEPRECATED headers "Use generic-lens or generic-optics with 'headers' instead"  #-}

-- | The query string keys and values in an HTTP authorization request.
--
-- /Note:/ Consider using 'queryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcQueryString :: Lens.Lens' HttpContext (Core.Maybe Types.HttpQueryString)
hcQueryString = Lens.field @"queryString"
{-# INLINEABLE hcQueryString #-}
{-# DEPRECATED queryString "Use generic-lens or generic-optics with 'queryString' instead"  #-}

instance Core.FromJSON HttpContext where
        toJSON HttpContext{..}
          = Core.object
              (Core.catMaybes
                 [("headers" Core..=) Core.<$> headers,
                  ("queryString" Core..=) Core.<$> queryString])

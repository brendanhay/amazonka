{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.HttpHeader
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.HttpHeader
  ( HttpHeader (..)
  -- * Smart constructor
  , mkHttpHeader
  -- * Lenses
  , hhHeaderName
  , hhHeaderValue
  ) where

import qualified Network.AWS.CognitoIdentityProvider.Types.HeaderName as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.HeaderValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The HTTP header.
--
-- /See:/ 'mkHttpHeader' smart constructor.
data HttpHeader = HttpHeader'
  { headerName :: Core.Maybe Types.HeaderName
    -- ^ The header name
  , headerValue :: Core.Maybe Types.HeaderValue
    -- ^ The header value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HttpHeader' value with any optional fields omitted.
mkHttpHeader
    :: HttpHeader
mkHttpHeader
  = HttpHeader'{headerName = Core.Nothing,
                headerValue = Core.Nothing}

-- | The header name
--
-- /Note:/ Consider using 'headerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hhHeaderName :: Lens.Lens' HttpHeader (Core.Maybe Types.HeaderName)
hhHeaderName = Lens.field @"headerName"
{-# INLINEABLE hhHeaderName #-}
{-# DEPRECATED headerName "Use generic-lens or generic-optics with 'headerName' instead"  #-}

-- | The header value.
--
-- /Note:/ Consider using 'headerValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hhHeaderValue :: Lens.Lens' HttpHeader (Core.Maybe Types.HeaderValue)
hhHeaderValue = Lens.field @"headerValue"
{-# INLINEABLE hhHeaderValue #-}
{-# DEPRECATED headerValue "Use generic-lens or generic-optics with 'headerValue' instead"  #-}

instance Core.FromJSON HttpHeader where
        toJSON HttpHeader{..}
          = Core.object
              (Core.catMaybes
                 [("headerName" Core..=) Core.<$> headerName,
                  ("headerValue" Core..=) Core.<$> headerValue])

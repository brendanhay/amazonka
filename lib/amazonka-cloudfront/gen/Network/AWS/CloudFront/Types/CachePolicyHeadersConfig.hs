{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CachePolicyHeadersConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CachePolicyHeadersConfig
  ( CachePolicyHeadersConfig (..),

    -- * Smart constructor
    mkCachePolicyHeadersConfig,

    -- * Lenses
    cphcHeaderBehavior,
    cphcHeaders,
  )
where

import qualified Network.AWS.CloudFront.Types.CachePolicyHeaderBehavior as Types
import qualified Network.AWS.CloudFront.Types.Headers as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object that determines whether any HTTP headers (and if so, which headers) are included in the cache key and automatically included in requests that CloudFront sends to the origin.
--
-- /See:/ 'mkCachePolicyHeadersConfig' smart constructor.
data CachePolicyHeadersConfig = CachePolicyHeadersConfig'
  { -- | Determines whether any HTTP headers are included in the cache key and automatically included in requests that CloudFront sends to the origin. Valid values are:
    --
    --
    --     * @none@ – HTTP headers are not included in the cache key and are not automatically included in requests that CloudFront sends to the origin. Even when this field is set to @none@ , any headers that are listed in an @OriginRequestPolicy@ /are/ included in origin requests.
    --
    --
    --     * @whitelist@ – The HTTP headers that are listed in the @Headers@ type are included in the cache key and are automatically included in requests that CloudFront sends to the origin.
    headerBehavior :: Types.CachePolicyHeaderBehavior,
    headers :: Core.Maybe Types.Headers
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CachePolicyHeadersConfig' value with any optional fields omitted.
mkCachePolicyHeadersConfig ::
  -- | 'headerBehavior'
  Types.CachePolicyHeaderBehavior ->
  CachePolicyHeadersConfig
mkCachePolicyHeadersConfig headerBehavior =
  CachePolicyHeadersConfig' {headerBehavior, headers = Core.Nothing}

-- | Determines whether any HTTP headers are included in the cache key and automatically included in requests that CloudFront sends to the origin. Valid values are:
--
--
--     * @none@ – HTTP headers are not included in the cache key and are not automatically included in requests that CloudFront sends to the origin. Even when this field is set to @none@ , any headers that are listed in an @OriginRequestPolicy@ /are/ included in origin requests.
--
--
--     * @whitelist@ – The HTTP headers that are listed in the @Headers@ type are included in the cache key and are automatically included in requests that CloudFront sends to the origin.
--
--
--
-- /Note:/ Consider using 'headerBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cphcHeaderBehavior :: Lens.Lens' CachePolicyHeadersConfig Types.CachePolicyHeaderBehavior
cphcHeaderBehavior = Lens.field @"headerBehavior"
{-# DEPRECATED cphcHeaderBehavior "Use generic-lens or generic-optics with 'headerBehavior' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'headers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cphcHeaders :: Lens.Lens' CachePolicyHeadersConfig (Core.Maybe Types.Headers)
cphcHeaders = Lens.field @"headers"
{-# DEPRECATED cphcHeaders "Use generic-lens or generic-optics with 'headers' instead." #-}

instance Core.ToXML CachePolicyHeadersConfig where
  toXML CachePolicyHeadersConfig {..} =
    Core.toXMLNode "HeaderBehavior" headerBehavior
      Core.<> Core.toXMLNode "Headers" Core.<$> headers

instance Core.FromXML CachePolicyHeadersConfig where
  parseXML x =
    CachePolicyHeadersConfig'
      Core.<$> (x Core..@ "HeaderBehavior") Core.<*> (x Core..@? "Headers")

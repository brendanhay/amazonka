{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginRequestPolicyHeadersConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginRequestPolicyHeadersConfig
  ( OriginRequestPolicyHeadersConfig (..),

    -- * Smart constructor
    mkOriginRequestPolicyHeadersConfig,

    -- * Lenses
    orphcHeaderBehavior,
    orphcHeaders,
  )
where

import qualified Network.AWS.CloudFront.Types.Headers as Types
import qualified Network.AWS.CloudFront.Types.OriginRequestPolicyHeaderBehavior as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object that determines whether any HTTP headers (and if so, which headers) are included in requests that CloudFront sends to the origin.
--
-- /See:/ 'mkOriginRequestPolicyHeadersConfig' smart constructor.
data OriginRequestPolicyHeadersConfig = OriginRequestPolicyHeadersConfig'
  { -- | Determines whether any HTTP headers are included in requests that CloudFront sends to the origin. Valid values are:
    --
    --
    --     * @none@ – HTTP headers are not included in requests that CloudFront sends to the origin. Even when this field is set to @none@ , any headers that are listed in a @CachePolicy@ /are/ included in origin requests.
    --
    --
    --     * @whitelist@ – The HTTP headers that are listed in the @Headers@ type are included in requests that CloudFront sends to the origin.
    --
    --
    --     * @allViewer@ – All HTTP headers in viewer requests are included in requests that CloudFront sends to the origin.
    --
    --
    --     * @allViewerAndWhitelistCloudFront@ – All HTTP headers in viewer requests and the additional CloudFront headers that are listed in the @Headers@ type are included in requests that CloudFront sends to the origin. The additional headers are added by CloudFront.
    headerBehavior :: Types.OriginRequestPolicyHeaderBehavior,
    headers :: Core.Maybe Types.Headers
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OriginRequestPolicyHeadersConfig' value with any optional fields omitted.
mkOriginRequestPolicyHeadersConfig ::
  -- | 'headerBehavior'
  Types.OriginRequestPolicyHeaderBehavior ->
  OriginRequestPolicyHeadersConfig
mkOriginRequestPolicyHeadersConfig headerBehavior =
  OriginRequestPolicyHeadersConfig'
    { headerBehavior,
      headers = Core.Nothing
    }

-- | Determines whether any HTTP headers are included in requests that CloudFront sends to the origin. Valid values are:
--
--
--     * @none@ – HTTP headers are not included in requests that CloudFront sends to the origin. Even when this field is set to @none@ , any headers that are listed in a @CachePolicy@ /are/ included in origin requests.
--
--
--     * @whitelist@ – The HTTP headers that are listed in the @Headers@ type are included in requests that CloudFront sends to the origin.
--
--
--     * @allViewer@ – All HTTP headers in viewer requests are included in requests that CloudFront sends to the origin.
--
--
--     * @allViewerAndWhitelistCloudFront@ – All HTTP headers in viewer requests and the additional CloudFront headers that are listed in the @Headers@ type are included in requests that CloudFront sends to the origin. The additional headers are added by CloudFront.
--
--
--
-- /Note:/ Consider using 'headerBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orphcHeaderBehavior :: Lens.Lens' OriginRequestPolicyHeadersConfig Types.OriginRequestPolicyHeaderBehavior
orphcHeaderBehavior = Lens.field @"headerBehavior"
{-# DEPRECATED orphcHeaderBehavior "Use generic-lens or generic-optics with 'headerBehavior' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'headers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orphcHeaders :: Lens.Lens' OriginRequestPolicyHeadersConfig (Core.Maybe Types.Headers)
orphcHeaders = Lens.field @"headers"
{-# DEPRECATED orphcHeaders "Use generic-lens or generic-optics with 'headers' instead." #-}

instance Core.ToXML OriginRequestPolicyHeadersConfig where
  toXML OriginRequestPolicyHeadersConfig {..} =
    Core.toXMLNode "HeaderBehavior" headerBehavior
      Core.<> Core.toXMLNode "Headers" Core.<$> headers

instance Core.FromXML OriginRequestPolicyHeadersConfig where
  parseXML x =
    OriginRequestPolicyHeadersConfig'
      Core.<$> (x Core..@ "HeaderBehavior") Core.<*> (x Core..@? "Headers")

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

import Network.AWS.CloudFront.Types.Headers
import Network.AWS.CloudFront.Types.OriginRequestPolicyHeaderBehavior
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

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
    headerBehavior :: OriginRequestPolicyHeaderBehavior,
    headers :: Lude.Maybe Headers
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OriginRequestPolicyHeadersConfig' with the minimum fields required to make a request.
--
-- * 'headerBehavior' - Determines whether any HTTP headers are included in requests that CloudFront sends to the origin. Valid values are:
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
-- * 'headers' -
mkOriginRequestPolicyHeadersConfig ::
  -- | 'headerBehavior'
  OriginRequestPolicyHeaderBehavior ->
  OriginRequestPolicyHeadersConfig
mkOriginRequestPolicyHeadersConfig pHeaderBehavior_ =
  OriginRequestPolicyHeadersConfig'
    { headerBehavior =
        pHeaderBehavior_,
      headers = Lude.Nothing
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
orphcHeaderBehavior :: Lens.Lens' OriginRequestPolicyHeadersConfig OriginRequestPolicyHeaderBehavior
orphcHeaderBehavior = Lens.lens (headerBehavior :: OriginRequestPolicyHeadersConfig -> OriginRequestPolicyHeaderBehavior) (\s a -> s {headerBehavior = a} :: OriginRequestPolicyHeadersConfig)
{-# DEPRECATED orphcHeaderBehavior "Use generic-lens or generic-optics with 'headerBehavior' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'headers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orphcHeaders :: Lens.Lens' OriginRequestPolicyHeadersConfig (Lude.Maybe Headers)
orphcHeaders = Lens.lens (headers :: OriginRequestPolicyHeadersConfig -> Lude.Maybe Headers) (\s a -> s {headers = a} :: OriginRequestPolicyHeadersConfig)
{-# DEPRECATED orphcHeaders "Use generic-lens or generic-optics with 'headers' instead." #-}

instance Lude.FromXML OriginRequestPolicyHeadersConfig where
  parseXML x =
    OriginRequestPolicyHeadersConfig'
      Lude.<$> (x Lude..@ "HeaderBehavior") Lude.<*> (x Lude..@? "Headers")

instance Lude.ToXML OriginRequestPolicyHeadersConfig where
  toXML OriginRequestPolicyHeadersConfig' {..} =
    Lude.mconcat
      [ "HeaderBehavior" Lude.@= headerBehavior,
        "Headers" Lude.@= headers
      ]

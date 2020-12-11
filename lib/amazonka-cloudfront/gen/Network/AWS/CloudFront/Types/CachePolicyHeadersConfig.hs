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
    cphcHeaders,
    cphcHeaderBehavior,
  )
where

import Network.AWS.CloudFront.Types.CachePolicyHeaderBehavior
import Network.AWS.CloudFront.Types.Headers
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object that determines whether any HTTP headers (and if so, which headers) are included in the cache key and automatically included in requests that CloudFront sends to the origin.
--
-- /See:/ 'mkCachePolicyHeadersConfig' smart constructor.
data CachePolicyHeadersConfig = CachePolicyHeadersConfig'
  { headers ::
      Lude.Maybe Headers,
    headerBehavior ::
      CachePolicyHeaderBehavior
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CachePolicyHeadersConfig' with the minimum fields required to make a request.
--
-- * 'headerBehavior' - Determines whether any HTTP headers are included in the cache key and automatically included in requests that CloudFront sends to the origin. Valid values are:
--
--
--     * @none@ – HTTP headers are not included in the cache key and are not automatically included in requests that CloudFront sends to the origin. Even when this field is set to @none@ , any headers that are listed in an @OriginRequestPolicy@ /are/ included in origin requests.
--
--
--     * @whitelist@ – The HTTP headers that are listed in the @Headers@ type are included in the cache key and are automatically included in requests that CloudFront sends to the origin.
--
--
-- * 'headers' - Undocumented field.
mkCachePolicyHeadersConfig ::
  -- | 'headerBehavior'
  CachePolicyHeaderBehavior ->
  CachePolicyHeadersConfig
mkCachePolicyHeadersConfig pHeaderBehavior_ =
  CachePolicyHeadersConfig'
    { headers = Lude.Nothing,
      headerBehavior = pHeaderBehavior_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'headers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cphcHeaders :: Lens.Lens' CachePolicyHeadersConfig (Lude.Maybe Headers)
cphcHeaders = Lens.lens (headers :: CachePolicyHeadersConfig -> Lude.Maybe Headers) (\s a -> s {headers = a} :: CachePolicyHeadersConfig)
{-# DEPRECATED cphcHeaders "Use generic-lens or generic-optics with 'headers' instead." #-}

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
cphcHeaderBehavior :: Lens.Lens' CachePolicyHeadersConfig CachePolicyHeaderBehavior
cphcHeaderBehavior = Lens.lens (headerBehavior :: CachePolicyHeadersConfig -> CachePolicyHeaderBehavior) (\s a -> s {headerBehavior = a} :: CachePolicyHeadersConfig)
{-# DEPRECATED cphcHeaderBehavior "Use generic-lens or generic-optics with 'headerBehavior' instead." #-}

instance Lude.FromXML CachePolicyHeadersConfig where
  parseXML x =
    CachePolicyHeadersConfig'
      Lude.<$> (x Lude..@? "Headers") Lude.<*> (x Lude..@ "HeaderBehavior")

instance Lude.ToXML CachePolicyHeadersConfig where
  toXML CachePolicyHeadersConfig' {..} =
    Lude.mconcat
      [ "Headers" Lude.@= headers,
        "HeaderBehavior" Lude.@= headerBehavior
      ]

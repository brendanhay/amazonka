{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CachePolicyQueryStringsConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CachePolicyQueryStringsConfig
  ( CachePolicyQueryStringsConfig (..),

    -- * Smart constructor
    mkCachePolicyQueryStringsConfig,

    -- * Lenses
    cpqscQueryStrings,
    cpqscQueryStringBehavior,
  )
where

import Network.AWS.CloudFront.Types.CachePolicyQueryStringBehavior
import Network.AWS.CloudFront.Types.QueryStringNames
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object that determines whether any URL query strings in viewer requests (and if so, which query strings) are included in the cache key and automatically included in requests that CloudFront sends to the origin.
--
-- /See:/ 'mkCachePolicyQueryStringsConfig' smart constructor.
data CachePolicyQueryStringsConfig = CachePolicyQueryStringsConfig'
  { queryStrings ::
      Lude.Maybe QueryStringNames,
    queryStringBehavior ::
      CachePolicyQueryStringBehavior
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CachePolicyQueryStringsConfig' with the minimum fields required to make a request.
--
-- * 'queryStringBehavior' - Determines whether any URL query strings in viewer requests are included in the cache key and automatically included in requests that CloudFront sends to the origin. Valid values are:
--
--
--     * @none@ – Query strings in viewer requests are not included in the cache key and are not automatically included in requests that CloudFront sends to the origin. Even when this field is set to @none@ , any query strings that are listed in an @OriginRequestPolicy@ /are/ included in origin requests.
--
--
--     * @whitelist@ – The query strings in viewer requests that are listed in the @QueryStringNames@ type are included in the cache key and automatically included in requests that CloudFront sends to the origin.
--
--
--     * @allExcept@ – All query strings in viewer requests that are /__not__ / listed in the @QueryStringNames@ type are included in the cache key and automatically included in requests that CloudFront sends to the origin.
--
--
--     * @all@ – All query strings in viewer requests are included in the cache key and are automatically included in requests that CloudFront sends to the origin.
--
--
-- * 'queryStrings' - Contains the specific query strings in viewer requests that either /__are__ / or /__are not__ / included in the cache key and automatically included in requests that CloudFront sends to the origin. The behavior depends on whether the @QueryStringBehavior@ field in the @CachePolicyQueryStringsConfig@ type is set to @whitelist@ (the listed query strings /__are__ / included) or @allExcept@ (the listed query strings /__are not__ / included, but all other query strings are).
mkCachePolicyQueryStringsConfig ::
  -- | 'queryStringBehavior'
  CachePolicyQueryStringBehavior ->
  CachePolicyQueryStringsConfig
mkCachePolicyQueryStringsConfig pQueryStringBehavior_ =
  CachePolicyQueryStringsConfig'
    { queryStrings = Lude.Nothing,
      queryStringBehavior = pQueryStringBehavior_
    }

-- | Contains the specific query strings in viewer requests that either /__are__ / or /__are not__ / included in the cache key and automatically included in requests that CloudFront sends to the origin. The behavior depends on whether the @QueryStringBehavior@ field in the @CachePolicyQueryStringsConfig@ type is set to @whitelist@ (the listed query strings /__are__ / included) or @allExcept@ (the listed query strings /__are not__ / included, but all other query strings are).
--
-- /Note:/ Consider using 'queryStrings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpqscQueryStrings :: Lens.Lens' CachePolicyQueryStringsConfig (Lude.Maybe QueryStringNames)
cpqscQueryStrings = Lens.lens (queryStrings :: CachePolicyQueryStringsConfig -> Lude.Maybe QueryStringNames) (\s a -> s {queryStrings = a} :: CachePolicyQueryStringsConfig)
{-# DEPRECATED cpqscQueryStrings "Use generic-lens or generic-optics with 'queryStrings' instead." #-}

-- | Determines whether any URL query strings in viewer requests are included in the cache key and automatically included in requests that CloudFront sends to the origin. Valid values are:
--
--
--     * @none@ – Query strings in viewer requests are not included in the cache key and are not automatically included in requests that CloudFront sends to the origin. Even when this field is set to @none@ , any query strings that are listed in an @OriginRequestPolicy@ /are/ included in origin requests.
--
--
--     * @whitelist@ – The query strings in viewer requests that are listed in the @QueryStringNames@ type are included in the cache key and automatically included in requests that CloudFront sends to the origin.
--
--
--     * @allExcept@ – All query strings in viewer requests that are /__not__ / listed in the @QueryStringNames@ type are included in the cache key and automatically included in requests that CloudFront sends to the origin.
--
--
--     * @all@ – All query strings in viewer requests are included in the cache key and are automatically included in requests that CloudFront sends to the origin.
--
--
--
-- /Note:/ Consider using 'queryStringBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpqscQueryStringBehavior :: Lens.Lens' CachePolicyQueryStringsConfig CachePolicyQueryStringBehavior
cpqscQueryStringBehavior = Lens.lens (queryStringBehavior :: CachePolicyQueryStringsConfig -> CachePolicyQueryStringBehavior) (\s a -> s {queryStringBehavior = a} :: CachePolicyQueryStringsConfig)
{-# DEPRECATED cpqscQueryStringBehavior "Use generic-lens or generic-optics with 'queryStringBehavior' instead." #-}

instance Lude.FromXML CachePolicyQueryStringsConfig where
  parseXML x =
    CachePolicyQueryStringsConfig'
      Lude.<$> (x Lude..@? "QueryStrings")
      Lude.<*> (x Lude..@ "QueryStringBehavior")

instance Lude.ToXML CachePolicyQueryStringsConfig where
  toXML CachePolicyQueryStringsConfig' {..} =
    Lude.mconcat
      [ "QueryStrings" Lude.@= queryStrings,
        "QueryStringBehavior" Lude.@= queryStringBehavior
      ]

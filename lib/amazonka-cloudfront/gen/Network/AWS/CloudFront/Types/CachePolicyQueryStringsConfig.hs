{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CachePolicyQueryStringsConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.CachePolicyQueryStringsConfig
  ( CachePolicyQueryStringsConfig (..)
  -- * Smart constructor
  , mkCachePolicyQueryStringsConfig
  -- * Lenses
  , cpqscQueryStringBehavior
  , cpqscQueryStrings
  ) where

import qualified Network.AWS.CloudFront.Types.CachePolicyQueryStringBehavior as Types
import qualified Network.AWS.CloudFront.Types.QueryStringNames as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object that determines whether any URL query strings in viewer requests (and if so, which query strings) are included in the cache key and automatically included in requests that CloudFront sends to the origin.
--
-- /See:/ 'mkCachePolicyQueryStringsConfig' smart constructor.
data CachePolicyQueryStringsConfig = CachePolicyQueryStringsConfig'
  { queryStringBehavior :: Types.CachePolicyQueryStringBehavior
    -- ^ Determines whether any URL query strings in viewer requests are included in the cache key and automatically included in requests that CloudFront sends to the origin. Valid values are:
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
  , queryStrings :: Core.Maybe Types.QueryStringNames
    -- ^ Contains the specific query strings in viewer requests that either /__are__ / or /__are not__ / included in the cache key and automatically included in requests that CloudFront sends to the origin. The behavior depends on whether the @QueryStringBehavior@ field in the @CachePolicyQueryStringsConfig@ type is set to @whitelist@ (the listed query strings /__are__ / included) or @allExcept@ (the listed query strings /__are not__ / included, but all other query strings are).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CachePolicyQueryStringsConfig' value with any optional fields omitted.
mkCachePolicyQueryStringsConfig
    :: Types.CachePolicyQueryStringBehavior -- ^ 'queryStringBehavior'
    -> CachePolicyQueryStringsConfig
mkCachePolicyQueryStringsConfig queryStringBehavior
  = CachePolicyQueryStringsConfig'{queryStringBehavior,
                                   queryStrings = Core.Nothing}

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
cpqscQueryStringBehavior :: Lens.Lens' CachePolicyQueryStringsConfig Types.CachePolicyQueryStringBehavior
cpqscQueryStringBehavior = Lens.field @"queryStringBehavior"
{-# INLINEABLE cpqscQueryStringBehavior #-}
{-# DEPRECATED queryStringBehavior "Use generic-lens or generic-optics with 'queryStringBehavior' instead"  #-}

-- | Contains the specific query strings in viewer requests that either /__are__ / or /__are not__ / included in the cache key and automatically included in requests that CloudFront sends to the origin. The behavior depends on whether the @QueryStringBehavior@ field in the @CachePolicyQueryStringsConfig@ type is set to @whitelist@ (the listed query strings /__are__ / included) or @allExcept@ (the listed query strings /__are not__ / included, but all other query strings are).
--
-- /Note:/ Consider using 'queryStrings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpqscQueryStrings :: Lens.Lens' CachePolicyQueryStringsConfig (Core.Maybe Types.QueryStringNames)
cpqscQueryStrings = Lens.field @"queryStrings"
{-# INLINEABLE cpqscQueryStrings #-}
{-# DEPRECATED queryStrings "Use generic-lens or generic-optics with 'queryStrings' instead"  #-}

instance Core.ToXML CachePolicyQueryStringsConfig where
        toXML CachePolicyQueryStringsConfig{..}
          = Core.toXMLElement "QueryStringBehavior" queryStringBehavior
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "QueryStrings")
                queryStrings

instance Core.FromXML CachePolicyQueryStringsConfig where
        parseXML x
          = CachePolicyQueryStringsConfig' Core.<$>
              (x Core..@ "QueryStringBehavior") Core.<*>
                x Core..@? "QueryStrings"

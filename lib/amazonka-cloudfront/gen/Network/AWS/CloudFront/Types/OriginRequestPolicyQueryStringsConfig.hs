{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginRequestPolicyQueryStringsConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginRequestPolicyQueryStringsConfig
  ( OriginRequestPolicyQueryStringsConfig (..),

    -- * Smart constructor
    mkOriginRequestPolicyQueryStringsConfig,

    -- * Lenses
    orpqscQueryStringBehavior,
    orpqscQueryStrings,
  )
where

import qualified Network.AWS.CloudFront.Types.OriginRequestPolicyQueryStringBehavior as Types
import qualified Network.AWS.CloudFront.Types.QueryStringNames as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object that determines whether any URL query strings in viewer requests (and if so, which query strings) are included in requests that CloudFront sends to the origin.
--
-- /See:/ 'mkOriginRequestPolicyQueryStringsConfig' smart constructor.
data OriginRequestPolicyQueryStringsConfig = OriginRequestPolicyQueryStringsConfig'
  { -- | Determines whether any URL query strings in viewer requests are included in requests that CloudFront sends to the origin. Valid values are:
    --
    --
    --     * @none@ – Query strings in viewer requests are not included in requests that CloudFront sends to the origin. Even when this field is set to @none@ , any query strings that are listed in a @CachePolicy@ /are/ included in origin requests.
    --
    --
    --     * @whitelist@ – The query strings in viewer requests that are listed in the @QueryStringNames@ type are included in requests that CloudFront sends to the origin.
    --
    --
    --     * @all@ – All query strings in viewer requests are included in requests that CloudFront sends to the origin.
    queryStringBehavior :: Types.OriginRequestPolicyQueryStringBehavior,
    -- | Contains a list of the query strings in viewer requests that are included in requests that CloudFront sends to the origin.
    queryStrings :: Core.Maybe Types.QueryStringNames
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OriginRequestPolicyQueryStringsConfig' value with any optional fields omitted.
mkOriginRequestPolicyQueryStringsConfig ::
  -- | 'queryStringBehavior'
  Types.OriginRequestPolicyQueryStringBehavior ->
  OriginRequestPolicyQueryStringsConfig
mkOriginRequestPolicyQueryStringsConfig queryStringBehavior =
  OriginRequestPolicyQueryStringsConfig'
    { queryStringBehavior,
      queryStrings = Core.Nothing
    }

-- | Determines whether any URL query strings in viewer requests are included in requests that CloudFront sends to the origin. Valid values are:
--
--
--     * @none@ – Query strings in viewer requests are not included in requests that CloudFront sends to the origin. Even when this field is set to @none@ , any query strings that are listed in a @CachePolicy@ /are/ included in origin requests.
--
--
--     * @whitelist@ – The query strings in viewer requests that are listed in the @QueryStringNames@ type are included in requests that CloudFront sends to the origin.
--
--
--     * @all@ – All query strings in viewer requests are included in requests that CloudFront sends to the origin.
--
--
--
-- /Note:/ Consider using 'queryStringBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orpqscQueryStringBehavior :: Lens.Lens' OriginRequestPolicyQueryStringsConfig Types.OriginRequestPolicyQueryStringBehavior
orpqscQueryStringBehavior = Lens.field @"queryStringBehavior"
{-# DEPRECATED orpqscQueryStringBehavior "Use generic-lens or generic-optics with 'queryStringBehavior' instead." #-}

-- | Contains a list of the query strings in viewer requests that are included in requests that CloudFront sends to the origin.
--
-- /Note:/ Consider using 'queryStrings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orpqscQueryStrings :: Lens.Lens' OriginRequestPolicyQueryStringsConfig (Core.Maybe Types.QueryStringNames)
orpqscQueryStrings = Lens.field @"queryStrings"
{-# DEPRECATED orpqscQueryStrings "Use generic-lens or generic-optics with 'queryStrings' instead." #-}

instance Core.ToXML OriginRequestPolicyQueryStringsConfig where
  toXML OriginRequestPolicyQueryStringsConfig {..} =
    Core.toXMLNode "QueryStringBehavior" queryStringBehavior
      Core.<> Core.toXMLNode "QueryStrings" Core.<$> queryStrings

instance Core.FromXML OriginRequestPolicyQueryStringsConfig where
  parseXML x =
    OriginRequestPolicyQueryStringsConfig'
      Core.<$> (x Core..@ "QueryStringBehavior")
      Core.<*> (x Core..@? "QueryStrings")

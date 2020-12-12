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
    orpqscQueryStrings,
    orpqscQueryStringBehavior,
  )
where

import Network.AWS.CloudFront.Types.OriginRequestPolicyQueryStringBehavior
import Network.AWS.CloudFront.Types.QueryStringNames
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object that determines whether any URL query strings in viewer requests (and if so, which query strings) are included in requests that CloudFront sends to the origin.
--
-- /See:/ 'mkOriginRequestPolicyQueryStringsConfig' smart constructor.
data OriginRequestPolicyQueryStringsConfig = OriginRequestPolicyQueryStringsConfig'
  { queryStrings ::
      Lude.Maybe
        QueryStringNames,
    queryStringBehavior ::
      OriginRequestPolicyQueryStringBehavior
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OriginRequestPolicyQueryStringsConfig' with the minimum fields required to make a request.
--
-- * 'queryStringBehavior' - Determines whether any URL query strings in viewer requests are included in requests that CloudFront sends to the origin. Valid values are:
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
-- * 'queryStrings' - Contains a list of the query strings in viewer requests that are included in requests that CloudFront sends to the origin.
mkOriginRequestPolicyQueryStringsConfig ::
  -- | 'queryStringBehavior'
  OriginRequestPolicyQueryStringBehavior ->
  OriginRequestPolicyQueryStringsConfig
mkOriginRequestPolicyQueryStringsConfig pQueryStringBehavior_ =
  OriginRequestPolicyQueryStringsConfig'
    { queryStrings =
        Lude.Nothing,
      queryStringBehavior = pQueryStringBehavior_
    }

-- | Contains a list of the query strings in viewer requests that are included in requests that CloudFront sends to the origin.
--
-- /Note:/ Consider using 'queryStrings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orpqscQueryStrings :: Lens.Lens' OriginRequestPolicyQueryStringsConfig (Lude.Maybe QueryStringNames)
orpqscQueryStrings = Lens.lens (queryStrings :: OriginRequestPolicyQueryStringsConfig -> Lude.Maybe QueryStringNames) (\s a -> s {queryStrings = a} :: OriginRequestPolicyQueryStringsConfig)
{-# DEPRECATED orpqscQueryStrings "Use generic-lens or generic-optics with 'queryStrings' instead." #-}

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
orpqscQueryStringBehavior :: Lens.Lens' OriginRequestPolicyQueryStringsConfig OriginRequestPolicyQueryStringBehavior
orpqscQueryStringBehavior = Lens.lens (queryStringBehavior :: OriginRequestPolicyQueryStringsConfig -> OriginRequestPolicyQueryStringBehavior) (\s a -> s {queryStringBehavior = a} :: OriginRequestPolicyQueryStringsConfig)
{-# DEPRECATED orpqscQueryStringBehavior "Use generic-lens or generic-optics with 'queryStringBehavior' instead." #-}

instance Lude.FromXML OriginRequestPolicyQueryStringsConfig where
  parseXML x =
    OriginRequestPolicyQueryStringsConfig'
      Lude.<$> (x Lude..@? "QueryStrings")
      Lude.<*> (x Lude..@ "QueryStringBehavior")

instance Lude.ToXML OriginRequestPolicyQueryStringsConfig where
  toXML OriginRequestPolicyQueryStringsConfig' {..} =
    Lude.mconcat
      [ "QueryStrings" Lude.@= queryStrings,
        "QueryStringBehavior" Lude.@= queryStringBehavior
      ]

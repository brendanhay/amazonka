{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CachePolicyQueryStringsConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CachePolicyQueryStringsConfig where

import Network.AWS.CloudFront.Types.CachePolicyQueryStringBehavior
import Network.AWS.CloudFront.Types.QueryStringNames
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object that determines whether any URL query strings in viewer requests (and if so, which query strings) are included in the cache key and automatically included in requests that CloudFront sends to the origin.
--
--
--
-- /See:/ 'cachePolicyQueryStringsConfig' smart constructor.
data CachePolicyQueryStringsConfig = CachePolicyQueryStringsConfig'
  { _cpqscQueryStrings ::
      !(Maybe QueryStringNames),
    _cpqscQueryStringBehavior ::
      !CachePolicyQueryStringBehavior
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CachePolicyQueryStringsConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpqscQueryStrings' - Contains the specific query strings in viewer requests that either /__are__ / or /__are not__ / included in the cache key and automatically included in requests that CloudFront sends to the origin. The behavior depends on whether the @QueryStringBehavior@ field in the @CachePolicyQueryStringsConfig@ type is set to @whitelist@ (the listed query strings /__are__ / included) or @allExcept@ (the listed query strings /__are not__ / included, but all other query strings are).
--
-- * 'cpqscQueryStringBehavior' - Determines whether any URL query strings in viewer requests are included in the cache key and automatically included in requests that CloudFront sends to the origin. Valid values are:     * @none@ – Query strings in viewer requests are not included in the cache key and are not automatically included in requests that CloudFront sends to the origin. Even when this field is set to @none@ , any query strings that are listed in an @OriginRequestPolicy@ /are/ included in origin requests.     * @whitelist@ – The query strings in viewer requests that are listed in the @QueryStringNames@ type are included in the cache key and automatically included in requests that CloudFront sends to the origin.     * @allExcept@ – All query strings in viewer requests that are /__not__ / listed in the @QueryStringNames@ type are included in the cache key and automatically included in requests that CloudFront sends to the origin.     * @all@ – All query strings in viewer requests are included in the cache key and are automatically included in requests that CloudFront sends to the origin.
cachePolicyQueryStringsConfig ::
  -- | 'cpqscQueryStringBehavior'
  CachePolicyQueryStringBehavior ->
  CachePolicyQueryStringsConfig
cachePolicyQueryStringsConfig pQueryStringBehavior_ =
  CachePolicyQueryStringsConfig'
    { _cpqscQueryStrings = Nothing,
      _cpqscQueryStringBehavior = pQueryStringBehavior_
    }

-- | Contains the specific query strings in viewer requests that either /__are__ / or /__are not__ / included in the cache key and automatically included in requests that CloudFront sends to the origin. The behavior depends on whether the @QueryStringBehavior@ field in the @CachePolicyQueryStringsConfig@ type is set to @whitelist@ (the listed query strings /__are__ / included) or @allExcept@ (the listed query strings /__are not__ / included, but all other query strings are).
cpqscQueryStrings :: Lens' CachePolicyQueryStringsConfig (Maybe QueryStringNames)
cpqscQueryStrings = lens _cpqscQueryStrings (\s a -> s {_cpqscQueryStrings = a})

-- | Determines whether any URL query strings in viewer requests are included in the cache key and automatically included in requests that CloudFront sends to the origin. Valid values are:     * @none@ – Query strings in viewer requests are not included in the cache key and are not automatically included in requests that CloudFront sends to the origin. Even when this field is set to @none@ , any query strings that are listed in an @OriginRequestPolicy@ /are/ included in origin requests.     * @whitelist@ – The query strings in viewer requests that are listed in the @QueryStringNames@ type are included in the cache key and automatically included in requests that CloudFront sends to the origin.     * @allExcept@ – All query strings in viewer requests that are /__not__ / listed in the @QueryStringNames@ type are included in the cache key and automatically included in requests that CloudFront sends to the origin.     * @all@ – All query strings in viewer requests are included in the cache key and are automatically included in requests that CloudFront sends to the origin.
cpqscQueryStringBehavior :: Lens' CachePolicyQueryStringsConfig CachePolicyQueryStringBehavior
cpqscQueryStringBehavior = lens _cpqscQueryStringBehavior (\s a -> s {_cpqscQueryStringBehavior = a})

instance FromXML CachePolicyQueryStringsConfig where
  parseXML x =
    CachePolicyQueryStringsConfig'
      <$> (x .@? "QueryStrings") <*> (x .@ "QueryStringBehavior")

instance Hashable CachePolicyQueryStringsConfig

instance NFData CachePolicyQueryStringsConfig

instance ToXML CachePolicyQueryStringsConfig where
  toXML CachePolicyQueryStringsConfig' {..} =
    mconcat
      [ "QueryStrings" @= _cpqscQueryStrings,
        "QueryStringBehavior" @= _cpqscQueryStringBehavior
      ]

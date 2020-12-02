{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CachePolicyCookiesConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CachePolicyCookiesConfig where

import Network.AWS.CloudFront.Types.CachePolicyCookieBehavior
import Network.AWS.CloudFront.Types.CookieNames
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object that determines whether any cookies in viewer requests (and if so, which cookies) are included in the cache key and automatically included in requests that CloudFront sends to the origin.
--
--
--
-- /See:/ 'cachePolicyCookiesConfig' smart constructor.
data CachePolicyCookiesConfig = CachePolicyCookiesConfig'
  { _cpccCookies ::
      !(Maybe CookieNames),
    _cpccCookieBehavior ::
      !CachePolicyCookieBehavior
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CachePolicyCookiesConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpccCookies' - Undocumented member.
--
-- * 'cpccCookieBehavior' - Determines whether any cookies in viewer requests are included in the cache key and automatically included in requests that CloudFront sends to the origin. Valid values are:     * @none@ – Cookies in viewer requests are not included in the cache key and are not automatically included in requests that CloudFront sends to the origin. Even when this field is set to @none@ , any cookies that are listed in an @OriginRequestPolicy@ /are/ included in origin requests.     * @whitelist@ – The cookies in viewer requests that are listed in the @CookieNames@ type are included in the cache key and automatically included in requests that CloudFront sends to the origin.     * @allExcept@ – All cookies in viewer requests that are /__not__ / listed in the @CookieNames@ type are included in the cache key and automatically included in requests that CloudFront sends to the origin.     * @all@ – All cookies in viewer requests are included in the cache key and are automatically included in requests that CloudFront sends to the origin.
cachePolicyCookiesConfig ::
  -- | 'cpccCookieBehavior'
  CachePolicyCookieBehavior ->
  CachePolicyCookiesConfig
cachePolicyCookiesConfig pCookieBehavior_ =
  CachePolicyCookiesConfig'
    { _cpccCookies = Nothing,
      _cpccCookieBehavior = pCookieBehavior_
    }

-- | Undocumented member.
cpccCookies :: Lens' CachePolicyCookiesConfig (Maybe CookieNames)
cpccCookies = lens _cpccCookies (\s a -> s {_cpccCookies = a})

-- | Determines whether any cookies in viewer requests are included in the cache key and automatically included in requests that CloudFront sends to the origin. Valid values are:     * @none@ – Cookies in viewer requests are not included in the cache key and are not automatically included in requests that CloudFront sends to the origin. Even when this field is set to @none@ , any cookies that are listed in an @OriginRequestPolicy@ /are/ included in origin requests.     * @whitelist@ – The cookies in viewer requests that are listed in the @CookieNames@ type are included in the cache key and automatically included in requests that CloudFront sends to the origin.     * @allExcept@ – All cookies in viewer requests that are /__not__ / listed in the @CookieNames@ type are included in the cache key and automatically included in requests that CloudFront sends to the origin.     * @all@ – All cookies in viewer requests are included in the cache key and are automatically included in requests that CloudFront sends to the origin.
cpccCookieBehavior :: Lens' CachePolicyCookiesConfig CachePolicyCookieBehavior
cpccCookieBehavior = lens _cpccCookieBehavior (\s a -> s {_cpccCookieBehavior = a})

instance FromXML CachePolicyCookiesConfig where
  parseXML x =
    CachePolicyCookiesConfig'
      <$> (x .@? "Cookies") <*> (x .@ "CookieBehavior")

instance Hashable CachePolicyCookiesConfig

instance NFData CachePolicyCookiesConfig

instance ToXML CachePolicyCookiesConfig where
  toXML CachePolicyCookiesConfig' {..} =
    mconcat
      [ "Cookies" @= _cpccCookies,
        "CookieBehavior" @= _cpccCookieBehavior
      ]

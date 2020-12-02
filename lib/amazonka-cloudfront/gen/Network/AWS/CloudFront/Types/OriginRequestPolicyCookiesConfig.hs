{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginRequestPolicyCookiesConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginRequestPolicyCookiesConfig where

import Network.AWS.CloudFront.Types.CookieNames
import Network.AWS.CloudFront.Types.OriginRequestPolicyCookieBehavior
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object that determines whether any cookies in viewer requests (and if so, which cookies) are included in requests that CloudFront sends to the origin.
--
--
--
-- /See:/ 'originRequestPolicyCookiesConfig' smart constructor.
data OriginRequestPolicyCookiesConfig = OriginRequestPolicyCookiesConfig'
  { _orpccCookies ::
      !(Maybe CookieNames),
    _orpccCookieBehavior ::
      !OriginRequestPolicyCookieBehavior
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OriginRequestPolicyCookiesConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'orpccCookies' - Undocumented member.
--
-- * 'orpccCookieBehavior' - Determines whether cookies in viewer requests are included in requests that CloudFront sends to the origin. Valid values are:     * @none@ – Cookies in viewer requests are not included in requests that CloudFront sends to the origin. Even when this field is set to @none@ , any cookies that are listed in a @CachePolicy@ /are/ included in origin requests.     * @whitelist@ – The cookies in viewer requests that are listed in the @CookieNames@ type are included in requests that CloudFront sends to the origin.     * @all@ – All cookies in viewer requests are included in requests that CloudFront sends to the origin.
originRequestPolicyCookiesConfig ::
  -- | 'orpccCookieBehavior'
  OriginRequestPolicyCookieBehavior ->
  OriginRequestPolicyCookiesConfig
originRequestPolicyCookiesConfig pCookieBehavior_ =
  OriginRequestPolicyCookiesConfig'
    { _orpccCookies = Nothing,
      _orpccCookieBehavior = pCookieBehavior_
    }

-- | Undocumented member.
orpccCookies :: Lens' OriginRequestPolicyCookiesConfig (Maybe CookieNames)
orpccCookies = lens _orpccCookies (\s a -> s {_orpccCookies = a})

-- | Determines whether cookies in viewer requests are included in requests that CloudFront sends to the origin. Valid values are:     * @none@ – Cookies in viewer requests are not included in requests that CloudFront sends to the origin. Even when this field is set to @none@ , any cookies that are listed in a @CachePolicy@ /are/ included in origin requests.     * @whitelist@ – The cookies in viewer requests that are listed in the @CookieNames@ type are included in requests that CloudFront sends to the origin.     * @all@ – All cookies in viewer requests are included in requests that CloudFront sends to the origin.
orpccCookieBehavior :: Lens' OriginRequestPolicyCookiesConfig OriginRequestPolicyCookieBehavior
orpccCookieBehavior = lens _orpccCookieBehavior (\s a -> s {_orpccCookieBehavior = a})

instance FromXML OriginRequestPolicyCookiesConfig where
  parseXML x =
    OriginRequestPolicyCookiesConfig'
      <$> (x .@? "Cookies") <*> (x .@ "CookieBehavior")

instance Hashable OriginRequestPolicyCookiesConfig

instance NFData OriginRequestPolicyCookiesConfig

instance ToXML OriginRequestPolicyCookiesConfig where
  toXML OriginRequestPolicyCookiesConfig' {..} =
    mconcat
      [ "Cookies" @= _orpccCookies,
        "CookieBehavior" @= _orpccCookieBehavior
      ]

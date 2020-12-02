{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginRequestPolicyConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginRequestPolicyConfig where

import Network.AWS.CloudFront.Types.OriginRequestPolicyCookiesConfig
import Network.AWS.CloudFront.Types.OriginRequestPolicyHeadersConfig
import Network.AWS.CloudFront.Types.OriginRequestPolicyQueryStringsConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An origin request policy configuration.
--
--
-- This configuration determines the values that CloudFront includes in requests that it sends to the origin. Each request that CloudFront sends to the origin includes the following:
--
--     * The request body and the URL path (without the domain name) from the viewer request.
--
--     * The headers that CloudFront automatically includes in every origin request, including @Host@ , @User-Agent@ , and @X-Amz-Cf-Id@ .
--
--     * All HTTP headers, cookies, and URL query strings that are specified in the cache policy or the origin request policy. These can include items from the viewer request and, in the case of headers, additional ones that are added by CloudFront.
--
--
--
-- CloudFront sends a request when it can’t find an object in its cache that matches the request. If you want to send values to the origin and also include them in the cache key, use @CachePolicy@ .
--
--
-- /See:/ 'originRequestPolicyConfig' smart constructor.
data OriginRequestPolicyConfig = OriginRequestPolicyConfig'
  { _orpcComment ::
      !(Maybe Text),
    _orpcName :: !Text,
    _orpcHeadersConfig ::
      !OriginRequestPolicyHeadersConfig,
    _orpcCookiesConfig ::
      !OriginRequestPolicyCookiesConfig,
    _orpcQueryStringsConfig ::
      !OriginRequestPolicyQueryStringsConfig
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OriginRequestPolicyConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'orpcComment' - A comment to describe the origin request policy.
--
-- * 'orpcName' - A unique name to identify the origin request policy.
--
-- * 'orpcHeadersConfig' - The HTTP headers to include in origin requests. These can include headers from viewer requests and additional headers added by CloudFront.
--
-- * 'orpcCookiesConfig' - The cookies from viewer requests to include in origin requests.
--
-- * 'orpcQueryStringsConfig' - The URL query strings from viewer requests to include in origin requests.
originRequestPolicyConfig ::
  -- | 'orpcName'
  Text ->
  -- | 'orpcHeadersConfig'
  OriginRequestPolicyHeadersConfig ->
  -- | 'orpcCookiesConfig'
  OriginRequestPolicyCookiesConfig ->
  -- | 'orpcQueryStringsConfig'
  OriginRequestPolicyQueryStringsConfig ->
  OriginRequestPolicyConfig
originRequestPolicyConfig
  pName_
  pHeadersConfig_
  pCookiesConfig_
  pQueryStringsConfig_ =
    OriginRequestPolicyConfig'
      { _orpcComment = Nothing,
        _orpcName = pName_,
        _orpcHeadersConfig = pHeadersConfig_,
        _orpcCookiesConfig = pCookiesConfig_,
        _orpcQueryStringsConfig = pQueryStringsConfig_
      }

-- | A comment to describe the origin request policy.
orpcComment :: Lens' OriginRequestPolicyConfig (Maybe Text)
orpcComment = lens _orpcComment (\s a -> s {_orpcComment = a})

-- | A unique name to identify the origin request policy.
orpcName :: Lens' OriginRequestPolicyConfig Text
orpcName = lens _orpcName (\s a -> s {_orpcName = a})

-- | The HTTP headers to include in origin requests. These can include headers from viewer requests and additional headers added by CloudFront.
orpcHeadersConfig :: Lens' OriginRequestPolicyConfig OriginRequestPolicyHeadersConfig
orpcHeadersConfig = lens _orpcHeadersConfig (\s a -> s {_orpcHeadersConfig = a})

-- | The cookies from viewer requests to include in origin requests.
orpcCookiesConfig :: Lens' OriginRequestPolicyConfig OriginRequestPolicyCookiesConfig
orpcCookiesConfig = lens _orpcCookiesConfig (\s a -> s {_orpcCookiesConfig = a})

-- | The URL query strings from viewer requests to include in origin requests.
orpcQueryStringsConfig :: Lens' OriginRequestPolicyConfig OriginRequestPolicyQueryStringsConfig
orpcQueryStringsConfig = lens _orpcQueryStringsConfig (\s a -> s {_orpcQueryStringsConfig = a})

instance FromXML OriginRequestPolicyConfig where
  parseXML x =
    OriginRequestPolicyConfig'
      <$> (x .@? "Comment")
      <*> (x .@ "Name")
      <*> (x .@ "HeadersConfig")
      <*> (x .@ "CookiesConfig")
      <*> (x .@ "QueryStringsConfig")

instance Hashable OriginRequestPolicyConfig

instance NFData OriginRequestPolicyConfig

instance ToXML OriginRequestPolicyConfig where
  toXML OriginRequestPolicyConfig' {..} =
    mconcat
      [ "Comment" @= _orpcComment,
        "Name" @= _orpcName,
        "HeadersConfig" @= _orpcHeadersConfig,
        "CookiesConfig" @= _orpcCookiesConfig,
        "QueryStringsConfig" @= _orpcQueryStringsConfig
      ]

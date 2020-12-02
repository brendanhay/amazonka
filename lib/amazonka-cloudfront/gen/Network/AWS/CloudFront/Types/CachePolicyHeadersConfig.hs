{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CachePolicyHeadersConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CachePolicyHeadersConfig where

import Network.AWS.CloudFront.Types.CachePolicyHeaderBehavior
import Network.AWS.CloudFront.Types.Headers
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object that determines whether any HTTP headers (and if so, which headers) are included in the cache key and automatically included in requests that CloudFront sends to the origin.
--
--
--
-- /See:/ 'cachePolicyHeadersConfig' smart constructor.
data CachePolicyHeadersConfig = CachePolicyHeadersConfig'
  { _cphcHeaders ::
      !(Maybe Headers),
    _cphcHeaderBehavior ::
      !CachePolicyHeaderBehavior
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CachePolicyHeadersConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cphcHeaders' - Undocumented member.
--
-- * 'cphcHeaderBehavior' - Determines whether any HTTP headers are included in the cache key and automatically included in requests that CloudFront sends to the origin. Valid values are:     * @none@ – HTTP headers are not included in the cache key and are not automatically included in requests that CloudFront sends to the origin. Even when this field is set to @none@ , any headers that are listed in an @OriginRequestPolicy@ /are/ included in origin requests.     * @whitelist@ – The HTTP headers that are listed in the @Headers@ type are included in the cache key and are automatically included in requests that CloudFront sends to the origin.
cachePolicyHeadersConfig ::
  -- | 'cphcHeaderBehavior'
  CachePolicyHeaderBehavior ->
  CachePolicyHeadersConfig
cachePolicyHeadersConfig pHeaderBehavior_ =
  CachePolicyHeadersConfig'
    { _cphcHeaders = Nothing,
      _cphcHeaderBehavior = pHeaderBehavior_
    }

-- | Undocumented member.
cphcHeaders :: Lens' CachePolicyHeadersConfig (Maybe Headers)
cphcHeaders = lens _cphcHeaders (\s a -> s {_cphcHeaders = a})

-- | Determines whether any HTTP headers are included in the cache key and automatically included in requests that CloudFront sends to the origin. Valid values are:     * @none@ – HTTP headers are not included in the cache key and are not automatically included in requests that CloudFront sends to the origin. Even when this field is set to @none@ , any headers that are listed in an @OriginRequestPolicy@ /are/ included in origin requests.     * @whitelist@ – The HTTP headers that are listed in the @Headers@ type are included in the cache key and are automatically included in requests that CloudFront sends to the origin.
cphcHeaderBehavior :: Lens' CachePolicyHeadersConfig CachePolicyHeaderBehavior
cphcHeaderBehavior = lens _cphcHeaderBehavior (\s a -> s {_cphcHeaderBehavior = a})

instance FromXML CachePolicyHeadersConfig where
  parseXML x =
    CachePolicyHeadersConfig'
      <$> (x .@? "Headers") <*> (x .@ "HeaderBehavior")

instance Hashable CachePolicyHeadersConfig

instance NFData CachePolicyHeadersConfig

instance ToXML CachePolicyHeadersConfig where
  toXML CachePolicyHeadersConfig' {..} =
    mconcat
      [ "Headers" @= _cphcHeaders,
        "HeaderBehavior" @= _cphcHeaderBehavior
      ]

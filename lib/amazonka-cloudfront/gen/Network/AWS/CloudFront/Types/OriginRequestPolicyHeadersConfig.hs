{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginRequestPolicyHeadersConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginRequestPolicyHeadersConfig where

import Network.AWS.CloudFront.Types.Headers
import Network.AWS.CloudFront.Types.OriginRequestPolicyHeaderBehavior
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object that determines whether any HTTP headers (and if so, which headers) are included in requests that CloudFront sends to the origin.
--
--
--
-- /See:/ 'originRequestPolicyHeadersConfig' smart constructor.
data OriginRequestPolicyHeadersConfig = OriginRequestPolicyHeadersConfig'
  { _orphcHeaders ::
      !(Maybe Headers),
    _orphcHeaderBehavior ::
      !OriginRequestPolicyHeaderBehavior
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OriginRequestPolicyHeadersConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'orphcHeaders' - Undocumented member.
--
-- * 'orphcHeaderBehavior' - Determines whether any HTTP headers are included in requests that CloudFront sends to the origin. Valid values are:     * @none@ – HTTP headers are not included in requests that CloudFront sends to the origin. Even when this field is set to @none@ , any headers that are listed in a @CachePolicy@ /are/ included in origin requests.     * @whitelist@ – The HTTP headers that are listed in the @Headers@ type are included in requests that CloudFront sends to the origin.     * @allViewer@ – All HTTP headers in viewer requests are included in requests that CloudFront sends to the origin.     * @allViewerAndWhitelistCloudFront@ – All HTTP headers in viewer requests and the additional CloudFront headers that are listed in the @Headers@ type are included in requests that CloudFront sends to the origin. The additional headers are added by CloudFront.
originRequestPolicyHeadersConfig ::
  -- | 'orphcHeaderBehavior'
  OriginRequestPolicyHeaderBehavior ->
  OriginRequestPolicyHeadersConfig
originRequestPolicyHeadersConfig pHeaderBehavior_ =
  OriginRequestPolicyHeadersConfig'
    { _orphcHeaders = Nothing,
      _orphcHeaderBehavior = pHeaderBehavior_
    }

-- | Undocumented member.
orphcHeaders :: Lens' OriginRequestPolicyHeadersConfig (Maybe Headers)
orphcHeaders = lens _orphcHeaders (\s a -> s {_orphcHeaders = a})

-- | Determines whether any HTTP headers are included in requests that CloudFront sends to the origin. Valid values are:     * @none@ – HTTP headers are not included in requests that CloudFront sends to the origin. Even when this field is set to @none@ , any headers that are listed in a @CachePolicy@ /are/ included in origin requests.     * @whitelist@ – The HTTP headers that are listed in the @Headers@ type are included in requests that CloudFront sends to the origin.     * @allViewer@ – All HTTP headers in viewer requests are included in requests that CloudFront sends to the origin.     * @allViewerAndWhitelistCloudFront@ – All HTTP headers in viewer requests and the additional CloudFront headers that are listed in the @Headers@ type are included in requests that CloudFront sends to the origin. The additional headers are added by CloudFront.
orphcHeaderBehavior :: Lens' OriginRequestPolicyHeadersConfig OriginRequestPolicyHeaderBehavior
orphcHeaderBehavior = lens _orphcHeaderBehavior (\s a -> s {_orphcHeaderBehavior = a})

instance FromXML OriginRequestPolicyHeadersConfig where
  parseXML x =
    OriginRequestPolicyHeadersConfig'
      <$> (x .@? "Headers") <*> (x .@ "HeaderBehavior")

instance Hashable OriginRequestPolicyHeadersConfig

instance NFData OriginRequestPolicyHeadersConfig

instance ToXML OriginRequestPolicyHeadersConfig where
  toXML OriginRequestPolicyHeadersConfig' {..} =
    mconcat
      [ "Headers" @= _orphcHeaders,
        "HeaderBehavior" @= _orphcHeaderBehavior
      ]

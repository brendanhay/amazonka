{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.HTTP
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.HTTP where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an HTTP request.
--
--
--
-- /See:/ 'hTTP' smart constructor.
data HTTP = HTTP'
  { _httpHTTPMethod :: !(Maybe Text),
    _httpHTTPStatus :: !(Maybe Int),
    _httpClientIP :: !(Maybe Text),
    _httpUserAgent :: !(Maybe Text),
    _httpHTTPURL :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HTTP' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'httpHTTPMethod' - The request method.
--
-- * 'httpHTTPStatus' - The response status.
--
-- * 'httpClientIP' - The IP address of the requestor.
--
-- * 'httpUserAgent' - The request's user agent string.
--
-- * 'httpHTTPURL' - The request URL.
hTTP ::
  HTTP
hTTP =
  HTTP'
    { _httpHTTPMethod = Nothing,
      _httpHTTPStatus = Nothing,
      _httpClientIP = Nothing,
      _httpUserAgent = Nothing,
      _httpHTTPURL = Nothing
    }

-- | The request method.
httpHTTPMethod :: Lens' HTTP (Maybe Text)
httpHTTPMethod = lens _httpHTTPMethod (\s a -> s {_httpHTTPMethod = a})

-- | The response status.
httpHTTPStatus :: Lens' HTTP (Maybe Int)
httpHTTPStatus = lens _httpHTTPStatus (\s a -> s {_httpHTTPStatus = a})

-- | The IP address of the requestor.
httpClientIP :: Lens' HTTP (Maybe Text)
httpClientIP = lens _httpClientIP (\s a -> s {_httpClientIP = a})

-- | The request's user agent string.
httpUserAgent :: Lens' HTTP (Maybe Text)
httpUserAgent = lens _httpUserAgent (\s a -> s {_httpUserAgent = a})

-- | The request URL.
httpHTTPURL :: Lens' HTTP (Maybe Text)
httpHTTPURL = lens _httpHTTPURL (\s a -> s {_httpHTTPURL = a})

instance FromJSON HTTP where
  parseJSON =
    withObject
      "HTTP"
      ( \x ->
          HTTP'
            <$> (x .:? "HttpMethod")
            <*> (x .:? "HttpStatus")
            <*> (x .:? "ClientIp")
            <*> (x .:? "UserAgent")
            <*> (x .:? "HttpURL")
      )

instance Hashable HTTP

instance NFData HTTP

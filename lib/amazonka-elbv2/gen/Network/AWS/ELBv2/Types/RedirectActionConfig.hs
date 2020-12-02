{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.RedirectActionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.RedirectActionConfig where

import Network.AWS.ELBv2.Types.RedirectActionStatusCodeEnum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a redirect action.
--
--
-- A URI consists of the following components: protocol://hostname:port/path?query. You must modify at least one of the following components to avoid a redirect loop: protocol, hostname, port, or path. Any components that you do not modify retain their original values.
--
-- You can reuse URI components using the following reserved keywords:
--
--     * #{protocol}
--
--     * #{host}
--
--     * #{port}
--
--     * #{path} (the leading "/" is removed)
--
--     * #{query}
--
--
--
-- For example, you can change the path to "/new/#{path}", the hostname to "example.#{host}", or the query to "#{query}&value=xyz".
--
--
-- /See:/ 'redirectActionConfig' smart constructor.
data RedirectActionConfig = RedirectActionConfig'
  { _racPath ::
      !(Maybe Text),
    _racProtocol :: !(Maybe Text),
    _racQuery :: !(Maybe Text),
    _racHost :: !(Maybe Text),
    _racPort :: !(Maybe Text),
    _racStatusCode :: !RedirectActionStatusCodeEnum
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RedirectActionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'racPath' - The absolute path, starting with the leading "/". This component is not percent-encoded. The path can contain #{host}, #{path}, and #{port}.
--
-- * 'racProtocol' - The protocol. You can specify HTTP, HTTPS, or #{protocol}. You can redirect HTTP to HTTP, HTTP to HTTPS, and HTTPS to HTTPS. You cannot redirect HTTPS to HTTP.
--
-- * 'racQuery' - The query parameters, URL-encoded when necessary, but not percent-encoded. Do not include the leading "?", as it is automatically added. You can specify any of the reserved keywords.
--
-- * 'racHost' - The hostname. This component is not percent-encoded. The hostname can contain #{host}.
--
-- * 'racPort' - The port. You can specify a value from 1 to 65535 or #{port}.
--
-- * 'racStatusCode' - The HTTP redirect code. The redirect is either permanent (HTTP 301) or temporary (HTTP 302).
redirectActionConfig ::
  -- | 'racStatusCode'
  RedirectActionStatusCodeEnum ->
  RedirectActionConfig
redirectActionConfig pStatusCode_ =
  RedirectActionConfig'
    { _racPath = Nothing,
      _racProtocol = Nothing,
      _racQuery = Nothing,
      _racHost = Nothing,
      _racPort = Nothing,
      _racStatusCode = pStatusCode_
    }

-- | The absolute path, starting with the leading "/". This component is not percent-encoded. The path can contain #{host}, #{path}, and #{port}.
racPath :: Lens' RedirectActionConfig (Maybe Text)
racPath = lens _racPath (\s a -> s {_racPath = a})

-- | The protocol. You can specify HTTP, HTTPS, or #{protocol}. You can redirect HTTP to HTTP, HTTP to HTTPS, and HTTPS to HTTPS. You cannot redirect HTTPS to HTTP.
racProtocol :: Lens' RedirectActionConfig (Maybe Text)
racProtocol = lens _racProtocol (\s a -> s {_racProtocol = a})

-- | The query parameters, URL-encoded when necessary, but not percent-encoded. Do not include the leading "?", as it is automatically added. You can specify any of the reserved keywords.
racQuery :: Lens' RedirectActionConfig (Maybe Text)
racQuery = lens _racQuery (\s a -> s {_racQuery = a})

-- | The hostname. This component is not percent-encoded. The hostname can contain #{host}.
racHost :: Lens' RedirectActionConfig (Maybe Text)
racHost = lens _racHost (\s a -> s {_racHost = a})

-- | The port. You can specify a value from 1 to 65535 or #{port}.
racPort :: Lens' RedirectActionConfig (Maybe Text)
racPort = lens _racPort (\s a -> s {_racPort = a})

-- | The HTTP redirect code. The redirect is either permanent (HTTP 301) or temporary (HTTP 302).
racStatusCode :: Lens' RedirectActionConfig RedirectActionStatusCodeEnum
racStatusCode = lens _racStatusCode (\s a -> s {_racStatusCode = a})

instance FromXML RedirectActionConfig where
  parseXML x =
    RedirectActionConfig'
      <$> (x .@? "Path")
      <*> (x .@? "Protocol")
      <*> (x .@? "Query")
      <*> (x .@? "Host")
      <*> (x .@? "Port")
      <*> (x .@ "StatusCode")

instance Hashable RedirectActionConfig

instance NFData RedirectActionConfig

instance ToQuery RedirectActionConfig where
  toQuery RedirectActionConfig' {..} =
    mconcat
      [ "Path" =: _racPath,
        "Protocol" =: _racProtocol,
        "Query" =: _racQuery,
        "Host" =: _racHost,
        "Port" =: _racPort,
        "StatusCode" =: _racStatusCode
      ]

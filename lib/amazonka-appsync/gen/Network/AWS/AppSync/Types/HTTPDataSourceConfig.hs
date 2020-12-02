{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.HTTPDataSourceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.HTTPDataSourceConfig where

import Network.AWS.AppSync.Types.AuthorizationConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an HTTP data source configuration.
--
--
--
-- /See:/ 'hTTPDataSourceConfig' smart constructor.
data HTTPDataSourceConfig = HTTPDataSourceConfig'
  { _httpdscAuthorizationConfig ::
      !(Maybe AuthorizationConfig),
    _httpdscEndpoint :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HTTPDataSourceConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'httpdscAuthorizationConfig' - The authorization config in case the HTTP endpoint requires authorization.
--
-- * 'httpdscEndpoint' - The HTTP URL endpoint. You can either specify the domain name or IP, and port combination, and the URL scheme must be HTTP or HTTPS. If the port is not specified, AWS AppSync uses the default port 80 for the HTTP endpoint and port 443 for HTTPS endpoints.
hTTPDataSourceConfig ::
  HTTPDataSourceConfig
hTTPDataSourceConfig =
  HTTPDataSourceConfig'
    { _httpdscAuthorizationConfig = Nothing,
      _httpdscEndpoint = Nothing
    }

-- | The authorization config in case the HTTP endpoint requires authorization.
httpdscAuthorizationConfig :: Lens' HTTPDataSourceConfig (Maybe AuthorizationConfig)
httpdscAuthorizationConfig = lens _httpdscAuthorizationConfig (\s a -> s {_httpdscAuthorizationConfig = a})

-- | The HTTP URL endpoint. You can either specify the domain name or IP, and port combination, and the URL scheme must be HTTP or HTTPS. If the port is not specified, AWS AppSync uses the default port 80 for the HTTP endpoint and port 443 for HTTPS endpoints.
httpdscEndpoint :: Lens' HTTPDataSourceConfig (Maybe Text)
httpdscEndpoint = lens _httpdscEndpoint (\s a -> s {_httpdscEndpoint = a})

instance FromJSON HTTPDataSourceConfig where
  parseJSON =
    withObject
      "HTTPDataSourceConfig"
      ( \x ->
          HTTPDataSourceConfig'
            <$> (x .:? "authorizationConfig") <*> (x .:? "endpoint")
      )

instance Hashable HTTPDataSourceConfig

instance NFData HTTPDataSourceConfig

instance ToJSON HTTPDataSourceConfig where
  toJSON HTTPDataSourceConfig' {..} =
    object
      ( catMaybes
          [ ("authorizationConfig" .=) <$> _httpdscAuthorizationConfig,
            ("endpoint" .=) <$> _httpdscEndpoint
          ]
      )

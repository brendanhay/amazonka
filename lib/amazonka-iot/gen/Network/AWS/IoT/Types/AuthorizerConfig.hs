{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuthorizerConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuthorizerConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object that specifies the authorization service for a domain.
--
--
--
-- /See:/ 'authorizerConfig' smart constructor.
data AuthorizerConfig = AuthorizerConfig'
  { _acAllowAuthorizerOverride ::
      !(Maybe Bool),
    _acDefaultAuthorizerName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AuthorizerConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acAllowAuthorizerOverride' - A Boolean that specifies whether the domain configuration's authorization service can be overridden.
--
-- * 'acDefaultAuthorizerName' - The name of the authorization service for a domain configuration.
authorizerConfig ::
  AuthorizerConfig
authorizerConfig =
  AuthorizerConfig'
    { _acAllowAuthorizerOverride = Nothing,
      _acDefaultAuthorizerName = Nothing
    }

-- | A Boolean that specifies whether the domain configuration's authorization service can be overridden.
acAllowAuthorizerOverride :: Lens' AuthorizerConfig (Maybe Bool)
acAllowAuthorizerOverride = lens _acAllowAuthorizerOverride (\s a -> s {_acAllowAuthorizerOverride = a})

-- | The name of the authorization service for a domain configuration.
acDefaultAuthorizerName :: Lens' AuthorizerConfig (Maybe Text)
acDefaultAuthorizerName = lens _acDefaultAuthorizerName (\s a -> s {_acDefaultAuthorizerName = a})

instance FromJSON AuthorizerConfig where
  parseJSON =
    withObject
      "AuthorizerConfig"
      ( \x ->
          AuthorizerConfig'
            <$> (x .:? "allowAuthorizerOverride")
            <*> (x .:? "defaultAuthorizerName")
      )

instance Hashable AuthorizerConfig

instance NFData AuthorizerConfig

instance ToJSON AuthorizerConfig where
  toJSON AuthorizerConfig' {..} =
    object
      ( catMaybes
          [ ("allowAuthorizerOverride" .=) <$> _acAllowAuthorizerOverride,
            ("defaultAuthorizerName" .=) <$> _acDefaultAuthorizerName
          ]
      )

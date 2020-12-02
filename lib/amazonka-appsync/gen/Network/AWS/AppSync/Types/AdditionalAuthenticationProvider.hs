{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.AdditionalAuthenticationProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.AdditionalAuthenticationProvider where

import Network.AWS.AppSync.Types.AuthenticationType
import Network.AWS.AppSync.Types.CognitoUserPoolConfig
import Network.AWS.AppSync.Types.OpenIdConnectConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an additional authentication provider.
--
--
--
-- /See:/ 'additionalAuthenticationProvider' smart constructor.
data AdditionalAuthenticationProvider = AdditionalAuthenticationProvider'
  { _aapOpenIdConnectConfig ::
      !( Maybe
           OpenIdConnectConfig
       ),
    _aapUserPoolConfig ::
      !( Maybe
           CognitoUserPoolConfig
       ),
    _aapAuthenticationType ::
      !( Maybe
           AuthenticationType
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AdditionalAuthenticationProvider' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aapOpenIdConnectConfig' - The OpenID Connect configuration.
--
-- * 'aapUserPoolConfig' - The Amazon Cognito user pool configuration.
--
-- * 'aapAuthenticationType' - The authentication type: API key, AWS IAM, OIDC, or Amazon Cognito user pools.
additionalAuthenticationProvider ::
  AdditionalAuthenticationProvider
additionalAuthenticationProvider =
  AdditionalAuthenticationProvider'
    { _aapOpenIdConnectConfig =
        Nothing,
      _aapUserPoolConfig = Nothing,
      _aapAuthenticationType = Nothing
    }

-- | The OpenID Connect configuration.
aapOpenIdConnectConfig :: Lens' AdditionalAuthenticationProvider (Maybe OpenIdConnectConfig)
aapOpenIdConnectConfig = lens _aapOpenIdConnectConfig (\s a -> s {_aapOpenIdConnectConfig = a})

-- | The Amazon Cognito user pool configuration.
aapUserPoolConfig :: Lens' AdditionalAuthenticationProvider (Maybe CognitoUserPoolConfig)
aapUserPoolConfig = lens _aapUserPoolConfig (\s a -> s {_aapUserPoolConfig = a})

-- | The authentication type: API key, AWS IAM, OIDC, or Amazon Cognito user pools.
aapAuthenticationType :: Lens' AdditionalAuthenticationProvider (Maybe AuthenticationType)
aapAuthenticationType = lens _aapAuthenticationType (\s a -> s {_aapAuthenticationType = a})

instance FromJSON AdditionalAuthenticationProvider where
  parseJSON =
    withObject
      "AdditionalAuthenticationProvider"
      ( \x ->
          AdditionalAuthenticationProvider'
            <$> (x .:? "openIDConnectConfig")
            <*> (x .:? "userPoolConfig")
            <*> (x .:? "authenticationType")
      )

instance Hashable AdditionalAuthenticationProvider

instance NFData AdditionalAuthenticationProvider

instance ToJSON AdditionalAuthenticationProvider where
  toJSON AdditionalAuthenticationProvider' {..} =
    object
      ( catMaybes
          [ ("openIDConnectConfig" .=) <$> _aapOpenIdConnectConfig,
            ("userPoolConfig" .=) <$> _aapUserPoolConfig,
            ("authenticationType" .=) <$> _aapAuthenticationType
          ]
      )

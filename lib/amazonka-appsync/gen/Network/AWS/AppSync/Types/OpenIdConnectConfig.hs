{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.OpenIdConnectConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.OpenIdConnectConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an OpenID Connect configuration.
--
--
--
-- /See:/ 'openIdConnectConfig' smart constructor.
data OpenIdConnectConfig = OpenIdConnectConfig'
  { _oiccAuthTTL ::
      !(Maybe Integer),
    _oiccClientId :: !(Maybe Text),
    _oiccIatTTL :: !(Maybe Integer),
    _oiccIssuer :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OpenIdConnectConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oiccAuthTTL' - The number of milliseconds a token is valid after being authenticated.
--
-- * 'oiccClientId' - The client identifier of the Relying party at the OpenID identity provider. This identifier is typically obtained when the Relying party is registered with the OpenID identity provider. You can specify a regular expression so the AWS AppSync can validate against multiple client identifiers at a time.
--
-- * 'oiccIatTTL' - The number of milliseconds a token is valid after being issued to a user.
--
-- * 'oiccIssuer' - The issuer for the OpenID Connect configuration. The issuer returned by discovery must exactly match the value of @iss@ in the ID token.
openIdConnectConfig ::
  -- | 'oiccIssuer'
  Text ->
  OpenIdConnectConfig
openIdConnectConfig pIssuer_ =
  OpenIdConnectConfig'
    { _oiccAuthTTL = Nothing,
      _oiccClientId = Nothing,
      _oiccIatTTL = Nothing,
      _oiccIssuer = pIssuer_
    }

-- | The number of milliseconds a token is valid after being authenticated.
oiccAuthTTL :: Lens' OpenIdConnectConfig (Maybe Integer)
oiccAuthTTL = lens _oiccAuthTTL (\s a -> s {_oiccAuthTTL = a})

-- | The client identifier of the Relying party at the OpenID identity provider. This identifier is typically obtained when the Relying party is registered with the OpenID identity provider. You can specify a regular expression so the AWS AppSync can validate against multiple client identifiers at a time.
oiccClientId :: Lens' OpenIdConnectConfig (Maybe Text)
oiccClientId = lens _oiccClientId (\s a -> s {_oiccClientId = a})

-- | The number of milliseconds a token is valid after being issued to a user.
oiccIatTTL :: Lens' OpenIdConnectConfig (Maybe Integer)
oiccIatTTL = lens _oiccIatTTL (\s a -> s {_oiccIatTTL = a})

-- | The issuer for the OpenID Connect configuration. The issuer returned by discovery must exactly match the value of @iss@ in the ID token.
oiccIssuer :: Lens' OpenIdConnectConfig Text
oiccIssuer = lens _oiccIssuer (\s a -> s {_oiccIssuer = a})

instance FromJSON OpenIdConnectConfig where
  parseJSON =
    withObject
      "OpenIdConnectConfig"
      ( \x ->
          OpenIdConnectConfig'
            <$> (x .:? "authTTL")
            <*> (x .:? "clientId")
            <*> (x .:? "iatTTL")
            <*> (x .: "issuer")
      )

instance Hashable OpenIdConnectConfig

instance NFData OpenIdConnectConfig

instance ToJSON OpenIdConnectConfig where
  toJSON OpenIdConnectConfig' {..} =
    object
      ( catMaybes
          [ ("authTTL" .=) <$> _oiccAuthTTL,
            ("clientId" .=) <$> _oiccClientId,
            ("iatTTL" .=) <$> _oiccIatTTL,
            Just ("issuer" .= _oiccIssuer)
          ]
      )

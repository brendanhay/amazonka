{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.OidcConfigForResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.OidcConfigForResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Your OIDC IdP workforce configuration.
--
--
--
-- /See:/ 'oidcConfigForResponse' smart constructor.
data OidcConfigForResponse = OidcConfigForResponse'
  { _ocfClientId ::
      !(Maybe Text),
    _ocfJwksURI :: !(Maybe Text),
    _ocfUserInfoEndpoint :: !(Maybe Text),
    _ocfAuthorizationEndpoint :: !(Maybe Text),
    _ocfTokenEndpoint :: !(Maybe Text),
    _ocfIssuer :: !(Maybe Text),
    _ocfLogoutEndpoint :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OidcConfigForResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocfClientId' - The OIDC IdP client ID used to configure your private workforce.
--
-- * 'ocfJwksURI' - The OIDC IdP JSON Web Key Set (Jwks) URI used to configure your private workforce.
--
-- * 'ocfUserInfoEndpoint' - The OIDC IdP user information endpoint used to configure your private workforce.
--
-- * 'ocfAuthorizationEndpoint' - The OIDC IdP authorization endpoint used to configure your private workforce.
--
-- * 'ocfTokenEndpoint' - The OIDC IdP token endpoint used to configure your private workforce.
--
-- * 'ocfIssuer' - The OIDC IdP issuer used to configure your private workforce.
--
-- * 'ocfLogoutEndpoint' - The OIDC IdP logout endpoint used to configure your private workforce.
oidcConfigForResponse ::
  OidcConfigForResponse
oidcConfigForResponse =
  OidcConfigForResponse'
    { _ocfClientId = Nothing,
      _ocfJwksURI = Nothing,
      _ocfUserInfoEndpoint = Nothing,
      _ocfAuthorizationEndpoint = Nothing,
      _ocfTokenEndpoint = Nothing,
      _ocfIssuer = Nothing,
      _ocfLogoutEndpoint = Nothing
    }

-- | The OIDC IdP client ID used to configure your private workforce.
ocfClientId :: Lens' OidcConfigForResponse (Maybe Text)
ocfClientId = lens _ocfClientId (\s a -> s {_ocfClientId = a})

-- | The OIDC IdP JSON Web Key Set (Jwks) URI used to configure your private workforce.
ocfJwksURI :: Lens' OidcConfigForResponse (Maybe Text)
ocfJwksURI = lens _ocfJwksURI (\s a -> s {_ocfJwksURI = a})

-- | The OIDC IdP user information endpoint used to configure your private workforce.
ocfUserInfoEndpoint :: Lens' OidcConfigForResponse (Maybe Text)
ocfUserInfoEndpoint = lens _ocfUserInfoEndpoint (\s a -> s {_ocfUserInfoEndpoint = a})

-- | The OIDC IdP authorization endpoint used to configure your private workforce.
ocfAuthorizationEndpoint :: Lens' OidcConfigForResponse (Maybe Text)
ocfAuthorizationEndpoint = lens _ocfAuthorizationEndpoint (\s a -> s {_ocfAuthorizationEndpoint = a})

-- | The OIDC IdP token endpoint used to configure your private workforce.
ocfTokenEndpoint :: Lens' OidcConfigForResponse (Maybe Text)
ocfTokenEndpoint = lens _ocfTokenEndpoint (\s a -> s {_ocfTokenEndpoint = a})

-- | The OIDC IdP issuer used to configure your private workforce.
ocfIssuer :: Lens' OidcConfigForResponse (Maybe Text)
ocfIssuer = lens _ocfIssuer (\s a -> s {_ocfIssuer = a})

-- | The OIDC IdP logout endpoint used to configure your private workforce.
ocfLogoutEndpoint :: Lens' OidcConfigForResponse (Maybe Text)
ocfLogoutEndpoint = lens _ocfLogoutEndpoint (\s a -> s {_ocfLogoutEndpoint = a})

instance FromJSON OidcConfigForResponse where
  parseJSON =
    withObject
      "OidcConfigForResponse"
      ( \x ->
          OidcConfigForResponse'
            <$> (x .:? "ClientId")
            <*> (x .:? "JwksUri")
            <*> (x .:? "UserInfoEndpoint")
            <*> (x .:? "AuthorizationEndpoint")
            <*> (x .:? "TokenEndpoint")
            <*> (x .:? "Issuer")
            <*> (x .:? "LogoutEndpoint")
      )

instance Hashable OidcConfigForResponse

instance NFData OidcConfigForResponse

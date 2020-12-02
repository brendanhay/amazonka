{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.OidcConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.OidcConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Use this parameter to configure your OIDC Identity Provider (IdP).
--
--
--
-- /See:/ 'oidcConfig' smart constructor.
data OidcConfig = OidcConfig'
  { _ocClientId :: !Text,
    _ocClientSecret :: !(Sensitive Text),
    _ocIssuer :: !Text,
    _ocAuthorizationEndpoint :: !Text,
    _ocTokenEndpoint :: !Text,
    _ocUserInfoEndpoint :: !Text,
    _ocLogoutEndpoint :: !Text,
    _ocJwksURI :: !Text
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'OidcConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocClientId' - The OIDC IdP client ID used to configure your private workforce.
--
-- * 'ocClientSecret' - The OIDC IdP client secret used to configure your private workforce.
--
-- * 'ocIssuer' - The OIDC IdP issuer used to configure your private workforce.
--
-- * 'ocAuthorizationEndpoint' - The OIDC IdP authorization endpoint used to configure your private workforce.
--
-- * 'ocTokenEndpoint' - The OIDC IdP token endpoint used to configure your private workforce.
--
-- * 'ocUserInfoEndpoint' - The OIDC IdP user information endpoint used to configure your private workforce.
--
-- * 'ocLogoutEndpoint' - The OIDC IdP logout endpoint used to configure your private workforce.
--
-- * 'ocJwksURI' - The OIDC IdP JSON Web Key Set (Jwks) URI used to configure your private workforce.
oidcConfig ::
  -- | 'ocClientId'
  Text ->
  -- | 'ocClientSecret'
  Text ->
  -- | 'ocIssuer'
  Text ->
  -- | 'ocAuthorizationEndpoint'
  Text ->
  -- | 'ocTokenEndpoint'
  Text ->
  -- | 'ocUserInfoEndpoint'
  Text ->
  -- | 'ocLogoutEndpoint'
  Text ->
  -- | 'ocJwksURI'
  Text ->
  OidcConfig
oidcConfig
  pClientId_
  pClientSecret_
  pIssuer_
  pAuthorizationEndpoint_
  pTokenEndpoint_
  pUserInfoEndpoint_
  pLogoutEndpoint_
  pJwksURI_ =
    OidcConfig'
      { _ocClientId = pClientId_,
        _ocClientSecret = _Sensitive # pClientSecret_,
        _ocIssuer = pIssuer_,
        _ocAuthorizationEndpoint = pAuthorizationEndpoint_,
        _ocTokenEndpoint = pTokenEndpoint_,
        _ocUserInfoEndpoint = pUserInfoEndpoint_,
        _ocLogoutEndpoint = pLogoutEndpoint_,
        _ocJwksURI = pJwksURI_
      }

-- | The OIDC IdP client ID used to configure your private workforce.
ocClientId :: Lens' OidcConfig Text
ocClientId = lens _ocClientId (\s a -> s {_ocClientId = a})

-- | The OIDC IdP client secret used to configure your private workforce.
ocClientSecret :: Lens' OidcConfig Text
ocClientSecret = lens _ocClientSecret (\s a -> s {_ocClientSecret = a}) . _Sensitive

-- | The OIDC IdP issuer used to configure your private workforce.
ocIssuer :: Lens' OidcConfig Text
ocIssuer = lens _ocIssuer (\s a -> s {_ocIssuer = a})

-- | The OIDC IdP authorization endpoint used to configure your private workforce.
ocAuthorizationEndpoint :: Lens' OidcConfig Text
ocAuthorizationEndpoint = lens _ocAuthorizationEndpoint (\s a -> s {_ocAuthorizationEndpoint = a})

-- | The OIDC IdP token endpoint used to configure your private workforce.
ocTokenEndpoint :: Lens' OidcConfig Text
ocTokenEndpoint = lens _ocTokenEndpoint (\s a -> s {_ocTokenEndpoint = a})

-- | The OIDC IdP user information endpoint used to configure your private workforce.
ocUserInfoEndpoint :: Lens' OidcConfig Text
ocUserInfoEndpoint = lens _ocUserInfoEndpoint (\s a -> s {_ocUserInfoEndpoint = a})

-- | The OIDC IdP logout endpoint used to configure your private workforce.
ocLogoutEndpoint :: Lens' OidcConfig Text
ocLogoutEndpoint = lens _ocLogoutEndpoint (\s a -> s {_ocLogoutEndpoint = a})

-- | The OIDC IdP JSON Web Key Set (Jwks) URI used to configure your private workforce.
ocJwksURI :: Lens' OidcConfig Text
ocJwksURI = lens _ocJwksURI (\s a -> s {_ocJwksURI = a})

instance Hashable OidcConfig

instance NFData OidcConfig

instance ToJSON OidcConfig where
  toJSON OidcConfig' {..} =
    object
      ( catMaybes
          [ Just ("ClientId" .= _ocClientId),
            Just ("ClientSecret" .= _ocClientSecret),
            Just ("Issuer" .= _ocIssuer),
            Just ("AuthorizationEndpoint" .= _ocAuthorizationEndpoint),
            Just ("TokenEndpoint" .= _ocTokenEndpoint),
            Just ("UserInfoEndpoint" .= _ocUserInfoEndpoint),
            Just ("LogoutEndpoint" .= _ocLogoutEndpoint),
            Just ("JwksUri" .= _ocJwksURI)
          ]
      )

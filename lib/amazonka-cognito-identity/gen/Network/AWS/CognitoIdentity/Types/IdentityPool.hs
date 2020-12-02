{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types.IdentityPool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Types.IdentityPool where

import Network.AWS.CognitoIdentity.Types.CognitoIdentityProvider
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object representing an Amazon Cognito identity pool.
--
--
--
-- /See:/ 'identityPool' smart constructor.
data IdentityPool = IdentityPool'
  { _ipSamlProviderARNs ::
      !(Maybe [Text]),
    _ipSupportedLoginProviders :: !(Maybe (Map Text (Text))),
    _ipAllowClassicFlow :: !(Maybe Bool),
    _ipDeveloperProviderName :: !(Maybe Text),
    _ipIdentityPoolTags :: !(Maybe (Map Text (Text))),
    _ipOpenIdConnectProviderARNs :: !(Maybe [Text]),
    _ipCognitoIdentityProviders :: !(Maybe [CognitoIdentityProvider]),
    _ipIdentityPoolId :: !Text,
    _ipIdentityPoolName :: !Text,
    _ipAllowUnauthenticatedIdentities :: !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IdentityPool' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipSamlProviderARNs' - An array of Amazon Resource Names (ARNs) of the SAML provider for your identity pool.
--
-- * 'ipSupportedLoginProviders' - Optional key:value pairs mapping provider names to provider app IDs.
--
-- * 'ipAllowClassicFlow' - Enables or disables the Basic (Classic) authentication flow. For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/authentication-flow.html Identity Pools (Federated Identities) Authentication Flow> in the /Amazon Cognito Developer Guide/ .
--
-- * 'ipDeveloperProviderName' - The "domain" by which Cognito will refer to your users.
--
-- * 'ipIdentityPoolTags' - The tags that are assigned to the identity pool. A tag is a label that you can apply to identity pools to categorize and manage them in different ways, such as by purpose, owner, environment, or other criteria.
--
-- * 'ipOpenIdConnectProviderARNs' - A list of OpendID Connect provider ARNs.
--
-- * 'ipCognitoIdentityProviders' - A list representing an Amazon Cognito user pool and its client ID.
--
-- * 'ipIdentityPoolId' - An identity pool ID in the format REGION:GUID.
--
-- * 'ipIdentityPoolName' - A string that you provide.
--
-- * 'ipAllowUnauthenticatedIdentities' - TRUE if the identity pool supports unauthenticated logins.
identityPool ::
  -- | 'ipIdentityPoolId'
  Text ->
  -- | 'ipIdentityPoolName'
  Text ->
  -- | 'ipAllowUnauthenticatedIdentities'
  Bool ->
  IdentityPool
identityPool
  pIdentityPoolId_
  pIdentityPoolName_
  pAllowUnauthenticatedIdentities_ =
    IdentityPool'
      { _ipSamlProviderARNs = Nothing,
        _ipSupportedLoginProviders = Nothing,
        _ipAllowClassicFlow = Nothing,
        _ipDeveloperProviderName = Nothing,
        _ipIdentityPoolTags = Nothing,
        _ipOpenIdConnectProviderARNs = Nothing,
        _ipCognitoIdentityProviders = Nothing,
        _ipIdentityPoolId = pIdentityPoolId_,
        _ipIdentityPoolName = pIdentityPoolName_,
        _ipAllowUnauthenticatedIdentities =
          pAllowUnauthenticatedIdentities_
      }

-- | An array of Amazon Resource Names (ARNs) of the SAML provider for your identity pool.
ipSamlProviderARNs :: Lens' IdentityPool [Text]
ipSamlProviderARNs = lens _ipSamlProviderARNs (\s a -> s {_ipSamlProviderARNs = a}) . _Default . _Coerce

-- | Optional key:value pairs mapping provider names to provider app IDs.
ipSupportedLoginProviders :: Lens' IdentityPool (HashMap Text (Text))
ipSupportedLoginProviders = lens _ipSupportedLoginProviders (\s a -> s {_ipSupportedLoginProviders = a}) . _Default . _Map

-- | Enables or disables the Basic (Classic) authentication flow. For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/authentication-flow.html Identity Pools (Federated Identities) Authentication Flow> in the /Amazon Cognito Developer Guide/ .
ipAllowClassicFlow :: Lens' IdentityPool (Maybe Bool)
ipAllowClassicFlow = lens _ipAllowClassicFlow (\s a -> s {_ipAllowClassicFlow = a})

-- | The "domain" by which Cognito will refer to your users.
ipDeveloperProviderName :: Lens' IdentityPool (Maybe Text)
ipDeveloperProviderName = lens _ipDeveloperProviderName (\s a -> s {_ipDeveloperProviderName = a})

-- | The tags that are assigned to the identity pool. A tag is a label that you can apply to identity pools to categorize and manage them in different ways, such as by purpose, owner, environment, or other criteria.
ipIdentityPoolTags :: Lens' IdentityPool (HashMap Text (Text))
ipIdentityPoolTags = lens _ipIdentityPoolTags (\s a -> s {_ipIdentityPoolTags = a}) . _Default . _Map

-- | A list of OpendID Connect provider ARNs.
ipOpenIdConnectProviderARNs :: Lens' IdentityPool [Text]
ipOpenIdConnectProviderARNs = lens _ipOpenIdConnectProviderARNs (\s a -> s {_ipOpenIdConnectProviderARNs = a}) . _Default . _Coerce

-- | A list representing an Amazon Cognito user pool and its client ID.
ipCognitoIdentityProviders :: Lens' IdentityPool [CognitoIdentityProvider]
ipCognitoIdentityProviders = lens _ipCognitoIdentityProviders (\s a -> s {_ipCognitoIdentityProviders = a}) . _Default . _Coerce

-- | An identity pool ID in the format REGION:GUID.
ipIdentityPoolId :: Lens' IdentityPool Text
ipIdentityPoolId = lens _ipIdentityPoolId (\s a -> s {_ipIdentityPoolId = a})

-- | A string that you provide.
ipIdentityPoolName :: Lens' IdentityPool Text
ipIdentityPoolName = lens _ipIdentityPoolName (\s a -> s {_ipIdentityPoolName = a})

-- | TRUE if the identity pool supports unauthenticated logins.
ipAllowUnauthenticatedIdentities :: Lens' IdentityPool Bool
ipAllowUnauthenticatedIdentities = lens _ipAllowUnauthenticatedIdentities (\s a -> s {_ipAllowUnauthenticatedIdentities = a})

instance FromJSON IdentityPool where
  parseJSON =
    withObject
      "IdentityPool"
      ( \x ->
          IdentityPool'
            <$> (x .:? "SamlProviderARNs" .!= mempty)
            <*> (x .:? "SupportedLoginProviders" .!= mempty)
            <*> (x .:? "AllowClassicFlow")
            <*> (x .:? "DeveloperProviderName")
            <*> (x .:? "IdentityPoolTags" .!= mempty)
            <*> (x .:? "OpenIdConnectProviderARNs" .!= mempty)
            <*> (x .:? "CognitoIdentityProviders" .!= mempty)
            <*> (x .: "IdentityPoolId")
            <*> (x .: "IdentityPoolName")
            <*> (x .: "AllowUnauthenticatedIdentities")
      )

instance Hashable IdentityPool

instance NFData IdentityPool

instance ToJSON IdentityPool where
  toJSON IdentityPool' {..} =
    object
      ( catMaybes
          [ ("SamlProviderARNs" .=) <$> _ipSamlProviderARNs,
            ("SupportedLoginProviders" .=) <$> _ipSupportedLoginProviders,
            ("AllowClassicFlow" .=) <$> _ipAllowClassicFlow,
            ("DeveloperProviderName" .=) <$> _ipDeveloperProviderName,
            ("IdentityPoolTags" .=) <$> _ipIdentityPoolTags,
            ("OpenIdConnectProviderARNs" .=) <$> _ipOpenIdConnectProviderARNs,
            ("CognitoIdentityProviders" .=) <$> _ipCognitoIdentityProviders,
            Just ("IdentityPoolId" .= _ipIdentityPoolId),
            Just ("IdentityPoolName" .= _ipIdentityPoolName),
            Just
              ( "AllowUnauthenticatedIdentities"
                  .= _ipAllowUnauthenticatedIdentities
              )
          ]
      )

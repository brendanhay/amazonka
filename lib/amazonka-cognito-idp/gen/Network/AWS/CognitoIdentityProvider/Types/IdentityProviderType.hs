{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.IdentityProviderType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.IdentityProviderType where

import Network.AWS.CognitoIdentityProvider.Types.IdentityProviderTypeType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A container for information about an identity provider.
--
--
--
-- /See:/ 'identityProviderType' smart constructor.
data IdentityProviderType = IdentityProviderType'
  { _iptLastModifiedDate ::
      !(Maybe POSIX),
    _iptUserPoolId :: !(Maybe Text),
    _iptProviderType ::
      !(Maybe IdentityProviderTypeType),
    _iptCreationDate :: !(Maybe POSIX),
    _iptIdpIdentifiers :: !(Maybe [Text]),
    _iptAttributeMapping ::
      !(Maybe (Map Text (Text))),
    _iptProviderDetails :: !(Maybe (Map Text (Text))),
    _iptProviderName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IdentityProviderType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iptLastModifiedDate' - The date the identity provider was last modified.
--
-- * 'iptUserPoolId' - The user pool ID.
--
-- * 'iptProviderType' - The identity provider type.
--
-- * 'iptCreationDate' - The date the identity provider was created.
--
-- * 'iptIdpIdentifiers' - A list of identity provider identifiers.
--
-- * 'iptAttributeMapping' - A mapping of identity provider attributes to standard and custom user pool attributes.
--
-- * 'iptProviderDetails' - The identity provider details. The following list describes the provider detail keys for each identity provider type.     * For Google and Login with Amazon:     * client_id     * client_secret     * authorize_scopes     * For Facebook:     * client_id     * client_secret     * authorize_scopes     * api_version     * For Sign in with Apple:     * client_id     * team_id     * key_id     * private_key     * authorize_scopes     * For OIDC providers:     * client_id     * client_secret     * attributes_request_method     * oidc_issuer     * authorize_scopes     * authorize_url /if not available from discovery URL specified by oidc_issuer key/      * token_url /if not available from discovery URL specified by oidc_issuer key/      * attributes_url /if not available from discovery URL specified by oidc_issuer key/      * jwks_uri /if not available from discovery URL specified by oidc_issuer key/      * authorize_scopes     * For SAML providers:     * MetadataFile OR MetadataURL     * IDPSignOut /optional/
--
-- * 'iptProviderName' - The identity provider name.
identityProviderType ::
  IdentityProviderType
identityProviderType =
  IdentityProviderType'
    { _iptLastModifiedDate = Nothing,
      _iptUserPoolId = Nothing,
      _iptProviderType = Nothing,
      _iptCreationDate = Nothing,
      _iptIdpIdentifiers = Nothing,
      _iptAttributeMapping = Nothing,
      _iptProviderDetails = Nothing,
      _iptProviderName = Nothing
    }

-- | The date the identity provider was last modified.
iptLastModifiedDate :: Lens' IdentityProviderType (Maybe UTCTime)
iptLastModifiedDate = lens _iptLastModifiedDate (\s a -> s {_iptLastModifiedDate = a}) . mapping _Time

-- | The user pool ID.
iptUserPoolId :: Lens' IdentityProviderType (Maybe Text)
iptUserPoolId = lens _iptUserPoolId (\s a -> s {_iptUserPoolId = a})

-- | The identity provider type.
iptProviderType :: Lens' IdentityProviderType (Maybe IdentityProviderTypeType)
iptProviderType = lens _iptProviderType (\s a -> s {_iptProviderType = a})

-- | The date the identity provider was created.
iptCreationDate :: Lens' IdentityProviderType (Maybe UTCTime)
iptCreationDate = lens _iptCreationDate (\s a -> s {_iptCreationDate = a}) . mapping _Time

-- | A list of identity provider identifiers.
iptIdpIdentifiers :: Lens' IdentityProviderType [Text]
iptIdpIdentifiers = lens _iptIdpIdentifiers (\s a -> s {_iptIdpIdentifiers = a}) . _Default . _Coerce

-- | A mapping of identity provider attributes to standard and custom user pool attributes.
iptAttributeMapping :: Lens' IdentityProviderType (HashMap Text (Text))
iptAttributeMapping = lens _iptAttributeMapping (\s a -> s {_iptAttributeMapping = a}) . _Default . _Map

-- | The identity provider details. The following list describes the provider detail keys for each identity provider type.     * For Google and Login with Amazon:     * client_id     * client_secret     * authorize_scopes     * For Facebook:     * client_id     * client_secret     * authorize_scopes     * api_version     * For Sign in with Apple:     * client_id     * team_id     * key_id     * private_key     * authorize_scopes     * For OIDC providers:     * client_id     * client_secret     * attributes_request_method     * oidc_issuer     * authorize_scopes     * authorize_url /if not available from discovery URL specified by oidc_issuer key/      * token_url /if not available from discovery URL specified by oidc_issuer key/      * attributes_url /if not available from discovery URL specified by oidc_issuer key/      * jwks_uri /if not available from discovery URL specified by oidc_issuer key/      * authorize_scopes     * For SAML providers:     * MetadataFile OR MetadataURL     * IDPSignOut /optional/
iptProviderDetails :: Lens' IdentityProviderType (HashMap Text (Text))
iptProviderDetails = lens _iptProviderDetails (\s a -> s {_iptProviderDetails = a}) . _Default . _Map

-- | The identity provider name.
iptProviderName :: Lens' IdentityProviderType (Maybe Text)
iptProviderName = lens _iptProviderName (\s a -> s {_iptProviderName = a})

instance FromJSON IdentityProviderType where
  parseJSON =
    withObject
      "IdentityProviderType"
      ( \x ->
          IdentityProviderType'
            <$> (x .:? "LastModifiedDate")
            <*> (x .:? "UserPoolId")
            <*> (x .:? "ProviderType")
            <*> (x .:? "CreationDate")
            <*> (x .:? "IdpIdentifiers" .!= mempty)
            <*> (x .:? "AttributeMapping" .!= mempty)
            <*> (x .:? "ProviderDetails" .!= mempty)
            <*> (x .:? "ProviderName")
      )

instance Hashable IdentityProviderType

instance NFData IdentityProviderType

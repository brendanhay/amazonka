{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.IdentityProviderType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.IdentityProviderType where

import Network.AWS.CognitoIdentityProvider.Types.IdentityProviderTypeType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A container for information about an identity provider.
--
-- /See:/ 'newIdentityProviderType' smart constructor.
data IdentityProviderType = IdentityProviderType'
  { -- | The date the identity provider was last modified.
    lastModifiedDate :: Prelude.Maybe Core.POSIX,
    -- | The identity provider type.
    providerType :: Prelude.Maybe IdentityProviderTypeType,
    -- | The identity provider name.
    providerName :: Prelude.Maybe Prelude.Text,
    -- | The identity provider details. The following list describes the provider
    -- detail keys for each identity provider type.
    --
    -- -   For Google and Login with Amazon:
    --
    --     -   client_id
    --
    --     -   client_secret
    --
    --     -   authorize_scopes
    --
    -- -   For Facebook:
    --
    --     -   client_id
    --
    --     -   client_secret
    --
    --     -   authorize_scopes
    --
    --     -   api_version
    --
    -- -   For Sign in with Apple:
    --
    --     -   client_id
    --
    --     -   team_id
    --
    --     -   key_id
    --
    --     -   private_key
    --
    --     -   authorize_scopes
    --
    -- -   For OIDC providers:
    --
    --     -   client_id
    --
    --     -   client_secret
    --
    --     -   attributes_request_method
    --
    --     -   oidc_issuer
    --
    --     -   authorize_scopes
    --
    --     -   authorize_url /if not available from discovery URL specified by
    --         oidc_issuer key/
    --
    --     -   token_url /if not available from discovery URL specified by
    --         oidc_issuer key/
    --
    --     -   attributes_url /if not available from discovery URL specified by
    --         oidc_issuer key/
    --
    --     -   jwks_uri /if not available from discovery URL specified by
    --         oidc_issuer key/
    --
    --     -   authorize_scopes
    --
    -- -   For SAML providers:
    --
    --     -   MetadataFile OR MetadataURL
    --
    --     -   IDPSignOut /optional/
    providerDetails :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The user pool ID.
    userPoolId :: Prelude.Maybe Prelude.Text,
    -- | The date the identity provider was created.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | A list of identity provider identifiers.
    idpIdentifiers :: Prelude.Maybe [Prelude.Text],
    -- | A mapping of identity provider attributes to standard and custom user
    -- pool attributes.
    attributeMapping :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IdentityProviderType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'identityProviderType_lastModifiedDate' - The date the identity provider was last modified.
--
-- 'providerType', 'identityProviderType_providerType' - The identity provider type.
--
-- 'providerName', 'identityProviderType_providerName' - The identity provider name.
--
-- 'providerDetails', 'identityProviderType_providerDetails' - The identity provider details. The following list describes the provider
-- detail keys for each identity provider type.
--
-- -   For Google and Login with Amazon:
--
--     -   client_id
--
--     -   client_secret
--
--     -   authorize_scopes
--
-- -   For Facebook:
--
--     -   client_id
--
--     -   client_secret
--
--     -   authorize_scopes
--
--     -   api_version
--
-- -   For Sign in with Apple:
--
--     -   client_id
--
--     -   team_id
--
--     -   key_id
--
--     -   private_key
--
--     -   authorize_scopes
--
-- -   For OIDC providers:
--
--     -   client_id
--
--     -   client_secret
--
--     -   attributes_request_method
--
--     -   oidc_issuer
--
--     -   authorize_scopes
--
--     -   authorize_url /if not available from discovery URL specified by
--         oidc_issuer key/
--
--     -   token_url /if not available from discovery URL specified by
--         oidc_issuer key/
--
--     -   attributes_url /if not available from discovery URL specified by
--         oidc_issuer key/
--
--     -   jwks_uri /if not available from discovery URL specified by
--         oidc_issuer key/
--
--     -   authorize_scopes
--
-- -   For SAML providers:
--
--     -   MetadataFile OR MetadataURL
--
--     -   IDPSignOut /optional/
--
-- 'userPoolId', 'identityProviderType_userPoolId' - The user pool ID.
--
-- 'creationDate', 'identityProviderType_creationDate' - The date the identity provider was created.
--
-- 'idpIdentifiers', 'identityProviderType_idpIdentifiers' - A list of identity provider identifiers.
--
-- 'attributeMapping', 'identityProviderType_attributeMapping' - A mapping of identity provider attributes to standard and custom user
-- pool attributes.
newIdentityProviderType ::
  IdentityProviderType
newIdentityProviderType =
  IdentityProviderType'
    { lastModifiedDate =
        Prelude.Nothing,
      providerType = Prelude.Nothing,
      providerName = Prelude.Nothing,
      providerDetails = Prelude.Nothing,
      userPoolId = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      idpIdentifiers = Prelude.Nothing,
      attributeMapping = Prelude.Nothing
    }

-- | The date the identity provider was last modified.
identityProviderType_lastModifiedDate :: Lens.Lens' IdentityProviderType (Prelude.Maybe Prelude.UTCTime)
identityProviderType_lastModifiedDate = Lens.lens (\IdentityProviderType' {lastModifiedDate} -> lastModifiedDate) (\s@IdentityProviderType' {} a -> s {lastModifiedDate = a} :: IdentityProviderType) Prelude.. Lens.mapping Core._Time

-- | The identity provider type.
identityProviderType_providerType :: Lens.Lens' IdentityProviderType (Prelude.Maybe IdentityProviderTypeType)
identityProviderType_providerType = Lens.lens (\IdentityProviderType' {providerType} -> providerType) (\s@IdentityProviderType' {} a -> s {providerType = a} :: IdentityProviderType)

-- | The identity provider name.
identityProviderType_providerName :: Lens.Lens' IdentityProviderType (Prelude.Maybe Prelude.Text)
identityProviderType_providerName = Lens.lens (\IdentityProviderType' {providerName} -> providerName) (\s@IdentityProviderType' {} a -> s {providerName = a} :: IdentityProviderType)

-- | The identity provider details. The following list describes the provider
-- detail keys for each identity provider type.
--
-- -   For Google and Login with Amazon:
--
--     -   client_id
--
--     -   client_secret
--
--     -   authorize_scopes
--
-- -   For Facebook:
--
--     -   client_id
--
--     -   client_secret
--
--     -   authorize_scopes
--
--     -   api_version
--
-- -   For Sign in with Apple:
--
--     -   client_id
--
--     -   team_id
--
--     -   key_id
--
--     -   private_key
--
--     -   authorize_scopes
--
-- -   For OIDC providers:
--
--     -   client_id
--
--     -   client_secret
--
--     -   attributes_request_method
--
--     -   oidc_issuer
--
--     -   authorize_scopes
--
--     -   authorize_url /if not available from discovery URL specified by
--         oidc_issuer key/
--
--     -   token_url /if not available from discovery URL specified by
--         oidc_issuer key/
--
--     -   attributes_url /if not available from discovery URL specified by
--         oidc_issuer key/
--
--     -   jwks_uri /if not available from discovery URL specified by
--         oidc_issuer key/
--
--     -   authorize_scopes
--
-- -   For SAML providers:
--
--     -   MetadataFile OR MetadataURL
--
--     -   IDPSignOut /optional/
identityProviderType_providerDetails :: Lens.Lens' IdentityProviderType (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
identityProviderType_providerDetails = Lens.lens (\IdentityProviderType' {providerDetails} -> providerDetails) (\s@IdentityProviderType' {} a -> s {providerDetails = a} :: IdentityProviderType) Prelude.. Lens.mapping Lens._Coerce

-- | The user pool ID.
identityProviderType_userPoolId :: Lens.Lens' IdentityProviderType (Prelude.Maybe Prelude.Text)
identityProviderType_userPoolId = Lens.lens (\IdentityProviderType' {userPoolId} -> userPoolId) (\s@IdentityProviderType' {} a -> s {userPoolId = a} :: IdentityProviderType)

-- | The date the identity provider was created.
identityProviderType_creationDate :: Lens.Lens' IdentityProviderType (Prelude.Maybe Prelude.UTCTime)
identityProviderType_creationDate = Lens.lens (\IdentityProviderType' {creationDate} -> creationDate) (\s@IdentityProviderType' {} a -> s {creationDate = a} :: IdentityProviderType) Prelude.. Lens.mapping Core._Time

-- | A list of identity provider identifiers.
identityProviderType_idpIdentifiers :: Lens.Lens' IdentityProviderType (Prelude.Maybe [Prelude.Text])
identityProviderType_idpIdentifiers = Lens.lens (\IdentityProviderType' {idpIdentifiers} -> idpIdentifiers) (\s@IdentityProviderType' {} a -> s {idpIdentifiers = a} :: IdentityProviderType) Prelude.. Lens.mapping Lens._Coerce

-- | A mapping of identity provider attributes to standard and custom user
-- pool attributes.
identityProviderType_attributeMapping :: Lens.Lens' IdentityProviderType (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
identityProviderType_attributeMapping = Lens.lens (\IdentityProviderType' {attributeMapping} -> attributeMapping) (\s@IdentityProviderType' {} a -> s {attributeMapping = a} :: IdentityProviderType) Prelude.. Lens.mapping Lens._Coerce

instance Core.FromJSON IdentityProviderType where
  parseJSON =
    Core.withObject
      "IdentityProviderType"
      ( \x ->
          IdentityProviderType'
            Prelude.<$> (x Core..:? "LastModifiedDate")
            Prelude.<*> (x Core..:? "ProviderType")
            Prelude.<*> (x Core..:? "ProviderName")
            Prelude.<*> ( x Core..:? "ProviderDetails"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "UserPoolId")
            Prelude.<*> (x Core..:? "CreationDate")
            Prelude.<*> (x Core..:? "IdpIdentifiers" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "AttributeMapping"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable IdentityProviderType

instance Prelude.NFData IdentityProviderType

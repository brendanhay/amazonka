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
-- Module      : Amazonka.CognitoIdentityProvider.Types.IdentityProviderType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.IdentityProviderType where

import Amazonka.CognitoIdentityProvider.Types.IdentityProviderTypeType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A container for information about an IdP.
--
-- /See:/ 'newIdentityProviderType' smart constructor.
data IdentityProviderType = IdentityProviderType'
  { -- | The IdP name.
    providerName :: Prelude.Maybe Prelude.Text,
    -- | The date the IdP was last modified.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | A mapping of IdP attributes to standard and custom user pool attributes.
    attributeMapping :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The IdP details. The following list describes the provider detail keys
    -- for each IdP type.
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
    --         /You can submit a private_key when you add or update an IdP.
    --         Describe operations don\'t return the private key./
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
    --     -   The following keys are only present if Amazon Cognito didn\'t
    --         discover them at the @oidc_issuer@ URL.
    --
    --         -   authorize_url
    --
    --         -   token_url
    --
    --         -   attributes_url
    --
    --         -   jwks_uri
    --
    --     -   Amazon Cognito sets the value of the following keys
    --         automatically. They are read-only.
    --
    --         -   attributes_url_add_attributes
    --
    -- -   For SAML providers:
    --
    --     -   MetadataFile or MetadataURL
    --
    --     -   IDPSignout /optional/
    providerDetails :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of IdP identifiers.
    idpIdentifiers :: Prelude.Maybe [Prelude.Text],
    -- | The date the IdP was created.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The IdP type.
    providerType :: Prelude.Maybe IdentityProviderTypeType,
    -- | The user pool ID.
    userPoolId :: Prelude.Maybe Prelude.Text
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
-- 'providerName', 'identityProviderType_providerName' - The IdP name.
--
-- 'lastModifiedDate', 'identityProviderType_lastModifiedDate' - The date the IdP was last modified.
--
-- 'attributeMapping', 'identityProviderType_attributeMapping' - A mapping of IdP attributes to standard and custom user pool attributes.
--
-- 'providerDetails', 'identityProviderType_providerDetails' - The IdP details. The following list describes the provider detail keys
-- for each IdP type.
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
--         /You can submit a private_key when you add or update an IdP.
--         Describe operations don\'t return the private key./
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
--     -   The following keys are only present if Amazon Cognito didn\'t
--         discover them at the @oidc_issuer@ URL.
--
--         -   authorize_url
--
--         -   token_url
--
--         -   attributes_url
--
--         -   jwks_uri
--
--     -   Amazon Cognito sets the value of the following keys
--         automatically. They are read-only.
--
--         -   attributes_url_add_attributes
--
-- -   For SAML providers:
--
--     -   MetadataFile or MetadataURL
--
--     -   IDPSignout /optional/
--
-- 'idpIdentifiers', 'identityProviderType_idpIdentifiers' - A list of IdP identifiers.
--
-- 'creationDate', 'identityProviderType_creationDate' - The date the IdP was created.
--
-- 'providerType', 'identityProviderType_providerType' - The IdP type.
--
-- 'userPoolId', 'identityProviderType_userPoolId' - The user pool ID.
newIdentityProviderType ::
  IdentityProviderType
newIdentityProviderType =
  IdentityProviderType'
    { providerName =
        Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      attributeMapping = Prelude.Nothing,
      providerDetails = Prelude.Nothing,
      idpIdentifiers = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      providerType = Prelude.Nothing,
      userPoolId = Prelude.Nothing
    }

-- | The IdP name.
identityProviderType_providerName :: Lens.Lens' IdentityProviderType (Prelude.Maybe Prelude.Text)
identityProviderType_providerName = Lens.lens (\IdentityProviderType' {providerName} -> providerName) (\s@IdentityProviderType' {} a -> s {providerName = a} :: IdentityProviderType)

-- | The date the IdP was last modified.
identityProviderType_lastModifiedDate :: Lens.Lens' IdentityProviderType (Prelude.Maybe Prelude.UTCTime)
identityProviderType_lastModifiedDate = Lens.lens (\IdentityProviderType' {lastModifiedDate} -> lastModifiedDate) (\s@IdentityProviderType' {} a -> s {lastModifiedDate = a} :: IdentityProviderType) Prelude.. Lens.mapping Data._Time

-- | A mapping of IdP attributes to standard and custom user pool attributes.
identityProviderType_attributeMapping :: Lens.Lens' IdentityProviderType (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
identityProviderType_attributeMapping = Lens.lens (\IdentityProviderType' {attributeMapping} -> attributeMapping) (\s@IdentityProviderType' {} a -> s {attributeMapping = a} :: IdentityProviderType) Prelude.. Lens.mapping Lens.coerced

-- | The IdP details. The following list describes the provider detail keys
-- for each IdP type.
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
--         /You can submit a private_key when you add or update an IdP.
--         Describe operations don\'t return the private key./
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
--     -   The following keys are only present if Amazon Cognito didn\'t
--         discover them at the @oidc_issuer@ URL.
--
--         -   authorize_url
--
--         -   token_url
--
--         -   attributes_url
--
--         -   jwks_uri
--
--     -   Amazon Cognito sets the value of the following keys
--         automatically. They are read-only.
--
--         -   attributes_url_add_attributes
--
-- -   For SAML providers:
--
--     -   MetadataFile or MetadataURL
--
--     -   IDPSignout /optional/
identityProviderType_providerDetails :: Lens.Lens' IdentityProviderType (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
identityProviderType_providerDetails = Lens.lens (\IdentityProviderType' {providerDetails} -> providerDetails) (\s@IdentityProviderType' {} a -> s {providerDetails = a} :: IdentityProviderType) Prelude.. Lens.mapping Lens.coerced

-- | A list of IdP identifiers.
identityProviderType_idpIdentifiers :: Lens.Lens' IdentityProviderType (Prelude.Maybe [Prelude.Text])
identityProviderType_idpIdentifiers = Lens.lens (\IdentityProviderType' {idpIdentifiers} -> idpIdentifiers) (\s@IdentityProviderType' {} a -> s {idpIdentifiers = a} :: IdentityProviderType) Prelude.. Lens.mapping Lens.coerced

-- | The date the IdP was created.
identityProviderType_creationDate :: Lens.Lens' IdentityProviderType (Prelude.Maybe Prelude.UTCTime)
identityProviderType_creationDate = Lens.lens (\IdentityProviderType' {creationDate} -> creationDate) (\s@IdentityProviderType' {} a -> s {creationDate = a} :: IdentityProviderType) Prelude.. Lens.mapping Data._Time

-- | The IdP type.
identityProviderType_providerType :: Lens.Lens' IdentityProviderType (Prelude.Maybe IdentityProviderTypeType)
identityProviderType_providerType = Lens.lens (\IdentityProviderType' {providerType} -> providerType) (\s@IdentityProviderType' {} a -> s {providerType = a} :: IdentityProviderType)

-- | The user pool ID.
identityProviderType_userPoolId :: Lens.Lens' IdentityProviderType (Prelude.Maybe Prelude.Text)
identityProviderType_userPoolId = Lens.lens (\IdentityProviderType' {userPoolId} -> userPoolId) (\s@IdentityProviderType' {} a -> s {userPoolId = a} :: IdentityProviderType)

instance Data.FromJSON IdentityProviderType where
  parseJSON =
    Data.withObject
      "IdentityProviderType"
      ( \x ->
          IdentityProviderType'
            Prelude.<$> (x Data..:? "ProviderName")
            Prelude.<*> (x Data..:? "LastModifiedDate")
            Prelude.<*> ( x Data..:? "AttributeMapping"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "ProviderDetails"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "IdpIdentifiers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "ProviderType")
            Prelude.<*> (x Data..:? "UserPoolId")
      )

instance Prelude.Hashable IdentityProviderType where
  hashWithSalt _salt IdentityProviderType' {..} =
    _salt `Prelude.hashWithSalt` providerName
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` attributeMapping
      `Prelude.hashWithSalt` providerDetails
      `Prelude.hashWithSalt` idpIdentifiers
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` providerType
      `Prelude.hashWithSalt` userPoolId

instance Prelude.NFData IdentityProviderType where
  rnf IdentityProviderType' {..} =
    Prelude.rnf providerName
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf attributeMapping
      `Prelude.seq` Prelude.rnf providerDetails
      `Prelude.seq` Prelude.rnf idpIdentifiers
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf providerType
      `Prelude.seq` Prelude.rnf userPoolId

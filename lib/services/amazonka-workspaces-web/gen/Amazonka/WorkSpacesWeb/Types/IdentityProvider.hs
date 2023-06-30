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
-- Module      : Amazonka.WorkSpacesWeb.Types.IdentityProvider
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpacesWeb.Types.IdentityProvider where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkSpacesWeb.Types.IdentityProviderType

-- | The identity provider.
--
-- /See:/ 'newIdentityProvider' smart constructor.
data IdentityProvider = IdentityProvider'
  { -- | The identity provider details. The following list describes the provider
    -- detail keys for each identity provider type.
    --
    -- -   For Google and Login with Amazon:
    --
    --     -   @client_id@
    --
    --     -   @client_secret@
    --
    --     -   @authorize_scopes@
    --
    -- -   For Facebook:
    --
    --     -   @client_id@
    --
    --     -   @client_secret@
    --
    --     -   @authorize_scopes@
    --
    --     -   @api_version@
    --
    -- -   For Sign in with Apple:
    --
    --     -   @client_id@
    --
    --     -   @team_id@
    --
    --     -   @key_id@
    --
    --     -   @private_key@
    --
    --     -   @authorize_scopes@
    --
    -- -   For OIDC providers:
    --
    --     -   @client_id@
    --
    --     -   @client_secret@
    --
    --     -   @attributes_request_method@
    --
    --     -   @oidc_issuer@
    --
    --     -   @authorize_scopes@
    --
    --     -   @authorize_url@ /if not available from discovery URL specified
    --         by oidc_issuer key/
    --
    --     -   @token_url@ /if not available from discovery URL specified by
    --         oidc_issuer key/
    --
    --     -   @attributes_url@ /if not available from discovery URL specified
    --         by oidc_issuer key/
    --
    --     -   @jwks_uri@ /if not available from discovery URL specified by
    --         oidc_issuer key/
    --
    -- -   For SAML providers:
    --
    --     -   @MetadataFile@ OR @MetadataURL@
    --
    --     -   @IDPSignout@ /optional/
    identityProviderDetails :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The identity provider name.
    identityProviderName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The identity provider type.
    identityProviderType :: Prelude.Maybe IdentityProviderType,
    -- | The ARN of the identity provider.
    identityProviderArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IdentityProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityProviderDetails', 'identityProvider_identityProviderDetails' - The identity provider details. The following list describes the provider
-- detail keys for each identity provider type.
--
-- -   For Google and Login with Amazon:
--
--     -   @client_id@
--
--     -   @client_secret@
--
--     -   @authorize_scopes@
--
-- -   For Facebook:
--
--     -   @client_id@
--
--     -   @client_secret@
--
--     -   @authorize_scopes@
--
--     -   @api_version@
--
-- -   For Sign in with Apple:
--
--     -   @client_id@
--
--     -   @team_id@
--
--     -   @key_id@
--
--     -   @private_key@
--
--     -   @authorize_scopes@
--
-- -   For OIDC providers:
--
--     -   @client_id@
--
--     -   @client_secret@
--
--     -   @attributes_request_method@
--
--     -   @oidc_issuer@
--
--     -   @authorize_scopes@
--
--     -   @authorize_url@ /if not available from discovery URL specified
--         by oidc_issuer key/
--
--     -   @token_url@ /if not available from discovery URL specified by
--         oidc_issuer key/
--
--     -   @attributes_url@ /if not available from discovery URL specified
--         by oidc_issuer key/
--
--     -   @jwks_uri@ /if not available from discovery URL specified by
--         oidc_issuer key/
--
-- -   For SAML providers:
--
--     -   @MetadataFile@ OR @MetadataURL@
--
--     -   @IDPSignout@ /optional/
--
-- 'identityProviderName', 'identityProvider_identityProviderName' - The identity provider name.
--
-- 'identityProviderType', 'identityProvider_identityProviderType' - The identity provider type.
--
-- 'identityProviderArn', 'identityProvider_identityProviderArn' - The ARN of the identity provider.
newIdentityProvider ::
  -- | 'identityProviderArn'
  Prelude.Text ->
  IdentityProvider
newIdentityProvider pIdentityProviderArn_ =
  IdentityProvider'
    { identityProviderDetails =
        Prelude.Nothing,
      identityProviderName = Prelude.Nothing,
      identityProviderType = Prelude.Nothing,
      identityProviderArn = pIdentityProviderArn_
    }

-- | The identity provider details. The following list describes the provider
-- detail keys for each identity provider type.
--
-- -   For Google and Login with Amazon:
--
--     -   @client_id@
--
--     -   @client_secret@
--
--     -   @authorize_scopes@
--
-- -   For Facebook:
--
--     -   @client_id@
--
--     -   @client_secret@
--
--     -   @authorize_scopes@
--
--     -   @api_version@
--
-- -   For Sign in with Apple:
--
--     -   @client_id@
--
--     -   @team_id@
--
--     -   @key_id@
--
--     -   @private_key@
--
--     -   @authorize_scopes@
--
-- -   For OIDC providers:
--
--     -   @client_id@
--
--     -   @client_secret@
--
--     -   @attributes_request_method@
--
--     -   @oidc_issuer@
--
--     -   @authorize_scopes@
--
--     -   @authorize_url@ /if not available from discovery URL specified
--         by oidc_issuer key/
--
--     -   @token_url@ /if not available from discovery URL specified by
--         oidc_issuer key/
--
--     -   @attributes_url@ /if not available from discovery URL specified
--         by oidc_issuer key/
--
--     -   @jwks_uri@ /if not available from discovery URL specified by
--         oidc_issuer key/
--
-- -   For SAML providers:
--
--     -   @MetadataFile@ OR @MetadataURL@
--
--     -   @IDPSignout@ /optional/
identityProvider_identityProviderDetails :: Lens.Lens' IdentityProvider (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
identityProvider_identityProviderDetails = Lens.lens (\IdentityProvider' {identityProviderDetails} -> identityProviderDetails) (\s@IdentityProvider' {} a -> s {identityProviderDetails = a} :: IdentityProvider) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The identity provider name.
identityProvider_identityProviderName :: Lens.Lens' IdentityProvider (Prelude.Maybe Prelude.Text)
identityProvider_identityProviderName = Lens.lens (\IdentityProvider' {identityProviderName} -> identityProviderName) (\s@IdentityProvider' {} a -> s {identityProviderName = a} :: IdentityProvider) Prelude.. Lens.mapping Data._Sensitive

-- | The identity provider type.
identityProvider_identityProviderType :: Lens.Lens' IdentityProvider (Prelude.Maybe IdentityProviderType)
identityProvider_identityProviderType = Lens.lens (\IdentityProvider' {identityProviderType} -> identityProviderType) (\s@IdentityProvider' {} a -> s {identityProviderType = a} :: IdentityProvider)

-- | The ARN of the identity provider.
identityProvider_identityProviderArn :: Lens.Lens' IdentityProvider Prelude.Text
identityProvider_identityProviderArn = Lens.lens (\IdentityProvider' {identityProviderArn} -> identityProviderArn) (\s@IdentityProvider' {} a -> s {identityProviderArn = a} :: IdentityProvider)

instance Data.FromJSON IdentityProvider where
  parseJSON =
    Data.withObject
      "IdentityProvider"
      ( \x ->
          IdentityProvider'
            Prelude.<$> ( x
                            Data..:? "identityProviderDetails"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "identityProviderName")
            Prelude.<*> (x Data..:? "identityProviderType")
            Prelude.<*> (x Data..: "identityProviderArn")
      )

instance Prelude.Hashable IdentityProvider where
  hashWithSalt _salt IdentityProvider' {..} =
    _salt
      `Prelude.hashWithSalt` identityProviderDetails
      `Prelude.hashWithSalt` identityProviderName
      `Prelude.hashWithSalt` identityProviderType
      `Prelude.hashWithSalt` identityProviderArn

instance Prelude.NFData IdentityProvider where
  rnf IdentityProvider' {..} =
    Prelude.rnf identityProviderDetails
      `Prelude.seq` Prelude.rnf identityProviderName
      `Prelude.seq` Prelude.rnf identityProviderType
      `Prelude.seq` Prelude.rnf identityProviderArn

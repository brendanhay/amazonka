{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WorkSpacesWeb.UpdateIdentityProvider
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the identity provider.
module Amazonka.WorkSpacesWeb.UpdateIdentityProvider
  ( -- * Creating a Request
    UpdateIdentityProvider (..),
    newUpdateIdentityProvider,

    -- * Request Lenses
    updateIdentityProvider_clientToken,
    updateIdentityProvider_identityProviderDetails,
    updateIdentityProvider_identityProviderName,
    updateIdentityProvider_identityProviderType,
    updateIdentityProvider_identityProviderArn,

    -- * Destructuring the Response
    UpdateIdentityProviderResponse (..),
    newUpdateIdentityProviderResponse,

    -- * Response Lenses
    updateIdentityProviderResponse_httpStatus,
    updateIdentityProviderResponse_identityProvider,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newUpdateIdentityProvider' smart constructor.
data UpdateIdentityProvider = UpdateIdentityProvider'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. Idempotency ensures that an API request
    -- completes only once. With an idempotent request, if the original request
    -- completes successfully, subsequent retries with the same client token
    -- return the result from the original successful request.
    --
    -- If you do not specify a client token, one is automatically generated by
    -- the AWS SDK.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The details of the identity provider. The following list describes the
    -- provider detail keys for each identity provider type.
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
    --         by @oidc_issuer@ key/
    --
    --     -   @token_url@ /if not available from discovery URL specified by
    --         @oidc_issuer@ key/
    --
    --     -   @attributes_url@ /if not available from discovery URL specified
    --         by @oidc_issuer@ key/
    --
    --     -   @jwks_uri@ /if not available from discovery URL specified by
    --         @oidc_issuer@ key/
    --
    -- -   For SAML providers:
    --
    --     -   @MetadataFile@ OR @MetadataURL@
    --
    --     -   @IDPSignout@ (boolean) /optional/
    identityProviderDetails :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The name of the identity provider.
    identityProviderName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The type of the identity provider.
    identityProviderType :: Prelude.Maybe IdentityProviderType,
    -- | The ARN of the identity provider.
    identityProviderArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateIdentityProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateIdentityProvider_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. Idempotency ensures that an API request
-- completes only once. With an idempotent request, if the original request
-- completes successfully, subsequent retries with the same client token
-- return the result from the original successful request.
--
-- If you do not specify a client token, one is automatically generated by
-- the AWS SDK.
--
-- 'identityProviderDetails', 'updateIdentityProvider_identityProviderDetails' - The details of the identity provider. The following list describes the
-- provider detail keys for each identity provider type.
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
--         by @oidc_issuer@ key/
--
--     -   @token_url@ /if not available from discovery URL specified by
--         @oidc_issuer@ key/
--
--     -   @attributes_url@ /if not available from discovery URL specified
--         by @oidc_issuer@ key/
--
--     -   @jwks_uri@ /if not available from discovery URL specified by
--         @oidc_issuer@ key/
--
-- -   For SAML providers:
--
--     -   @MetadataFile@ OR @MetadataURL@
--
--     -   @IDPSignout@ (boolean) /optional/
--
-- 'identityProviderName', 'updateIdentityProvider_identityProviderName' - The name of the identity provider.
--
-- 'identityProviderType', 'updateIdentityProvider_identityProviderType' - The type of the identity provider.
--
-- 'identityProviderArn', 'updateIdentityProvider_identityProviderArn' - The ARN of the identity provider.
newUpdateIdentityProvider ::
  -- | 'identityProviderArn'
  Prelude.Text ->
  UpdateIdentityProvider
newUpdateIdentityProvider pIdentityProviderArn_ =
  UpdateIdentityProvider'
    { clientToken =
        Prelude.Nothing,
      identityProviderDetails = Prelude.Nothing,
      identityProviderName = Prelude.Nothing,
      identityProviderType = Prelude.Nothing,
      identityProviderArn = pIdentityProviderArn_
    }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. Idempotency ensures that an API request
-- completes only once. With an idempotent request, if the original request
-- completes successfully, subsequent retries with the same client token
-- return the result from the original successful request.
--
-- If you do not specify a client token, one is automatically generated by
-- the AWS SDK.
updateIdentityProvider_clientToken :: Lens.Lens' UpdateIdentityProvider (Prelude.Maybe Prelude.Text)
updateIdentityProvider_clientToken = Lens.lens (\UpdateIdentityProvider' {clientToken} -> clientToken) (\s@UpdateIdentityProvider' {} a -> s {clientToken = a} :: UpdateIdentityProvider)

-- | The details of the identity provider. The following list describes the
-- provider detail keys for each identity provider type.
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
--         by @oidc_issuer@ key/
--
--     -   @token_url@ /if not available from discovery URL specified by
--         @oidc_issuer@ key/
--
--     -   @attributes_url@ /if not available from discovery URL specified
--         by @oidc_issuer@ key/
--
--     -   @jwks_uri@ /if not available from discovery URL specified by
--         @oidc_issuer@ key/
--
-- -   For SAML providers:
--
--     -   @MetadataFile@ OR @MetadataURL@
--
--     -   @IDPSignout@ (boolean) /optional/
updateIdentityProvider_identityProviderDetails :: Lens.Lens' UpdateIdentityProvider (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateIdentityProvider_identityProviderDetails = Lens.lens (\UpdateIdentityProvider' {identityProviderDetails} -> identityProviderDetails) (\s@UpdateIdentityProvider' {} a -> s {identityProviderDetails = a} :: UpdateIdentityProvider) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The name of the identity provider.
updateIdentityProvider_identityProviderName :: Lens.Lens' UpdateIdentityProvider (Prelude.Maybe Prelude.Text)
updateIdentityProvider_identityProviderName = Lens.lens (\UpdateIdentityProvider' {identityProviderName} -> identityProviderName) (\s@UpdateIdentityProvider' {} a -> s {identityProviderName = a} :: UpdateIdentityProvider) Prelude.. Lens.mapping Data._Sensitive

-- | The type of the identity provider.
updateIdentityProvider_identityProviderType :: Lens.Lens' UpdateIdentityProvider (Prelude.Maybe IdentityProviderType)
updateIdentityProvider_identityProviderType = Lens.lens (\UpdateIdentityProvider' {identityProviderType} -> identityProviderType) (\s@UpdateIdentityProvider' {} a -> s {identityProviderType = a} :: UpdateIdentityProvider)

-- | The ARN of the identity provider.
updateIdentityProvider_identityProviderArn :: Lens.Lens' UpdateIdentityProvider Prelude.Text
updateIdentityProvider_identityProviderArn = Lens.lens (\UpdateIdentityProvider' {identityProviderArn} -> identityProviderArn) (\s@UpdateIdentityProvider' {} a -> s {identityProviderArn = a} :: UpdateIdentityProvider)

instance Core.AWSRequest UpdateIdentityProvider where
  type
    AWSResponse UpdateIdentityProvider =
      UpdateIdentityProviderResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateIdentityProviderResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "identityProvider")
      )

instance Prelude.Hashable UpdateIdentityProvider where
  hashWithSalt _salt UpdateIdentityProvider' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` identityProviderDetails
      `Prelude.hashWithSalt` identityProviderName
      `Prelude.hashWithSalt` identityProviderType
      `Prelude.hashWithSalt` identityProviderArn

instance Prelude.NFData UpdateIdentityProvider where
  rnf UpdateIdentityProvider' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf identityProviderDetails `Prelude.seq`
        Prelude.rnf identityProviderName `Prelude.seq`
          Prelude.rnf identityProviderType `Prelude.seq`
            Prelude.rnf identityProviderArn

instance Data.ToHeaders UpdateIdentityProvider where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateIdentityProvider where
  toJSON UpdateIdentityProvider' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("identityProviderDetails" Data..=)
              Prelude.<$> identityProviderDetails,
            ("identityProviderName" Data..=)
              Prelude.<$> identityProviderName,
            ("identityProviderType" Data..=)
              Prelude.<$> identityProviderType
          ]
      )

instance Data.ToPath UpdateIdentityProvider where
  toPath UpdateIdentityProvider' {..} =
    Prelude.mconcat
      [ "/identityProviders/",
        Data.toBS identityProviderArn
      ]

instance Data.ToQuery UpdateIdentityProvider where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateIdentityProviderResponse' smart constructor.
data UpdateIdentityProviderResponse = UpdateIdentityProviderResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The identity provider.
    identityProvider :: IdentityProvider
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateIdentityProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateIdentityProviderResponse_httpStatus' - The response's http status code.
--
-- 'identityProvider', 'updateIdentityProviderResponse_identityProvider' - The identity provider.
newUpdateIdentityProviderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'identityProvider'
  IdentityProvider ->
  UpdateIdentityProviderResponse
newUpdateIdentityProviderResponse
  pHttpStatus_
  pIdentityProvider_ =
    UpdateIdentityProviderResponse'
      { httpStatus =
          pHttpStatus_,
        identityProvider = pIdentityProvider_
      }

-- | The response's http status code.
updateIdentityProviderResponse_httpStatus :: Lens.Lens' UpdateIdentityProviderResponse Prelude.Int
updateIdentityProviderResponse_httpStatus = Lens.lens (\UpdateIdentityProviderResponse' {httpStatus} -> httpStatus) (\s@UpdateIdentityProviderResponse' {} a -> s {httpStatus = a} :: UpdateIdentityProviderResponse)

-- | The identity provider.
updateIdentityProviderResponse_identityProvider :: Lens.Lens' UpdateIdentityProviderResponse IdentityProvider
updateIdentityProviderResponse_identityProvider = Lens.lens (\UpdateIdentityProviderResponse' {identityProvider} -> identityProvider) (\s@UpdateIdentityProviderResponse' {} a -> s {identityProvider = a} :: UpdateIdentityProviderResponse)

instance
  Prelude.NFData
    UpdateIdentityProviderResponse
  where
  rnf UpdateIdentityProviderResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf identityProvider

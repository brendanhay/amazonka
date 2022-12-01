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
-- Module      : Amazonka.CognitoIdentityProvider.CreateIdentityProvider
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an IdP for a user pool.
module Amazonka.CognitoIdentityProvider.CreateIdentityProvider
  ( -- * Creating a Request
    CreateIdentityProvider (..),
    newCreateIdentityProvider,

    -- * Request Lenses
    createIdentityProvider_attributeMapping,
    createIdentityProvider_idpIdentifiers,
    createIdentityProvider_userPoolId,
    createIdentityProvider_providerName,
    createIdentityProvider_providerType,
    createIdentityProvider_providerDetails,

    -- * Destructuring the Response
    CreateIdentityProviderResponse (..),
    newCreateIdentityProviderResponse,

    -- * Response Lenses
    createIdentityProviderResponse_httpStatus,
    createIdentityProviderResponse_identityProvider,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateIdentityProvider' smart constructor.
data CreateIdentityProvider = CreateIdentityProvider'
  { -- | A mapping of IdP attributes to standard and custom user pool attributes.
    attributeMapping :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of IdP identifiers.
    idpIdentifiers :: Prelude.Maybe [Prelude.Text],
    -- | The user pool ID.
    userPoolId :: Prelude.Text,
    -- | The IdP name.
    providerName :: Prelude.Text,
    -- | The IdP type.
    providerType :: IdentityProviderTypeType,
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
    --     -   authorize_scopes
    --
    -- -   For OpenID Connect (OIDC) providers:
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
    providerDetails :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateIdentityProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeMapping', 'createIdentityProvider_attributeMapping' - A mapping of IdP attributes to standard and custom user pool attributes.
--
-- 'idpIdentifiers', 'createIdentityProvider_idpIdentifiers' - A list of IdP identifiers.
--
-- 'userPoolId', 'createIdentityProvider_userPoolId' - The user pool ID.
--
-- 'providerName', 'createIdentityProvider_providerName' - The IdP name.
--
-- 'providerType', 'createIdentityProvider_providerType' - The IdP type.
--
-- 'providerDetails', 'createIdentityProvider_providerDetails' - The IdP details. The following list describes the provider detail keys
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
--     -   authorize_scopes
--
-- -   For OpenID Connect (OIDC) providers:
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
newCreateIdentityProvider ::
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'providerName'
  Prelude.Text ->
  -- | 'providerType'
  IdentityProviderTypeType ->
  CreateIdentityProvider
newCreateIdentityProvider
  pUserPoolId_
  pProviderName_
  pProviderType_ =
    CreateIdentityProvider'
      { attributeMapping =
          Prelude.Nothing,
        idpIdentifiers = Prelude.Nothing,
        userPoolId = pUserPoolId_,
        providerName = pProviderName_,
        providerType = pProviderType_,
        providerDetails = Prelude.mempty
      }

-- | A mapping of IdP attributes to standard and custom user pool attributes.
createIdentityProvider_attributeMapping :: Lens.Lens' CreateIdentityProvider (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createIdentityProvider_attributeMapping = Lens.lens (\CreateIdentityProvider' {attributeMapping} -> attributeMapping) (\s@CreateIdentityProvider' {} a -> s {attributeMapping = a} :: CreateIdentityProvider) Prelude.. Lens.mapping Lens.coerced

-- | A list of IdP identifiers.
createIdentityProvider_idpIdentifiers :: Lens.Lens' CreateIdentityProvider (Prelude.Maybe [Prelude.Text])
createIdentityProvider_idpIdentifiers = Lens.lens (\CreateIdentityProvider' {idpIdentifiers} -> idpIdentifiers) (\s@CreateIdentityProvider' {} a -> s {idpIdentifiers = a} :: CreateIdentityProvider) Prelude.. Lens.mapping Lens.coerced

-- | The user pool ID.
createIdentityProvider_userPoolId :: Lens.Lens' CreateIdentityProvider Prelude.Text
createIdentityProvider_userPoolId = Lens.lens (\CreateIdentityProvider' {userPoolId} -> userPoolId) (\s@CreateIdentityProvider' {} a -> s {userPoolId = a} :: CreateIdentityProvider)

-- | The IdP name.
createIdentityProvider_providerName :: Lens.Lens' CreateIdentityProvider Prelude.Text
createIdentityProvider_providerName = Lens.lens (\CreateIdentityProvider' {providerName} -> providerName) (\s@CreateIdentityProvider' {} a -> s {providerName = a} :: CreateIdentityProvider)

-- | The IdP type.
createIdentityProvider_providerType :: Lens.Lens' CreateIdentityProvider IdentityProviderTypeType
createIdentityProvider_providerType = Lens.lens (\CreateIdentityProvider' {providerType} -> providerType) (\s@CreateIdentityProvider' {} a -> s {providerType = a} :: CreateIdentityProvider)

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
--     -   authorize_scopes
--
-- -   For OpenID Connect (OIDC) providers:
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
createIdentityProvider_providerDetails :: Lens.Lens' CreateIdentityProvider (Prelude.HashMap Prelude.Text Prelude.Text)
createIdentityProvider_providerDetails = Lens.lens (\CreateIdentityProvider' {providerDetails} -> providerDetails) (\s@CreateIdentityProvider' {} a -> s {providerDetails = a} :: CreateIdentityProvider) Prelude.. Lens.coerced

instance Core.AWSRequest CreateIdentityProvider where
  type
    AWSResponse CreateIdentityProvider =
      CreateIdentityProviderResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateIdentityProviderResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "IdentityProvider")
      )

instance Prelude.Hashable CreateIdentityProvider where
  hashWithSalt _salt CreateIdentityProvider' {..} =
    _salt `Prelude.hashWithSalt` attributeMapping
      `Prelude.hashWithSalt` idpIdentifiers
      `Prelude.hashWithSalt` userPoolId
      `Prelude.hashWithSalt` providerName
      `Prelude.hashWithSalt` providerType
      `Prelude.hashWithSalt` providerDetails

instance Prelude.NFData CreateIdentityProvider where
  rnf CreateIdentityProvider' {..} =
    Prelude.rnf attributeMapping
      `Prelude.seq` Prelude.rnf idpIdentifiers
      `Prelude.seq` Prelude.rnf userPoolId
      `Prelude.seq` Prelude.rnf providerName
      `Prelude.seq` Prelude.rnf providerType
      `Prelude.seq` Prelude.rnf providerDetails

instance Core.ToHeaders CreateIdentityProvider where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.CreateIdentityProvider" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateIdentityProvider where
  toJSON CreateIdentityProvider' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AttributeMapping" Core..=)
              Prelude.<$> attributeMapping,
            ("IdpIdentifiers" Core..=)
              Prelude.<$> idpIdentifiers,
            Prelude.Just ("UserPoolId" Core..= userPoolId),
            Prelude.Just ("ProviderName" Core..= providerName),
            Prelude.Just ("ProviderType" Core..= providerType),
            Prelude.Just
              ("ProviderDetails" Core..= providerDetails)
          ]
      )

instance Core.ToPath CreateIdentityProvider where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateIdentityProvider where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateIdentityProviderResponse' smart constructor.
data CreateIdentityProviderResponse = CreateIdentityProviderResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The newly created IdP object.
    identityProvider :: IdentityProviderType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateIdentityProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createIdentityProviderResponse_httpStatus' - The response's http status code.
--
-- 'identityProvider', 'createIdentityProviderResponse_identityProvider' - The newly created IdP object.
newCreateIdentityProviderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'identityProvider'
  IdentityProviderType ->
  CreateIdentityProviderResponse
newCreateIdentityProviderResponse
  pHttpStatus_
  pIdentityProvider_ =
    CreateIdentityProviderResponse'
      { httpStatus =
          pHttpStatus_,
        identityProvider = pIdentityProvider_
      }

-- | The response's http status code.
createIdentityProviderResponse_httpStatus :: Lens.Lens' CreateIdentityProviderResponse Prelude.Int
createIdentityProviderResponse_httpStatus = Lens.lens (\CreateIdentityProviderResponse' {httpStatus} -> httpStatus) (\s@CreateIdentityProviderResponse' {} a -> s {httpStatus = a} :: CreateIdentityProviderResponse)

-- | The newly created IdP object.
createIdentityProviderResponse_identityProvider :: Lens.Lens' CreateIdentityProviderResponse IdentityProviderType
createIdentityProviderResponse_identityProvider = Lens.lens (\CreateIdentityProviderResponse' {identityProvider} -> identityProvider) (\s@CreateIdentityProviderResponse' {} a -> s {identityProvider = a} :: CreateIdentityProviderResponse)

instance
  Prelude.NFData
    CreateIdentityProviderResponse
  where
  rnf CreateIdentityProviderResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf identityProvider

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
-- Module      : Network.AWS.CognitoIdentityProvider.CreateIdentityProvider
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an identity provider for a user pool.
module Network.AWS.CognitoIdentityProvider.CreateIdentityProvider
  ( -- * Creating a Request
    CreateIdentityProvider (..),
    newCreateIdentityProvider,

    -- * Request Lenses
    createIdentityProvider_idpIdentifiers,
    createIdentityProvider_attributeMapping,
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

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateIdentityProvider' smart constructor.
data CreateIdentityProvider = CreateIdentityProvider'
  { -- | A list of identity provider identifiers.
    idpIdentifiers :: Core.Maybe [Core.Text],
    -- | A mapping of identity provider attributes to standard and custom user
    -- pool attributes.
    attributeMapping :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The user pool ID.
    userPoolId :: Core.Text,
    -- | The identity provider name.
    providerName :: Core.Text,
    -- | The identity provider type.
    providerType :: IdentityProviderTypeType,
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
    -- -   For SAML providers:
    --
    --     -   MetadataFile OR MetadataURL
    --
    --     -   IDPSignout /optional/
    providerDetails :: Core.HashMap Core.Text Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateIdentityProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'idpIdentifiers', 'createIdentityProvider_idpIdentifiers' - A list of identity provider identifiers.
--
-- 'attributeMapping', 'createIdentityProvider_attributeMapping' - A mapping of identity provider attributes to standard and custom user
-- pool attributes.
--
-- 'userPoolId', 'createIdentityProvider_userPoolId' - The user pool ID.
--
-- 'providerName', 'createIdentityProvider_providerName' - The identity provider name.
--
-- 'providerType', 'createIdentityProvider_providerType' - The identity provider type.
--
-- 'providerDetails', 'createIdentityProvider_providerDetails' - The identity provider details. The following list describes the provider
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
-- -   For SAML providers:
--
--     -   MetadataFile OR MetadataURL
--
--     -   IDPSignout /optional/
newCreateIdentityProvider ::
  -- | 'userPoolId'
  Core.Text ->
  -- | 'providerName'
  Core.Text ->
  -- | 'providerType'
  IdentityProviderTypeType ->
  CreateIdentityProvider
newCreateIdentityProvider
  pUserPoolId_
  pProviderName_
  pProviderType_ =
    CreateIdentityProvider'
      { idpIdentifiers =
          Core.Nothing,
        attributeMapping = Core.Nothing,
        userPoolId = pUserPoolId_,
        providerName = pProviderName_,
        providerType = pProviderType_,
        providerDetails = Core.mempty
      }

-- | A list of identity provider identifiers.
createIdentityProvider_idpIdentifiers :: Lens.Lens' CreateIdentityProvider (Core.Maybe [Core.Text])
createIdentityProvider_idpIdentifiers = Lens.lens (\CreateIdentityProvider' {idpIdentifiers} -> idpIdentifiers) (\s@CreateIdentityProvider' {} a -> s {idpIdentifiers = a} :: CreateIdentityProvider) Core.. Lens.mapping Lens._Coerce

-- | A mapping of identity provider attributes to standard and custom user
-- pool attributes.
createIdentityProvider_attributeMapping :: Lens.Lens' CreateIdentityProvider (Core.Maybe (Core.HashMap Core.Text Core.Text))
createIdentityProvider_attributeMapping = Lens.lens (\CreateIdentityProvider' {attributeMapping} -> attributeMapping) (\s@CreateIdentityProvider' {} a -> s {attributeMapping = a} :: CreateIdentityProvider) Core.. Lens.mapping Lens._Coerce

-- | The user pool ID.
createIdentityProvider_userPoolId :: Lens.Lens' CreateIdentityProvider Core.Text
createIdentityProvider_userPoolId = Lens.lens (\CreateIdentityProvider' {userPoolId} -> userPoolId) (\s@CreateIdentityProvider' {} a -> s {userPoolId = a} :: CreateIdentityProvider)

-- | The identity provider name.
createIdentityProvider_providerName :: Lens.Lens' CreateIdentityProvider Core.Text
createIdentityProvider_providerName = Lens.lens (\CreateIdentityProvider' {providerName} -> providerName) (\s@CreateIdentityProvider' {} a -> s {providerName = a} :: CreateIdentityProvider)

-- | The identity provider type.
createIdentityProvider_providerType :: Lens.Lens' CreateIdentityProvider IdentityProviderTypeType
createIdentityProvider_providerType = Lens.lens (\CreateIdentityProvider' {providerType} -> providerType) (\s@CreateIdentityProvider' {} a -> s {providerType = a} :: CreateIdentityProvider)

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
-- -   For SAML providers:
--
--     -   MetadataFile OR MetadataURL
--
--     -   IDPSignout /optional/
createIdentityProvider_providerDetails :: Lens.Lens' CreateIdentityProvider (Core.HashMap Core.Text Core.Text)
createIdentityProvider_providerDetails = Lens.lens (\CreateIdentityProvider' {providerDetails} -> providerDetails) (\s@CreateIdentityProvider' {} a -> s {providerDetails = a} :: CreateIdentityProvider) Core.. Lens._Coerce

instance Core.AWSRequest CreateIdentityProvider where
  type
    AWSResponse CreateIdentityProvider =
      CreateIdentityProviderResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateIdentityProviderResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "IdentityProvider")
      )

instance Core.Hashable CreateIdentityProvider

instance Core.NFData CreateIdentityProvider

instance Core.ToHeaders CreateIdentityProvider where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.CreateIdentityProvider" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateIdentityProvider where
  toJSON CreateIdentityProvider' {..} =
    Core.object
      ( Core.catMaybes
          [ ("IdpIdentifiers" Core..=) Core.<$> idpIdentifiers,
            ("AttributeMapping" Core..=)
              Core.<$> attributeMapping,
            Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("ProviderName" Core..= providerName),
            Core.Just ("ProviderType" Core..= providerType),
            Core.Just
              ("ProviderDetails" Core..= providerDetails)
          ]
      )

instance Core.ToPath CreateIdentityProvider where
  toPath = Core.const "/"

instance Core.ToQuery CreateIdentityProvider where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateIdentityProviderResponse' smart constructor.
data CreateIdentityProviderResponse = CreateIdentityProviderResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The newly created identity provider object.
    identityProvider :: IdentityProviderType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'identityProvider', 'createIdentityProviderResponse_identityProvider' - The newly created identity provider object.
newCreateIdentityProviderResponse ::
  -- | 'httpStatus'
  Core.Int ->
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
createIdentityProviderResponse_httpStatus :: Lens.Lens' CreateIdentityProviderResponse Core.Int
createIdentityProviderResponse_httpStatus = Lens.lens (\CreateIdentityProviderResponse' {httpStatus} -> httpStatus) (\s@CreateIdentityProviderResponse' {} a -> s {httpStatus = a} :: CreateIdentityProviderResponse)

-- | The newly created identity provider object.
createIdentityProviderResponse_identityProvider :: Lens.Lens' CreateIdentityProviderResponse IdentityProviderType
createIdentityProviderResponse_identityProvider = Lens.lens (\CreateIdentityProviderResponse' {identityProvider} -> identityProvider) (\s@CreateIdentityProviderResponse' {} a -> s {identityProvider = a} :: CreateIdentityProviderResponse)

instance Core.NFData CreateIdentityProviderResponse

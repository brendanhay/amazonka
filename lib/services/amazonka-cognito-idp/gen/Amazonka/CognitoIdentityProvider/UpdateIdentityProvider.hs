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
-- Module      : Amazonka.CognitoIdentityProvider.UpdateIdentityProvider
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates IdP information for a user pool.
module Amazonka.CognitoIdentityProvider.UpdateIdentityProvider
  ( -- * Creating a Request
    UpdateIdentityProvider (..),
    newUpdateIdentityProvider,

    -- * Request Lenses
    updateIdentityProvider_attributeMapping,
    updateIdentityProvider_idpIdentifiers,
    updateIdentityProvider_providerDetails,
    updateIdentityProvider_userPoolId,
    updateIdentityProvider_providerName,

    -- * Destructuring the Response
    UpdateIdentityProviderResponse (..),
    newUpdateIdentityProviderResponse,

    -- * Response Lenses
    updateIdentityProviderResponse_httpStatus,
    updateIdentityProviderResponse_identityProvider,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateIdentityProvider' smart constructor.
data UpdateIdentityProvider = UpdateIdentityProvider'
  { -- | The IdP attribute mapping to be changed.
    attributeMapping :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of IdP identifiers.
    idpIdentifiers :: Prelude.Maybe [Prelude.Text],
    -- | The IdP details to be updated, such as @MetadataURL@ and @MetadataFile@.
    providerDetails :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The user pool ID.
    userPoolId :: Prelude.Text,
    -- | The IdP name.
    providerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateIdentityProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeMapping', 'updateIdentityProvider_attributeMapping' - The IdP attribute mapping to be changed.
--
-- 'idpIdentifiers', 'updateIdentityProvider_idpIdentifiers' - A list of IdP identifiers.
--
-- 'providerDetails', 'updateIdentityProvider_providerDetails' - The IdP details to be updated, such as @MetadataURL@ and @MetadataFile@.
--
-- 'userPoolId', 'updateIdentityProvider_userPoolId' - The user pool ID.
--
-- 'providerName', 'updateIdentityProvider_providerName' - The IdP name.
newUpdateIdentityProvider ::
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'providerName'
  Prelude.Text ->
  UpdateIdentityProvider
newUpdateIdentityProvider pUserPoolId_ pProviderName_ =
  UpdateIdentityProvider'
    { attributeMapping =
        Prelude.Nothing,
      idpIdentifiers = Prelude.Nothing,
      providerDetails = Prelude.Nothing,
      userPoolId = pUserPoolId_,
      providerName = pProviderName_
    }

-- | The IdP attribute mapping to be changed.
updateIdentityProvider_attributeMapping :: Lens.Lens' UpdateIdentityProvider (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateIdentityProvider_attributeMapping = Lens.lens (\UpdateIdentityProvider' {attributeMapping} -> attributeMapping) (\s@UpdateIdentityProvider' {} a -> s {attributeMapping = a} :: UpdateIdentityProvider) Prelude.. Lens.mapping Lens.coerced

-- | A list of IdP identifiers.
updateIdentityProvider_idpIdentifiers :: Lens.Lens' UpdateIdentityProvider (Prelude.Maybe [Prelude.Text])
updateIdentityProvider_idpIdentifiers = Lens.lens (\UpdateIdentityProvider' {idpIdentifiers} -> idpIdentifiers) (\s@UpdateIdentityProvider' {} a -> s {idpIdentifiers = a} :: UpdateIdentityProvider) Prelude.. Lens.mapping Lens.coerced

-- | The IdP details to be updated, such as @MetadataURL@ and @MetadataFile@.
updateIdentityProvider_providerDetails :: Lens.Lens' UpdateIdentityProvider (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateIdentityProvider_providerDetails = Lens.lens (\UpdateIdentityProvider' {providerDetails} -> providerDetails) (\s@UpdateIdentityProvider' {} a -> s {providerDetails = a} :: UpdateIdentityProvider) Prelude.. Lens.mapping Lens.coerced

-- | The user pool ID.
updateIdentityProvider_userPoolId :: Lens.Lens' UpdateIdentityProvider Prelude.Text
updateIdentityProvider_userPoolId = Lens.lens (\UpdateIdentityProvider' {userPoolId} -> userPoolId) (\s@UpdateIdentityProvider' {} a -> s {userPoolId = a} :: UpdateIdentityProvider)

-- | The IdP name.
updateIdentityProvider_providerName :: Lens.Lens' UpdateIdentityProvider Prelude.Text
updateIdentityProvider_providerName = Lens.lens (\UpdateIdentityProvider' {providerName} -> providerName) (\s@UpdateIdentityProvider' {} a -> s {providerName = a} :: UpdateIdentityProvider)

instance Core.AWSRequest UpdateIdentityProvider where
  type
    AWSResponse UpdateIdentityProvider =
      UpdateIdentityProviderResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateIdentityProviderResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "IdentityProvider")
      )

instance Prelude.Hashable UpdateIdentityProvider where
  hashWithSalt _salt UpdateIdentityProvider' {..} =
    _salt
      `Prelude.hashWithSalt` attributeMapping
      `Prelude.hashWithSalt` idpIdentifiers
      `Prelude.hashWithSalt` providerDetails
      `Prelude.hashWithSalt` userPoolId
      `Prelude.hashWithSalt` providerName

instance Prelude.NFData UpdateIdentityProvider where
  rnf UpdateIdentityProvider' {..} =
    Prelude.rnf attributeMapping `Prelude.seq`
      Prelude.rnf idpIdentifiers `Prelude.seq`
        Prelude.rnf providerDetails `Prelude.seq`
          Prelude.rnf userPoolId `Prelude.seq`
            Prelude.rnf providerName

instance Data.ToHeaders UpdateIdentityProvider where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.UpdateIdentityProvider" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateIdentityProvider where
  toJSON UpdateIdentityProvider' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AttributeMapping" Data..=)
              Prelude.<$> attributeMapping,
            ("IdpIdentifiers" Data..=)
              Prelude.<$> idpIdentifiers,
            ("ProviderDetails" Data..=)
              Prelude.<$> providerDetails,
            Prelude.Just ("UserPoolId" Data..= userPoolId),
            Prelude.Just ("ProviderName" Data..= providerName)
          ]
      )

instance Data.ToPath UpdateIdentityProvider where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateIdentityProvider where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateIdentityProviderResponse' smart constructor.
data UpdateIdentityProviderResponse = UpdateIdentityProviderResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The identity provider details.
    identityProvider :: IdentityProviderType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'identityProvider', 'updateIdentityProviderResponse_identityProvider' - The identity provider details.
newUpdateIdentityProviderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'identityProvider'
  IdentityProviderType ->
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

-- | The identity provider details.
updateIdentityProviderResponse_identityProvider :: Lens.Lens' UpdateIdentityProviderResponse IdentityProviderType
updateIdentityProviderResponse_identityProvider = Lens.lens (\UpdateIdentityProviderResponse' {identityProvider} -> identityProvider) (\s@UpdateIdentityProviderResponse' {} a -> s {identityProvider = a} :: UpdateIdentityProviderResponse)

instance
  Prelude.NFData
    UpdateIdentityProviderResponse
  where
  rnf UpdateIdentityProviderResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf identityProvider

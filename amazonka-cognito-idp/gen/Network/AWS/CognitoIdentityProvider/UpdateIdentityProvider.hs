{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CognitoIdentityProvider.UpdateIdentityProvider
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates identity provider information for a user pool.
module Network.AWS.CognitoIdentityProvider.UpdateIdentityProvider
  ( -- * Creating a Request
    UpdateIdentityProvider (..),
    newUpdateIdentityProvider,

    -- * Request Lenses
    updateIdentityProvider_providerDetails,
    updateIdentityProvider_idpIdentifiers,
    updateIdentityProvider_attributeMapping,
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

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateIdentityProvider' smart constructor.
data UpdateIdentityProvider = UpdateIdentityProvider'
  { -- | The identity provider details to be updated, such as @MetadataURL@ and
    -- @MetadataFile@.
    providerDetails :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of identity provider identifiers.
    idpIdentifiers :: Prelude.Maybe [Prelude.Text],
    -- | The identity provider attribute mapping to be changed.
    attributeMapping :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The user pool ID.
    userPoolId :: Prelude.Text,
    -- | The identity provider name.
    providerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateIdentityProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'providerDetails', 'updateIdentityProvider_providerDetails' - The identity provider details to be updated, such as @MetadataURL@ and
-- @MetadataFile@.
--
-- 'idpIdentifiers', 'updateIdentityProvider_idpIdentifiers' - A list of identity provider identifiers.
--
-- 'attributeMapping', 'updateIdentityProvider_attributeMapping' - The identity provider attribute mapping to be changed.
--
-- 'userPoolId', 'updateIdentityProvider_userPoolId' - The user pool ID.
--
-- 'providerName', 'updateIdentityProvider_providerName' - The identity provider name.
newUpdateIdentityProvider ::
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'providerName'
  Prelude.Text ->
  UpdateIdentityProvider
newUpdateIdentityProvider pUserPoolId_ pProviderName_ =
  UpdateIdentityProvider'
    { providerDetails =
        Prelude.Nothing,
      idpIdentifiers = Prelude.Nothing,
      attributeMapping = Prelude.Nothing,
      userPoolId = pUserPoolId_,
      providerName = pProviderName_
    }

-- | The identity provider details to be updated, such as @MetadataURL@ and
-- @MetadataFile@.
updateIdentityProvider_providerDetails :: Lens.Lens' UpdateIdentityProvider (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateIdentityProvider_providerDetails = Lens.lens (\UpdateIdentityProvider' {providerDetails} -> providerDetails) (\s@UpdateIdentityProvider' {} a -> s {providerDetails = a} :: UpdateIdentityProvider) Prelude.. Lens.mapping Prelude._Coerce

-- | A list of identity provider identifiers.
updateIdentityProvider_idpIdentifiers :: Lens.Lens' UpdateIdentityProvider (Prelude.Maybe [Prelude.Text])
updateIdentityProvider_idpIdentifiers = Lens.lens (\UpdateIdentityProvider' {idpIdentifiers} -> idpIdentifiers) (\s@UpdateIdentityProvider' {} a -> s {idpIdentifiers = a} :: UpdateIdentityProvider) Prelude.. Lens.mapping Prelude._Coerce

-- | The identity provider attribute mapping to be changed.
updateIdentityProvider_attributeMapping :: Lens.Lens' UpdateIdentityProvider (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateIdentityProvider_attributeMapping = Lens.lens (\UpdateIdentityProvider' {attributeMapping} -> attributeMapping) (\s@UpdateIdentityProvider' {} a -> s {attributeMapping = a} :: UpdateIdentityProvider) Prelude.. Lens.mapping Prelude._Coerce

-- | The user pool ID.
updateIdentityProvider_userPoolId :: Lens.Lens' UpdateIdentityProvider Prelude.Text
updateIdentityProvider_userPoolId = Lens.lens (\UpdateIdentityProvider' {userPoolId} -> userPoolId) (\s@UpdateIdentityProvider' {} a -> s {userPoolId = a} :: UpdateIdentityProvider)

-- | The identity provider name.
updateIdentityProvider_providerName :: Lens.Lens' UpdateIdentityProvider Prelude.Text
updateIdentityProvider_providerName = Lens.lens (\UpdateIdentityProvider' {providerName} -> providerName) (\s@UpdateIdentityProvider' {} a -> s {providerName = a} :: UpdateIdentityProvider)

instance Prelude.AWSRequest UpdateIdentityProvider where
  type
    Rs UpdateIdentityProvider =
      UpdateIdentityProviderResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateIdentityProviderResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "IdentityProvider")
      )

instance Prelude.Hashable UpdateIdentityProvider

instance Prelude.NFData UpdateIdentityProvider

instance Prelude.ToHeaders UpdateIdentityProvider where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.UpdateIdentityProvider" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateIdentityProvider where
  toJSON UpdateIdentityProvider' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ProviderDetails" Prelude..=)
              Prelude.<$> providerDetails,
            ("IdpIdentifiers" Prelude..=)
              Prelude.<$> idpIdentifiers,
            ("AttributeMapping" Prelude..=)
              Prelude.<$> attributeMapping,
            Prelude.Just ("UserPoolId" Prelude..= userPoolId),
            Prelude.Just
              ("ProviderName" Prelude..= providerName)
          ]
      )

instance Prelude.ToPath UpdateIdentityProvider where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateIdentityProvider where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateIdentityProviderResponse' smart constructor.
data UpdateIdentityProviderResponse = UpdateIdentityProviderResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The identity provider object.
    identityProvider :: IdentityProviderType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'identityProvider', 'updateIdentityProviderResponse_identityProvider' - The identity provider object.
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

-- | The identity provider object.
updateIdentityProviderResponse_identityProvider :: Lens.Lens' UpdateIdentityProviderResponse IdentityProviderType
updateIdentityProviderResponse_identityProvider = Lens.lens (\UpdateIdentityProviderResponse' {identityProvider} -> identityProvider) (\s@UpdateIdentityProviderResponse' {} a -> s {identityProvider = a} :: UpdateIdentityProviderResponse)

instance
  Prelude.NFData
    UpdateIdentityProviderResponse

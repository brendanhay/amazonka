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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateIdentityProvider' smart constructor.
data UpdateIdentityProvider = UpdateIdentityProvider'
  { -- | The identity provider details to be updated, such as @MetadataURL@ and
    -- @MetadataFile@.
    providerDetails :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A list of identity provider identifiers.
    idpIdentifiers :: Core.Maybe [Core.Text],
    -- | The identity provider attribute mapping to be changed.
    attributeMapping :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The user pool ID.
    userPoolId :: Core.Text,
    -- | The identity provider name.
    providerName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'providerName'
  Core.Text ->
  UpdateIdentityProvider
newUpdateIdentityProvider pUserPoolId_ pProviderName_ =
  UpdateIdentityProvider'
    { providerDetails =
        Core.Nothing,
      idpIdentifiers = Core.Nothing,
      attributeMapping = Core.Nothing,
      userPoolId = pUserPoolId_,
      providerName = pProviderName_
    }

-- | The identity provider details to be updated, such as @MetadataURL@ and
-- @MetadataFile@.
updateIdentityProvider_providerDetails :: Lens.Lens' UpdateIdentityProvider (Core.Maybe (Core.HashMap Core.Text Core.Text))
updateIdentityProvider_providerDetails = Lens.lens (\UpdateIdentityProvider' {providerDetails} -> providerDetails) (\s@UpdateIdentityProvider' {} a -> s {providerDetails = a} :: UpdateIdentityProvider) Core.. Lens.mapping Lens._Coerce

-- | A list of identity provider identifiers.
updateIdentityProvider_idpIdentifiers :: Lens.Lens' UpdateIdentityProvider (Core.Maybe [Core.Text])
updateIdentityProvider_idpIdentifiers = Lens.lens (\UpdateIdentityProvider' {idpIdentifiers} -> idpIdentifiers) (\s@UpdateIdentityProvider' {} a -> s {idpIdentifiers = a} :: UpdateIdentityProvider) Core.. Lens.mapping Lens._Coerce

-- | The identity provider attribute mapping to be changed.
updateIdentityProvider_attributeMapping :: Lens.Lens' UpdateIdentityProvider (Core.Maybe (Core.HashMap Core.Text Core.Text))
updateIdentityProvider_attributeMapping = Lens.lens (\UpdateIdentityProvider' {attributeMapping} -> attributeMapping) (\s@UpdateIdentityProvider' {} a -> s {attributeMapping = a} :: UpdateIdentityProvider) Core.. Lens.mapping Lens._Coerce

-- | The user pool ID.
updateIdentityProvider_userPoolId :: Lens.Lens' UpdateIdentityProvider Core.Text
updateIdentityProvider_userPoolId = Lens.lens (\UpdateIdentityProvider' {userPoolId} -> userPoolId) (\s@UpdateIdentityProvider' {} a -> s {userPoolId = a} :: UpdateIdentityProvider)

-- | The identity provider name.
updateIdentityProvider_providerName :: Lens.Lens' UpdateIdentityProvider Core.Text
updateIdentityProvider_providerName = Lens.lens (\UpdateIdentityProvider' {providerName} -> providerName) (\s@UpdateIdentityProvider' {} a -> s {providerName = a} :: UpdateIdentityProvider)

instance Core.AWSRequest UpdateIdentityProvider where
  type
    AWSResponse UpdateIdentityProvider =
      UpdateIdentityProviderResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateIdentityProviderResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "IdentityProvider")
      )

instance Core.Hashable UpdateIdentityProvider

instance Core.NFData UpdateIdentityProvider

instance Core.ToHeaders UpdateIdentityProvider where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.UpdateIdentityProvider" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateIdentityProvider where
  toJSON UpdateIdentityProvider' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ProviderDetails" Core..=)
              Core.<$> providerDetails,
            ("IdpIdentifiers" Core..=) Core.<$> idpIdentifiers,
            ("AttributeMapping" Core..=)
              Core.<$> attributeMapping,
            Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("ProviderName" Core..= providerName)
          ]
      )

instance Core.ToPath UpdateIdentityProvider where
  toPath = Core.const "/"

instance Core.ToQuery UpdateIdentityProvider where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateIdentityProviderResponse' smart constructor.
data UpdateIdentityProviderResponse = UpdateIdentityProviderResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The identity provider object.
    identityProvider :: IdentityProviderType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
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
updateIdentityProviderResponse_httpStatus :: Lens.Lens' UpdateIdentityProviderResponse Core.Int
updateIdentityProviderResponse_httpStatus = Lens.lens (\UpdateIdentityProviderResponse' {httpStatus} -> httpStatus) (\s@UpdateIdentityProviderResponse' {} a -> s {httpStatus = a} :: UpdateIdentityProviderResponse)

-- | The identity provider object.
updateIdentityProviderResponse_identityProvider :: Lens.Lens' UpdateIdentityProviderResponse IdentityProviderType
updateIdentityProviderResponse_identityProvider = Lens.lens (\UpdateIdentityProviderResponse' {identityProvider} -> identityProvider) (\s@UpdateIdentityProviderResponse' {} a -> s {identityProvider = a} :: UpdateIdentityProviderResponse)

instance Core.NFData UpdateIdentityProviderResponse

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
-- Module      : Amazonka.LicenseManagerUserSubscriptions.UpdateIdentityProviderSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates additional product configuration settings for the registered
-- identity provider.
module Amazonka.LicenseManagerUserSubscriptions.UpdateIdentityProviderSettings
  ( -- * Creating a Request
    UpdateIdentityProviderSettings (..),
    newUpdateIdentityProviderSettings,

    -- * Request Lenses
    updateIdentityProviderSettings_identityProvider,
    updateIdentityProviderSettings_product,
    updateIdentityProviderSettings_updateSettings,

    -- * Destructuring the Response
    UpdateIdentityProviderSettingsResponse (..),
    newUpdateIdentityProviderSettingsResponse,

    -- * Response Lenses
    updateIdentityProviderSettingsResponse_httpStatus,
    updateIdentityProviderSettingsResponse_identityProviderSummary,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManagerUserSubscriptions.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateIdentityProviderSettings' smart constructor.
data UpdateIdentityProviderSettings = UpdateIdentityProviderSettings'
  { identityProvider :: IdentityProvider,
    -- | The name of the user-based subscription product.
    product :: Prelude.Text,
    -- | Updates the registered identity provider’s product related configuration
    -- settings. You can update any combination of settings in a single
    -- operation such as the:
    --
    -- -   Subnets which you want to add to provision VPC endpoints.
    --
    -- -   Subnets which you want to remove the VPC endpoints from.
    --
    -- -   Security group ID which permits traffic to the VPC endpoints.
    updateSettings :: UpdateSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateIdentityProviderSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityProvider', 'updateIdentityProviderSettings_identityProvider' - Undocumented member.
--
-- 'product', 'updateIdentityProviderSettings_product' - The name of the user-based subscription product.
--
-- 'updateSettings', 'updateIdentityProviderSettings_updateSettings' - Updates the registered identity provider’s product related configuration
-- settings. You can update any combination of settings in a single
-- operation such as the:
--
-- -   Subnets which you want to add to provision VPC endpoints.
--
-- -   Subnets which you want to remove the VPC endpoints from.
--
-- -   Security group ID which permits traffic to the VPC endpoints.
newUpdateIdentityProviderSettings ::
  -- | 'identityProvider'
  IdentityProvider ->
  -- | 'product'
  Prelude.Text ->
  -- | 'updateSettings'
  UpdateSettings ->
  UpdateIdentityProviderSettings
newUpdateIdentityProviderSettings
  pIdentityProvider_
  pProduct_
  pUpdateSettings_ =
    UpdateIdentityProviderSettings'
      { identityProvider =
          pIdentityProvider_,
        product = pProduct_,
        updateSettings = pUpdateSettings_
      }

-- | Undocumented member.
updateIdentityProviderSettings_identityProvider :: Lens.Lens' UpdateIdentityProviderSettings IdentityProvider
updateIdentityProviderSettings_identityProvider = Lens.lens (\UpdateIdentityProviderSettings' {identityProvider} -> identityProvider) (\s@UpdateIdentityProviderSettings' {} a -> s {identityProvider = a} :: UpdateIdentityProviderSettings)

-- | The name of the user-based subscription product.
updateIdentityProviderSettings_product :: Lens.Lens' UpdateIdentityProviderSettings Prelude.Text
updateIdentityProviderSettings_product = Lens.lens (\UpdateIdentityProviderSettings' {product} -> product) (\s@UpdateIdentityProviderSettings' {} a -> s {product = a} :: UpdateIdentityProviderSettings)

-- | Updates the registered identity provider’s product related configuration
-- settings. You can update any combination of settings in a single
-- operation such as the:
--
-- -   Subnets which you want to add to provision VPC endpoints.
--
-- -   Subnets which you want to remove the VPC endpoints from.
--
-- -   Security group ID which permits traffic to the VPC endpoints.
updateIdentityProviderSettings_updateSettings :: Lens.Lens' UpdateIdentityProviderSettings UpdateSettings
updateIdentityProviderSettings_updateSettings = Lens.lens (\UpdateIdentityProviderSettings' {updateSettings} -> updateSettings) (\s@UpdateIdentityProviderSettings' {} a -> s {updateSettings = a} :: UpdateIdentityProviderSettings)

instance
  Core.AWSRequest
    UpdateIdentityProviderSettings
  where
  type
    AWSResponse UpdateIdentityProviderSettings =
      UpdateIdentityProviderSettingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateIdentityProviderSettingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "IdentityProviderSummary")
      )

instance
  Prelude.Hashable
    UpdateIdentityProviderSettings
  where
  hashWithSalt
    _salt
    UpdateIdentityProviderSettings' {..} =
      _salt
        `Prelude.hashWithSalt` identityProvider
        `Prelude.hashWithSalt` product
        `Prelude.hashWithSalt` updateSettings

instance
  Prelude.NFData
    UpdateIdentityProviderSettings
  where
  rnf UpdateIdentityProviderSettings' {..} =
    Prelude.rnf identityProvider
      `Prelude.seq` Prelude.rnf product
      `Prelude.seq` Prelude.rnf updateSettings

instance
  Data.ToHeaders
    UpdateIdentityProviderSettings
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateIdentityProviderSettings where
  toJSON UpdateIdentityProviderSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("IdentityProvider" Data..= identityProvider),
            Prelude.Just ("Product" Data..= product),
            Prelude.Just
              ("UpdateSettings" Data..= updateSettings)
          ]
      )

instance Data.ToPath UpdateIdentityProviderSettings where
  toPath =
    Prelude.const
      "/identity-provider/UpdateIdentityProviderSettings"

instance Data.ToQuery UpdateIdentityProviderSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateIdentityProviderSettingsResponse' smart constructor.
data UpdateIdentityProviderSettingsResponse = UpdateIdentityProviderSettingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    identityProviderSummary :: IdentityProviderSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateIdentityProviderSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateIdentityProviderSettingsResponse_httpStatus' - The response's http status code.
--
-- 'identityProviderSummary', 'updateIdentityProviderSettingsResponse_identityProviderSummary' - Undocumented member.
newUpdateIdentityProviderSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'identityProviderSummary'
  IdentityProviderSummary ->
  UpdateIdentityProviderSettingsResponse
newUpdateIdentityProviderSettingsResponse
  pHttpStatus_
  pIdentityProviderSummary_ =
    UpdateIdentityProviderSettingsResponse'
      { httpStatus =
          pHttpStatus_,
        identityProviderSummary =
          pIdentityProviderSummary_
      }

-- | The response's http status code.
updateIdentityProviderSettingsResponse_httpStatus :: Lens.Lens' UpdateIdentityProviderSettingsResponse Prelude.Int
updateIdentityProviderSettingsResponse_httpStatus = Lens.lens (\UpdateIdentityProviderSettingsResponse' {httpStatus} -> httpStatus) (\s@UpdateIdentityProviderSettingsResponse' {} a -> s {httpStatus = a} :: UpdateIdentityProviderSettingsResponse)

-- | Undocumented member.
updateIdentityProviderSettingsResponse_identityProviderSummary :: Lens.Lens' UpdateIdentityProviderSettingsResponse IdentityProviderSummary
updateIdentityProviderSettingsResponse_identityProviderSummary = Lens.lens (\UpdateIdentityProviderSettingsResponse' {identityProviderSummary} -> identityProviderSummary) (\s@UpdateIdentityProviderSettingsResponse' {} a -> s {identityProviderSummary = a} :: UpdateIdentityProviderSettingsResponse)

instance
  Prelude.NFData
    UpdateIdentityProviderSettingsResponse
  where
  rnf UpdateIdentityProviderSettingsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf identityProviderSummary

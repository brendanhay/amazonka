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
-- Module      : Amazonka.WorkLink.UpdateIdentityProviderConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the identity provider configuration for the fleet.
module Amazonka.WorkLink.UpdateIdentityProviderConfiguration
  ( -- * Creating a Request
    UpdateIdentityProviderConfiguration (..),
    newUpdateIdentityProviderConfiguration,

    -- * Request Lenses
    updateIdentityProviderConfiguration_identityProviderSamlMetadata,
    updateIdentityProviderConfiguration_fleetArn,
    updateIdentityProviderConfiguration_identityProviderType,

    -- * Destructuring the Response
    UpdateIdentityProviderConfigurationResponse (..),
    newUpdateIdentityProviderConfigurationResponse,

    -- * Response Lenses
    updateIdentityProviderConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkLink.Types

-- | /See:/ 'newUpdateIdentityProviderConfiguration' smart constructor.
data UpdateIdentityProviderConfiguration = UpdateIdentityProviderConfiguration'
  { -- | The SAML metadata document provided by the customer’s identity provider.
    -- The existing IdentityProviderSamlMetadata is unset if null is passed.
    identityProviderSamlMetadata :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the fleet.
    fleetArn :: Prelude.Text,
    -- | The type of identity provider.
    identityProviderType :: IdentityProviderType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateIdentityProviderConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityProviderSamlMetadata', 'updateIdentityProviderConfiguration_identityProviderSamlMetadata' - The SAML metadata document provided by the customer’s identity provider.
-- The existing IdentityProviderSamlMetadata is unset if null is passed.
--
-- 'fleetArn', 'updateIdentityProviderConfiguration_fleetArn' - The ARN of the fleet.
--
-- 'identityProviderType', 'updateIdentityProviderConfiguration_identityProviderType' - The type of identity provider.
newUpdateIdentityProviderConfiguration ::
  -- | 'fleetArn'
  Prelude.Text ->
  -- | 'identityProviderType'
  IdentityProviderType ->
  UpdateIdentityProviderConfiguration
newUpdateIdentityProviderConfiguration
  pFleetArn_
  pIdentityProviderType_ =
    UpdateIdentityProviderConfiguration'
      { identityProviderSamlMetadata =
          Prelude.Nothing,
        fleetArn = pFleetArn_,
        identityProviderType =
          pIdentityProviderType_
      }

-- | The SAML metadata document provided by the customer’s identity provider.
-- The existing IdentityProviderSamlMetadata is unset if null is passed.
updateIdentityProviderConfiguration_identityProviderSamlMetadata :: Lens.Lens' UpdateIdentityProviderConfiguration (Prelude.Maybe Prelude.Text)
updateIdentityProviderConfiguration_identityProviderSamlMetadata = Lens.lens (\UpdateIdentityProviderConfiguration' {identityProviderSamlMetadata} -> identityProviderSamlMetadata) (\s@UpdateIdentityProviderConfiguration' {} a -> s {identityProviderSamlMetadata = a} :: UpdateIdentityProviderConfiguration)

-- | The ARN of the fleet.
updateIdentityProviderConfiguration_fleetArn :: Lens.Lens' UpdateIdentityProviderConfiguration Prelude.Text
updateIdentityProviderConfiguration_fleetArn = Lens.lens (\UpdateIdentityProviderConfiguration' {fleetArn} -> fleetArn) (\s@UpdateIdentityProviderConfiguration' {} a -> s {fleetArn = a} :: UpdateIdentityProviderConfiguration)

-- | The type of identity provider.
updateIdentityProviderConfiguration_identityProviderType :: Lens.Lens' UpdateIdentityProviderConfiguration IdentityProviderType
updateIdentityProviderConfiguration_identityProviderType = Lens.lens (\UpdateIdentityProviderConfiguration' {identityProviderType} -> identityProviderType) (\s@UpdateIdentityProviderConfiguration' {} a -> s {identityProviderType = a} :: UpdateIdentityProviderConfiguration)

instance
  Core.AWSRequest
    UpdateIdentityProviderConfiguration
  where
  type
    AWSResponse UpdateIdentityProviderConfiguration =
      UpdateIdentityProviderConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateIdentityProviderConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateIdentityProviderConfiguration
  where
  hashWithSalt
    _salt
    UpdateIdentityProviderConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` identityProviderSamlMetadata
        `Prelude.hashWithSalt` fleetArn
        `Prelude.hashWithSalt` identityProviderType

instance
  Prelude.NFData
    UpdateIdentityProviderConfiguration
  where
  rnf UpdateIdentityProviderConfiguration' {..} =
    Prelude.rnf identityProviderSamlMetadata
      `Prelude.seq` Prelude.rnf fleetArn
      `Prelude.seq` Prelude.rnf identityProviderType

instance
  Core.ToHeaders
    UpdateIdentityProviderConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    UpdateIdentityProviderConfiguration
  where
  toJSON UpdateIdentityProviderConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("IdentityProviderSamlMetadata" Core..=)
              Prelude.<$> identityProviderSamlMetadata,
            Prelude.Just ("FleetArn" Core..= fleetArn),
            Prelude.Just
              ( "IdentityProviderType"
                  Core..= identityProviderType
              )
          ]
      )

instance
  Core.ToPath
    UpdateIdentityProviderConfiguration
  where
  toPath =
    Prelude.const
      "/updateIdentityProviderConfiguration"

instance
  Core.ToQuery
    UpdateIdentityProviderConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateIdentityProviderConfigurationResponse' smart constructor.
data UpdateIdentityProviderConfigurationResponse = UpdateIdentityProviderConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateIdentityProviderConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateIdentityProviderConfigurationResponse_httpStatus' - The response's http status code.
newUpdateIdentityProviderConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateIdentityProviderConfigurationResponse
newUpdateIdentityProviderConfigurationResponse
  pHttpStatus_ =
    UpdateIdentityProviderConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateIdentityProviderConfigurationResponse_httpStatus :: Lens.Lens' UpdateIdentityProviderConfigurationResponse Prelude.Int
updateIdentityProviderConfigurationResponse_httpStatus = Lens.lens (\UpdateIdentityProviderConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateIdentityProviderConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateIdentityProviderConfigurationResponse)

instance
  Prelude.NFData
    UpdateIdentityProviderConfigurationResponse
  where
  rnf UpdateIdentityProviderConfigurationResponse' {..} =
    Prelude.rnf httpStatus

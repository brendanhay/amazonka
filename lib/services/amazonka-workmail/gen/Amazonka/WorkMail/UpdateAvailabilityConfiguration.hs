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
-- Module      : Amazonka.WorkMail.UpdateAvailabilityConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing @AvailabilityConfiguration@ for the given WorkMail
-- organization and domain.
module Amazonka.WorkMail.UpdateAvailabilityConfiguration
  ( -- * Creating a Request
    UpdateAvailabilityConfiguration (..),
    newUpdateAvailabilityConfiguration,

    -- * Request Lenses
    updateAvailabilityConfiguration_ewsProvider,
    updateAvailabilityConfiguration_lambdaProvider,
    updateAvailabilityConfiguration_organizationId,
    updateAvailabilityConfiguration_domainName,

    -- * Destructuring the Response
    UpdateAvailabilityConfigurationResponse (..),
    newUpdateAvailabilityConfigurationResponse,

    -- * Response Lenses
    updateAvailabilityConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newUpdateAvailabilityConfiguration' smart constructor.
data UpdateAvailabilityConfiguration = UpdateAvailabilityConfiguration'
  { -- | The EWS availability provider definition. The request must contain
    -- exactly one provider definition, either @EwsProvider@ or
    -- @LambdaProvider@. The previously stored provider will be overridden by
    -- the one provided.
    ewsProvider :: Prelude.Maybe EwsAvailabilityProvider,
    -- | The Lambda availability provider definition. The request must contain
    -- exactly one provider definition, either @EwsProvider@ or
    -- @LambdaProvider@. The previously stored provider will be overridden by
    -- the one provided.
    lambdaProvider :: Prelude.Maybe LambdaAvailabilityProvider,
    -- | The WorkMail organization for which the @AvailabilityConfiguration@ will
    -- be updated.
    organizationId :: Prelude.Text,
    -- | The domain to which the provider applies the availability configuration.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAvailabilityConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ewsProvider', 'updateAvailabilityConfiguration_ewsProvider' - The EWS availability provider definition. The request must contain
-- exactly one provider definition, either @EwsProvider@ or
-- @LambdaProvider@. The previously stored provider will be overridden by
-- the one provided.
--
-- 'lambdaProvider', 'updateAvailabilityConfiguration_lambdaProvider' - The Lambda availability provider definition. The request must contain
-- exactly one provider definition, either @EwsProvider@ or
-- @LambdaProvider@. The previously stored provider will be overridden by
-- the one provided.
--
-- 'organizationId', 'updateAvailabilityConfiguration_organizationId' - The WorkMail organization for which the @AvailabilityConfiguration@ will
-- be updated.
--
-- 'domainName', 'updateAvailabilityConfiguration_domainName' - The domain to which the provider applies the availability configuration.
newUpdateAvailabilityConfiguration ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'domainName'
  Prelude.Text ->
  UpdateAvailabilityConfiguration
newUpdateAvailabilityConfiguration
  pOrganizationId_
  pDomainName_ =
    UpdateAvailabilityConfiguration'
      { ewsProvider =
          Prelude.Nothing,
        lambdaProvider = Prelude.Nothing,
        organizationId = pOrganizationId_,
        domainName = pDomainName_
      }

-- | The EWS availability provider definition. The request must contain
-- exactly one provider definition, either @EwsProvider@ or
-- @LambdaProvider@. The previously stored provider will be overridden by
-- the one provided.
updateAvailabilityConfiguration_ewsProvider :: Lens.Lens' UpdateAvailabilityConfiguration (Prelude.Maybe EwsAvailabilityProvider)
updateAvailabilityConfiguration_ewsProvider = Lens.lens (\UpdateAvailabilityConfiguration' {ewsProvider} -> ewsProvider) (\s@UpdateAvailabilityConfiguration' {} a -> s {ewsProvider = a} :: UpdateAvailabilityConfiguration)

-- | The Lambda availability provider definition. The request must contain
-- exactly one provider definition, either @EwsProvider@ or
-- @LambdaProvider@. The previously stored provider will be overridden by
-- the one provided.
updateAvailabilityConfiguration_lambdaProvider :: Lens.Lens' UpdateAvailabilityConfiguration (Prelude.Maybe LambdaAvailabilityProvider)
updateAvailabilityConfiguration_lambdaProvider = Lens.lens (\UpdateAvailabilityConfiguration' {lambdaProvider} -> lambdaProvider) (\s@UpdateAvailabilityConfiguration' {} a -> s {lambdaProvider = a} :: UpdateAvailabilityConfiguration)

-- | The WorkMail organization for which the @AvailabilityConfiguration@ will
-- be updated.
updateAvailabilityConfiguration_organizationId :: Lens.Lens' UpdateAvailabilityConfiguration Prelude.Text
updateAvailabilityConfiguration_organizationId = Lens.lens (\UpdateAvailabilityConfiguration' {organizationId} -> organizationId) (\s@UpdateAvailabilityConfiguration' {} a -> s {organizationId = a} :: UpdateAvailabilityConfiguration)

-- | The domain to which the provider applies the availability configuration.
updateAvailabilityConfiguration_domainName :: Lens.Lens' UpdateAvailabilityConfiguration Prelude.Text
updateAvailabilityConfiguration_domainName = Lens.lens (\UpdateAvailabilityConfiguration' {domainName} -> domainName) (\s@UpdateAvailabilityConfiguration' {} a -> s {domainName = a} :: UpdateAvailabilityConfiguration)

instance
  Core.AWSRequest
    UpdateAvailabilityConfiguration
  where
  type
    AWSResponse UpdateAvailabilityConfiguration =
      UpdateAvailabilityConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateAvailabilityConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateAvailabilityConfiguration
  where
  hashWithSalt
    _salt
    UpdateAvailabilityConfiguration' {..} =
      _salt `Prelude.hashWithSalt` ewsProvider
        `Prelude.hashWithSalt` lambdaProvider
        `Prelude.hashWithSalt` organizationId
        `Prelude.hashWithSalt` domainName

instance
  Prelude.NFData
    UpdateAvailabilityConfiguration
  where
  rnf UpdateAvailabilityConfiguration' {..} =
    Prelude.rnf ewsProvider
      `Prelude.seq` Prelude.rnf lambdaProvider
      `Prelude.seq` Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf domainName

instance
  Data.ToHeaders
    UpdateAvailabilityConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.UpdateAvailabilityConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateAvailabilityConfiguration where
  toJSON UpdateAvailabilityConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EwsProvider" Data..=) Prelude.<$> ewsProvider,
            ("LambdaProvider" Data..=)
              Prelude.<$> lambdaProvider,
            Prelude.Just
              ("OrganizationId" Data..= organizationId),
            Prelude.Just ("DomainName" Data..= domainName)
          ]
      )

instance Data.ToPath UpdateAvailabilityConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateAvailabilityConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAvailabilityConfigurationResponse' smart constructor.
data UpdateAvailabilityConfigurationResponse = UpdateAvailabilityConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAvailabilityConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateAvailabilityConfigurationResponse_httpStatus' - The response's http status code.
newUpdateAvailabilityConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateAvailabilityConfigurationResponse
newUpdateAvailabilityConfigurationResponse
  pHttpStatus_ =
    UpdateAvailabilityConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateAvailabilityConfigurationResponse_httpStatus :: Lens.Lens' UpdateAvailabilityConfigurationResponse Prelude.Int
updateAvailabilityConfigurationResponse_httpStatus = Lens.lens (\UpdateAvailabilityConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateAvailabilityConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateAvailabilityConfigurationResponse)

instance
  Prelude.NFData
    UpdateAvailabilityConfigurationResponse
  where
  rnf UpdateAvailabilityConfigurationResponse' {..} =
    Prelude.rnf httpStatus

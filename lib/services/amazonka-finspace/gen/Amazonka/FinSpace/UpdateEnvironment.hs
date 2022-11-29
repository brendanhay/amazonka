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
-- Module      : Amazonka.FinSpace.UpdateEnvironment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update your FinSpace environment.
module Amazonka.FinSpace.UpdateEnvironment
  ( -- * Creating a Request
    UpdateEnvironment (..),
    newUpdateEnvironment,

    -- * Request Lenses
    updateEnvironment_name,
    updateEnvironment_federationParameters,
    updateEnvironment_description,
    updateEnvironment_federationMode,
    updateEnvironment_environmentId,

    -- * Destructuring the Response
    UpdateEnvironmentResponse (..),
    newUpdateEnvironmentResponse,

    -- * Response Lenses
    updateEnvironmentResponse_environment,
    updateEnvironmentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FinSpace.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateEnvironment' smart constructor.
data UpdateEnvironment = UpdateEnvironment'
  { -- | The name of the environment.
    name :: Prelude.Maybe Prelude.Text,
    federationParameters :: Prelude.Maybe FederationParameters,
    -- | The description of the environment.
    description :: Prelude.Maybe Prelude.Text,
    -- | Authentication mode for the environment.
    --
    -- -   @FEDERATED@ - Users access FinSpace through Single Sign On (SSO) via
    --     your Identity provider.
    --
    -- -   @LOCAL@ - Users access FinSpace via email and password managed
    --     within the FinSpace environment.
    federationMode :: Prelude.Maybe FederationMode,
    -- | The identifier of the FinSpace environment.
    environmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateEnvironment_name' - The name of the environment.
--
-- 'federationParameters', 'updateEnvironment_federationParameters' - Undocumented member.
--
-- 'description', 'updateEnvironment_description' - The description of the environment.
--
-- 'federationMode', 'updateEnvironment_federationMode' - Authentication mode for the environment.
--
-- -   @FEDERATED@ - Users access FinSpace through Single Sign On (SSO) via
--     your Identity provider.
--
-- -   @LOCAL@ - Users access FinSpace via email and password managed
--     within the FinSpace environment.
--
-- 'environmentId', 'updateEnvironment_environmentId' - The identifier of the FinSpace environment.
newUpdateEnvironment ::
  -- | 'environmentId'
  Prelude.Text ->
  UpdateEnvironment
newUpdateEnvironment pEnvironmentId_ =
  UpdateEnvironment'
    { name = Prelude.Nothing,
      federationParameters = Prelude.Nothing,
      description = Prelude.Nothing,
      federationMode = Prelude.Nothing,
      environmentId = pEnvironmentId_
    }

-- | The name of the environment.
updateEnvironment_name :: Lens.Lens' UpdateEnvironment (Prelude.Maybe Prelude.Text)
updateEnvironment_name = Lens.lens (\UpdateEnvironment' {name} -> name) (\s@UpdateEnvironment' {} a -> s {name = a} :: UpdateEnvironment)

-- | Undocumented member.
updateEnvironment_federationParameters :: Lens.Lens' UpdateEnvironment (Prelude.Maybe FederationParameters)
updateEnvironment_federationParameters = Lens.lens (\UpdateEnvironment' {federationParameters} -> federationParameters) (\s@UpdateEnvironment' {} a -> s {federationParameters = a} :: UpdateEnvironment)

-- | The description of the environment.
updateEnvironment_description :: Lens.Lens' UpdateEnvironment (Prelude.Maybe Prelude.Text)
updateEnvironment_description = Lens.lens (\UpdateEnvironment' {description} -> description) (\s@UpdateEnvironment' {} a -> s {description = a} :: UpdateEnvironment)

-- | Authentication mode for the environment.
--
-- -   @FEDERATED@ - Users access FinSpace through Single Sign On (SSO) via
--     your Identity provider.
--
-- -   @LOCAL@ - Users access FinSpace via email and password managed
--     within the FinSpace environment.
updateEnvironment_federationMode :: Lens.Lens' UpdateEnvironment (Prelude.Maybe FederationMode)
updateEnvironment_federationMode = Lens.lens (\UpdateEnvironment' {federationMode} -> federationMode) (\s@UpdateEnvironment' {} a -> s {federationMode = a} :: UpdateEnvironment)

-- | The identifier of the FinSpace environment.
updateEnvironment_environmentId :: Lens.Lens' UpdateEnvironment Prelude.Text
updateEnvironment_environmentId = Lens.lens (\UpdateEnvironment' {environmentId} -> environmentId) (\s@UpdateEnvironment' {} a -> s {environmentId = a} :: UpdateEnvironment)

instance Core.AWSRequest UpdateEnvironment where
  type
    AWSResponse UpdateEnvironment =
      UpdateEnvironmentResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateEnvironmentResponse'
            Prelude.<$> (x Core..?> "environment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateEnvironment where
  hashWithSalt _salt UpdateEnvironment' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` federationParameters
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` federationMode
      `Prelude.hashWithSalt` environmentId

instance Prelude.NFData UpdateEnvironment where
  rnf UpdateEnvironment' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf federationParameters
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf federationMode
      `Prelude.seq` Prelude.rnf environmentId

instance Core.ToHeaders UpdateEnvironment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateEnvironment where
  toJSON UpdateEnvironment' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("name" Core..=) Prelude.<$> name,
            ("federationParameters" Core..=)
              Prelude.<$> federationParameters,
            ("description" Core..=) Prelude.<$> description,
            ("federationMode" Core..=)
              Prelude.<$> federationMode
          ]
      )

instance Core.ToPath UpdateEnvironment where
  toPath UpdateEnvironment' {..} =
    Prelude.mconcat
      ["/environment/", Core.toBS environmentId]

instance Core.ToQuery UpdateEnvironment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateEnvironmentResponse' smart constructor.
data UpdateEnvironmentResponse = UpdateEnvironmentResponse'
  { -- | Returns the FinSpace environment object.
    environment :: Prelude.Maybe Environment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEnvironmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environment', 'updateEnvironmentResponse_environment' - Returns the FinSpace environment object.
--
-- 'httpStatus', 'updateEnvironmentResponse_httpStatus' - The response's http status code.
newUpdateEnvironmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateEnvironmentResponse
newUpdateEnvironmentResponse pHttpStatus_ =
  UpdateEnvironmentResponse'
    { environment =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns the FinSpace environment object.
updateEnvironmentResponse_environment :: Lens.Lens' UpdateEnvironmentResponse (Prelude.Maybe Environment)
updateEnvironmentResponse_environment = Lens.lens (\UpdateEnvironmentResponse' {environment} -> environment) (\s@UpdateEnvironmentResponse' {} a -> s {environment = a} :: UpdateEnvironmentResponse)

-- | The response's http status code.
updateEnvironmentResponse_httpStatus :: Lens.Lens' UpdateEnvironmentResponse Prelude.Int
updateEnvironmentResponse_httpStatus = Lens.lens (\UpdateEnvironmentResponse' {httpStatus} -> httpStatus) (\s@UpdateEnvironmentResponse' {} a -> s {httpStatus = a} :: UpdateEnvironmentResponse)

instance Prelude.NFData UpdateEnvironmentResponse where
  rnf UpdateEnvironmentResponse' {..} =
    Prelude.rnf environment
      `Prelude.seq` Prelude.rnf httpStatus

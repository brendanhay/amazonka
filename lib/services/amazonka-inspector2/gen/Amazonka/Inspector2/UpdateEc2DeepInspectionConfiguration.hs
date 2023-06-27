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
-- Module      : Amazonka.Inspector2.UpdateEc2DeepInspectionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Activates, deactivates Amazon Inspector deep inspection, or updates
-- custom paths for your account.
module Amazonka.Inspector2.UpdateEc2DeepInspectionConfiguration
  ( -- * Creating a Request
    UpdateEc2DeepInspectionConfiguration (..),
    newUpdateEc2DeepInspectionConfiguration,

    -- * Request Lenses
    updateEc2DeepInspectionConfiguration_activateDeepInspection,
    updateEc2DeepInspectionConfiguration_packagePaths,

    -- * Destructuring the Response
    UpdateEc2DeepInspectionConfigurationResponse (..),
    newUpdateEc2DeepInspectionConfigurationResponse,

    -- * Response Lenses
    updateEc2DeepInspectionConfigurationResponse_errorMessage,
    updateEc2DeepInspectionConfigurationResponse_orgPackagePaths,
    updateEc2DeepInspectionConfigurationResponse_packagePaths,
    updateEc2DeepInspectionConfigurationResponse_status,
    updateEc2DeepInspectionConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateEc2DeepInspectionConfiguration' smart constructor.
data UpdateEc2DeepInspectionConfiguration = UpdateEc2DeepInspectionConfiguration'
  { -- | Specify @TRUE@ to activate Amazon Inspector deep inspection in your
    -- account, or @FALSE@ to deactivate. Member accounts in an organization
    -- cannot deactivate deep inspection, instead the delegated administrator
    -- for the organization can deactivate a member account using
    -- <https://docs.aws.amazon.com/inspector/v2/APIReference/API_BatchUpdateMemberEc2DeepInspectionStatus.html BatchUpdateMemberEc2DeepInspectionStatus>.
    activateDeepInspection :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Inspector deep inspection custom paths you are adding for
    -- your account.
    packagePaths :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEc2DeepInspectionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activateDeepInspection', 'updateEc2DeepInspectionConfiguration_activateDeepInspection' - Specify @TRUE@ to activate Amazon Inspector deep inspection in your
-- account, or @FALSE@ to deactivate. Member accounts in an organization
-- cannot deactivate deep inspection, instead the delegated administrator
-- for the organization can deactivate a member account using
-- <https://docs.aws.amazon.com/inspector/v2/APIReference/API_BatchUpdateMemberEc2DeepInspectionStatus.html BatchUpdateMemberEc2DeepInspectionStatus>.
--
-- 'packagePaths', 'updateEc2DeepInspectionConfiguration_packagePaths' - The Amazon Inspector deep inspection custom paths you are adding for
-- your account.
newUpdateEc2DeepInspectionConfiguration ::
  UpdateEc2DeepInspectionConfiguration
newUpdateEc2DeepInspectionConfiguration =
  UpdateEc2DeepInspectionConfiguration'
    { activateDeepInspection =
        Prelude.Nothing,
      packagePaths = Prelude.Nothing
    }

-- | Specify @TRUE@ to activate Amazon Inspector deep inspection in your
-- account, or @FALSE@ to deactivate. Member accounts in an organization
-- cannot deactivate deep inspection, instead the delegated administrator
-- for the organization can deactivate a member account using
-- <https://docs.aws.amazon.com/inspector/v2/APIReference/API_BatchUpdateMemberEc2DeepInspectionStatus.html BatchUpdateMemberEc2DeepInspectionStatus>.
updateEc2DeepInspectionConfiguration_activateDeepInspection :: Lens.Lens' UpdateEc2DeepInspectionConfiguration (Prelude.Maybe Prelude.Bool)
updateEc2DeepInspectionConfiguration_activateDeepInspection = Lens.lens (\UpdateEc2DeepInspectionConfiguration' {activateDeepInspection} -> activateDeepInspection) (\s@UpdateEc2DeepInspectionConfiguration' {} a -> s {activateDeepInspection = a} :: UpdateEc2DeepInspectionConfiguration)

-- | The Amazon Inspector deep inspection custom paths you are adding for
-- your account.
updateEc2DeepInspectionConfiguration_packagePaths :: Lens.Lens' UpdateEc2DeepInspectionConfiguration (Prelude.Maybe [Prelude.Text])
updateEc2DeepInspectionConfiguration_packagePaths = Lens.lens (\UpdateEc2DeepInspectionConfiguration' {packagePaths} -> packagePaths) (\s@UpdateEc2DeepInspectionConfiguration' {} a -> s {packagePaths = a} :: UpdateEc2DeepInspectionConfiguration) Prelude.. Lens.mapping Lens.coerced

instance
  Core.AWSRequest
    UpdateEc2DeepInspectionConfiguration
  where
  type
    AWSResponse UpdateEc2DeepInspectionConfiguration =
      UpdateEc2DeepInspectionConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateEc2DeepInspectionConfigurationResponse'
            Prelude.<$> (x Data..?> "errorMessage")
            Prelude.<*> ( x
                            Data..?> "orgPackagePaths"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "packagePaths" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateEc2DeepInspectionConfiguration
  where
  hashWithSalt
    _salt
    UpdateEc2DeepInspectionConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` activateDeepInspection
        `Prelude.hashWithSalt` packagePaths

instance
  Prelude.NFData
    UpdateEc2DeepInspectionConfiguration
  where
  rnf UpdateEc2DeepInspectionConfiguration' {..} =
    Prelude.rnf activateDeepInspection
      `Prelude.seq` Prelude.rnf packagePaths

instance
  Data.ToHeaders
    UpdateEc2DeepInspectionConfiguration
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

instance
  Data.ToJSON
    UpdateEc2DeepInspectionConfiguration
  where
  toJSON UpdateEc2DeepInspectionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("activateDeepInspection" Data..=)
              Prelude.<$> activateDeepInspection,
            ("packagePaths" Data..=) Prelude.<$> packagePaths
          ]
      )

instance
  Data.ToPath
    UpdateEc2DeepInspectionConfiguration
  where
  toPath =
    Prelude.const
      "/ec2deepinspectionconfiguration/update"

instance
  Data.ToQuery
    UpdateEc2DeepInspectionConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateEc2DeepInspectionConfigurationResponse' smart constructor.
data UpdateEc2DeepInspectionConfigurationResponse = UpdateEc2DeepInspectionConfigurationResponse'
  { -- | An error message explaining why new Amazon Inspector deep inspection
    -- custom paths could not be added.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The current Amazon Inspector deep inspection custom paths for the
    -- organization.
    orgPackagePaths :: Prelude.Maybe [Prelude.Text],
    -- | The current Amazon Inspector deep inspection custom paths for your
    -- account.
    packagePaths :: Prelude.Maybe [Prelude.Text],
    -- | The status of Amazon Inspector deep inspection in your account.
    status :: Prelude.Maybe Ec2DeepInspectionStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEc2DeepInspectionConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorMessage', 'updateEc2DeepInspectionConfigurationResponse_errorMessage' - An error message explaining why new Amazon Inspector deep inspection
-- custom paths could not be added.
--
-- 'orgPackagePaths', 'updateEc2DeepInspectionConfigurationResponse_orgPackagePaths' - The current Amazon Inspector deep inspection custom paths for the
-- organization.
--
-- 'packagePaths', 'updateEc2DeepInspectionConfigurationResponse_packagePaths' - The current Amazon Inspector deep inspection custom paths for your
-- account.
--
-- 'status', 'updateEc2DeepInspectionConfigurationResponse_status' - The status of Amazon Inspector deep inspection in your account.
--
-- 'httpStatus', 'updateEc2DeepInspectionConfigurationResponse_httpStatus' - The response's http status code.
newUpdateEc2DeepInspectionConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateEc2DeepInspectionConfigurationResponse
newUpdateEc2DeepInspectionConfigurationResponse
  pHttpStatus_ =
    UpdateEc2DeepInspectionConfigurationResponse'
      { errorMessage =
          Prelude.Nothing,
        orgPackagePaths =
          Prelude.Nothing,
        packagePaths =
          Prelude.Nothing,
        status = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An error message explaining why new Amazon Inspector deep inspection
-- custom paths could not be added.
updateEc2DeepInspectionConfigurationResponse_errorMessage :: Lens.Lens' UpdateEc2DeepInspectionConfigurationResponse (Prelude.Maybe Prelude.Text)
updateEc2DeepInspectionConfigurationResponse_errorMessage = Lens.lens (\UpdateEc2DeepInspectionConfigurationResponse' {errorMessage} -> errorMessage) (\s@UpdateEc2DeepInspectionConfigurationResponse' {} a -> s {errorMessage = a} :: UpdateEc2DeepInspectionConfigurationResponse)

-- | The current Amazon Inspector deep inspection custom paths for the
-- organization.
updateEc2DeepInspectionConfigurationResponse_orgPackagePaths :: Lens.Lens' UpdateEc2DeepInspectionConfigurationResponse (Prelude.Maybe [Prelude.Text])
updateEc2DeepInspectionConfigurationResponse_orgPackagePaths = Lens.lens (\UpdateEc2DeepInspectionConfigurationResponse' {orgPackagePaths} -> orgPackagePaths) (\s@UpdateEc2DeepInspectionConfigurationResponse' {} a -> s {orgPackagePaths = a} :: UpdateEc2DeepInspectionConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The current Amazon Inspector deep inspection custom paths for your
-- account.
updateEc2DeepInspectionConfigurationResponse_packagePaths :: Lens.Lens' UpdateEc2DeepInspectionConfigurationResponse (Prelude.Maybe [Prelude.Text])
updateEc2DeepInspectionConfigurationResponse_packagePaths = Lens.lens (\UpdateEc2DeepInspectionConfigurationResponse' {packagePaths} -> packagePaths) (\s@UpdateEc2DeepInspectionConfigurationResponse' {} a -> s {packagePaths = a} :: UpdateEc2DeepInspectionConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The status of Amazon Inspector deep inspection in your account.
updateEc2DeepInspectionConfigurationResponse_status :: Lens.Lens' UpdateEc2DeepInspectionConfigurationResponse (Prelude.Maybe Ec2DeepInspectionStatus)
updateEc2DeepInspectionConfigurationResponse_status = Lens.lens (\UpdateEc2DeepInspectionConfigurationResponse' {status} -> status) (\s@UpdateEc2DeepInspectionConfigurationResponse' {} a -> s {status = a} :: UpdateEc2DeepInspectionConfigurationResponse)

-- | The response's http status code.
updateEc2DeepInspectionConfigurationResponse_httpStatus :: Lens.Lens' UpdateEc2DeepInspectionConfigurationResponse Prelude.Int
updateEc2DeepInspectionConfigurationResponse_httpStatus = Lens.lens (\UpdateEc2DeepInspectionConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateEc2DeepInspectionConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateEc2DeepInspectionConfigurationResponse)

instance
  Prelude.NFData
    UpdateEc2DeepInspectionConfigurationResponse
  where
  rnf UpdateEc2DeepInspectionConfigurationResponse' {..} =
    Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf orgPackagePaths
      `Prelude.seq` Prelude.rnf packagePaths
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus

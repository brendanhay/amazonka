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
-- Module      : Amazonka.Proton.UpdateAccountSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update the Proton service pipeline role or repository settings.
module Amazonka.Proton.UpdateAccountSettings
  ( -- * Creating a Request
    UpdateAccountSettings (..),
    newUpdateAccountSettings,

    -- * Request Lenses
    updateAccountSettings_pipelineProvisioningRepository,
    updateAccountSettings_pipelineServiceRoleArn,

    -- * Destructuring the Response
    UpdateAccountSettingsResponse (..),
    newUpdateAccountSettingsResponse,

    -- * Response Lenses
    updateAccountSettingsResponse_httpStatus,
    updateAccountSettingsResponse_accountSettings,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAccountSettings' smart constructor.
data UpdateAccountSettings = UpdateAccountSettings'
  { -- | A repository for pipeline provisioning. Specify it if you have
    -- environments configured for self-managed provisioning with services that
    -- include pipelines.
    pipelineProvisioningRepository :: Prelude.Maybe RepositoryBranchInput,
    -- | The Amazon Resource Name (ARN) of the service role you want to use for
    -- provisioning pipelines. Assumed by Proton for Amazon Web
    -- Services-managed provisioning, and by customer-owned automation for
    -- self-managed provisioning.
    pipelineServiceRoleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAccountSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineProvisioningRepository', 'updateAccountSettings_pipelineProvisioningRepository' - A repository for pipeline provisioning. Specify it if you have
-- environments configured for self-managed provisioning with services that
-- include pipelines.
--
-- 'pipelineServiceRoleArn', 'updateAccountSettings_pipelineServiceRoleArn' - The Amazon Resource Name (ARN) of the service role you want to use for
-- provisioning pipelines. Assumed by Proton for Amazon Web
-- Services-managed provisioning, and by customer-owned automation for
-- self-managed provisioning.
newUpdateAccountSettings ::
  UpdateAccountSettings
newUpdateAccountSettings =
  UpdateAccountSettings'
    { pipelineProvisioningRepository =
        Prelude.Nothing,
      pipelineServiceRoleArn = Prelude.Nothing
    }

-- | A repository for pipeline provisioning. Specify it if you have
-- environments configured for self-managed provisioning with services that
-- include pipelines.
updateAccountSettings_pipelineProvisioningRepository :: Lens.Lens' UpdateAccountSettings (Prelude.Maybe RepositoryBranchInput)
updateAccountSettings_pipelineProvisioningRepository = Lens.lens (\UpdateAccountSettings' {pipelineProvisioningRepository} -> pipelineProvisioningRepository) (\s@UpdateAccountSettings' {} a -> s {pipelineProvisioningRepository = a} :: UpdateAccountSettings)

-- | The Amazon Resource Name (ARN) of the service role you want to use for
-- provisioning pipelines. Assumed by Proton for Amazon Web
-- Services-managed provisioning, and by customer-owned automation for
-- self-managed provisioning.
updateAccountSettings_pipelineServiceRoleArn :: Lens.Lens' UpdateAccountSettings (Prelude.Maybe Prelude.Text)
updateAccountSettings_pipelineServiceRoleArn = Lens.lens (\UpdateAccountSettings' {pipelineServiceRoleArn} -> pipelineServiceRoleArn) (\s@UpdateAccountSettings' {} a -> s {pipelineServiceRoleArn = a} :: UpdateAccountSettings)

instance Core.AWSRequest UpdateAccountSettings where
  type
    AWSResponse UpdateAccountSettings =
      UpdateAccountSettingsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAccountSettingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "accountSettings")
      )

instance Prelude.Hashable UpdateAccountSettings where
  hashWithSalt _salt UpdateAccountSettings' {..} =
    _salt
      `Prelude.hashWithSalt` pipelineProvisioningRepository
      `Prelude.hashWithSalt` pipelineServiceRoleArn

instance Prelude.NFData UpdateAccountSettings where
  rnf UpdateAccountSettings' {..} =
    Prelude.rnf pipelineProvisioningRepository
      `Prelude.seq` Prelude.rnf pipelineServiceRoleArn

instance Core.ToHeaders UpdateAccountSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AwsProton20200720.UpdateAccountSettings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateAccountSettings where
  toJSON UpdateAccountSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("pipelineProvisioningRepository" Core..=)
              Prelude.<$> pipelineProvisioningRepository,
            ("pipelineServiceRoleArn" Core..=)
              Prelude.<$> pipelineServiceRoleArn
          ]
      )

instance Core.ToPath UpdateAccountSettings where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateAccountSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAccountSettingsResponse' smart constructor.
data UpdateAccountSettingsResponse = UpdateAccountSettingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Proton pipeline service role and repository data shared across the
    -- Amazon Web Services account.
    accountSettings :: AccountSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAccountSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateAccountSettingsResponse_httpStatus' - The response's http status code.
--
-- 'accountSettings', 'updateAccountSettingsResponse_accountSettings' - The Proton pipeline service role and repository data shared across the
-- Amazon Web Services account.
newUpdateAccountSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'accountSettings'
  AccountSettings ->
  UpdateAccountSettingsResponse
newUpdateAccountSettingsResponse
  pHttpStatus_
  pAccountSettings_ =
    UpdateAccountSettingsResponse'
      { httpStatus =
          pHttpStatus_,
        accountSettings = pAccountSettings_
      }

-- | The response's http status code.
updateAccountSettingsResponse_httpStatus :: Lens.Lens' UpdateAccountSettingsResponse Prelude.Int
updateAccountSettingsResponse_httpStatus = Lens.lens (\UpdateAccountSettingsResponse' {httpStatus} -> httpStatus) (\s@UpdateAccountSettingsResponse' {} a -> s {httpStatus = a} :: UpdateAccountSettingsResponse)

-- | The Proton pipeline service role and repository data shared across the
-- Amazon Web Services account.
updateAccountSettingsResponse_accountSettings :: Lens.Lens' UpdateAccountSettingsResponse AccountSettings
updateAccountSettingsResponse_accountSettings = Lens.lens (\UpdateAccountSettingsResponse' {accountSettings} -> accountSettings) (\s@UpdateAccountSettingsResponse' {} a -> s {accountSettings = a} :: UpdateAccountSettingsResponse)

instance Prelude.NFData UpdateAccountSettingsResponse where
  rnf UpdateAccountSettingsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf accountSettings

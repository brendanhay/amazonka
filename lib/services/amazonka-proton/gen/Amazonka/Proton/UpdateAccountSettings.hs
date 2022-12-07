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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update Proton settings that are used for multiple services in the Amazon
-- Web Services account.
module Amazonka.Proton.UpdateAccountSettings
  ( -- * Creating a Request
    UpdateAccountSettings (..),
    newUpdateAccountSettings,

    -- * Request Lenses
    updateAccountSettings_deletePipelineProvisioningRepository,
    updateAccountSettings_pipelineProvisioningRepository,
    updateAccountSettings_pipelineServiceRoleArn,
    updateAccountSettings_pipelineCodebuildRoleArn,

    -- * Destructuring the Response
    UpdateAccountSettingsResponse (..),
    newUpdateAccountSettingsResponse,

    -- * Response Lenses
    updateAccountSettingsResponse_httpStatus,
    updateAccountSettingsResponse_accountSettings,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAccountSettings' smart constructor.
data UpdateAccountSettings = UpdateAccountSettings'
  { -- | Set to @true@ to remove a configured pipeline repository from the
    -- account settings. Don\'t set this field if you are updating the
    -- configured pipeline repository.
    deletePipelineProvisioningRepository :: Prelude.Maybe Prelude.Bool,
    -- | A linked repository for pipeline provisioning. Specify it if you have
    -- environments configured for self-managed provisioning with services that
    -- include pipelines. A linked repository is a repository that has been
    -- registered with Proton. For more information, see CreateRepository.
    --
    -- To remove a previously configured repository, set
    -- @deletePipelineProvisioningRepository@ to @true@, and don\'t set
    -- @pipelineProvisioningRepository@.
    pipelineProvisioningRepository :: Prelude.Maybe RepositoryBranchInput,
    -- | The Amazon Resource Name (ARN) of the service role you want to use for
    -- provisioning pipelines. Assumed by Proton for Amazon Web
    -- Services-managed provisioning, and by customer-owned automation for
    -- self-managed provisioning.
    --
    -- To remove a previously configured ARN, specify an empty string.
    pipelineServiceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the service role you want to use for
    -- provisioning pipelines. Proton assumes this role for CodeBuild-based
    -- provisioning.
    pipelineCodebuildRoleArn :: Prelude.Maybe Prelude.Text
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
-- 'deletePipelineProvisioningRepository', 'updateAccountSettings_deletePipelineProvisioningRepository' - Set to @true@ to remove a configured pipeline repository from the
-- account settings. Don\'t set this field if you are updating the
-- configured pipeline repository.
--
-- 'pipelineProvisioningRepository', 'updateAccountSettings_pipelineProvisioningRepository' - A linked repository for pipeline provisioning. Specify it if you have
-- environments configured for self-managed provisioning with services that
-- include pipelines. A linked repository is a repository that has been
-- registered with Proton. For more information, see CreateRepository.
--
-- To remove a previously configured repository, set
-- @deletePipelineProvisioningRepository@ to @true@, and don\'t set
-- @pipelineProvisioningRepository@.
--
-- 'pipelineServiceRoleArn', 'updateAccountSettings_pipelineServiceRoleArn' - The Amazon Resource Name (ARN) of the service role you want to use for
-- provisioning pipelines. Assumed by Proton for Amazon Web
-- Services-managed provisioning, and by customer-owned automation for
-- self-managed provisioning.
--
-- To remove a previously configured ARN, specify an empty string.
--
-- 'pipelineCodebuildRoleArn', 'updateAccountSettings_pipelineCodebuildRoleArn' - The Amazon Resource Name (ARN) of the service role you want to use for
-- provisioning pipelines. Proton assumes this role for CodeBuild-based
-- provisioning.
newUpdateAccountSettings ::
  UpdateAccountSettings
newUpdateAccountSettings =
  UpdateAccountSettings'
    { deletePipelineProvisioningRepository =
        Prelude.Nothing,
      pipelineProvisioningRepository = Prelude.Nothing,
      pipelineServiceRoleArn = Prelude.Nothing,
      pipelineCodebuildRoleArn = Prelude.Nothing
    }

-- | Set to @true@ to remove a configured pipeline repository from the
-- account settings. Don\'t set this field if you are updating the
-- configured pipeline repository.
updateAccountSettings_deletePipelineProvisioningRepository :: Lens.Lens' UpdateAccountSettings (Prelude.Maybe Prelude.Bool)
updateAccountSettings_deletePipelineProvisioningRepository = Lens.lens (\UpdateAccountSettings' {deletePipelineProvisioningRepository} -> deletePipelineProvisioningRepository) (\s@UpdateAccountSettings' {} a -> s {deletePipelineProvisioningRepository = a} :: UpdateAccountSettings)

-- | A linked repository for pipeline provisioning. Specify it if you have
-- environments configured for self-managed provisioning with services that
-- include pipelines. A linked repository is a repository that has been
-- registered with Proton. For more information, see CreateRepository.
--
-- To remove a previously configured repository, set
-- @deletePipelineProvisioningRepository@ to @true@, and don\'t set
-- @pipelineProvisioningRepository@.
updateAccountSettings_pipelineProvisioningRepository :: Lens.Lens' UpdateAccountSettings (Prelude.Maybe RepositoryBranchInput)
updateAccountSettings_pipelineProvisioningRepository = Lens.lens (\UpdateAccountSettings' {pipelineProvisioningRepository} -> pipelineProvisioningRepository) (\s@UpdateAccountSettings' {} a -> s {pipelineProvisioningRepository = a} :: UpdateAccountSettings)

-- | The Amazon Resource Name (ARN) of the service role you want to use for
-- provisioning pipelines. Assumed by Proton for Amazon Web
-- Services-managed provisioning, and by customer-owned automation for
-- self-managed provisioning.
--
-- To remove a previously configured ARN, specify an empty string.
updateAccountSettings_pipelineServiceRoleArn :: Lens.Lens' UpdateAccountSettings (Prelude.Maybe Prelude.Text)
updateAccountSettings_pipelineServiceRoleArn = Lens.lens (\UpdateAccountSettings' {pipelineServiceRoleArn} -> pipelineServiceRoleArn) (\s@UpdateAccountSettings' {} a -> s {pipelineServiceRoleArn = a} :: UpdateAccountSettings)

-- | The Amazon Resource Name (ARN) of the service role you want to use for
-- provisioning pipelines. Proton assumes this role for CodeBuild-based
-- provisioning.
updateAccountSettings_pipelineCodebuildRoleArn :: Lens.Lens' UpdateAccountSettings (Prelude.Maybe Prelude.Text)
updateAccountSettings_pipelineCodebuildRoleArn = Lens.lens (\UpdateAccountSettings' {pipelineCodebuildRoleArn} -> pipelineCodebuildRoleArn) (\s@UpdateAccountSettings' {} a -> s {pipelineCodebuildRoleArn = a} :: UpdateAccountSettings)

instance Core.AWSRequest UpdateAccountSettings where
  type
    AWSResponse UpdateAccountSettings =
      UpdateAccountSettingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAccountSettingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "accountSettings")
      )

instance Prelude.Hashable UpdateAccountSettings where
  hashWithSalt _salt UpdateAccountSettings' {..} =
    _salt
      `Prelude.hashWithSalt` deletePipelineProvisioningRepository
      `Prelude.hashWithSalt` pipelineProvisioningRepository
      `Prelude.hashWithSalt` pipelineServiceRoleArn
      `Prelude.hashWithSalt` pipelineCodebuildRoleArn

instance Prelude.NFData UpdateAccountSettings where
  rnf UpdateAccountSettings' {..} =
    Prelude.rnf deletePipelineProvisioningRepository
      `Prelude.seq` Prelude.rnf pipelineProvisioningRepository
      `Prelude.seq` Prelude.rnf pipelineServiceRoleArn
      `Prelude.seq` Prelude.rnf pipelineCodebuildRoleArn

instance Data.ToHeaders UpdateAccountSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.UpdateAccountSettings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateAccountSettings where
  toJSON UpdateAccountSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("deletePipelineProvisioningRepository" Data..=)
              Prelude.<$> deletePipelineProvisioningRepository,
            ("pipelineProvisioningRepository" Data..=)
              Prelude.<$> pipelineProvisioningRepository,
            ("pipelineServiceRoleArn" Data..=)
              Prelude.<$> pipelineServiceRoleArn,
            ("pipelineCodebuildRoleArn" Data..=)
              Prelude.<$> pipelineCodebuildRoleArn
          ]
      )

instance Data.ToPath UpdateAccountSettings where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateAccountSettings where
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

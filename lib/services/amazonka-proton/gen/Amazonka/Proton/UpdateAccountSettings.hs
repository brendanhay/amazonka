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
-- Update the AWS Proton pipeline service account settings.
module Amazonka.Proton.UpdateAccountSettings
  ( -- * Creating a Request
    UpdateAccountSettings (..),
    newUpdateAccountSettings,

    -- * Request Lenses
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
  { -- | The Amazon Resource Name (ARN) of the AWS Proton pipeline service role.
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
-- 'pipelineServiceRoleArn', 'updateAccountSettings_pipelineServiceRoleArn' - The Amazon Resource Name (ARN) of the AWS Proton pipeline service role.
newUpdateAccountSettings ::
  UpdateAccountSettings
newUpdateAccountSettings =
  UpdateAccountSettings'
    { pipelineServiceRoleArn =
        Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the AWS Proton pipeline service role.
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
    _salt `Prelude.hashWithSalt` pipelineServiceRoleArn

instance Prelude.NFData UpdateAccountSettings where
  rnf UpdateAccountSettings' {..} =
    Prelude.rnf pipelineServiceRoleArn

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
          [ ("pipelineServiceRoleArn" Core..=)
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
    -- | The AWS Proton pipeline service role detail data that\'s returned by AWS
    -- Proton.
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
-- 'accountSettings', 'updateAccountSettingsResponse_accountSettings' - The AWS Proton pipeline service role detail data that\'s returned by AWS
-- Proton.
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

-- | The AWS Proton pipeline service role detail data that\'s returned by AWS
-- Proton.
updateAccountSettingsResponse_accountSettings :: Lens.Lens' UpdateAccountSettingsResponse AccountSettings
updateAccountSettingsResponse_accountSettings = Lens.lens (\UpdateAccountSettingsResponse' {accountSettings} -> accountSettings) (\s@UpdateAccountSettingsResponse' {} a -> s {accountSettings = a} :: UpdateAccountSettingsResponse)

instance Prelude.NFData UpdateAccountSettingsResponse where
  rnf UpdateAccountSettingsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf accountSettings

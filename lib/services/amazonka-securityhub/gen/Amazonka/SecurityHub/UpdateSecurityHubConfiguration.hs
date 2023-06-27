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
-- Module      : Amazonka.SecurityHub.UpdateSecurityHubConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates configuration options for Security Hub.
module Amazonka.SecurityHub.UpdateSecurityHubConfiguration
  ( -- * Creating a Request
    UpdateSecurityHubConfiguration (..),
    newUpdateSecurityHubConfiguration,

    -- * Request Lenses
    updateSecurityHubConfiguration_autoEnableControls,
    updateSecurityHubConfiguration_controlFindingGenerator,

    -- * Destructuring the Response
    UpdateSecurityHubConfigurationResponse (..),
    newUpdateSecurityHubConfigurationResponse,

    -- * Response Lenses
    updateSecurityHubConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newUpdateSecurityHubConfiguration' smart constructor.
data UpdateSecurityHubConfiguration = UpdateSecurityHubConfiguration'
  { -- | Whether to automatically enable new controls when they are added to
    -- standards that are enabled.
    --
    -- By default, this is set to @true@, and new controls are enabled
    -- automatically. To not automatically enable new controls, set this to
    -- @false@.
    autoEnableControls :: Prelude.Maybe Prelude.Bool,
    -- | Updates whether the calling account has consolidated control findings
    -- turned on. If the value for this field is set to @SECURITY_CONTROL@,
    -- Security Hub generates a single finding for a control check even when
    -- the check applies to multiple enabled standards.
    --
    -- If the value for this field is set to @STANDARD_CONTROL@, Security Hub
    -- generates separate findings for a control check when the check applies
    -- to multiple enabled standards.
    --
    -- For accounts that are part of an organization, this value can only be
    -- updated in the administrator account.
    controlFindingGenerator :: Prelude.Maybe ControlFindingGenerator
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSecurityHubConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoEnableControls', 'updateSecurityHubConfiguration_autoEnableControls' - Whether to automatically enable new controls when they are added to
-- standards that are enabled.
--
-- By default, this is set to @true@, and new controls are enabled
-- automatically. To not automatically enable new controls, set this to
-- @false@.
--
-- 'controlFindingGenerator', 'updateSecurityHubConfiguration_controlFindingGenerator' - Updates whether the calling account has consolidated control findings
-- turned on. If the value for this field is set to @SECURITY_CONTROL@,
-- Security Hub generates a single finding for a control check even when
-- the check applies to multiple enabled standards.
--
-- If the value for this field is set to @STANDARD_CONTROL@, Security Hub
-- generates separate findings for a control check when the check applies
-- to multiple enabled standards.
--
-- For accounts that are part of an organization, this value can only be
-- updated in the administrator account.
newUpdateSecurityHubConfiguration ::
  UpdateSecurityHubConfiguration
newUpdateSecurityHubConfiguration =
  UpdateSecurityHubConfiguration'
    { autoEnableControls =
        Prelude.Nothing,
      controlFindingGenerator = Prelude.Nothing
    }

-- | Whether to automatically enable new controls when they are added to
-- standards that are enabled.
--
-- By default, this is set to @true@, and new controls are enabled
-- automatically. To not automatically enable new controls, set this to
-- @false@.
updateSecurityHubConfiguration_autoEnableControls :: Lens.Lens' UpdateSecurityHubConfiguration (Prelude.Maybe Prelude.Bool)
updateSecurityHubConfiguration_autoEnableControls = Lens.lens (\UpdateSecurityHubConfiguration' {autoEnableControls} -> autoEnableControls) (\s@UpdateSecurityHubConfiguration' {} a -> s {autoEnableControls = a} :: UpdateSecurityHubConfiguration)

-- | Updates whether the calling account has consolidated control findings
-- turned on. If the value for this field is set to @SECURITY_CONTROL@,
-- Security Hub generates a single finding for a control check even when
-- the check applies to multiple enabled standards.
--
-- If the value for this field is set to @STANDARD_CONTROL@, Security Hub
-- generates separate findings for a control check when the check applies
-- to multiple enabled standards.
--
-- For accounts that are part of an organization, this value can only be
-- updated in the administrator account.
updateSecurityHubConfiguration_controlFindingGenerator :: Lens.Lens' UpdateSecurityHubConfiguration (Prelude.Maybe ControlFindingGenerator)
updateSecurityHubConfiguration_controlFindingGenerator = Lens.lens (\UpdateSecurityHubConfiguration' {controlFindingGenerator} -> controlFindingGenerator) (\s@UpdateSecurityHubConfiguration' {} a -> s {controlFindingGenerator = a} :: UpdateSecurityHubConfiguration)

instance
  Core.AWSRequest
    UpdateSecurityHubConfiguration
  where
  type
    AWSResponse UpdateSecurityHubConfiguration =
      UpdateSecurityHubConfigurationResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateSecurityHubConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateSecurityHubConfiguration
  where
  hashWithSalt
    _salt
    UpdateSecurityHubConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` autoEnableControls
        `Prelude.hashWithSalt` controlFindingGenerator

instance
  Prelude.NFData
    UpdateSecurityHubConfiguration
  where
  rnf UpdateSecurityHubConfiguration' {..} =
    Prelude.rnf autoEnableControls
      `Prelude.seq` Prelude.rnf controlFindingGenerator

instance
  Data.ToHeaders
    UpdateSecurityHubConfiguration
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

instance Data.ToJSON UpdateSecurityHubConfiguration where
  toJSON UpdateSecurityHubConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AutoEnableControls" Data..=)
              Prelude.<$> autoEnableControls,
            ("ControlFindingGenerator" Data..=)
              Prelude.<$> controlFindingGenerator
          ]
      )

instance Data.ToPath UpdateSecurityHubConfiguration where
  toPath = Prelude.const "/accounts"

instance Data.ToQuery UpdateSecurityHubConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSecurityHubConfigurationResponse' smart constructor.
data UpdateSecurityHubConfigurationResponse = UpdateSecurityHubConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSecurityHubConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateSecurityHubConfigurationResponse_httpStatus' - The response's http status code.
newUpdateSecurityHubConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSecurityHubConfigurationResponse
newUpdateSecurityHubConfigurationResponse
  pHttpStatus_ =
    UpdateSecurityHubConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateSecurityHubConfigurationResponse_httpStatus :: Lens.Lens' UpdateSecurityHubConfigurationResponse Prelude.Int
updateSecurityHubConfigurationResponse_httpStatus = Lens.lens (\UpdateSecurityHubConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateSecurityHubConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateSecurityHubConfigurationResponse)

instance
  Prelude.NFData
    UpdateSecurityHubConfigurationResponse
  where
  rnf UpdateSecurityHubConfigurationResponse' {..} =
    Prelude.rnf httpStatus

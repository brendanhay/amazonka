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
-- Module      : Amazonka.SecurityHub.UpdateOrganizationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to update the configuration related to Organizations. Can only be
-- called from a Security Hub administrator account.
module Amazonka.SecurityHub.UpdateOrganizationConfiguration
  ( -- * Creating a Request
    UpdateOrganizationConfiguration (..),
    newUpdateOrganizationConfiguration,

    -- * Request Lenses
    updateOrganizationConfiguration_autoEnableStandards,
    updateOrganizationConfiguration_autoEnable,

    -- * Destructuring the Response
    UpdateOrganizationConfigurationResponse (..),
    newUpdateOrganizationConfigurationResponse,

    -- * Response Lenses
    updateOrganizationConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newUpdateOrganizationConfiguration' smart constructor.
data UpdateOrganizationConfiguration = UpdateOrganizationConfiguration'
  { -- | Whether to automatically enable Security Hub
    -- <https://docs.aws.amazon.com/securityhub/latest/userguide/securityhub-standards-enable-disable.html default standards>
    -- for new member accounts in the organization.
    --
    -- By default, this parameter is equal to @DEFAULT@, and new member
    -- accounts are automatically enabled with default Security Hub standards.
    --
    -- To opt out of enabling default standards for new member accounts, set
    -- this parameter equal to @NONE@.
    autoEnableStandards :: Prelude.Maybe AutoEnableStandards,
    -- | Whether to automatically enable Security Hub for new accounts in the
    -- organization.
    --
    -- By default, this is @false@, and new accounts are not added
    -- automatically.
    --
    -- To automatically enable Security Hub for new accounts, set this to
    -- @true@.
    autoEnable :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateOrganizationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoEnableStandards', 'updateOrganizationConfiguration_autoEnableStandards' - Whether to automatically enable Security Hub
-- <https://docs.aws.amazon.com/securityhub/latest/userguide/securityhub-standards-enable-disable.html default standards>
-- for new member accounts in the organization.
--
-- By default, this parameter is equal to @DEFAULT@, and new member
-- accounts are automatically enabled with default Security Hub standards.
--
-- To opt out of enabling default standards for new member accounts, set
-- this parameter equal to @NONE@.
--
-- 'autoEnable', 'updateOrganizationConfiguration_autoEnable' - Whether to automatically enable Security Hub for new accounts in the
-- organization.
--
-- By default, this is @false@, and new accounts are not added
-- automatically.
--
-- To automatically enable Security Hub for new accounts, set this to
-- @true@.
newUpdateOrganizationConfiguration ::
  -- | 'autoEnable'
  Prelude.Bool ->
  UpdateOrganizationConfiguration
newUpdateOrganizationConfiguration pAutoEnable_ =
  UpdateOrganizationConfiguration'
    { autoEnableStandards =
        Prelude.Nothing,
      autoEnable = pAutoEnable_
    }

-- | Whether to automatically enable Security Hub
-- <https://docs.aws.amazon.com/securityhub/latest/userguide/securityhub-standards-enable-disable.html default standards>
-- for new member accounts in the organization.
--
-- By default, this parameter is equal to @DEFAULT@, and new member
-- accounts are automatically enabled with default Security Hub standards.
--
-- To opt out of enabling default standards for new member accounts, set
-- this parameter equal to @NONE@.
updateOrganizationConfiguration_autoEnableStandards :: Lens.Lens' UpdateOrganizationConfiguration (Prelude.Maybe AutoEnableStandards)
updateOrganizationConfiguration_autoEnableStandards = Lens.lens (\UpdateOrganizationConfiguration' {autoEnableStandards} -> autoEnableStandards) (\s@UpdateOrganizationConfiguration' {} a -> s {autoEnableStandards = a} :: UpdateOrganizationConfiguration)

-- | Whether to automatically enable Security Hub for new accounts in the
-- organization.
--
-- By default, this is @false@, and new accounts are not added
-- automatically.
--
-- To automatically enable Security Hub for new accounts, set this to
-- @true@.
updateOrganizationConfiguration_autoEnable :: Lens.Lens' UpdateOrganizationConfiguration Prelude.Bool
updateOrganizationConfiguration_autoEnable = Lens.lens (\UpdateOrganizationConfiguration' {autoEnable} -> autoEnable) (\s@UpdateOrganizationConfiguration' {} a -> s {autoEnable = a} :: UpdateOrganizationConfiguration)

instance
  Core.AWSRequest
    UpdateOrganizationConfiguration
  where
  type
    AWSResponse UpdateOrganizationConfiguration =
      UpdateOrganizationConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateOrganizationConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateOrganizationConfiguration
  where
  hashWithSalt
    _salt
    UpdateOrganizationConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` autoEnableStandards
        `Prelude.hashWithSalt` autoEnable

instance
  Prelude.NFData
    UpdateOrganizationConfiguration
  where
  rnf UpdateOrganizationConfiguration' {..} =
    Prelude.rnf autoEnableStandards `Prelude.seq`
      Prelude.rnf autoEnable

instance
  Data.ToHeaders
    UpdateOrganizationConfiguration
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

instance Data.ToJSON UpdateOrganizationConfiguration where
  toJSON UpdateOrganizationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AutoEnableStandards" Data..=)
              Prelude.<$> autoEnableStandards,
            Prelude.Just ("AutoEnable" Data..= autoEnable)
          ]
      )

instance Data.ToPath UpdateOrganizationConfiguration where
  toPath = Prelude.const "/organization/configuration"

instance Data.ToQuery UpdateOrganizationConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateOrganizationConfigurationResponse' smart constructor.
data UpdateOrganizationConfigurationResponse = UpdateOrganizationConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateOrganizationConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateOrganizationConfigurationResponse_httpStatus' - The response's http status code.
newUpdateOrganizationConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateOrganizationConfigurationResponse
newUpdateOrganizationConfigurationResponse
  pHttpStatus_ =
    UpdateOrganizationConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateOrganizationConfigurationResponse_httpStatus :: Lens.Lens' UpdateOrganizationConfigurationResponse Prelude.Int
updateOrganizationConfigurationResponse_httpStatus = Lens.lens (\UpdateOrganizationConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateOrganizationConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateOrganizationConfigurationResponse)

instance
  Prelude.NFData
    UpdateOrganizationConfigurationResponse
  where
  rnf UpdateOrganizationConfigurationResponse' {..} =
    Prelude.rnf httpStatus

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
-- Module      : Amazonka.ResourceGroups.UpdateAccountSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Turns on or turns off optional features in Resource Groups.
--
-- The preceding example shows that the request to turn on group lifecycle
-- events is @IN_PROGRESS@. You can call the GetAccountSettings operation
-- to check for completion by looking for @GroupLifecycleEventsStatus@ to
-- change to @ACTIVE@.
module Amazonka.ResourceGroups.UpdateAccountSettings
  ( -- * Creating a Request
    UpdateAccountSettings (..),
    newUpdateAccountSettings,

    -- * Request Lenses
    updateAccountSettings_groupLifecycleEventsDesiredStatus,

    -- * Destructuring the Response
    UpdateAccountSettingsResponse (..),
    newUpdateAccountSettingsResponse,

    -- * Response Lenses
    updateAccountSettingsResponse_accountSettings,
    updateAccountSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResourceGroups.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAccountSettings' smart constructor.
data UpdateAccountSettings = UpdateAccountSettings'
  { -- | Specifies whether you want to turn
    -- <https://docs.aws.amazon.com/ARG/latest/userguide/monitor-groups.html group lifecycle events>
    -- on or off.
    groupLifecycleEventsDesiredStatus :: Prelude.Maybe GroupLifecycleEventsDesiredStatus
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
-- 'groupLifecycleEventsDesiredStatus', 'updateAccountSettings_groupLifecycleEventsDesiredStatus' - Specifies whether you want to turn
-- <https://docs.aws.amazon.com/ARG/latest/userguide/monitor-groups.html group lifecycle events>
-- on or off.
newUpdateAccountSettings ::
  UpdateAccountSettings
newUpdateAccountSettings =
  UpdateAccountSettings'
    { groupLifecycleEventsDesiredStatus =
        Prelude.Nothing
    }

-- | Specifies whether you want to turn
-- <https://docs.aws.amazon.com/ARG/latest/userguide/monitor-groups.html group lifecycle events>
-- on or off.
updateAccountSettings_groupLifecycleEventsDesiredStatus :: Lens.Lens' UpdateAccountSettings (Prelude.Maybe GroupLifecycleEventsDesiredStatus)
updateAccountSettings_groupLifecycleEventsDesiredStatus = Lens.lens (\UpdateAccountSettings' {groupLifecycleEventsDesiredStatus} -> groupLifecycleEventsDesiredStatus) (\s@UpdateAccountSettings' {} a -> s {groupLifecycleEventsDesiredStatus = a} :: UpdateAccountSettings)

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
            Prelude.<$> (x Data..?> "AccountSettings")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAccountSettings where
  hashWithSalt _salt UpdateAccountSettings' {..} =
    _salt
      `Prelude.hashWithSalt` groupLifecycleEventsDesiredStatus

instance Prelude.NFData UpdateAccountSettings where
  rnf UpdateAccountSettings' {..} =
    Prelude.rnf groupLifecycleEventsDesiredStatus

instance Data.ToHeaders UpdateAccountSettings where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateAccountSettings where
  toJSON UpdateAccountSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GroupLifecycleEventsDesiredStatus" Data..=)
              Prelude.<$> groupLifecycleEventsDesiredStatus
          ]
      )

instance Data.ToPath UpdateAccountSettings where
  toPath = Prelude.const "/update-account-settings"

instance Data.ToQuery UpdateAccountSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAccountSettingsResponse' smart constructor.
data UpdateAccountSettingsResponse = UpdateAccountSettingsResponse'
  { -- | A structure that displays the status of the optional features in the
    -- account.
    accountSettings :: Prelude.Maybe AccountSettings,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'accountSettings', 'updateAccountSettingsResponse_accountSettings' - A structure that displays the status of the optional features in the
-- account.
--
-- 'httpStatus', 'updateAccountSettingsResponse_httpStatus' - The response's http status code.
newUpdateAccountSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateAccountSettingsResponse
newUpdateAccountSettingsResponse pHttpStatus_ =
  UpdateAccountSettingsResponse'
    { accountSettings =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure that displays the status of the optional features in the
-- account.
updateAccountSettingsResponse_accountSettings :: Lens.Lens' UpdateAccountSettingsResponse (Prelude.Maybe AccountSettings)
updateAccountSettingsResponse_accountSettings = Lens.lens (\UpdateAccountSettingsResponse' {accountSettings} -> accountSettings) (\s@UpdateAccountSettingsResponse' {} a -> s {accountSettings = a} :: UpdateAccountSettingsResponse)

-- | The response's http status code.
updateAccountSettingsResponse_httpStatus :: Lens.Lens' UpdateAccountSettingsResponse Prelude.Int
updateAccountSettingsResponse_httpStatus = Lens.lens (\UpdateAccountSettingsResponse' {httpStatus} -> httpStatus) (\s@UpdateAccountSettingsResponse' {} a -> s {httpStatus = a} :: UpdateAccountSettingsResponse)

instance Prelude.NFData UpdateAccountSettingsResponse where
  rnf UpdateAccountSettingsResponse' {..} =
    Prelude.rnf accountSettings
      `Prelude.seq` Prelude.rnf httpStatus

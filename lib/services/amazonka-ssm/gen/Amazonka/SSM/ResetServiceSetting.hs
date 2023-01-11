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
-- Module      : Amazonka.SSM.ResetServiceSetting
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- @ServiceSetting@ is an account-level setting for an Amazon Web Services
-- service. This setting defines how a user interacts with or uses a
-- service or a feature of a service. For example, if an Amazon Web
-- Services service charges money to the account based on feature or
-- service usage, then the Amazon Web Services service team might create a
-- default setting of \"false\". This means the user can\'t use this
-- feature unless they change the setting to \"true\" and intentionally opt
-- in for a paid feature.
--
-- Services map a @SettingId@ object to a setting value. Amazon Web
-- Services services teams define the default value for a @SettingId@. You
-- can\'t create a new @SettingId@, but you can overwrite the default value
-- if you have the @ssm:UpdateServiceSetting@ permission for the setting.
-- Use the GetServiceSetting API operation to view the current value. Use
-- the UpdateServiceSetting API operation to change the default setting.
--
-- Reset the service setting for the account to the default value as
-- provisioned by the Amazon Web Services service team.
module Amazonka.SSM.ResetServiceSetting
  ( -- * Creating a Request
    ResetServiceSetting (..),
    newResetServiceSetting,

    -- * Request Lenses
    resetServiceSetting_settingId,

    -- * Destructuring the Response
    ResetServiceSettingResponse (..),
    newResetServiceSettingResponse,

    -- * Response Lenses
    resetServiceSettingResponse_serviceSetting,
    resetServiceSettingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | The request body of the ResetServiceSetting API operation.
--
-- /See:/ 'newResetServiceSetting' smart constructor.
data ResetServiceSetting = ResetServiceSetting'
  { -- | The Amazon Resource Name (ARN) of the service setting to reset. The
    -- setting ID can be one of the following.
    --
    -- -   @\/ssm\/automation\/customer-script-log-destination@
    --
    -- -   @\/ssm\/automation\/customer-script-log-group-name@
    --
    -- -   @\/ssm\/documents\/console\/public-sharing-permission@
    --
    -- -   @\/ssm\/managed-instance\/activation-tier@
    --
    -- -   @\/ssm\/opsinsights\/opscenter@
    --
    -- -   @\/ssm\/parameter-store\/default-parameter-tier@
    --
    -- -   @\/ssm\/parameter-store\/high-throughput-enabled@
    settingId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetServiceSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'settingId', 'resetServiceSetting_settingId' - The Amazon Resource Name (ARN) of the service setting to reset. The
-- setting ID can be one of the following.
--
-- -   @\/ssm\/automation\/customer-script-log-destination@
--
-- -   @\/ssm\/automation\/customer-script-log-group-name@
--
-- -   @\/ssm\/documents\/console\/public-sharing-permission@
--
-- -   @\/ssm\/managed-instance\/activation-tier@
--
-- -   @\/ssm\/opsinsights\/opscenter@
--
-- -   @\/ssm\/parameter-store\/default-parameter-tier@
--
-- -   @\/ssm\/parameter-store\/high-throughput-enabled@
newResetServiceSetting ::
  -- | 'settingId'
  Prelude.Text ->
  ResetServiceSetting
newResetServiceSetting pSettingId_ =
  ResetServiceSetting' {settingId = pSettingId_}

-- | The Amazon Resource Name (ARN) of the service setting to reset. The
-- setting ID can be one of the following.
--
-- -   @\/ssm\/automation\/customer-script-log-destination@
--
-- -   @\/ssm\/automation\/customer-script-log-group-name@
--
-- -   @\/ssm\/documents\/console\/public-sharing-permission@
--
-- -   @\/ssm\/managed-instance\/activation-tier@
--
-- -   @\/ssm\/opsinsights\/opscenter@
--
-- -   @\/ssm\/parameter-store\/default-parameter-tier@
--
-- -   @\/ssm\/parameter-store\/high-throughput-enabled@
resetServiceSetting_settingId :: Lens.Lens' ResetServiceSetting Prelude.Text
resetServiceSetting_settingId = Lens.lens (\ResetServiceSetting' {settingId} -> settingId) (\s@ResetServiceSetting' {} a -> s {settingId = a} :: ResetServiceSetting)

instance Core.AWSRequest ResetServiceSetting where
  type
    AWSResponse ResetServiceSetting =
      ResetServiceSettingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ResetServiceSettingResponse'
            Prelude.<$> (x Data..?> "ServiceSetting")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ResetServiceSetting where
  hashWithSalt _salt ResetServiceSetting' {..} =
    _salt `Prelude.hashWithSalt` settingId

instance Prelude.NFData ResetServiceSetting where
  rnf ResetServiceSetting' {..} = Prelude.rnf settingId

instance Data.ToHeaders ResetServiceSetting where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.ResetServiceSetting" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ResetServiceSetting where
  toJSON ResetServiceSetting' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("SettingId" Data..= settingId)]
      )

instance Data.ToPath ResetServiceSetting where
  toPath = Prelude.const "/"

instance Data.ToQuery ResetServiceSetting where
  toQuery = Prelude.const Prelude.mempty

-- | The result body of the ResetServiceSetting API operation.
--
-- /See:/ 'newResetServiceSettingResponse' smart constructor.
data ResetServiceSettingResponse = ResetServiceSettingResponse'
  { -- | The current, effective service setting after calling the
    -- ResetServiceSetting API operation.
    serviceSetting :: Prelude.Maybe ServiceSetting,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetServiceSettingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceSetting', 'resetServiceSettingResponse_serviceSetting' - The current, effective service setting after calling the
-- ResetServiceSetting API operation.
--
-- 'httpStatus', 'resetServiceSettingResponse_httpStatus' - The response's http status code.
newResetServiceSettingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ResetServiceSettingResponse
newResetServiceSettingResponse pHttpStatus_ =
  ResetServiceSettingResponse'
    { serviceSetting =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current, effective service setting after calling the
-- ResetServiceSetting API operation.
resetServiceSettingResponse_serviceSetting :: Lens.Lens' ResetServiceSettingResponse (Prelude.Maybe ServiceSetting)
resetServiceSettingResponse_serviceSetting = Lens.lens (\ResetServiceSettingResponse' {serviceSetting} -> serviceSetting) (\s@ResetServiceSettingResponse' {} a -> s {serviceSetting = a} :: ResetServiceSettingResponse)

-- | The response's http status code.
resetServiceSettingResponse_httpStatus :: Lens.Lens' ResetServiceSettingResponse Prelude.Int
resetServiceSettingResponse_httpStatus = Lens.lens (\ResetServiceSettingResponse' {httpStatus} -> httpStatus) (\s@ResetServiceSettingResponse' {} a -> s {httpStatus = a} :: ResetServiceSettingResponse)

instance Prelude.NFData ResetServiceSettingResponse where
  rnf ResetServiceSettingResponse' {..} =
    Prelude.rnf serviceSetting
      `Prelude.seq` Prelude.rnf httpStatus

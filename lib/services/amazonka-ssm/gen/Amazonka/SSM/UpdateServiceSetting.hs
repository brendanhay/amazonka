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
-- Module      : Amazonka.SSM.UpdateServiceSetting
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
-- Use the GetServiceSetting API operation to view the current value. Or,
-- use the ResetServiceSetting to change the value back to the original
-- value defined by the Amazon Web Services service team.
--
-- Update the service setting for the account.
module Amazonka.SSM.UpdateServiceSetting
  ( -- * Creating a Request
    UpdateServiceSetting (..),
    newUpdateServiceSetting,

    -- * Request Lenses
    updateServiceSetting_settingId,
    updateServiceSetting_settingValue,

    -- * Destructuring the Response
    UpdateServiceSettingResponse (..),
    newUpdateServiceSettingResponse,

    -- * Response Lenses
    updateServiceSettingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | The request body of the UpdateServiceSetting API operation.
--
-- /See:/ 'newUpdateServiceSetting' smart constructor.
data UpdateServiceSetting = UpdateServiceSetting'
  { -- | The Amazon Resource Name (ARN) of the service setting to reset. For
    -- example,
    -- @arn:aws:ssm:us-east-1:111122223333:servicesetting\/ssm\/parameter-store\/high-throughput-enabled@.
    -- The setting ID can be one of the following.
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
    settingId :: Prelude.Text,
    -- | The new value to specify for the service setting. The following list
    -- specifies the available values for each setting.
    --
    -- -   @\/ssm\/automation\/customer-script-log-destination@: @CloudWatch@
    --
    -- -   @\/ssm\/automation\/customer-script-log-group-name@: the name of an
    --     Amazon CloudWatch Logs log group
    --
    -- -   @\/ssm\/documents\/console\/public-sharing-permission@: @Enable@ or
    --     @Disable@
    --
    -- -   @\/ssm\/managed-instance\/activation-tier@: @standard@ or @advanced@
    --
    -- -   @\/ssm\/opsinsights\/opscenter@: @Enabled@ or @Disabled@
    --
    -- -   @\/ssm\/parameter-store\/default-parameter-tier@: @Standard@,
    --     @Advanced@, @Intelligent-Tiering@
    --
    -- -   @\/ssm\/parameter-store\/high-throughput-enabled@: @true@ or @false@
    settingValue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateServiceSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'settingId', 'updateServiceSetting_settingId' - The Amazon Resource Name (ARN) of the service setting to reset. For
-- example,
-- @arn:aws:ssm:us-east-1:111122223333:servicesetting\/ssm\/parameter-store\/high-throughput-enabled@.
-- The setting ID can be one of the following.
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
--
-- 'settingValue', 'updateServiceSetting_settingValue' - The new value to specify for the service setting. The following list
-- specifies the available values for each setting.
--
-- -   @\/ssm\/automation\/customer-script-log-destination@: @CloudWatch@
--
-- -   @\/ssm\/automation\/customer-script-log-group-name@: the name of an
--     Amazon CloudWatch Logs log group
--
-- -   @\/ssm\/documents\/console\/public-sharing-permission@: @Enable@ or
--     @Disable@
--
-- -   @\/ssm\/managed-instance\/activation-tier@: @standard@ or @advanced@
--
-- -   @\/ssm\/opsinsights\/opscenter@: @Enabled@ or @Disabled@
--
-- -   @\/ssm\/parameter-store\/default-parameter-tier@: @Standard@,
--     @Advanced@, @Intelligent-Tiering@
--
-- -   @\/ssm\/parameter-store\/high-throughput-enabled@: @true@ or @false@
newUpdateServiceSetting ::
  -- | 'settingId'
  Prelude.Text ->
  -- | 'settingValue'
  Prelude.Text ->
  UpdateServiceSetting
newUpdateServiceSetting pSettingId_ pSettingValue_ =
  UpdateServiceSetting'
    { settingId = pSettingId_,
      settingValue = pSettingValue_
    }

-- | The Amazon Resource Name (ARN) of the service setting to reset. For
-- example,
-- @arn:aws:ssm:us-east-1:111122223333:servicesetting\/ssm\/parameter-store\/high-throughput-enabled@.
-- The setting ID can be one of the following.
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
updateServiceSetting_settingId :: Lens.Lens' UpdateServiceSetting Prelude.Text
updateServiceSetting_settingId = Lens.lens (\UpdateServiceSetting' {settingId} -> settingId) (\s@UpdateServiceSetting' {} a -> s {settingId = a} :: UpdateServiceSetting)

-- | The new value to specify for the service setting. The following list
-- specifies the available values for each setting.
--
-- -   @\/ssm\/automation\/customer-script-log-destination@: @CloudWatch@
--
-- -   @\/ssm\/automation\/customer-script-log-group-name@: the name of an
--     Amazon CloudWatch Logs log group
--
-- -   @\/ssm\/documents\/console\/public-sharing-permission@: @Enable@ or
--     @Disable@
--
-- -   @\/ssm\/managed-instance\/activation-tier@: @standard@ or @advanced@
--
-- -   @\/ssm\/opsinsights\/opscenter@: @Enabled@ or @Disabled@
--
-- -   @\/ssm\/parameter-store\/default-parameter-tier@: @Standard@,
--     @Advanced@, @Intelligent-Tiering@
--
-- -   @\/ssm\/parameter-store\/high-throughput-enabled@: @true@ or @false@
updateServiceSetting_settingValue :: Lens.Lens' UpdateServiceSetting Prelude.Text
updateServiceSetting_settingValue = Lens.lens (\UpdateServiceSetting' {settingValue} -> settingValue) (\s@UpdateServiceSetting' {} a -> s {settingValue = a} :: UpdateServiceSetting)

instance Core.AWSRequest UpdateServiceSetting where
  type
    AWSResponse UpdateServiceSetting =
      UpdateServiceSettingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateServiceSettingResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateServiceSetting where
  hashWithSalt _salt UpdateServiceSetting' {..} =
    _salt `Prelude.hashWithSalt` settingId
      `Prelude.hashWithSalt` settingValue

instance Prelude.NFData UpdateServiceSetting where
  rnf UpdateServiceSetting' {..} =
    Prelude.rnf settingId
      `Prelude.seq` Prelude.rnf settingValue

instance Data.ToHeaders UpdateServiceSetting where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.UpdateServiceSetting" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateServiceSetting where
  toJSON UpdateServiceSetting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SettingId" Data..= settingId),
            Prelude.Just ("SettingValue" Data..= settingValue)
          ]
      )

instance Data.ToPath UpdateServiceSetting where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateServiceSetting where
  toQuery = Prelude.const Prelude.mempty

-- | The result body of the UpdateServiceSetting API operation.
--
-- /See:/ 'newUpdateServiceSettingResponse' smart constructor.
data UpdateServiceSettingResponse = UpdateServiceSettingResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateServiceSettingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateServiceSettingResponse_httpStatus' - The response's http status code.
newUpdateServiceSettingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateServiceSettingResponse
newUpdateServiceSettingResponse pHttpStatus_ =
  UpdateServiceSettingResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateServiceSettingResponse_httpStatus :: Lens.Lens' UpdateServiceSettingResponse Prelude.Int
updateServiceSettingResponse_httpStatus = Lens.lens (\UpdateServiceSettingResponse' {httpStatus} -> httpStatus) (\s@UpdateServiceSettingResponse' {} a -> s {httpStatus = a} :: UpdateServiceSettingResponse)

instance Prelude.NFData UpdateServiceSettingResponse where
  rnf UpdateServiceSettingResponse' {..} =
    Prelude.rnf httpStatus

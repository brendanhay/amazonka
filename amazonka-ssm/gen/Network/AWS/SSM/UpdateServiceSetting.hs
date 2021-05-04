{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SSM.UpdateServiceSetting
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- @ServiceSetting@ is an account-level setting for an AWS service. This
-- setting defines how a user interacts with or uses a service or a feature
-- of a service. For example, if an AWS service charges money to the
-- account based on feature or service usage, then the AWS service team
-- might create a default setting of \"false\". This means the user can\'t
-- use this feature unless they change the setting to \"true\" and
-- intentionally opt in for a paid feature.
--
-- Services map a @SettingId@ object to a setting value. AWS services teams
-- define the default value for a @SettingId@. You can\'t create a new
-- @SettingId@, but you can overwrite the default value if you have the
-- @ssm:UpdateServiceSetting@ permission for the setting. Use the
-- GetServiceSetting API action to view the current value. Or, use the
-- ResetServiceSetting to change the value back to the original value
-- defined by the AWS service team.
--
-- Update the service setting for the account.
module Network.AWS.SSM.UpdateServiceSetting
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | The request body of the UpdateServiceSetting API action.
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
    -- -   @\/ssm\/parameter-store\/default-parameter-tier@
    --
    -- -   @\/ssm\/parameter-store\/high-throughput-enabled@
    --
    -- -   @\/ssm\/managed-instance\/activation-tier@
    settingId :: Prelude.Text,
    -- | The new value to specify for the service setting. For the
    -- @\/ssm\/parameter-store\/default-parameter-tier@ setting ID, the setting
    -- value can be one of the following.
    --
    -- -   Standard
    --
    -- -   Advanced
    --
    -- -   Intelligent-Tiering
    --
    -- For the @\/ssm\/parameter-store\/high-throughput-enabled@, and
    -- @\/ssm\/managed-instance\/activation-tier@ setting IDs, the setting
    -- value can be true or false.
    --
    -- For the @\/ssm\/automation\/customer-script-log-destination@ setting ID,
    -- the setting value can be CloudWatch.
    --
    -- For the @\/ssm\/automation\/customer-script-log-group-name@ setting ID,
    -- the setting value can be the name of a CloudWatch Logs log group.
    settingValue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- -   @\/ssm\/parameter-store\/default-parameter-tier@
--
-- -   @\/ssm\/parameter-store\/high-throughput-enabled@
--
-- -   @\/ssm\/managed-instance\/activation-tier@
--
-- 'settingValue', 'updateServiceSetting_settingValue' - The new value to specify for the service setting. For the
-- @\/ssm\/parameter-store\/default-parameter-tier@ setting ID, the setting
-- value can be one of the following.
--
-- -   Standard
--
-- -   Advanced
--
-- -   Intelligent-Tiering
--
-- For the @\/ssm\/parameter-store\/high-throughput-enabled@, and
-- @\/ssm\/managed-instance\/activation-tier@ setting IDs, the setting
-- value can be true or false.
--
-- For the @\/ssm\/automation\/customer-script-log-destination@ setting ID,
-- the setting value can be CloudWatch.
--
-- For the @\/ssm\/automation\/customer-script-log-group-name@ setting ID,
-- the setting value can be the name of a CloudWatch Logs log group.
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
-- -   @\/ssm\/parameter-store\/default-parameter-tier@
--
-- -   @\/ssm\/parameter-store\/high-throughput-enabled@
--
-- -   @\/ssm\/managed-instance\/activation-tier@
updateServiceSetting_settingId :: Lens.Lens' UpdateServiceSetting Prelude.Text
updateServiceSetting_settingId = Lens.lens (\UpdateServiceSetting' {settingId} -> settingId) (\s@UpdateServiceSetting' {} a -> s {settingId = a} :: UpdateServiceSetting)

-- | The new value to specify for the service setting. For the
-- @\/ssm\/parameter-store\/default-parameter-tier@ setting ID, the setting
-- value can be one of the following.
--
-- -   Standard
--
-- -   Advanced
--
-- -   Intelligent-Tiering
--
-- For the @\/ssm\/parameter-store\/high-throughput-enabled@, and
-- @\/ssm\/managed-instance\/activation-tier@ setting IDs, the setting
-- value can be true or false.
--
-- For the @\/ssm\/automation\/customer-script-log-destination@ setting ID,
-- the setting value can be CloudWatch.
--
-- For the @\/ssm\/automation\/customer-script-log-group-name@ setting ID,
-- the setting value can be the name of a CloudWatch Logs log group.
updateServiceSetting_settingValue :: Lens.Lens' UpdateServiceSetting Prelude.Text
updateServiceSetting_settingValue = Lens.lens (\UpdateServiceSetting' {settingValue} -> settingValue) (\s@UpdateServiceSetting' {} a -> s {settingValue = a} :: UpdateServiceSetting)

instance Prelude.AWSRequest UpdateServiceSetting where
  type
    Rs UpdateServiceSetting =
      UpdateServiceSettingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateServiceSettingResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateServiceSetting

instance Prelude.NFData UpdateServiceSetting

instance Prelude.ToHeaders UpdateServiceSetting where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonSSM.UpdateServiceSetting" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateServiceSetting where
  toJSON UpdateServiceSetting' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SettingId" Prelude..= settingId),
            Prelude.Just
              ("SettingValue" Prelude..= settingValue)
          ]
      )

instance Prelude.ToPath UpdateServiceSetting where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateServiceSetting where
  toQuery = Prelude.const Prelude.mempty

-- | The result body of the UpdateServiceSetting API action.
--
-- /See:/ 'newUpdateServiceSettingResponse' smart constructor.
data UpdateServiceSettingResponse = UpdateServiceSettingResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData UpdateServiceSettingResponse

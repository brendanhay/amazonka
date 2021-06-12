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
-- Module      : Network.AWS.SSM.ResetServiceSetting
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
-- GetServiceSetting API action to view the current value. Use the
-- UpdateServiceSetting API action to change the default setting.
--
-- Reset the service setting for the account to the default value as
-- provisioned by the AWS service team.
module Network.AWS.SSM.ResetServiceSetting
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | The request body of the ResetServiceSetting API action.
--
-- /See:/ 'newResetServiceSetting' smart constructor.
data ResetServiceSetting = ResetServiceSetting'
  { -- | The Amazon Resource Name (ARN) of the service setting to reset. The
    -- setting ID can be @\/ssm\/automation\/customer-script-log-destination@,
    -- @\/ssm\/automation\/customer-script-log-group-name@,
    -- @\/ssm\/parameter-store\/default-parameter-tier@,
    -- @\/ssm\/parameter-store\/high-throughput-enabled@, or
    -- @\/ssm\/managed-instance\/activation-tier@. For example,
    -- @arn:aws:ssm:us-east-1:111122223333:servicesetting\/ssm\/parameter-store\/high-throughput-enabled@.
    settingId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResetServiceSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'settingId', 'resetServiceSetting_settingId' - The Amazon Resource Name (ARN) of the service setting to reset. The
-- setting ID can be @\/ssm\/automation\/customer-script-log-destination@,
-- @\/ssm\/automation\/customer-script-log-group-name@,
-- @\/ssm\/parameter-store\/default-parameter-tier@,
-- @\/ssm\/parameter-store\/high-throughput-enabled@, or
-- @\/ssm\/managed-instance\/activation-tier@. For example,
-- @arn:aws:ssm:us-east-1:111122223333:servicesetting\/ssm\/parameter-store\/high-throughput-enabled@.
newResetServiceSetting ::
  -- | 'settingId'
  Core.Text ->
  ResetServiceSetting
newResetServiceSetting pSettingId_ =
  ResetServiceSetting' {settingId = pSettingId_}

-- | The Amazon Resource Name (ARN) of the service setting to reset. The
-- setting ID can be @\/ssm\/automation\/customer-script-log-destination@,
-- @\/ssm\/automation\/customer-script-log-group-name@,
-- @\/ssm\/parameter-store\/default-parameter-tier@,
-- @\/ssm\/parameter-store\/high-throughput-enabled@, or
-- @\/ssm\/managed-instance\/activation-tier@. For example,
-- @arn:aws:ssm:us-east-1:111122223333:servicesetting\/ssm\/parameter-store\/high-throughput-enabled@.
resetServiceSetting_settingId :: Lens.Lens' ResetServiceSetting Core.Text
resetServiceSetting_settingId = Lens.lens (\ResetServiceSetting' {settingId} -> settingId) (\s@ResetServiceSetting' {} a -> s {settingId = a} :: ResetServiceSetting)

instance Core.AWSRequest ResetServiceSetting where
  type
    AWSResponse ResetServiceSetting =
      ResetServiceSettingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ResetServiceSettingResponse'
            Core.<$> (x Core..?> "ServiceSetting")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ResetServiceSetting

instance Core.NFData ResetServiceSetting

instance Core.ToHeaders ResetServiceSetting where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.ResetServiceSetting" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ResetServiceSetting where
  toJSON ResetServiceSetting' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("SettingId" Core..= settingId)]
      )

instance Core.ToPath ResetServiceSetting where
  toPath = Core.const "/"

instance Core.ToQuery ResetServiceSetting where
  toQuery = Core.const Core.mempty

-- | The result body of the ResetServiceSetting API action.
--
-- /See:/ 'newResetServiceSettingResponse' smart constructor.
data ResetServiceSettingResponse = ResetServiceSettingResponse'
  { -- | The current, effective service setting after calling the
    -- ResetServiceSetting API action.
    serviceSetting :: Core.Maybe ServiceSetting,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResetServiceSettingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceSetting', 'resetServiceSettingResponse_serviceSetting' - The current, effective service setting after calling the
-- ResetServiceSetting API action.
--
-- 'httpStatus', 'resetServiceSettingResponse_httpStatus' - The response's http status code.
newResetServiceSettingResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ResetServiceSettingResponse
newResetServiceSettingResponse pHttpStatus_ =
  ResetServiceSettingResponse'
    { serviceSetting =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current, effective service setting after calling the
-- ResetServiceSetting API action.
resetServiceSettingResponse_serviceSetting :: Lens.Lens' ResetServiceSettingResponse (Core.Maybe ServiceSetting)
resetServiceSettingResponse_serviceSetting = Lens.lens (\ResetServiceSettingResponse' {serviceSetting} -> serviceSetting) (\s@ResetServiceSettingResponse' {} a -> s {serviceSetting = a} :: ResetServiceSettingResponse)

-- | The response's http status code.
resetServiceSettingResponse_httpStatus :: Lens.Lens' ResetServiceSettingResponse Core.Int
resetServiceSettingResponse_httpStatus = Lens.lens (\ResetServiceSettingResponse' {httpStatus} -> httpStatus) (\s@ResetServiceSettingResponse' {} a -> s {httpStatus = a} :: ResetServiceSettingResponse)

instance Core.NFData ResetServiceSettingResponse

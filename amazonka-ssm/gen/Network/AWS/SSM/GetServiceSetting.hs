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
-- Module      : Network.AWS.SSM.GetServiceSetting
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
-- UpdateServiceSetting API action to change the default setting. Or use
-- the ResetServiceSetting to change the value back to the original value
-- defined by the AWS service team.
--
-- Query the current service setting for the account.
module Network.AWS.SSM.GetServiceSetting
  ( -- * Creating a Request
    GetServiceSetting (..),
    newGetServiceSetting,

    -- * Request Lenses
    getServiceSetting_settingId,

    -- * Destructuring the Response
    GetServiceSettingResponse (..),
    newGetServiceSettingResponse,

    -- * Response Lenses
    getServiceSettingResponse_serviceSetting,
    getServiceSettingResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | The request body of the GetServiceSetting API action.
--
-- /See:/ 'newGetServiceSetting' smart constructor.
data GetServiceSetting = GetServiceSetting'
  { -- | The ID of the service setting to get. The setting ID can be
    -- @\/ssm\/automation\/customer-script-log-destination@,
    -- @\/ssm\/automation\/customer-script-log-group-name@,
    -- @\/ssm\/parameter-store\/default-parameter-tier@,
    -- @\/ssm\/parameter-store\/high-throughput-enabled@, or
    -- @\/ssm\/managed-instance\/activation-tier@.
    settingId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'settingId', 'getServiceSetting_settingId' - The ID of the service setting to get. The setting ID can be
-- @\/ssm\/automation\/customer-script-log-destination@,
-- @\/ssm\/automation\/customer-script-log-group-name@,
-- @\/ssm\/parameter-store\/default-parameter-tier@,
-- @\/ssm\/parameter-store\/high-throughput-enabled@, or
-- @\/ssm\/managed-instance\/activation-tier@.
newGetServiceSetting ::
  -- | 'settingId'
  Prelude.Text ->
  GetServiceSetting
newGetServiceSetting pSettingId_ =
  GetServiceSetting' {settingId = pSettingId_}

-- | The ID of the service setting to get. The setting ID can be
-- @\/ssm\/automation\/customer-script-log-destination@,
-- @\/ssm\/automation\/customer-script-log-group-name@,
-- @\/ssm\/parameter-store\/default-parameter-tier@,
-- @\/ssm\/parameter-store\/high-throughput-enabled@, or
-- @\/ssm\/managed-instance\/activation-tier@.
getServiceSetting_settingId :: Lens.Lens' GetServiceSetting Prelude.Text
getServiceSetting_settingId = Lens.lens (\GetServiceSetting' {settingId} -> settingId) (\s@GetServiceSetting' {} a -> s {settingId = a} :: GetServiceSetting)

instance Core.AWSRequest GetServiceSetting where
  type
    AWSResponse GetServiceSetting =
      GetServiceSettingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetServiceSettingResponse'
            Prelude.<$> (x Core..?> "ServiceSetting")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetServiceSetting

instance Prelude.NFData GetServiceSetting

instance Core.ToHeaders GetServiceSetting where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.GetServiceSetting" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetServiceSetting where
  toJSON GetServiceSetting' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("SettingId" Core..= settingId)]
      )

instance Core.ToPath GetServiceSetting where
  toPath = Prelude.const "/"

instance Core.ToQuery GetServiceSetting where
  toQuery = Prelude.const Prelude.mempty

-- | The query result body of the GetServiceSetting API action.
--
-- /See:/ 'newGetServiceSettingResponse' smart constructor.
data GetServiceSettingResponse = GetServiceSettingResponse'
  { -- | The query result of the current service setting.
    serviceSetting :: Prelude.Maybe ServiceSetting,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceSettingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceSetting', 'getServiceSettingResponse_serviceSetting' - The query result of the current service setting.
--
-- 'httpStatus', 'getServiceSettingResponse_httpStatus' - The response's http status code.
newGetServiceSettingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetServiceSettingResponse
newGetServiceSettingResponse pHttpStatus_ =
  GetServiceSettingResponse'
    { serviceSetting =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The query result of the current service setting.
getServiceSettingResponse_serviceSetting :: Lens.Lens' GetServiceSettingResponse (Prelude.Maybe ServiceSetting)
getServiceSettingResponse_serviceSetting = Lens.lens (\GetServiceSettingResponse' {serviceSetting} -> serviceSetting) (\s@GetServiceSettingResponse' {} a -> s {serviceSetting = a} :: GetServiceSettingResponse)

-- | The response's http status code.
getServiceSettingResponse_httpStatus :: Lens.Lens' GetServiceSettingResponse Prelude.Int
getServiceSettingResponse_httpStatus = Lens.lens (\GetServiceSettingResponse' {httpStatus} -> httpStatus) (\s@GetServiceSettingResponse' {} a -> s {httpStatus = a} :: GetServiceSettingResponse)

instance Prelude.NFData GetServiceSettingResponse

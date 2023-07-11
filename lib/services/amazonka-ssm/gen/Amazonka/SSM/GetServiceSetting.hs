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
-- Module      : Amazonka.SSM.GetServiceSetting
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- @ServiceSetting@ is an account-level setting for an Amazon Web Services
-- service. This setting defines how a user interacts with or uses a
-- service or a feature of a service. For example, if an Amazon Web
-- Services service charges money to the account based on feature or
-- service usage, then the Amazon Web Services service team might create a
-- default setting of @false@. This means the user can\'t use this feature
-- unless they change the setting to @true@ and intentionally opt in for a
-- paid feature.
--
-- Services map a @SettingId@ object to a setting value. Amazon Web
-- Services services teams define the default value for a @SettingId@. You
-- can\'t create a new @SettingId@, but you can overwrite the default value
-- if you have the @ssm:UpdateServiceSetting@ permission for the setting.
-- Use the UpdateServiceSetting API operation to change the default
-- setting. Or use the ResetServiceSetting to change the value back to the
-- original value defined by the Amazon Web Services service team.
--
-- Query the current service setting for the Amazon Web Services account.
module Amazonka.SSM.GetServiceSetting
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | The request body of the GetServiceSetting API operation.
--
-- /See:/ 'newGetServiceSetting' smart constructor.
data GetServiceSetting = GetServiceSetting'
  { -- | The ID of the service setting to get. The setting ID can be one of the
    -- following.
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
-- Create a value of 'GetServiceSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'settingId', 'getServiceSetting_settingId' - The ID of the service setting to get. The setting ID can be one of the
-- following.
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
newGetServiceSetting ::
  -- | 'settingId'
  Prelude.Text ->
  GetServiceSetting
newGetServiceSetting pSettingId_ =
  GetServiceSetting' {settingId = pSettingId_}

-- | The ID of the service setting to get. The setting ID can be one of the
-- following.
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
getServiceSetting_settingId :: Lens.Lens' GetServiceSetting Prelude.Text
getServiceSetting_settingId = Lens.lens (\GetServiceSetting' {settingId} -> settingId) (\s@GetServiceSetting' {} a -> s {settingId = a} :: GetServiceSetting)

instance Core.AWSRequest GetServiceSetting where
  type
    AWSResponse GetServiceSetting =
      GetServiceSettingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetServiceSettingResponse'
            Prelude.<$> (x Data..?> "ServiceSetting")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetServiceSetting where
  hashWithSalt _salt GetServiceSetting' {..} =
    _salt `Prelude.hashWithSalt` settingId

instance Prelude.NFData GetServiceSetting where
  rnf GetServiceSetting' {..} = Prelude.rnf settingId

instance Data.ToHeaders GetServiceSetting where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.GetServiceSetting" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetServiceSetting where
  toJSON GetServiceSetting' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("SettingId" Data..= settingId)]
      )

instance Data.ToPath GetServiceSetting where
  toPath = Prelude.const "/"

instance Data.ToQuery GetServiceSetting where
  toQuery = Prelude.const Prelude.mempty

-- | The query result body of the GetServiceSetting API operation.
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

instance Prelude.NFData GetServiceSettingResponse where
  rnf GetServiceSettingResponse' {..} =
    Prelude.rnf serviceSetting
      `Prelude.seq` Prelude.rnf httpStatus

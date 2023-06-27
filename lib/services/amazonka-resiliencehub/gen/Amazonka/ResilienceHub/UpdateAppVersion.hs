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
-- Module      : Amazonka.ResilienceHub.UpdateAppVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the Resilience Hub application version.
--
-- This API updates the Resilience Hub application draft version. To use
-- this information for running resiliency assessments, you must publish
-- the Resilience Hub application using the @PublishAppVersion@ API.
module Amazonka.ResilienceHub.UpdateAppVersion
  ( -- * Creating a Request
    UpdateAppVersion (..),
    newUpdateAppVersion,

    -- * Request Lenses
    updateAppVersion_additionalInfo,
    updateAppVersion_appArn,

    -- * Destructuring the Response
    UpdateAppVersionResponse (..),
    newUpdateAppVersionResponse,

    -- * Response Lenses
    updateAppVersionResponse_additionalInfo,
    updateAppVersionResponse_httpStatus,
    updateAppVersionResponse_appArn,
    updateAppVersionResponse_appVersion,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAppVersion' smart constructor.
data UpdateAppVersion = UpdateAppVersion'
  { -- | Additional configuration parameters for an Resilience Hub application.
    -- If you want to implement @additionalInfo@ through the Resilience Hub
    -- console rather than using an API call, see
    -- <https://docs.aws.amazon.com/resilience-hub/latest/userguide/app-config-param.html Configure the application configuration parameters>.
    --
    -- Currently, this parameter accepts a key-value mapping (in a string
    -- format) of only one failover region and one associated account.
    --
    -- Key: @\"failover-regions\"@
    --
    -- Value:
    -- @\"[{\"region\":\"\<REGION>\", \"accounts\":[{\"id\":\"\<ACCOUNT_ID>\"}]}]\"@
    additionalInfo :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text)),
    -- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
    -- format for this ARN is:
    -- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
    -- information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/ guide.
    appArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAppVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalInfo', 'updateAppVersion_additionalInfo' - Additional configuration parameters for an Resilience Hub application.
-- If you want to implement @additionalInfo@ through the Resilience Hub
-- console rather than using an API call, see
-- <https://docs.aws.amazon.com/resilience-hub/latest/userguide/app-config-param.html Configure the application configuration parameters>.
--
-- Currently, this parameter accepts a key-value mapping (in a string
-- format) of only one failover region and one associated account.
--
-- Key: @\"failover-regions\"@
--
-- Value:
-- @\"[{\"region\":\"\<REGION>\", \"accounts\":[{\"id\":\"\<ACCOUNT_ID>\"}]}]\"@
--
-- 'appArn', 'updateAppVersion_appArn' - The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
newUpdateAppVersion ::
  -- | 'appArn'
  Prelude.Text ->
  UpdateAppVersion
newUpdateAppVersion pAppArn_ =
  UpdateAppVersion'
    { additionalInfo = Prelude.Nothing,
      appArn = pAppArn_
    }

-- | Additional configuration parameters for an Resilience Hub application.
-- If you want to implement @additionalInfo@ through the Resilience Hub
-- console rather than using an API call, see
-- <https://docs.aws.amazon.com/resilience-hub/latest/userguide/app-config-param.html Configure the application configuration parameters>.
--
-- Currently, this parameter accepts a key-value mapping (in a string
-- format) of only one failover region and one associated account.
--
-- Key: @\"failover-regions\"@
--
-- Value:
-- @\"[{\"region\":\"\<REGION>\", \"accounts\":[{\"id\":\"\<ACCOUNT_ID>\"}]}]\"@
updateAppVersion_additionalInfo :: Lens.Lens' UpdateAppVersion (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text)))
updateAppVersion_additionalInfo = Lens.lens (\UpdateAppVersion' {additionalInfo} -> additionalInfo) (\s@UpdateAppVersion' {} a -> s {additionalInfo = a} :: UpdateAppVersion) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
updateAppVersion_appArn :: Lens.Lens' UpdateAppVersion Prelude.Text
updateAppVersion_appArn = Lens.lens (\UpdateAppVersion' {appArn} -> appArn) (\s@UpdateAppVersion' {} a -> s {appArn = a} :: UpdateAppVersion)

instance Core.AWSRequest UpdateAppVersion where
  type
    AWSResponse UpdateAppVersion =
      UpdateAppVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAppVersionResponse'
            Prelude.<$> (x Data..?> "additionalInfo" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "appArn")
            Prelude.<*> (x Data..:> "appVersion")
      )

instance Prelude.Hashable UpdateAppVersion where
  hashWithSalt _salt UpdateAppVersion' {..} =
    _salt
      `Prelude.hashWithSalt` additionalInfo
      `Prelude.hashWithSalt` appArn

instance Prelude.NFData UpdateAppVersion where
  rnf UpdateAppVersion' {..} =
    Prelude.rnf additionalInfo
      `Prelude.seq` Prelude.rnf appArn

instance Data.ToHeaders UpdateAppVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateAppVersion where
  toJSON UpdateAppVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("additionalInfo" Data..=)
              Prelude.<$> additionalInfo,
            Prelude.Just ("appArn" Data..= appArn)
          ]
      )

instance Data.ToPath UpdateAppVersion where
  toPath = Prelude.const "/update-app-version"

instance Data.ToQuery UpdateAppVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAppVersionResponse' smart constructor.
data UpdateAppVersionResponse = UpdateAppVersionResponse'
  { -- | Additional configuration parameters for an Resilience Hub application.
    -- If you want to implement @additionalInfo@ through the Resilience Hub
    -- console rather than using an API call, see
    -- <https://docs.aws.amazon.com/resilience-hub/latest/userguide/app-config-param.html Configure the application configuration parameters>.
    --
    -- Currently, this parameter supports only failover region and account.
    additionalInfo :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text)),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
    -- format for this ARN is:
    -- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
    -- information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/ guide.
    appArn :: Prelude.Text,
    -- | The Resilience Hub application version.
    appVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAppVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalInfo', 'updateAppVersionResponse_additionalInfo' - Additional configuration parameters for an Resilience Hub application.
-- If you want to implement @additionalInfo@ through the Resilience Hub
-- console rather than using an API call, see
-- <https://docs.aws.amazon.com/resilience-hub/latest/userguide/app-config-param.html Configure the application configuration parameters>.
--
-- Currently, this parameter supports only failover region and account.
--
-- 'httpStatus', 'updateAppVersionResponse_httpStatus' - The response's http status code.
--
-- 'appArn', 'updateAppVersionResponse_appArn' - The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
--
-- 'appVersion', 'updateAppVersionResponse_appVersion' - The Resilience Hub application version.
newUpdateAppVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'appArn'
  Prelude.Text ->
  -- | 'appVersion'
  Prelude.Text ->
  UpdateAppVersionResponse
newUpdateAppVersionResponse
  pHttpStatus_
  pAppArn_
  pAppVersion_ =
    UpdateAppVersionResponse'
      { additionalInfo =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        appArn = pAppArn_,
        appVersion = pAppVersion_
      }

-- | Additional configuration parameters for an Resilience Hub application.
-- If you want to implement @additionalInfo@ through the Resilience Hub
-- console rather than using an API call, see
-- <https://docs.aws.amazon.com/resilience-hub/latest/userguide/app-config-param.html Configure the application configuration parameters>.
--
-- Currently, this parameter supports only failover region and account.
updateAppVersionResponse_additionalInfo :: Lens.Lens' UpdateAppVersionResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text)))
updateAppVersionResponse_additionalInfo = Lens.lens (\UpdateAppVersionResponse' {additionalInfo} -> additionalInfo) (\s@UpdateAppVersionResponse' {} a -> s {additionalInfo = a} :: UpdateAppVersionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateAppVersionResponse_httpStatus :: Lens.Lens' UpdateAppVersionResponse Prelude.Int
updateAppVersionResponse_httpStatus = Lens.lens (\UpdateAppVersionResponse' {httpStatus} -> httpStatus) (\s@UpdateAppVersionResponse' {} a -> s {httpStatus = a} :: UpdateAppVersionResponse)

-- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
updateAppVersionResponse_appArn :: Lens.Lens' UpdateAppVersionResponse Prelude.Text
updateAppVersionResponse_appArn = Lens.lens (\UpdateAppVersionResponse' {appArn} -> appArn) (\s@UpdateAppVersionResponse' {} a -> s {appArn = a} :: UpdateAppVersionResponse)

-- | The Resilience Hub application version.
updateAppVersionResponse_appVersion :: Lens.Lens' UpdateAppVersionResponse Prelude.Text
updateAppVersionResponse_appVersion = Lens.lens (\UpdateAppVersionResponse' {appVersion} -> appVersion) (\s@UpdateAppVersionResponse' {} a -> s {appVersion = a} :: UpdateAppVersionResponse)

instance Prelude.NFData UpdateAppVersionResponse where
  rnf UpdateAppVersionResponse' {..} =
    Prelude.rnf additionalInfo
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf appVersion

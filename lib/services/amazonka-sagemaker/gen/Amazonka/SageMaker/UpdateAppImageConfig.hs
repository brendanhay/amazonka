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
-- Module      : Amazonka.SageMaker.UpdateAppImageConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the properties of an AppImageConfig.
module Amazonka.SageMaker.UpdateAppImageConfig
  ( -- * Creating a Request
    UpdateAppImageConfig (..),
    newUpdateAppImageConfig,

    -- * Request Lenses
    updateAppImageConfig_kernelGatewayImageConfig,
    updateAppImageConfig_appImageConfigName,

    -- * Destructuring the Response
    UpdateAppImageConfigResponse (..),
    newUpdateAppImageConfigResponse,

    -- * Response Lenses
    updateAppImageConfigResponse_appImageConfigArn,
    updateAppImageConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newUpdateAppImageConfig' smart constructor.
data UpdateAppImageConfig = UpdateAppImageConfig'
  { -- | The new KernelGateway app to run on the image.
    kernelGatewayImageConfig :: Prelude.Maybe KernelGatewayImageConfig,
    -- | The name of the AppImageConfig to update.
    appImageConfigName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAppImageConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kernelGatewayImageConfig', 'updateAppImageConfig_kernelGatewayImageConfig' - The new KernelGateway app to run on the image.
--
-- 'appImageConfigName', 'updateAppImageConfig_appImageConfigName' - The name of the AppImageConfig to update.
newUpdateAppImageConfig ::
  -- | 'appImageConfigName'
  Prelude.Text ->
  UpdateAppImageConfig
newUpdateAppImageConfig pAppImageConfigName_ =
  UpdateAppImageConfig'
    { kernelGatewayImageConfig =
        Prelude.Nothing,
      appImageConfigName = pAppImageConfigName_
    }

-- | The new KernelGateway app to run on the image.
updateAppImageConfig_kernelGatewayImageConfig :: Lens.Lens' UpdateAppImageConfig (Prelude.Maybe KernelGatewayImageConfig)
updateAppImageConfig_kernelGatewayImageConfig = Lens.lens (\UpdateAppImageConfig' {kernelGatewayImageConfig} -> kernelGatewayImageConfig) (\s@UpdateAppImageConfig' {} a -> s {kernelGatewayImageConfig = a} :: UpdateAppImageConfig)

-- | The name of the AppImageConfig to update.
updateAppImageConfig_appImageConfigName :: Lens.Lens' UpdateAppImageConfig Prelude.Text
updateAppImageConfig_appImageConfigName = Lens.lens (\UpdateAppImageConfig' {appImageConfigName} -> appImageConfigName) (\s@UpdateAppImageConfig' {} a -> s {appImageConfigName = a} :: UpdateAppImageConfig)

instance Core.AWSRequest UpdateAppImageConfig where
  type
    AWSResponse UpdateAppImageConfig =
      UpdateAppImageConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAppImageConfigResponse'
            Prelude.<$> (x Data..?> "AppImageConfigArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAppImageConfig where
  hashWithSalt _salt UpdateAppImageConfig' {..} =
    _salt
      `Prelude.hashWithSalt` kernelGatewayImageConfig
      `Prelude.hashWithSalt` appImageConfigName

instance Prelude.NFData UpdateAppImageConfig where
  rnf UpdateAppImageConfig' {..} =
    Prelude.rnf kernelGatewayImageConfig
      `Prelude.seq` Prelude.rnf appImageConfigName

instance Data.ToHeaders UpdateAppImageConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.UpdateAppImageConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateAppImageConfig where
  toJSON UpdateAppImageConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KernelGatewayImageConfig" Data..=)
              Prelude.<$> kernelGatewayImageConfig,
            Prelude.Just
              ("AppImageConfigName" Data..= appImageConfigName)
          ]
      )

instance Data.ToPath UpdateAppImageConfig where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateAppImageConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAppImageConfigResponse' smart constructor.
data UpdateAppImageConfigResponse = UpdateAppImageConfigResponse'
  { -- | The Amazon Resource Name (ARN) for the AppImageConfig.
    appImageConfigArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAppImageConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appImageConfigArn', 'updateAppImageConfigResponse_appImageConfigArn' - The Amazon Resource Name (ARN) for the AppImageConfig.
--
-- 'httpStatus', 'updateAppImageConfigResponse_httpStatus' - The response's http status code.
newUpdateAppImageConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateAppImageConfigResponse
newUpdateAppImageConfigResponse pHttpStatus_ =
  UpdateAppImageConfigResponse'
    { appImageConfigArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) for the AppImageConfig.
updateAppImageConfigResponse_appImageConfigArn :: Lens.Lens' UpdateAppImageConfigResponse (Prelude.Maybe Prelude.Text)
updateAppImageConfigResponse_appImageConfigArn = Lens.lens (\UpdateAppImageConfigResponse' {appImageConfigArn} -> appImageConfigArn) (\s@UpdateAppImageConfigResponse' {} a -> s {appImageConfigArn = a} :: UpdateAppImageConfigResponse)

-- | The response's http status code.
updateAppImageConfigResponse_httpStatus :: Lens.Lens' UpdateAppImageConfigResponse Prelude.Int
updateAppImageConfigResponse_httpStatus = Lens.lens (\UpdateAppImageConfigResponse' {httpStatus} -> httpStatus) (\s@UpdateAppImageConfigResponse' {} a -> s {httpStatus = a} :: UpdateAppImageConfigResponse)

instance Prelude.NFData UpdateAppImageConfigResponse where
  rnf UpdateAppImageConfigResponse' {..} =
    Prelude.rnf appImageConfigArn
      `Prelude.seq` Prelude.rnf httpStatus

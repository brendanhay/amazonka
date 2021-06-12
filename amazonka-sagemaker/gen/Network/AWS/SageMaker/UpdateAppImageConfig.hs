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
-- Module      : Network.AWS.SageMaker.UpdateAppImageConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the properties of an AppImageConfig.
module Network.AWS.SageMaker.UpdateAppImageConfig
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newUpdateAppImageConfig' smart constructor.
data UpdateAppImageConfig = UpdateAppImageConfig'
  { -- | The new KernelGateway app to run on the image.
    kernelGatewayImageConfig :: Core.Maybe KernelGatewayImageConfig,
    -- | The name of the AppImageConfig to update.
    appImageConfigName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  UpdateAppImageConfig
newUpdateAppImageConfig pAppImageConfigName_ =
  UpdateAppImageConfig'
    { kernelGatewayImageConfig =
        Core.Nothing,
      appImageConfigName = pAppImageConfigName_
    }

-- | The new KernelGateway app to run on the image.
updateAppImageConfig_kernelGatewayImageConfig :: Lens.Lens' UpdateAppImageConfig (Core.Maybe KernelGatewayImageConfig)
updateAppImageConfig_kernelGatewayImageConfig = Lens.lens (\UpdateAppImageConfig' {kernelGatewayImageConfig} -> kernelGatewayImageConfig) (\s@UpdateAppImageConfig' {} a -> s {kernelGatewayImageConfig = a} :: UpdateAppImageConfig)

-- | The name of the AppImageConfig to update.
updateAppImageConfig_appImageConfigName :: Lens.Lens' UpdateAppImageConfig Core.Text
updateAppImageConfig_appImageConfigName = Lens.lens (\UpdateAppImageConfig' {appImageConfigName} -> appImageConfigName) (\s@UpdateAppImageConfig' {} a -> s {appImageConfigName = a} :: UpdateAppImageConfig)

instance Core.AWSRequest UpdateAppImageConfig where
  type
    AWSResponse UpdateAppImageConfig =
      UpdateAppImageConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAppImageConfigResponse'
            Core.<$> (x Core..?> "AppImageConfigArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateAppImageConfig

instance Core.NFData UpdateAppImageConfig

instance Core.ToHeaders UpdateAppImageConfig where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.UpdateAppImageConfig" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateAppImageConfig where
  toJSON UpdateAppImageConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("KernelGatewayImageConfig" Core..=)
              Core.<$> kernelGatewayImageConfig,
            Core.Just
              ("AppImageConfigName" Core..= appImageConfigName)
          ]
      )

instance Core.ToPath UpdateAppImageConfig where
  toPath = Core.const "/"

instance Core.ToQuery UpdateAppImageConfig where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateAppImageConfigResponse' smart constructor.
data UpdateAppImageConfigResponse = UpdateAppImageConfigResponse'
  { -- | The Amazon Resource Name (ARN) for the AppImageConfig.
    appImageConfigArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateAppImageConfigResponse
newUpdateAppImageConfigResponse pHttpStatus_ =
  UpdateAppImageConfigResponse'
    { appImageConfigArn =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) for the AppImageConfig.
updateAppImageConfigResponse_appImageConfigArn :: Lens.Lens' UpdateAppImageConfigResponse (Core.Maybe Core.Text)
updateAppImageConfigResponse_appImageConfigArn = Lens.lens (\UpdateAppImageConfigResponse' {appImageConfigArn} -> appImageConfigArn) (\s@UpdateAppImageConfigResponse' {} a -> s {appImageConfigArn = a} :: UpdateAppImageConfigResponse)

-- | The response's http status code.
updateAppImageConfigResponse_httpStatus :: Lens.Lens' UpdateAppImageConfigResponse Core.Int
updateAppImageConfigResponse_httpStatus = Lens.lens (\UpdateAppImageConfigResponse' {httpStatus} -> httpStatus) (\s@UpdateAppImageConfigResponse' {} a -> s {httpStatus = a} :: UpdateAppImageConfigResponse)

instance Core.NFData UpdateAppImageConfigResponse

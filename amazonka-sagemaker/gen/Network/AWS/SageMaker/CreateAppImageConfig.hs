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
-- Module      : Network.AWS.SageMaker.CreateAppImageConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a configuration for running a SageMaker image as a KernelGateway
-- app. The configuration specifies the Amazon Elastic File System (EFS)
-- storage volume on the image, and a list of the kernels in the image.
module Network.AWS.SageMaker.CreateAppImageConfig
  ( -- * Creating a Request
    CreateAppImageConfig (..),
    newCreateAppImageConfig,

    -- * Request Lenses
    createAppImageConfig_kernelGatewayImageConfig,
    createAppImageConfig_tags,
    createAppImageConfig_appImageConfigName,

    -- * Destructuring the Response
    CreateAppImageConfigResponse (..),
    newCreateAppImageConfigResponse,

    -- * Response Lenses
    createAppImageConfigResponse_appImageConfigArn,
    createAppImageConfigResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateAppImageConfig' smart constructor.
data CreateAppImageConfig = CreateAppImageConfig'
  { -- | The KernelGatewayImageConfig.
    kernelGatewayImageConfig :: Core.Maybe KernelGatewayImageConfig,
    -- | A list of tags to apply to the AppImageConfig.
    tags :: Core.Maybe [Tag],
    -- | The name of the AppImageConfig. Must be unique to your account.
    appImageConfigName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateAppImageConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kernelGatewayImageConfig', 'createAppImageConfig_kernelGatewayImageConfig' - The KernelGatewayImageConfig.
--
-- 'tags', 'createAppImageConfig_tags' - A list of tags to apply to the AppImageConfig.
--
-- 'appImageConfigName', 'createAppImageConfig_appImageConfigName' - The name of the AppImageConfig. Must be unique to your account.
newCreateAppImageConfig ::
  -- | 'appImageConfigName'
  Core.Text ->
  CreateAppImageConfig
newCreateAppImageConfig pAppImageConfigName_ =
  CreateAppImageConfig'
    { kernelGatewayImageConfig =
        Core.Nothing,
      tags = Core.Nothing,
      appImageConfigName = pAppImageConfigName_
    }

-- | The KernelGatewayImageConfig.
createAppImageConfig_kernelGatewayImageConfig :: Lens.Lens' CreateAppImageConfig (Core.Maybe KernelGatewayImageConfig)
createAppImageConfig_kernelGatewayImageConfig = Lens.lens (\CreateAppImageConfig' {kernelGatewayImageConfig} -> kernelGatewayImageConfig) (\s@CreateAppImageConfig' {} a -> s {kernelGatewayImageConfig = a} :: CreateAppImageConfig)

-- | A list of tags to apply to the AppImageConfig.
createAppImageConfig_tags :: Lens.Lens' CreateAppImageConfig (Core.Maybe [Tag])
createAppImageConfig_tags = Lens.lens (\CreateAppImageConfig' {tags} -> tags) (\s@CreateAppImageConfig' {} a -> s {tags = a} :: CreateAppImageConfig) Core.. Lens.mapping Lens._Coerce

-- | The name of the AppImageConfig. Must be unique to your account.
createAppImageConfig_appImageConfigName :: Lens.Lens' CreateAppImageConfig Core.Text
createAppImageConfig_appImageConfigName = Lens.lens (\CreateAppImageConfig' {appImageConfigName} -> appImageConfigName) (\s@CreateAppImageConfig' {} a -> s {appImageConfigName = a} :: CreateAppImageConfig)

instance Core.AWSRequest CreateAppImageConfig where
  type
    AWSResponse CreateAppImageConfig =
      CreateAppImageConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAppImageConfigResponse'
            Core.<$> (x Core..?> "AppImageConfigArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateAppImageConfig

instance Core.NFData CreateAppImageConfig

instance Core.ToHeaders CreateAppImageConfig where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.CreateAppImageConfig" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateAppImageConfig where
  toJSON CreateAppImageConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("KernelGatewayImageConfig" Core..=)
              Core.<$> kernelGatewayImageConfig,
            ("Tags" Core..=) Core.<$> tags,
            Core.Just
              ("AppImageConfigName" Core..= appImageConfigName)
          ]
      )

instance Core.ToPath CreateAppImageConfig where
  toPath = Core.const "/"

instance Core.ToQuery CreateAppImageConfig where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateAppImageConfigResponse' smart constructor.
data CreateAppImageConfigResponse = CreateAppImageConfigResponse'
  { -- | The Amazon Resource Name (ARN) of the AppImageConfig.
    appImageConfigArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateAppImageConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appImageConfigArn', 'createAppImageConfigResponse_appImageConfigArn' - The Amazon Resource Name (ARN) of the AppImageConfig.
--
-- 'httpStatus', 'createAppImageConfigResponse_httpStatus' - The response's http status code.
newCreateAppImageConfigResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateAppImageConfigResponse
newCreateAppImageConfigResponse pHttpStatus_ =
  CreateAppImageConfigResponse'
    { appImageConfigArn =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the AppImageConfig.
createAppImageConfigResponse_appImageConfigArn :: Lens.Lens' CreateAppImageConfigResponse (Core.Maybe Core.Text)
createAppImageConfigResponse_appImageConfigArn = Lens.lens (\CreateAppImageConfigResponse' {appImageConfigArn} -> appImageConfigArn) (\s@CreateAppImageConfigResponse' {} a -> s {appImageConfigArn = a} :: CreateAppImageConfigResponse)

-- | The response's http status code.
createAppImageConfigResponse_httpStatus :: Lens.Lens' CreateAppImageConfigResponse Core.Int
createAppImageConfigResponse_httpStatus = Lens.lens (\CreateAppImageConfigResponse' {httpStatus} -> httpStatus) (\s@CreateAppImageConfigResponse' {} a -> s {httpStatus = a} :: CreateAppImageConfigResponse)

instance Core.NFData CreateAppImageConfigResponse

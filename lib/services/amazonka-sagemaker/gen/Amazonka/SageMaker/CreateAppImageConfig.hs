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
-- Module      : Amazonka.SageMaker.CreateAppImageConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a configuration for running a SageMaker image as a KernelGateway
-- app. The configuration specifies the Amazon Elastic File System (EFS)
-- storage volume on the image, and a list of the kernels in the image.
module Amazonka.SageMaker.CreateAppImageConfig
  ( -- * Creating a Request
    CreateAppImageConfig (..),
    newCreateAppImageConfig,

    -- * Request Lenses
    createAppImageConfig_tags,
    createAppImageConfig_kernelGatewayImageConfig,
    createAppImageConfig_appImageConfigName,

    -- * Destructuring the Response
    CreateAppImageConfigResponse (..),
    newCreateAppImageConfigResponse,

    -- * Response Lenses
    createAppImageConfigResponse_appImageConfigArn,
    createAppImageConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateAppImageConfig' smart constructor.
data CreateAppImageConfig = CreateAppImageConfig'
  { -- | A list of tags to apply to the AppImageConfig.
    tags :: Prelude.Maybe [Tag],
    -- | The KernelGatewayImageConfig. You can only specify one image kernel in
    -- the AppImageConfig API. This kernel will be shown to users before the
    -- image starts. Once the image runs, all kernels are visible in
    -- JupyterLab.
    kernelGatewayImageConfig :: Prelude.Maybe KernelGatewayImageConfig,
    -- | The name of the AppImageConfig. Must be unique to your account.
    appImageConfigName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAppImageConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createAppImageConfig_tags' - A list of tags to apply to the AppImageConfig.
--
-- 'kernelGatewayImageConfig', 'createAppImageConfig_kernelGatewayImageConfig' - The KernelGatewayImageConfig. You can only specify one image kernel in
-- the AppImageConfig API. This kernel will be shown to users before the
-- image starts. Once the image runs, all kernels are visible in
-- JupyterLab.
--
-- 'appImageConfigName', 'createAppImageConfig_appImageConfigName' - The name of the AppImageConfig. Must be unique to your account.
newCreateAppImageConfig ::
  -- | 'appImageConfigName'
  Prelude.Text ->
  CreateAppImageConfig
newCreateAppImageConfig pAppImageConfigName_ =
  CreateAppImageConfig'
    { tags = Prelude.Nothing,
      kernelGatewayImageConfig = Prelude.Nothing,
      appImageConfigName = pAppImageConfigName_
    }

-- | A list of tags to apply to the AppImageConfig.
createAppImageConfig_tags :: Lens.Lens' CreateAppImageConfig (Prelude.Maybe [Tag])
createAppImageConfig_tags = Lens.lens (\CreateAppImageConfig' {tags} -> tags) (\s@CreateAppImageConfig' {} a -> s {tags = a} :: CreateAppImageConfig) Prelude.. Lens.mapping Lens.coerced

-- | The KernelGatewayImageConfig. You can only specify one image kernel in
-- the AppImageConfig API. This kernel will be shown to users before the
-- image starts. Once the image runs, all kernels are visible in
-- JupyterLab.
createAppImageConfig_kernelGatewayImageConfig :: Lens.Lens' CreateAppImageConfig (Prelude.Maybe KernelGatewayImageConfig)
createAppImageConfig_kernelGatewayImageConfig = Lens.lens (\CreateAppImageConfig' {kernelGatewayImageConfig} -> kernelGatewayImageConfig) (\s@CreateAppImageConfig' {} a -> s {kernelGatewayImageConfig = a} :: CreateAppImageConfig)

-- | The name of the AppImageConfig. Must be unique to your account.
createAppImageConfig_appImageConfigName :: Lens.Lens' CreateAppImageConfig Prelude.Text
createAppImageConfig_appImageConfigName = Lens.lens (\CreateAppImageConfig' {appImageConfigName} -> appImageConfigName) (\s@CreateAppImageConfig' {} a -> s {appImageConfigName = a} :: CreateAppImageConfig)

instance Core.AWSRequest CreateAppImageConfig where
  type
    AWSResponse CreateAppImageConfig =
      CreateAppImageConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAppImageConfigResponse'
            Prelude.<$> (x Data..?> "AppImageConfigArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAppImageConfig where
  hashWithSalt _salt CreateAppImageConfig' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` kernelGatewayImageConfig
      `Prelude.hashWithSalt` appImageConfigName

instance Prelude.NFData CreateAppImageConfig where
  rnf CreateAppImageConfig' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf kernelGatewayImageConfig
      `Prelude.seq` Prelude.rnf appImageConfigName

instance Data.ToHeaders CreateAppImageConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.CreateAppImageConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAppImageConfig where
  toJSON CreateAppImageConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("KernelGatewayImageConfig" Data..=)
              Prelude.<$> kernelGatewayImageConfig,
            Prelude.Just
              ("AppImageConfigName" Data..= appImageConfigName)
          ]
      )

instance Data.ToPath CreateAppImageConfig where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateAppImageConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAppImageConfigResponse' smart constructor.
data CreateAppImageConfigResponse = CreateAppImageConfigResponse'
  { -- | The Amazon Resource Name (ARN) of the AppImageConfig.
    appImageConfigArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateAppImageConfigResponse
newCreateAppImageConfigResponse pHttpStatus_ =
  CreateAppImageConfigResponse'
    { appImageConfigArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the AppImageConfig.
createAppImageConfigResponse_appImageConfigArn :: Lens.Lens' CreateAppImageConfigResponse (Prelude.Maybe Prelude.Text)
createAppImageConfigResponse_appImageConfigArn = Lens.lens (\CreateAppImageConfigResponse' {appImageConfigArn} -> appImageConfigArn) (\s@CreateAppImageConfigResponse' {} a -> s {appImageConfigArn = a} :: CreateAppImageConfigResponse)

-- | The response's http status code.
createAppImageConfigResponse_httpStatus :: Lens.Lens' CreateAppImageConfigResponse Prelude.Int
createAppImageConfigResponse_httpStatus = Lens.lens (\CreateAppImageConfigResponse' {httpStatus} -> httpStatus) (\s@CreateAppImageConfigResponse' {} a -> s {httpStatus = a} :: CreateAppImageConfigResponse)

instance Prelude.NFData CreateAppImageConfigResponse where
  rnf CreateAppImageConfigResponse' {..} =
    Prelude.rnf appImageConfigArn
      `Prelude.seq` Prelude.rnf httpStatus

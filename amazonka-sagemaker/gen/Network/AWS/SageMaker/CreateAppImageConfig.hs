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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateAppImageConfig' smart constructor.
data CreateAppImageConfig = CreateAppImageConfig'
  { -- | The KernelGatewayImageConfig.
    kernelGatewayImageConfig :: Prelude.Maybe KernelGatewayImageConfig,
    -- | A list of tags to apply to the AppImageConfig.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the AppImageConfig. Must be unique to your account.
    appImageConfigName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  CreateAppImageConfig
newCreateAppImageConfig pAppImageConfigName_ =
  CreateAppImageConfig'
    { kernelGatewayImageConfig =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      appImageConfigName = pAppImageConfigName_
    }

-- | The KernelGatewayImageConfig.
createAppImageConfig_kernelGatewayImageConfig :: Lens.Lens' CreateAppImageConfig (Prelude.Maybe KernelGatewayImageConfig)
createAppImageConfig_kernelGatewayImageConfig = Lens.lens (\CreateAppImageConfig' {kernelGatewayImageConfig} -> kernelGatewayImageConfig) (\s@CreateAppImageConfig' {} a -> s {kernelGatewayImageConfig = a} :: CreateAppImageConfig)

-- | A list of tags to apply to the AppImageConfig.
createAppImageConfig_tags :: Lens.Lens' CreateAppImageConfig (Prelude.Maybe [Tag])
createAppImageConfig_tags = Lens.lens (\CreateAppImageConfig' {tags} -> tags) (\s@CreateAppImageConfig' {} a -> s {tags = a} :: CreateAppImageConfig) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the AppImageConfig. Must be unique to your account.
createAppImageConfig_appImageConfigName :: Lens.Lens' CreateAppImageConfig Prelude.Text
createAppImageConfig_appImageConfigName = Lens.lens (\CreateAppImageConfig' {appImageConfigName} -> appImageConfigName) (\s@CreateAppImageConfig' {} a -> s {appImageConfigName = a} :: CreateAppImageConfig)

instance Prelude.AWSRequest CreateAppImageConfig where
  type
    Rs CreateAppImageConfig =
      CreateAppImageConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAppImageConfigResponse'
            Prelude.<$> (x Prelude..?> "AppImageConfigArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAppImageConfig

instance Prelude.NFData CreateAppImageConfig

instance Prelude.ToHeaders CreateAppImageConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.CreateAppImageConfig" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateAppImageConfig where
  toJSON CreateAppImageConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("KernelGatewayImageConfig" Prelude..=)
              Prelude.<$> kernelGatewayImageConfig,
            ("Tags" Prelude..=) Prelude.<$> tags,
            Prelude.Just
              ( "AppImageConfigName"
                  Prelude..= appImageConfigName
              )
          ]
      )

instance Prelude.ToPath CreateAppImageConfig where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateAppImageConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAppImageConfigResponse' smart constructor.
data CreateAppImageConfigResponse = CreateAppImageConfigResponse'
  { -- | The Amazon Resource Name (ARN) of the AppImageConfig.
    appImageConfigArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData CreateAppImageConfigResponse

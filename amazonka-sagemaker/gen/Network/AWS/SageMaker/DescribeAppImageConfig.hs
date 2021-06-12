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
-- Module      : Network.AWS.SageMaker.DescribeAppImageConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an AppImageConfig.
module Network.AWS.SageMaker.DescribeAppImageConfig
  ( -- * Creating a Request
    DescribeAppImageConfig (..),
    newDescribeAppImageConfig,

    -- * Request Lenses
    describeAppImageConfig_appImageConfigName,

    -- * Destructuring the Response
    DescribeAppImageConfigResponse (..),
    newDescribeAppImageConfigResponse,

    -- * Response Lenses
    describeAppImageConfigResponse_creationTime,
    describeAppImageConfigResponse_appImageConfigArn,
    describeAppImageConfigResponse_kernelGatewayImageConfig,
    describeAppImageConfigResponse_appImageConfigName,
    describeAppImageConfigResponse_lastModifiedTime,
    describeAppImageConfigResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeAppImageConfig' smart constructor.
data DescribeAppImageConfig = DescribeAppImageConfig'
  { -- | The name of the AppImageConfig to describe.
    appImageConfigName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAppImageConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appImageConfigName', 'describeAppImageConfig_appImageConfigName' - The name of the AppImageConfig to describe.
newDescribeAppImageConfig ::
  -- | 'appImageConfigName'
  Core.Text ->
  DescribeAppImageConfig
newDescribeAppImageConfig pAppImageConfigName_ =
  DescribeAppImageConfig'
    { appImageConfigName =
        pAppImageConfigName_
    }

-- | The name of the AppImageConfig to describe.
describeAppImageConfig_appImageConfigName :: Lens.Lens' DescribeAppImageConfig Core.Text
describeAppImageConfig_appImageConfigName = Lens.lens (\DescribeAppImageConfig' {appImageConfigName} -> appImageConfigName) (\s@DescribeAppImageConfig' {} a -> s {appImageConfigName = a} :: DescribeAppImageConfig)

instance Core.AWSRequest DescribeAppImageConfig where
  type
    AWSResponse DescribeAppImageConfig =
      DescribeAppImageConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAppImageConfigResponse'
            Core.<$> (x Core..?> "CreationTime")
            Core.<*> (x Core..?> "AppImageConfigArn")
            Core.<*> (x Core..?> "KernelGatewayImageConfig")
            Core.<*> (x Core..?> "AppImageConfigName")
            Core.<*> (x Core..?> "LastModifiedTime")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeAppImageConfig

instance Core.NFData DescribeAppImageConfig

instance Core.ToHeaders DescribeAppImageConfig where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DescribeAppImageConfig" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeAppImageConfig where
  toJSON DescribeAppImageConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("AppImageConfigName" Core..= appImageConfigName)
          ]
      )

instance Core.ToPath DescribeAppImageConfig where
  toPath = Core.const "/"

instance Core.ToQuery DescribeAppImageConfig where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeAppImageConfigResponse' smart constructor.
data DescribeAppImageConfigResponse = DescribeAppImageConfigResponse'
  { -- | When the AppImageConfig was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the AppImageConfig.
    appImageConfigArn :: Core.Maybe Core.Text,
    -- | The configuration of a KernelGateway app.
    kernelGatewayImageConfig :: Core.Maybe KernelGatewayImageConfig,
    -- | The name of the AppImageConfig.
    appImageConfigName :: Core.Maybe Core.Text,
    -- | When the AppImageConfig was last modified.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAppImageConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'describeAppImageConfigResponse_creationTime' - When the AppImageConfig was created.
--
-- 'appImageConfigArn', 'describeAppImageConfigResponse_appImageConfigArn' - The Amazon Resource Name (ARN) of the AppImageConfig.
--
-- 'kernelGatewayImageConfig', 'describeAppImageConfigResponse_kernelGatewayImageConfig' - The configuration of a KernelGateway app.
--
-- 'appImageConfigName', 'describeAppImageConfigResponse_appImageConfigName' - The name of the AppImageConfig.
--
-- 'lastModifiedTime', 'describeAppImageConfigResponse_lastModifiedTime' - When the AppImageConfig was last modified.
--
-- 'httpStatus', 'describeAppImageConfigResponse_httpStatus' - The response's http status code.
newDescribeAppImageConfigResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeAppImageConfigResponse
newDescribeAppImageConfigResponse pHttpStatus_ =
  DescribeAppImageConfigResponse'
    { creationTime =
        Core.Nothing,
      appImageConfigArn = Core.Nothing,
      kernelGatewayImageConfig = Core.Nothing,
      appImageConfigName = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When the AppImageConfig was created.
describeAppImageConfigResponse_creationTime :: Lens.Lens' DescribeAppImageConfigResponse (Core.Maybe Core.UTCTime)
describeAppImageConfigResponse_creationTime = Lens.lens (\DescribeAppImageConfigResponse' {creationTime} -> creationTime) (\s@DescribeAppImageConfigResponse' {} a -> s {creationTime = a} :: DescribeAppImageConfigResponse) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the AppImageConfig.
describeAppImageConfigResponse_appImageConfigArn :: Lens.Lens' DescribeAppImageConfigResponse (Core.Maybe Core.Text)
describeAppImageConfigResponse_appImageConfigArn = Lens.lens (\DescribeAppImageConfigResponse' {appImageConfigArn} -> appImageConfigArn) (\s@DescribeAppImageConfigResponse' {} a -> s {appImageConfigArn = a} :: DescribeAppImageConfigResponse)

-- | The configuration of a KernelGateway app.
describeAppImageConfigResponse_kernelGatewayImageConfig :: Lens.Lens' DescribeAppImageConfigResponse (Core.Maybe KernelGatewayImageConfig)
describeAppImageConfigResponse_kernelGatewayImageConfig = Lens.lens (\DescribeAppImageConfigResponse' {kernelGatewayImageConfig} -> kernelGatewayImageConfig) (\s@DescribeAppImageConfigResponse' {} a -> s {kernelGatewayImageConfig = a} :: DescribeAppImageConfigResponse)

-- | The name of the AppImageConfig.
describeAppImageConfigResponse_appImageConfigName :: Lens.Lens' DescribeAppImageConfigResponse (Core.Maybe Core.Text)
describeAppImageConfigResponse_appImageConfigName = Lens.lens (\DescribeAppImageConfigResponse' {appImageConfigName} -> appImageConfigName) (\s@DescribeAppImageConfigResponse' {} a -> s {appImageConfigName = a} :: DescribeAppImageConfigResponse)

-- | When the AppImageConfig was last modified.
describeAppImageConfigResponse_lastModifiedTime :: Lens.Lens' DescribeAppImageConfigResponse (Core.Maybe Core.UTCTime)
describeAppImageConfigResponse_lastModifiedTime = Lens.lens (\DescribeAppImageConfigResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeAppImageConfigResponse' {} a -> s {lastModifiedTime = a} :: DescribeAppImageConfigResponse) Core.. Lens.mapping Core._Time

-- | The response's http status code.
describeAppImageConfigResponse_httpStatus :: Lens.Lens' DescribeAppImageConfigResponse Core.Int
describeAppImageConfigResponse_httpStatus = Lens.lens (\DescribeAppImageConfigResponse' {httpStatus} -> httpStatus) (\s@DescribeAppImageConfigResponse' {} a -> s {httpStatus = a} :: DescribeAppImageConfigResponse)

instance Core.NFData DescribeAppImageConfigResponse

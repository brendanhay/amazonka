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
-- Module      : Amazonka.SageMaker.DescribeAppImageConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an AppImageConfig.
module Amazonka.SageMaker.DescribeAppImageConfig
  ( -- * Creating a Request
    DescribeAppImageConfig (..),
    newDescribeAppImageConfig,

    -- * Request Lenses
    describeAppImageConfig_appImageConfigName,

    -- * Destructuring the Response
    DescribeAppImageConfigResponse (..),
    newDescribeAppImageConfigResponse,

    -- * Response Lenses
    describeAppImageConfigResponse_appImageConfigArn,
    describeAppImageConfigResponse_appImageConfigName,
    describeAppImageConfigResponse_kernelGatewayImageConfig,
    describeAppImageConfigResponse_lastModifiedTime,
    describeAppImageConfigResponse_creationTime,
    describeAppImageConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeAppImageConfig' smart constructor.
data DescribeAppImageConfig = DescribeAppImageConfig'
  { -- | The name of the AppImageConfig to describe.
    appImageConfigName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeAppImageConfig
newDescribeAppImageConfig pAppImageConfigName_ =
  DescribeAppImageConfig'
    { appImageConfigName =
        pAppImageConfigName_
    }

-- | The name of the AppImageConfig to describe.
describeAppImageConfig_appImageConfigName :: Lens.Lens' DescribeAppImageConfig Prelude.Text
describeAppImageConfig_appImageConfigName = Lens.lens (\DescribeAppImageConfig' {appImageConfigName} -> appImageConfigName) (\s@DescribeAppImageConfig' {} a -> s {appImageConfigName = a} :: DescribeAppImageConfig)

instance Core.AWSRequest DescribeAppImageConfig where
  type
    AWSResponse DescribeAppImageConfig =
      DescribeAppImageConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAppImageConfigResponse'
            Prelude.<$> (x Data..?> "AppImageConfigArn")
            Prelude.<*> (x Data..?> "AppImageConfigName")
            Prelude.<*> (x Data..?> "KernelGatewayImageConfig")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAppImageConfig where
  hashWithSalt _salt DescribeAppImageConfig' {..} =
    _salt `Prelude.hashWithSalt` appImageConfigName

instance Prelude.NFData DescribeAppImageConfig where
  rnf DescribeAppImageConfig' {..} =
    Prelude.rnf appImageConfigName

instance Data.ToHeaders DescribeAppImageConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DescribeAppImageConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeAppImageConfig where
  toJSON DescribeAppImageConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AppImageConfigName" Data..= appImageConfigName)
          ]
      )

instance Data.ToPath DescribeAppImageConfig where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeAppImageConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAppImageConfigResponse' smart constructor.
data DescribeAppImageConfigResponse = DescribeAppImageConfigResponse'
  { -- | The Amazon Resource Name (ARN) of the AppImageConfig.
    appImageConfigArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the AppImageConfig.
    appImageConfigName :: Prelude.Maybe Prelude.Text,
    -- | The configuration of a KernelGateway app.
    kernelGatewayImageConfig :: Prelude.Maybe KernelGatewayImageConfig,
    -- | When the AppImageConfig was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | When the AppImageConfig was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAppImageConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appImageConfigArn', 'describeAppImageConfigResponse_appImageConfigArn' - The Amazon Resource Name (ARN) of the AppImageConfig.
--
-- 'appImageConfigName', 'describeAppImageConfigResponse_appImageConfigName' - The name of the AppImageConfig.
--
-- 'kernelGatewayImageConfig', 'describeAppImageConfigResponse_kernelGatewayImageConfig' - The configuration of a KernelGateway app.
--
-- 'lastModifiedTime', 'describeAppImageConfigResponse_lastModifiedTime' - When the AppImageConfig was last modified.
--
-- 'creationTime', 'describeAppImageConfigResponse_creationTime' - When the AppImageConfig was created.
--
-- 'httpStatus', 'describeAppImageConfigResponse_httpStatus' - The response's http status code.
newDescribeAppImageConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAppImageConfigResponse
newDescribeAppImageConfigResponse pHttpStatus_ =
  DescribeAppImageConfigResponse'
    { appImageConfigArn =
        Prelude.Nothing,
      appImageConfigName = Prelude.Nothing,
      kernelGatewayImageConfig = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the AppImageConfig.
describeAppImageConfigResponse_appImageConfigArn :: Lens.Lens' DescribeAppImageConfigResponse (Prelude.Maybe Prelude.Text)
describeAppImageConfigResponse_appImageConfigArn = Lens.lens (\DescribeAppImageConfigResponse' {appImageConfigArn} -> appImageConfigArn) (\s@DescribeAppImageConfigResponse' {} a -> s {appImageConfigArn = a} :: DescribeAppImageConfigResponse)

-- | The name of the AppImageConfig.
describeAppImageConfigResponse_appImageConfigName :: Lens.Lens' DescribeAppImageConfigResponse (Prelude.Maybe Prelude.Text)
describeAppImageConfigResponse_appImageConfigName = Lens.lens (\DescribeAppImageConfigResponse' {appImageConfigName} -> appImageConfigName) (\s@DescribeAppImageConfigResponse' {} a -> s {appImageConfigName = a} :: DescribeAppImageConfigResponse)

-- | The configuration of a KernelGateway app.
describeAppImageConfigResponse_kernelGatewayImageConfig :: Lens.Lens' DescribeAppImageConfigResponse (Prelude.Maybe KernelGatewayImageConfig)
describeAppImageConfigResponse_kernelGatewayImageConfig = Lens.lens (\DescribeAppImageConfigResponse' {kernelGatewayImageConfig} -> kernelGatewayImageConfig) (\s@DescribeAppImageConfigResponse' {} a -> s {kernelGatewayImageConfig = a} :: DescribeAppImageConfigResponse)

-- | When the AppImageConfig was last modified.
describeAppImageConfigResponse_lastModifiedTime :: Lens.Lens' DescribeAppImageConfigResponse (Prelude.Maybe Prelude.UTCTime)
describeAppImageConfigResponse_lastModifiedTime = Lens.lens (\DescribeAppImageConfigResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeAppImageConfigResponse' {} a -> s {lastModifiedTime = a} :: DescribeAppImageConfigResponse) Prelude.. Lens.mapping Data._Time

-- | When the AppImageConfig was created.
describeAppImageConfigResponse_creationTime :: Lens.Lens' DescribeAppImageConfigResponse (Prelude.Maybe Prelude.UTCTime)
describeAppImageConfigResponse_creationTime = Lens.lens (\DescribeAppImageConfigResponse' {creationTime} -> creationTime) (\s@DescribeAppImageConfigResponse' {} a -> s {creationTime = a} :: DescribeAppImageConfigResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
describeAppImageConfigResponse_httpStatus :: Lens.Lens' DescribeAppImageConfigResponse Prelude.Int
describeAppImageConfigResponse_httpStatus = Lens.lens (\DescribeAppImageConfigResponse' {httpStatus} -> httpStatus) (\s@DescribeAppImageConfigResponse' {} a -> s {httpStatus = a} :: DescribeAppImageConfigResponse)

instance
  Prelude.NFData
    DescribeAppImageConfigResponse
  where
  rnf DescribeAppImageConfigResponse' {..} =
    Prelude.rnf appImageConfigArn
      `Prelude.seq` Prelude.rnf appImageConfigName
      `Prelude.seq` Prelude.rnf kernelGatewayImageConfig
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf httpStatus

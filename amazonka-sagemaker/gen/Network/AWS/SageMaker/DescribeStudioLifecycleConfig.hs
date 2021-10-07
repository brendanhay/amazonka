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
-- Module      : Network.AWS.SageMaker.DescribeStudioLifecycleConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the Studio Lifecycle Configuration.
module Network.AWS.SageMaker.DescribeStudioLifecycleConfig
  ( -- * Creating a Request
    DescribeStudioLifecycleConfig (..),
    newDescribeStudioLifecycleConfig,

    -- * Request Lenses
    describeStudioLifecycleConfig_studioLifecycleConfigName,

    -- * Destructuring the Response
    DescribeStudioLifecycleConfigResponse (..),
    newDescribeStudioLifecycleConfigResponse,

    -- * Response Lenses
    describeStudioLifecycleConfigResponse_creationTime,
    describeStudioLifecycleConfigResponse_studioLifecycleConfigArn,
    describeStudioLifecycleConfigResponse_studioLifecycleConfigName,
    describeStudioLifecycleConfigResponse_studioLifecycleConfigAppType,
    describeStudioLifecycleConfigResponse_lastModifiedTime,
    describeStudioLifecycleConfigResponse_studioLifecycleConfigContent,
    describeStudioLifecycleConfigResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeStudioLifecycleConfig' smart constructor.
data DescribeStudioLifecycleConfig = DescribeStudioLifecycleConfig'
  { -- | The name of the Studio Lifecycle Configuration to describe.
    studioLifecycleConfigName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStudioLifecycleConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'studioLifecycleConfigName', 'describeStudioLifecycleConfig_studioLifecycleConfigName' - The name of the Studio Lifecycle Configuration to describe.
newDescribeStudioLifecycleConfig ::
  -- | 'studioLifecycleConfigName'
  Prelude.Text ->
  DescribeStudioLifecycleConfig
newDescribeStudioLifecycleConfig
  pStudioLifecycleConfigName_ =
    DescribeStudioLifecycleConfig'
      { studioLifecycleConfigName =
          pStudioLifecycleConfigName_
      }

-- | The name of the Studio Lifecycle Configuration to describe.
describeStudioLifecycleConfig_studioLifecycleConfigName :: Lens.Lens' DescribeStudioLifecycleConfig Prelude.Text
describeStudioLifecycleConfig_studioLifecycleConfigName = Lens.lens (\DescribeStudioLifecycleConfig' {studioLifecycleConfigName} -> studioLifecycleConfigName) (\s@DescribeStudioLifecycleConfig' {} a -> s {studioLifecycleConfigName = a} :: DescribeStudioLifecycleConfig)

instance
  Core.AWSRequest
    DescribeStudioLifecycleConfig
  where
  type
    AWSResponse DescribeStudioLifecycleConfig =
      DescribeStudioLifecycleConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStudioLifecycleConfigResponse'
            Prelude.<$> (x Core..?> "CreationTime")
            Prelude.<*> (x Core..?> "StudioLifecycleConfigArn")
            Prelude.<*> (x Core..?> "StudioLifecycleConfigName")
            Prelude.<*> (x Core..?> "StudioLifecycleConfigAppType")
            Prelude.<*> (x Core..?> "LastModifiedTime")
            Prelude.<*> (x Core..?> "StudioLifecycleConfigContent")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeStudioLifecycleConfig

instance Prelude.NFData DescribeStudioLifecycleConfig

instance Core.ToHeaders DescribeStudioLifecycleConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DescribeStudioLifecycleConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeStudioLifecycleConfig where
  toJSON DescribeStudioLifecycleConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "StudioLifecycleConfigName"
                  Core..= studioLifecycleConfigName
              )
          ]
      )

instance Core.ToPath DescribeStudioLifecycleConfig where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeStudioLifecycleConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeStudioLifecycleConfigResponse' smart constructor.
data DescribeStudioLifecycleConfigResponse = DescribeStudioLifecycleConfigResponse'
  { -- | The creation time of the Studio Lifecycle Configuration.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The ARN of the Lifecycle Configuration to describe.
    studioLifecycleConfigArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the Studio Lifecycle Configuration that is described.
    studioLifecycleConfigName :: Prelude.Maybe Prelude.Text,
    -- | The App type that the Lifecycle Configuration is attached to.
    studioLifecycleConfigAppType :: Prelude.Maybe StudioLifecycleConfigAppType,
    -- | This value is equivalent to CreationTime because Studio Lifecycle
    -- Configurations are immutable.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The content of your Studio Lifecycle Configuration script.
    studioLifecycleConfigContent :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStudioLifecycleConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'describeStudioLifecycleConfigResponse_creationTime' - The creation time of the Studio Lifecycle Configuration.
--
-- 'studioLifecycleConfigArn', 'describeStudioLifecycleConfigResponse_studioLifecycleConfigArn' - The ARN of the Lifecycle Configuration to describe.
--
-- 'studioLifecycleConfigName', 'describeStudioLifecycleConfigResponse_studioLifecycleConfigName' - The name of the Studio Lifecycle Configuration that is described.
--
-- 'studioLifecycleConfigAppType', 'describeStudioLifecycleConfigResponse_studioLifecycleConfigAppType' - The App type that the Lifecycle Configuration is attached to.
--
-- 'lastModifiedTime', 'describeStudioLifecycleConfigResponse_lastModifiedTime' - This value is equivalent to CreationTime because Studio Lifecycle
-- Configurations are immutable.
--
-- 'studioLifecycleConfigContent', 'describeStudioLifecycleConfigResponse_studioLifecycleConfigContent' - The content of your Studio Lifecycle Configuration script.
--
-- 'httpStatus', 'describeStudioLifecycleConfigResponse_httpStatus' - The response's http status code.
newDescribeStudioLifecycleConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeStudioLifecycleConfigResponse
newDescribeStudioLifecycleConfigResponse pHttpStatus_ =
  DescribeStudioLifecycleConfigResponse'
    { creationTime =
        Prelude.Nothing,
      studioLifecycleConfigArn =
        Prelude.Nothing,
      studioLifecycleConfigName =
        Prelude.Nothing,
      studioLifecycleConfigAppType =
        Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      studioLifecycleConfigContent =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The creation time of the Studio Lifecycle Configuration.
describeStudioLifecycleConfigResponse_creationTime :: Lens.Lens' DescribeStudioLifecycleConfigResponse (Prelude.Maybe Prelude.UTCTime)
describeStudioLifecycleConfigResponse_creationTime = Lens.lens (\DescribeStudioLifecycleConfigResponse' {creationTime} -> creationTime) (\s@DescribeStudioLifecycleConfigResponse' {} a -> s {creationTime = a} :: DescribeStudioLifecycleConfigResponse) Prelude.. Lens.mapping Core._Time

-- | The ARN of the Lifecycle Configuration to describe.
describeStudioLifecycleConfigResponse_studioLifecycleConfigArn :: Lens.Lens' DescribeStudioLifecycleConfigResponse (Prelude.Maybe Prelude.Text)
describeStudioLifecycleConfigResponse_studioLifecycleConfigArn = Lens.lens (\DescribeStudioLifecycleConfigResponse' {studioLifecycleConfigArn} -> studioLifecycleConfigArn) (\s@DescribeStudioLifecycleConfigResponse' {} a -> s {studioLifecycleConfigArn = a} :: DescribeStudioLifecycleConfigResponse)

-- | The name of the Studio Lifecycle Configuration that is described.
describeStudioLifecycleConfigResponse_studioLifecycleConfigName :: Lens.Lens' DescribeStudioLifecycleConfigResponse (Prelude.Maybe Prelude.Text)
describeStudioLifecycleConfigResponse_studioLifecycleConfigName = Lens.lens (\DescribeStudioLifecycleConfigResponse' {studioLifecycleConfigName} -> studioLifecycleConfigName) (\s@DescribeStudioLifecycleConfigResponse' {} a -> s {studioLifecycleConfigName = a} :: DescribeStudioLifecycleConfigResponse)

-- | The App type that the Lifecycle Configuration is attached to.
describeStudioLifecycleConfigResponse_studioLifecycleConfigAppType :: Lens.Lens' DescribeStudioLifecycleConfigResponse (Prelude.Maybe StudioLifecycleConfigAppType)
describeStudioLifecycleConfigResponse_studioLifecycleConfigAppType = Lens.lens (\DescribeStudioLifecycleConfigResponse' {studioLifecycleConfigAppType} -> studioLifecycleConfigAppType) (\s@DescribeStudioLifecycleConfigResponse' {} a -> s {studioLifecycleConfigAppType = a} :: DescribeStudioLifecycleConfigResponse)

-- | This value is equivalent to CreationTime because Studio Lifecycle
-- Configurations are immutable.
describeStudioLifecycleConfigResponse_lastModifiedTime :: Lens.Lens' DescribeStudioLifecycleConfigResponse (Prelude.Maybe Prelude.UTCTime)
describeStudioLifecycleConfigResponse_lastModifiedTime = Lens.lens (\DescribeStudioLifecycleConfigResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeStudioLifecycleConfigResponse' {} a -> s {lastModifiedTime = a} :: DescribeStudioLifecycleConfigResponse) Prelude.. Lens.mapping Core._Time

-- | The content of your Studio Lifecycle Configuration script.
describeStudioLifecycleConfigResponse_studioLifecycleConfigContent :: Lens.Lens' DescribeStudioLifecycleConfigResponse (Prelude.Maybe Prelude.Text)
describeStudioLifecycleConfigResponse_studioLifecycleConfigContent = Lens.lens (\DescribeStudioLifecycleConfigResponse' {studioLifecycleConfigContent} -> studioLifecycleConfigContent) (\s@DescribeStudioLifecycleConfigResponse' {} a -> s {studioLifecycleConfigContent = a} :: DescribeStudioLifecycleConfigResponse)

-- | The response's http status code.
describeStudioLifecycleConfigResponse_httpStatus :: Lens.Lens' DescribeStudioLifecycleConfigResponse Prelude.Int
describeStudioLifecycleConfigResponse_httpStatus = Lens.lens (\DescribeStudioLifecycleConfigResponse' {httpStatus} -> httpStatus) (\s@DescribeStudioLifecycleConfigResponse' {} a -> s {httpStatus = a} :: DescribeStudioLifecycleConfigResponse)

instance
  Prelude.NFData
    DescribeStudioLifecycleConfigResponse

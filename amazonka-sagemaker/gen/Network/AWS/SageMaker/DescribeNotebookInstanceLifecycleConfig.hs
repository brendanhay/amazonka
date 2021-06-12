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
-- Module      : Network.AWS.SageMaker.DescribeNotebookInstanceLifecycleConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of a notebook instance lifecycle configuration.
--
-- For information about notebook instance lifestyle configurations, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance>.
module Network.AWS.SageMaker.DescribeNotebookInstanceLifecycleConfig
  ( -- * Creating a Request
    DescribeNotebookInstanceLifecycleConfig (..),
    newDescribeNotebookInstanceLifecycleConfig,

    -- * Request Lenses
    describeNotebookInstanceLifecycleConfig_notebookInstanceLifecycleConfigName,

    -- * Destructuring the Response
    DescribeNotebookInstanceLifecycleConfigResponse (..),
    newDescribeNotebookInstanceLifecycleConfigResponse,

    -- * Response Lenses
    describeNotebookInstanceLifecycleConfigResponse_creationTime,
    describeNotebookInstanceLifecycleConfigResponse_notebookInstanceLifecycleConfigArn,
    describeNotebookInstanceLifecycleConfigResponse_notebookInstanceLifecycleConfigName,
    describeNotebookInstanceLifecycleConfigResponse_onStart,
    describeNotebookInstanceLifecycleConfigResponse_lastModifiedTime,
    describeNotebookInstanceLifecycleConfigResponse_onCreate,
    describeNotebookInstanceLifecycleConfigResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeNotebookInstanceLifecycleConfig' smart constructor.
data DescribeNotebookInstanceLifecycleConfig = DescribeNotebookInstanceLifecycleConfig'
  { -- | The name of the lifecycle configuration to describe.
    notebookInstanceLifecycleConfigName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeNotebookInstanceLifecycleConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notebookInstanceLifecycleConfigName', 'describeNotebookInstanceLifecycleConfig_notebookInstanceLifecycleConfigName' - The name of the lifecycle configuration to describe.
newDescribeNotebookInstanceLifecycleConfig ::
  -- | 'notebookInstanceLifecycleConfigName'
  Core.Text ->
  DescribeNotebookInstanceLifecycleConfig
newDescribeNotebookInstanceLifecycleConfig
  pNotebookInstanceLifecycleConfigName_ =
    DescribeNotebookInstanceLifecycleConfig'
      { notebookInstanceLifecycleConfigName =
          pNotebookInstanceLifecycleConfigName_
      }

-- | The name of the lifecycle configuration to describe.
describeNotebookInstanceLifecycleConfig_notebookInstanceLifecycleConfigName :: Lens.Lens' DescribeNotebookInstanceLifecycleConfig Core.Text
describeNotebookInstanceLifecycleConfig_notebookInstanceLifecycleConfigName = Lens.lens (\DescribeNotebookInstanceLifecycleConfig' {notebookInstanceLifecycleConfigName} -> notebookInstanceLifecycleConfigName) (\s@DescribeNotebookInstanceLifecycleConfig' {} a -> s {notebookInstanceLifecycleConfigName = a} :: DescribeNotebookInstanceLifecycleConfig)

instance
  Core.AWSRequest
    DescribeNotebookInstanceLifecycleConfig
  where
  type
    AWSResponse
      DescribeNotebookInstanceLifecycleConfig =
      DescribeNotebookInstanceLifecycleConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeNotebookInstanceLifecycleConfigResponse'
            Core.<$> (x Core..?> "CreationTime")
              Core.<*> (x Core..?> "NotebookInstanceLifecycleConfigArn")
              Core.<*> (x Core..?> "NotebookInstanceLifecycleConfigName")
              Core.<*> (x Core..?> "OnStart" Core..!@ Core.mempty)
              Core.<*> (x Core..?> "LastModifiedTime")
              Core.<*> (x Core..?> "OnCreate" Core..!@ Core.mempty)
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeNotebookInstanceLifecycleConfig

instance
  Core.NFData
    DescribeNotebookInstanceLifecycleConfig

instance
  Core.ToHeaders
    DescribeNotebookInstanceLifecycleConfig
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DescribeNotebookInstanceLifecycleConfig" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DescribeNotebookInstanceLifecycleConfig
  where
  toJSON DescribeNotebookInstanceLifecycleConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "NotebookInstanceLifecycleConfigName"
                  Core..= notebookInstanceLifecycleConfigName
              )
          ]
      )

instance
  Core.ToPath
    DescribeNotebookInstanceLifecycleConfig
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeNotebookInstanceLifecycleConfig
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeNotebookInstanceLifecycleConfigResponse' smart constructor.
data DescribeNotebookInstanceLifecycleConfigResponse = DescribeNotebookInstanceLifecycleConfigResponse'
  { -- | A timestamp that tells when the lifecycle configuration was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the lifecycle configuration.
    notebookInstanceLifecycleConfigArn :: Core.Maybe Core.Text,
    -- | The name of the lifecycle configuration.
    notebookInstanceLifecycleConfigName :: Core.Maybe Core.Text,
    -- | The shell script that runs every time you start a notebook instance,
    -- including when you create the notebook instance.
    onStart :: Core.Maybe [NotebookInstanceLifecycleHook],
    -- | A timestamp that tells when the lifecycle configuration was last
    -- modified.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | The shell script that runs only once, when you create a notebook
    -- instance.
    onCreate :: Core.Maybe [NotebookInstanceLifecycleHook],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeNotebookInstanceLifecycleConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'describeNotebookInstanceLifecycleConfigResponse_creationTime' - A timestamp that tells when the lifecycle configuration was created.
--
-- 'notebookInstanceLifecycleConfigArn', 'describeNotebookInstanceLifecycleConfigResponse_notebookInstanceLifecycleConfigArn' - The Amazon Resource Name (ARN) of the lifecycle configuration.
--
-- 'notebookInstanceLifecycleConfigName', 'describeNotebookInstanceLifecycleConfigResponse_notebookInstanceLifecycleConfigName' - The name of the lifecycle configuration.
--
-- 'onStart', 'describeNotebookInstanceLifecycleConfigResponse_onStart' - The shell script that runs every time you start a notebook instance,
-- including when you create the notebook instance.
--
-- 'lastModifiedTime', 'describeNotebookInstanceLifecycleConfigResponse_lastModifiedTime' - A timestamp that tells when the lifecycle configuration was last
-- modified.
--
-- 'onCreate', 'describeNotebookInstanceLifecycleConfigResponse_onCreate' - The shell script that runs only once, when you create a notebook
-- instance.
--
-- 'httpStatus', 'describeNotebookInstanceLifecycleConfigResponse_httpStatus' - The response's http status code.
newDescribeNotebookInstanceLifecycleConfigResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeNotebookInstanceLifecycleConfigResponse
newDescribeNotebookInstanceLifecycleConfigResponse
  pHttpStatus_ =
    DescribeNotebookInstanceLifecycleConfigResponse'
      { creationTime =
          Core.Nothing,
        notebookInstanceLifecycleConfigArn =
          Core.Nothing,
        notebookInstanceLifecycleConfigName =
          Core.Nothing,
        onStart = Core.Nothing,
        lastModifiedTime =
          Core.Nothing,
        onCreate = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A timestamp that tells when the lifecycle configuration was created.
describeNotebookInstanceLifecycleConfigResponse_creationTime :: Lens.Lens' DescribeNotebookInstanceLifecycleConfigResponse (Core.Maybe Core.UTCTime)
describeNotebookInstanceLifecycleConfigResponse_creationTime = Lens.lens (\DescribeNotebookInstanceLifecycleConfigResponse' {creationTime} -> creationTime) (\s@DescribeNotebookInstanceLifecycleConfigResponse' {} a -> s {creationTime = a} :: DescribeNotebookInstanceLifecycleConfigResponse) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the lifecycle configuration.
describeNotebookInstanceLifecycleConfigResponse_notebookInstanceLifecycleConfigArn :: Lens.Lens' DescribeNotebookInstanceLifecycleConfigResponse (Core.Maybe Core.Text)
describeNotebookInstanceLifecycleConfigResponse_notebookInstanceLifecycleConfigArn = Lens.lens (\DescribeNotebookInstanceLifecycleConfigResponse' {notebookInstanceLifecycleConfigArn} -> notebookInstanceLifecycleConfigArn) (\s@DescribeNotebookInstanceLifecycleConfigResponse' {} a -> s {notebookInstanceLifecycleConfigArn = a} :: DescribeNotebookInstanceLifecycleConfigResponse)

-- | The name of the lifecycle configuration.
describeNotebookInstanceLifecycleConfigResponse_notebookInstanceLifecycleConfigName :: Lens.Lens' DescribeNotebookInstanceLifecycleConfigResponse (Core.Maybe Core.Text)
describeNotebookInstanceLifecycleConfigResponse_notebookInstanceLifecycleConfigName = Lens.lens (\DescribeNotebookInstanceLifecycleConfigResponse' {notebookInstanceLifecycleConfigName} -> notebookInstanceLifecycleConfigName) (\s@DescribeNotebookInstanceLifecycleConfigResponse' {} a -> s {notebookInstanceLifecycleConfigName = a} :: DescribeNotebookInstanceLifecycleConfigResponse)

-- | The shell script that runs every time you start a notebook instance,
-- including when you create the notebook instance.
describeNotebookInstanceLifecycleConfigResponse_onStart :: Lens.Lens' DescribeNotebookInstanceLifecycleConfigResponse (Core.Maybe [NotebookInstanceLifecycleHook])
describeNotebookInstanceLifecycleConfigResponse_onStart = Lens.lens (\DescribeNotebookInstanceLifecycleConfigResponse' {onStart} -> onStart) (\s@DescribeNotebookInstanceLifecycleConfigResponse' {} a -> s {onStart = a} :: DescribeNotebookInstanceLifecycleConfigResponse) Core.. Lens.mapping Lens._Coerce

-- | A timestamp that tells when the lifecycle configuration was last
-- modified.
describeNotebookInstanceLifecycleConfigResponse_lastModifiedTime :: Lens.Lens' DescribeNotebookInstanceLifecycleConfigResponse (Core.Maybe Core.UTCTime)
describeNotebookInstanceLifecycleConfigResponse_lastModifiedTime = Lens.lens (\DescribeNotebookInstanceLifecycleConfigResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeNotebookInstanceLifecycleConfigResponse' {} a -> s {lastModifiedTime = a} :: DescribeNotebookInstanceLifecycleConfigResponse) Core.. Lens.mapping Core._Time

-- | The shell script that runs only once, when you create a notebook
-- instance.
describeNotebookInstanceLifecycleConfigResponse_onCreate :: Lens.Lens' DescribeNotebookInstanceLifecycleConfigResponse (Core.Maybe [NotebookInstanceLifecycleHook])
describeNotebookInstanceLifecycleConfigResponse_onCreate = Lens.lens (\DescribeNotebookInstanceLifecycleConfigResponse' {onCreate} -> onCreate) (\s@DescribeNotebookInstanceLifecycleConfigResponse' {} a -> s {onCreate = a} :: DescribeNotebookInstanceLifecycleConfigResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeNotebookInstanceLifecycleConfigResponse_httpStatus :: Lens.Lens' DescribeNotebookInstanceLifecycleConfigResponse Core.Int
describeNotebookInstanceLifecycleConfigResponse_httpStatus = Lens.lens (\DescribeNotebookInstanceLifecycleConfigResponse' {httpStatus} -> httpStatus) (\s@DescribeNotebookInstanceLifecycleConfigResponse' {} a -> s {httpStatus = a} :: DescribeNotebookInstanceLifecycleConfigResponse)

instance
  Core.NFData
    DescribeNotebookInstanceLifecycleConfigResponse

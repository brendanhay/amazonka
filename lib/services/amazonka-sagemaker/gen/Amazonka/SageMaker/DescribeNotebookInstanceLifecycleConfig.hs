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
-- Module      : Amazonka.SageMaker.DescribeNotebookInstanceLifecycleConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of a notebook instance lifecycle configuration.
--
-- For information about notebook instance lifestyle configurations, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance>.
module Amazonka.SageMaker.DescribeNotebookInstanceLifecycleConfig
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
    describeNotebookInstanceLifecycleConfigResponse_lastModifiedTime,
    describeNotebookInstanceLifecycleConfigResponse_notebookInstanceLifecycleConfigArn,
    describeNotebookInstanceLifecycleConfigResponse_notebookInstanceLifecycleConfigName,
    describeNotebookInstanceLifecycleConfigResponse_onCreate,
    describeNotebookInstanceLifecycleConfigResponse_onStart,
    describeNotebookInstanceLifecycleConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeNotebookInstanceLifecycleConfig' smart constructor.
data DescribeNotebookInstanceLifecycleConfig = DescribeNotebookInstanceLifecycleConfig'
  { -- | The name of the lifecycle configuration to describe.
    notebookInstanceLifecycleConfigName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeNotebookInstanceLifecycleConfig
newDescribeNotebookInstanceLifecycleConfig
  pNotebookInstanceLifecycleConfigName_ =
    DescribeNotebookInstanceLifecycleConfig'
      { notebookInstanceLifecycleConfigName =
          pNotebookInstanceLifecycleConfigName_
      }

-- | The name of the lifecycle configuration to describe.
describeNotebookInstanceLifecycleConfig_notebookInstanceLifecycleConfigName :: Lens.Lens' DescribeNotebookInstanceLifecycleConfig Prelude.Text
describeNotebookInstanceLifecycleConfig_notebookInstanceLifecycleConfigName = Lens.lens (\DescribeNotebookInstanceLifecycleConfig' {notebookInstanceLifecycleConfigName} -> notebookInstanceLifecycleConfigName) (\s@DescribeNotebookInstanceLifecycleConfig' {} a -> s {notebookInstanceLifecycleConfigName = a} :: DescribeNotebookInstanceLifecycleConfig)

instance
  Core.AWSRequest
    DescribeNotebookInstanceLifecycleConfig
  where
  type
    AWSResponse
      DescribeNotebookInstanceLifecycleConfig =
      DescribeNotebookInstanceLifecycleConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeNotebookInstanceLifecycleConfigResponse'
            Prelude.<$> (x Data..?> "CreationTime")
              Prelude.<*> (x Data..?> "LastModifiedTime")
              Prelude.<*> (x Data..?> "NotebookInstanceLifecycleConfigArn")
              Prelude.<*> (x Data..?> "NotebookInstanceLifecycleConfigName")
              Prelude.<*> (x Data..?> "OnCreate" Core..!@ Prelude.mempty)
              Prelude.<*> (x Data..?> "OnStart" Core..!@ Prelude.mempty)
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeNotebookInstanceLifecycleConfig
  where
  hashWithSalt
    _salt
    DescribeNotebookInstanceLifecycleConfig' {..} =
      _salt
        `Prelude.hashWithSalt` notebookInstanceLifecycleConfigName

instance
  Prelude.NFData
    DescribeNotebookInstanceLifecycleConfig
  where
  rnf DescribeNotebookInstanceLifecycleConfig' {..} =
    Prelude.rnf notebookInstanceLifecycleConfigName

instance
  Data.ToHeaders
    DescribeNotebookInstanceLifecycleConfig
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DescribeNotebookInstanceLifecycleConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeNotebookInstanceLifecycleConfig
  where
  toJSON DescribeNotebookInstanceLifecycleConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "NotebookInstanceLifecycleConfigName"
                  Data..= notebookInstanceLifecycleConfigName
              )
          ]
      )

instance
  Data.ToPath
    DescribeNotebookInstanceLifecycleConfig
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeNotebookInstanceLifecycleConfig
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeNotebookInstanceLifecycleConfigResponse' smart constructor.
data DescribeNotebookInstanceLifecycleConfigResponse = DescribeNotebookInstanceLifecycleConfigResponse'
  { -- | A timestamp that tells when the lifecycle configuration was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | A timestamp that tells when the lifecycle configuration was last
    -- modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the lifecycle configuration.
    notebookInstanceLifecycleConfigArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the lifecycle configuration.
    notebookInstanceLifecycleConfigName :: Prelude.Maybe Prelude.Text,
    -- | The shell script that runs only once, when you create a notebook
    -- instance.
    onCreate :: Prelude.Maybe [NotebookInstanceLifecycleHook],
    -- | The shell script that runs every time you start a notebook instance,
    -- including when you create the notebook instance.
    onStart :: Prelude.Maybe [NotebookInstanceLifecycleHook],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'lastModifiedTime', 'describeNotebookInstanceLifecycleConfigResponse_lastModifiedTime' - A timestamp that tells when the lifecycle configuration was last
-- modified.
--
-- 'notebookInstanceLifecycleConfigArn', 'describeNotebookInstanceLifecycleConfigResponse_notebookInstanceLifecycleConfigArn' - The Amazon Resource Name (ARN) of the lifecycle configuration.
--
-- 'notebookInstanceLifecycleConfigName', 'describeNotebookInstanceLifecycleConfigResponse_notebookInstanceLifecycleConfigName' - The name of the lifecycle configuration.
--
-- 'onCreate', 'describeNotebookInstanceLifecycleConfigResponse_onCreate' - The shell script that runs only once, when you create a notebook
-- instance.
--
-- 'onStart', 'describeNotebookInstanceLifecycleConfigResponse_onStart' - The shell script that runs every time you start a notebook instance,
-- including when you create the notebook instance.
--
-- 'httpStatus', 'describeNotebookInstanceLifecycleConfigResponse_httpStatus' - The response's http status code.
newDescribeNotebookInstanceLifecycleConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeNotebookInstanceLifecycleConfigResponse
newDescribeNotebookInstanceLifecycleConfigResponse
  pHttpStatus_ =
    DescribeNotebookInstanceLifecycleConfigResponse'
      { creationTime =
          Prelude.Nothing,
        lastModifiedTime =
          Prelude.Nothing,
        notebookInstanceLifecycleConfigArn =
          Prelude.Nothing,
        notebookInstanceLifecycleConfigName =
          Prelude.Nothing,
        onCreate = Prelude.Nothing,
        onStart = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A timestamp that tells when the lifecycle configuration was created.
describeNotebookInstanceLifecycleConfigResponse_creationTime :: Lens.Lens' DescribeNotebookInstanceLifecycleConfigResponse (Prelude.Maybe Prelude.UTCTime)
describeNotebookInstanceLifecycleConfigResponse_creationTime = Lens.lens (\DescribeNotebookInstanceLifecycleConfigResponse' {creationTime} -> creationTime) (\s@DescribeNotebookInstanceLifecycleConfigResponse' {} a -> s {creationTime = a} :: DescribeNotebookInstanceLifecycleConfigResponse) Prelude.. Lens.mapping Data._Time

-- | A timestamp that tells when the lifecycle configuration was last
-- modified.
describeNotebookInstanceLifecycleConfigResponse_lastModifiedTime :: Lens.Lens' DescribeNotebookInstanceLifecycleConfigResponse (Prelude.Maybe Prelude.UTCTime)
describeNotebookInstanceLifecycleConfigResponse_lastModifiedTime = Lens.lens (\DescribeNotebookInstanceLifecycleConfigResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeNotebookInstanceLifecycleConfigResponse' {} a -> s {lastModifiedTime = a} :: DescribeNotebookInstanceLifecycleConfigResponse) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the lifecycle configuration.
describeNotebookInstanceLifecycleConfigResponse_notebookInstanceLifecycleConfigArn :: Lens.Lens' DescribeNotebookInstanceLifecycleConfigResponse (Prelude.Maybe Prelude.Text)
describeNotebookInstanceLifecycleConfigResponse_notebookInstanceLifecycleConfigArn = Lens.lens (\DescribeNotebookInstanceLifecycleConfigResponse' {notebookInstanceLifecycleConfigArn} -> notebookInstanceLifecycleConfigArn) (\s@DescribeNotebookInstanceLifecycleConfigResponse' {} a -> s {notebookInstanceLifecycleConfigArn = a} :: DescribeNotebookInstanceLifecycleConfigResponse)

-- | The name of the lifecycle configuration.
describeNotebookInstanceLifecycleConfigResponse_notebookInstanceLifecycleConfigName :: Lens.Lens' DescribeNotebookInstanceLifecycleConfigResponse (Prelude.Maybe Prelude.Text)
describeNotebookInstanceLifecycleConfigResponse_notebookInstanceLifecycleConfigName = Lens.lens (\DescribeNotebookInstanceLifecycleConfigResponse' {notebookInstanceLifecycleConfigName} -> notebookInstanceLifecycleConfigName) (\s@DescribeNotebookInstanceLifecycleConfigResponse' {} a -> s {notebookInstanceLifecycleConfigName = a} :: DescribeNotebookInstanceLifecycleConfigResponse)

-- | The shell script that runs only once, when you create a notebook
-- instance.
describeNotebookInstanceLifecycleConfigResponse_onCreate :: Lens.Lens' DescribeNotebookInstanceLifecycleConfigResponse (Prelude.Maybe [NotebookInstanceLifecycleHook])
describeNotebookInstanceLifecycleConfigResponse_onCreate = Lens.lens (\DescribeNotebookInstanceLifecycleConfigResponse' {onCreate} -> onCreate) (\s@DescribeNotebookInstanceLifecycleConfigResponse' {} a -> s {onCreate = a} :: DescribeNotebookInstanceLifecycleConfigResponse) Prelude.. Lens.mapping Lens.coerced

-- | The shell script that runs every time you start a notebook instance,
-- including when you create the notebook instance.
describeNotebookInstanceLifecycleConfigResponse_onStart :: Lens.Lens' DescribeNotebookInstanceLifecycleConfigResponse (Prelude.Maybe [NotebookInstanceLifecycleHook])
describeNotebookInstanceLifecycleConfigResponse_onStart = Lens.lens (\DescribeNotebookInstanceLifecycleConfigResponse' {onStart} -> onStart) (\s@DescribeNotebookInstanceLifecycleConfigResponse' {} a -> s {onStart = a} :: DescribeNotebookInstanceLifecycleConfigResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeNotebookInstanceLifecycleConfigResponse_httpStatus :: Lens.Lens' DescribeNotebookInstanceLifecycleConfigResponse Prelude.Int
describeNotebookInstanceLifecycleConfigResponse_httpStatus = Lens.lens (\DescribeNotebookInstanceLifecycleConfigResponse' {httpStatus} -> httpStatus) (\s@DescribeNotebookInstanceLifecycleConfigResponse' {} a -> s {httpStatus = a} :: DescribeNotebookInstanceLifecycleConfigResponse)

instance
  Prelude.NFData
    DescribeNotebookInstanceLifecycleConfigResponse
  where
  rnf
    DescribeNotebookInstanceLifecycleConfigResponse' {..} =
      Prelude.rnf creationTime
        `Prelude.seq` Prelude.rnf lastModifiedTime
        `Prelude.seq` Prelude.rnf notebookInstanceLifecycleConfigArn
        `Prelude.seq` Prelude.rnf notebookInstanceLifecycleConfigName
        `Prelude.seq` Prelude.rnf onCreate
        `Prelude.seq` Prelude.rnf onStart
        `Prelude.seq` Prelude.rnf httpStatus

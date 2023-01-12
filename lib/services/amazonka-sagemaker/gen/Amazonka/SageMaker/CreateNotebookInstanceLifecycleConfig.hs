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
-- Module      : Amazonka.SageMaker.CreateNotebookInstanceLifecycleConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a lifecycle configuration that you can associate with a notebook
-- instance. A /lifecycle configuration/ is a collection of shell scripts
-- that run when you create or start a notebook instance.
--
-- Each lifecycle configuration script has a limit of 16384 characters.
--
-- The value of the @$PATH@ environment variable that is available to both
-- scripts is @\/sbin:bin:\/usr\/sbin:\/usr\/bin@.
--
-- View CloudWatch Logs for notebook instance lifecycle configurations in
-- log group @\/aws\/sagemaker\/NotebookInstances@ in log stream
-- @[notebook-instance-name]\/[LifecycleConfigHook]@.
--
-- Lifecycle configuration scripts cannot run for longer than 5 minutes. If
-- a script runs for longer than 5 minutes, it fails and the notebook
-- instance is not created or started.
--
-- For information about notebook instance lifestyle configurations, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance>.
module Amazonka.SageMaker.CreateNotebookInstanceLifecycleConfig
  ( -- * Creating a Request
    CreateNotebookInstanceLifecycleConfig (..),
    newCreateNotebookInstanceLifecycleConfig,

    -- * Request Lenses
    createNotebookInstanceLifecycleConfig_onCreate,
    createNotebookInstanceLifecycleConfig_onStart,
    createNotebookInstanceLifecycleConfig_notebookInstanceLifecycleConfigName,

    -- * Destructuring the Response
    CreateNotebookInstanceLifecycleConfigResponse (..),
    newCreateNotebookInstanceLifecycleConfigResponse,

    -- * Response Lenses
    createNotebookInstanceLifecycleConfigResponse_notebookInstanceLifecycleConfigArn,
    createNotebookInstanceLifecycleConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateNotebookInstanceLifecycleConfig' smart constructor.
data CreateNotebookInstanceLifecycleConfig = CreateNotebookInstanceLifecycleConfig'
  { -- | A shell script that runs only once, when you create a notebook instance.
    -- The shell script must be a base64-encoded string.
    onCreate :: Prelude.Maybe [NotebookInstanceLifecycleHook],
    -- | A shell script that runs every time you start a notebook instance,
    -- including when you create the notebook instance. The shell script must
    -- be a base64-encoded string.
    onStart :: Prelude.Maybe [NotebookInstanceLifecycleHook],
    -- | The name of the lifecycle configuration.
    notebookInstanceLifecycleConfigName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNotebookInstanceLifecycleConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'onCreate', 'createNotebookInstanceLifecycleConfig_onCreate' - A shell script that runs only once, when you create a notebook instance.
-- The shell script must be a base64-encoded string.
--
-- 'onStart', 'createNotebookInstanceLifecycleConfig_onStart' - A shell script that runs every time you start a notebook instance,
-- including when you create the notebook instance. The shell script must
-- be a base64-encoded string.
--
-- 'notebookInstanceLifecycleConfigName', 'createNotebookInstanceLifecycleConfig_notebookInstanceLifecycleConfigName' - The name of the lifecycle configuration.
newCreateNotebookInstanceLifecycleConfig ::
  -- | 'notebookInstanceLifecycleConfigName'
  Prelude.Text ->
  CreateNotebookInstanceLifecycleConfig
newCreateNotebookInstanceLifecycleConfig
  pNotebookInstanceLifecycleConfigName_ =
    CreateNotebookInstanceLifecycleConfig'
      { onCreate =
          Prelude.Nothing,
        onStart = Prelude.Nothing,
        notebookInstanceLifecycleConfigName =
          pNotebookInstanceLifecycleConfigName_
      }

-- | A shell script that runs only once, when you create a notebook instance.
-- The shell script must be a base64-encoded string.
createNotebookInstanceLifecycleConfig_onCreate :: Lens.Lens' CreateNotebookInstanceLifecycleConfig (Prelude.Maybe [NotebookInstanceLifecycleHook])
createNotebookInstanceLifecycleConfig_onCreate = Lens.lens (\CreateNotebookInstanceLifecycleConfig' {onCreate} -> onCreate) (\s@CreateNotebookInstanceLifecycleConfig' {} a -> s {onCreate = a} :: CreateNotebookInstanceLifecycleConfig) Prelude.. Lens.mapping Lens.coerced

-- | A shell script that runs every time you start a notebook instance,
-- including when you create the notebook instance. The shell script must
-- be a base64-encoded string.
createNotebookInstanceLifecycleConfig_onStart :: Lens.Lens' CreateNotebookInstanceLifecycleConfig (Prelude.Maybe [NotebookInstanceLifecycleHook])
createNotebookInstanceLifecycleConfig_onStart = Lens.lens (\CreateNotebookInstanceLifecycleConfig' {onStart} -> onStart) (\s@CreateNotebookInstanceLifecycleConfig' {} a -> s {onStart = a} :: CreateNotebookInstanceLifecycleConfig) Prelude.. Lens.mapping Lens.coerced

-- | The name of the lifecycle configuration.
createNotebookInstanceLifecycleConfig_notebookInstanceLifecycleConfigName :: Lens.Lens' CreateNotebookInstanceLifecycleConfig Prelude.Text
createNotebookInstanceLifecycleConfig_notebookInstanceLifecycleConfigName = Lens.lens (\CreateNotebookInstanceLifecycleConfig' {notebookInstanceLifecycleConfigName} -> notebookInstanceLifecycleConfigName) (\s@CreateNotebookInstanceLifecycleConfig' {} a -> s {notebookInstanceLifecycleConfigName = a} :: CreateNotebookInstanceLifecycleConfig)

instance
  Core.AWSRequest
    CreateNotebookInstanceLifecycleConfig
  where
  type
    AWSResponse
      CreateNotebookInstanceLifecycleConfig =
      CreateNotebookInstanceLifecycleConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateNotebookInstanceLifecycleConfigResponse'
            Prelude.<$> (x Data..?> "NotebookInstanceLifecycleConfigArn")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateNotebookInstanceLifecycleConfig
  where
  hashWithSalt
    _salt
    CreateNotebookInstanceLifecycleConfig' {..} =
      _salt `Prelude.hashWithSalt` onCreate
        `Prelude.hashWithSalt` onStart
        `Prelude.hashWithSalt` notebookInstanceLifecycleConfigName

instance
  Prelude.NFData
    CreateNotebookInstanceLifecycleConfig
  where
  rnf CreateNotebookInstanceLifecycleConfig' {..} =
    Prelude.rnf onCreate
      `Prelude.seq` Prelude.rnf onStart
      `Prelude.seq` Prelude.rnf notebookInstanceLifecycleConfigName

instance
  Data.ToHeaders
    CreateNotebookInstanceLifecycleConfig
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.CreateNotebookInstanceLifecycleConfig" ::
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
    CreateNotebookInstanceLifecycleConfig
  where
  toJSON CreateNotebookInstanceLifecycleConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("OnCreate" Data..=) Prelude.<$> onCreate,
            ("OnStart" Data..=) Prelude.<$> onStart,
            Prelude.Just
              ( "NotebookInstanceLifecycleConfigName"
                  Data..= notebookInstanceLifecycleConfigName
              )
          ]
      )

instance
  Data.ToPath
    CreateNotebookInstanceLifecycleConfig
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    CreateNotebookInstanceLifecycleConfig
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateNotebookInstanceLifecycleConfigResponse' smart constructor.
data CreateNotebookInstanceLifecycleConfigResponse = CreateNotebookInstanceLifecycleConfigResponse'
  { -- | The Amazon Resource Name (ARN) of the lifecycle configuration.
    notebookInstanceLifecycleConfigArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNotebookInstanceLifecycleConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notebookInstanceLifecycleConfigArn', 'createNotebookInstanceLifecycleConfigResponse_notebookInstanceLifecycleConfigArn' - The Amazon Resource Name (ARN) of the lifecycle configuration.
--
-- 'httpStatus', 'createNotebookInstanceLifecycleConfigResponse_httpStatus' - The response's http status code.
newCreateNotebookInstanceLifecycleConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateNotebookInstanceLifecycleConfigResponse
newCreateNotebookInstanceLifecycleConfigResponse
  pHttpStatus_ =
    CreateNotebookInstanceLifecycleConfigResponse'
      { notebookInstanceLifecycleConfigArn =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The Amazon Resource Name (ARN) of the lifecycle configuration.
createNotebookInstanceLifecycleConfigResponse_notebookInstanceLifecycleConfigArn :: Lens.Lens' CreateNotebookInstanceLifecycleConfigResponse (Prelude.Maybe Prelude.Text)
createNotebookInstanceLifecycleConfigResponse_notebookInstanceLifecycleConfigArn = Lens.lens (\CreateNotebookInstanceLifecycleConfigResponse' {notebookInstanceLifecycleConfigArn} -> notebookInstanceLifecycleConfigArn) (\s@CreateNotebookInstanceLifecycleConfigResponse' {} a -> s {notebookInstanceLifecycleConfigArn = a} :: CreateNotebookInstanceLifecycleConfigResponse)

-- | The response's http status code.
createNotebookInstanceLifecycleConfigResponse_httpStatus :: Lens.Lens' CreateNotebookInstanceLifecycleConfigResponse Prelude.Int
createNotebookInstanceLifecycleConfigResponse_httpStatus = Lens.lens (\CreateNotebookInstanceLifecycleConfigResponse' {httpStatus} -> httpStatus) (\s@CreateNotebookInstanceLifecycleConfigResponse' {} a -> s {httpStatus = a} :: CreateNotebookInstanceLifecycleConfigResponse)

instance
  Prelude.NFData
    CreateNotebookInstanceLifecycleConfigResponse
  where
  rnf
    CreateNotebookInstanceLifecycleConfigResponse' {..} =
      Prelude.rnf notebookInstanceLifecycleConfigArn
        `Prelude.seq` Prelude.rnf httpStatus

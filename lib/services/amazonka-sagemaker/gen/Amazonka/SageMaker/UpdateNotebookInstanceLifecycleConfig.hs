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
-- Module      : Amazonka.SageMaker.UpdateNotebookInstanceLifecycleConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a notebook instance lifecycle configuration created with the
-- CreateNotebookInstanceLifecycleConfig API.
module Amazonka.SageMaker.UpdateNotebookInstanceLifecycleConfig
  ( -- * Creating a Request
    UpdateNotebookInstanceLifecycleConfig (..),
    newUpdateNotebookInstanceLifecycleConfig,

    -- * Request Lenses
    updateNotebookInstanceLifecycleConfig_onCreate,
    updateNotebookInstanceLifecycleConfig_onStart,
    updateNotebookInstanceLifecycleConfig_notebookInstanceLifecycleConfigName,

    -- * Destructuring the Response
    UpdateNotebookInstanceLifecycleConfigResponse (..),
    newUpdateNotebookInstanceLifecycleConfigResponse,

    -- * Response Lenses
    updateNotebookInstanceLifecycleConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newUpdateNotebookInstanceLifecycleConfig' smart constructor.
data UpdateNotebookInstanceLifecycleConfig = UpdateNotebookInstanceLifecycleConfig'
  { -- | The shell script that runs only once, when you create a notebook
    -- instance. The shell script must be a base64-encoded string.
    onCreate :: Prelude.Maybe [NotebookInstanceLifecycleHook],
    -- | The shell script that runs every time you start a notebook instance,
    -- including when you create the notebook instance. The shell script must
    -- be a base64-encoded string.
    onStart :: Prelude.Maybe [NotebookInstanceLifecycleHook],
    -- | The name of the lifecycle configuration.
    notebookInstanceLifecycleConfigName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateNotebookInstanceLifecycleConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'onCreate', 'updateNotebookInstanceLifecycleConfig_onCreate' - The shell script that runs only once, when you create a notebook
-- instance. The shell script must be a base64-encoded string.
--
-- 'onStart', 'updateNotebookInstanceLifecycleConfig_onStart' - The shell script that runs every time you start a notebook instance,
-- including when you create the notebook instance. The shell script must
-- be a base64-encoded string.
--
-- 'notebookInstanceLifecycleConfigName', 'updateNotebookInstanceLifecycleConfig_notebookInstanceLifecycleConfigName' - The name of the lifecycle configuration.
newUpdateNotebookInstanceLifecycleConfig ::
  -- | 'notebookInstanceLifecycleConfigName'
  Prelude.Text ->
  UpdateNotebookInstanceLifecycleConfig
newUpdateNotebookInstanceLifecycleConfig
  pNotebookInstanceLifecycleConfigName_ =
    UpdateNotebookInstanceLifecycleConfig'
      { onCreate =
          Prelude.Nothing,
        onStart = Prelude.Nothing,
        notebookInstanceLifecycleConfigName =
          pNotebookInstanceLifecycleConfigName_
      }

-- | The shell script that runs only once, when you create a notebook
-- instance. The shell script must be a base64-encoded string.
updateNotebookInstanceLifecycleConfig_onCreate :: Lens.Lens' UpdateNotebookInstanceLifecycleConfig (Prelude.Maybe [NotebookInstanceLifecycleHook])
updateNotebookInstanceLifecycleConfig_onCreate = Lens.lens (\UpdateNotebookInstanceLifecycleConfig' {onCreate} -> onCreate) (\s@UpdateNotebookInstanceLifecycleConfig' {} a -> s {onCreate = a} :: UpdateNotebookInstanceLifecycleConfig) Prelude.. Lens.mapping Lens.coerced

-- | The shell script that runs every time you start a notebook instance,
-- including when you create the notebook instance. The shell script must
-- be a base64-encoded string.
updateNotebookInstanceLifecycleConfig_onStart :: Lens.Lens' UpdateNotebookInstanceLifecycleConfig (Prelude.Maybe [NotebookInstanceLifecycleHook])
updateNotebookInstanceLifecycleConfig_onStart = Lens.lens (\UpdateNotebookInstanceLifecycleConfig' {onStart} -> onStart) (\s@UpdateNotebookInstanceLifecycleConfig' {} a -> s {onStart = a} :: UpdateNotebookInstanceLifecycleConfig) Prelude.. Lens.mapping Lens.coerced

-- | The name of the lifecycle configuration.
updateNotebookInstanceLifecycleConfig_notebookInstanceLifecycleConfigName :: Lens.Lens' UpdateNotebookInstanceLifecycleConfig Prelude.Text
updateNotebookInstanceLifecycleConfig_notebookInstanceLifecycleConfigName = Lens.lens (\UpdateNotebookInstanceLifecycleConfig' {notebookInstanceLifecycleConfigName} -> notebookInstanceLifecycleConfigName) (\s@UpdateNotebookInstanceLifecycleConfig' {} a -> s {notebookInstanceLifecycleConfigName = a} :: UpdateNotebookInstanceLifecycleConfig)

instance
  Core.AWSRequest
    UpdateNotebookInstanceLifecycleConfig
  where
  type
    AWSResponse
      UpdateNotebookInstanceLifecycleConfig =
      UpdateNotebookInstanceLifecycleConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateNotebookInstanceLifecycleConfigResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateNotebookInstanceLifecycleConfig
  where
  hashWithSalt
    _salt
    UpdateNotebookInstanceLifecycleConfig' {..} =
      _salt `Prelude.hashWithSalt` onCreate
        `Prelude.hashWithSalt` onStart
        `Prelude.hashWithSalt` notebookInstanceLifecycleConfigName

instance
  Prelude.NFData
    UpdateNotebookInstanceLifecycleConfig
  where
  rnf UpdateNotebookInstanceLifecycleConfig' {..} =
    Prelude.rnf onCreate
      `Prelude.seq` Prelude.rnf onStart
      `Prelude.seq` Prelude.rnf notebookInstanceLifecycleConfigName

instance
  Data.ToHeaders
    UpdateNotebookInstanceLifecycleConfig
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.UpdateNotebookInstanceLifecycleConfig" ::
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
    UpdateNotebookInstanceLifecycleConfig
  where
  toJSON UpdateNotebookInstanceLifecycleConfig' {..} =
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
    UpdateNotebookInstanceLifecycleConfig
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    UpdateNotebookInstanceLifecycleConfig
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateNotebookInstanceLifecycleConfigResponse' smart constructor.
data UpdateNotebookInstanceLifecycleConfigResponse = UpdateNotebookInstanceLifecycleConfigResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateNotebookInstanceLifecycleConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateNotebookInstanceLifecycleConfigResponse_httpStatus' - The response's http status code.
newUpdateNotebookInstanceLifecycleConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateNotebookInstanceLifecycleConfigResponse
newUpdateNotebookInstanceLifecycleConfigResponse
  pHttpStatus_ =
    UpdateNotebookInstanceLifecycleConfigResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateNotebookInstanceLifecycleConfigResponse_httpStatus :: Lens.Lens' UpdateNotebookInstanceLifecycleConfigResponse Prelude.Int
updateNotebookInstanceLifecycleConfigResponse_httpStatus = Lens.lens (\UpdateNotebookInstanceLifecycleConfigResponse' {httpStatus} -> httpStatus) (\s@UpdateNotebookInstanceLifecycleConfigResponse' {} a -> s {httpStatus = a} :: UpdateNotebookInstanceLifecycleConfigResponse)

instance
  Prelude.NFData
    UpdateNotebookInstanceLifecycleConfigResponse
  where
  rnf
    UpdateNotebookInstanceLifecycleConfigResponse' {..} =
      Prelude.rnf httpStatus

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
-- Module      : Network.AWS.SageMaker.UpdateNotebookInstanceLifecycleConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a notebook instance lifecycle configuration created with the
-- CreateNotebookInstanceLifecycleConfig API.
module Network.AWS.SageMaker.UpdateNotebookInstanceLifecycleConfig
  ( -- * Creating a Request
    UpdateNotebookInstanceLifecycleConfig (..),
    newUpdateNotebookInstanceLifecycleConfig,

    -- * Request Lenses
    updateNotebookInstanceLifecycleConfig_onStart,
    updateNotebookInstanceLifecycleConfig_onCreate,
    updateNotebookInstanceLifecycleConfig_notebookInstanceLifecycleConfigName,

    -- * Destructuring the Response
    UpdateNotebookInstanceLifecycleConfigResponse (..),
    newUpdateNotebookInstanceLifecycleConfigResponse,

    -- * Response Lenses
    updateNotebookInstanceLifecycleConfigResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newUpdateNotebookInstanceLifecycleConfig' smart constructor.
data UpdateNotebookInstanceLifecycleConfig = UpdateNotebookInstanceLifecycleConfig'
  { -- | The shell script that runs every time you start a notebook instance,
    -- including when you create the notebook instance. The shell script must
    -- be a base64-encoded string.
    onStart :: Core.Maybe [NotebookInstanceLifecycleHook],
    -- | The shell script that runs only once, when you create a notebook
    -- instance. The shell script must be a base64-encoded string.
    onCreate :: Core.Maybe [NotebookInstanceLifecycleHook],
    -- | The name of the lifecycle configuration.
    notebookInstanceLifecycleConfigName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateNotebookInstanceLifecycleConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'onStart', 'updateNotebookInstanceLifecycleConfig_onStart' - The shell script that runs every time you start a notebook instance,
-- including when you create the notebook instance. The shell script must
-- be a base64-encoded string.
--
-- 'onCreate', 'updateNotebookInstanceLifecycleConfig_onCreate' - The shell script that runs only once, when you create a notebook
-- instance. The shell script must be a base64-encoded string.
--
-- 'notebookInstanceLifecycleConfigName', 'updateNotebookInstanceLifecycleConfig_notebookInstanceLifecycleConfigName' - The name of the lifecycle configuration.
newUpdateNotebookInstanceLifecycleConfig ::
  -- | 'notebookInstanceLifecycleConfigName'
  Core.Text ->
  UpdateNotebookInstanceLifecycleConfig
newUpdateNotebookInstanceLifecycleConfig
  pNotebookInstanceLifecycleConfigName_ =
    UpdateNotebookInstanceLifecycleConfig'
      { onStart =
          Core.Nothing,
        onCreate = Core.Nothing,
        notebookInstanceLifecycleConfigName =
          pNotebookInstanceLifecycleConfigName_
      }

-- | The shell script that runs every time you start a notebook instance,
-- including when you create the notebook instance. The shell script must
-- be a base64-encoded string.
updateNotebookInstanceLifecycleConfig_onStart :: Lens.Lens' UpdateNotebookInstanceLifecycleConfig (Core.Maybe [NotebookInstanceLifecycleHook])
updateNotebookInstanceLifecycleConfig_onStart = Lens.lens (\UpdateNotebookInstanceLifecycleConfig' {onStart} -> onStart) (\s@UpdateNotebookInstanceLifecycleConfig' {} a -> s {onStart = a} :: UpdateNotebookInstanceLifecycleConfig) Core.. Lens.mapping Lens._Coerce

-- | The shell script that runs only once, when you create a notebook
-- instance. The shell script must be a base64-encoded string.
updateNotebookInstanceLifecycleConfig_onCreate :: Lens.Lens' UpdateNotebookInstanceLifecycleConfig (Core.Maybe [NotebookInstanceLifecycleHook])
updateNotebookInstanceLifecycleConfig_onCreate = Lens.lens (\UpdateNotebookInstanceLifecycleConfig' {onCreate} -> onCreate) (\s@UpdateNotebookInstanceLifecycleConfig' {} a -> s {onCreate = a} :: UpdateNotebookInstanceLifecycleConfig) Core.. Lens.mapping Lens._Coerce

-- | The name of the lifecycle configuration.
updateNotebookInstanceLifecycleConfig_notebookInstanceLifecycleConfigName :: Lens.Lens' UpdateNotebookInstanceLifecycleConfig Core.Text
updateNotebookInstanceLifecycleConfig_notebookInstanceLifecycleConfigName = Lens.lens (\UpdateNotebookInstanceLifecycleConfig' {notebookInstanceLifecycleConfigName} -> notebookInstanceLifecycleConfigName) (\s@UpdateNotebookInstanceLifecycleConfig' {} a -> s {notebookInstanceLifecycleConfigName = a} :: UpdateNotebookInstanceLifecycleConfig)

instance
  Core.AWSRequest
    UpdateNotebookInstanceLifecycleConfig
  where
  type
    AWSResponse
      UpdateNotebookInstanceLifecycleConfig =
      UpdateNotebookInstanceLifecycleConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateNotebookInstanceLifecycleConfigResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    UpdateNotebookInstanceLifecycleConfig

instance
  Core.NFData
    UpdateNotebookInstanceLifecycleConfig

instance
  Core.ToHeaders
    UpdateNotebookInstanceLifecycleConfig
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.UpdateNotebookInstanceLifecycleConfig" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    UpdateNotebookInstanceLifecycleConfig
  where
  toJSON UpdateNotebookInstanceLifecycleConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("OnStart" Core..=) Core.<$> onStart,
            ("OnCreate" Core..=) Core.<$> onCreate,
            Core.Just
              ( "NotebookInstanceLifecycleConfigName"
                  Core..= notebookInstanceLifecycleConfigName
              )
          ]
      )

instance
  Core.ToPath
    UpdateNotebookInstanceLifecycleConfig
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    UpdateNotebookInstanceLifecycleConfig
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateNotebookInstanceLifecycleConfigResponse' smart constructor.
data UpdateNotebookInstanceLifecycleConfigResponse = UpdateNotebookInstanceLifecycleConfigResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateNotebookInstanceLifecycleConfigResponse
newUpdateNotebookInstanceLifecycleConfigResponse
  pHttpStatus_ =
    UpdateNotebookInstanceLifecycleConfigResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateNotebookInstanceLifecycleConfigResponse_httpStatus :: Lens.Lens' UpdateNotebookInstanceLifecycleConfigResponse Core.Int
updateNotebookInstanceLifecycleConfigResponse_httpStatus = Lens.lens (\UpdateNotebookInstanceLifecycleConfigResponse' {httpStatus} -> httpStatus) (\s@UpdateNotebookInstanceLifecycleConfigResponse' {} a -> s {httpStatus = a} :: UpdateNotebookInstanceLifecycleConfigResponse)

instance
  Core.NFData
    UpdateNotebookInstanceLifecycleConfigResponse

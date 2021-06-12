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
-- Module      : Network.AWS.SageMaker.UpdatePipelineExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a pipeline execution.
module Network.AWS.SageMaker.UpdatePipelineExecution
  ( -- * Creating a Request
    UpdatePipelineExecution (..),
    newUpdatePipelineExecution,

    -- * Request Lenses
    updatePipelineExecution_pipelineExecutionDescription,
    updatePipelineExecution_pipelineExecutionDisplayName,
    updatePipelineExecution_pipelineExecutionArn,

    -- * Destructuring the Response
    UpdatePipelineExecutionResponse (..),
    newUpdatePipelineExecutionResponse,

    -- * Response Lenses
    updatePipelineExecutionResponse_pipelineExecutionArn,
    updatePipelineExecutionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newUpdatePipelineExecution' smart constructor.
data UpdatePipelineExecution = UpdatePipelineExecution'
  { -- | The description of the pipeline execution.
    pipelineExecutionDescription :: Core.Maybe Core.Text,
    -- | The display name of the pipeline execution.
    pipelineExecutionDisplayName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the pipeline execution.
    pipelineExecutionArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdatePipelineExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineExecutionDescription', 'updatePipelineExecution_pipelineExecutionDescription' - The description of the pipeline execution.
--
-- 'pipelineExecutionDisplayName', 'updatePipelineExecution_pipelineExecutionDisplayName' - The display name of the pipeline execution.
--
-- 'pipelineExecutionArn', 'updatePipelineExecution_pipelineExecutionArn' - The Amazon Resource Name (ARN) of the pipeline execution.
newUpdatePipelineExecution ::
  -- | 'pipelineExecutionArn'
  Core.Text ->
  UpdatePipelineExecution
newUpdatePipelineExecution pPipelineExecutionArn_ =
  UpdatePipelineExecution'
    { pipelineExecutionDescription =
        Core.Nothing,
      pipelineExecutionDisplayName = Core.Nothing,
      pipelineExecutionArn = pPipelineExecutionArn_
    }

-- | The description of the pipeline execution.
updatePipelineExecution_pipelineExecutionDescription :: Lens.Lens' UpdatePipelineExecution (Core.Maybe Core.Text)
updatePipelineExecution_pipelineExecutionDescription = Lens.lens (\UpdatePipelineExecution' {pipelineExecutionDescription} -> pipelineExecutionDescription) (\s@UpdatePipelineExecution' {} a -> s {pipelineExecutionDescription = a} :: UpdatePipelineExecution)

-- | The display name of the pipeline execution.
updatePipelineExecution_pipelineExecutionDisplayName :: Lens.Lens' UpdatePipelineExecution (Core.Maybe Core.Text)
updatePipelineExecution_pipelineExecutionDisplayName = Lens.lens (\UpdatePipelineExecution' {pipelineExecutionDisplayName} -> pipelineExecutionDisplayName) (\s@UpdatePipelineExecution' {} a -> s {pipelineExecutionDisplayName = a} :: UpdatePipelineExecution)

-- | The Amazon Resource Name (ARN) of the pipeline execution.
updatePipelineExecution_pipelineExecutionArn :: Lens.Lens' UpdatePipelineExecution Core.Text
updatePipelineExecution_pipelineExecutionArn = Lens.lens (\UpdatePipelineExecution' {pipelineExecutionArn} -> pipelineExecutionArn) (\s@UpdatePipelineExecution' {} a -> s {pipelineExecutionArn = a} :: UpdatePipelineExecution)

instance Core.AWSRequest UpdatePipelineExecution where
  type
    AWSResponse UpdatePipelineExecution =
      UpdatePipelineExecutionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePipelineExecutionResponse'
            Core.<$> (x Core..?> "PipelineExecutionArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdatePipelineExecution

instance Core.NFData UpdatePipelineExecution

instance Core.ToHeaders UpdatePipelineExecution where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.UpdatePipelineExecution" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdatePipelineExecution where
  toJSON UpdatePipelineExecution' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PipelineExecutionDescription" Core..=)
              Core.<$> pipelineExecutionDescription,
            ("PipelineExecutionDisplayName" Core..=)
              Core.<$> pipelineExecutionDisplayName,
            Core.Just
              ( "PipelineExecutionArn"
                  Core..= pipelineExecutionArn
              )
          ]
      )

instance Core.ToPath UpdatePipelineExecution where
  toPath = Core.const "/"

instance Core.ToQuery UpdatePipelineExecution where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdatePipelineExecutionResponse' smart constructor.
data UpdatePipelineExecutionResponse = UpdatePipelineExecutionResponse'
  { -- | The Amazon Resource Name (ARN) of the updated pipeline execution.
    pipelineExecutionArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdatePipelineExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineExecutionArn', 'updatePipelineExecutionResponse_pipelineExecutionArn' - The Amazon Resource Name (ARN) of the updated pipeline execution.
--
-- 'httpStatus', 'updatePipelineExecutionResponse_httpStatus' - The response's http status code.
newUpdatePipelineExecutionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdatePipelineExecutionResponse
newUpdatePipelineExecutionResponse pHttpStatus_ =
  UpdatePipelineExecutionResponse'
    { pipelineExecutionArn =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the updated pipeline execution.
updatePipelineExecutionResponse_pipelineExecutionArn :: Lens.Lens' UpdatePipelineExecutionResponse (Core.Maybe Core.Text)
updatePipelineExecutionResponse_pipelineExecutionArn = Lens.lens (\UpdatePipelineExecutionResponse' {pipelineExecutionArn} -> pipelineExecutionArn) (\s@UpdatePipelineExecutionResponse' {} a -> s {pipelineExecutionArn = a} :: UpdatePipelineExecutionResponse)

-- | The response's http status code.
updatePipelineExecutionResponse_httpStatus :: Lens.Lens' UpdatePipelineExecutionResponse Core.Int
updatePipelineExecutionResponse_httpStatus = Lens.lens (\UpdatePipelineExecutionResponse' {httpStatus} -> httpStatus) (\s@UpdatePipelineExecutionResponse' {} a -> s {httpStatus = a} :: UpdatePipelineExecutionResponse)

instance Core.NFData UpdatePipelineExecutionResponse

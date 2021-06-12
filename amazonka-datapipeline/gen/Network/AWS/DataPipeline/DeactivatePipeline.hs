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
-- Module      : Network.AWS.DataPipeline.DeactivatePipeline
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deactivates the specified running pipeline. The pipeline is set to the
-- @DEACTIVATING@ state until the deactivation process completes.
--
-- To resume a deactivated pipeline, use ActivatePipeline. By default, the
-- pipeline resumes from the last completed execution. Optionally, you can
-- specify the date and time to resume the pipeline.
module Network.AWS.DataPipeline.DeactivatePipeline
  ( -- * Creating a Request
    DeactivatePipeline (..),
    newDeactivatePipeline,

    -- * Request Lenses
    deactivatePipeline_cancelActive,
    deactivatePipeline_pipelineId,

    -- * Destructuring the Response
    DeactivatePipelineResponse (..),
    newDeactivatePipelineResponse,

    -- * Response Lenses
    deactivatePipelineResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DataPipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DeactivatePipeline.
--
-- /See:/ 'newDeactivatePipeline' smart constructor.
data DeactivatePipeline = DeactivatePipeline'
  { -- | Indicates whether to cancel any running objects. The default is true,
    -- which sets the state of any running objects to @CANCELED@. If this value
    -- is false, the pipeline is deactivated after all running objects finish.
    cancelActive :: Core.Maybe Core.Bool,
    -- | The ID of the pipeline.
    pipelineId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeactivatePipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cancelActive', 'deactivatePipeline_cancelActive' - Indicates whether to cancel any running objects. The default is true,
-- which sets the state of any running objects to @CANCELED@. If this value
-- is false, the pipeline is deactivated after all running objects finish.
--
-- 'pipelineId', 'deactivatePipeline_pipelineId' - The ID of the pipeline.
newDeactivatePipeline ::
  -- | 'pipelineId'
  Core.Text ->
  DeactivatePipeline
newDeactivatePipeline pPipelineId_ =
  DeactivatePipeline'
    { cancelActive = Core.Nothing,
      pipelineId = pPipelineId_
    }

-- | Indicates whether to cancel any running objects. The default is true,
-- which sets the state of any running objects to @CANCELED@. If this value
-- is false, the pipeline is deactivated after all running objects finish.
deactivatePipeline_cancelActive :: Lens.Lens' DeactivatePipeline (Core.Maybe Core.Bool)
deactivatePipeline_cancelActive = Lens.lens (\DeactivatePipeline' {cancelActive} -> cancelActive) (\s@DeactivatePipeline' {} a -> s {cancelActive = a} :: DeactivatePipeline)

-- | The ID of the pipeline.
deactivatePipeline_pipelineId :: Lens.Lens' DeactivatePipeline Core.Text
deactivatePipeline_pipelineId = Lens.lens (\DeactivatePipeline' {pipelineId} -> pipelineId) (\s@DeactivatePipeline' {} a -> s {pipelineId = a} :: DeactivatePipeline)

instance Core.AWSRequest DeactivatePipeline where
  type
    AWSResponse DeactivatePipeline =
      DeactivatePipelineResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeactivatePipelineResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeactivatePipeline

instance Core.NFData DeactivatePipeline

instance Core.ToHeaders DeactivatePipeline where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DataPipeline.DeactivatePipeline" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeactivatePipeline where
  toJSON DeactivatePipeline' {..} =
    Core.object
      ( Core.catMaybes
          [ ("cancelActive" Core..=) Core.<$> cancelActive,
            Core.Just ("pipelineId" Core..= pipelineId)
          ]
      )

instance Core.ToPath DeactivatePipeline where
  toPath = Core.const "/"

instance Core.ToQuery DeactivatePipeline where
  toQuery = Core.const Core.mempty

-- | Contains the output of DeactivatePipeline.
--
-- /See:/ 'newDeactivatePipelineResponse' smart constructor.
data DeactivatePipelineResponse = DeactivatePipelineResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeactivatePipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deactivatePipelineResponse_httpStatus' - The response's http status code.
newDeactivatePipelineResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeactivatePipelineResponse
newDeactivatePipelineResponse pHttpStatus_ =
  DeactivatePipelineResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deactivatePipelineResponse_httpStatus :: Lens.Lens' DeactivatePipelineResponse Core.Int
deactivatePipelineResponse_httpStatus = Lens.lens (\DeactivatePipelineResponse' {httpStatus} -> httpStatus) (\s@DeactivatePipelineResponse' {} a -> s {httpStatus = a} :: DeactivatePipelineResponse)

instance Core.NFData DeactivatePipelineResponse

{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.DataPipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DeactivatePipeline.
--
-- /See:/ 'newDeactivatePipeline' smart constructor.
data DeactivatePipeline = DeactivatePipeline'
  { -- | Indicates whether to cancel any running objects. The default is true,
    -- which sets the state of any running objects to @CANCELED@. If this value
    -- is false, the pipeline is deactivated after all running objects finish.
    cancelActive :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the pipeline.
    pipelineId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeactivatePipeline
newDeactivatePipeline pPipelineId_ =
  DeactivatePipeline'
    { cancelActive = Prelude.Nothing,
      pipelineId = pPipelineId_
    }

-- | Indicates whether to cancel any running objects. The default is true,
-- which sets the state of any running objects to @CANCELED@. If this value
-- is false, the pipeline is deactivated after all running objects finish.
deactivatePipeline_cancelActive :: Lens.Lens' DeactivatePipeline (Prelude.Maybe Prelude.Bool)
deactivatePipeline_cancelActive = Lens.lens (\DeactivatePipeline' {cancelActive} -> cancelActive) (\s@DeactivatePipeline' {} a -> s {cancelActive = a} :: DeactivatePipeline)

-- | The ID of the pipeline.
deactivatePipeline_pipelineId :: Lens.Lens' DeactivatePipeline Prelude.Text
deactivatePipeline_pipelineId = Lens.lens (\DeactivatePipeline' {pipelineId} -> pipelineId) (\s@DeactivatePipeline' {} a -> s {pipelineId = a} :: DeactivatePipeline)

instance Prelude.AWSRequest DeactivatePipeline where
  type
    Rs DeactivatePipeline =
      DeactivatePipelineResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeactivatePipelineResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeactivatePipeline

instance Prelude.NFData DeactivatePipeline

instance Prelude.ToHeaders DeactivatePipeline where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DataPipeline.DeactivatePipeline" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeactivatePipeline where
  toJSON DeactivatePipeline' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("cancelActive" Prelude..=)
              Prelude.<$> cancelActive,
            Prelude.Just ("pipelineId" Prelude..= pipelineId)
          ]
      )

instance Prelude.ToPath DeactivatePipeline where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeactivatePipeline where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the output of DeactivatePipeline.
--
-- /See:/ 'newDeactivatePipelineResponse' smart constructor.
data DeactivatePipelineResponse = DeactivatePipelineResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeactivatePipelineResponse
newDeactivatePipelineResponse pHttpStatus_ =
  DeactivatePipelineResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deactivatePipelineResponse_httpStatus :: Lens.Lens' DeactivatePipelineResponse Prelude.Int
deactivatePipelineResponse_httpStatus = Lens.lens (\DeactivatePipelineResponse' {httpStatus} -> httpStatus) (\s@DeactivatePipelineResponse' {} a -> s {httpStatus = a} :: DeactivatePipelineResponse)

instance Prelude.NFData DeactivatePipelineResponse

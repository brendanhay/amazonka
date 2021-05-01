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
-- Module      : Network.AWS.CodePipeline.StartPipelineExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the specified pipeline. Specifically, it begins processing the
-- latest commit to the source location specified as part of the pipeline.
module Network.AWS.CodePipeline.StartPipelineExecution
  ( -- * Creating a Request
    StartPipelineExecution (..),
    newStartPipelineExecution,

    -- * Request Lenses
    startPipelineExecution_clientRequestToken,
    startPipelineExecution_name,

    -- * Destructuring the Response
    StartPipelineExecutionResponse (..),
    newStartPipelineExecutionResponse,

    -- * Response Lenses
    startPipelineExecutionResponse_pipelineExecutionId,
    startPipelineExecutionResponse_httpStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @StartPipelineExecution@ action.
--
-- /See:/ 'newStartPipelineExecution' smart constructor.
data StartPipelineExecution = StartPipelineExecution'
  { -- | The system-generated unique ID used to identify a unique execution
    -- request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the pipeline to start.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartPipelineExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'startPipelineExecution_clientRequestToken' - The system-generated unique ID used to identify a unique execution
-- request.
--
-- 'name', 'startPipelineExecution_name' - The name of the pipeline to start.
newStartPipelineExecution ::
  -- | 'name'
  Prelude.Text ->
  StartPipelineExecution
newStartPipelineExecution pName_ =
  StartPipelineExecution'
    { clientRequestToken =
        Prelude.Nothing,
      name = pName_
    }

-- | The system-generated unique ID used to identify a unique execution
-- request.
startPipelineExecution_clientRequestToken :: Lens.Lens' StartPipelineExecution (Prelude.Maybe Prelude.Text)
startPipelineExecution_clientRequestToken = Lens.lens (\StartPipelineExecution' {clientRequestToken} -> clientRequestToken) (\s@StartPipelineExecution' {} a -> s {clientRequestToken = a} :: StartPipelineExecution)

-- | The name of the pipeline to start.
startPipelineExecution_name :: Lens.Lens' StartPipelineExecution Prelude.Text
startPipelineExecution_name = Lens.lens (\StartPipelineExecution' {name} -> name) (\s@StartPipelineExecution' {} a -> s {name = a} :: StartPipelineExecution)

instance Prelude.AWSRequest StartPipelineExecution where
  type
    Rs StartPipelineExecution =
      StartPipelineExecutionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartPipelineExecutionResponse'
            Prelude.<$> (x Prelude..?> "pipelineExecutionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartPipelineExecution

instance Prelude.NFData StartPipelineExecution

instance Prelude.ToHeaders StartPipelineExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodePipeline_20150709.StartPipelineExecution" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StartPipelineExecution where
  toJSON StartPipelineExecution' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("clientRequestToken" Prelude..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just ("name" Prelude..= name)
          ]
      )

instance Prelude.ToPath StartPipelineExecution where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StartPipelineExecution where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @StartPipelineExecution@ action.
--
-- /See:/ 'newStartPipelineExecutionResponse' smart constructor.
data StartPipelineExecutionResponse = StartPipelineExecutionResponse'
  { -- | The unique system-generated ID of the pipeline execution that was
    -- started.
    pipelineExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartPipelineExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineExecutionId', 'startPipelineExecutionResponse_pipelineExecutionId' - The unique system-generated ID of the pipeline execution that was
-- started.
--
-- 'httpStatus', 'startPipelineExecutionResponse_httpStatus' - The response's http status code.
newStartPipelineExecutionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartPipelineExecutionResponse
newStartPipelineExecutionResponse pHttpStatus_ =
  StartPipelineExecutionResponse'
    { pipelineExecutionId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique system-generated ID of the pipeline execution that was
-- started.
startPipelineExecutionResponse_pipelineExecutionId :: Lens.Lens' StartPipelineExecutionResponse (Prelude.Maybe Prelude.Text)
startPipelineExecutionResponse_pipelineExecutionId = Lens.lens (\StartPipelineExecutionResponse' {pipelineExecutionId} -> pipelineExecutionId) (\s@StartPipelineExecutionResponse' {} a -> s {pipelineExecutionId = a} :: StartPipelineExecutionResponse)

-- | The response's http status code.
startPipelineExecutionResponse_httpStatus :: Lens.Lens' StartPipelineExecutionResponse Prelude.Int
startPipelineExecutionResponse_httpStatus = Lens.lens (\StartPipelineExecutionResponse' {httpStatus} -> httpStatus) (\s@StartPipelineExecutionResponse' {} a -> s {httpStatus = a} :: StartPipelineExecutionResponse)

instance
  Prelude.NFData
    StartPipelineExecutionResponse

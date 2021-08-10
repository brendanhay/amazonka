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
-- Module      : Network.AWS.IoTAnalytics.DescribePipeline
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a pipeline.
module Network.AWS.IoTAnalytics.DescribePipeline
  ( -- * Creating a Request
    DescribePipeline (..),
    newDescribePipeline,

    -- * Request Lenses
    describePipeline_pipelineName,

    -- * Destructuring the Response
    DescribePipelineResponse (..),
    newDescribePipelineResponse,

    -- * Response Lenses
    describePipelineResponse_pipeline,
    describePipelineResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribePipeline' smart constructor.
data DescribePipeline = DescribePipeline'
  { -- | The name of the pipeline whose information is retrieved.
    pipelineName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineName', 'describePipeline_pipelineName' - The name of the pipeline whose information is retrieved.
newDescribePipeline ::
  -- | 'pipelineName'
  Prelude.Text ->
  DescribePipeline
newDescribePipeline pPipelineName_ =
  DescribePipeline' {pipelineName = pPipelineName_}

-- | The name of the pipeline whose information is retrieved.
describePipeline_pipelineName :: Lens.Lens' DescribePipeline Prelude.Text
describePipeline_pipelineName = Lens.lens (\DescribePipeline' {pipelineName} -> pipelineName) (\s@DescribePipeline' {} a -> s {pipelineName = a} :: DescribePipeline)

instance Core.AWSRequest DescribePipeline where
  type
    AWSResponse DescribePipeline =
      DescribePipelineResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePipelineResponse'
            Prelude.<$> (x Core..?> "pipeline")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePipeline

instance Prelude.NFData DescribePipeline

instance Core.ToHeaders DescribePipeline where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribePipeline where
  toPath DescribePipeline' {..} =
    Prelude.mconcat
      ["/pipelines/", Core.toBS pipelineName]

instance Core.ToQuery DescribePipeline where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribePipelineResponse' smart constructor.
data DescribePipelineResponse = DescribePipelineResponse'
  { -- | A @Pipeline@ object that contains information about the pipeline.
    pipeline :: Prelude.Maybe Pipeline,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipeline', 'describePipelineResponse_pipeline' - A @Pipeline@ object that contains information about the pipeline.
--
-- 'httpStatus', 'describePipelineResponse_httpStatus' - The response's http status code.
newDescribePipelineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePipelineResponse
newDescribePipelineResponse pHttpStatus_ =
  DescribePipelineResponse'
    { pipeline =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A @Pipeline@ object that contains information about the pipeline.
describePipelineResponse_pipeline :: Lens.Lens' DescribePipelineResponse (Prelude.Maybe Pipeline)
describePipelineResponse_pipeline = Lens.lens (\DescribePipelineResponse' {pipeline} -> pipeline) (\s@DescribePipelineResponse' {} a -> s {pipeline = a} :: DescribePipelineResponse)

-- | The response's http status code.
describePipelineResponse_httpStatus :: Lens.Lens' DescribePipelineResponse Prelude.Int
describePipelineResponse_httpStatus = Lens.lens (\DescribePipelineResponse' {httpStatus} -> httpStatus) (\s@DescribePipelineResponse' {} a -> s {httpStatus = a} :: DescribePipelineResponse)

instance Prelude.NFData DescribePipelineResponse

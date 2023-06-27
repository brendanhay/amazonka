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
-- Module      : Amazonka.OsIs.StopPipeline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an OpenSearch Ingestion pipeline. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/pipeline--stop-start.html#pipeline--stop Stopping an OpenSearch Ingestion pipeline>.
module Amazonka.OsIs.StopPipeline
  ( -- * Creating a Request
    StopPipeline (..),
    newStopPipeline,

    -- * Request Lenses
    stopPipeline_pipelineName,

    -- * Destructuring the Response
    StopPipelineResponse (..),
    newStopPipelineResponse,

    -- * Response Lenses
    stopPipelineResponse_pipeline,
    stopPipelineResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OsIs.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopPipeline' smart constructor.
data StopPipeline = StopPipeline'
  { -- | The name of the pipeline to stop.
    pipelineName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopPipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineName', 'stopPipeline_pipelineName' - The name of the pipeline to stop.
newStopPipeline ::
  -- | 'pipelineName'
  Prelude.Text ->
  StopPipeline
newStopPipeline pPipelineName_ =
  StopPipeline' {pipelineName = pPipelineName_}

-- | The name of the pipeline to stop.
stopPipeline_pipelineName :: Lens.Lens' StopPipeline Prelude.Text
stopPipeline_pipelineName = Lens.lens (\StopPipeline' {pipelineName} -> pipelineName) (\s@StopPipeline' {} a -> s {pipelineName = a} :: StopPipeline)

instance Core.AWSRequest StopPipeline where
  type AWSResponse StopPipeline = StopPipelineResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopPipelineResponse'
            Prelude.<$> (x Data..?> "Pipeline")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopPipeline where
  hashWithSalt _salt StopPipeline' {..} =
    _salt `Prelude.hashWithSalt` pipelineName

instance Prelude.NFData StopPipeline where
  rnf StopPipeline' {..} = Prelude.rnf pipelineName

instance Data.ToHeaders StopPipeline where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON StopPipeline where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath StopPipeline where
  toPath StopPipeline' {..} =
    Prelude.mconcat
      [ "/2022-01-01/osis/stopPipeline/",
        Data.toBS pipelineName
      ]

instance Data.ToQuery StopPipeline where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopPipelineResponse' smart constructor.
data StopPipelineResponse = StopPipelineResponse'
  { pipeline :: Prelude.Maybe Pipeline,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopPipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipeline', 'stopPipelineResponse_pipeline' - Undocumented member.
--
-- 'httpStatus', 'stopPipelineResponse_httpStatus' - The response's http status code.
newStopPipelineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopPipelineResponse
newStopPipelineResponse pHttpStatus_ =
  StopPipelineResponse'
    { pipeline = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
stopPipelineResponse_pipeline :: Lens.Lens' StopPipelineResponse (Prelude.Maybe Pipeline)
stopPipelineResponse_pipeline = Lens.lens (\StopPipelineResponse' {pipeline} -> pipeline) (\s@StopPipelineResponse' {} a -> s {pipeline = a} :: StopPipelineResponse)

-- | The response's http status code.
stopPipelineResponse_httpStatus :: Lens.Lens' StopPipelineResponse Prelude.Int
stopPipelineResponse_httpStatus = Lens.lens (\StopPipelineResponse' {httpStatus} -> httpStatus) (\s@StopPipelineResponse' {} a -> s {httpStatus = a} :: StopPipelineResponse)

instance Prelude.NFData StopPipelineResponse where
  rnf StopPipelineResponse' {..} =
    Prelude.rnf pipeline
      `Prelude.seq` Prelude.rnf httpStatus

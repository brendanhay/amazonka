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
-- Module      : Amazonka.OsIs.StartPipeline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an OpenSearch Ingestion pipeline. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/pipeline--stop-start.html#pipeline--start Starting an OpenSearch Ingestion pipeline>.
module Amazonka.OsIs.StartPipeline
  ( -- * Creating a Request
    StartPipeline (..),
    newStartPipeline,

    -- * Request Lenses
    startPipeline_pipelineName,

    -- * Destructuring the Response
    StartPipelineResponse (..),
    newStartPipelineResponse,

    -- * Response Lenses
    startPipelineResponse_pipeline,
    startPipelineResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OsIs.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartPipeline' smart constructor.
data StartPipeline = StartPipeline'
  { -- | The name of the pipeline to start.
    pipelineName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartPipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineName', 'startPipeline_pipelineName' - The name of the pipeline to start.
newStartPipeline ::
  -- | 'pipelineName'
  Prelude.Text ->
  StartPipeline
newStartPipeline pPipelineName_ =
  StartPipeline' {pipelineName = pPipelineName_}

-- | The name of the pipeline to start.
startPipeline_pipelineName :: Lens.Lens' StartPipeline Prelude.Text
startPipeline_pipelineName = Lens.lens (\StartPipeline' {pipelineName} -> pipelineName) (\s@StartPipeline' {} a -> s {pipelineName = a} :: StartPipeline)

instance Core.AWSRequest StartPipeline where
  type
    AWSResponse StartPipeline =
      StartPipelineResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartPipelineResponse'
            Prelude.<$> (x Data..?> "Pipeline")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartPipeline where
  hashWithSalt _salt StartPipeline' {..} =
    _salt `Prelude.hashWithSalt` pipelineName

instance Prelude.NFData StartPipeline where
  rnf StartPipeline' {..} = Prelude.rnf pipelineName

instance Data.ToHeaders StartPipeline where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON StartPipeline where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath StartPipeline where
  toPath StartPipeline' {..} =
    Prelude.mconcat
      [ "/2022-01-01/osis/startPipeline/",
        Data.toBS pipelineName
      ]

instance Data.ToQuery StartPipeline where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartPipelineResponse' smart constructor.
data StartPipelineResponse = StartPipelineResponse'
  { pipeline :: Prelude.Maybe Pipeline,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartPipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipeline', 'startPipelineResponse_pipeline' - Undocumented member.
--
-- 'httpStatus', 'startPipelineResponse_httpStatus' - The response's http status code.
newStartPipelineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartPipelineResponse
newStartPipelineResponse pHttpStatus_ =
  StartPipelineResponse'
    { pipeline = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
startPipelineResponse_pipeline :: Lens.Lens' StartPipelineResponse (Prelude.Maybe Pipeline)
startPipelineResponse_pipeline = Lens.lens (\StartPipelineResponse' {pipeline} -> pipeline) (\s@StartPipelineResponse' {} a -> s {pipeline = a} :: StartPipelineResponse)

-- | The response's http status code.
startPipelineResponse_httpStatus :: Lens.Lens' StartPipelineResponse Prelude.Int
startPipelineResponse_httpStatus = Lens.lens (\StartPipelineResponse' {httpStatus} -> httpStatus) (\s@StartPipelineResponse' {} a -> s {httpStatus = a} :: StartPipelineResponse)

instance Prelude.NFData StartPipelineResponse where
  rnf StartPipelineResponse' {..} =
    Prelude.rnf pipeline
      `Prelude.seq` Prelude.rnf httpStatus

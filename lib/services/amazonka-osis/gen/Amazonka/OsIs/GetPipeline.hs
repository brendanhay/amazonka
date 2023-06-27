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
-- Module      : Amazonka.OsIs.GetPipeline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about an OpenSearch Ingestion pipeline.
module Amazonka.OsIs.GetPipeline
  ( -- * Creating a Request
    GetPipeline (..),
    newGetPipeline,

    -- * Request Lenses
    getPipeline_pipelineName,

    -- * Destructuring the Response
    GetPipelineResponse (..),
    newGetPipelineResponse,

    -- * Response Lenses
    getPipelineResponse_pipeline,
    getPipelineResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OsIs.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPipeline' smart constructor.
data GetPipeline = GetPipeline'
  { -- | The name of the pipeline to get information about.
    pipelineName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineName', 'getPipeline_pipelineName' - The name of the pipeline to get information about.
newGetPipeline ::
  -- | 'pipelineName'
  Prelude.Text ->
  GetPipeline
newGetPipeline pPipelineName_ =
  GetPipeline' {pipelineName = pPipelineName_}

-- | The name of the pipeline to get information about.
getPipeline_pipelineName :: Lens.Lens' GetPipeline Prelude.Text
getPipeline_pipelineName = Lens.lens (\GetPipeline' {pipelineName} -> pipelineName) (\s@GetPipeline' {} a -> s {pipelineName = a} :: GetPipeline)

instance Core.AWSRequest GetPipeline where
  type AWSResponse GetPipeline = GetPipelineResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPipelineResponse'
            Prelude.<$> (x Data..?> "Pipeline")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPipeline where
  hashWithSalt _salt GetPipeline' {..} =
    _salt `Prelude.hashWithSalt` pipelineName

instance Prelude.NFData GetPipeline where
  rnf GetPipeline' {..} = Prelude.rnf pipelineName

instance Data.ToHeaders GetPipeline where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetPipeline where
  toPath GetPipeline' {..} =
    Prelude.mconcat
      [ "/2022-01-01/osis/getPipeline/",
        Data.toBS pipelineName
      ]

instance Data.ToQuery GetPipeline where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPipelineResponse' smart constructor.
data GetPipelineResponse = GetPipelineResponse'
  { -- | Detailed information about the requested pipeline.
    pipeline :: Prelude.Maybe Pipeline,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipeline', 'getPipelineResponse_pipeline' - Detailed information about the requested pipeline.
--
-- 'httpStatus', 'getPipelineResponse_httpStatus' - The response's http status code.
newGetPipelineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPipelineResponse
newGetPipelineResponse pHttpStatus_ =
  GetPipelineResponse'
    { pipeline = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Detailed information about the requested pipeline.
getPipelineResponse_pipeline :: Lens.Lens' GetPipelineResponse (Prelude.Maybe Pipeline)
getPipelineResponse_pipeline = Lens.lens (\GetPipelineResponse' {pipeline} -> pipeline) (\s@GetPipelineResponse' {} a -> s {pipeline = a} :: GetPipelineResponse)

-- | The response's http status code.
getPipelineResponse_httpStatus :: Lens.Lens' GetPipelineResponse Prelude.Int
getPipelineResponse_httpStatus = Lens.lens (\GetPipelineResponse' {httpStatus} -> httpStatus) (\s@GetPipelineResponse' {} a -> s {httpStatus = a} :: GetPipelineResponse)

instance Prelude.NFData GetPipelineResponse where
  rnf GetPipelineResponse' {..} =
    Prelude.rnf pipeline
      `Prelude.seq` Prelude.rnf httpStatus

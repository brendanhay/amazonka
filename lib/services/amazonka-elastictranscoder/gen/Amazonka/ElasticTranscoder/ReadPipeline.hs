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
-- Module      : Amazonka.ElasticTranscoder.ReadPipeline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The ReadPipeline operation gets detailed information about a pipeline.
module Amazonka.ElasticTranscoder.ReadPipeline
  ( -- * Creating a Request
    ReadPipeline (..),
    newReadPipeline,

    -- * Request Lenses
    readPipeline_id,

    -- * Destructuring the Response
    ReadPipelineResponse (..),
    newReadPipelineResponse,

    -- * Response Lenses
    readPipelineResponse_pipeline,
    readPipelineResponse_warnings,
    readPipelineResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticTranscoder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The @ReadPipelineRequest@ structure.
--
-- /See:/ 'newReadPipeline' smart constructor.
data ReadPipeline = ReadPipeline'
  { -- | The identifier of the pipeline to read.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReadPipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'readPipeline_id' - The identifier of the pipeline to read.
newReadPipeline ::
  -- | 'id'
  Prelude.Text ->
  ReadPipeline
newReadPipeline pId_ = ReadPipeline' {id = pId_}

-- | The identifier of the pipeline to read.
readPipeline_id :: Lens.Lens' ReadPipeline Prelude.Text
readPipeline_id = Lens.lens (\ReadPipeline' {id} -> id) (\s@ReadPipeline' {} a -> s {id = a} :: ReadPipeline)

instance Core.AWSRequest ReadPipeline where
  type AWSResponse ReadPipeline = ReadPipelineResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ReadPipelineResponse'
            Prelude.<$> (x Data..?> "Pipeline")
            Prelude.<*> (x Data..?> "Warnings" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ReadPipeline where
  hashWithSalt _salt ReadPipeline' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData ReadPipeline where
  rnf ReadPipeline' {..} = Prelude.rnf id

instance Data.ToHeaders ReadPipeline where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ReadPipeline where
  toPath ReadPipeline' {..} =
    Prelude.mconcat
      ["/2012-09-25/pipelines/", Data.toBS id]

instance Data.ToQuery ReadPipeline where
  toQuery = Prelude.const Prelude.mempty

-- | The @ReadPipelineResponse@ structure.
--
-- /See:/ 'newReadPipelineResponse' smart constructor.
data ReadPipelineResponse = ReadPipelineResponse'
  { -- | A section of the response body that provides information about the
    -- pipeline.
    pipeline :: Prelude.Maybe Pipeline,
    -- | Elastic Transcoder returns a warning if the resources used by your
    -- pipeline are not in the same region as the pipeline.
    --
    -- Using resources in the same region, such as your Amazon S3 buckets,
    -- Amazon SNS notification topics, and AWS KMS key, reduces processing time
    -- and prevents cross-regional charges.
    warnings :: Prelude.Maybe [Warning],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReadPipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipeline', 'readPipelineResponse_pipeline' - A section of the response body that provides information about the
-- pipeline.
--
-- 'warnings', 'readPipelineResponse_warnings' - Elastic Transcoder returns a warning if the resources used by your
-- pipeline are not in the same region as the pipeline.
--
-- Using resources in the same region, such as your Amazon S3 buckets,
-- Amazon SNS notification topics, and AWS KMS key, reduces processing time
-- and prevents cross-regional charges.
--
-- 'httpStatus', 'readPipelineResponse_httpStatus' - The response's http status code.
newReadPipelineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ReadPipelineResponse
newReadPipelineResponse pHttpStatus_ =
  ReadPipelineResponse'
    { pipeline = Prelude.Nothing,
      warnings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A section of the response body that provides information about the
-- pipeline.
readPipelineResponse_pipeline :: Lens.Lens' ReadPipelineResponse (Prelude.Maybe Pipeline)
readPipelineResponse_pipeline = Lens.lens (\ReadPipelineResponse' {pipeline} -> pipeline) (\s@ReadPipelineResponse' {} a -> s {pipeline = a} :: ReadPipelineResponse)

-- | Elastic Transcoder returns a warning if the resources used by your
-- pipeline are not in the same region as the pipeline.
--
-- Using resources in the same region, such as your Amazon S3 buckets,
-- Amazon SNS notification topics, and AWS KMS key, reduces processing time
-- and prevents cross-regional charges.
readPipelineResponse_warnings :: Lens.Lens' ReadPipelineResponse (Prelude.Maybe [Warning])
readPipelineResponse_warnings = Lens.lens (\ReadPipelineResponse' {warnings} -> warnings) (\s@ReadPipelineResponse' {} a -> s {warnings = a} :: ReadPipelineResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
readPipelineResponse_httpStatus :: Lens.Lens' ReadPipelineResponse Prelude.Int
readPipelineResponse_httpStatus = Lens.lens (\ReadPipelineResponse' {httpStatus} -> httpStatus) (\s@ReadPipelineResponse' {} a -> s {httpStatus = a} :: ReadPipelineResponse)

instance Prelude.NFData ReadPipelineResponse where
  rnf ReadPipelineResponse' {..} =
    Prelude.rnf pipeline
      `Prelude.seq` Prelude.rnf warnings
      `Prelude.seq` Prelude.rnf httpStatus

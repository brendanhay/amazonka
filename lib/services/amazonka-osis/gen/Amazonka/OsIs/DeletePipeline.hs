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
-- Module      : Amazonka.OsIs.DeletePipeline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an OpenSearch Ingestion pipeline. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/delete-pipeline.html Deleting Amazon OpenSearch Ingestion pipelines>.
module Amazonka.OsIs.DeletePipeline
  ( -- * Creating a Request
    DeletePipeline (..),
    newDeletePipeline,

    -- * Request Lenses
    deletePipeline_pipelineName,

    -- * Destructuring the Response
    DeletePipelineResponse (..),
    newDeletePipelineResponse,

    -- * Response Lenses
    deletePipelineResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OsIs.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeletePipeline' smart constructor.
data DeletePipeline = DeletePipeline'
  { -- | The name of the pipeline to delete.
    pipelineName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineName', 'deletePipeline_pipelineName' - The name of the pipeline to delete.
newDeletePipeline ::
  -- | 'pipelineName'
  Prelude.Text ->
  DeletePipeline
newDeletePipeline pPipelineName_ =
  DeletePipeline' {pipelineName = pPipelineName_}

-- | The name of the pipeline to delete.
deletePipeline_pipelineName :: Lens.Lens' DeletePipeline Prelude.Text
deletePipeline_pipelineName = Lens.lens (\DeletePipeline' {pipelineName} -> pipelineName) (\s@DeletePipeline' {} a -> s {pipelineName = a} :: DeletePipeline)

instance Core.AWSRequest DeletePipeline where
  type
    AWSResponse DeletePipeline =
      DeletePipelineResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeletePipelineResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePipeline where
  hashWithSalt _salt DeletePipeline' {..} =
    _salt `Prelude.hashWithSalt` pipelineName

instance Prelude.NFData DeletePipeline where
  rnf DeletePipeline' {..} = Prelude.rnf pipelineName

instance Data.ToHeaders DeletePipeline where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeletePipeline where
  toPath DeletePipeline' {..} =
    Prelude.mconcat
      [ "/2022-01-01/osis/deletePipeline/",
        Data.toBS pipelineName
      ]

instance Data.ToQuery DeletePipeline where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePipelineResponse' smart constructor.
data DeletePipelineResponse = DeletePipelineResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deletePipelineResponse_httpStatus' - The response's http status code.
newDeletePipelineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeletePipelineResponse
newDeletePipelineResponse pHttpStatus_ =
  DeletePipelineResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deletePipelineResponse_httpStatus :: Lens.Lens' DeletePipelineResponse Prelude.Int
deletePipelineResponse_httpStatus = Lens.lens (\DeletePipelineResponse' {httpStatus} -> httpStatus) (\s@DeletePipelineResponse' {} a -> s {httpStatus = a} :: DeletePipelineResponse)

instance Prelude.NFData DeletePipelineResponse where
  rnf DeletePipelineResponse' {..} =
    Prelude.rnf httpStatus

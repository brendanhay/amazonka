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
-- Module      : Amazonka.OsIs.GetPipelineChangeProgress
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns progress information for the current change happening on an
-- OpenSearch Ingestion pipeline. Currently, this operation only returns
-- information when a pipeline is being created.
--
-- For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/creating-pipeline.html#get-pipeline-progress Tracking the status of pipeline creation>.
module Amazonka.OsIs.GetPipelineChangeProgress
  ( -- * Creating a Request
    GetPipelineChangeProgress (..),
    newGetPipelineChangeProgress,

    -- * Request Lenses
    getPipelineChangeProgress_pipelineName,

    -- * Destructuring the Response
    GetPipelineChangeProgressResponse (..),
    newGetPipelineChangeProgressResponse,

    -- * Response Lenses
    getPipelineChangeProgressResponse_changeProgressStatuses,
    getPipelineChangeProgressResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OsIs.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPipelineChangeProgress' smart constructor.
data GetPipelineChangeProgress = GetPipelineChangeProgress'
  { -- | The name of the pipeline.
    pipelineName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPipelineChangeProgress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineName', 'getPipelineChangeProgress_pipelineName' - The name of the pipeline.
newGetPipelineChangeProgress ::
  -- | 'pipelineName'
  Prelude.Text ->
  GetPipelineChangeProgress
newGetPipelineChangeProgress pPipelineName_ =
  GetPipelineChangeProgress'
    { pipelineName =
        pPipelineName_
    }

-- | The name of the pipeline.
getPipelineChangeProgress_pipelineName :: Lens.Lens' GetPipelineChangeProgress Prelude.Text
getPipelineChangeProgress_pipelineName = Lens.lens (\GetPipelineChangeProgress' {pipelineName} -> pipelineName) (\s@GetPipelineChangeProgress' {} a -> s {pipelineName = a} :: GetPipelineChangeProgress)

instance Core.AWSRequest GetPipelineChangeProgress where
  type
    AWSResponse GetPipelineChangeProgress =
      GetPipelineChangeProgressResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPipelineChangeProgressResponse'
            Prelude.<$> ( x
                            Data..?> "ChangeProgressStatuses"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPipelineChangeProgress where
  hashWithSalt _salt GetPipelineChangeProgress' {..} =
    _salt `Prelude.hashWithSalt` pipelineName

instance Prelude.NFData GetPipelineChangeProgress where
  rnf GetPipelineChangeProgress' {..} =
    Prelude.rnf pipelineName

instance Data.ToHeaders GetPipelineChangeProgress where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetPipelineChangeProgress where
  toPath GetPipelineChangeProgress' {..} =
    Prelude.mconcat
      [ "/2022-01-01/osis/getPipelineChangeProgress/",
        Data.toBS pipelineName
      ]

instance Data.ToQuery GetPipelineChangeProgress where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPipelineChangeProgressResponse' smart constructor.
data GetPipelineChangeProgressResponse = GetPipelineChangeProgressResponse'
  { -- | The current status of the change happening on the pipeline.
    changeProgressStatuses :: Prelude.Maybe [ChangeProgressStatus],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPipelineChangeProgressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeProgressStatuses', 'getPipelineChangeProgressResponse_changeProgressStatuses' - The current status of the change happening on the pipeline.
--
-- 'httpStatus', 'getPipelineChangeProgressResponse_httpStatus' - The response's http status code.
newGetPipelineChangeProgressResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPipelineChangeProgressResponse
newGetPipelineChangeProgressResponse pHttpStatus_ =
  GetPipelineChangeProgressResponse'
    { changeProgressStatuses =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current status of the change happening on the pipeline.
getPipelineChangeProgressResponse_changeProgressStatuses :: Lens.Lens' GetPipelineChangeProgressResponse (Prelude.Maybe [ChangeProgressStatus])
getPipelineChangeProgressResponse_changeProgressStatuses = Lens.lens (\GetPipelineChangeProgressResponse' {changeProgressStatuses} -> changeProgressStatuses) (\s@GetPipelineChangeProgressResponse' {} a -> s {changeProgressStatuses = a} :: GetPipelineChangeProgressResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getPipelineChangeProgressResponse_httpStatus :: Lens.Lens' GetPipelineChangeProgressResponse Prelude.Int
getPipelineChangeProgressResponse_httpStatus = Lens.lens (\GetPipelineChangeProgressResponse' {httpStatus} -> httpStatus) (\s@GetPipelineChangeProgressResponse' {} a -> s {httpStatus = a} :: GetPipelineChangeProgressResponse)

instance
  Prelude.NFData
    GetPipelineChangeProgressResponse
  where
  rnf GetPipelineChangeProgressResponse' {..} =
    Prelude.rnf changeProgressStatuses
      `Prelude.seq` Prelude.rnf httpStatus

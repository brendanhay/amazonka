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
-- Module      : Amazonka.DataPipeline.DescribePipelines
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metadata about one or more pipelines. The information
-- retrieved includes the name of the pipeline, the pipeline identifier,
-- its current state, and the user account that owns the pipeline. Using
-- account credentials, you can retrieve metadata about pipelines that you
-- or your IAM users have created. If you are using an IAM user account,
-- you can retrieve metadata about only those pipelines for which you have
-- read permissions.
--
-- To retrieve the full pipeline definition instead of metadata about the
-- pipeline, call GetPipelineDefinition.
module Amazonka.DataPipeline.DescribePipelines
  ( -- * Creating a Request
    DescribePipelines (..),
    newDescribePipelines,

    -- * Request Lenses
    describePipelines_pipelineIds,

    -- * Destructuring the Response
    DescribePipelinesResponse (..),
    newDescribePipelinesResponse,

    -- * Response Lenses
    describePipelinesResponse_httpStatus,
    describePipelinesResponse_pipelineDescriptionList,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataPipeline.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for DescribePipelines.
--
-- /See:/ 'newDescribePipelines' smart constructor.
data DescribePipelines = DescribePipelines'
  { -- | The IDs of the pipelines to describe. You can pass as many as 25
    -- identifiers in a single call. To obtain pipeline IDs, call
    -- ListPipelines.
    pipelineIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePipelines' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineIds', 'describePipelines_pipelineIds' - The IDs of the pipelines to describe. You can pass as many as 25
-- identifiers in a single call. To obtain pipeline IDs, call
-- ListPipelines.
newDescribePipelines ::
  DescribePipelines
newDescribePipelines =
  DescribePipelines' {pipelineIds = Prelude.mempty}

-- | The IDs of the pipelines to describe. You can pass as many as 25
-- identifiers in a single call. To obtain pipeline IDs, call
-- ListPipelines.
describePipelines_pipelineIds :: Lens.Lens' DescribePipelines [Prelude.Text]
describePipelines_pipelineIds = Lens.lens (\DescribePipelines' {pipelineIds} -> pipelineIds) (\s@DescribePipelines' {} a -> s {pipelineIds = a} :: DescribePipelines) Prelude.. Lens.coerced

instance Core.AWSRequest DescribePipelines where
  type
    AWSResponse DescribePipelines =
      DescribePipelinesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePipelinesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "pipelineDescriptionList"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable DescribePipelines where
  hashWithSalt _salt DescribePipelines' {..} =
    _salt `Prelude.hashWithSalt` pipelineIds

instance Prelude.NFData DescribePipelines where
  rnf DescribePipelines' {..} = Prelude.rnf pipelineIds

instance Data.ToHeaders DescribePipelines where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DataPipeline.DescribePipelines" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribePipelines where
  toJSON DescribePipelines' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("pipelineIds" Data..= pipelineIds)]
      )

instance Data.ToPath DescribePipelines where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribePipelines where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the output of DescribePipelines.
--
-- /See:/ 'newDescribePipelinesResponse' smart constructor.
data DescribePipelinesResponse = DescribePipelinesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of descriptions for the specified pipelines.
    pipelineDescriptionList :: [PipelineDescription]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePipelinesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describePipelinesResponse_httpStatus' - The response's http status code.
--
-- 'pipelineDescriptionList', 'describePipelinesResponse_pipelineDescriptionList' - An array of descriptions for the specified pipelines.
newDescribePipelinesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePipelinesResponse
newDescribePipelinesResponse pHttpStatus_ =
  DescribePipelinesResponse'
    { httpStatus =
        pHttpStatus_,
      pipelineDescriptionList = Prelude.mempty
    }

-- | The response's http status code.
describePipelinesResponse_httpStatus :: Lens.Lens' DescribePipelinesResponse Prelude.Int
describePipelinesResponse_httpStatus = Lens.lens (\DescribePipelinesResponse' {httpStatus} -> httpStatus) (\s@DescribePipelinesResponse' {} a -> s {httpStatus = a} :: DescribePipelinesResponse)

-- | An array of descriptions for the specified pipelines.
describePipelinesResponse_pipelineDescriptionList :: Lens.Lens' DescribePipelinesResponse [PipelineDescription]
describePipelinesResponse_pipelineDescriptionList = Lens.lens (\DescribePipelinesResponse' {pipelineDescriptionList} -> pipelineDescriptionList) (\s@DescribePipelinesResponse' {} a -> s {pipelineDescriptionList = a} :: DescribePipelinesResponse) Prelude.. Lens.coerced

instance Prelude.NFData DescribePipelinesResponse where
  rnf DescribePipelinesResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf pipelineDescriptionList

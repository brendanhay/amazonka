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
-- Module      : Network.AWS.DataPipeline.DescribePipelines
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.DataPipeline.DescribePipelines
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

import qualified Network.AWS.Core as Core
import Network.AWS.DataPipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribePipelines.
--
-- /See:/ 'newDescribePipelines' smart constructor.
data DescribePipelines = DescribePipelines'
  { -- | The IDs of the pipelines to describe. You can pass as many as 25
    -- identifiers in a single call. To obtain pipeline IDs, call
    -- ListPipelines.
    pipelineIds :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  DescribePipelines' {pipelineIds = Core.mempty}

-- | The IDs of the pipelines to describe. You can pass as many as 25
-- identifiers in a single call. To obtain pipeline IDs, call
-- ListPipelines.
describePipelines_pipelineIds :: Lens.Lens' DescribePipelines [Core.Text]
describePipelines_pipelineIds = Lens.lens (\DescribePipelines' {pipelineIds} -> pipelineIds) (\s@DescribePipelines' {} a -> s {pipelineIds = a} :: DescribePipelines) Core.. Lens._Coerce

instance Core.AWSRequest DescribePipelines where
  type
    AWSResponse DescribePipelines =
      DescribePipelinesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePipelinesResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..?> "pipelineDescriptionList"
                         Core..!@ Core.mempty
                     )
      )

instance Core.Hashable DescribePipelines

instance Core.NFData DescribePipelines

instance Core.ToHeaders DescribePipelines where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DataPipeline.DescribePipelines" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribePipelines where
  toJSON DescribePipelines' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("pipelineIds" Core..= pipelineIds)]
      )

instance Core.ToPath DescribePipelines where
  toPath = Core.const "/"

instance Core.ToQuery DescribePipelines where
  toQuery = Core.const Core.mempty

-- | Contains the output of DescribePipelines.
--
-- /See:/ 'newDescribePipelinesResponse' smart constructor.
data DescribePipelinesResponse = DescribePipelinesResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | An array of descriptions for the specified pipelines.
    pipelineDescriptionList :: [PipelineDescription]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribePipelinesResponse
newDescribePipelinesResponse pHttpStatus_ =
  DescribePipelinesResponse'
    { httpStatus =
        pHttpStatus_,
      pipelineDescriptionList = Core.mempty
    }

-- | The response's http status code.
describePipelinesResponse_httpStatus :: Lens.Lens' DescribePipelinesResponse Core.Int
describePipelinesResponse_httpStatus = Lens.lens (\DescribePipelinesResponse' {httpStatus} -> httpStatus) (\s@DescribePipelinesResponse' {} a -> s {httpStatus = a} :: DescribePipelinesResponse)

-- | An array of descriptions for the specified pipelines.
describePipelinesResponse_pipelineDescriptionList :: Lens.Lens' DescribePipelinesResponse [PipelineDescription]
describePipelinesResponse_pipelineDescriptionList = Lens.lens (\DescribePipelinesResponse' {pipelineDescriptionList} -> pipelineDescriptionList) (\s@DescribePipelinesResponse' {} a -> s {pipelineDescriptionList = a} :: DescribePipelinesResponse) Core.. Lens._Coerce

instance Core.NFData DescribePipelinesResponse

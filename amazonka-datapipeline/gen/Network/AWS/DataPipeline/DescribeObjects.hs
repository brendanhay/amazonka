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
-- Module      : Network.AWS.DataPipeline.DescribeObjects
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the object definitions for a set of objects associated with the
-- pipeline. Object definitions are composed of a set of fields that define
-- the properties of the object.
--
-- This operation returns paginated results.
module Network.AWS.DataPipeline.DescribeObjects
  ( -- * Creating a Request
    DescribeObjects (..),
    newDescribeObjects,

    -- * Request Lenses
    describeObjects_marker,
    describeObjects_evaluateExpressions,
    describeObjects_pipelineId,
    describeObjects_objectIds,

    -- * Destructuring the Response
    DescribeObjectsResponse (..),
    newDescribeObjectsResponse,

    -- * Response Lenses
    describeObjectsResponse_hasMoreResults,
    describeObjectsResponse_marker,
    describeObjectsResponse_httpStatus,
    describeObjectsResponse_pipelineObjects,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DataPipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeObjects.
--
-- /See:/ 'newDescribeObjects' smart constructor.
data DescribeObjects = DescribeObjects'
  { -- | The starting point for the results to be returned. For the first call,
    -- this value should be empty. As long as there are more results, continue
    -- to call @DescribeObjects@ with the marker value from the previous call
    -- to retrieve the next set of results.
    marker :: Core.Maybe Core.Text,
    -- | Indicates whether any expressions in the object should be evaluated when
    -- the object descriptions are returned.
    evaluateExpressions :: Core.Maybe Core.Bool,
    -- | The ID of the pipeline that contains the object definitions.
    pipelineId :: Core.Text,
    -- | The IDs of the pipeline objects that contain the definitions to be
    -- described. You can pass as many as 25 identifiers in a single call to
    -- @DescribeObjects@.
    objectIds :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeObjects' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeObjects_marker' - The starting point for the results to be returned. For the first call,
-- this value should be empty. As long as there are more results, continue
-- to call @DescribeObjects@ with the marker value from the previous call
-- to retrieve the next set of results.
--
-- 'evaluateExpressions', 'describeObjects_evaluateExpressions' - Indicates whether any expressions in the object should be evaluated when
-- the object descriptions are returned.
--
-- 'pipelineId', 'describeObjects_pipelineId' - The ID of the pipeline that contains the object definitions.
--
-- 'objectIds', 'describeObjects_objectIds' - The IDs of the pipeline objects that contain the definitions to be
-- described. You can pass as many as 25 identifiers in a single call to
-- @DescribeObjects@.
newDescribeObjects ::
  -- | 'pipelineId'
  Core.Text ->
  DescribeObjects
newDescribeObjects pPipelineId_ =
  DescribeObjects'
    { marker = Core.Nothing,
      evaluateExpressions = Core.Nothing,
      pipelineId = pPipelineId_,
      objectIds = Core.mempty
    }

-- | The starting point for the results to be returned. For the first call,
-- this value should be empty. As long as there are more results, continue
-- to call @DescribeObjects@ with the marker value from the previous call
-- to retrieve the next set of results.
describeObjects_marker :: Lens.Lens' DescribeObjects (Core.Maybe Core.Text)
describeObjects_marker = Lens.lens (\DescribeObjects' {marker} -> marker) (\s@DescribeObjects' {} a -> s {marker = a} :: DescribeObjects)

-- | Indicates whether any expressions in the object should be evaluated when
-- the object descriptions are returned.
describeObjects_evaluateExpressions :: Lens.Lens' DescribeObjects (Core.Maybe Core.Bool)
describeObjects_evaluateExpressions = Lens.lens (\DescribeObjects' {evaluateExpressions} -> evaluateExpressions) (\s@DescribeObjects' {} a -> s {evaluateExpressions = a} :: DescribeObjects)

-- | The ID of the pipeline that contains the object definitions.
describeObjects_pipelineId :: Lens.Lens' DescribeObjects Core.Text
describeObjects_pipelineId = Lens.lens (\DescribeObjects' {pipelineId} -> pipelineId) (\s@DescribeObjects' {} a -> s {pipelineId = a} :: DescribeObjects)

-- | The IDs of the pipeline objects that contain the definitions to be
-- described. You can pass as many as 25 identifiers in a single call to
-- @DescribeObjects@.
describeObjects_objectIds :: Lens.Lens' DescribeObjects [Core.Text]
describeObjects_objectIds = Lens.lens (\DescribeObjects' {objectIds} -> objectIds) (\s@DescribeObjects' {} a -> s {objectIds = a} :: DescribeObjects) Core.. Lens._Coerce

instance Core.AWSPager DescribeObjects where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeObjectsResponse_hasMoreResults
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.isNothing
        ( rs
            Lens.^? describeObjectsResponse_marker Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeObjects_marker
          Lens..~ rs
          Lens.^? describeObjectsResponse_marker Core.. Lens._Just

instance Core.AWSRequest DescribeObjects where
  type
    AWSResponse DescribeObjects =
      DescribeObjectsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeObjectsResponse'
            Core.<$> (x Core..?> "hasMoreResults")
            Core.<*> (x Core..?> "marker")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "pipelineObjects" Core..!@ Core.mempty)
      )

instance Core.Hashable DescribeObjects

instance Core.NFData DescribeObjects

instance Core.ToHeaders DescribeObjects where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("DataPipeline.DescribeObjects" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeObjects where
  toJSON DescribeObjects' {..} =
    Core.object
      ( Core.catMaybes
          [ ("marker" Core..=) Core.<$> marker,
            ("evaluateExpressions" Core..=)
              Core.<$> evaluateExpressions,
            Core.Just ("pipelineId" Core..= pipelineId),
            Core.Just ("objectIds" Core..= objectIds)
          ]
      )

instance Core.ToPath DescribeObjects where
  toPath = Core.const "/"

instance Core.ToQuery DescribeObjects where
  toQuery = Core.const Core.mempty

-- | Contains the output of DescribeObjects.
--
-- /See:/ 'newDescribeObjectsResponse' smart constructor.
data DescribeObjectsResponse = DescribeObjectsResponse'
  { -- | Indicates whether there are more results to return.
    hasMoreResults :: Core.Maybe Core.Bool,
    -- | The starting point for the next page of results. To view the next page
    -- of results, call @DescribeObjects@ again with this marker value. If the
    -- value is null, there are no more results.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | An array of object definitions.
    pipelineObjects :: [PipelineObject]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeObjectsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hasMoreResults', 'describeObjectsResponse_hasMoreResults' - Indicates whether there are more results to return.
--
-- 'marker', 'describeObjectsResponse_marker' - The starting point for the next page of results. To view the next page
-- of results, call @DescribeObjects@ again with this marker value. If the
-- value is null, there are no more results.
--
-- 'httpStatus', 'describeObjectsResponse_httpStatus' - The response's http status code.
--
-- 'pipelineObjects', 'describeObjectsResponse_pipelineObjects' - An array of object definitions.
newDescribeObjectsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeObjectsResponse
newDescribeObjectsResponse pHttpStatus_ =
  DescribeObjectsResponse'
    { hasMoreResults =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_,
      pipelineObjects = Core.mempty
    }

-- | Indicates whether there are more results to return.
describeObjectsResponse_hasMoreResults :: Lens.Lens' DescribeObjectsResponse (Core.Maybe Core.Bool)
describeObjectsResponse_hasMoreResults = Lens.lens (\DescribeObjectsResponse' {hasMoreResults} -> hasMoreResults) (\s@DescribeObjectsResponse' {} a -> s {hasMoreResults = a} :: DescribeObjectsResponse)

-- | The starting point for the next page of results. To view the next page
-- of results, call @DescribeObjects@ again with this marker value. If the
-- value is null, there are no more results.
describeObjectsResponse_marker :: Lens.Lens' DescribeObjectsResponse (Core.Maybe Core.Text)
describeObjectsResponse_marker = Lens.lens (\DescribeObjectsResponse' {marker} -> marker) (\s@DescribeObjectsResponse' {} a -> s {marker = a} :: DescribeObjectsResponse)

-- | The response's http status code.
describeObjectsResponse_httpStatus :: Lens.Lens' DescribeObjectsResponse Core.Int
describeObjectsResponse_httpStatus = Lens.lens (\DescribeObjectsResponse' {httpStatus} -> httpStatus) (\s@DescribeObjectsResponse' {} a -> s {httpStatus = a} :: DescribeObjectsResponse)

-- | An array of object definitions.
describeObjectsResponse_pipelineObjects :: Lens.Lens' DescribeObjectsResponse [PipelineObject]
describeObjectsResponse_pipelineObjects = Lens.lens (\DescribeObjectsResponse' {pipelineObjects} -> pipelineObjects) (\s@DescribeObjectsResponse' {} a -> s {pipelineObjects = a} :: DescribeObjectsResponse) Core.. Lens._Coerce

instance Core.NFData DescribeObjectsResponse

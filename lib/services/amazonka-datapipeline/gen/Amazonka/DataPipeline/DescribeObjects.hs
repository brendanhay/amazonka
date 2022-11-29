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
-- Module      : Amazonka.DataPipeline.DescribeObjects
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.DataPipeline.DescribeObjects
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
    describeObjectsResponse_marker,
    describeObjectsResponse_hasMoreResults,
    describeObjectsResponse_httpStatus,
    describeObjectsResponse_pipelineObjects,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DataPipeline.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for DescribeObjects.
--
-- /See:/ 'newDescribeObjects' smart constructor.
data DescribeObjects = DescribeObjects'
  { -- | The starting point for the results to be returned. For the first call,
    -- this value should be empty. As long as there are more results, continue
    -- to call @DescribeObjects@ with the marker value from the previous call
    -- to retrieve the next set of results.
    marker :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether any expressions in the object should be evaluated when
    -- the object descriptions are returned.
    evaluateExpressions :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the pipeline that contains the object definitions.
    pipelineId :: Prelude.Text,
    -- | The IDs of the pipeline objects that contain the definitions to be
    -- described. You can pass as many as 25 identifiers in a single call to
    -- @DescribeObjects@.
    objectIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeObjects
newDescribeObjects pPipelineId_ =
  DescribeObjects'
    { marker = Prelude.Nothing,
      evaluateExpressions = Prelude.Nothing,
      pipelineId = pPipelineId_,
      objectIds = Prelude.mempty
    }

-- | The starting point for the results to be returned. For the first call,
-- this value should be empty. As long as there are more results, continue
-- to call @DescribeObjects@ with the marker value from the previous call
-- to retrieve the next set of results.
describeObjects_marker :: Lens.Lens' DescribeObjects (Prelude.Maybe Prelude.Text)
describeObjects_marker = Lens.lens (\DescribeObjects' {marker} -> marker) (\s@DescribeObjects' {} a -> s {marker = a} :: DescribeObjects)

-- | Indicates whether any expressions in the object should be evaluated when
-- the object descriptions are returned.
describeObjects_evaluateExpressions :: Lens.Lens' DescribeObjects (Prelude.Maybe Prelude.Bool)
describeObjects_evaluateExpressions = Lens.lens (\DescribeObjects' {evaluateExpressions} -> evaluateExpressions) (\s@DescribeObjects' {} a -> s {evaluateExpressions = a} :: DescribeObjects)

-- | The ID of the pipeline that contains the object definitions.
describeObjects_pipelineId :: Lens.Lens' DescribeObjects Prelude.Text
describeObjects_pipelineId = Lens.lens (\DescribeObjects' {pipelineId} -> pipelineId) (\s@DescribeObjects' {} a -> s {pipelineId = a} :: DescribeObjects)

-- | The IDs of the pipeline objects that contain the definitions to be
-- described. You can pass as many as 25 identifiers in a single call to
-- @DescribeObjects@.
describeObjects_objectIds :: Lens.Lens' DescribeObjects [Prelude.Text]
describeObjects_objectIds = Lens.lens (\DescribeObjects' {objectIds} -> objectIds) (\s@DescribeObjects' {} a -> s {objectIds = a} :: DescribeObjects) Prelude.. Lens.coerced

instance Core.AWSPager DescribeObjects where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeObjectsResponse_hasMoreResults
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? describeObjectsResponse_marker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeObjects_marker
          Lens..~ rs
          Lens.^? describeObjectsResponse_marker Prelude.. Lens._Just

instance Core.AWSRequest DescribeObjects where
  type
    AWSResponse DescribeObjects =
      DescribeObjectsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeObjectsResponse'
            Prelude.<$> (x Core..?> "marker")
            Prelude.<*> (x Core..?> "hasMoreResults")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "pipelineObjects"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable DescribeObjects where
  hashWithSalt _salt DescribeObjects' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` evaluateExpressions
      `Prelude.hashWithSalt` pipelineId
      `Prelude.hashWithSalt` objectIds

instance Prelude.NFData DescribeObjects where
  rnf DescribeObjects' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf evaluateExpressions
      `Prelude.seq` Prelude.rnf pipelineId
      `Prelude.seq` Prelude.rnf objectIds

instance Core.ToHeaders DescribeObjects where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DataPipeline.DescribeObjects" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeObjects where
  toJSON DescribeObjects' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("marker" Core..=) Prelude.<$> marker,
            ("evaluateExpressions" Core..=)
              Prelude.<$> evaluateExpressions,
            Prelude.Just ("pipelineId" Core..= pipelineId),
            Prelude.Just ("objectIds" Core..= objectIds)
          ]
      )

instance Core.ToPath DescribeObjects where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeObjects where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the output of DescribeObjects.
--
-- /See:/ 'newDescribeObjectsResponse' smart constructor.
data DescribeObjectsResponse = DescribeObjectsResponse'
  { -- | The starting point for the next page of results. To view the next page
    -- of results, call @DescribeObjects@ again with this marker value. If the
    -- value is null, there are no more results.
    marker :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether there are more results to return.
    hasMoreResults :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of object definitions.
    pipelineObjects :: [PipelineObject]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeObjectsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeObjectsResponse_marker' - The starting point for the next page of results. To view the next page
-- of results, call @DescribeObjects@ again with this marker value. If the
-- value is null, there are no more results.
--
-- 'hasMoreResults', 'describeObjectsResponse_hasMoreResults' - Indicates whether there are more results to return.
--
-- 'httpStatus', 'describeObjectsResponse_httpStatus' - The response's http status code.
--
-- 'pipelineObjects', 'describeObjectsResponse_pipelineObjects' - An array of object definitions.
newDescribeObjectsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeObjectsResponse
newDescribeObjectsResponse pHttpStatus_ =
  DescribeObjectsResponse'
    { marker = Prelude.Nothing,
      hasMoreResults = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      pipelineObjects = Prelude.mempty
    }

-- | The starting point for the next page of results. To view the next page
-- of results, call @DescribeObjects@ again with this marker value. If the
-- value is null, there are no more results.
describeObjectsResponse_marker :: Lens.Lens' DescribeObjectsResponse (Prelude.Maybe Prelude.Text)
describeObjectsResponse_marker = Lens.lens (\DescribeObjectsResponse' {marker} -> marker) (\s@DescribeObjectsResponse' {} a -> s {marker = a} :: DescribeObjectsResponse)

-- | Indicates whether there are more results to return.
describeObjectsResponse_hasMoreResults :: Lens.Lens' DescribeObjectsResponse (Prelude.Maybe Prelude.Bool)
describeObjectsResponse_hasMoreResults = Lens.lens (\DescribeObjectsResponse' {hasMoreResults} -> hasMoreResults) (\s@DescribeObjectsResponse' {} a -> s {hasMoreResults = a} :: DescribeObjectsResponse)

-- | The response's http status code.
describeObjectsResponse_httpStatus :: Lens.Lens' DescribeObjectsResponse Prelude.Int
describeObjectsResponse_httpStatus = Lens.lens (\DescribeObjectsResponse' {httpStatus} -> httpStatus) (\s@DescribeObjectsResponse' {} a -> s {httpStatus = a} :: DescribeObjectsResponse)

-- | An array of object definitions.
describeObjectsResponse_pipelineObjects :: Lens.Lens' DescribeObjectsResponse [PipelineObject]
describeObjectsResponse_pipelineObjects = Lens.lens (\DescribeObjectsResponse' {pipelineObjects} -> pipelineObjects) (\s@DescribeObjectsResponse' {} a -> s {pipelineObjects = a} :: DescribeObjectsResponse) Prelude.. Lens.coerced

instance Prelude.NFData DescribeObjectsResponse where
  rnf DescribeObjectsResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf hasMoreResults
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf pipelineObjects

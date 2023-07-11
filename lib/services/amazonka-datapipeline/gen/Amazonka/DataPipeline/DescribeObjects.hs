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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    describeObjects_evaluateExpressions,
    describeObjects_marker,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataPipeline.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for DescribeObjects.
--
-- /See:/ 'newDescribeObjects' smart constructor.
data DescribeObjects = DescribeObjects'
  { -- | Indicates whether any expressions in the object should be evaluated when
    -- the object descriptions are returned.
    evaluateExpressions :: Prelude.Maybe Prelude.Bool,
    -- | The starting point for the results to be returned. For the first call,
    -- this value should be empty. As long as there are more results, continue
    -- to call @DescribeObjects@ with the marker value from the previous call
    -- to retrieve the next set of results.
    marker :: Prelude.Maybe Prelude.Text,
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
-- 'evaluateExpressions', 'describeObjects_evaluateExpressions' - Indicates whether any expressions in the object should be evaluated when
-- the object descriptions are returned.
--
-- 'marker', 'describeObjects_marker' - The starting point for the results to be returned. For the first call,
-- this value should be empty. As long as there are more results, continue
-- to call @DescribeObjects@ with the marker value from the previous call
-- to retrieve the next set of results.
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
    { evaluateExpressions =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      pipelineId = pPipelineId_,
      objectIds = Prelude.mempty
    }

-- | Indicates whether any expressions in the object should be evaluated when
-- the object descriptions are returned.
describeObjects_evaluateExpressions :: Lens.Lens' DescribeObjects (Prelude.Maybe Prelude.Bool)
describeObjects_evaluateExpressions = Lens.lens (\DescribeObjects' {evaluateExpressions} -> evaluateExpressions) (\s@DescribeObjects' {} a -> s {evaluateExpressions = a} :: DescribeObjects)

-- | The starting point for the results to be returned. For the first call,
-- this value should be empty. As long as there are more results, continue
-- to call @DescribeObjects@ with the marker value from the previous call
-- to retrieve the next set of results.
describeObjects_marker :: Lens.Lens' DescribeObjects (Prelude.Maybe Prelude.Text)
describeObjects_marker = Lens.lens (\DescribeObjects' {marker} -> marker) (\s@DescribeObjects' {} a -> s {marker = a} :: DescribeObjects)

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
            Lens.^? describeObjectsResponse_marker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeObjects_marker
          Lens..~ rs
          Lens.^? describeObjectsResponse_marker
          Prelude.. Lens._Just

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
            Prelude.<$> (x Data..?> "hasMoreResults")
            Prelude.<*> (x Data..?> "marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "pipelineObjects"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable DescribeObjects where
  hashWithSalt _salt DescribeObjects' {..} =
    _salt
      `Prelude.hashWithSalt` evaluateExpressions
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` pipelineId
      `Prelude.hashWithSalt` objectIds

instance Prelude.NFData DescribeObjects where
  rnf DescribeObjects' {..} =
    Prelude.rnf evaluateExpressions
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf pipelineId
      `Prelude.seq` Prelude.rnf objectIds

instance Data.ToHeaders DescribeObjects where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DataPipeline.DescribeObjects" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeObjects where
  toJSON DescribeObjects' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("evaluateExpressions" Data..=)
              Prelude.<$> evaluateExpressions,
            ("marker" Data..=) Prelude.<$> marker,
            Prelude.Just ("pipelineId" Data..= pipelineId),
            Prelude.Just ("objectIds" Data..= objectIds)
          ]
      )

instance Data.ToPath DescribeObjects where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeObjects where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the output of DescribeObjects.
--
-- /See:/ 'newDescribeObjectsResponse' smart constructor.
data DescribeObjectsResponse = DescribeObjectsResponse'
  { -- | Indicates whether there are more results to return.
    hasMoreResults :: Prelude.Maybe Prelude.Bool,
    -- | The starting point for the next page of results. To view the next page
    -- of results, call @DescribeObjects@ again with this marker value. If the
    -- value is null, there are no more results.
    marker :: Prelude.Maybe Prelude.Text,
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
  Prelude.Int ->
  DescribeObjectsResponse
newDescribeObjectsResponse pHttpStatus_ =
  DescribeObjectsResponse'
    { hasMoreResults =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      pipelineObjects = Prelude.mempty
    }

-- | Indicates whether there are more results to return.
describeObjectsResponse_hasMoreResults :: Lens.Lens' DescribeObjectsResponse (Prelude.Maybe Prelude.Bool)
describeObjectsResponse_hasMoreResults = Lens.lens (\DescribeObjectsResponse' {hasMoreResults} -> hasMoreResults) (\s@DescribeObjectsResponse' {} a -> s {hasMoreResults = a} :: DescribeObjectsResponse)

-- | The starting point for the next page of results. To view the next page
-- of results, call @DescribeObjects@ again with this marker value. If the
-- value is null, there are no more results.
describeObjectsResponse_marker :: Lens.Lens' DescribeObjectsResponse (Prelude.Maybe Prelude.Text)
describeObjectsResponse_marker = Lens.lens (\DescribeObjectsResponse' {marker} -> marker) (\s@DescribeObjectsResponse' {} a -> s {marker = a} :: DescribeObjectsResponse)

-- | The response's http status code.
describeObjectsResponse_httpStatus :: Lens.Lens' DescribeObjectsResponse Prelude.Int
describeObjectsResponse_httpStatus = Lens.lens (\DescribeObjectsResponse' {httpStatus} -> httpStatus) (\s@DescribeObjectsResponse' {} a -> s {httpStatus = a} :: DescribeObjectsResponse)

-- | An array of object definitions.
describeObjectsResponse_pipelineObjects :: Lens.Lens' DescribeObjectsResponse [PipelineObject]
describeObjectsResponse_pipelineObjects = Lens.lens (\DescribeObjectsResponse' {pipelineObjects} -> pipelineObjects) (\s@DescribeObjectsResponse' {} a -> s {pipelineObjects = a} :: DescribeObjectsResponse) Prelude.. Lens.coerced

instance Prelude.NFData DescribeObjectsResponse where
  rnf DescribeObjectsResponse' {..} =
    Prelude.rnf hasMoreResults
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf pipelineObjects

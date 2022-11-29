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
-- Module      : Amazonka.SageMaker.QueryLineage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this action to inspect your lineage and discover relationships
-- between entities. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/querying-lineage-entities.html Querying Lineage Entities>
-- in the /Amazon SageMaker Developer Guide/.
module Amazonka.SageMaker.QueryLineage
  ( -- * Creating a Request
    QueryLineage (..),
    newQueryLineage,

    -- * Request Lenses
    queryLineage_nextToken,
    queryLineage_filters,
    queryLineage_maxDepth,
    queryLineage_maxResults,
    queryLineage_startArns,
    queryLineage_direction,
    queryLineage_includeEdges,

    -- * Destructuring the Response
    QueryLineageResponse (..),
    newQueryLineageResponse,

    -- * Response Lenses
    queryLineageResponse_edges,
    queryLineageResponse_nextToken,
    queryLineageResponse_vertices,
    queryLineageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newQueryLineage' smart constructor.
data QueryLineage = QueryLineage'
  { -- | Limits the number of vertices in the request. Use the @NextToken@ in a
    -- response to to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A set of filtering parameters that allow you to specify which entities
    -- should be returned.
    --
    -- -   Properties - Key-value pairs to match on the lineage entities\'
    --     properties.
    --
    -- -   LineageTypes - A set of lineage entity types to match on. For
    --     example: @TrialComponent@, @Artifact@, or @Context@.
    --
    -- -   CreatedBefore - Filter entities created before this date.
    --
    -- -   ModifiedBefore - Filter entities modified before this date.
    --
    -- -   ModifiedAfter - Filter entities modified after this date.
    filters :: Prelude.Maybe QueryFilters,
    -- | The maximum depth in lineage relationships from the @StartArns@ that are
    -- traversed. Depth is a measure of the number of @Associations@ from the
    -- @StartArn@ entity to the matched results.
    maxDepth :: Prelude.Maybe Prelude.Int,
    -- | Limits the number of vertices in the results. Use the @NextToken@ in a
    -- response to to retrieve the next page of results.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | A list of resource Amazon Resource Name (ARN) that represent the
    -- starting point for your lineage query.
    startArns :: Prelude.Maybe [Prelude.Text],
    -- | Associations between lineage entities have a direction. This parameter
    -- determines the direction from the StartArn(s) that the query traverses.
    direction :: Prelude.Maybe Direction,
    -- | Setting this value to @True@ retrieves not only the entities of interest
    -- but also the
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/lineage-tracking-entities.html Associations>
    -- and lineage entities on the path. Set to @False@ to only return lineage
    -- entities that match your query.
    includeEdges :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryLineage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'queryLineage_nextToken' - Limits the number of vertices in the request. Use the @NextToken@ in a
-- response to to retrieve the next page of results.
--
-- 'filters', 'queryLineage_filters' - A set of filtering parameters that allow you to specify which entities
-- should be returned.
--
-- -   Properties - Key-value pairs to match on the lineage entities\'
--     properties.
--
-- -   LineageTypes - A set of lineage entity types to match on. For
--     example: @TrialComponent@, @Artifact@, or @Context@.
--
-- -   CreatedBefore - Filter entities created before this date.
--
-- -   ModifiedBefore - Filter entities modified before this date.
--
-- -   ModifiedAfter - Filter entities modified after this date.
--
-- 'maxDepth', 'queryLineage_maxDepth' - The maximum depth in lineage relationships from the @StartArns@ that are
-- traversed. Depth is a measure of the number of @Associations@ from the
-- @StartArn@ entity to the matched results.
--
-- 'maxResults', 'queryLineage_maxResults' - Limits the number of vertices in the results. Use the @NextToken@ in a
-- response to to retrieve the next page of results.
--
-- 'startArns', 'queryLineage_startArns' - A list of resource Amazon Resource Name (ARN) that represent the
-- starting point for your lineage query.
--
-- 'direction', 'queryLineage_direction' - Associations between lineage entities have a direction. This parameter
-- determines the direction from the StartArn(s) that the query traverses.
--
-- 'includeEdges', 'queryLineage_includeEdges' - Setting this value to @True@ retrieves not only the entities of interest
-- but also the
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/lineage-tracking-entities.html Associations>
-- and lineage entities on the path. Set to @False@ to only return lineage
-- entities that match your query.
newQueryLineage ::
  QueryLineage
newQueryLineage =
  QueryLineage'
    { nextToken = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxDepth = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      startArns = Prelude.Nothing,
      direction = Prelude.Nothing,
      includeEdges = Prelude.Nothing
    }

-- | Limits the number of vertices in the request. Use the @NextToken@ in a
-- response to to retrieve the next page of results.
queryLineage_nextToken :: Lens.Lens' QueryLineage (Prelude.Maybe Prelude.Text)
queryLineage_nextToken = Lens.lens (\QueryLineage' {nextToken} -> nextToken) (\s@QueryLineage' {} a -> s {nextToken = a} :: QueryLineage)

-- | A set of filtering parameters that allow you to specify which entities
-- should be returned.
--
-- -   Properties - Key-value pairs to match on the lineage entities\'
--     properties.
--
-- -   LineageTypes - A set of lineage entity types to match on. For
--     example: @TrialComponent@, @Artifact@, or @Context@.
--
-- -   CreatedBefore - Filter entities created before this date.
--
-- -   ModifiedBefore - Filter entities modified before this date.
--
-- -   ModifiedAfter - Filter entities modified after this date.
queryLineage_filters :: Lens.Lens' QueryLineage (Prelude.Maybe QueryFilters)
queryLineage_filters = Lens.lens (\QueryLineage' {filters} -> filters) (\s@QueryLineage' {} a -> s {filters = a} :: QueryLineage)

-- | The maximum depth in lineage relationships from the @StartArns@ that are
-- traversed. Depth is a measure of the number of @Associations@ from the
-- @StartArn@ entity to the matched results.
queryLineage_maxDepth :: Lens.Lens' QueryLineage (Prelude.Maybe Prelude.Int)
queryLineage_maxDepth = Lens.lens (\QueryLineage' {maxDepth} -> maxDepth) (\s@QueryLineage' {} a -> s {maxDepth = a} :: QueryLineage)

-- | Limits the number of vertices in the results. Use the @NextToken@ in a
-- response to to retrieve the next page of results.
queryLineage_maxResults :: Lens.Lens' QueryLineage (Prelude.Maybe Prelude.Int)
queryLineage_maxResults = Lens.lens (\QueryLineage' {maxResults} -> maxResults) (\s@QueryLineage' {} a -> s {maxResults = a} :: QueryLineage)

-- | A list of resource Amazon Resource Name (ARN) that represent the
-- starting point for your lineage query.
queryLineage_startArns :: Lens.Lens' QueryLineage (Prelude.Maybe [Prelude.Text])
queryLineage_startArns = Lens.lens (\QueryLineage' {startArns} -> startArns) (\s@QueryLineage' {} a -> s {startArns = a} :: QueryLineage) Prelude.. Lens.mapping Lens.coerced

-- | Associations between lineage entities have a direction. This parameter
-- determines the direction from the StartArn(s) that the query traverses.
queryLineage_direction :: Lens.Lens' QueryLineage (Prelude.Maybe Direction)
queryLineage_direction = Lens.lens (\QueryLineage' {direction} -> direction) (\s@QueryLineage' {} a -> s {direction = a} :: QueryLineage)

-- | Setting this value to @True@ retrieves not only the entities of interest
-- but also the
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/lineage-tracking-entities.html Associations>
-- and lineage entities on the path. Set to @False@ to only return lineage
-- entities that match your query.
queryLineage_includeEdges :: Lens.Lens' QueryLineage (Prelude.Maybe Prelude.Bool)
queryLineage_includeEdges = Lens.lens (\QueryLineage' {includeEdges} -> includeEdges) (\s@QueryLineage' {} a -> s {includeEdges = a} :: QueryLineage)

instance Core.AWSRequest QueryLineage where
  type AWSResponse QueryLineage = QueryLineageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          QueryLineageResponse'
            Prelude.<$> (x Core..?> "Edges" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Vertices" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable QueryLineage where
  hashWithSalt _salt QueryLineage' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxDepth
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` startArns
      `Prelude.hashWithSalt` direction
      `Prelude.hashWithSalt` includeEdges

instance Prelude.NFData QueryLineage where
  rnf QueryLineage' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxDepth
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf startArns
      `Prelude.seq` Prelude.rnf direction
      `Prelude.seq` Prelude.rnf includeEdges

instance Core.ToHeaders QueryLineage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.QueryLineage" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON QueryLineage where
  toJSON QueryLineage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Filters" Core..=) Prelude.<$> filters,
            ("MaxDepth" Core..=) Prelude.<$> maxDepth,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("StartArns" Core..=) Prelude.<$> startArns,
            ("Direction" Core..=) Prelude.<$> direction,
            ("IncludeEdges" Core..=) Prelude.<$> includeEdges
          ]
      )

instance Core.ToPath QueryLineage where
  toPath = Prelude.const "/"

instance Core.ToQuery QueryLineage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newQueryLineageResponse' smart constructor.
data QueryLineageResponse = QueryLineageResponse'
  { -- | A list of edges that connect vertices in the response.
    edges :: Prelude.Maybe [Edge],
    -- | Limits the number of vertices in the response. Use the @NextToken@ in a
    -- response to to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of vertices connected to the start entity(ies) in the lineage
    -- graph.
    vertices :: Prelude.Maybe [Vertex],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryLineageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'edges', 'queryLineageResponse_edges' - A list of edges that connect vertices in the response.
--
-- 'nextToken', 'queryLineageResponse_nextToken' - Limits the number of vertices in the response. Use the @NextToken@ in a
-- response to to retrieve the next page of results.
--
-- 'vertices', 'queryLineageResponse_vertices' - A list of vertices connected to the start entity(ies) in the lineage
-- graph.
--
-- 'httpStatus', 'queryLineageResponse_httpStatus' - The response's http status code.
newQueryLineageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  QueryLineageResponse
newQueryLineageResponse pHttpStatus_ =
  QueryLineageResponse'
    { edges = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      vertices = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of edges that connect vertices in the response.
queryLineageResponse_edges :: Lens.Lens' QueryLineageResponse (Prelude.Maybe [Edge])
queryLineageResponse_edges = Lens.lens (\QueryLineageResponse' {edges} -> edges) (\s@QueryLineageResponse' {} a -> s {edges = a} :: QueryLineageResponse) Prelude.. Lens.mapping Lens.coerced

-- | Limits the number of vertices in the response. Use the @NextToken@ in a
-- response to to retrieve the next page of results.
queryLineageResponse_nextToken :: Lens.Lens' QueryLineageResponse (Prelude.Maybe Prelude.Text)
queryLineageResponse_nextToken = Lens.lens (\QueryLineageResponse' {nextToken} -> nextToken) (\s@QueryLineageResponse' {} a -> s {nextToken = a} :: QueryLineageResponse)

-- | A list of vertices connected to the start entity(ies) in the lineage
-- graph.
queryLineageResponse_vertices :: Lens.Lens' QueryLineageResponse (Prelude.Maybe [Vertex])
queryLineageResponse_vertices = Lens.lens (\QueryLineageResponse' {vertices} -> vertices) (\s@QueryLineageResponse' {} a -> s {vertices = a} :: QueryLineageResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
queryLineageResponse_httpStatus :: Lens.Lens' QueryLineageResponse Prelude.Int
queryLineageResponse_httpStatus = Lens.lens (\QueryLineageResponse' {httpStatus} -> httpStatus) (\s@QueryLineageResponse' {} a -> s {httpStatus = a} :: QueryLineageResponse)

instance Prelude.NFData QueryLineageResponse where
  rnf QueryLineageResponse' {..} =
    Prelude.rnf edges
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf vertices
      `Prelude.seq` Prelude.rnf httpStatus

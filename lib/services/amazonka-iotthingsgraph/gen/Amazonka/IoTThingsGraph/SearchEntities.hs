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
-- Module      : Amazonka.IoTThingsGraph.SearchEntities
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches for entities of the specified type. You can search for entities
-- in your namespace and the public namespace that you\'re tracking.
--
-- This operation returns paginated results.
module Amazonka.IoTThingsGraph.SearchEntities
  ( -- * Creating a Request
    SearchEntities (..),
    newSearchEntities,

    -- * Request Lenses
    searchEntities_filters,
    searchEntities_namespaceVersion,
    searchEntities_nextToken,
    searchEntities_maxResults,
    searchEntities_entityTypes,

    -- * Destructuring the Response
    SearchEntitiesResponse (..),
    newSearchEntitiesResponse,

    -- * Response Lenses
    searchEntitiesResponse_nextToken,
    searchEntitiesResponse_descriptions,
    searchEntitiesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchEntities' smart constructor.
data SearchEntities = SearchEntities'
  { -- | Optional filter to apply to the search. Valid filters are @NAME@
    -- @NAMESPACE@, @SEMANTIC_TYPE_PATH@ and @REFERENCED_ENTITY_ID@.
    -- @REFERENCED_ENTITY_ID@ filters on entities that are used by the entity
    -- in the result set. For example, you can filter on the ID of a property
    -- that is used in a state.
    --
    -- Multiple filters function as OR criteria in the query. Multiple values
    -- passed inside the filter function as AND criteria.
    filters :: Prelude.Maybe [EntityFilter],
    -- | The version of the user\'s namespace. Defaults to the latest version of
    -- the user\'s namespace.
    namespaceVersion :: Prelude.Maybe Prelude.Integer,
    -- | The string that specifies the next page of results. Use this when
    -- you\'re paginating results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The entity types for which to search.
    entityTypes :: [EntityType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchEntities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'searchEntities_filters' - Optional filter to apply to the search. Valid filters are @NAME@
-- @NAMESPACE@, @SEMANTIC_TYPE_PATH@ and @REFERENCED_ENTITY_ID@.
-- @REFERENCED_ENTITY_ID@ filters on entities that are used by the entity
-- in the result set. For example, you can filter on the ID of a property
-- that is used in a state.
--
-- Multiple filters function as OR criteria in the query. Multiple values
-- passed inside the filter function as AND criteria.
--
-- 'namespaceVersion', 'searchEntities_namespaceVersion' - The version of the user\'s namespace. Defaults to the latest version of
-- the user\'s namespace.
--
-- 'nextToken', 'searchEntities_nextToken' - The string that specifies the next page of results. Use this when
-- you\'re paginating results.
--
-- 'maxResults', 'searchEntities_maxResults' - The maximum number of results to return in the response.
--
-- 'entityTypes', 'searchEntities_entityTypes' - The entity types for which to search.
newSearchEntities ::
  SearchEntities
newSearchEntities =
  SearchEntities'
    { filters = Prelude.Nothing,
      namespaceVersion = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      entityTypes = Prelude.mempty
    }

-- | Optional filter to apply to the search. Valid filters are @NAME@
-- @NAMESPACE@, @SEMANTIC_TYPE_PATH@ and @REFERENCED_ENTITY_ID@.
-- @REFERENCED_ENTITY_ID@ filters on entities that are used by the entity
-- in the result set. For example, you can filter on the ID of a property
-- that is used in a state.
--
-- Multiple filters function as OR criteria in the query. Multiple values
-- passed inside the filter function as AND criteria.
searchEntities_filters :: Lens.Lens' SearchEntities (Prelude.Maybe [EntityFilter])
searchEntities_filters = Lens.lens (\SearchEntities' {filters} -> filters) (\s@SearchEntities' {} a -> s {filters = a} :: SearchEntities) Prelude.. Lens.mapping Lens.coerced

-- | The version of the user\'s namespace. Defaults to the latest version of
-- the user\'s namespace.
searchEntities_namespaceVersion :: Lens.Lens' SearchEntities (Prelude.Maybe Prelude.Integer)
searchEntities_namespaceVersion = Lens.lens (\SearchEntities' {namespaceVersion} -> namespaceVersion) (\s@SearchEntities' {} a -> s {namespaceVersion = a} :: SearchEntities)

-- | The string that specifies the next page of results. Use this when
-- you\'re paginating results.
searchEntities_nextToken :: Lens.Lens' SearchEntities (Prelude.Maybe Prelude.Text)
searchEntities_nextToken = Lens.lens (\SearchEntities' {nextToken} -> nextToken) (\s@SearchEntities' {} a -> s {nextToken = a} :: SearchEntities)

-- | The maximum number of results to return in the response.
searchEntities_maxResults :: Lens.Lens' SearchEntities (Prelude.Maybe Prelude.Natural)
searchEntities_maxResults = Lens.lens (\SearchEntities' {maxResults} -> maxResults) (\s@SearchEntities' {} a -> s {maxResults = a} :: SearchEntities)

-- | The entity types for which to search.
searchEntities_entityTypes :: Lens.Lens' SearchEntities [EntityType]
searchEntities_entityTypes = Lens.lens (\SearchEntities' {entityTypes} -> entityTypes) (\s@SearchEntities' {} a -> s {entityTypes = a} :: SearchEntities) Prelude.. Lens.coerced

instance Core.AWSPager SearchEntities where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchEntitiesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchEntitiesResponse_descriptions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& searchEntities_nextToken
          Lens..~ rs
          Lens.^? searchEntitiesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest SearchEntities where
  type
    AWSResponse SearchEntities =
      SearchEntitiesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchEntitiesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "descriptions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchEntities where
  hashWithSalt salt' SearchEntities' {..} =
    salt' `Prelude.hashWithSalt` entityTypes
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` namespaceVersion
      `Prelude.hashWithSalt` filters

instance Prelude.NFData SearchEntities where
  rnf SearchEntities' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf entityTypes
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf namespaceVersion

instance Core.ToHeaders SearchEntities where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IotThingsGraphFrontEndService.SearchEntities" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SearchEntities where
  toJSON SearchEntities' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("filters" Core..=) Prelude.<$> filters,
            ("namespaceVersion" Core..=)
              Prelude.<$> namespaceVersion,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("entityTypes" Core..= entityTypes)
          ]
      )

instance Core.ToPath SearchEntities where
  toPath = Prelude.const "/"

instance Core.ToQuery SearchEntities where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchEntitiesResponse' smart constructor.
data SearchEntitiesResponse = SearchEntitiesResponse'
  { -- | The string to specify as @nextToken@ when you request the next page of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of descriptions for each entity returned in the search result.
    descriptions :: Prelude.Maybe [EntityDescription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchEntitiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchEntitiesResponse_nextToken' - The string to specify as @nextToken@ when you request the next page of
-- results.
--
-- 'descriptions', 'searchEntitiesResponse_descriptions' - An array of descriptions for each entity returned in the search result.
--
-- 'httpStatus', 'searchEntitiesResponse_httpStatus' - The response's http status code.
newSearchEntitiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchEntitiesResponse
newSearchEntitiesResponse pHttpStatus_ =
  SearchEntitiesResponse'
    { nextToken =
        Prelude.Nothing,
      descriptions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The string to specify as @nextToken@ when you request the next page of
-- results.
searchEntitiesResponse_nextToken :: Lens.Lens' SearchEntitiesResponse (Prelude.Maybe Prelude.Text)
searchEntitiesResponse_nextToken = Lens.lens (\SearchEntitiesResponse' {nextToken} -> nextToken) (\s@SearchEntitiesResponse' {} a -> s {nextToken = a} :: SearchEntitiesResponse)

-- | An array of descriptions for each entity returned in the search result.
searchEntitiesResponse_descriptions :: Lens.Lens' SearchEntitiesResponse (Prelude.Maybe [EntityDescription])
searchEntitiesResponse_descriptions = Lens.lens (\SearchEntitiesResponse' {descriptions} -> descriptions) (\s@SearchEntitiesResponse' {} a -> s {descriptions = a} :: SearchEntitiesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
searchEntitiesResponse_httpStatus :: Lens.Lens' SearchEntitiesResponse Prelude.Int
searchEntitiesResponse_httpStatus = Lens.lens (\SearchEntitiesResponse' {httpStatus} -> httpStatus) (\s@SearchEntitiesResponse' {} a -> s {httpStatus = a} :: SearchEntitiesResponse)

instance Prelude.NFData SearchEntitiesResponse where
  rnf SearchEntitiesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf descriptions

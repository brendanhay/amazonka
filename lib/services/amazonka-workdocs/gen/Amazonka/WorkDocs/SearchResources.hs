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
-- Module      : Amazonka.WorkDocs.SearchResources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches metadata and the content of folders, documents, document
-- versions, and comments.
--
-- This operation returns paginated results.
module Amazonka.WorkDocs.SearchResources
  ( -- * Creating a Request
    SearchResources (..),
    newSearchResources,

    -- * Request Lenses
    searchResources_additionalResponseFields,
    searchResources_authenticationToken,
    searchResources_filters,
    searchResources_limit,
    searchResources_marker,
    searchResources_orderBy,
    searchResources_organizationId,
    searchResources_queryScopes,
    searchResources_queryText,

    -- * Destructuring the Response
    SearchResourcesResponse (..),
    newSearchResourcesResponse,

    -- * Response Lenses
    searchResourcesResponse_items,
    searchResourcesResponse_marker,
    searchResourcesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkDocs.Types

-- | /See:/ 'newSearchResources' smart constructor.
data SearchResources = SearchResources'
  { -- | A list of attributes to include in the response. Used to request fields
    -- that are not normally returned in a standard response.
    additionalResponseFields :: Prelude.Maybe [AdditionalResponseFieldType],
    -- | Amazon WorkDocs authentication token. Not required when using Amazon Web
    -- Services administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Filters results based on entity metadata.
    filters :: Prelude.Maybe Filters,
    -- | Max results count per page.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The marker for the next set of results.
    marker :: Prelude.Maybe Prelude.Text,
    -- | Order by results in one or more categories.
    orderBy :: Prelude.Maybe [SearchSortResult],
    -- | Filters based on the resource owner OrgId. This is a mandatory parameter
    -- when using Admin SigV4 credentials.
    organizationId :: Prelude.Maybe Prelude.Text,
    -- | Filter based on the text field type. A Folder has only a name and no
    -- content. A Comment has only content and no name. A Document or Document
    -- Version has a name and content
    queryScopes :: Prelude.Maybe [SearchQueryScopeType],
    -- | The String to search for. Searches across different text fields based on
    -- request parameters. Use double quotes around the query string for exact
    -- phrase matches.
    queryText :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalResponseFields', 'searchResources_additionalResponseFields' - A list of attributes to include in the response. Used to request fields
-- that are not normally returned in a standard response.
--
-- 'authenticationToken', 'searchResources_authenticationToken' - Amazon WorkDocs authentication token. Not required when using Amazon Web
-- Services administrator credentials to access the API.
--
-- 'filters', 'searchResources_filters' - Filters results based on entity metadata.
--
-- 'limit', 'searchResources_limit' - Max results count per page.
--
-- 'marker', 'searchResources_marker' - The marker for the next set of results.
--
-- 'orderBy', 'searchResources_orderBy' - Order by results in one or more categories.
--
-- 'organizationId', 'searchResources_organizationId' - Filters based on the resource owner OrgId. This is a mandatory parameter
-- when using Admin SigV4 credentials.
--
-- 'queryScopes', 'searchResources_queryScopes' - Filter based on the text field type. A Folder has only a name and no
-- content. A Comment has only content and no name. A Document or Document
-- Version has a name and content
--
-- 'queryText', 'searchResources_queryText' - The String to search for. Searches across different text fields based on
-- request parameters. Use double quotes around the query string for exact
-- phrase matches.
newSearchResources ::
  SearchResources
newSearchResources =
  SearchResources'
    { additionalResponseFields =
        Prelude.Nothing,
      authenticationToken = Prelude.Nothing,
      filters = Prelude.Nothing,
      limit = Prelude.Nothing,
      marker = Prelude.Nothing,
      orderBy = Prelude.Nothing,
      organizationId = Prelude.Nothing,
      queryScopes = Prelude.Nothing,
      queryText = Prelude.Nothing
    }

-- | A list of attributes to include in the response. Used to request fields
-- that are not normally returned in a standard response.
searchResources_additionalResponseFields :: Lens.Lens' SearchResources (Prelude.Maybe [AdditionalResponseFieldType])
searchResources_additionalResponseFields = Lens.lens (\SearchResources' {additionalResponseFields} -> additionalResponseFields) (\s@SearchResources' {} a -> s {additionalResponseFields = a} :: SearchResources) Prelude.. Lens.mapping Lens.coerced

-- | Amazon WorkDocs authentication token. Not required when using Amazon Web
-- Services administrator credentials to access the API.
searchResources_authenticationToken :: Lens.Lens' SearchResources (Prelude.Maybe Prelude.Text)
searchResources_authenticationToken = Lens.lens (\SearchResources' {authenticationToken} -> authenticationToken) (\s@SearchResources' {} a -> s {authenticationToken = a} :: SearchResources) Prelude.. Lens.mapping Data._Sensitive

-- | Filters results based on entity metadata.
searchResources_filters :: Lens.Lens' SearchResources (Prelude.Maybe Filters)
searchResources_filters = Lens.lens (\SearchResources' {filters} -> filters) (\s@SearchResources' {} a -> s {filters = a} :: SearchResources)

-- | Max results count per page.
searchResources_limit :: Lens.Lens' SearchResources (Prelude.Maybe Prelude.Natural)
searchResources_limit = Lens.lens (\SearchResources' {limit} -> limit) (\s@SearchResources' {} a -> s {limit = a} :: SearchResources)

-- | The marker for the next set of results.
searchResources_marker :: Lens.Lens' SearchResources (Prelude.Maybe Prelude.Text)
searchResources_marker = Lens.lens (\SearchResources' {marker} -> marker) (\s@SearchResources' {} a -> s {marker = a} :: SearchResources)

-- | Order by results in one or more categories.
searchResources_orderBy :: Lens.Lens' SearchResources (Prelude.Maybe [SearchSortResult])
searchResources_orderBy = Lens.lens (\SearchResources' {orderBy} -> orderBy) (\s@SearchResources' {} a -> s {orderBy = a} :: SearchResources) Prelude.. Lens.mapping Lens.coerced

-- | Filters based on the resource owner OrgId. This is a mandatory parameter
-- when using Admin SigV4 credentials.
searchResources_organizationId :: Lens.Lens' SearchResources (Prelude.Maybe Prelude.Text)
searchResources_organizationId = Lens.lens (\SearchResources' {organizationId} -> organizationId) (\s@SearchResources' {} a -> s {organizationId = a} :: SearchResources)

-- | Filter based on the text field type. A Folder has only a name and no
-- content. A Comment has only content and no name. A Document or Document
-- Version has a name and content
searchResources_queryScopes :: Lens.Lens' SearchResources (Prelude.Maybe [SearchQueryScopeType])
searchResources_queryScopes = Lens.lens (\SearchResources' {queryScopes} -> queryScopes) (\s@SearchResources' {} a -> s {queryScopes = a} :: SearchResources) Prelude.. Lens.mapping Lens.coerced

-- | The String to search for. Searches across different text fields based on
-- request parameters. Use double quotes around the query string for exact
-- phrase matches.
searchResources_queryText :: Lens.Lens' SearchResources (Prelude.Maybe Prelude.Text)
searchResources_queryText = Lens.lens (\SearchResources' {queryText} -> queryText) (\s@SearchResources' {} a -> s {queryText = a} :: SearchResources) Prelude.. Lens.mapping Data._Sensitive

instance Core.AWSPager SearchResources where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchResourcesResponse_marker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchResourcesResponse_items
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& searchResources_marker
          Lens..~ rs
          Lens.^? searchResourcesResponse_marker
          Prelude.. Lens._Just

instance Core.AWSRequest SearchResources where
  type
    AWSResponse SearchResources =
      SearchResourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchResourcesResponse'
            Prelude.<$> (x Data..?> "Items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchResources where
  hashWithSalt _salt SearchResources' {..} =
    _salt
      `Prelude.hashWithSalt` additionalResponseFields
      `Prelude.hashWithSalt` authenticationToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` orderBy
      `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` queryScopes
      `Prelude.hashWithSalt` queryText

instance Prelude.NFData SearchResources where
  rnf SearchResources' {..} =
    Prelude.rnf additionalResponseFields
      `Prelude.seq` Prelude.rnf authenticationToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf orderBy
      `Prelude.seq` Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf queryScopes
      `Prelude.seq` Prelude.rnf queryText

instance Data.ToHeaders SearchResources where
  toHeaders SearchResources' {..} =
    Prelude.mconcat
      [ "Authentication" Data.=# authenticationToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON SearchResources where
  toJSON SearchResources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdditionalResponseFields" Data..=)
              Prelude.<$> additionalResponseFields,
            ("Filters" Data..=) Prelude.<$> filters,
            ("Limit" Data..=) Prelude.<$> limit,
            ("Marker" Data..=) Prelude.<$> marker,
            ("OrderBy" Data..=) Prelude.<$> orderBy,
            ("OrganizationId" Data..=)
              Prelude.<$> organizationId,
            ("QueryScopes" Data..=) Prelude.<$> queryScopes,
            ("QueryText" Data..=) Prelude.<$> queryText
          ]
      )

instance Data.ToPath SearchResources where
  toPath = Prelude.const "/api/v1/search"

instance Data.ToQuery SearchResources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchResourcesResponse' smart constructor.
data SearchResourcesResponse = SearchResourcesResponse'
  { -- | List of Documents, Folders, Comments, and Document Versions matching the
    -- query.
    items :: Prelude.Maybe [ResponseItem],
    -- | The marker to use when requesting the next set of results. If there are
    -- no additional results, the string is empty.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'searchResourcesResponse_items' - List of Documents, Folders, Comments, and Document Versions matching the
-- query.
--
-- 'marker', 'searchResourcesResponse_marker' - The marker to use when requesting the next set of results. If there are
-- no additional results, the string is empty.
--
-- 'httpStatus', 'searchResourcesResponse_httpStatus' - The response's http status code.
newSearchResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchResourcesResponse
newSearchResourcesResponse pHttpStatus_ =
  SearchResourcesResponse'
    { items = Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of Documents, Folders, Comments, and Document Versions matching the
-- query.
searchResourcesResponse_items :: Lens.Lens' SearchResourcesResponse (Prelude.Maybe [ResponseItem])
searchResourcesResponse_items = Lens.lens (\SearchResourcesResponse' {items} -> items) (\s@SearchResourcesResponse' {} a -> s {items = a} :: SearchResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The marker to use when requesting the next set of results. If there are
-- no additional results, the string is empty.
searchResourcesResponse_marker :: Lens.Lens' SearchResourcesResponse (Prelude.Maybe Prelude.Text)
searchResourcesResponse_marker = Lens.lens (\SearchResourcesResponse' {marker} -> marker) (\s@SearchResourcesResponse' {} a -> s {marker = a} :: SearchResourcesResponse)

-- | The response's http status code.
searchResourcesResponse_httpStatus :: Lens.Lens' SearchResourcesResponse Prelude.Int
searchResourcesResponse_httpStatus = Lens.lens (\SearchResourcesResponse' {httpStatus} -> httpStatus) (\s@SearchResourcesResponse' {} a -> s {httpStatus = a} :: SearchResourcesResponse)

instance Prelude.NFData SearchResourcesResponse where
  rnf SearchResourcesResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Amazonka.Route53Resolver.ListResolverQueryLogConfigAssociations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about associations between Amazon VPCs and query
-- logging configurations.
--
-- This operation returns paginated results.
module Amazonka.Route53Resolver.ListResolverQueryLogConfigAssociations
  ( -- * Creating a Request
    ListResolverQueryLogConfigAssociations (..),
    newListResolverQueryLogConfigAssociations,

    -- * Request Lenses
    listResolverQueryLogConfigAssociations_filters,
    listResolverQueryLogConfigAssociations_maxResults,
    listResolverQueryLogConfigAssociations_nextToken,
    listResolverQueryLogConfigAssociations_sortBy,
    listResolverQueryLogConfigAssociations_sortOrder,

    -- * Destructuring the Response
    ListResolverQueryLogConfigAssociationsResponse (..),
    newListResolverQueryLogConfigAssociationsResponse,

    -- * Response Lenses
    listResolverQueryLogConfigAssociationsResponse_nextToken,
    listResolverQueryLogConfigAssociationsResponse_resolverQueryLogConfigAssociations,
    listResolverQueryLogConfigAssociationsResponse_totalCount,
    listResolverQueryLogConfigAssociationsResponse_totalFilteredCount,
    listResolverQueryLogConfigAssociationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newListResolverQueryLogConfigAssociations' smart constructor.
data ListResolverQueryLogConfigAssociations = ListResolverQueryLogConfigAssociations'
  { -- | An optional specification to return a subset of query logging
    -- associations.
    --
    -- If you submit a second or subsequent
    -- @ListResolverQueryLogConfigAssociations@ request and specify the
    -- @NextToken@ parameter, you must use the same values for @Filters@, if
    -- any, as in the previous request.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of query logging associations that you want to return
    -- in the response to a @ListResolverQueryLogConfigAssociations@ request.
    -- If you don\'t specify a value for @MaxResults@, Resolver returns up to
    -- 100 query logging associations.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | For the first @ListResolverQueryLogConfigAssociations@ request, omit
    -- this value.
    --
    -- If there are more than @MaxResults@ query logging associations that
    -- match the values that you specify for @Filters@, you can submit another
    -- @ListResolverQueryLogConfigAssociations@ request to get the next group
    -- of associations. In the next request, specify the value of @NextToken@
    -- from the previous response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The element that you want Resolver to sort query logging associations
    -- by.
    --
    -- If you submit a second or subsequent
    -- @ListResolverQueryLogConfigAssociations@ request and specify the
    -- @NextToken@ parameter, you must use the same value for @SortBy@, if any,
    -- as in the previous request.
    --
    -- Valid values include the following elements:
    --
    -- -   @CreationTime@: The ID of the query logging association.
    --
    -- -   @Error@: If the value of @Status@ is @FAILED@, the value of @Error@
    --     indicates the cause:
    --
    --     -   @DESTINATION_NOT_FOUND@: The specified destination (for example,
    --         an Amazon S3 bucket) was deleted.
    --
    --     -   @ACCESS_DENIED@: Permissions don\'t allow sending logs to the
    --         destination.
    --
    --     If @Status@ is a value other than @FAILED@, @ERROR@ is null.
    --
    -- -   @Id@: The ID of the query logging association
    --
    -- -   @ResolverQueryLogConfigId@: The ID of the query logging
    --     configuration
    --
    -- -   @ResourceId@: The ID of the VPC that is associated with the query
    --     logging configuration
    --
    -- -   @Status@: The current status of the configuration. Valid values
    --     include the following:
    --
    --     -   @CREATING@: Resolver is creating an association between an
    --         Amazon VPC and a query logging configuration.
    --
    --     -   @CREATED@: The association between an Amazon VPC and a query
    --         logging configuration was successfully created. Resolver is
    --         logging queries that originate in the specified VPC.
    --
    --     -   @DELETING@: Resolver is deleting this query logging association.
    --
    --     -   @FAILED@: Resolver either couldn\'t create or couldn\'t delete
    --         the query logging association. Here are two common causes:
    --
    --         -   The specified destination (for example, an Amazon S3 bucket)
    --             was deleted.
    --
    --         -   Permissions don\'t allow sending logs to the destination.
    sortBy :: Prelude.Maybe Prelude.Text,
    -- | If you specified a value for @SortBy@, the order that you want query
    -- logging associations to be listed in, @ASCENDING@ or @DESCENDING@.
    --
    -- If you submit a second or subsequent
    -- @ListResolverQueryLogConfigAssociations@ request and specify the
    -- @NextToken@ parameter, you must use the same value for @SortOrder@, if
    -- any, as in the previous request.
    sortOrder :: Prelude.Maybe SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResolverQueryLogConfigAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listResolverQueryLogConfigAssociations_filters' - An optional specification to return a subset of query logging
-- associations.
--
-- If you submit a second or subsequent
-- @ListResolverQueryLogConfigAssociations@ request and specify the
-- @NextToken@ parameter, you must use the same values for @Filters@, if
-- any, as in the previous request.
--
-- 'maxResults', 'listResolverQueryLogConfigAssociations_maxResults' - The maximum number of query logging associations that you want to return
-- in the response to a @ListResolverQueryLogConfigAssociations@ request.
-- If you don\'t specify a value for @MaxResults@, Resolver returns up to
-- 100 query logging associations.
--
-- 'nextToken', 'listResolverQueryLogConfigAssociations_nextToken' - For the first @ListResolverQueryLogConfigAssociations@ request, omit
-- this value.
--
-- If there are more than @MaxResults@ query logging associations that
-- match the values that you specify for @Filters@, you can submit another
-- @ListResolverQueryLogConfigAssociations@ request to get the next group
-- of associations. In the next request, specify the value of @NextToken@
-- from the previous response.
--
-- 'sortBy', 'listResolverQueryLogConfigAssociations_sortBy' - The element that you want Resolver to sort query logging associations
-- by.
--
-- If you submit a second or subsequent
-- @ListResolverQueryLogConfigAssociations@ request and specify the
-- @NextToken@ parameter, you must use the same value for @SortBy@, if any,
-- as in the previous request.
--
-- Valid values include the following elements:
--
-- -   @CreationTime@: The ID of the query logging association.
--
-- -   @Error@: If the value of @Status@ is @FAILED@, the value of @Error@
--     indicates the cause:
--
--     -   @DESTINATION_NOT_FOUND@: The specified destination (for example,
--         an Amazon S3 bucket) was deleted.
--
--     -   @ACCESS_DENIED@: Permissions don\'t allow sending logs to the
--         destination.
--
--     If @Status@ is a value other than @FAILED@, @ERROR@ is null.
--
-- -   @Id@: The ID of the query logging association
--
-- -   @ResolverQueryLogConfigId@: The ID of the query logging
--     configuration
--
-- -   @ResourceId@: The ID of the VPC that is associated with the query
--     logging configuration
--
-- -   @Status@: The current status of the configuration. Valid values
--     include the following:
--
--     -   @CREATING@: Resolver is creating an association between an
--         Amazon VPC and a query logging configuration.
--
--     -   @CREATED@: The association between an Amazon VPC and a query
--         logging configuration was successfully created. Resolver is
--         logging queries that originate in the specified VPC.
--
--     -   @DELETING@: Resolver is deleting this query logging association.
--
--     -   @FAILED@: Resolver either couldn\'t create or couldn\'t delete
--         the query logging association. Here are two common causes:
--
--         -   The specified destination (for example, an Amazon S3 bucket)
--             was deleted.
--
--         -   Permissions don\'t allow sending logs to the destination.
--
-- 'sortOrder', 'listResolverQueryLogConfigAssociations_sortOrder' - If you specified a value for @SortBy@, the order that you want query
-- logging associations to be listed in, @ASCENDING@ or @DESCENDING@.
--
-- If you submit a second or subsequent
-- @ListResolverQueryLogConfigAssociations@ request and specify the
-- @NextToken@ parameter, you must use the same value for @SortOrder@, if
-- any, as in the previous request.
newListResolverQueryLogConfigAssociations ::
  ListResolverQueryLogConfigAssociations
newListResolverQueryLogConfigAssociations =
  ListResolverQueryLogConfigAssociations'
    { filters =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing
    }

-- | An optional specification to return a subset of query logging
-- associations.
--
-- If you submit a second or subsequent
-- @ListResolverQueryLogConfigAssociations@ request and specify the
-- @NextToken@ parameter, you must use the same values for @Filters@, if
-- any, as in the previous request.
listResolverQueryLogConfigAssociations_filters :: Lens.Lens' ListResolverQueryLogConfigAssociations (Prelude.Maybe [Filter])
listResolverQueryLogConfigAssociations_filters = Lens.lens (\ListResolverQueryLogConfigAssociations' {filters} -> filters) (\s@ListResolverQueryLogConfigAssociations' {} a -> s {filters = a} :: ListResolverQueryLogConfigAssociations) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of query logging associations that you want to return
-- in the response to a @ListResolverQueryLogConfigAssociations@ request.
-- If you don\'t specify a value for @MaxResults@, Resolver returns up to
-- 100 query logging associations.
listResolverQueryLogConfigAssociations_maxResults :: Lens.Lens' ListResolverQueryLogConfigAssociations (Prelude.Maybe Prelude.Natural)
listResolverQueryLogConfigAssociations_maxResults = Lens.lens (\ListResolverQueryLogConfigAssociations' {maxResults} -> maxResults) (\s@ListResolverQueryLogConfigAssociations' {} a -> s {maxResults = a} :: ListResolverQueryLogConfigAssociations)

-- | For the first @ListResolverQueryLogConfigAssociations@ request, omit
-- this value.
--
-- If there are more than @MaxResults@ query logging associations that
-- match the values that you specify for @Filters@, you can submit another
-- @ListResolverQueryLogConfigAssociations@ request to get the next group
-- of associations. In the next request, specify the value of @NextToken@
-- from the previous response.
listResolverQueryLogConfigAssociations_nextToken :: Lens.Lens' ListResolverQueryLogConfigAssociations (Prelude.Maybe Prelude.Text)
listResolverQueryLogConfigAssociations_nextToken = Lens.lens (\ListResolverQueryLogConfigAssociations' {nextToken} -> nextToken) (\s@ListResolverQueryLogConfigAssociations' {} a -> s {nextToken = a} :: ListResolverQueryLogConfigAssociations)

-- | The element that you want Resolver to sort query logging associations
-- by.
--
-- If you submit a second or subsequent
-- @ListResolverQueryLogConfigAssociations@ request and specify the
-- @NextToken@ parameter, you must use the same value for @SortBy@, if any,
-- as in the previous request.
--
-- Valid values include the following elements:
--
-- -   @CreationTime@: The ID of the query logging association.
--
-- -   @Error@: If the value of @Status@ is @FAILED@, the value of @Error@
--     indicates the cause:
--
--     -   @DESTINATION_NOT_FOUND@: The specified destination (for example,
--         an Amazon S3 bucket) was deleted.
--
--     -   @ACCESS_DENIED@: Permissions don\'t allow sending logs to the
--         destination.
--
--     If @Status@ is a value other than @FAILED@, @ERROR@ is null.
--
-- -   @Id@: The ID of the query logging association
--
-- -   @ResolverQueryLogConfigId@: The ID of the query logging
--     configuration
--
-- -   @ResourceId@: The ID of the VPC that is associated with the query
--     logging configuration
--
-- -   @Status@: The current status of the configuration. Valid values
--     include the following:
--
--     -   @CREATING@: Resolver is creating an association between an
--         Amazon VPC and a query logging configuration.
--
--     -   @CREATED@: The association between an Amazon VPC and a query
--         logging configuration was successfully created. Resolver is
--         logging queries that originate in the specified VPC.
--
--     -   @DELETING@: Resolver is deleting this query logging association.
--
--     -   @FAILED@: Resolver either couldn\'t create or couldn\'t delete
--         the query logging association. Here are two common causes:
--
--         -   The specified destination (for example, an Amazon S3 bucket)
--             was deleted.
--
--         -   Permissions don\'t allow sending logs to the destination.
listResolverQueryLogConfigAssociations_sortBy :: Lens.Lens' ListResolverQueryLogConfigAssociations (Prelude.Maybe Prelude.Text)
listResolverQueryLogConfigAssociations_sortBy = Lens.lens (\ListResolverQueryLogConfigAssociations' {sortBy} -> sortBy) (\s@ListResolverQueryLogConfigAssociations' {} a -> s {sortBy = a} :: ListResolverQueryLogConfigAssociations)

-- | If you specified a value for @SortBy@, the order that you want query
-- logging associations to be listed in, @ASCENDING@ or @DESCENDING@.
--
-- If you submit a second or subsequent
-- @ListResolverQueryLogConfigAssociations@ request and specify the
-- @NextToken@ parameter, you must use the same value for @SortOrder@, if
-- any, as in the previous request.
listResolverQueryLogConfigAssociations_sortOrder :: Lens.Lens' ListResolverQueryLogConfigAssociations (Prelude.Maybe SortOrder)
listResolverQueryLogConfigAssociations_sortOrder = Lens.lens (\ListResolverQueryLogConfigAssociations' {sortOrder} -> sortOrder) (\s@ListResolverQueryLogConfigAssociations' {} a -> s {sortOrder = a} :: ListResolverQueryLogConfigAssociations)

instance
  Core.AWSPager
    ListResolverQueryLogConfigAssociations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listResolverQueryLogConfigAssociationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listResolverQueryLogConfigAssociationsResponse_resolverQueryLogConfigAssociations
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listResolverQueryLogConfigAssociations_nextToken
              Lens..~ rs
              Lens.^? listResolverQueryLogConfigAssociationsResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListResolverQueryLogConfigAssociations
  where
  type
    AWSResponse
      ListResolverQueryLogConfigAssociations =
      ListResolverQueryLogConfigAssociationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResolverQueryLogConfigAssociationsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "ResolverQueryLogConfigAssociations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "TotalCount")
            Prelude.<*> (x Data..?> "TotalFilteredCount")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListResolverQueryLogConfigAssociations
  where
  hashWithSalt
    _salt
    ListResolverQueryLogConfigAssociations' {..} =
      _salt
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` sortBy
        `Prelude.hashWithSalt` sortOrder

instance
  Prelude.NFData
    ListResolverQueryLogConfigAssociations
  where
  rnf ListResolverQueryLogConfigAssociations' {..} =
    Prelude.rnf filters `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf sortBy `Prelude.seq`
            Prelude.rnf sortOrder

instance
  Data.ToHeaders
    ListResolverQueryLogConfigAssociations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.ListResolverQueryLogConfigAssociations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    ListResolverQueryLogConfigAssociations
  where
  toJSON ListResolverQueryLogConfigAssociations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("SortOrder" Data..=) Prelude.<$> sortOrder
          ]
      )

instance
  Data.ToPath
    ListResolverQueryLogConfigAssociations
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListResolverQueryLogConfigAssociations
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListResolverQueryLogConfigAssociationsResponse' smart constructor.
data ListResolverQueryLogConfigAssociationsResponse = ListResolverQueryLogConfigAssociationsResponse'
  { -- | If there are more than @MaxResults@ query logging associations, you can
    -- submit another @ListResolverQueryLogConfigAssociations@ request to get
    -- the next group of associations. In the next request, specify the value
    -- of @NextToken@ from the previous response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list that contains one @ResolverQueryLogConfigAssociations@ element
    -- for each query logging association that matches the values that you
    -- specified for @Filter@.
    resolverQueryLogConfigAssociations :: Prelude.Maybe [ResolverQueryLogConfigAssociation],
    -- | The total number of query logging associations that were created by the
    -- current account in the specified Region. This count can differ from the
    -- number of associations that are returned in a
    -- @ListResolverQueryLogConfigAssociations@ response, depending on the
    -- values that you specify in the request.
    totalCount :: Prelude.Maybe Prelude.Int,
    -- | The total number of query logging associations that were created by the
    -- current account in the specified Region and that match the filters that
    -- were specified in the @ListResolverQueryLogConfigAssociations@ request.
    -- For the total number of associations that were created by the current
    -- account in the specified Region, see @TotalCount@.
    totalFilteredCount :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResolverQueryLogConfigAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResolverQueryLogConfigAssociationsResponse_nextToken' - If there are more than @MaxResults@ query logging associations, you can
-- submit another @ListResolverQueryLogConfigAssociations@ request to get
-- the next group of associations. In the next request, specify the value
-- of @NextToken@ from the previous response.
--
-- 'resolverQueryLogConfigAssociations', 'listResolverQueryLogConfigAssociationsResponse_resolverQueryLogConfigAssociations' - A list that contains one @ResolverQueryLogConfigAssociations@ element
-- for each query logging association that matches the values that you
-- specified for @Filter@.
--
-- 'totalCount', 'listResolverQueryLogConfigAssociationsResponse_totalCount' - The total number of query logging associations that were created by the
-- current account in the specified Region. This count can differ from the
-- number of associations that are returned in a
-- @ListResolverQueryLogConfigAssociations@ response, depending on the
-- values that you specify in the request.
--
-- 'totalFilteredCount', 'listResolverQueryLogConfigAssociationsResponse_totalFilteredCount' - The total number of query logging associations that were created by the
-- current account in the specified Region and that match the filters that
-- were specified in the @ListResolverQueryLogConfigAssociations@ request.
-- For the total number of associations that were created by the current
-- account in the specified Region, see @TotalCount@.
--
-- 'httpStatus', 'listResolverQueryLogConfigAssociationsResponse_httpStatus' - The response's http status code.
newListResolverQueryLogConfigAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListResolverQueryLogConfigAssociationsResponse
newListResolverQueryLogConfigAssociationsResponse
  pHttpStatus_ =
    ListResolverQueryLogConfigAssociationsResponse'
      { nextToken =
          Prelude.Nothing,
        resolverQueryLogConfigAssociations =
          Prelude.Nothing,
        totalCount =
          Prelude.Nothing,
        totalFilteredCount =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | If there are more than @MaxResults@ query logging associations, you can
-- submit another @ListResolverQueryLogConfigAssociations@ request to get
-- the next group of associations. In the next request, specify the value
-- of @NextToken@ from the previous response.
listResolverQueryLogConfigAssociationsResponse_nextToken :: Lens.Lens' ListResolverQueryLogConfigAssociationsResponse (Prelude.Maybe Prelude.Text)
listResolverQueryLogConfigAssociationsResponse_nextToken = Lens.lens (\ListResolverQueryLogConfigAssociationsResponse' {nextToken} -> nextToken) (\s@ListResolverQueryLogConfigAssociationsResponse' {} a -> s {nextToken = a} :: ListResolverQueryLogConfigAssociationsResponse)

-- | A list that contains one @ResolverQueryLogConfigAssociations@ element
-- for each query logging association that matches the values that you
-- specified for @Filter@.
listResolverQueryLogConfigAssociationsResponse_resolverQueryLogConfigAssociations :: Lens.Lens' ListResolverQueryLogConfigAssociationsResponse (Prelude.Maybe [ResolverQueryLogConfigAssociation])
listResolverQueryLogConfigAssociationsResponse_resolverQueryLogConfigAssociations = Lens.lens (\ListResolverQueryLogConfigAssociationsResponse' {resolverQueryLogConfigAssociations} -> resolverQueryLogConfigAssociations) (\s@ListResolverQueryLogConfigAssociationsResponse' {} a -> s {resolverQueryLogConfigAssociations = a} :: ListResolverQueryLogConfigAssociationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The total number of query logging associations that were created by the
-- current account in the specified Region. This count can differ from the
-- number of associations that are returned in a
-- @ListResolverQueryLogConfigAssociations@ response, depending on the
-- values that you specify in the request.
listResolverQueryLogConfigAssociationsResponse_totalCount :: Lens.Lens' ListResolverQueryLogConfigAssociationsResponse (Prelude.Maybe Prelude.Int)
listResolverQueryLogConfigAssociationsResponse_totalCount = Lens.lens (\ListResolverQueryLogConfigAssociationsResponse' {totalCount} -> totalCount) (\s@ListResolverQueryLogConfigAssociationsResponse' {} a -> s {totalCount = a} :: ListResolverQueryLogConfigAssociationsResponse)

-- | The total number of query logging associations that were created by the
-- current account in the specified Region and that match the filters that
-- were specified in the @ListResolverQueryLogConfigAssociations@ request.
-- For the total number of associations that were created by the current
-- account in the specified Region, see @TotalCount@.
listResolverQueryLogConfigAssociationsResponse_totalFilteredCount :: Lens.Lens' ListResolverQueryLogConfigAssociationsResponse (Prelude.Maybe Prelude.Int)
listResolverQueryLogConfigAssociationsResponse_totalFilteredCount = Lens.lens (\ListResolverQueryLogConfigAssociationsResponse' {totalFilteredCount} -> totalFilteredCount) (\s@ListResolverQueryLogConfigAssociationsResponse' {} a -> s {totalFilteredCount = a} :: ListResolverQueryLogConfigAssociationsResponse)

-- | The response's http status code.
listResolverQueryLogConfigAssociationsResponse_httpStatus :: Lens.Lens' ListResolverQueryLogConfigAssociationsResponse Prelude.Int
listResolverQueryLogConfigAssociationsResponse_httpStatus = Lens.lens (\ListResolverQueryLogConfigAssociationsResponse' {httpStatus} -> httpStatus) (\s@ListResolverQueryLogConfigAssociationsResponse' {} a -> s {httpStatus = a} :: ListResolverQueryLogConfigAssociationsResponse)

instance
  Prelude.NFData
    ListResolverQueryLogConfigAssociationsResponse
  where
  rnf
    ListResolverQueryLogConfigAssociationsResponse' {..} =
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf resolverQueryLogConfigAssociations `Prelude.seq`
          Prelude.rnf totalCount `Prelude.seq`
            Prelude.rnf totalFilteredCount `Prelude.seq`
              Prelude.rnf httpStatus

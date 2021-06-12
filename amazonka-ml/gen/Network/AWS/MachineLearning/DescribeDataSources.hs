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
-- Module      : Network.AWS.MachineLearning.DescribeDataSources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @DataSource@ that match the search criteria in the
-- request.
--
-- This operation returns paginated results.
module Network.AWS.MachineLearning.DescribeDataSources
  ( -- * Creating a Request
    DescribeDataSources (..),
    newDescribeDataSources,

    -- * Request Lenses
    describeDataSources_sortOrder,
    describeDataSources_eq,
    describeDataSources_nextToken,
    describeDataSources_filterVariable,
    describeDataSources_gt,
    describeDataSources_ne,
    describeDataSources_prefix,
    describeDataSources_ge,
    describeDataSources_le,
    describeDataSources_lt,
    describeDataSources_limit,

    -- * Destructuring the Response
    DescribeDataSourcesResponse (..),
    newDescribeDataSourcesResponse,

    -- * Response Lenses
    describeDataSourcesResponse_nextToken,
    describeDataSourcesResponse_results,
    describeDataSourcesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeDataSources' smart constructor.
data DescribeDataSources = DescribeDataSources'
  { -- | A two-value parameter that determines the sequence of the resulting list
    -- of @DataSource@.
    --
    -- -   @asc@ - Arranges the list in ascending order (A-Z, 0-9).
    -- -   @dsc@ - Arranges the list in descending order (Z-A, 9-0).
    --
    -- Results are sorted by @FilterVariable@.
    sortOrder :: Core.Maybe SortOrder,
    -- | The equal to operator. The @DataSource@ results will have
    -- @FilterVariable@ values that exactly match the value specified with
    -- @EQ@.
    eq :: Core.Maybe Core.Text,
    -- | The ID of the page in the paginated results.
    nextToken :: Core.Maybe Core.Text,
    -- | Use one of the following variables to filter a list of @DataSource@:
    --
    -- -   @CreatedAt@ - Sets the search criteria to @DataSource@ creation
    --     dates.
    -- -   @Status@ - Sets the search criteria to @DataSource@ statuses.
    -- -   @Name@ - Sets the search criteria to the contents of @DataSource@
    --     ____ @Name@.
    -- -   @DataUri@ - Sets the search criteria to the URI of data files used
    --     to create the @DataSource@. The URI can identify either a file or an
    --     Amazon Simple Storage Service (Amazon S3) bucket or directory.
    -- -   @IAMUser@ - Sets the search criteria to the user account that
    --     invoked the @DataSource@ creation.
    filterVariable :: Core.Maybe DataSourceFilterVariable,
    -- | The greater than operator. The @DataSource@ results will have
    -- @FilterVariable@ values that are greater than the value specified with
    -- @GT@.
    gt :: Core.Maybe Core.Text,
    -- | The not equal to operator. The @DataSource@ results will have
    -- @FilterVariable@ values not equal to the value specified with @NE@.
    ne :: Core.Maybe Core.Text,
    -- | A string that is found at the beginning of a variable, such as @Name@ or
    -- @Id@.
    --
    -- For example, a @DataSource@ could have the @Name@
    -- @2014-09-09-HolidayGiftMailer@. To search for this @DataSource@, select
    -- @Name@ for the @FilterVariable@ and any of the following strings for the
    -- @Prefix@:
    --
    -- -   2014-09
    --
    -- -   2014-09-09
    --
    -- -   2014-09-09-Holiday
    prefix :: Core.Maybe Core.Text,
    -- | The greater than or equal to operator. The @DataSource@ results will
    -- have @FilterVariable@ values that are greater than or equal to the value
    -- specified with @GE@.
    ge :: Core.Maybe Core.Text,
    -- | The less than or equal to operator. The @DataSource@ results will have
    -- @FilterVariable@ values that are less than or equal to the value
    -- specified with @LE@.
    le :: Core.Maybe Core.Text,
    -- | The less than operator. The @DataSource@ results will have
    -- @FilterVariable@ values that are less than the value specified with
    -- @LT@.
    lt :: Core.Maybe Core.Text,
    -- | The maximum number of @DataSource@ to include in the result.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDataSources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'describeDataSources_sortOrder' - A two-value parameter that determines the sequence of the resulting list
-- of @DataSource@.
--
-- -   @asc@ - Arranges the list in ascending order (A-Z, 0-9).
-- -   @dsc@ - Arranges the list in descending order (Z-A, 9-0).
--
-- Results are sorted by @FilterVariable@.
--
-- 'eq', 'describeDataSources_eq' - The equal to operator. The @DataSource@ results will have
-- @FilterVariable@ values that exactly match the value specified with
-- @EQ@.
--
-- 'nextToken', 'describeDataSources_nextToken' - The ID of the page in the paginated results.
--
-- 'filterVariable', 'describeDataSources_filterVariable' - Use one of the following variables to filter a list of @DataSource@:
--
-- -   @CreatedAt@ - Sets the search criteria to @DataSource@ creation
--     dates.
-- -   @Status@ - Sets the search criteria to @DataSource@ statuses.
-- -   @Name@ - Sets the search criteria to the contents of @DataSource@
--     ____ @Name@.
-- -   @DataUri@ - Sets the search criteria to the URI of data files used
--     to create the @DataSource@. The URI can identify either a file or an
--     Amazon Simple Storage Service (Amazon S3) bucket or directory.
-- -   @IAMUser@ - Sets the search criteria to the user account that
--     invoked the @DataSource@ creation.
--
-- 'gt', 'describeDataSources_gt' - The greater than operator. The @DataSource@ results will have
-- @FilterVariable@ values that are greater than the value specified with
-- @GT@.
--
-- 'ne', 'describeDataSources_ne' - The not equal to operator. The @DataSource@ results will have
-- @FilterVariable@ values not equal to the value specified with @NE@.
--
-- 'prefix', 'describeDataSources_prefix' - A string that is found at the beginning of a variable, such as @Name@ or
-- @Id@.
--
-- For example, a @DataSource@ could have the @Name@
-- @2014-09-09-HolidayGiftMailer@. To search for this @DataSource@, select
-- @Name@ for the @FilterVariable@ and any of the following strings for the
-- @Prefix@:
--
-- -   2014-09
--
-- -   2014-09-09
--
-- -   2014-09-09-Holiday
--
-- 'ge', 'describeDataSources_ge' - The greater than or equal to operator. The @DataSource@ results will
-- have @FilterVariable@ values that are greater than or equal to the value
-- specified with @GE@.
--
-- 'le', 'describeDataSources_le' - The less than or equal to operator. The @DataSource@ results will have
-- @FilterVariable@ values that are less than or equal to the value
-- specified with @LE@.
--
-- 'lt', 'describeDataSources_lt' - The less than operator. The @DataSource@ results will have
-- @FilterVariable@ values that are less than the value specified with
-- @LT@.
--
-- 'limit', 'describeDataSources_limit' - The maximum number of @DataSource@ to include in the result.
newDescribeDataSources ::
  DescribeDataSources
newDescribeDataSources =
  DescribeDataSources'
    { sortOrder = Core.Nothing,
      eq = Core.Nothing,
      nextToken = Core.Nothing,
      filterVariable = Core.Nothing,
      gt = Core.Nothing,
      ne = Core.Nothing,
      prefix = Core.Nothing,
      ge = Core.Nothing,
      le = Core.Nothing,
      lt = Core.Nothing,
      limit = Core.Nothing
    }

-- | A two-value parameter that determines the sequence of the resulting list
-- of @DataSource@.
--
-- -   @asc@ - Arranges the list in ascending order (A-Z, 0-9).
-- -   @dsc@ - Arranges the list in descending order (Z-A, 9-0).
--
-- Results are sorted by @FilterVariable@.
describeDataSources_sortOrder :: Lens.Lens' DescribeDataSources (Core.Maybe SortOrder)
describeDataSources_sortOrder = Lens.lens (\DescribeDataSources' {sortOrder} -> sortOrder) (\s@DescribeDataSources' {} a -> s {sortOrder = a} :: DescribeDataSources)

-- | The equal to operator. The @DataSource@ results will have
-- @FilterVariable@ values that exactly match the value specified with
-- @EQ@.
describeDataSources_eq :: Lens.Lens' DescribeDataSources (Core.Maybe Core.Text)
describeDataSources_eq = Lens.lens (\DescribeDataSources' {eq} -> eq) (\s@DescribeDataSources' {} a -> s {eq = a} :: DescribeDataSources)

-- | The ID of the page in the paginated results.
describeDataSources_nextToken :: Lens.Lens' DescribeDataSources (Core.Maybe Core.Text)
describeDataSources_nextToken = Lens.lens (\DescribeDataSources' {nextToken} -> nextToken) (\s@DescribeDataSources' {} a -> s {nextToken = a} :: DescribeDataSources)

-- | Use one of the following variables to filter a list of @DataSource@:
--
-- -   @CreatedAt@ - Sets the search criteria to @DataSource@ creation
--     dates.
-- -   @Status@ - Sets the search criteria to @DataSource@ statuses.
-- -   @Name@ - Sets the search criteria to the contents of @DataSource@
--     ____ @Name@.
-- -   @DataUri@ - Sets the search criteria to the URI of data files used
--     to create the @DataSource@. The URI can identify either a file or an
--     Amazon Simple Storage Service (Amazon S3) bucket or directory.
-- -   @IAMUser@ - Sets the search criteria to the user account that
--     invoked the @DataSource@ creation.
describeDataSources_filterVariable :: Lens.Lens' DescribeDataSources (Core.Maybe DataSourceFilterVariable)
describeDataSources_filterVariable = Lens.lens (\DescribeDataSources' {filterVariable} -> filterVariable) (\s@DescribeDataSources' {} a -> s {filterVariable = a} :: DescribeDataSources)

-- | The greater than operator. The @DataSource@ results will have
-- @FilterVariable@ values that are greater than the value specified with
-- @GT@.
describeDataSources_gt :: Lens.Lens' DescribeDataSources (Core.Maybe Core.Text)
describeDataSources_gt = Lens.lens (\DescribeDataSources' {gt} -> gt) (\s@DescribeDataSources' {} a -> s {gt = a} :: DescribeDataSources)

-- | The not equal to operator. The @DataSource@ results will have
-- @FilterVariable@ values not equal to the value specified with @NE@.
describeDataSources_ne :: Lens.Lens' DescribeDataSources (Core.Maybe Core.Text)
describeDataSources_ne = Lens.lens (\DescribeDataSources' {ne} -> ne) (\s@DescribeDataSources' {} a -> s {ne = a} :: DescribeDataSources)

-- | A string that is found at the beginning of a variable, such as @Name@ or
-- @Id@.
--
-- For example, a @DataSource@ could have the @Name@
-- @2014-09-09-HolidayGiftMailer@. To search for this @DataSource@, select
-- @Name@ for the @FilterVariable@ and any of the following strings for the
-- @Prefix@:
--
-- -   2014-09
--
-- -   2014-09-09
--
-- -   2014-09-09-Holiday
describeDataSources_prefix :: Lens.Lens' DescribeDataSources (Core.Maybe Core.Text)
describeDataSources_prefix = Lens.lens (\DescribeDataSources' {prefix} -> prefix) (\s@DescribeDataSources' {} a -> s {prefix = a} :: DescribeDataSources)

-- | The greater than or equal to operator. The @DataSource@ results will
-- have @FilterVariable@ values that are greater than or equal to the value
-- specified with @GE@.
describeDataSources_ge :: Lens.Lens' DescribeDataSources (Core.Maybe Core.Text)
describeDataSources_ge = Lens.lens (\DescribeDataSources' {ge} -> ge) (\s@DescribeDataSources' {} a -> s {ge = a} :: DescribeDataSources)

-- | The less than or equal to operator. The @DataSource@ results will have
-- @FilterVariable@ values that are less than or equal to the value
-- specified with @LE@.
describeDataSources_le :: Lens.Lens' DescribeDataSources (Core.Maybe Core.Text)
describeDataSources_le = Lens.lens (\DescribeDataSources' {le} -> le) (\s@DescribeDataSources' {} a -> s {le = a} :: DescribeDataSources)

-- | The less than operator. The @DataSource@ results will have
-- @FilterVariable@ values that are less than the value specified with
-- @LT@.
describeDataSources_lt :: Lens.Lens' DescribeDataSources (Core.Maybe Core.Text)
describeDataSources_lt = Lens.lens (\DescribeDataSources' {lt} -> lt) (\s@DescribeDataSources' {} a -> s {lt = a} :: DescribeDataSources)

-- | The maximum number of @DataSource@ to include in the result.
describeDataSources_limit :: Lens.Lens' DescribeDataSources (Core.Maybe Core.Natural)
describeDataSources_limit = Lens.lens (\DescribeDataSources' {limit} -> limit) (\s@DescribeDataSources' {} a -> s {limit = a} :: DescribeDataSources)

instance Core.AWSPager DescribeDataSources where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDataSourcesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDataSourcesResponse_results
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeDataSources_nextToken
          Lens..~ rs
          Lens.^? describeDataSourcesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeDataSources where
  type
    AWSResponse DescribeDataSources =
      DescribeDataSourcesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDataSourcesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Results" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeDataSources

instance Core.NFData DescribeDataSources

instance Core.ToHeaders DescribeDataSources where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonML_20141212.DescribeDataSources" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeDataSources where
  toJSON DescribeDataSources' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SortOrder" Core..=) Core.<$> sortOrder,
            ("EQ" Core..=) Core.<$> eq,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("FilterVariable" Core..=) Core.<$> filterVariable,
            ("GT" Core..=) Core.<$> gt,
            ("NE" Core..=) Core.<$> ne,
            ("Prefix" Core..=) Core.<$> prefix,
            ("GE" Core..=) Core.<$> ge,
            ("LE" Core..=) Core.<$> le,
            ("LT" Core..=) Core.<$> lt,
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance Core.ToPath DescribeDataSources where
  toPath = Core.const "/"

instance Core.ToQuery DescribeDataSources where
  toQuery = Core.const Core.mempty

-- | Represents the query results from a DescribeDataSources operation. The
-- content is essentially a list of @DataSource@.
--
-- /See:/ 'newDescribeDataSourcesResponse' smart constructor.
data DescribeDataSourcesResponse = DescribeDataSourcesResponse'
  { -- | An ID of the next page in the paginated results that indicates at least
    -- one more page follows.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of @DataSource@ that meet the search criteria.
    results :: Core.Maybe [DataSource],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDataSourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeDataSourcesResponse_nextToken' - An ID of the next page in the paginated results that indicates at least
-- one more page follows.
--
-- 'results', 'describeDataSourcesResponse_results' - A list of @DataSource@ that meet the search criteria.
--
-- 'httpStatus', 'describeDataSourcesResponse_httpStatus' - The response's http status code.
newDescribeDataSourcesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeDataSourcesResponse
newDescribeDataSourcesResponse pHttpStatus_ =
  DescribeDataSourcesResponse'
    { nextToken =
        Core.Nothing,
      results = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An ID of the next page in the paginated results that indicates at least
-- one more page follows.
describeDataSourcesResponse_nextToken :: Lens.Lens' DescribeDataSourcesResponse (Core.Maybe Core.Text)
describeDataSourcesResponse_nextToken = Lens.lens (\DescribeDataSourcesResponse' {nextToken} -> nextToken) (\s@DescribeDataSourcesResponse' {} a -> s {nextToken = a} :: DescribeDataSourcesResponse)

-- | A list of @DataSource@ that meet the search criteria.
describeDataSourcesResponse_results :: Lens.Lens' DescribeDataSourcesResponse (Core.Maybe [DataSource])
describeDataSourcesResponse_results = Lens.lens (\DescribeDataSourcesResponse' {results} -> results) (\s@DescribeDataSourcesResponse' {} a -> s {results = a} :: DescribeDataSourcesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeDataSourcesResponse_httpStatus :: Lens.Lens' DescribeDataSourcesResponse Core.Int
describeDataSourcesResponse_httpStatus = Lens.lens (\DescribeDataSourcesResponse' {httpStatus} -> httpStatus) (\s@DescribeDataSourcesResponse' {} a -> s {httpStatus = a} :: DescribeDataSourcesResponse)

instance Core.NFData DescribeDataSourcesResponse

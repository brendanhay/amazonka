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
-- Module      : Amazonka.MachineLearning.DescribeDataSources
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @DataSource@ that match the search criteria in the
-- request.
--
-- This operation returns paginated results.
module Amazonka.MachineLearning.DescribeDataSources
  ( -- * Creating a Request
    DescribeDataSources (..),
    newDescribeDataSources,

    -- * Request Lenses
    describeDataSources_sortOrder,
    describeDataSources_nextToken,
    describeDataSources_filterVariable,
    describeDataSources_limit,
    describeDataSources_le,
    describeDataSources_lt,
    describeDataSources_eq,
    describeDataSources_prefix,
    describeDataSources_gt,
    describeDataSources_ne,
    describeDataSources_ge,

    -- * Destructuring the Response
    DescribeDataSourcesResponse (..),
    newDescribeDataSourcesResponse,

    -- * Response Lenses
    describeDataSourcesResponse_nextToken,
    describeDataSourcesResponse_results,
    describeDataSourcesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MachineLearning.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDataSources' smart constructor.
data DescribeDataSources = DescribeDataSources'
  { -- | A two-value parameter that determines the sequence of the resulting list
    -- of @DataSource@.
    --
    -- -   @asc@ - Arranges the list in ascending order (A-Z, 0-9).
    --
    -- -   @dsc@ - Arranges the list in descending order (Z-A, 9-0).
    --
    -- Results are sorted by @FilterVariable@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | The ID of the page in the paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Use one of the following variables to filter a list of @DataSource@:
    --
    -- -   @CreatedAt@ - Sets the search criteria to @DataSource@ creation
    --     dates.
    --
    -- -   @Status@ - Sets the search criteria to @DataSource@ statuses.
    --
    -- -   @Name@ - Sets the search criteria to the contents of @DataSource@
    --     @Name@.
    --
    -- -   @DataUri@ - Sets the search criteria to the URI of data files used
    --     to create the @DataSource@. The URI can identify either a file or an
    --     Amazon Simple Storage Service (Amazon S3) bucket or directory.
    --
    -- -   @IAMUser@ - Sets the search criteria to the user account that
    --     invoked the @DataSource@ creation.
    filterVariable :: Prelude.Maybe DataSourceFilterVariable,
    -- | The maximum number of @DataSource@ to include in the result.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The less than or equal to operator. The @DataSource@ results will have
    -- @FilterVariable@ values that are less than or equal to the value
    -- specified with @LE@.
    le :: Prelude.Maybe Prelude.Text,
    -- | The less than operator. The @DataSource@ results will have
    -- @FilterVariable@ values that are less than the value specified with
    -- @LT@.
    lt :: Prelude.Maybe Prelude.Text,
    -- | The equal to operator. The @DataSource@ results will have
    -- @FilterVariable@ values that exactly match the value specified with
    -- @EQ@.
    eq :: Prelude.Maybe Prelude.Text,
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
    prefix :: Prelude.Maybe Prelude.Text,
    -- | The greater than operator. The @DataSource@ results will have
    -- @FilterVariable@ values that are greater than the value specified with
    -- @GT@.
    gt :: Prelude.Maybe Prelude.Text,
    -- | The not equal to operator. The @DataSource@ results will have
    -- @FilterVariable@ values not equal to the value specified with @NE@.
    ne :: Prelude.Maybe Prelude.Text,
    -- | The greater than or equal to operator. The @DataSource@ results will
    -- have @FilterVariable@ values that are greater than or equal to the value
    -- specified with @GE@.
    ge :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
--
-- -   @dsc@ - Arranges the list in descending order (Z-A, 9-0).
--
-- Results are sorted by @FilterVariable@.
--
-- 'nextToken', 'describeDataSources_nextToken' - The ID of the page in the paginated results.
--
-- 'filterVariable', 'describeDataSources_filterVariable' - Use one of the following variables to filter a list of @DataSource@:
--
-- -   @CreatedAt@ - Sets the search criteria to @DataSource@ creation
--     dates.
--
-- -   @Status@ - Sets the search criteria to @DataSource@ statuses.
--
-- -   @Name@ - Sets the search criteria to the contents of @DataSource@
--     @Name@.
--
-- -   @DataUri@ - Sets the search criteria to the URI of data files used
--     to create the @DataSource@. The URI can identify either a file or an
--     Amazon Simple Storage Service (Amazon S3) bucket or directory.
--
-- -   @IAMUser@ - Sets the search criteria to the user account that
--     invoked the @DataSource@ creation.
--
-- 'limit', 'describeDataSources_limit' - The maximum number of @DataSource@ to include in the result.
--
-- 'le', 'describeDataSources_le' - The less than or equal to operator. The @DataSource@ results will have
-- @FilterVariable@ values that are less than or equal to the value
-- specified with @LE@.
--
-- 'lt', 'describeDataSources_lt' - The less than operator. The @DataSource@ results will have
-- @FilterVariable@ values that are less than the value specified with
-- @LT@.
--
-- 'eq', 'describeDataSources_eq' - The equal to operator. The @DataSource@ results will have
-- @FilterVariable@ values that exactly match the value specified with
-- @EQ@.
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
-- 'gt', 'describeDataSources_gt' - The greater than operator. The @DataSource@ results will have
-- @FilterVariable@ values that are greater than the value specified with
-- @GT@.
--
-- 'ne', 'describeDataSources_ne' - The not equal to operator. The @DataSource@ results will have
-- @FilterVariable@ values not equal to the value specified with @NE@.
--
-- 'ge', 'describeDataSources_ge' - The greater than or equal to operator. The @DataSource@ results will
-- have @FilterVariable@ values that are greater than or equal to the value
-- specified with @GE@.
newDescribeDataSources ::
  DescribeDataSources
newDescribeDataSources =
  DescribeDataSources'
    { sortOrder = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      filterVariable = Prelude.Nothing,
      limit = Prelude.Nothing,
      le = Prelude.Nothing,
      lt = Prelude.Nothing,
      eq = Prelude.Nothing,
      prefix = Prelude.Nothing,
      gt = Prelude.Nothing,
      ne = Prelude.Nothing,
      ge = Prelude.Nothing
    }

-- | A two-value parameter that determines the sequence of the resulting list
-- of @DataSource@.
--
-- -   @asc@ - Arranges the list in ascending order (A-Z, 0-9).
--
-- -   @dsc@ - Arranges the list in descending order (Z-A, 9-0).
--
-- Results are sorted by @FilterVariable@.
describeDataSources_sortOrder :: Lens.Lens' DescribeDataSources (Prelude.Maybe SortOrder)
describeDataSources_sortOrder = Lens.lens (\DescribeDataSources' {sortOrder} -> sortOrder) (\s@DescribeDataSources' {} a -> s {sortOrder = a} :: DescribeDataSources)

-- | The ID of the page in the paginated results.
describeDataSources_nextToken :: Lens.Lens' DescribeDataSources (Prelude.Maybe Prelude.Text)
describeDataSources_nextToken = Lens.lens (\DescribeDataSources' {nextToken} -> nextToken) (\s@DescribeDataSources' {} a -> s {nextToken = a} :: DescribeDataSources)

-- | Use one of the following variables to filter a list of @DataSource@:
--
-- -   @CreatedAt@ - Sets the search criteria to @DataSource@ creation
--     dates.
--
-- -   @Status@ - Sets the search criteria to @DataSource@ statuses.
--
-- -   @Name@ - Sets the search criteria to the contents of @DataSource@
--     @Name@.
--
-- -   @DataUri@ - Sets the search criteria to the URI of data files used
--     to create the @DataSource@. The URI can identify either a file or an
--     Amazon Simple Storage Service (Amazon S3) bucket or directory.
--
-- -   @IAMUser@ - Sets the search criteria to the user account that
--     invoked the @DataSource@ creation.
describeDataSources_filterVariable :: Lens.Lens' DescribeDataSources (Prelude.Maybe DataSourceFilterVariable)
describeDataSources_filterVariable = Lens.lens (\DescribeDataSources' {filterVariable} -> filterVariable) (\s@DescribeDataSources' {} a -> s {filterVariable = a} :: DescribeDataSources)

-- | The maximum number of @DataSource@ to include in the result.
describeDataSources_limit :: Lens.Lens' DescribeDataSources (Prelude.Maybe Prelude.Natural)
describeDataSources_limit = Lens.lens (\DescribeDataSources' {limit} -> limit) (\s@DescribeDataSources' {} a -> s {limit = a} :: DescribeDataSources)

-- | The less than or equal to operator. The @DataSource@ results will have
-- @FilterVariable@ values that are less than or equal to the value
-- specified with @LE@.
describeDataSources_le :: Lens.Lens' DescribeDataSources (Prelude.Maybe Prelude.Text)
describeDataSources_le = Lens.lens (\DescribeDataSources' {le} -> le) (\s@DescribeDataSources' {} a -> s {le = a} :: DescribeDataSources)

-- | The less than operator. The @DataSource@ results will have
-- @FilterVariable@ values that are less than the value specified with
-- @LT@.
describeDataSources_lt :: Lens.Lens' DescribeDataSources (Prelude.Maybe Prelude.Text)
describeDataSources_lt = Lens.lens (\DescribeDataSources' {lt} -> lt) (\s@DescribeDataSources' {} a -> s {lt = a} :: DescribeDataSources)

-- | The equal to operator. The @DataSource@ results will have
-- @FilterVariable@ values that exactly match the value specified with
-- @EQ@.
describeDataSources_eq :: Lens.Lens' DescribeDataSources (Prelude.Maybe Prelude.Text)
describeDataSources_eq = Lens.lens (\DescribeDataSources' {eq} -> eq) (\s@DescribeDataSources' {} a -> s {eq = a} :: DescribeDataSources)

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
describeDataSources_prefix :: Lens.Lens' DescribeDataSources (Prelude.Maybe Prelude.Text)
describeDataSources_prefix = Lens.lens (\DescribeDataSources' {prefix} -> prefix) (\s@DescribeDataSources' {} a -> s {prefix = a} :: DescribeDataSources)

-- | The greater than operator. The @DataSource@ results will have
-- @FilterVariable@ values that are greater than the value specified with
-- @GT@.
describeDataSources_gt :: Lens.Lens' DescribeDataSources (Prelude.Maybe Prelude.Text)
describeDataSources_gt = Lens.lens (\DescribeDataSources' {gt} -> gt) (\s@DescribeDataSources' {} a -> s {gt = a} :: DescribeDataSources)

-- | The not equal to operator. The @DataSource@ results will have
-- @FilterVariable@ values not equal to the value specified with @NE@.
describeDataSources_ne :: Lens.Lens' DescribeDataSources (Prelude.Maybe Prelude.Text)
describeDataSources_ne = Lens.lens (\DescribeDataSources' {ne} -> ne) (\s@DescribeDataSources' {} a -> s {ne = a} :: DescribeDataSources)

-- | The greater than or equal to operator. The @DataSource@ results will
-- have @FilterVariable@ values that are greater than or equal to the value
-- specified with @GE@.
describeDataSources_ge :: Lens.Lens' DescribeDataSources (Prelude.Maybe Prelude.Text)
describeDataSources_ge = Lens.lens (\DescribeDataSources' {ge} -> ge) (\s@DescribeDataSources' {} a -> s {ge = a} :: DescribeDataSources)

instance Core.AWSPager DescribeDataSources where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDataSourcesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDataSourcesResponse_results
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeDataSources_nextToken
          Lens..~ rs
          Lens.^? describeDataSourcesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeDataSources where
  type
    AWSResponse DescribeDataSources =
      DescribeDataSourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDataSourcesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Results" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDataSources where
  hashWithSalt _salt DescribeDataSources' {..} =
    _salt `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filterVariable
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` le
      `Prelude.hashWithSalt` lt
      `Prelude.hashWithSalt` eq
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` gt
      `Prelude.hashWithSalt` ne
      `Prelude.hashWithSalt` ge

instance Prelude.NFData DescribeDataSources where
  rnf DescribeDataSources' {..} =
    Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filterVariable
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf le
      `Prelude.seq` Prelude.rnf lt
      `Prelude.seq` Prelude.rnf eq
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf gt
      `Prelude.seq` Prelude.rnf ne
      `Prelude.seq` Prelude.rnf ge

instance Data.ToHeaders DescribeDataSources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonML_20141212.DescribeDataSources" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeDataSources where
  toJSON DescribeDataSources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SortOrder" Data..=) Prelude.<$> sortOrder,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("FilterVariable" Data..=)
              Prelude.<$> filterVariable,
            ("Limit" Data..=) Prelude.<$> limit,
            ("LE" Data..=) Prelude.<$> le,
            ("LT" Data..=) Prelude.<$> lt,
            ("EQ" Data..=) Prelude.<$> eq,
            ("Prefix" Data..=) Prelude.<$> prefix,
            ("GT" Data..=) Prelude.<$> gt,
            ("NE" Data..=) Prelude.<$> ne,
            ("GE" Data..=) Prelude.<$> ge
          ]
      )

instance Data.ToPath DescribeDataSources where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDataSources where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the query results from a DescribeDataSources operation. The
-- content is essentially a list of @DataSource@.
--
-- /See:/ 'newDescribeDataSourcesResponse' smart constructor.
data DescribeDataSourcesResponse = DescribeDataSourcesResponse'
  { -- | An ID of the next page in the paginated results that indicates at least
    -- one more page follows.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of @DataSource@ that meet the search criteria.
    results :: Prelude.Maybe [DataSource],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeDataSourcesResponse
newDescribeDataSourcesResponse pHttpStatus_ =
  DescribeDataSourcesResponse'
    { nextToken =
        Prelude.Nothing,
      results = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An ID of the next page in the paginated results that indicates at least
-- one more page follows.
describeDataSourcesResponse_nextToken :: Lens.Lens' DescribeDataSourcesResponse (Prelude.Maybe Prelude.Text)
describeDataSourcesResponse_nextToken = Lens.lens (\DescribeDataSourcesResponse' {nextToken} -> nextToken) (\s@DescribeDataSourcesResponse' {} a -> s {nextToken = a} :: DescribeDataSourcesResponse)

-- | A list of @DataSource@ that meet the search criteria.
describeDataSourcesResponse_results :: Lens.Lens' DescribeDataSourcesResponse (Prelude.Maybe [DataSource])
describeDataSourcesResponse_results = Lens.lens (\DescribeDataSourcesResponse' {results} -> results) (\s@DescribeDataSourcesResponse' {} a -> s {results = a} :: DescribeDataSourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeDataSourcesResponse_httpStatus :: Lens.Lens' DescribeDataSourcesResponse Prelude.Int
describeDataSourcesResponse_httpStatus = Lens.lens (\DescribeDataSourcesResponse' {httpStatus} -> httpStatus) (\s@DescribeDataSourcesResponse' {} a -> s {httpStatus = a} :: DescribeDataSourcesResponse)

instance Prelude.NFData DescribeDataSourcesResponse where
  rnf DescribeDataSourcesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf results
      `Prelude.seq` Prelude.rnf httpStatus

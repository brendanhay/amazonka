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
-- Module      : Amazonka.MachineLearning.DescribeEvaluations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @DescribeEvaluations@ that match the search criteria
-- in the request.
--
-- This operation returns paginated results.
module Amazonka.MachineLearning.DescribeEvaluations
  ( -- * Creating a Request
    DescribeEvaluations (..),
    newDescribeEvaluations,

    -- * Request Lenses
    describeEvaluations_eq,
    describeEvaluations_filterVariable,
    describeEvaluations_ge,
    describeEvaluations_gt,
    describeEvaluations_le,
    describeEvaluations_lt,
    describeEvaluations_limit,
    describeEvaluations_ne,
    describeEvaluations_nextToken,
    describeEvaluations_prefix,
    describeEvaluations_sortOrder,

    -- * Destructuring the Response
    DescribeEvaluationsResponse (..),
    newDescribeEvaluationsResponse,

    -- * Response Lenses
    describeEvaluationsResponse_nextToken,
    describeEvaluationsResponse_results,
    describeEvaluationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MachineLearning.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeEvaluations' smart constructor.
data DescribeEvaluations = DescribeEvaluations'
  { -- | The equal to operator. The @Evaluation@ results will have
    -- @FilterVariable@ values that exactly match the value specified with
    -- @EQ@.
    eq :: Prelude.Maybe Prelude.Text,
    -- | Use one of the following variable to filter a list of @Evaluation@
    -- objects:
    --
    -- -   @CreatedAt@ - Sets the search criteria to the @Evaluation@ creation
    --     date.
    --
    -- -   @Status@ - Sets the search criteria to the @Evaluation@ status.
    --
    -- -   @Name@ - Sets the search criteria to the contents of @Evaluation@
    --     ____ @Name@.
    --
    -- -   @IAMUser@ - Sets the search criteria to the user account that
    --     invoked an @Evaluation@.
    --
    -- -   @MLModelId@ - Sets the search criteria to the @MLModel@ that was
    --     evaluated.
    --
    -- -   @DataSourceId@ - Sets the search criteria to the @DataSource@ used
    --     in @Evaluation@.
    --
    -- -   @DataUri@ - Sets the search criteria to the data file(s) used in
    --     @Evaluation@. The URL can identify either a file or an Amazon Simple
    --     Storage Solution (Amazon S3) bucket or directory.
    filterVariable :: Prelude.Maybe EvaluationFilterVariable,
    -- | The greater than or equal to operator. The @Evaluation@ results will
    -- have @FilterVariable@ values that are greater than or equal to the value
    -- specified with @GE@.
    ge :: Prelude.Maybe Prelude.Text,
    -- | The greater than operator. The @Evaluation@ results will have
    -- @FilterVariable@ values that are greater than the value specified with
    -- @GT@.
    gt :: Prelude.Maybe Prelude.Text,
    -- | The less than or equal to operator. The @Evaluation@ results will have
    -- @FilterVariable@ values that are less than or equal to the value
    -- specified with @LE@.
    le :: Prelude.Maybe Prelude.Text,
    -- | The less than operator. The @Evaluation@ results will have
    -- @FilterVariable@ values that are less than the value specified with
    -- @LT@.
    lt :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of @Evaluation@ to include in the result.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The not equal to operator. The @Evaluation@ results will have
    -- @FilterVariable@ values not equal to the value specified with @NE@.
    ne :: Prelude.Maybe Prelude.Text,
    -- | The ID of the page in the paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A string that is found at the beginning of a variable, such as @Name@ or
    -- @Id@.
    --
    -- For example, an @Evaluation@ could have the @Name@
    -- @2014-09-09-HolidayGiftMailer@. To search for this @Evaluation@, select
    -- @Name@ for the @FilterVariable@ and any of the following strings for the
    -- @Prefix@:
    --
    -- -   2014-09
    --
    -- -   2014-09-09
    --
    -- -   2014-09-09-Holiday
    prefix :: Prelude.Maybe Prelude.Text,
    -- | A two-value parameter that determines the sequence of the resulting list
    -- of @Evaluation@.
    --
    -- -   @asc@ - Arranges the list in ascending order (A-Z, 0-9).
    --
    -- -   @dsc@ - Arranges the list in descending order (Z-A, 9-0).
    --
    -- Results are sorted by @FilterVariable@.
    sortOrder :: Prelude.Maybe SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEvaluations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eq', 'describeEvaluations_eq' - The equal to operator. The @Evaluation@ results will have
-- @FilterVariable@ values that exactly match the value specified with
-- @EQ@.
--
-- 'filterVariable', 'describeEvaluations_filterVariable' - Use one of the following variable to filter a list of @Evaluation@
-- objects:
--
-- -   @CreatedAt@ - Sets the search criteria to the @Evaluation@ creation
--     date.
--
-- -   @Status@ - Sets the search criteria to the @Evaluation@ status.
--
-- -   @Name@ - Sets the search criteria to the contents of @Evaluation@
--     ____ @Name@.
--
-- -   @IAMUser@ - Sets the search criteria to the user account that
--     invoked an @Evaluation@.
--
-- -   @MLModelId@ - Sets the search criteria to the @MLModel@ that was
--     evaluated.
--
-- -   @DataSourceId@ - Sets the search criteria to the @DataSource@ used
--     in @Evaluation@.
--
-- -   @DataUri@ - Sets the search criteria to the data file(s) used in
--     @Evaluation@. The URL can identify either a file or an Amazon Simple
--     Storage Solution (Amazon S3) bucket or directory.
--
-- 'ge', 'describeEvaluations_ge' - The greater than or equal to operator. The @Evaluation@ results will
-- have @FilterVariable@ values that are greater than or equal to the value
-- specified with @GE@.
--
-- 'gt', 'describeEvaluations_gt' - The greater than operator. The @Evaluation@ results will have
-- @FilterVariable@ values that are greater than the value specified with
-- @GT@.
--
-- 'le', 'describeEvaluations_le' - The less than or equal to operator. The @Evaluation@ results will have
-- @FilterVariable@ values that are less than or equal to the value
-- specified with @LE@.
--
-- 'lt', 'describeEvaluations_lt' - The less than operator. The @Evaluation@ results will have
-- @FilterVariable@ values that are less than the value specified with
-- @LT@.
--
-- 'limit', 'describeEvaluations_limit' - The maximum number of @Evaluation@ to include in the result.
--
-- 'ne', 'describeEvaluations_ne' - The not equal to operator. The @Evaluation@ results will have
-- @FilterVariable@ values not equal to the value specified with @NE@.
--
-- 'nextToken', 'describeEvaluations_nextToken' - The ID of the page in the paginated results.
--
-- 'prefix', 'describeEvaluations_prefix' - A string that is found at the beginning of a variable, such as @Name@ or
-- @Id@.
--
-- For example, an @Evaluation@ could have the @Name@
-- @2014-09-09-HolidayGiftMailer@. To search for this @Evaluation@, select
-- @Name@ for the @FilterVariable@ and any of the following strings for the
-- @Prefix@:
--
-- -   2014-09
--
-- -   2014-09-09
--
-- -   2014-09-09-Holiday
--
-- 'sortOrder', 'describeEvaluations_sortOrder' - A two-value parameter that determines the sequence of the resulting list
-- of @Evaluation@.
--
-- -   @asc@ - Arranges the list in ascending order (A-Z, 0-9).
--
-- -   @dsc@ - Arranges the list in descending order (Z-A, 9-0).
--
-- Results are sorted by @FilterVariable@.
newDescribeEvaluations ::
  DescribeEvaluations
newDescribeEvaluations =
  DescribeEvaluations'
    { eq = Prelude.Nothing,
      filterVariable = Prelude.Nothing,
      ge = Prelude.Nothing,
      gt = Prelude.Nothing,
      le = Prelude.Nothing,
      lt = Prelude.Nothing,
      limit = Prelude.Nothing,
      ne = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      prefix = Prelude.Nothing,
      sortOrder = Prelude.Nothing
    }

-- | The equal to operator. The @Evaluation@ results will have
-- @FilterVariable@ values that exactly match the value specified with
-- @EQ@.
describeEvaluations_eq :: Lens.Lens' DescribeEvaluations (Prelude.Maybe Prelude.Text)
describeEvaluations_eq = Lens.lens (\DescribeEvaluations' {eq} -> eq) (\s@DescribeEvaluations' {} a -> s {eq = a} :: DescribeEvaluations)

-- | Use one of the following variable to filter a list of @Evaluation@
-- objects:
--
-- -   @CreatedAt@ - Sets the search criteria to the @Evaluation@ creation
--     date.
--
-- -   @Status@ - Sets the search criteria to the @Evaluation@ status.
--
-- -   @Name@ - Sets the search criteria to the contents of @Evaluation@
--     ____ @Name@.
--
-- -   @IAMUser@ - Sets the search criteria to the user account that
--     invoked an @Evaluation@.
--
-- -   @MLModelId@ - Sets the search criteria to the @MLModel@ that was
--     evaluated.
--
-- -   @DataSourceId@ - Sets the search criteria to the @DataSource@ used
--     in @Evaluation@.
--
-- -   @DataUri@ - Sets the search criteria to the data file(s) used in
--     @Evaluation@. The URL can identify either a file or an Amazon Simple
--     Storage Solution (Amazon S3) bucket or directory.
describeEvaluations_filterVariable :: Lens.Lens' DescribeEvaluations (Prelude.Maybe EvaluationFilterVariable)
describeEvaluations_filterVariable = Lens.lens (\DescribeEvaluations' {filterVariable} -> filterVariable) (\s@DescribeEvaluations' {} a -> s {filterVariable = a} :: DescribeEvaluations)

-- | The greater than or equal to operator. The @Evaluation@ results will
-- have @FilterVariable@ values that are greater than or equal to the value
-- specified with @GE@.
describeEvaluations_ge :: Lens.Lens' DescribeEvaluations (Prelude.Maybe Prelude.Text)
describeEvaluations_ge = Lens.lens (\DescribeEvaluations' {ge} -> ge) (\s@DescribeEvaluations' {} a -> s {ge = a} :: DescribeEvaluations)

-- | The greater than operator. The @Evaluation@ results will have
-- @FilterVariable@ values that are greater than the value specified with
-- @GT@.
describeEvaluations_gt :: Lens.Lens' DescribeEvaluations (Prelude.Maybe Prelude.Text)
describeEvaluations_gt = Lens.lens (\DescribeEvaluations' {gt} -> gt) (\s@DescribeEvaluations' {} a -> s {gt = a} :: DescribeEvaluations)

-- | The less than or equal to operator. The @Evaluation@ results will have
-- @FilterVariable@ values that are less than or equal to the value
-- specified with @LE@.
describeEvaluations_le :: Lens.Lens' DescribeEvaluations (Prelude.Maybe Prelude.Text)
describeEvaluations_le = Lens.lens (\DescribeEvaluations' {le} -> le) (\s@DescribeEvaluations' {} a -> s {le = a} :: DescribeEvaluations)

-- | The less than operator. The @Evaluation@ results will have
-- @FilterVariable@ values that are less than the value specified with
-- @LT@.
describeEvaluations_lt :: Lens.Lens' DescribeEvaluations (Prelude.Maybe Prelude.Text)
describeEvaluations_lt = Lens.lens (\DescribeEvaluations' {lt} -> lt) (\s@DescribeEvaluations' {} a -> s {lt = a} :: DescribeEvaluations)

-- | The maximum number of @Evaluation@ to include in the result.
describeEvaluations_limit :: Lens.Lens' DescribeEvaluations (Prelude.Maybe Prelude.Natural)
describeEvaluations_limit = Lens.lens (\DescribeEvaluations' {limit} -> limit) (\s@DescribeEvaluations' {} a -> s {limit = a} :: DescribeEvaluations)

-- | The not equal to operator. The @Evaluation@ results will have
-- @FilterVariable@ values not equal to the value specified with @NE@.
describeEvaluations_ne :: Lens.Lens' DescribeEvaluations (Prelude.Maybe Prelude.Text)
describeEvaluations_ne = Lens.lens (\DescribeEvaluations' {ne} -> ne) (\s@DescribeEvaluations' {} a -> s {ne = a} :: DescribeEvaluations)

-- | The ID of the page in the paginated results.
describeEvaluations_nextToken :: Lens.Lens' DescribeEvaluations (Prelude.Maybe Prelude.Text)
describeEvaluations_nextToken = Lens.lens (\DescribeEvaluations' {nextToken} -> nextToken) (\s@DescribeEvaluations' {} a -> s {nextToken = a} :: DescribeEvaluations)

-- | A string that is found at the beginning of a variable, such as @Name@ or
-- @Id@.
--
-- For example, an @Evaluation@ could have the @Name@
-- @2014-09-09-HolidayGiftMailer@. To search for this @Evaluation@, select
-- @Name@ for the @FilterVariable@ and any of the following strings for the
-- @Prefix@:
--
-- -   2014-09
--
-- -   2014-09-09
--
-- -   2014-09-09-Holiday
describeEvaluations_prefix :: Lens.Lens' DescribeEvaluations (Prelude.Maybe Prelude.Text)
describeEvaluations_prefix = Lens.lens (\DescribeEvaluations' {prefix} -> prefix) (\s@DescribeEvaluations' {} a -> s {prefix = a} :: DescribeEvaluations)

-- | A two-value parameter that determines the sequence of the resulting list
-- of @Evaluation@.
--
-- -   @asc@ - Arranges the list in ascending order (A-Z, 0-9).
--
-- -   @dsc@ - Arranges the list in descending order (Z-A, 9-0).
--
-- Results are sorted by @FilterVariable@.
describeEvaluations_sortOrder :: Lens.Lens' DescribeEvaluations (Prelude.Maybe SortOrder)
describeEvaluations_sortOrder = Lens.lens (\DescribeEvaluations' {sortOrder} -> sortOrder) (\s@DescribeEvaluations' {} a -> s {sortOrder = a} :: DescribeEvaluations)

instance Core.AWSPager DescribeEvaluations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeEvaluationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeEvaluationsResponse_results
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeEvaluations_nextToken
          Lens..~ rs
          Lens.^? describeEvaluationsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeEvaluations where
  type
    AWSResponse DescribeEvaluations =
      DescribeEvaluationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEvaluationsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Results" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEvaluations where
  hashWithSalt _salt DescribeEvaluations' {..} =
    _salt
      `Prelude.hashWithSalt` eq
      `Prelude.hashWithSalt` filterVariable
      `Prelude.hashWithSalt` ge
      `Prelude.hashWithSalt` gt
      `Prelude.hashWithSalt` le
      `Prelude.hashWithSalt` lt
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` ne
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` sortOrder

instance Prelude.NFData DescribeEvaluations where
  rnf DescribeEvaluations' {..} =
    Prelude.rnf eq
      `Prelude.seq` Prelude.rnf filterVariable
      `Prelude.seq` Prelude.rnf ge
      `Prelude.seq` Prelude.rnf gt
      `Prelude.seq` Prelude.rnf le
      `Prelude.seq` Prelude.rnf lt
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf ne
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf sortOrder

instance Data.ToHeaders DescribeEvaluations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonML_20141212.DescribeEvaluations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeEvaluations where
  toJSON DescribeEvaluations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EQ" Data..=) Prelude.<$> eq,
            ("FilterVariable" Data..=)
              Prelude.<$> filterVariable,
            ("GE" Data..=) Prelude.<$> ge,
            ("GT" Data..=) Prelude.<$> gt,
            ("LE" Data..=) Prelude.<$> le,
            ("LT" Data..=) Prelude.<$> lt,
            ("Limit" Data..=) Prelude.<$> limit,
            ("NE" Data..=) Prelude.<$> ne,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Prefix" Data..=) Prelude.<$> prefix,
            ("SortOrder" Data..=) Prelude.<$> sortOrder
          ]
      )

instance Data.ToPath DescribeEvaluations where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeEvaluations where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the query results from a @DescribeEvaluations@ operation. The
-- content is essentially a list of @Evaluation@.
--
-- /See:/ 'newDescribeEvaluationsResponse' smart constructor.
data DescribeEvaluationsResponse = DescribeEvaluationsResponse'
  { -- | The ID of the next page in the paginated results that indicates at least
    -- one more page follows.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of @Evaluation@ that meet the search criteria.
    results :: Prelude.Maybe [Evaluation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEvaluationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeEvaluationsResponse_nextToken' - The ID of the next page in the paginated results that indicates at least
-- one more page follows.
--
-- 'results', 'describeEvaluationsResponse_results' - A list of @Evaluation@ that meet the search criteria.
--
-- 'httpStatus', 'describeEvaluationsResponse_httpStatus' - The response's http status code.
newDescribeEvaluationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEvaluationsResponse
newDescribeEvaluationsResponse pHttpStatus_ =
  DescribeEvaluationsResponse'
    { nextToken =
        Prelude.Nothing,
      results = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the next page in the paginated results that indicates at least
-- one more page follows.
describeEvaluationsResponse_nextToken :: Lens.Lens' DescribeEvaluationsResponse (Prelude.Maybe Prelude.Text)
describeEvaluationsResponse_nextToken = Lens.lens (\DescribeEvaluationsResponse' {nextToken} -> nextToken) (\s@DescribeEvaluationsResponse' {} a -> s {nextToken = a} :: DescribeEvaluationsResponse)

-- | A list of @Evaluation@ that meet the search criteria.
describeEvaluationsResponse_results :: Lens.Lens' DescribeEvaluationsResponse (Prelude.Maybe [Evaluation])
describeEvaluationsResponse_results = Lens.lens (\DescribeEvaluationsResponse' {results} -> results) (\s@DescribeEvaluationsResponse' {} a -> s {results = a} :: DescribeEvaluationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeEvaluationsResponse_httpStatus :: Lens.Lens' DescribeEvaluationsResponse Prelude.Int
describeEvaluationsResponse_httpStatus = Lens.lens (\DescribeEvaluationsResponse' {httpStatus} -> httpStatus) (\s@DescribeEvaluationsResponse' {} a -> s {httpStatus = a} :: DescribeEvaluationsResponse)

instance Prelude.NFData DescribeEvaluationsResponse where
  rnf DescribeEvaluationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf results
      `Prelude.seq` Prelude.rnf httpStatus

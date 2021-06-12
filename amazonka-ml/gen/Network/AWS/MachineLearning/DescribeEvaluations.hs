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
-- Module      : Network.AWS.MachineLearning.DescribeEvaluations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @DescribeEvaluations@ that match the search criteria
-- in the request.
--
-- This operation returns paginated results.
module Network.AWS.MachineLearning.DescribeEvaluations
  ( -- * Creating a Request
    DescribeEvaluations (..),
    newDescribeEvaluations,

    -- * Request Lenses
    describeEvaluations_sortOrder,
    describeEvaluations_eq,
    describeEvaluations_nextToken,
    describeEvaluations_filterVariable,
    describeEvaluations_gt,
    describeEvaluations_ne,
    describeEvaluations_prefix,
    describeEvaluations_ge,
    describeEvaluations_le,
    describeEvaluations_lt,
    describeEvaluations_limit,

    -- * Destructuring the Response
    DescribeEvaluationsResponse (..),
    newDescribeEvaluationsResponse,

    -- * Response Lenses
    describeEvaluationsResponse_nextToken,
    describeEvaluationsResponse_results,
    describeEvaluationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeEvaluations' smart constructor.
data DescribeEvaluations = DescribeEvaluations'
  { -- | A two-value parameter that determines the sequence of the resulting list
    -- of @Evaluation@.
    --
    -- -   @asc@ - Arranges the list in ascending order (A-Z, 0-9).
    -- -   @dsc@ - Arranges the list in descending order (Z-A, 9-0).
    --
    -- Results are sorted by @FilterVariable@.
    sortOrder :: Core.Maybe SortOrder,
    -- | The equal to operator. The @Evaluation@ results will have
    -- @FilterVariable@ values that exactly match the value specified with
    -- @EQ@.
    eq :: Core.Maybe Core.Text,
    -- | The ID of the page in the paginated results.
    nextToken :: Core.Maybe Core.Text,
    -- | Use one of the following variable to filter a list of @Evaluation@
    -- objects:
    --
    -- -   @CreatedAt@ - Sets the search criteria to the @Evaluation@ creation
    --     date.
    -- -   @Status@ - Sets the search criteria to the @Evaluation@ status.
    -- -   @Name@ - Sets the search criteria to the contents of @Evaluation@
    --     ____ @Name@.
    -- -   @IAMUser@ - Sets the search criteria to the user account that
    --     invoked an @Evaluation@.
    -- -   @MLModelId@ - Sets the search criteria to the @MLModel@ that was
    --     evaluated.
    -- -   @DataSourceId@ - Sets the search criteria to the @DataSource@ used
    --     in @Evaluation@.
    -- -   @DataUri@ - Sets the search criteria to the data file(s) used in
    --     @Evaluation@. The URL can identify either a file or an Amazon Simple
    --     Storage Solution (Amazon S3) bucket or directory.
    filterVariable :: Core.Maybe EvaluationFilterVariable,
    -- | The greater than operator. The @Evaluation@ results will have
    -- @FilterVariable@ values that are greater than the value specified with
    -- @GT@.
    gt :: Core.Maybe Core.Text,
    -- | The not equal to operator. The @Evaluation@ results will have
    -- @FilterVariable@ values not equal to the value specified with @NE@.
    ne :: Core.Maybe Core.Text,
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
    prefix :: Core.Maybe Core.Text,
    -- | The greater than or equal to operator. The @Evaluation@ results will
    -- have @FilterVariable@ values that are greater than or equal to the value
    -- specified with @GE@.
    ge :: Core.Maybe Core.Text,
    -- | The less than or equal to operator. The @Evaluation@ results will have
    -- @FilterVariable@ values that are less than or equal to the value
    -- specified with @LE@.
    le :: Core.Maybe Core.Text,
    -- | The less than operator. The @Evaluation@ results will have
    -- @FilterVariable@ values that are less than the value specified with
    -- @LT@.
    lt :: Core.Maybe Core.Text,
    -- | The maximum number of @Evaluation@ to include in the result.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEvaluations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'describeEvaluations_sortOrder' - A two-value parameter that determines the sequence of the resulting list
-- of @Evaluation@.
--
-- -   @asc@ - Arranges the list in ascending order (A-Z, 0-9).
-- -   @dsc@ - Arranges the list in descending order (Z-A, 9-0).
--
-- Results are sorted by @FilterVariable@.
--
-- 'eq', 'describeEvaluations_eq' - The equal to operator. The @Evaluation@ results will have
-- @FilterVariable@ values that exactly match the value specified with
-- @EQ@.
--
-- 'nextToken', 'describeEvaluations_nextToken' - The ID of the page in the paginated results.
--
-- 'filterVariable', 'describeEvaluations_filterVariable' - Use one of the following variable to filter a list of @Evaluation@
-- objects:
--
-- -   @CreatedAt@ - Sets the search criteria to the @Evaluation@ creation
--     date.
-- -   @Status@ - Sets the search criteria to the @Evaluation@ status.
-- -   @Name@ - Sets the search criteria to the contents of @Evaluation@
--     ____ @Name@.
-- -   @IAMUser@ - Sets the search criteria to the user account that
--     invoked an @Evaluation@.
-- -   @MLModelId@ - Sets the search criteria to the @MLModel@ that was
--     evaluated.
-- -   @DataSourceId@ - Sets the search criteria to the @DataSource@ used
--     in @Evaluation@.
-- -   @DataUri@ - Sets the search criteria to the data file(s) used in
--     @Evaluation@. The URL can identify either a file or an Amazon Simple
--     Storage Solution (Amazon S3) bucket or directory.
--
-- 'gt', 'describeEvaluations_gt' - The greater than operator. The @Evaluation@ results will have
-- @FilterVariable@ values that are greater than the value specified with
-- @GT@.
--
-- 'ne', 'describeEvaluations_ne' - The not equal to operator. The @Evaluation@ results will have
-- @FilterVariable@ values not equal to the value specified with @NE@.
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
-- 'ge', 'describeEvaluations_ge' - The greater than or equal to operator. The @Evaluation@ results will
-- have @FilterVariable@ values that are greater than or equal to the value
-- specified with @GE@.
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
newDescribeEvaluations ::
  DescribeEvaluations
newDescribeEvaluations =
  DescribeEvaluations'
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
-- of @Evaluation@.
--
-- -   @asc@ - Arranges the list in ascending order (A-Z, 0-9).
-- -   @dsc@ - Arranges the list in descending order (Z-A, 9-0).
--
-- Results are sorted by @FilterVariable@.
describeEvaluations_sortOrder :: Lens.Lens' DescribeEvaluations (Core.Maybe SortOrder)
describeEvaluations_sortOrder = Lens.lens (\DescribeEvaluations' {sortOrder} -> sortOrder) (\s@DescribeEvaluations' {} a -> s {sortOrder = a} :: DescribeEvaluations)

-- | The equal to operator. The @Evaluation@ results will have
-- @FilterVariable@ values that exactly match the value specified with
-- @EQ@.
describeEvaluations_eq :: Lens.Lens' DescribeEvaluations (Core.Maybe Core.Text)
describeEvaluations_eq = Lens.lens (\DescribeEvaluations' {eq} -> eq) (\s@DescribeEvaluations' {} a -> s {eq = a} :: DescribeEvaluations)

-- | The ID of the page in the paginated results.
describeEvaluations_nextToken :: Lens.Lens' DescribeEvaluations (Core.Maybe Core.Text)
describeEvaluations_nextToken = Lens.lens (\DescribeEvaluations' {nextToken} -> nextToken) (\s@DescribeEvaluations' {} a -> s {nextToken = a} :: DescribeEvaluations)

-- | Use one of the following variable to filter a list of @Evaluation@
-- objects:
--
-- -   @CreatedAt@ - Sets the search criteria to the @Evaluation@ creation
--     date.
-- -   @Status@ - Sets the search criteria to the @Evaluation@ status.
-- -   @Name@ - Sets the search criteria to the contents of @Evaluation@
--     ____ @Name@.
-- -   @IAMUser@ - Sets the search criteria to the user account that
--     invoked an @Evaluation@.
-- -   @MLModelId@ - Sets the search criteria to the @MLModel@ that was
--     evaluated.
-- -   @DataSourceId@ - Sets the search criteria to the @DataSource@ used
--     in @Evaluation@.
-- -   @DataUri@ - Sets the search criteria to the data file(s) used in
--     @Evaluation@. The URL can identify either a file or an Amazon Simple
--     Storage Solution (Amazon S3) bucket or directory.
describeEvaluations_filterVariable :: Lens.Lens' DescribeEvaluations (Core.Maybe EvaluationFilterVariable)
describeEvaluations_filterVariable = Lens.lens (\DescribeEvaluations' {filterVariable} -> filterVariable) (\s@DescribeEvaluations' {} a -> s {filterVariable = a} :: DescribeEvaluations)

-- | The greater than operator. The @Evaluation@ results will have
-- @FilterVariable@ values that are greater than the value specified with
-- @GT@.
describeEvaluations_gt :: Lens.Lens' DescribeEvaluations (Core.Maybe Core.Text)
describeEvaluations_gt = Lens.lens (\DescribeEvaluations' {gt} -> gt) (\s@DescribeEvaluations' {} a -> s {gt = a} :: DescribeEvaluations)

-- | The not equal to operator. The @Evaluation@ results will have
-- @FilterVariable@ values not equal to the value specified with @NE@.
describeEvaluations_ne :: Lens.Lens' DescribeEvaluations (Core.Maybe Core.Text)
describeEvaluations_ne = Lens.lens (\DescribeEvaluations' {ne} -> ne) (\s@DescribeEvaluations' {} a -> s {ne = a} :: DescribeEvaluations)

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
describeEvaluations_prefix :: Lens.Lens' DescribeEvaluations (Core.Maybe Core.Text)
describeEvaluations_prefix = Lens.lens (\DescribeEvaluations' {prefix} -> prefix) (\s@DescribeEvaluations' {} a -> s {prefix = a} :: DescribeEvaluations)

-- | The greater than or equal to operator. The @Evaluation@ results will
-- have @FilterVariable@ values that are greater than or equal to the value
-- specified with @GE@.
describeEvaluations_ge :: Lens.Lens' DescribeEvaluations (Core.Maybe Core.Text)
describeEvaluations_ge = Lens.lens (\DescribeEvaluations' {ge} -> ge) (\s@DescribeEvaluations' {} a -> s {ge = a} :: DescribeEvaluations)

-- | The less than or equal to operator. The @Evaluation@ results will have
-- @FilterVariable@ values that are less than or equal to the value
-- specified with @LE@.
describeEvaluations_le :: Lens.Lens' DescribeEvaluations (Core.Maybe Core.Text)
describeEvaluations_le = Lens.lens (\DescribeEvaluations' {le} -> le) (\s@DescribeEvaluations' {} a -> s {le = a} :: DescribeEvaluations)

-- | The less than operator. The @Evaluation@ results will have
-- @FilterVariable@ values that are less than the value specified with
-- @LT@.
describeEvaluations_lt :: Lens.Lens' DescribeEvaluations (Core.Maybe Core.Text)
describeEvaluations_lt = Lens.lens (\DescribeEvaluations' {lt} -> lt) (\s@DescribeEvaluations' {} a -> s {lt = a} :: DescribeEvaluations)

-- | The maximum number of @Evaluation@ to include in the result.
describeEvaluations_limit :: Lens.Lens' DescribeEvaluations (Core.Maybe Core.Natural)
describeEvaluations_limit = Lens.lens (\DescribeEvaluations' {limit} -> limit) (\s@DescribeEvaluations' {} a -> s {limit = a} :: DescribeEvaluations)

instance Core.AWSPager DescribeEvaluations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeEvaluationsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeEvaluationsResponse_results
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeEvaluations_nextToken
          Lens..~ rs
          Lens.^? describeEvaluationsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeEvaluations where
  type
    AWSResponse DescribeEvaluations =
      DescribeEvaluationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEvaluationsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Results" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeEvaluations

instance Core.NFData DescribeEvaluations

instance Core.ToHeaders DescribeEvaluations where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonML_20141212.DescribeEvaluations" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeEvaluations where
  toJSON DescribeEvaluations' {..} =
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

instance Core.ToPath DescribeEvaluations where
  toPath = Core.const "/"

instance Core.ToQuery DescribeEvaluations where
  toQuery = Core.const Core.mempty

-- | Represents the query results from a @DescribeEvaluations@ operation. The
-- content is essentially a list of @Evaluation@.
--
-- /See:/ 'newDescribeEvaluationsResponse' smart constructor.
data DescribeEvaluationsResponse = DescribeEvaluationsResponse'
  { -- | The ID of the next page in the paginated results that indicates at least
    -- one more page follows.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of @Evaluation@ that meet the search criteria.
    results :: Core.Maybe [Evaluation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeEvaluationsResponse
newDescribeEvaluationsResponse pHttpStatus_ =
  DescribeEvaluationsResponse'
    { nextToken =
        Core.Nothing,
      results = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the next page in the paginated results that indicates at least
-- one more page follows.
describeEvaluationsResponse_nextToken :: Lens.Lens' DescribeEvaluationsResponse (Core.Maybe Core.Text)
describeEvaluationsResponse_nextToken = Lens.lens (\DescribeEvaluationsResponse' {nextToken} -> nextToken) (\s@DescribeEvaluationsResponse' {} a -> s {nextToken = a} :: DescribeEvaluationsResponse)

-- | A list of @Evaluation@ that meet the search criteria.
describeEvaluationsResponse_results :: Lens.Lens' DescribeEvaluationsResponse (Core.Maybe [Evaluation])
describeEvaluationsResponse_results = Lens.lens (\DescribeEvaluationsResponse' {results} -> results) (\s@DescribeEvaluationsResponse' {} a -> s {results = a} :: DescribeEvaluationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeEvaluationsResponse_httpStatus :: Lens.Lens' DescribeEvaluationsResponse Core.Int
describeEvaluationsResponse_httpStatus = Lens.lens (\DescribeEvaluationsResponse' {httpStatus} -> httpStatus) (\s@DescribeEvaluationsResponse' {} a -> s {httpStatus = a} :: DescribeEvaluationsResponse)

instance Core.NFData DescribeEvaluationsResponse

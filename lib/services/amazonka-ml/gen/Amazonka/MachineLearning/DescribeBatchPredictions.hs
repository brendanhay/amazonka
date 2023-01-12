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
-- Module      : Amazonka.MachineLearning.DescribeBatchPredictions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @BatchPrediction@ operations that match the search
-- criteria in the request.
--
-- This operation returns paginated results.
module Amazonka.MachineLearning.DescribeBatchPredictions
  ( -- * Creating a Request
    DescribeBatchPredictions (..),
    newDescribeBatchPredictions,

    -- * Request Lenses
    describeBatchPredictions_eq,
    describeBatchPredictions_filterVariable,
    describeBatchPredictions_ge,
    describeBatchPredictions_gt,
    describeBatchPredictions_le,
    describeBatchPredictions_lt,
    describeBatchPredictions_limit,
    describeBatchPredictions_ne,
    describeBatchPredictions_nextToken,
    describeBatchPredictions_prefix,
    describeBatchPredictions_sortOrder,

    -- * Destructuring the Response
    DescribeBatchPredictionsResponse (..),
    newDescribeBatchPredictionsResponse,

    -- * Response Lenses
    describeBatchPredictionsResponse_nextToken,
    describeBatchPredictionsResponse_results,
    describeBatchPredictionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MachineLearning.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeBatchPredictions' smart constructor.
data DescribeBatchPredictions = DescribeBatchPredictions'
  { -- | The equal to operator. The @BatchPrediction@ results will have
    -- @FilterVariable@ values that exactly match the value specified with
    -- @EQ@.
    eq :: Prelude.Maybe Prelude.Text,
    -- | Use one of the following variables to filter a list of
    -- @BatchPrediction@:
    --
    -- -   @CreatedAt@ - Sets the search criteria to the @BatchPrediction@
    --     creation date.
    --
    -- -   @Status@ - Sets the search criteria to the @BatchPrediction@ status.
    --
    -- -   @Name@ - Sets the search criteria to the contents of the
    --     @BatchPrediction@ ____ @Name@.
    --
    -- -   @IAMUser@ - Sets the search criteria to the user account that
    --     invoked the @BatchPrediction@ creation.
    --
    -- -   @MLModelId@ - Sets the search criteria to the @MLModel@ used in the
    --     @BatchPrediction@.
    --
    -- -   @DataSourceId@ - Sets the search criteria to the @DataSource@ used
    --     in the @BatchPrediction@.
    --
    -- -   @DataURI@ - Sets the search criteria to the data file(s) used in the
    --     @BatchPrediction@. The URL can identify either a file or an Amazon
    --     Simple Storage Solution (Amazon S3) bucket or directory.
    filterVariable :: Prelude.Maybe BatchPredictionFilterVariable,
    -- | The greater than or equal to operator. The @BatchPrediction@ results
    -- will have @FilterVariable@ values that are greater than or equal to the
    -- value specified with @GE@.
    ge :: Prelude.Maybe Prelude.Text,
    -- | The greater than operator. The @BatchPrediction@ results will have
    -- @FilterVariable@ values that are greater than the value specified with
    -- @GT@.
    gt :: Prelude.Maybe Prelude.Text,
    -- | The less than or equal to operator. The @BatchPrediction@ results will
    -- have @FilterVariable@ values that are less than or equal to the value
    -- specified with @LE@.
    le :: Prelude.Maybe Prelude.Text,
    -- | The less than operator. The @BatchPrediction@ results will have
    -- @FilterVariable@ values that are less than the value specified with
    -- @LT@.
    lt :: Prelude.Maybe Prelude.Text,
    -- | The number of pages of information to include in the result. The range
    -- of acceptable values is @1@ through @100@. The default value is @100@.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The not equal to operator. The @BatchPrediction@ results will have
    -- @FilterVariable@ values not equal to the value specified with @NE@.
    ne :: Prelude.Maybe Prelude.Text,
    -- | An ID of the page in the paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A string that is found at the beginning of a variable, such as @Name@ or
    -- @Id@.
    --
    -- For example, a @Batch Prediction@ operation could have the @Name@
    -- @2014-09-09-HolidayGiftMailer@. To search for this @BatchPrediction@,
    -- select @Name@ for the @FilterVariable@ and any of the following strings
    -- for the @Prefix@:
    --
    -- -   2014-09
    --
    -- -   2014-09-09
    --
    -- -   2014-09-09-Holiday
    prefix :: Prelude.Maybe Prelude.Text,
    -- | A two-value parameter that determines the sequence of the resulting list
    -- of @MLModel@s.
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
-- Create a value of 'DescribeBatchPredictions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eq', 'describeBatchPredictions_eq' - The equal to operator. The @BatchPrediction@ results will have
-- @FilterVariable@ values that exactly match the value specified with
-- @EQ@.
--
-- 'filterVariable', 'describeBatchPredictions_filterVariable' - Use one of the following variables to filter a list of
-- @BatchPrediction@:
--
-- -   @CreatedAt@ - Sets the search criteria to the @BatchPrediction@
--     creation date.
--
-- -   @Status@ - Sets the search criteria to the @BatchPrediction@ status.
--
-- -   @Name@ - Sets the search criteria to the contents of the
--     @BatchPrediction@ ____ @Name@.
--
-- -   @IAMUser@ - Sets the search criteria to the user account that
--     invoked the @BatchPrediction@ creation.
--
-- -   @MLModelId@ - Sets the search criteria to the @MLModel@ used in the
--     @BatchPrediction@.
--
-- -   @DataSourceId@ - Sets the search criteria to the @DataSource@ used
--     in the @BatchPrediction@.
--
-- -   @DataURI@ - Sets the search criteria to the data file(s) used in the
--     @BatchPrediction@. The URL can identify either a file or an Amazon
--     Simple Storage Solution (Amazon S3) bucket or directory.
--
-- 'ge', 'describeBatchPredictions_ge' - The greater than or equal to operator. The @BatchPrediction@ results
-- will have @FilterVariable@ values that are greater than or equal to the
-- value specified with @GE@.
--
-- 'gt', 'describeBatchPredictions_gt' - The greater than operator. The @BatchPrediction@ results will have
-- @FilterVariable@ values that are greater than the value specified with
-- @GT@.
--
-- 'le', 'describeBatchPredictions_le' - The less than or equal to operator. The @BatchPrediction@ results will
-- have @FilterVariable@ values that are less than or equal to the value
-- specified with @LE@.
--
-- 'lt', 'describeBatchPredictions_lt' - The less than operator. The @BatchPrediction@ results will have
-- @FilterVariable@ values that are less than the value specified with
-- @LT@.
--
-- 'limit', 'describeBatchPredictions_limit' - The number of pages of information to include in the result. The range
-- of acceptable values is @1@ through @100@. The default value is @100@.
--
-- 'ne', 'describeBatchPredictions_ne' - The not equal to operator. The @BatchPrediction@ results will have
-- @FilterVariable@ values not equal to the value specified with @NE@.
--
-- 'nextToken', 'describeBatchPredictions_nextToken' - An ID of the page in the paginated results.
--
-- 'prefix', 'describeBatchPredictions_prefix' - A string that is found at the beginning of a variable, such as @Name@ or
-- @Id@.
--
-- For example, a @Batch Prediction@ operation could have the @Name@
-- @2014-09-09-HolidayGiftMailer@. To search for this @BatchPrediction@,
-- select @Name@ for the @FilterVariable@ and any of the following strings
-- for the @Prefix@:
--
-- -   2014-09
--
-- -   2014-09-09
--
-- -   2014-09-09-Holiday
--
-- 'sortOrder', 'describeBatchPredictions_sortOrder' - A two-value parameter that determines the sequence of the resulting list
-- of @MLModel@s.
--
-- -   @asc@ - Arranges the list in ascending order (A-Z, 0-9).
--
-- -   @dsc@ - Arranges the list in descending order (Z-A, 9-0).
--
-- Results are sorted by @FilterVariable@.
newDescribeBatchPredictions ::
  DescribeBatchPredictions
newDescribeBatchPredictions =
  DescribeBatchPredictions'
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

-- | The equal to operator. The @BatchPrediction@ results will have
-- @FilterVariable@ values that exactly match the value specified with
-- @EQ@.
describeBatchPredictions_eq :: Lens.Lens' DescribeBatchPredictions (Prelude.Maybe Prelude.Text)
describeBatchPredictions_eq = Lens.lens (\DescribeBatchPredictions' {eq} -> eq) (\s@DescribeBatchPredictions' {} a -> s {eq = a} :: DescribeBatchPredictions)

-- | Use one of the following variables to filter a list of
-- @BatchPrediction@:
--
-- -   @CreatedAt@ - Sets the search criteria to the @BatchPrediction@
--     creation date.
--
-- -   @Status@ - Sets the search criteria to the @BatchPrediction@ status.
--
-- -   @Name@ - Sets the search criteria to the contents of the
--     @BatchPrediction@ ____ @Name@.
--
-- -   @IAMUser@ - Sets the search criteria to the user account that
--     invoked the @BatchPrediction@ creation.
--
-- -   @MLModelId@ - Sets the search criteria to the @MLModel@ used in the
--     @BatchPrediction@.
--
-- -   @DataSourceId@ - Sets the search criteria to the @DataSource@ used
--     in the @BatchPrediction@.
--
-- -   @DataURI@ - Sets the search criteria to the data file(s) used in the
--     @BatchPrediction@. The URL can identify either a file or an Amazon
--     Simple Storage Solution (Amazon S3) bucket or directory.
describeBatchPredictions_filterVariable :: Lens.Lens' DescribeBatchPredictions (Prelude.Maybe BatchPredictionFilterVariable)
describeBatchPredictions_filterVariable = Lens.lens (\DescribeBatchPredictions' {filterVariable} -> filterVariable) (\s@DescribeBatchPredictions' {} a -> s {filterVariable = a} :: DescribeBatchPredictions)

-- | The greater than or equal to operator. The @BatchPrediction@ results
-- will have @FilterVariable@ values that are greater than or equal to the
-- value specified with @GE@.
describeBatchPredictions_ge :: Lens.Lens' DescribeBatchPredictions (Prelude.Maybe Prelude.Text)
describeBatchPredictions_ge = Lens.lens (\DescribeBatchPredictions' {ge} -> ge) (\s@DescribeBatchPredictions' {} a -> s {ge = a} :: DescribeBatchPredictions)

-- | The greater than operator. The @BatchPrediction@ results will have
-- @FilterVariable@ values that are greater than the value specified with
-- @GT@.
describeBatchPredictions_gt :: Lens.Lens' DescribeBatchPredictions (Prelude.Maybe Prelude.Text)
describeBatchPredictions_gt = Lens.lens (\DescribeBatchPredictions' {gt} -> gt) (\s@DescribeBatchPredictions' {} a -> s {gt = a} :: DescribeBatchPredictions)

-- | The less than or equal to operator. The @BatchPrediction@ results will
-- have @FilterVariable@ values that are less than or equal to the value
-- specified with @LE@.
describeBatchPredictions_le :: Lens.Lens' DescribeBatchPredictions (Prelude.Maybe Prelude.Text)
describeBatchPredictions_le = Lens.lens (\DescribeBatchPredictions' {le} -> le) (\s@DescribeBatchPredictions' {} a -> s {le = a} :: DescribeBatchPredictions)

-- | The less than operator. The @BatchPrediction@ results will have
-- @FilterVariable@ values that are less than the value specified with
-- @LT@.
describeBatchPredictions_lt :: Lens.Lens' DescribeBatchPredictions (Prelude.Maybe Prelude.Text)
describeBatchPredictions_lt = Lens.lens (\DescribeBatchPredictions' {lt} -> lt) (\s@DescribeBatchPredictions' {} a -> s {lt = a} :: DescribeBatchPredictions)

-- | The number of pages of information to include in the result. The range
-- of acceptable values is @1@ through @100@. The default value is @100@.
describeBatchPredictions_limit :: Lens.Lens' DescribeBatchPredictions (Prelude.Maybe Prelude.Natural)
describeBatchPredictions_limit = Lens.lens (\DescribeBatchPredictions' {limit} -> limit) (\s@DescribeBatchPredictions' {} a -> s {limit = a} :: DescribeBatchPredictions)

-- | The not equal to operator. The @BatchPrediction@ results will have
-- @FilterVariable@ values not equal to the value specified with @NE@.
describeBatchPredictions_ne :: Lens.Lens' DescribeBatchPredictions (Prelude.Maybe Prelude.Text)
describeBatchPredictions_ne = Lens.lens (\DescribeBatchPredictions' {ne} -> ne) (\s@DescribeBatchPredictions' {} a -> s {ne = a} :: DescribeBatchPredictions)

-- | An ID of the page in the paginated results.
describeBatchPredictions_nextToken :: Lens.Lens' DescribeBatchPredictions (Prelude.Maybe Prelude.Text)
describeBatchPredictions_nextToken = Lens.lens (\DescribeBatchPredictions' {nextToken} -> nextToken) (\s@DescribeBatchPredictions' {} a -> s {nextToken = a} :: DescribeBatchPredictions)

-- | A string that is found at the beginning of a variable, such as @Name@ or
-- @Id@.
--
-- For example, a @Batch Prediction@ operation could have the @Name@
-- @2014-09-09-HolidayGiftMailer@. To search for this @BatchPrediction@,
-- select @Name@ for the @FilterVariable@ and any of the following strings
-- for the @Prefix@:
--
-- -   2014-09
--
-- -   2014-09-09
--
-- -   2014-09-09-Holiday
describeBatchPredictions_prefix :: Lens.Lens' DescribeBatchPredictions (Prelude.Maybe Prelude.Text)
describeBatchPredictions_prefix = Lens.lens (\DescribeBatchPredictions' {prefix} -> prefix) (\s@DescribeBatchPredictions' {} a -> s {prefix = a} :: DescribeBatchPredictions)

-- | A two-value parameter that determines the sequence of the resulting list
-- of @MLModel@s.
--
-- -   @asc@ - Arranges the list in ascending order (A-Z, 0-9).
--
-- -   @dsc@ - Arranges the list in descending order (Z-A, 9-0).
--
-- Results are sorted by @FilterVariable@.
describeBatchPredictions_sortOrder :: Lens.Lens' DescribeBatchPredictions (Prelude.Maybe SortOrder)
describeBatchPredictions_sortOrder = Lens.lens (\DescribeBatchPredictions' {sortOrder} -> sortOrder) (\s@DescribeBatchPredictions' {} a -> s {sortOrder = a} :: DescribeBatchPredictions)

instance Core.AWSPager DescribeBatchPredictions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeBatchPredictionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeBatchPredictionsResponse_results
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeBatchPredictions_nextToken
          Lens..~ rs
          Lens.^? describeBatchPredictionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeBatchPredictions where
  type
    AWSResponse DescribeBatchPredictions =
      DescribeBatchPredictionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBatchPredictionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Results" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeBatchPredictions where
  hashWithSalt _salt DescribeBatchPredictions' {..} =
    _salt `Prelude.hashWithSalt` eq
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

instance Prelude.NFData DescribeBatchPredictions where
  rnf DescribeBatchPredictions' {..} =
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

instance Data.ToHeaders DescribeBatchPredictions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonML_20141212.DescribeBatchPredictions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeBatchPredictions where
  toJSON DescribeBatchPredictions' {..} =
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

instance Data.ToPath DescribeBatchPredictions where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeBatchPredictions where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @DescribeBatchPredictions@ operation. The
-- content is essentially a list of @BatchPrediction@s.
--
-- /See:/ 'newDescribeBatchPredictionsResponse' smart constructor.
data DescribeBatchPredictionsResponse = DescribeBatchPredictionsResponse'
  { -- | The ID of the next page in the paginated results that indicates at least
    -- one more page follows.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of @BatchPrediction@ objects that meet the search criteria.
    results :: Prelude.Maybe [BatchPrediction],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBatchPredictionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeBatchPredictionsResponse_nextToken' - The ID of the next page in the paginated results that indicates at least
-- one more page follows.
--
-- 'results', 'describeBatchPredictionsResponse_results' - A list of @BatchPrediction@ objects that meet the search criteria.
--
-- 'httpStatus', 'describeBatchPredictionsResponse_httpStatus' - The response's http status code.
newDescribeBatchPredictionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeBatchPredictionsResponse
newDescribeBatchPredictionsResponse pHttpStatus_ =
  DescribeBatchPredictionsResponse'
    { nextToken =
        Prelude.Nothing,
      results = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the next page in the paginated results that indicates at least
-- one more page follows.
describeBatchPredictionsResponse_nextToken :: Lens.Lens' DescribeBatchPredictionsResponse (Prelude.Maybe Prelude.Text)
describeBatchPredictionsResponse_nextToken = Lens.lens (\DescribeBatchPredictionsResponse' {nextToken} -> nextToken) (\s@DescribeBatchPredictionsResponse' {} a -> s {nextToken = a} :: DescribeBatchPredictionsResponse)

-- | A list of @BatchPrediction@ objects that meet the search criteria.
describeBatchPredictionsResponse_results :: Lens.Lens' DescribeBatchPredictionsResponse (Prelude.Maybe [BatchPrediction])
describeBatchPredictionsResponse_results = Lens.lens (\DescribeBatchPredictionsResponse' {results} -> results) (\s@DescribeBatchPredictionsResponse' {} a -> s {results = a} :: DescribeBatchPredictionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeBatchPredictionsResponse_httpStatus :: Lens.Lens' DescribeBatchPredictionsResponse Prelude.Int
describeBatchPredictionsResponse_httpStatus = Lens.lens (\DescribeBatchPredictionsResponse' {httpStatus} -> httpStatus) (\s@DescribeBatchPredictionsResponse' {} a -> s {httpStatus = a} :: DescribeBatchPredictionsResponse)

instance
  Prelude.NFData
    DescribeBatchPredictionsResponse
  where
  rnf DescribeBatchPredictionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf results
      `Prelude.seq` Prelude.rnf httpStatus

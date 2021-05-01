{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MachineLearning.DescribeBatchPredictions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @BatchPrediction@ operations that match the search
-- criteria in the request.
--
-- This operation returns paginated results.
module Network.AWS.MachineLearning.DescribeBatchPredictions
  ( -- * Creating a Request
    DescribeBatchPredictions (..),
    newDescribeBatchPredictions,

    -- * Request Lenses
    describeBatchPredictions_sortOrder,
    describeBatchPredictions_eq,
    describeBatchPredictions_nextToken,
    describeBatchPredictions_filterVariable,
    describeBatchPredictions_gt,
    describeBatchPredictions_ne,
    describeBatchPredictions_prefix,
    describeBatchPredictions_ge,
    describeBatchPredictions_le,
    describeBatchPredictions_lt,
    describeBatchPredictions_limit,

    -- * Destructuring the Response
    DescribeBatchPredictionsResponse (..),
    newDescribeBatchPredictionsResponse,

    -- * Response Lenses
    describeBatchPredictionsResponse_nextToken,
    describeBatchPredictionsResponse_results,
    describeBatchPredictionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeBatchPredictions' smart constructor.
data DescribeBatchPredictions = DescribeBatchPredictions'
  { -- | A two-value parameter that determines the sequence of the resulting list
    -- of @MLModel@s.
    --
    -- -   @asc@ - Arranges the list in ascending order (A-Z, 0-9).
    -- -   @dsc@ - Arranges the list in descending order (Z-A, 9-0).
    --
    -- Results are sorted by @FilterVariable@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | The equal to operator. The @BatchPrediction@ results will have
    -- @FilterVariable@ values that exactly match the value specified with
    -- @EQ@.
    eq :: Prelude.Maybe Prelude.Text,
    -- | An ID of the page in the paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Use one of the following variables to filter a list of
    -- @BatchPrediction@:
    --
    -- -   @CreatedAt@ - Sets the search criteria to the @BatchPrediction@
    --     creation date.
    -- -   @Status@ - Sets the search criteria to the @BatchPrediction@ status.
    -- -   @Name@ - Sets the search criteria to the contents of the
    --     @BatchPrediction@ ____ @Name@.
    -- -   @IAMUser@ - Sets the search criteria to the user account that
    --     invoked the @BatchPrediction@ creation.
    -- -   @MLModelId@ - Sets the search criteria to the @MLModel@ used in the
    --     @BatchPrediction@.
    -- -   @DataSourceId@ - Sets the search criteria to the @DataSource@ used
    --     in the @BatchPrediction@.
    -- -   @DataURI@ - Sets the search criteria to the data file(s) used in the
    --     @BatchPrediction@. The URL can identify either a file or an Amazon
    --     Simple Storage Solution (Amazon S3) bucket or directory.
    filterVariable :: Prelude.Maybe BatchPredictionFilterVariable,
    -- | The greater than operator. The @BatchPrediction@ results will have
    -- @FilterVariable@ values that are greater than the value specified with
    -- @GT@.
    gt :: Prelude.Maybe Prelude.Text,
    -- | The not equal to operator. The @BatchPrediction@ results will have
    -- @FilterVariable@ values not equal to the value specified with @NE@.
    ne :: Prelude.Maybe Prelude.Text,
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
    -- | The greater than or equal to operator. The @BatchPrediction@ results
    -- will have @FilterVariable@ values that are greater than or equal to the
    -- value specified with @GE@.
    ge :: Prelude.Maybe Prelude.Text,
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
    limit :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeBatchPredictions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'describeBatchPredictions_sortOrder' - A two-value parameter that determines the sequence of the resulting list
-- of @MLModel@s.
--
-- -   @asc@ - Arranges the list in ascending order (A-Z, 0-9).
-- -   @dsc@ - Arranges the list in descending order (Z-A, 9-0).
--
-- Results are sorted by @FilterVariable@.
--
-- 'eq', 'describeBatchPredictions_eq' - The equal to operator. The @BatchPrediction@ results will have
-- @FilterVariable@ values that exactly match the value specified with
-- @EQ@.
--
-- 'nextToken', 'describeBatchPredictions_nextToken' - An ID of the page in the paginated results.
--
-- 'filterVariable', 'describeBatchPredictions_filterVariable' - Use one of the following variables to filter a list of
-- @BatchPrediction@:
--
-- -   @CreatedAt@ - Sets the search criteria to the @BatchPrediction@
--     creation date.
-- -   @Status@ - Sets the search criteria to the @BatchPrediction@ status.
-- -   @Name@ - Sets the search criteria to the contents of the
--     @BatchPrediction@ ____ @Name@.
-- -   @IAMUser@ - Sets the search criteria to the user account that
--     invoked the @BatchPrediction@ creation.
-- -   @MLModelId@ - Sets the search criteria to the @MLModel@ used in the
--     @BatchPrediction@.
-- -   @DataSourceId@ - Sets the search criteria to the @DataSource@ used
--     in the @BatchPrediction@.
-- -   @DataURI@ - Sets the search criteria to the data file(s) used in the
--     @BatchPrediction@. The URL can identify either a file or an Amazon
--     Simple Storage Solution (Amazon S3) bucket or directory.
--
-- 'gt', 'describeBatchPredictions_gt' - The greater than operator. The @BatchPrediction@ results will have
-- @FilterVariable@ values that are greater than the value specified with
-- @GT@.
--
-- 'ne', 'describeBatchPredictions_ne' - The not equal to operator. The @BatchPrediction@ results will have
-- @FilterVariable@ values not equal to the value specified with @NE@.
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
-- 'ge', 'describeBatchPredictions_ge' - The greater than or equal to operator. The @BatchPrediction@ results
-- will have @FilterVariable@ values that are greater than or equal to the
-- value specified with @GE@.
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
newDescribeBatchPredictions ::
  DescribeBatchPredictions
newDescribeBatchPredictions =
  DescribeBatchPredictions'
    { sortOrder =
        Prelude.Nothing,
      eq = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      filterVariable = Prelude.Nothing,
      gt = Prelude.Nothing,
      ne = Prelude.Nothing,
      prefix = Prelude.Nothing,
      ge = Prelude.Nothing,
      le = Prelude.Nothing,
      lt = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | A two-value parameter that determines the sequence of the resulting list
-- of @MLModel@s.
--
-- -   @asc@ - Arranges the list in ascending order (A-Z, 0-9).
-- -   @dsc@ - Arranges the list in descending order (Z-A, 9-0).
--
-- Results are sorted by @FilterVariable@.
describeBatchPredictions_sortOrder :: Lens.Lens' DescribeBatchPredictions (Prelude.Maybe SortOrder)
describeBatchPredictions_sortOrder = Lens.lens (\DescribeBatchPredictions' {sortOrder} -> sortOrder) (\s@DescribeBatchPredictions' {} a -> s {sortOrder = a} :: DescribeBatchPredictions)

-- | The equal to operator. The @BatchPrediction@ results will have
-- @FilterVariable@ values that exactly match the value specified with
-- @EQ@.
describeBatchPredictions_eq :: Lens.Lens' DescribeBatchPredictions (Prelude.Maybe Prelude.Text)
describeBatchPredictions_eq = Lens.lens (\DescribeBatchPredictions' {eq} -> eq) (\s@DescribeBatchPredictions' {} a -> s {eq = a} :: DescribeBatchPredictions)

-- | An ID of the page in the paginated results.
describeBatchPredictions_nextToken :: Lens.Lens' DescribeBatchPredictions (Prelude.Maybe Prelude.Text)
describeBatchPredictions_nextToken = Lens.lens (\DescribeBatchPredictions' {nextToken} -> nextToken) (\s@DescribeBatchPredictions' {} a -> s {nextToken = a} :: DescribeBatchPredictions)

-- | Use one of the following variables to filter a list of
-- @BatchPrediction@:
--
-- -   @CreatedAt@ - Sets the search criteria to the @BatchPrediction@
--     creation date.
-- -   @Status@ - Sets the search criteria to the @BatchPrediction@ status.
-- -   @Name@ - Sets the search criteria to the contents of the
--     @BatchPrediction@ ____ @Name@.
-- -   @IAMUser@ - Sets the search criteria to the user account that
--     invoked the @BatchPrediction@ creation.
-- -   @MLModelId@ - Sets the search criteria to the @MLModel@ used in the
--     @BatchPrediction@.
-- -   @DataSourceId@ - Sets the search criteria to the @DataSource@ used
--     in the @BatchPrediction@.
-- -   @DataURI@ - Sets the search criteria to the data file(s) used in the
--     @BatchPrediction@. The URL can identify either a file or an Amazon
--     Simple Storage Solution (Amazon S3) bucket or directory.
describeBatchPredictions_filterVariable :: Lens.Lens' DescribeBatchPredictions (Prelude.Maybe BatchPredictionFilterVariable)
describeBatchPredictions_filterVariable = Lens.lens (\DescribeBatchPredictions' {filterVariable} -> filterVariable) (\s@DescribeBatchPredictions' {} a -> s {filterVariable = a} :: DescribeBatchPredictions)

-- | The greater than operator. The @BatchPrediction@ results will have
-- @FilterVariable@ values that are greater than the value specified with
-- @GT@.
describeBatchPredictions_gt :: Lens.Lens' DescribeBatchPredictions (Prelude.Maybe Prelude.Text)
describeBatchPredictions_gt = Lens.lens (\DescribeBatchPredictions' {gt} -> gt) (\s@DescribeBatchPredictions' {} a -> s {gt = a} :: DescribeBatchPredictions)

-- | The not equal to operator. The @BatchPrediction@ results will have
-- @FilterVariable@ values not equal to the value specified with @NE@.
describeBatchPredictions_ne :: Lens.Lens' DescribeBatchPredictions (Prelude.Maybe Prelude.Text)
describeBatchPredictions_ne = Lens.lens (\DescribeBatchPredictions' {ne} -> ne) (\s@DescribeBatchPredictions' {} a -> s {ne = a} :: DescribeBatchPredictions)

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

-- | The greater than or equal to operator. The @BatchPrediction@ results
-- will have @FilterVariable@ values that are greater than or equal to the
-- value specified with @GE@.
describeBatchPredictions_ge :: Lens.Lens' DescribeBatchPredictions (Prelude.Maybe Prelude.Text)
describeBatchPredictions_ge = Lens.lens (\DescribeBatchPredictions' {ge} -> ge) (\s@DescribeBatchPredictions' {} a -> s {ge = a} :: DescribeBatchPredictions)

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

instance Pager.AWSPager DescribeBatchPredictions where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeBatchPredictionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeBatchPredictionsResponse_results
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeBatchPredictions_nextToken
          Lens..~ rs
          Lens.^? describeBatchPredictionsResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest DescribeBatchPredictions where
  type
    Rs DescribeBatchPredictions =
      DescribeBatchPredictionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBatchPredictionsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> (x Prelude..?> "Results" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeBatchPredictions

instance Prelude.NFData DescribeBatchPredictions

instance Prelude.ToHeaders DescribeBatchPredictions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonML_20141212.DescribeBatchPredictions" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeBatchPredictions where
  toJSON DescribeBatchPredictions' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SortOrder" Prelude..=) Prelude.<$> sortOrder,
            ("EQ" Prelude..=) Prelude.<$> eq,
            ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("FilterVariable" Prelude..=)
              Prelude.<$> filterVariable,
            ("GT" Prelude..=) Prelude.<$> gt,
            ("NE" Prelude..=) Prelude.<$> ne,
            ("Prefix" Prelude..=) Prelude.<$> prefix,
            ("GE" Prelude..=) Prelude.<$> ge,
            ("LE" Prelude..=) Prelude.<$> le,
            ("LT" Prelude..=) Prelude.<$> lt,
            ("Limit" Prelude..=) Prelude.<$> limit
          ]
      )

instance Prelude.ToPath DescribeBatchPredictions where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeBatchPredictions where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
describeBatchPredictionsResponse_results = Lens.lens (\DescribeBatchPredictionsResponse' {results} -> results) (\s@DescribeBatchPredictionsResponse' {} a -> s {results = a} :: DescribeBatchPredictionsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeBatchPredictionsResponse_httpStatus :: Lens.Lens' DescribeBatchPredictionsResponse Prelude.Int
describeBatchPredictionsResponse_httpStatus = Lens.lens (\DescribeBatchPredictionsResponse' {httpStatus} -> httpStatus) (\s@DescribeBatchPredictionsResponse' {} a -> s {httpStatus = a} :: DescribeBatchPredictionsResponse)

instance
  Prelude.NFData
    DescribeBatchPredictionsResponse

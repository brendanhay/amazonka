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
-- Module      : Amazonka.MachineLearning.DescribeMLModels
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @MLModel@ that match the search criteria in the
-- request.
--
-- This operation returns paginated results.
module Amazonka.MachineLearning.DescribeMLModels
  ( -- * Creating a Request
    DescribeMLModels (..),
    newDescribeMLModels,

    -- * Request Lenses
    describeMLModels_eq,
    describeMLModels_filterVariable,
    describeMLModels_ge,
    describeMLModels_gt,
    describeMLModels_le,
    describeMLModels_lt,
    describeMLModels_limit,
    describeMLModels_ne,
    describeMLModels_nextToken,
    describeMLModels_prefix,
    describeMLModels_sortOrder,

    -- * Destructuring the Response
    DescribeMLModelsResponse (..),
    newDescribeMLModelsResponse,

    -- * Response Lenses
    describeMLModelsResponse_nextToken,
    describeMLModelsResponse_results,
    describeMLModelsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MachineLearning.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeMLModels' smart constructor.
data DescribeMLModels = DescribeMLModels'
  { -- | The equal to operator. The @MLModel@ results will have @FilterVariable@
    -- values that exactly match the value specified with @EQ@.
    eq :: Prelude.Maybe Prelude.Text,
    -- | Use one of the following variables to filter a list of @MLModel@:
    --
    -- -   @CreatedAt@ - Sets the search criteria to @MLModel@ creation date.
    --
    -- -   @Status@ - Sets the search criteria to @MLModel@ status.
    --
    -- -   @Name@ - Sets the search criteria to the contents of @MLModel@ ____
    --     @Name@.
    --
    -- -   @IAMUser@ - Sets the search criteria to the user account that
    --     invoked the @MLModel@ creation.
    --
    -- -   @TrainingDataSourceId@ - Sets the search criteria to the
    --     @DataSource@ used to train one or more @MLModel@.
    --
    -- -   @RealtimeEndpointStatus@ - Sets the search criteria to the @MLModel@
    --     real-time endpoint status.
    --
    -- -   @MLModelType@ - Sets the search criteria to @MLModel@ type: binary,
    --     regression, or multi-class.
    --
    -- -   @Algorithm@ - Sets the search criteria to the algorithm that the
    --     @MLModel@ uses.
    --
    -- -   @TrainingDataURI@ - Sets the search criteria to the data file(s)
    --     used in training a @MLModel@. The URL can identify either a file or
    --     an Amazon Simple Storage Service (Amazon S3) bucket or directory.
    filterVariable :: Prelude.Maybe MLModelFilterVariable,
    -- | The greater than or equal to operator. The @MLModel@ results will have
    -- @FilterVariable@ values that are greater than or equal to the value
    -- specified with @GE@.
    ge :: Prelude.Maybe Prelude.Text,
    -- | The greater than operator. The @MLModel@ results will have
    -- @FilterVariable@ values that are greater than the value specified with
    -- @GT@.
    gt :: Prelude.Maybe Prelude.Text,
    -- | The less than or equal to operator. The @MLModel@ results will have
    -- @FilterVariable@ values that are less than or equal to the value
    -- specified with @LE@.
    le :: Prelude.Maybe Prelude.Text,
    -- | The less than operator. The @MLModel@ results will have @FilterVariable@
    -- values that are less than the value specified with @LT@.
    lt :: Prelude.Maybe Prelude.Text,
    -- | The number of pages of information to include in the result. The range
    -- of acceptable values is @1@ through @100@. The default value is @100@.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The not equal to operator. The @MLModel@ results will have
    -- @FilterVariable@ values not equal to the value specified with @NE@.
    ne :: Prelude.Maybe Prelude.Text,
    -- | The ID of the page in the paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A string that is found at the beginning of a variable, such as @Name@ or
    -- @Id@.
    --
    -- For example, an @MLModel@ could have the @Name@
    -- @2014-09-09-HolidayGiftMailer@. To search for this @MLModel@, select
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
    -- of @MLModel@.
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
-- Create a value of 'DescribeMLModels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eq', 'describeMLModels_eq' - The equal to operator. The @MLModel@ results will have @FilterVariable@
-- values that exactly match the value specified with @EQ@.
--
-- 'filterVariable', 'describeMLModels_filterVariable' - Use one of the following variables to filter a list of @MLModel@:
--
-- -   @CreatedAt@ - Sets the search criteria to @MLModel@ creation date.
--
-- -   @Status@ - Sets the search criteria to @MLModel@ status.
--
-- -   @Name@ - Sets the search criteria to the contents of @MLModel@ ____
--     @Name@.
--
-- -   @IAMUser@ - Sets the search criteria to the user account that
--     invoked the @MLModel@ creation.
--
-- -   @TrainingDataSourceId@ - Sets the search criteria to the
--     @DataSource@ used to train one or more @MLModel@.
--
-- -   @RealtimeEndpointStatus@ - Sets the search criteria to the @MLModel@
--     real-time endpoint status.
--
-- -   @MLModelType@ - Sets the search criteria to @MLModel@ type: binary,
--     regression, or multi-class.
--
-- -   @Algorithm@ - Sets the search criteria to the algorithm that the
--     @MLModel@ uses.
--
-- -   @TrainingDataURI@ - Sets the search criteria to the data file(s)
--     used in training a @MLModel@. The URL can identify either a file or
--     an Amazon Simple Storage Service (Amazon S3) bucket or directory.
--
-- 'ge', 'describeMLModels_ge' - The greater than or equal to operator. The @MLModel@ results will have
-- @FilterVariable@ values that are greater than or equal to the value
-- specified with @GE@.
--
-- 'gt', 'describeMLModels_gt' - The greater than operator. The @MLModel@ results will have
-- @FilterVariable@ values that are greater than the value specified with
-- @GT@.
--
-- 'le', 'describeMLModels_le' - The less than or equal to operator. The @MLModel@ results will have
-- @FilterVariable@ values that are less than or equal to the value
-- specified with @LE@.
--
-- 'lt', 'describeMLModels_lt' - The less than operator. The @MLModel@ results will have @FilterVariable@
-- values that are less than the value specified with @LT@.
--
-- 'limit', 'describeMLModels_limit' - The number of pages of information to include in the result. The range
-- of acceptable values is @1@ through @100@. The default value is @100@.
--
-- 'ne', 'describeMLModels_ne' - The not equal to operator. The @MLModel@ results will have
-- @FilterVariable@ values not equal to the value specified with @NE@.
--
-- 'nextToken', 'describeMLModels_nextToken' - The ID of the page in the paginated results.
--
-- 'prefix', 'describeMLModels_prefix' - A string that is found at the beginning of a variable, such as @Name@ or
-- @Id@.
--
-- For example, an @MLModel@ could have the @Name@
-- @2014-09-09-HolidayGiftMailer@. To search for this @MLModel@, select
-- @Name@ for the @FilterVariable@ and any of the following strings for the
-- @Prefix@:
--
-- -   2014-09
--
-- -   2014-09-09
--
-- -   2014-09-09-Holiday
--
-- 'sortOrder', 'describeMLModels_sortOrder' - A two-value parameter that determines the sequence of the resulting list
-- of @MLModel@.
--
-- -   @asc@ - Arranges the list in ascending order (A-Z, 0-9).
--
-- -   @dsc@ - Arranges the list in descending order (Z-A, 9-0).
--
-- Results are sorted by @FilterVariable@.
newDescribeMLModels ::
  DescribeMLModels
newDescribeMLModels =
  DescribeMLModels'
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

-- | The equal to operator. The @MLModel@ results will have @FilterVariable@
-- values that exactly match the value specified with @EQ@.
describeMLModels_eq :: Lens.Lens' DescribeMLModels (Prelude.Maybe Prelude.Text)
describeMLModels_eq = Lens.lens (\DescribeMLModels' {eq} -> eq) (\s@DescribeMLModels' {} a -> s {eq = a} :: DescribeMLModels)

-- | Use one of the following variables to filter a list of @MLModel@:
--
-- -   @CreatedAt@ - Sets the search criteria to @MLModel@ creation date.
--
-- -   @Status@ - Sets the search criteria to @MLModel@ status.
--
-- -   @Name@ - Sets the search criteria to the contents of @MLModel@ ____
--     @Name@.
--
-- -   @IAMUser@ - Sets the search criteria to the user account that
--     invoked the @MLModel@ creation.
--
-- -   @TrainingDataSourceId@ - Sets the search criteria to the
--     @DataSource@ used to train one or more @MLModel@.
--
-- -   @RealtimeEndpointStatus@ - Sets the search criteria to the @MLModel@
--     real-time endpoint status.
--
-- -   @MLModelType@ - Sets the search criteria to @MLModel@ type: binary,
--     regression, or multi-class.
--
-- -   @Algorithm@ - Sets the search criteria to the algorithm that the
--     @MLModel@ uses.
--
-- -   @TrainingDataURI@ - Sets the search criteria to the data file(s)
--     used in training a @MLModel@. The URL can identify either a file or
--     an Amazon Simple Storage Service (Amazon S3) bucket or directory.
describeMLModels_filterVariable :: Lens.Lens' DescribeMLModels (Prelude.Maybe MLModelFilterVariable)
describeMLModels_filterVariable = Lens.lens (\DescribeMLModels' {filterVariable} -> filterVariable) (\s@DescribeMLModels' {} a -> s {filterVariable = a} :: DescribeMLModels)

-- | The greater than or equal to operator. The @MLModel@ results will have
-- @FilterVariable@ values that are greater than or equal to the value
-- specified with @GE@.
describeMLModels_ge :: Lens.Lens' DescribeMLModels (Prelude.Maybe Prelude.Text)
describeMLModels_ge = Lens.lens (\DescribeMLModels' {ge} -> ge) (\s@DescribeMLModels' {} a -> s {ge = a} :: DescribeMLModels)

-- | The greater than operator. The @MLModel@ results will have
-- @FilterVariable@ values that are greater than the value specified with
-- @GT@.
describeMLModels_gt :: Lens.Lens' DescribeMLModels (Prelude.Maybe Prelude.Text)
describeMLModels_gt = Lens.lens (\DescribeMLModels' {gt} -> gt) (\s@DescribeMLModels' {} a -> s {gt = a} :: DescribeMLModels)

-- | The less than or equal to operator. The @MLModel@ results will have
-- @FilterVariable@ values that are less than or equal to the value
-- specified with @LE@.
describeMLModels_le :: Lens.Lens' DescribeMLModels (Prelude.Maybe Prelude.Text)
describeMLModels_le = Lens.lens (\DescribeMLModels' {le} -> le) (\s@DescribeMLModels' {} a -> s {le = a} :: DescribeMLModels)

-- | The less than operator. The @MLModel@ results will have @FilterVariable@
-- values that are less than the value specified with @LT@.
describeMLModels_lt :: Lens.Lens' DescribeMLModels (Prelude.Maybe Prelude.Text)
describeMLModels_lt = Lens.lens (\DescribeMLModels' {lt} -> lt) (\s@DescribeMLModels' {} a -> s {lt = a} :: DescribeMLModels)

-- | The number of pages of information to include in the result. The range
-- of acceptable values is @1@ through @100@. The default value is @100@.
describeMLModels_limit :: Lens.Lens' DescribeMLModels (Prelude.Maybe Prelude.Natural)
describeMLModels_limit = Lens.lens (\DescribeMLModels' {limit} -> limit) (\s@DescribeMLModels' {} a -> s {limit = a} :: DescribeMLModels)

-- | The not equal to operator. The @MLModel@ results will have
-- @FilterVariable@ values not equal to the value specified with @NE@.
describeMLModels_ne :: Lens.Lens' DescribeMLModels (Prelude.Maybe Prelude.Text)
describeMLModels_ne = Lens.lens (\DescribeMLModels' {ne} -> ne) (\s@DescribeMLModels' {} a -> s {ne = a} :: DescribeMLModels)

-- | The ID of the page in the paginated results.
describeMLModels_nextToken :: Lens.Lens' DescribeMLModels (Prelude.Maybe Prelude.Text)
describeMLModels_nextToken = Lens.lens (\DescribeMLModels' {nextToken} -> nextToken) (\s@DescribeMLModels' {} a -> s {nextToken = a} :: DescribeMLModels)

-- | A string that is found at the beginning of a variable, such as @Name@ or
-- @Id@.
--
-- For example, an @MLModel@ could have the @Name@
-- @2014-09-09-HolidayGiftMailer@. To search for this @MLModel@, select
-- @Name@ for the @FilterVariable@ and any of the following strings for the
-- @Prefix@:
--
-- -   2014-09
--
-- -   2014-09-09
--
-- -   2014-09-09-Holiday
describeMLModels_prefix :: Lens.Lens' DescribeMLModels (Prelude.Maybe Prelude.Text)
describeMLModels_prefix = Lens.lens (\DescribeMLModels' {prefix} -> prefix) (\s@DescribeMLModels' {} a -> s {prefix = a} :: DescribeMLModels)

-- | A two-value parameter that determines the sequence of the resulting list
-- of @MLModel@.
--
-- -   @asc@ - Arranges the list in ascending order (A-Z, 0-9).
--
-- -   @dsc@ - Arranges the list in descending order (Z-A, 9-0).
--
-- Results are sorted by @FilterVariable@.
describeMLModels_sortOrder :: Lens.Lens' DescribeMLModels (Prelude.Maybe SortOrder)
describeMLModels_sortOrder = Lens.lens (\DescribeMLModels' {sortOrder} -> sortOrder) (\s@DescribeMLModels' {} a -> s {sortOrder = a} :: DescribeMLModels)

instance Core.AWSPager DescribeMLModels where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeMLModelsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeMLModelsResponse_results
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeMLModels_nextToken
          Lens..~ rs
          Lens.^? describeMLModelsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeMLModels where
  type
    AWSResponse DescribeMLModels =
      DescribeMLModelsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMLModelsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Results" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeMLModels where
  hashWithSalt _salt DescribeMLModels' {..} =
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

instance Prelude.NFData DescribeMLModels where
  rnf DescribeMLModels' {..} =
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

instance Data.ToHeaders DescribeMLModels where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonML_20141212.DescribeMLModels" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeMLModels where
  toJSON DescribeMLModels' {..} =
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

instance Data.ToPath DescribeMLModels where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeMLModels where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @DescribeMLModels@ operation. The content is
-- essentially a list of @MLModel@.
--
-- /See:/ 'newDescribeMLModelsResponse' smart constructor.
data DescribeMLModelsResponse = DescribeMLModelsResponse'
  { -- | The ID of the next page in the paginated results that indicates at least
    -- one more page follows.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of @MLModel@ that meet the search criteria.
    results :: Prelude.Maybe [MLModel],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMLModelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeMLModelsResponse_nextToken' - The ID of the next page in the paginated results that indicates at least
-- one more page follows.
--
-- 'results', 'describeMLModelsResponse_results' - A list of @MLModel@ that meet the search criteria.
--
-- 'httpStatus', 'describeMLModelsResponse_httpStatus' - The response's http status code.
newDescribeMLModelsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeMLModelsResponse
newDescribeMLModelsResponse pHttpStatus_ =
  DescribeMLModelsResponse'
    { nextToken =
        Prelude.Nothing,
      results = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the next page in the paginated results that indicates at least
-- one more page follows.
describeMLModelsResponse_nextToken :: Lens.Lens' DescribeMLModelsResponse (Prelude.Maybe Prelude.Text)
describeMLModelsResponse_nextToken = Lens.lens (\DescribeMLModelsResponse' {nextToken} -> nextToken) (\s@DescribeMLModelsResponse' {} a -> s {nextToken = a} :: DescribeMLModelsResponse)

-- | A list of @MLModel@ that meet the search criteria.
describeMLModelsResponse_results :: Lens.Lens' DescribeMLModelsResponse (Prelude.Maybe [MLModel])
describeMLModelsResponse_results = Lens.lens (\DescribeMLModelsResponse' {results} -> results) (\s@DescribeMLModelsResponse' {} a -> s {results = a} :: DescribeMLModelsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeMLModelsResponse_httpStatus :: Lens.Lens' DescribeMLModelsResponse Prelude.Int
describeMLModelsResponse_httpStatus = Lens.lens (\DescribeMLModelsResponse' {httpStatus} -> httpStatus) (\s@DescribeMLModelsResponse' {} a -> s {httpStatus = a} :: DescribeMLModelsResponse)

instance Prelude.NFData DescribeMLModelsResponse where
  rnf DescribeMLModelsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf results
      `Prelude.seq` Prelude.rnf httpStatus

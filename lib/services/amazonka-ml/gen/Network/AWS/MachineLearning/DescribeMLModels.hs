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
-- Module      : Network.AWS.MachineLearning.DescribeMLModels
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @MLModel@ that match the search criteria in the
-- request.
--
-- This operation returns paginated results.
module Network.AWS.MachineLearning.DescribeMLModels
  ( -- * Creating a Request
    DescribeMLModels (..),
    newDescribeMLModels,

    -- * Request Lenses
    describeMLModels_eq,
    describeMLModels_ge,
    describeMLModels_prefix,
    describeMLModels_gt,
    describeMLModels_ne,
    describeMLModels_nextToken,
    describeMLModels_sortOrder,
    describeMLModels_limit,
    describeMLModels_lt,
    describeMLModels_filterVariable,
    describeMLModels_le,

    -- * Destructuring the Response
    DescribeMLModelsResponse (..),
    newDescribeMLModelsResponse,

    -- * Response Lenses
    describeMLModelsResponse_results,
    describeMLModelsResponse_nextToken,
    describeMLModelsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeMLModels' smart constructor.
data DescribeMLModels = DescribeMLModels'
  { -- | The equal to operator. The @MLModel@ results will have @FilterVariable@
    -- values that exactly match the value specified with @EQ@.
    eq :: Prelude.Maybe Prelude.Text,
    -- | The greater than or equal to operator. The @MLModel@ results will have
    -- @FilterVariable@ values that are greater than or equal to the value
    -- specified with @GE@.
    ge :: Prelude.Maybe Prelude.Text,
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
    -- | The greater than operator. The @MLModel@ results will have
    -- @FilterVariable@ values that are greater than the value specified with
    -- @GT@.
    gt :: Prelude.Maybe Prelude.Text,
    -- | The not equal to operator. The @MLModel@ results will have
    -- @FilterVariable@ values not equal to the value specified with @NE@.
    ne :: Prelude.Maybe Prelude.Text,
    -- | The ID of the page in the paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A two-value parameter that determines the sequence of the resulting list
    -- of @MLModel@.
    --
    -- -   @asc@ - Arranges the list in ascending order (A-Z, 0-9).
    --
    -- -   @dsc@ - Arranges the list in descending order (Z-A, 9-0).
    --
    -- Results are sorted by @FilterVariable@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | The number of pages of information to include in the result. The range
    -- of acceptable values is @1@ through @100@. The default value is @100@.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The less than operator. The @MLModel@ results will have @FilterVariable@
    -- values that are less than the value specified with @LT@.
    lt :: Prelude.Maybe Prelude.Text,
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
    -- | The less than or equal to operator. The @MLModel@ results will have
    -- @FilterVariable@ values that are less than or equal to the value
    -- specified with @LE@.
    le :: Prelude.Maybe Prelude.Text
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
-- 'ge', 'describeMLModels_ge' - The greater than or equal to operator. The @MLModel@ results will have
-- @FilterVariable@ values that are greater than or equal to the value
-- specified with @GE@.
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
-- 'gt', 'describeMLModels_gt' - The greater than operator. The @MLModel@ results will have
-- @FilterVariable@ values that are greater than the value specified with
-- @GT@.
--
-- 'ne', 'describeMLModels_ne' - The not equal to operator. The @MLModel@ results will have
-- @FilterVariable@ values not equal to the value specified with @NE@.
--
-- 'nextToken', 'describeMLModels_nextToken' - The ID of the page in the paginated results.
--
-- 'sortOrder', 'describeMLModels_sortOrder' - A two-value parameter that determines the sequence of the resulting list
-- of @MLModel@.
--
-- -   @asc@ - Arranges the list in ascending order (A-Z, 0-9).
--
-- -   @dsc@ - Arranges the list in descending order (Z-A, 9-0).
--
-- Results are sorted by @FilterVariable@.
--
-- 'limit', 'describeMLModels_limit' - The number of pages of information to include in the result. The range
-- of acceptable values is @1@ through @100@. The default value is @100@.
--
-- 'lt', 'describeMLModels_lt' - The less than operator. The @MLModel@ results will have @FilterVariable@
-- values that are less than the value specified with @LT@.
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
-- 'le', 'describeMLModels_le' - The less than or equal to operator. The @MLModel@ results will have
-- @FilterVariable@ values that are less than or equal to the value
-- specified with @LE@.
newDescribeMLModels ::
  DescribeMLModels
newDescribeMLModels =
  DescribeMLModels'
    { eq = Prelude.Nothing,
      ge = Prelude.Nothing,
      prefix = Prelude.Nothing,
      gt = Prelude.Nothing,
      ne = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      limit = Prelude.Nothing,
      lt = Prelude.Nothing,
      filterVariable = Prelude.Nothing,
      le = Prelude.Nothing
    }

-- | The equal to operator. The @MLModel@ results will have @FilterVariable@
-- values that exactly match the value specified with @EQ@.
describeMLModels_eq :: Lens.Lens' DescribeMLModels (Prelude.Maybe Prelude.Text)
describeMLModels_eq = Lens.lens (\DescribeMLModels' {eq} -> eq) (\s@DescribeMLModels' {} a -> s {eq = a} :: DescribeMLModels)

-- | The greater than or equal to operator. The @MLModel@ results will have
-- @FilterVariable@ values that are greater than or equal to the value
-- specified with @GE@.
describeMLModels_ge :: Lens.Lens' DescribeMLModels (Prelude.Maybe Prelude.Text)
describeMLModels_ge = Lens.lens (\DescribeMLModels' {ge} -> ge) (\s@DescribeMLModels' {} a -> s {ge = a} :: DescribeMLModels)

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

-- | The greater than operator. The @MLModel@ results will have
-- @FilterVariable@ values that are greater than the value specified with
-- @GT@.
describeMLModels_gt :: Lens.Lens' DescribeMLModels (Prelude.Maybe Prelude.Text)
describeMLModels_gt = Lens.lens (\DescribeMLModels' {gt} -> gt) (\s@DescribeMLModels' {} a -> s {gt = a} :: DescribeMLModels)

-- | The not equal to operator. The @MLModel@ results will have
-- @FilterVariable@ values not equal to the value specified with @NE@.
describeMLModels_ne :: Lens.Lens' DescribeMLModels (Prelude.Maybe Prelude.Text)
describeMLModels_ne = Lens.lens (\DescribeMLModels' {ne} -> ne) (\s@DescribeMLModels' {} a -> s {ne = a} :: DescribeMLModels)

-- | The ID of the page in the paginated results.
describeMLModels_nextToken :: Lens.Lens' DescribeMLModels (Prelude.Maybe Prelude.Text)
describeMLModels_nextToken = Lens.lens (\DescribeMLModels' {nextToken} -> nextToken) (\s@DescribeMLModels' {} a -> s {nextToken = a} :: DescribeMLModels)

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

-- | The number of pages of information to include in the result. The range
-- of acceptable values is @1@ through @100@. The default value is @100@.
describeMLModels_limit :: Lens.Lens' DescribeMLModels (Prelude.Maybe Prelude.Natural)
describeMLModels_limit = Lens.lens (\DescribeMLModels' {limit} -> limit) (\s@DescribeMLModels' {} a -> s {limit = a} :: DescribeMLModels)

-- | The less than operator. The @MLModel@ results will have @FilterVariable@
-- values that are less than the value specified with @LT@.
describeMLModels_lt :: Lens.Lens' DescribeMLModels (Prelude.Maybe Prelude.Text)
describeMLModels_lt = Lens.lens (\DescribeMLModels' {lt} -> lt) (\s@DescribeMLModels' {} a -> s {lt = a} :: DescribeMLModels)

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

-- | The less than or equal to operator. The @MLModel@ results will have
-- @FilterVariable@ values that are less than or equal to the value
-- specified with @LE@.
describeMLModels_le :: Lens.Lens' DescribeMLModels (Prelude.Maybe Prelude.Text)
describeMLModels_le = Lens.lens (\DescribeMLModels' {le} -> le) (\s@DescribeMLModels' {} a -> s {le = a} :: DescribeMLModels)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMLModelsResponse'
            Prelude.<$> (x Core..?> "Results" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeMLModels

instance Prelude.NFData DescribeMLModels

instance Core.ToHeaders DescribeMLModels where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonML_20141212.DescribeMLModels" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeMLModels where
  toJSON DescribeMLModels' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EQ" Core..=) Prelude.<$> eq,
            ("GE" Core..=) Prelude.<$> ge,
            ("Prefix" Core..=) Prelude.<$> prefix,
            ("GT" Core..=) Prelude.<$> gt,
            ("NE" Core..=) Prelude.<$> ne,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("Limit" Core..=) Prelude.<$> limit,
            ("LT" Core..=) Prelude.<$> lt,
            ("FilterVariable" Core..=)
              Prelude.<$> filterVariable,
            ("LE" Core..=) Prelude.<$> le
          ]
      )

instance Core.ToPath DescribeMLModels where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeMLModels where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @DescribeMLModels@ operation. The content is
-- essentially a list of @MLModel@.
--
-- /See:/ 'newDescribeMLModelsResponse' smart constructor.
data DescribeMLModelsResponse = DescribeMLModelsResponse'
  { -- | A list of @MLModel@ that meet the search criteria.
    results :: Prelude.Maybe [MLModel],
    -- | The ID of the next page in the paginated results that indicates at least
    -- one more page follows.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'results', 'describeMLModelsResponse_results' - A list of @MLModel@ that meet the search criteria.
--
-- 'nextToken', 'describeMLModelsResponse_nextToken' - The ID of the next page in the paginated results that indicates at least
-- one more page follows.
--
-- 'httpStatus', 'describeMLModelsResponse_httpStatus' - The response's http status code.
newDescribeMLModelsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeMLModelsResponse
newDescribeMLModelsResponse pHttpStatus_ =
  DescribeMLModelsResponse'
    { results =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of @MLModel@ that meet the search criteria.
describeMLModelsResponse_results :: Lens.Lens' DescribeMLModelsResponse (Prelude.Maybe [MLModel])
describeMLModelsResponse_results = Lens.lens (\DescribeMLModelsResponse' {results} -> results) (\s@DescribeMLModelsResponse' {} a -> s {results = a} :: DescribeMLModelsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the next page in the paginated results that indicates at least
-- one more page follows.
describeMLModelsResponse_nextToken :: Lens.Lens' DescribeMLModelsResponse (Prelude.Maybe Prelude.Text)
describeMLModelsResponse_nextToken = Lens.lens (\DescribeMLModelsResponse' {nextToken} -> nextToken) (\s@DescribeMLModelsResponse' {} a -> s {nextToken = a} :: DescribeMLModelsResponse)

-- | The response's http status code.
describeMLModelsResponse_httpStatus :: Lens.Lens' DescribeMLModelsResponse Prelude.Int
describeMLModelsResponse_httpStatus = Lens.lens (\DescribeMLModelsResponse' {httpStatus} -> httpStatus) (\s@DescribeMLModelsResponse' {} a -> s {httpStatus = a} :: DescribeMLModelsResponse)

instance Prelude.NFData DescribeMLModelsResponse

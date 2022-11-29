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
-- Module      : Amazonka.Batch.DescribeComputeEnvironments
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your compute environments.
--
-- If you\'re using an unmanaged compute environment, you can use the
-- @DescribeComputeEnvironment@ operation to determine the @ecsClusterArn@
-- that you launch your Amazon ECS container instances into.
--
-- This operation returns paginated results.
module Amazonka.Batch.DescribeComputeEnvironments
  ( -- * Creating a Request
    DescribeComputeEnvironments (..),
    newDescribeComputeEnvironments,

    -- * Request Lenses
    describeComputeEnvironments_nextToken,
    describeComputeEnvironments_computeEnvironments,
    describeComputeEnvironments_maxResults,

    -- * Destructuring the Response
    DescribeComputeEnvironmentsResponse (..),
    newDescribeComputeEnvironmentsResponse,

    -- * Response Lenses
    describeComputeEnvironmentsResponse_nextToken,
    describeComputeEnvironmentsResponse_computeEnvironments,
    describeComputeEnvironmentsResponse_httpStatus,
  )
where

import Amazonka.Batch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for @DescribeComputeEnvironments@.
--
-- /See:/ 'newDescribeComputeEnvironments' smart constructor.
data DescribeComputeEnvironments = DescribeComputeEnvironments'
  { -- | The @nextToken@ value returned from a previous paginated
    -- @DescribeComputeEnvironments@ request where @maxResults@ was used and
    -- the results exceeded the value of that parameter. Pagination continues
    -- from the end of the previous results that returned the @nextToken@
    -- value. This value is @null@ when there are no more results to return.
    --
    -- Treat this token as an opaque identifier that\'s only used to retrieve
    -- the next items in a list and not for other programmatic purposes.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of up to 100 compute environment names or full Amazon Resource
    -- Name (ARN) entries.
    computeEnvironments :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of cluster results returned by
    -- @DescribeComputeEnvironments@ in paginated output. When this parameter
    -- is used, @DescribeComputeEnvironments@ only returns @maxResults@ results
    -- in a single page along with a @nextToken@ response element. The
    -- remaining results of the initial request can be seen by sending another
    -- @DescribeComputeEnvironments@ request with the returned @nextToken@
    -- value. This value can be between 1 and 100. If this parameter isn\'t
    -- used, then @DescribeComputeEnvironments@ returns up to 100 results and a
    -- @nextToken@ value if applicable.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeComputeEnvironments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeComputeEnvironments_nextToken' - The @nextToken@ value returned from a previous paginated
-- @DescribeComputeEnvironments@ request where @maxResults@ was used and
-- the results exceeded the value of that parameter. Pagination continues
-- from the end of the previous results that returned the @nextToken@
-- value. This value is @null@ when there are no more results to return.
--
-- Treat this token as an opaque identifier that\'s only used to retrieve
-- the next items in a list and not for other programmatic purposes.
--
-- 'computeEnvironments', 'describeComputeEnvironments_computeEnvironments' - A list of up to 100 compute environment names or full Amazon Resource
-- Name (ARN) entries.
--
-- 'maxResults', 'describeComputeEnvironments_maxResults' - The maximum number of cluster results returned by
-- @DescribeComputeEnvironments@ in paginated output. When this parameter
-- is used, @DescribeComputeEnvironments@ only returns @maxResults@ results
-- in a single page along with a @nextToken@ response element. The
-- remaining results of the initial request can be seen by sending another
-- @DescribeComputeEnvironments@ request with the returned @nextToken@
-- value. This value can be between 1 and 100. If this parameter isn\'t
-- used, then @DescribeComputeEnvironments@ returns up to 100 results and a
-- @nextToken@ value if applicable.
newDescribeComputeEnvironments ::
  DescribeComputeEnvironments
newDescribeComputeEnvironments =
  DescribeComputeEnvironments'
    { nextToken =
        Prelude.Nothing,
      computeEnvironments = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The @nextToken@ value returned from a previous paginated
-- @DescribeComputeEnvironments@ request where @maxResults@ was used and
-- the results exceeded the value of that parameter. Pagination continues
-- from the end of the previous results that returned the @nextToken@
-- value. This value is @null@ when there are no more results to return.
--
-- Treat this token as an opaque identifier that\'s only used to retrieve
-- the next items in a list and not for other programmatic purposes.
describeComputeEnvironments_nextToken :: Lens.Lens' DescribeComputeEnvironments (Prelude.Maybe Prelude.Text)
describeComputeEnvironments_nextToken = Lens.lens (\DescribeComputeEnvironments' {nextToken} -> nextToken) (\s@DescribeComputeEnvironments' {} a -> s {nextToken = a} :: DescribeComputeEnvironments)

-- | A list of up to 100 compute environment names or full Amazon Resource
-- Name (ARN) entries.
describeComputeEnvironments_computeEnvironments :: Lens.Lens' DescribeComputeEnvironments (Prelude.Maybe [Prelude.Text])
describeComputeEnvironments_computeEnvironments = Lens.lens (\DescribeComputeEnvironments' {computeEnvironments} -> computeEnvironments) (\s@DescribeComputeEnvironments' {} a -> s {computeEnvironments = a} :: DescribeComputeEnvironments) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of cluster results returned by
-- @DescribeComputeEnvironments@ in paginated output. When this parameter
-- is used, @DescribeComputeEnvironments@ only returns @maxResults@ results
-- in a single page along with a @nextToken@ response element. The
-- remaining results of the initial request can be seen by sending another
-- @DescribeComputeEnvironments@ request with the returned @nextToken@
-- value. This value can be between 1 and 100. If this parameter isn\'t
-- used, then @DescribeComputeEnvironments@ returns up to 100 results and a
-- @nextToken@ value if applicable.
describeComputeEnvironments_maxResults :: Lens.Lens' DescribeComputeEnvironments (Prelude.Maybe Prelude.Int)
describeComputeEnvironments_maxResults = Lens.lens (\DescribeComputeEnvironments' {maxResults} -> maxResults) (\s@DescribeComputeEnvironments' {} a -> s {maxResults = a} :: DescribeComputeEnvironments)

instance Core.AWSPager DescribeComputeEnvironments where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeComputeEnvironmentsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeComputeEnvironmentsResponse_computeEnvironments
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeComputeEnvironments_nextToken
          Lens..~ rs
          Lens.^? describeComputeEnvironmentsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeComputeEnvironments where
  type
    AWSResponse DescribeComputeEnvironments =
      DescribeComputeEnvironmentsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeComputeEnvironmentsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "computeEnvironments"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeComputeEnvironments where
  hashWithSalt _salt DescribeComputeEnvironments' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` computeEnvironments
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeComputeEnvironments where
  rnf DescribeComputeEnvironments' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf computeEnvironments
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders DescribeComputeEnvironments where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeComputeEnvironments where
  toJSON DescribeComputeEnvironments' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("computeEnvironments" Core..=)
              Prelude.<$> computeEnvironments,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath DescribeComputeEnvironments where
  toPath =
    Prelude.const "/v1/describecomputeenvironments"

instance Core.ToQuery DescribeComputeEnvironments where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeComputeEnvironmentsResponse' smart constructor.
data DescribeComputeEnvironmentsResponse = DescribeComputeEnvironmentsResponse'
  { -- | The @nextToken@ value to include in a future
    -- @DescribeComputeEnvironments@ request. When the results of a
    -- @DescribeComputeEnvironments@ request exceed @maxResults@, this value
    -- can be used to retrieve the next page of results. This value is @null@
    -- when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of compute environments.
    computeEnvironments :: Prelude.Maybe [ComputeEnvironmentDetail],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeComputeEnvironmentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeComputeEnvironmentsResponse_nextToken' - The @nextToken@ value to include in a future
-- @DescribeComputeEnvironments@ request. When the results of a
-- @DescribeComputeEnvironments@ request exceed @maxResults@, this value
-- can be used to retrieve the next page of results. This value is @null@
-- when there are no more results to return.
--
-- 'computeEnvironments', 'describeComputeEnvironmentsResponse_computeEnvironments' - The list of compute environments.
--
-- 'httpStatus', 'describeComputeEnvironmentsResponse_httpStatus' - The response's http status code.
newDescribeComputeEnvironmentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeComputeEnvironmentsResponse
newDescribeComputeEnvironmentsResponse pHttpStatus_ =
  DescribeComputeEnvironmentsResponse'
    { nextToken =
        Prelude.Nothing,
      computeEnvironments = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ value to include in a future
-- @DescribeComputeEnvironments@ request. When the results of a
-- @DescribeComputeEnvironments@ request exceed @maxResults@, this value
-- can be used to retrieve the next page of results. This value is @null@
-- when there are no more results to return.
describeComputeEnvironmentsResponse_nextToken :: Lens.Lens' DescribeComputeEnvironmentsResponse (Prelude.Maybe Prelude.Text)
describeComputeEnvironmentsResponse_nextToken = Lens.lens (\DescribeComputeEnvironmentsResponse' {nextToken} -> nextToken) (\s@DescribeComputeEnvironmentsResponse' {} a -> s {nextToken = a} :: DescribeComputeEnvironmentsResponse)

-- | The list of compute environments.
describeComputeEnvironmentsResponse_computeEnvironments :: Lens.Lens' DescribeComputeEnvironmentsResponse (Prelude.Maybe [ComputeEnvironmentDetail])
describeComputeEnvironmentsResponse_computeEnvironments = Lens.lens (\DescribeComputeEnvironmentsResponse' {computeEnvironments} -> computeEnvironments) (\s@DescribeComputeEnvironmentsResponse' {} a -> s {computeEnvironments = a} :: DescribeComputeEnvironmentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeComputeEnvironmentsResponse_httpStatus :: Lens.Lens' DescribeComputeEnvironmentsResponse Prelude.Int
describeComputeEnvironmentsResponse_httpStatus = Lens.lens (\DescribeComputeEnvironmentsResponse' {httpStatus} -> httpStatus) (\s@DescribeComputeEnvironmentsResponse' {} a -> s {httpStatus = a} :: DescribeComputeEnvironmentsResponse)

instance
  Prelude.NFData
    DescribeComputeEnvironmentsResponse
  where
  rnf DescribeComputeEnvironmentsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf computeEnvironments
      `Prelude.seq` Prelude.rnf httpStatus

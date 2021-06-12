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
-- Module      : Network.AWS.Batch.DescribeComputeEnvironments
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your compute environments.
--
-- If you\'re using an unmanaged compute environment, you can use the
-- @DescribeComputeEnvironment@ operation to determine the @ecsClusterArn@
-- that you should launch your Amazon ECS container instances into.
--
-- This operation returns paginated results.
module Network.AWS.Batch.DescribeComputeEnvironments
  ( -- * Creating a Request
    DescribeComputeEnvironments (..),
    newDescribeComputeEnvironments,

    -- * Request Lenses
    describeComputeEnvironments_nextToken,
    describeComputeEnvironments_maxResults,
    describeComputeEnvironments_computeEnvironments,

    -- * Destructuring the Response
    DescribeComputeEnvironmentsResponse (..),
    newDescribeComputeEnvironmentsResponse,

    -- * Response Lenses
    describeComputeEnvironmentsResponse_nextToken,
    describeComputeEnvironmentsResponse_computeEnvironments,
    describeComputeEnvironmentsResponse_httpStatus,
  )
where

import Network.AWS.Batch.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
    -- This token should be treated as an opaque identifier that\'s only used
    -- to retrieve the next items in a list and not for other programmatic
    -- purposes.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of cluster results returned by
    -- @DescribeComputeEnvironments@ in paginated output. When this parameter
    -- is used, @DescribeComputeEnvironments@ only returns @maxResults@ results
    -- in a single page along with a @nextToken@ response element. The
    -- remaining results of the initial request can be seen by sending another
    -- @DescribeComputeEnvironments@ request with the returned @nextToken@
    -- value. This value can be between 1 and 100. If this parameter isn\'t
    -- used, then @DescribeComputeEnvironments@ returns up to 100 results and a
    -- @nextToken@ value if applicable.
    maxResults :: Core.Maybe Core.Int,
    -- | A list of up to 100 compute environment names or full Amazon Resource
    -- Name (ARN) entries.
    computeEnvironments :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- This token should be treated as an opaque identifier that\'s only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
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
--
-- 'computeEnvironments', 'describeComputeEnvironments_computeEnvironments' - A list of up to 100 compute environment names or full Amazon Resource
-- Name (ARN) entries.
newDescribeComputeEnvironments ::
  DescribeComputeEnvironments
newDescribeComputeEnvironments =
  DescribeComputeEnvironments'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      computeEnvironments = Core.Nothing
    }

-- | The @nextToken@ value returned from a previous paginated
-- @DescribeComputeEnvironments@ request where @maxResults@ was used and
-- the results exceeded the value of that parameter. Pagination continues
-- from the end of the previous results that returned the @nextToken@
-- value. This value is @null@ when there are no more results to return.
--
-- This token should be treated as an opaque identifier that\'s only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
describeComputeEnvironments_nextToken :: Lens.Lens' DescribeComputeEnvironments (Core.Maybe Core.Text)
describeComputeEnvironments_nextToken = Lens.lens (\DescribeComputeEnvironments' {nextToken} -> nextToken) (\s@DescribeComputeEnvironments' {} a -> s {nextToken = a} :: DescribeComputeEnvironments)

-- | The maximum number of cluster results returned by
-- @DescribeComputeEnvironments@ in paginated output. When this parameter
-- is used, @DescribeComputeEnvironments@ only returns @maxResults@ results
-- in a single page along with a @nextToken@ response element. The
-- remaining results of the initial request can be seen by sending another
-- @DescribeComputeEnvironments@ request with the returned @nextToken@
-- value. This value can be between 1 and 100. If this parameter isn\'t
-- used, then @DescribeComputeEnvironments@ returns up to 100 results and a
-- @nextToken@ value if applicable.
describeComputeEnvironments_maxResults :: Lens.Lens' DescribeComputeEnvironments (Core.Maybe Core.Int)
describeComputeEnvironments_maxResults = Lens.lens (\DescribeComputeEnvironments' {maxResults} -> maxResults) (\s@DescribeComputeEnvironments' {} a -> s {maxResults = a} :: DescribeComputeEnvironments)

-- | A list of up to 100 compute environment names or full Amazon Resource
-- Name (ARN) entries.
describeComputeEnvironments_computeEnvironments :: Lens.Lens' DescribeComputeEnvironments (Core.Maybe [Core.Text])
describeComputeEnvironments_computeEnvironments = Lens.lens (\DescribeComputeEnvironments' {computeEnvironments} -> computeEnvironments) (\s@DescribeComputeEnvironments' {} a -> s {computeEnvironments = a} :: DescribeComputeEnvironments) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeComputeEnvironments where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeComputeEnvironmentsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeComputeEnvironmentsResponse_computeEnvironments
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeComputeEnvironments_nextToken
          Lens..~ rs
          Lens.^? describeComputeEnvironmentsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeComputeEnvironments where
  type
    AWSResponse DescribeComputeEnvironments =
      DescribeComputeEnvironmentsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeComputeEnvironmentsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> ( x Core..?> "computeEnvironments"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeComputeEnvironments

instance Core.NFData DescribeComputeEnvironments

instance Core.ToHeaders DescribeComputeEnvironments where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeComputeEnvironments where
  toJSON DescribeComputeEnvironments' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("computeEnvironments" Core..=)
              Core.<$> computeEnvironments
          ]
      )

instance Core.ToPath DescribeComputeEnvironments where
  toPath = Core.const "/v1/describecomputeenvironments"

instance Core.ToQuery DescribeComputeEnvironments where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeComputeEnvironmentsResponse' smart constructor.
data DescribeComputeEnvironmentsResponse = DescribeComputeEnvironmentsResponse'
  { -- | The @nextToken@ value to include in a future
    -- @DescribeComputeEnvironments@ request. When the results of a
    -- @DescribeJobDefinitions@ request exceed @maxResults@, this value can be
    -- used to retrieve the next page of results. This value is @null@ when
    -- there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of compute environments.
    computeEnvironments :: Core.Maybe [ComputeEnvironmentDetail],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- @DescribeJobDefinitions@ request exceed @maxResults@, this value can be
-- used to retrieve the next page of results. This value is @null@ when
-- there are no more results to return.
--
-- 'computeEnvironments', 'describeComputeEnvironmentsResponse_computeEnvironments' - The list of compute environments.
--
-- 'httpStatus', 'describeComputeEnvironmentsResponse_httpStatus' - The response's http status code.
newDescribeComputeEnvironmentsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeComputeEnvironmentsResponse
newDescribeComputeEnvironmentsResponse pHttpStatus_ =
  DescribeComputeEnvironmentsResponse'
    { nextToken =
        Core.Nothing,
      computeEnvironments = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ value to include in a future
-- @DescribeComputeEnvironments@ request. When the results of a
-- @DescribeJobDefinitions@ request exceed @maxResults@, this value can be
-- used to retrieve the next page of results. This value is @null@ when
-- there are no more results to return.
describeComputeEnvironmentsResponse_nextToken :: Lens.Lens' DescribeComputeEnvironmentsResponse (Core.Maybe Core.Text)
describeComputeEnvironmentsResponse_nextToken = Lens.lens (\DescribeComputeEnvironmentsResponse' {nextToken} -> nextToken) (\s@DescribeComputeEnvironmentsResponse' {} a -> s {nextToken = a} :: DescribeComputeEnvironmentsResponse)

-- | The list of compute environments.
describeComputeEnvironmentsResponse_computeEnvironments :: Lens.Lens' DescribeComputeEnvironmentsResponse (Core.Maybe [ComputeEnvironmentDetail])
describeComputeEnvironmentsResponse_computeEnvironments = Lens.lens (\DescribeComputeEnvironmentsResponse' {computeEnvironments} -> computeEnvironments) (\s@DescribeComputeEnvironmentsResponse' {} a -> s {computeEnvironments = a} :: DescribeComputeEnvironmentsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeComputeEnvironmentsResponse_httpStatus :: Lens.Lens' DescribeComputeEnvironmentsResponse Core.Int
describeComputeEnvironmentsResponse_httpStatus = Lens.lens (\DescribeComputeEnvironmentsResponse' {httpStatus} -> httpStatus) (\s@DescribeComputeEnvironmentsResponse' {} a -> s {httpStatus = a} :: DescribeComputeEnvironmentsResponse)

instance
  Core.NFData
    DescribeComputeEnvironmentsResponse

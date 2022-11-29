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
-- Module      : Amazonka.Batch.DescribeJobQueues
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your job queues.
--
-- This operation returns paginated results.
module Amazonka.Batch.DescribeJobQueues
  ( -- * Creating a Request
    DescribeJobQueues (..),
    newDescribeJobQueues,

    -- * Request Lenses
    describeJobQueues_nextToken,
    describeJobQueues_maxResults,
    describeJobQueues_jobQueues,

    -- * Destructuring the Response
    DescribeJobQueuesResponse (..),
    newDescribeJobQueuesResponse,

    -- * Response Lenses
    describeJobQueuesResponse_nextToken,
    describeJobQueuesResponse_jobQueues,
    describeJobQueuesResponse_httpStatus,
  )
where

import Amazonka.Batch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for @DescribeJobQueues@.
--
-- /See:/ 'newDescribeJobQueues' smart constructor.
data DescribeJobQueues = DescribeJobQueues'
  { -- | The @nextToken@ value returned from a previous paginated
    -- @DescribeJobQueues@ request where @maxResults@ was used and the results
    -- exceeded the value of that parameter. Pagination continues from the end
    -- of the previous results that returned the @nextToken@ value. This value
    -- is @null@ when there are no more results to return.
    --
    -- Treat this token as an opaque identifier that\'s only used to retrieve
    -- the next items in a list and not for other programmatic purposes.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results returned by @DescribeJobQueues@ in
    -- paginated output. When this parameter is used, @DescribeJobQueues@ only
    -- returns @maxResults@ results in a single page and a @nextToken@ response
    -- element. The remaining results of the initial request can be seen by
    -- sending another @DescribeJobQueues@ request with the returned
    -- @nextToken@ value. This value can be between 1 and 100. If this
    -- parameter isn\'t used, then @DescribeJobQueues@ returns up to 100
    -- results and a @nextToken@ value if applicable.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | A list of up to 100 queue names or full queue Amazon Resource Name (ARN)
    -- entries.
    jobQueues :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeJobQueues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeJobQueues_nextToken' - The @nextToken@ value returned from a previous paginated
-- @DescribeJobQueues@ request where @maxResults@ was used and the results
-- exceeded the value of that parameter. Pagination continues from the end
-- of the previous results that returned the @nextToken@ value. This value
-- is @null@ when there are no more results to return.
--
-- Treat this token as an opaque identifier that\'s only used to retrieve
-- the next items in a list and not for other programmatic purposes.
--
-- 'maxResults', 'describeJobQueues_maxResults' - The maximum number of results returned by @DescribeJobQueues@ in
-- paginated output. When this parameter is used, @DescribeJobQueues@ only
-- returns @maxResults@ results in a single page and a @nextToken@ response
-- element. The remaining results of the initial request can be seen by
-- sending another @DescribeJobQueues@ request with the returned
-- @nextToken@ value. This value can be between 1 and 100. If this
-- parameter isn\'t used, then @DescribeJobQueues@ returns up to 100
-- results and a @nextToken@ value if applicable.
--
-- 'jobQueues', 'describeJobQueues_jobQueues' - A list of up to 100 queue names or full queue Amazon Resource Name (ARN)
-- entries.
newDescribeJobQueues ::
  DescribeJobQueues
newDescribeJobQueues =
  DescribeJobQueues'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      jobQueues = Prelude.Nothing
    }

-- | The @nextToken@ value returned from a previous paginated
-- @DescribeJobQueues@ request where @maxResults@ was used and the results
-- exceeded the value of that parameter. Pagination continues from the end
-- of the previous results that returned the @nextToken@ value. This value
-- is @null@ when there are no more results to return.
--
-- Treat this token as an opaque identifier that\'s only used to retrieve
-- the next items in a list and not for other programmatic purposes.
describeJobQueues_nextToken :: Lens.Lens' DescribeJobQueues (Prelude.Maybe Prelude.Text)
describeJobQueues_nextToken = Lens.lens (\DescribeJobQueues' {nextToken} -> nextToken) (\s@DescribeJobQueues' {} a -> s {nextToken = a} :: DescribeJobQueues)

-- | The maximum number of results returned by @DescribeJobQueues@ in
-- paginated output. When this parameter is used, @DescribeJobQueues@ only
-- returns @maxResults@ results in a single page and a @nextToken@ response
-- element. The remaining results of the initial request can be seen by
-- sending another @DescribeJobQueues@ request with the returned
-- @nextToken@ value. This value can be between 1 and 100. If this
-- parameter isn\'t used, then @DescribeJobQueues@ returns up to 100
-- results and a @nextToken@ value if applicable.
describeJobQueues_maxResults :: Lens.Lens' DescribeJobQueues (Prelude.Maybe Prelude.Int)
describeJobQueues_maxResults = Lens.lens (\DescribeJobQueues' {maxResults} -> maxResults) (\s@DescribeJobQueues' {} a -> s {maxResults = a} :: DescribeJobQueues)

-- | A list of up to 100 queue names or full queue Amazon Resource Name (ARN)
-- entries.
describeJobQueues_jobQueues :: Lens.Lens' DescribeJobQueues (Prelude.Maybe [Prelude.Text])
describeJobQueues_jobQueues = Lens.lens (\DescribeJobQueues' {jobQueues} -> jobQueues) (\s@DescribeJobQueues' {} a -> s {jobQueues = a} :: DescribeJobQueues) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager DescribeJobQueues where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeJobQueuesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeJobQueuesResponse_jobQueues
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeJobQueues_nextToken
          Lens..~ rs
          Lens.^? describeJobQueuesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeJobQueues where
  type
    AWSResponse DescribeJobQueues =
      DescribeJobQueuesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeJobQueuesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "jobQueues" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeJobQueues where
  hashWithSalt _salt DescribeJobQueues' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` jobQueues

instance Prelude.NFData DescribeJobQueues where
  rnf DescribeJobQueues' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf jobQueues

instance Core.ToHeaders DescribeJobQueues where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeJobQueues where
  toJSON DescribeJobQueues' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("jobQueues" Core..=) Prelude.<$> jobQueues
          ]
      )

instance Core.ToPath DescribeJobQueues where
  toPath = Prelude.const "/v1/describejobqueues"

instance Core.ToQuery DescribeJobQueues where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeJobQueuesResponse' smart constructor.
data DescribeJobQueuesResponse = DescribeJobQueuesResponse'
  { -- | The @nextToken@ value to include in a future @DescribeJobQueues@
    -- request. When the results of a @DescribeJobQueues@ request exceed
    -- @maxResults@, this value can be used to retrieve the next page of
    -- results. This value is @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of job queues.
    jobQueues :: Prelude.Maybe [JobQueueDetail],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeJobQueuesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeJobQueuesResponse_nextToken' - The @nextToken@ value to include in a future @DescribeJobQueues@
-- request. When the results of a @DescribeJobQueues@ request exceed
-- @maxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
--
-- 'jobQueues', 'describeJobQueuesResponse_jobQueues' - The list of job queues.
--
-- 'httpStatus', 'describeJobQueuesResponse_httpStatus' - The response's http status code.
newDescribeJobQueuesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeJobQueuesResponse
newDescribeJobQueuesResponse pHttpStatus_ =
  DescribeJobQueuesResponse'
    { nextToken =
        Prelude.Nothing,
      jobQueues = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ value to include in a future @DescribeJobQueues@
-- request. When the results of a @DescribeJobQueues@ request exceed
-- @maxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
describeJobQueuesResponse_nextToken :: Lens.Lens' DescribeJobQueuesResponse (Prelude.Maybe Prelude.Text)
describeJobQueuesResponse_nextToken = Lens.lens (\DescribeJobQueuesResponse' {nextToken} -> nextToken) (\s@DescribeJobQueuesResponse' {} a -> s {nextToken = a} :: DescribeJobQueuesResponse)

-- | The list of job queues.
describeJobQueuesResponse_jobQueues :: Lens.Lens' DescribeJobQueuesResponse (Prelude.Maybe [JobQueueDetail])
describeJobQueuesResponse_jobQueues = Lens.lens (\DescribeJobQueuesResponse' {jobQueues} -> jobQueues) (\s@DescribeJobQueuesResponse' {} a -> s {jobQueues = a} :: DescribeJobQueuesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeJobQueuesResponse_httpStatus :: Lens.Lens' DescribeJobQueuesResponse Prelude.Int
describeJobQueuesResponse_httpStatus = Lens.lens (\DescribeJobQueuesResponse' {httpStatus} -> httpStatus) (\s@DescribeJobQueuesResponse' {} a -> s {httpStatus = a} :: DescribeJobQueuesResponse)

instance Prelude.NFData DescribeJobQueuesResponse where
  rnf DescribeJobQueuesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf jobQueues
      `Prelude.seq` Prelude.rnf httpStatus

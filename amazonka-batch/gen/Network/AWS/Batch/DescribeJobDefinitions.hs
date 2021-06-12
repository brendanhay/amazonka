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
-- Module      : Network.AWS.Batch.DescribeJobDefinitions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a list of job definitions. You can specify a @status@ (such as
-- @ACTIVE@) to only return job definitions that match that status.
--
-- This operation returns paginated results.
module Network.AWS.Batch.DescribeJobDefinitions
  ( -- * Creating a Request
    DescribeJobDefinitions (..),
    newDescribeJobDefinitions,

    -- * Request Lenses
    describeJobDefinitions_nextToken,
    describeJobDefinitions_status,
    describeJobDefinitions_jobDefinitions,
    describeJobDefinitions_maxResults,
    describeJobDefinitions_jobDefinitionName,

    -- * Destructuring the Response
    DescribeJobDefinitionsResponse (..),
    newDescribeJobDefinitionsResponse,

    -- * Response Lenses
    describeJobDefinitionsResponse_nextToken,
    describeJobDefinitionsResponse_jobDefinitions,
    describeJobDefinitionsResponse_httpStatus,
  )
where

import Network.AWS.Batch.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for @DescribeJobDefinitions@.
--
-- /See:/ 'newDescribeJobDefinitions' smart constructor.
data DescribeJobDefinitions = DescribeJobDefinitions'
  { -- | The @nextToken@ value returned from a previous paginated
    -- @DescribeJobDefinitions@ request where @maxResults@ was used and the
    -- results exceeded the value of that parameter. Pagination continues from
    -- the end of the previous results that returned the @nextToken@ value.
    -- This value is @null@ when there are no more results to return.
    --
    -- This token should be treated as an opaque identifier that\'s only used
    -- to retrieve the next items in a list and not for other programmatic
    -- purposes.
    nextToken :: Core.Maybe Core.Text,
    -- | The status used to filter job definitions.
    status :: Core.Maybe Core.Text,
    -- | A list of up to 100 job definition names or full Amazon Resource Name
    -- (ARN) entries.
    jobDefinitions :: Core.Maybe [Core.Text],
    -- | The maximum number of results returned by @DescribeJobDefinitions@ in
    -- paginated output. When this parameter is used, @DescribeJobDefinitions@
    -- only returns @maxResults@ results in a single page along with a
    -- @nextToken@ response element. The remaining results of the initial
    -- request can be seen by sending another @DescribeJobDefinitions@ request
    -- with the returned @nextToken@ value. This value can be between 1 and
    -- 100. If this parameter isn\'t used, then @DescribeJobDefinitions@
    -- returns up to 100 results and a @nextToken@ value if applicable.
    maxResults :: Core.Maybe Core.Int,
    -- | The name of the job definition to describe.
    jobDefinitionName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeJobDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeJobDefinitions_nextToken' - The @nextToken@ value returned from a previous paginated
-- @DescribeJobDefinitions@ request where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
-- This value is @null@ when there are no more results to return.
--
-- This token should be treated as an opaque identifier that\'s only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
--
-- 'status', 'describeJobDefinitions_status' - The status used to filter job definitions.
--
-- 'jobDefinitions', 'describeJobDefinitions_jobDefinitions' - A list of up to 100 job definition names or full Amazon Resource Name
-- (ARN) entries.
--
-- 'maxResults', 'describeJobDefinitions_maxResults' - The maximum number of results returned by @DescribeJobDefinitions@ in
-- paginated output. When this parameter is used, @DescribeJobDefinitions@
-- only returns @maxResults@ results in a single page along with a
-- @nextToken@ response element. The remaining results of the initial
-- request can be seen by sending another @DescribeJobDefinitions@ request
-- with the returned @nextToken@ value. This value can be between 1 and
-- 100. If this parameter isn\'t used, then @DescribeJobDefinitions@
-- returns up to 100 results and a @nextToken@ value if applicable.
--
-- 'jobDefinitionName', 'describeJobDefinitions_jobDefinitionName' - The name of the job definition to describe.
newDescribeJobDefinitions ::
  DescribeJobDefinitions
newDescribeJobDefinitions =
  DescribeJobDefinitions'
    { nextToken = Core.Nothing,
      status = Core.Nothing,
      jobDefinitions = Core.Nothing,
      maxResults = Core.Nothing,
      jobDefinitionName = Core.Nothing
    }

-- | The @nextToken@ value returned from a previous paginated
-- @DescribeJobDefinitions@ request where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
-- This value is @null@ when there are no more results to return.
--
-- This token should be treated as an opaque identifier that\'s only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
describeJobDefinitions_nextToken :: Lens.Lens' DescribeJobDefinitions (Core.Maybe Core.Text)
describeJobDefinitions_nextToken = Lens.lens (\DescribeJobDefinitions' {nextToken} -> nextToken) (\s@DescribeJobDefinitions' {} a -> s {nextToken = a} :: DescribeJobDefinitions)

-- | The status used to filter job definitions.
describeJobDefinitions_status :: Lens.Lens' DescribeJobDefinitions (Core.Maybe Core.Text)
describeJobDefinitions_status = Lens.lens (\DescribeJobDefinitions' {status} -> status) (\s@DescribeJobDefinitions' {} a -> s {status = a} :: DescribeJobDefinitions)

-- | A list of up to 100 job definition names or full Amazon Resource Name
-- (ARN) entries.
describeJobDefinitions_jobDefinitions :: Lens.Lens' DescribeJobDefinitions (Core.Maybe [Core.Text])
describeJobDefinitions_jobDefinitions = Lens.lens (\DescribeJobDefinitions' {jobDefinitions} -> jobDefinitions) (\s@DescribeJobDefinitions' {} a -> s {jobDefinitions = a} :: DescribeJobDefinitions) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of results returned by @DescribeJobDefinitions@ in
-- paginated output. When this parameter is used, @DescribeJobDefinitions@
-- only returns @maxResults@ results in a single page along with a
-- @nextToken@ response element. The remaining results of the initial
-- request can be seen by sending another @DescribeJobDefinitions@ request
-- with the returned @nextToken@ value. This value can be between 1 and
-- 100. If this parameter isn\'t used, then @DescribeJobDefinitions@
-- returns up to 100 results and a @nextToken@ value if applicable.
describeJobDefinitions_maxResults :: Lens.Lens' DescribeJobDefinitions (Core.Maybe Core.Int)
describeJobDefinitions_maxResults = Lens.lens (\DescribeJobDefinitions' {maxResults} -> maxResults) (\s@DescribeJobDefinitions' {} a -> s {maxResults = a} :: DescribeJobDefinitions)

-- | The name of the job definition to describe.
describeJobDefinitions_jobDefinitionName :: Lens.Lens' DescribeJobDefinitions (Core.Maybe Core.Text)
describeJobDefinitions_jobDefinitionName = Lens.lens (\DescribeJobDefinitions' {jobDefinitionName} -> jobDefinitionName) (\s@DescribeJobDefinitions' {} a -> s {jobDefinitionName = a} :: DescribeJobDefinitions)

instance Core.AWSPager DescribeJobDefinitions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeJobDefinitionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeJobDefinitionsResponse_jobDefinitions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeJobDefinitions_nextToken
          Lens..~ rs
          Lens.^? describeJobDefinitionsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeJobDefinitions where
  type
    AWSResponse DescribeJobDefinitions =
      DescribeJobDefinitionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeJobDefinitionsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "jobDefinitions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeJobDefinitions

instance Core.NFData DescribeJobDefinitions

instance Core.ToHeaders DescribeJobDefinitions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeJobDefinitions where
  toJSON DescribeJobDefinitions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("status" Core..=) Core.<$> status,
            ("jobDefinitions" Core..=) Core.<$> jobDefinitions,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("jobDefinitionName" Core..=)
              Core.<$> jobDefinitionName
          ]
      )

instance Core.ToPath DescribeJobDefinitions where
  toPath = Core.const "/v1/describejobdefinitions"

instance Core.ToQuery DescribeJobDefinitions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeJobDefinitionsResponse' smart constructor.
data DescribeJobDefinitionsResponse = DescribeJobDefinitionsResponse'
  { -- | The @nextToken@ value to include in a future @DescribeJobDefinitions@
    -- request. When the results of a @DescribeJobDefinitions@ request exceed
    -- @maxResults@, this value can be used to retrieve the next page of
    -- results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of job definitions.
    jobDefinitions :: Core.Maybe [JobDefinition],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeJobDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeJobDefinitionsResponse_nextToken' - The @nextToken@ value to include in a future @DescribeJobDefinitions@
-- request. When the results of a @DescribeJobDefinitions@ request exceed
-- @maxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
--
-- 'jobDefinitions', 'describeJobDefinitionsResponse_jobDefinitions' - The list of job definitions.
--
-- 'httpStatus', 'describeJobDefinitionsResponse_httpStatus' - The response's http status code.
newDescribeJobDefinitionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeJobDefinitionsResponse
newDescribeJobDefinitionsResponse pHttpStatus_ =
  DescribeJobDefinitionsResponse'
    { nextToken =
        Core.Nothing,
      jobDefinitions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ value to include in a future @DescribeJobDefinitions@
-- request. When the results of a @DescribeJobDefinitions@ request exceed
-- @maxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
describeJobDefinitionsResponse_nextToken :: Lens.Lens' DescribeJobDefinitionsResponse (Core.Maybe Core.Text)
describeJobDefinitionsResponse_nextToken = Lens.lens (\DescribeJobDefinitionsResponse' {nextToken} -> nextToken) (\s@DescribeJobDefinitionsResponse' {} a -> s {nextToken = a} :: DescribeJobDefinitionsResponse)

-- | The list of job definitions.
describeJobDefinitionsResponse_jobDefinitions :: Lens.Lens' DescribeJobDefinitionsResponse (Core.Maybe [JobDefinition])
describeJobDefinitionsResponse_jobDefinitions = Lens.lens (\DescribeJobDefinitionsResponse' {jobDefinitions} -> jobDefinitions) (\s@DescribeJobDefinitionsResponse' {} a -> s {jobDefinitions = a} :: DescribeJobDefinitionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeJobDefinitionsResponse_httpStatus :: Lens.Lens' DescribeJobDefinitionsResponse Core.Int
describeJobDefinitionsResponse_httpStatus = Lens.lens (\DescribeJobDefinitionsResponse' {httpStatus} -> httpStatus) (\s@DescribeJobDefinitionsResponse' {} a -> s {httpStatus = a} :: DescribeJobDefinitionsResponse)

instance Core.NFData DescribeJobDefinitionsResponse

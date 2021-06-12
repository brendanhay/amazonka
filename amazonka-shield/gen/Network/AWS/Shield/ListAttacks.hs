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
-- Module      : Network.AWS.Shield.ListAttacks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all ongoing DDoS attacks or all DDoS attacks during a specified
-- time period.
--
-- This operation returns paginated results.
module Network.AWS.Shield.ListAttacks
  ( -- * Creating a Request
    ListAttacks (..),
    newListAttacks,

    -- * Request Lenses
    listAttacks_nextToken,
    listAttacks_maxResults,
    listAttacks_startTime,
    listAttacks_endTime,
    listAttacks_resourceArns,

    -- * Destructuring the Response
    ListAttacksResponse (..),
    newListAttacksResponse,

    -- * Response Lenses
    listAttacksResponse_nextToken,
    listAttacksResponse_attackSummaries,
    listAttacksResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Shield.Types

-- | /See:/ 'newListAttacks' smart constructor.
data ListAttacks = ListAttacks'
  { -- | The @ListAttacksRequest.NextMarker@ value from a previous call to
    -- @ListAttacksRequest@. Pass null if this is the first call.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of AttackSummary objects to return. If you leave this
    -- blank, Shield Advanced returns the first 20 results.
    --
    -- This is a maximum value. Shield Advanced might return the results in
    -- smaller batches. That is, the number of objects returned could be less
    -- than @MaxResults@, even if there are still more objects yet to return.
    -- If there are more objects to return, Shield Advanced returns a value in
    -- @NextToken@ that you can use in your next request, to get the next batch
    -- of objects.
    maxResults :: Core.Maybe Core.Natural,
    -- | The start of the time period for the attacks. This is a @timestamp@
    -- type. The sample request above indicates a @number@ type because the
    -- default used by WAF is Unix time in seconds. However any valid
    -- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp format>
    -- is allowed.
    startTime :: Core.Maybe TimeRange,
    -- | The end of the time period for the attacks. This is a @timestamp@ type.
    -- The sample request above indicates a @number@ type because the default
    -- used by WAF is Unix time in seconds. However any valid
    -- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp format>
    -- is allowed.
    endTime :: Core.Maybe TimeRange,
    -- | The ARN (Amazon Resource Name) of the resource that was attacked. If
    -- this is left blank, all applicable resources for this account will be
    -- included.
    resourceArns :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAttacks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAttacks_nextToken' - The @ListAttacksRequest.NextMarker@ value from a previous call to
-- @ListAttacksRequest@. Pass null if this is the first call.
--
-- 'maxResults', 'listAttacks_maxResults' - The maximum number of AttackSummary objects to return. If you leave this
-- blank, Shield Advanced returns the first 20 results.
--
-- This is a maximum value. Shield Advanced might return the results in
-- smaller batches. That is, the number of objects returned could be less
-- than @MaxResults@, even if there are still more objects yet to return.
-- If there are more objects to return, Shield Advanced returns a value in
-- @NextToken@ that you can use in your next request, to get the next batch
-- of objects.
--
-- 'startTime', 'listAttacks_startTime' - The start of the time period for the attacks. This is a @timestamp@
-- type. The sample request above indicates a @number@ type because the
-- default used by WAF is Unix time in seconds. However any valid
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp format>
-- is allowed.
--
-- 'endTime', 'listAttacks_endTime' - The end of the time period for the attacks. This is a @timestamp@ type.
-- The sample request above indicates a @number@ type because the default
-- used by WAF is Unix time in seconds. However any valid
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp format>
-- is allowed.
--
-- 'resourceArns', 'listAttacks_resourceArns' - The ARN (Amazon Resource Name) of the resource that was attacked. If
-- this is left blank, all applicable resources for this account will be
-- included.
newListAttacks ::
  ListAttacks
newListAttacks =
  ListAttacks'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      startTime = Core.Nothing,
      endTime = Core.Nothing,
      resourceArns = Core.Nothing
    }

-- | The @ListAttacksRequest.NextMarker@ value from a previous call to
-- @ListAttacksRequest@. Pass null if this is the first call.
listAttacks_nextToken :: Lens.Lens' ListAttacks (Core.Maybe Core.Text)
listAttacks_nextToken = Lens.lens (\ListAttacks' {nextToken} -> nextToken) (\s@ListAttacks' {} a -> s {nextToken = a} :: ListAttacks)

-- | The maximum number of AttackSummary objects to return. If you leave this
-- blank, Shield Advanced returns the first 20 results.
--
-- This is a maximum value. Shield Advanced might return the results in
-- smaller batches. That is, the number of objects returned could be less
-- than @MaxResults@, even if there are still more objects yet to return.
-- If there are more objects to return, Shield Advanced returns a value in
-- @NextToken@ that you can use in your next request, to get the next batch
-- of objects.
listAttacks_maxResults :: Lens.Lens' ListAttacks (Core.Maybe Core.Natural)
listAttacks_maxResults = Lens.lens (\ListAttacks' {maxResults} -> maxResults) (\s@ListAttacks' {} a -> s {maxResults = a} :: ListAttacks)

-- | The start of the time period for the attacks. This is a @timestamp@
-- type. The sample request above indicates a @number@ type because the
-- default used by WAF is Unix time in seconds. However any valid
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp format>
-- is allowed.
listAttacks_startTime :: Lens.Lens' ListAttacks (Core.Maybe TimeRange)
listAttacks_startTime = Lens.lens (\ListAttacks' {startTime} -> startTime) (\s@ListAttacks' {} a -> s {startTime = a} :: ListAttacks)

-- | The end of the time period for the attacks. This is a @timestamp@ type.
-- The sample request above indicates a @number@ type because the default
-- used by WAF is Unix time in seconds. However any valid
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp format>
-- is allowed.
listAttacks_endTime :: Lens.Lens' ListAttacks (Core.Maybe TimeRange)
listAttacks_endTime = Lens.lens (\ListAttacks' {endTime} -> endTime) (\s@ListAttacks' {} a -> s {endTime = a} :: ListAttacks)

-- | The ARN (Amazon Resource Name) of the resource that was attacked. If
-- this is left blank, all applicable resources for this account will be
-- included.
listAttacks_resourceArns :: Lens.Lens' ListAttacks (Core.Maybe [Core.Text])
listAttacks_resourceArns = Lens.lens (\ListAttacks' {resourceArns} -> resourceArns) (\s@ListAttacks' {} a -> s {resourceArns = a} :: ListAttacks) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager ListAttacks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAttacksResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listAttacksResponse_attackSummaries
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listAttacks_nextToken
          Lens..~ rs
          Lens.^? listAttacksResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListAttacks where
  type AWSResponse ListAttacks = ListAttacksResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAttacksResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "AttackSummaries" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListAttacks

instance Core.NFData ListAttacks

instance Core.ToHeaders ListAttacks where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSShield_20160616.ListAttacks" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListAttacks where
  toJSON ListAttacks' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("StartTime" Core..=) Core.<$> startTime,
            ("EndTime" Core..=) Core.<$> endTime,
            ("ResourceArns" Core..=) Core.<$> resourceArns
          ]
      )

instance Core.ToPath ListAttacks where
  toPath = Core.const "/"

instance Core.ToQuery ListAttacks where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListAttacksResponse' smart constructor.
data ListAttacksResponse = ListAttacksResponse'
  { -- | The token returned by a previous call to indicate that there is more
    -- data available. If not null, more results are available. Pass this value
    -- for the @NextMarker@ parameter in a subsequent call to @ListAttacks@ to
    -- retrieve the next set of items.
    --
    -- Shield Advanced might return the list of AttackSummary objects in
    -- batches smaller than the number specified by MaxResults. If there are
    -- more attack summary objects to return, Shield Advanced will always also
    -- return a @NextToken@.
    nextToken :: Core.Maybe Core.Text,
    -- | The attack information for the specified time range.
    attackSummaries :: Core.Maybe [AttackSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAttacksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAttacksResponse_nextToken' - The token returned by a previous call to indicate that there is more
-- data available. If not null, more results are available. Pass this value
-- for the @NextMarker@ parameter in a subsequent call to @ListAttacks@ to
-- retrieve the next set of items.
--
-- Shield Advanced might return the list of AttackSummary objects in
-- batches smaller than the number specified by MaxResults. If there are
-- more attack summary objects to return, Shield Advanced will always also
-- return a @NextToken@.
--
-- 'attackSummaries', 'listAttacksResponse_attackSummaries' - The attack information for the specified time range.
--
-- 'httpStatus', 'listAttacksResponse_httpStatus' - The response's http status code.
newListAttacksResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListAttacksResponse
newListAttacksResponse pHttpStatus_ =
  ListAttacksResponse'
    { nextToken = Core.Nothing,
      attackSummaries = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token returned by a previous call to indicate that there is more
-- data available. If not null, more results are available. Pass this value
-- for the @NextMarker@ parameter in a subsequent call to @ListAttacks@ to
-- retrieve the next set of items.
--
-- Shield Advanced might return the list of AttackSummary objects in
-- batches smaller than the number specified by MaxResults. If there are
-- more attack summary objects to return, Shield Advanced will always also
-- return a @NextToken@.
listAttacksResponse_nextToken :: Lens.Lens' ListAttacksResponse (Core.Maybe Core.Text)
listAttacksResponse_nextToken = Lens.lens (\ListAttacksResponse' {nextToken} -> nextToken) (\s@ListAttacksResponse' {} a -> s {nextToken = a} :: ListAttacksResponse)

-- | The attack information for the specified time range.
listAttacksResponse_attackSummaries :: Lens.Lens' ListAttacksResponse (Core.Maybe [AttackSummary])
listAttacksResponse_attackSummaries = Lens.lens (\ListAttacksResponse' {attackSummaries} -> attackSummaries) (\s@ListAttacksResponse' {} a -> s {attackSummaries = a} :: ListAttacksResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listAttacksResponse_httpStatus :: Lens.Lens' ListAttacksResponse Core.Int
listAttacksResponse_httpStatus = Lens.lens (\ListAttacksResponse' {httpStatus} -> httpStatus) (\s@ListAttacksResponse' {} a -> s {httpStatus = a} :: ListAttacksResponse)

instance Core.NFData ListAttacksResponse

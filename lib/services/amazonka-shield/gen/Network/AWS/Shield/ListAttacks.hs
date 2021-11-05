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
-- Module      : Amazonka.Shield.ListAttacks
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
module Amazonka.Shield.ListAttacks
  ( -- * Creating a Request
    ListAttacks (..),
    newListAttacks,

    -- * Request Lenses
    listAttacks_startTime,
    listAttacks_resourceArns,
    listAttacks_nextToken,
    listAttacks_endTime,
    listAttacks_maxResults,

    -- * Destructuring the Response
    ListAttacksResponse (..),
    newListAttacksResponse,

    -- * Response Lenses
    listAttacksResponse_attackSummaries,
    listAttacksResponse_nextToken,
    listAttacksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Shield.Types

-- | /See:/ 'newListAttacks' smart constructor.
data ListAttacks = ListAttacks'
  { -- | The start of the time period for the attacks. This is a @timestamp@
    -- type. The sample request above indicates a @number@ type because the
    -- default used by WAF is Unix time in seconds. However any valid
    -- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp format>
    -- is allowed.
    startTime :: Prelude.Maybe TimeRange,
    -- | The ARN (Amazon Resource Name) of the resource that was attacked. If
    -- this is left blank, all applicable resources for this account will be
    -- included.
    resourceArns :: Prelude.Maybe [Prelude.Text],
    -- | The @ListAttacksRequest.NextMarker@ value from a previous call to
    -- @ListAttacksRequest@. Pass null if this is the first call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The end of the time period for the attacks. This is a @timestamp@ type.
    -- The sample request above indicates a @number@ type because the default
    -- used by WAF is Unix time in seconds. However any valid
    -- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp format>
    -- is allowed.
    endTime :: Prelude.Maybe TimeRange,
    -- | The maximum number of AttackSummary objects to return. If you leave this
    -- blank, Shield Advanced returns the first 20 results.
    --
    -- This is a maximum value. Shield Advanced might return the results in
    -- smaller batches. That is, the number of objects returned could be less
    -- than @MaxResults@, even if there are still more objects yet to return.
    -- If there are more objects to return, Shield Advanced returns a value in
    -- @NextToken@ that you can use in your next request, to get the next batch
    -- of objects.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAttacks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startTime', 'listAttacks_startTime' - The start of the time period for the attacks. This is a @timestamp@
-- type. The sample request above indicates a @number@ type because the
-- default used by WAF is Unix time in seconds. However any valid
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp format>
-- is allowed.
--
-- 'resourceArns', 'listAttacks_resourceArns' - The ARN (Amazon Resource Name) of the resource that was attacked. If
-- this is left blank, all applicable resources for this account will be
-- included.
--
-- 'nextToken', 'listAttacks_nextToken' - The @ListAttacksRequest.NextMarker@ value from a previous call to
-- @ListAttacksRequest@. Pass null if this is the first call.
--
-- 'endTime', 'listAttacks_endTime' - The end of the time period for the attacks. This is a @timestamp@ type.
-- The sample request above indicates a @number@ type because the default
-- used by WAF is Unix time in seconds. However any valid
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp format>
-- is allowed.
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
newListAttacks ::
  ListAttacks
newListAttacks =
  ListAttacks'
    { startTime = Prelude.Nothing,
      resourceArns = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      endTime = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The start of the time period for the attacks. This is a @timestamp@
-- type. The sample request above indicates a @number@ type because the
-- default used by WAF is Unix time in seconds. However any valid
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp format>
-- is allowed.
listAttacks_startTime :: Lens.Lens' ListAttacks (Prelude.Maybe TimeRange)
listAttacks_startTime = Lens.lens (\ListAttacks' {startTime} -> startTime) (\s@ListAttacks' {} a -> s {startTime = a} :: ListAttacks)

-- | The ARN (Amazon Resource Name) of the resource that was attacked. If
-- this is left blank, all applicable resources for this account will be
-- included.
listAttacks_resourceArns :: Lens.Lens' ListAttacks (Prelude.Maybe [Prelude.Text])
listAttacks_resourceArns = Lens.lens (\ListAttacks' {resourceArns} -> resourceArns) (\s@ListAttacks' {} a -> s {resourceArns = a} :: ListAttacks) Prelude.. Lens.mapping Lens.coerced

-- | The @ListAttacksRequest.NextMarker@ value from a previous call to
-- @ListAttacksRequest@. Pass null if this is the first call.
listAttacks_nextToken :: Lens.Lens' ListAttacks (Prelude.Maybe Prelude.Text)
listAttacks_nextToken = Lens.lens (\ListAttacks' {nextToken} -> nextToken) (\s@ListAttacks' {} a -> s {nextToken = a} :: ListAttacks)

-- | The end of the time period for the attacks. This is a @timestamp@ type.
-- The sample request above indicates a @number@ type because the default
-- used by WAF is Unix time in seconds. However any valid
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp format>
-- is allowed.
listAttacks_endTime :: Lens.Lens' ListAttacks (Prelude.Maybe TimeRange)
listAttacks_endTime = Lens.lens (\ListAttacks' {endTime} -> endTime) (\s@ListAttacks' {} a -> s {endTime = a} :: ListAttacks)

-- | The maximum number of AttackSummary objects to return. If you leave this
-- blank, Shield Advanced returns the first 20 results.
--
-- This is a maximum value. Shield Advanced might return the results in
-- smaller batches. That is, the number of objects returned could be less
-- than @MaxResults@, even if there are still more objects yet to return.
-- If there are more objects to return, Shield Advanced returns a value in
-- @NextToken@ that you can use in your next request, to get the next batch
-- of objects.
listAttacks_maxResults :: Lens.Lens' ListAttacks (Prelude.Maybe Prelude.Natural)
listAttacks_maxResults = Lens.lens (\ListAttacks' {maxResults} -> maxResults) (\s@ListAttacks' {} a -> s {maxResults = a} :: ListAttacks)

instance Core.AWSPager ListAttacks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAttacksResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAttacksResponse_attackSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAttacks_nextToken
          Lens..~ rs
          Lens.^? listAttacksResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListAttacks where
  type AWSResponse ListAttacks = ListAttacksResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAttacksResponse'
            Prelude.<$> ( x Core..?> "AttackSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAttacks

instance Prelude.NFData ListAttacks

instance Core.ToHeaders ListAttacks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSShield_20160616.ListAttacks" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListAttacks where
  toJSON ListAttacks' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("StartTime" Core..=) Prelude.<$> startTime,
            ("ResourceArns" Core..=) Prelude.<$> resourceArns,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("EndTime" Core..=) Prelude.<$> endTime,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListAttacks where
  toPath = Prelude.const "/"

instance Core.ToQuery ListAttacks where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAttacksResponse' smart constructor.
data ListAttacksResponse = ListAttacksResponse'
  { -- | The attack information for the specified time range.
    attackSummaries :: Prelude.Maybe [AttackSummary],
    -- | The token returned by a previous call to indicate that there is more
    -- data available. If not null, more results are available. Pass this value
    -- for the @NextMarker@ parameter in a subsequent call to @ListAttacks@ to
    -- retrieve the next set of items.
    --
    -- Shield Advanced might return the list of AttackSummary objects in
    -- batches smaller than the number specified by MaxResults. If there are
    -- more attack summary objects to return, Shield Advanced will always also
    -- return a @NextToken@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAttacksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attackSummaries', 'listAttacksResponse_attackSummaries' - The attack information for the specified time range.
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
-- 'httpStatus', 'listAttacksResponse_httpStatus' - The response's http status code.
newListAttacksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAttacksResponse
newListAttacksResponse pHttpStatus_ =
  ListAttacksResponse'
    { attackSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The attack information for the specified time range.
listAttacksResponse_attackSummaries :: Lens.Lens' ListAttacksResponse (Prelude.Maybe [AttackSummary])
listAttacksResponse_attackSummaries = Lens.lens (\ListAttacksResponse' {attackSummaries} -> attackSummaries) (\s@ListAttacksResponse' {} a -> s {attackSummaries = a} :: ListAttacksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token returned by a previous call to indicate that there is more
-- data available. If not null, more results are available. Pass this value
-- for the @NextMarker@ parameter in a subsequent call to @ListAttacks@ to
-- retrieve the next set of items.
--
-- Shield Advanced might return the list of AttackSummary objects in
-- batches smaller than the number specified by MaxResults. If there are
-- more attack summary objects to return, Shield Advanced will always also
-- return a @NextToken@.
listAttacksResponse_nextToken :: Lens.Lens' ListAttacksResponse (Prelude.Maybe Prelude.Text)
listAttacksResponse_nextToken = Lens.lens (\ListAttacksResponse' {nextToken} -> nextToken) (\s@ListAttacksResponse' {} a -> s {nextToken = a} :: ListAttacksResponse)

-- | The response's http status code.
listAttacksResponse_httpStatus :: Lens.Lens' ListAttacksResponse Prelude.Int
listAttacksResponse_httpStatus = Lens.lens (\ListAttacksResponse' {httpStatus} -> httpStatus) (\s@ListAttacksResponse' {} a -> s {httpStatus = a} :: ListAttacksResponse)

instance Prelude.NFData ListAttacksResponse

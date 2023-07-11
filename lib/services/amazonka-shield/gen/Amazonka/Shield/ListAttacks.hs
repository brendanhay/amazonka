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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    listAttacks_endTime,
    listAttacks_maxResults,
    listAttacks_nextToken,
    listAttacks_resourceArns,
    listAttacks_startTime,

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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Shield.Types

-- | /See:/ 'newListAttacks' smart constructor.
data ListAttacks = ListAttacks'
  { -- | The end of the time period for the attacks. This is a @timestamp@ type.
    -- The request syntax listing for this call indicates a @number@ type, but
    -- you can provide the time in any valid
    -- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-parameters-types.html#parameter-type-timestamp timestamp format>
    -- setting.
    endTime :: Prelude.Maybe TimeRange,
    -- | The greatest number of objects that you want Shield Advanced to return
    -- to the list request. Shield Advanced might return fewer objects than you
    -- indicate in this setting, even if more objects are available. If there
    -- are more objects remaining, Shield Advanced will always also return a
    -- @NextToken@ value in the response.
    --
    -- The default setting is 20.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | When you request a list of objects from Shield Advanced, if the response
    -- does not include all of the remaining available objects, Shield Advanced
    -- includes a @NextToken@ value in the response. You can retrieve the next
    -- batch of objects by requesting the list again and providing the token
    -- that was returned by the prior call in your request.
    --
    -- You can indicate the maximum number of objects that you want Shield
    -- Advanced to return for a single call with the @MaxResults@ setting.
    -- Shield Advanced will not return more than @MaxResults@ objects, but may
    -- return fewer, even if more objects are still available.
    --
    -- Whenever more objects remain that Shield Advanced has not yet returned
    -- to you, the response will include a @NextToken@ value.
    --
    -- On your first call to a list operation, leave this setting empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARNs (Amazon Resource Names) of the resources that were attacked. If
    -- you leave this blank, all applicable resources for this account will be
    -- included.
    resourceArns :: Prelude.Maybe [Prelude.Text],
    -- | The start of the time period for the attacks. This is a @timestamp@
    -- type. The request syntax listing for this call indicates a @number@
    -- type, but you can provide the time in any valid
    -- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-parameters-types.html#parameter-type-timestamp timestamp format>
    -- setting.
    startTime :: Prelude.Maybe TimeRange
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
-- 'endTime', 'listAttacks_endTime' - The end of the time period for the attacks. This is a @timestamp@ type.
-- The request syntax listing for this call indicates a @number@ type, but
-- you can provide the time in any valid
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-parameters-types.html#parameter-type-timestamp timestamp format>
-- setting.
--
-- 'maxResults', 'listAttacks_maxResults' - The greatest number of objects that you want Shield Advanced to return
-- to the list request. Shield Advanced might return fewer objects than you
-- indicate in this setting, even if more objects are available. If there
-- are more objects remaining, Shield Advanced will always also return a
-- @NextToken@ value in the response.
--
-- The default setting is 20.
--
-- 'nextToken', 'listAttacks_nextToken' - When you request a list of objects from Shield Advanced, if the response
-- does not include all of the remaining available objects, Shield Advanced
-- includes a @NextToken@ value in the response. You can retrieve the next
-- batch of objects by requesting the list again and providing the token
-- that was returned by the prior call in your request.
--
-- You can indicate the maximum number of objects that you want Shield
-- Advanced to return for a single call with the @MaxResults@ setting.
-- Shield Advanced will not return more than @MaxResults@ objects, but may
-- return fewer, even if more objects are still available.
--
-- Whenever more objects remain that Shield Advanced has not yet returned
-- to you, the response will include a @NextToken@ value.
--
-- On your first call to a list operation, leave this setting empty.
--
-- 'resourceArns', 'listAttacks_resourceArns' - The ARNs (Amazon Resource Names) of the resources that were attacked. If
-- you leave this blank, all applicable resources for this account will be
-- included.
--
-- 'startTime', 'listAttacks_startTime' - The start of the time period for the attacks. This is a @timestamp@
-- type. The request syntax listing for this call indicates a @number@
-- type, but you can provide the time in any valid
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-parameters-types.html#parameter-type-timestamp timestamp format>
-- setting.
newListAttacks ::
  ListAttacks
newListAttacks =
  ListAttacks'
    { endTime = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resourceArns = Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | The end of the time period for the attacks. This is a @timestamp@ type.
-- The request syntax listing for this call indicates a @number@ type, but
-- you can provide the time in any valid
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-parameters-types.html#parameter-type-timestamp timestamp format>
-- setting.
listAttacks_endTime :: Lens.Lens' ListAttacks (Prelude.Maybe TimeRange)
listAttacks_endTime = Lens.lens (\ListAttacks' {endTime} -> endTime) (\s@ListAttacks' {} a -> s {endTime = a} :: ListAttacks)

-- | The greatest number of objects that you want Shield Advanced to return
-- to the list request. Shield Advanced might return fewer objects than you
-- indicate in this setting, even if more objects are available. If there
-- are more objects remaining, Shield Advanced will always also return a
-- @NextToken@ value in the response.
--
-- The default setting is 20.
listAttacks_maxResults :: Lens.Lens' ListAttacks (Prelude.Maybe Prelude.Natural)
listAttacks_maxResults = Lens.lens (\ListAttacks' {maxResults} -> maxResults) (\s@ListAttacks' {} a -> s {maxResults = a} :: ListAttacks)

-- | When you request a list of objects from Shield Advanced, if the response
-- does not include all of the remaining available objects, Shield Advanced
-- includes a @NextToken@ value in the response. You can retrieve the next
-- batch of objects by requesting the list again and providing the token
-- that was returned by the prior call in your request.
--
-- You can indicate the maximum number of objects that you want Shield
-- Advanced to return for a single call with the @MaxResults@ setting.
-- Shield Advanced will not return more than @MaxResults@ objects, but may
-- return fewer, even if more objects are still available.
--
-- Whenever more objects remain that Shield Advanced has not yet returned
-- to you, the response will include a @NextToken@ value.
--
-- On your first call to a list operation, leave this setting empty.
listAttacks_nextToken :: Lens.Lens' ListAttacks (Prelude.Maybe Prelude.Text)
listAttacks_nextToken = Lens.lens (\ListAttacks' {nextToken} -> nextToken) (\s@ListAttacks' {} a -> s {nextToken = a} :: ListAttacks)

-- | The ARNs (Amazon Resource Names) of the resources that were attacked. If
-- you leave this blank, all applicable resources for this account will be
-- included.
listAttacks_resourceArns :: Lens.Lens' ListAttacks (Prelude.Maybe [Prelude.Text])
listAttacks_resourceArns = Lens.lens (\ListAttacks' {resourceArns} -> resourceArns) (\s@ListAttacks' {} a -> s {resourceArns = a} :: ListAttacks) Prelude.. Lens.mapping Lens.coerced

-- | The start of the time period for the attacks. This is a @timestamp@
-- type. The request syntax listing for this call indicates a @number@
-- type, but you can provide the time in any valid
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-parameters-types.html#parameter-type-timestamp timestamp format>
-- setting.
listAttacks_startTime :: Lens.Lens' ListAttacks (Prelude.Maybe TimeRange)
listAttacks_startTime = Lens.lens (\ListAttacks' {startTime} -> startTime) (\s@ListAttacks' {} a -> s {startTime = a} :: ListAttacks)

instance Core.AWSPager ListAttacks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAttacksResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAttacksResponse_attackSummaries
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listAttacks_nextToken
          Lens..~ rs
          Lens.^? listAttacksResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListAttacks where
  type AWSResponse ListAttacks = ListAttacksResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAttacksResponse'
            Prelude.<$> ( x
                            Data..?> "AttackSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAttacks where
  hashWithSalt _salt ListAttacks' {..} =
    _salt
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` resourceArns
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData ListAttacks where
  rnf ListAttacks' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceArns
      `Prelude.seq` Prelude.rnf startTime

instance Data.ToHeaders ListAttacks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSShield_20160616.ListAttacks" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAttacks where
  toJSON ListAttacks' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EndTime" Data..=) Prelude.<$> endTime,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("ResourceArns" Data..=) Prelude.<$> resourceArns,
            ("StartTime" Data..=) Prelude.<$> startTime
          ]
      )

instance Data.ToPath ListAttacks where
  toPath = Prelude.const "/"

instance Data.ToQuery ListAttacks where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAttacksResponse' smart constructor.
data ListAttacksResponse = ListAttacksResponse'
  { -- | The attack information for the specified time range.
    attackSummaries :: Prelude.Maybe [AttackSummary],
    -- | When you request a list of objects from Shield Advanced, if the response
    -- does not include all of the remaining available objects, Shield Advanced
    -- includes a @NextToken@ value in the response. You can retrieve the next
    -- batch of objects by requesting the list again and providing the token
    -- that was returned by the prior call in your request.
    --
    -- You can indicate the maximum number of objects that you want Shield
    -- Advanced to return for a single call with the @MaxResults@ setting.
    -- Shield Advanced will not return more than @MaxResults@ objects, but may
    -- return fewer, even if more objects are still available.
    --
    -- Whenever more objects remain that Shield Advanced has not yet returned
    -- to you, the response will include a @NextToken@ value.
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
-- 'nextToken', 'listAttacksResponse_nextToken' - When you request a list of objects from Shield Advanced, if the response
-- does not include all of the remaining available objects, Shield Advanced
-- includes a @NextToken@ value in the response. You can retrieve the next
-- batch of objects by requesting the list again and providing the token
-- that was returned by the prior call in your request.
--
-- You can indicate the maximum number of objects that you want Shield
-- Advanced to return for a single call with the @MaxResults@ setting.
-- Shield Advanced will not return more than @MaxResults@ objects, but may
-- return fewer, even if more objects are still available.
--
-- Whenever more objects remain that Shield Advanced has not yet returned
-- to you, the response will include a @NextToken@ value.
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

-- | When you request a list of objects from Shield Advanced, if the response
-- does not include all of the remaining available objects, Shield Advanced
-- includes a @NextToken@ value in the response. You can retrieve the next
-- batch of objects by requesting the list again and providing the token
-- that was returned by the prior call in your request.
--
-- You can indicate the maximum number of objects that you want Shield
-- Advanced to return for a single call with the @MaxResults@ setting.
-- Shield Advanced will not return more than @MaxResults@ objects, but may
-- return fewer, even if more objects are still available.
--
-- Whenever more objects remain that Shield Advanced has not yet returned
-- to you, the response will include a @NextToken@ value.
listAttacksResponse_nextToken :: Lens.Lens' ListAttacksResponse (Prelude.Maybe Prelude.Text)
listAttacksResponse_nextToken = Lens.lens (\ListAttacksResponse' {nextToken} -> nextToken) (\s@ListAttacksResponse' {} a -> s {nextToken = a} :: ListAttacksResponse)

-- | The response's http status code.
listAttacksResponse_httpStatus :: Lens.Lens' ListAttacksResponse Prelude.Int
listAttacksResponse_httpStatus = Lens.lens (\ListAttacksResponse' {httpStatus} -> httpStatus) (\s@ListAttacksResponse' {} a -> s {httpStatus = a} :: ListAttacksResponse)

instance Prelude.NFData ListAttacksResponse where
  rnf ListAttacksResponse' {..} =
    Prelude.rnf attackSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

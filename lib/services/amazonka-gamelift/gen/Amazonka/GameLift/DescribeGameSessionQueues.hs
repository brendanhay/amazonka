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
-- Module      : Amazonka.GameLift.DescribeGameSessionQueues
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the properties for one or more game session queues. When
-- requesting multiple queues, use the pagination parameters to retrieve
-- results as a set of sequential pages. If successful, a GameSessionQueue
-- object is returned for each requested queue. When specifying a list of
-- queues, objects are returned only for queues that currently exist in the
-- Region.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/queues-console.html View Your Queues>
--
-- __Related actions__
--
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_CreateGameSessionQueue.html CreateGameSessionQueue>
-- |
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_DescribeGameSessionQueues.html DescribeGameSessionQueues>
-- |
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_UpdateGameSessionQueue.html UpdateGameSessionQueue>
-- |
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_DeleteGameSessionQueue.html DeleteGameSessionQueue>
-- |
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
--
-- This operation returns paginated results.
module Amazonka.GameLift.DescribeGameSessionQueues
  ( -- * Creating a Request
    DescribeGameSessionQueues (..),
    newDescribeGameSessionQueues,

    -- * Request Lenses
    describeGameSessionQueues_nextToken,
    describeGameSessionQueues_names,
    describeGameSessionQueues_limit,

    -- * Destructuring the Response
    DescribeGameSessionQueuesResponse (..),
    newDescribeGameSessionQueuesResponse,

    -- * Response Lenses
    describeGameSessionQueuesResponse_nextToken,
    describeGameSessionQueuesResponse_gameSessionQueues,
    describeGameSessionQueuesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDescribeGameSessionQueues' smart constructor.
data DescribeGameSessionQueues = DescribeGameSessionQueues'
  { -- | A token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of queue names to retrieve information for. You can use either
    -- the queue ID or ARN value. To request settings for all queues, leave
    -- this parameter empty.
    names :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages. You can request
    -- up to 50 results.
    limit :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGameSessionQueues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeGameSessionQueues_nextToken' - A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
--
-- 'names', 'describeGameSessionQueues_names' - A list of queue names to retrieve information for. You can use either
-- the queue ID or ARN value. To request settings for all queues, leave
-- this parameter empty.
--
-- 'limit', 'describeGameSessionQueues_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages. You can request
-- up to 50 results.
newDescribeGameSessionQueues ::
  DescribeGameSessionQueues
newDescribeGameSessionQueues =
  DescribeGameSessionQueues'
    { nextToken =
        Prelude.Nothing,
      names = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
describeGameSessionQueues_nextToken :: Lens.Lens' DescribeGameSessionQueues (Prelude.Maybe Prelude.Text)
describeGameSessionQueues_nextToken = Lens.lens (\DescribeGameSessionQueues' {nextToken} -> nextToken) (\s@DescribeGameSessionQueues' {} a -> s {nextToken = a} :: DescribeGameSessionQueues)

-- | A list of queue names to retrieve information for. You can use either
-- the queue ID or ARN value. To request settings for all queues, leave
-- this parameter empty.
describeGameSessionQueues_names :: Lens.Lens' DescribeGameSessionQueues (Prelude.Maybe [Prelude.Text])
describeGameSessionQueues_names = Lens.lens (\DescribeGameSessionQueues' {names} -> names) (\s@DescribeGameSessionQueues' {} a -> s {names = a} :: DescribeGameSessionQueues) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages. You can request
-- up to 50 results.
describeGameSessionQueues_limit :: Lens.Lens' DescribeGameSessionQueues (Prelude.Maybe Prelude.Natural)
describeGameSessionQueues_limit = Lens.lens (\DescribeGameSessionQueues' {limit} -> limit) (\s@DescribeGameSessionQueues' {} a -> s {limit = a} :: DescribeGameSessionQueues)

instance Core.AWSPager DescribeGameSessionQueues where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeGameSessionQueuesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeGameSessionQueuesResponse_gameSessionQueues
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeGameSessionQueues_nextToken
          Lens..~ rs
          Lens.^? describeGameSessionQueuesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeGameSessionQueues where
  type
    AWSResponse DescribeGameSessionQueues =
      DescribeGameSessionQueuesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeGameSessionQueuesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "GameSessionQueues"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeGameSessionQueues where
  hashWithSalt _salt DescribeGameSessionQueues' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` names
      `Prelude.hashWithSalt` limit

instance Prelude.NFData DescribeGameSessionQueues where
  rnf DescribeGameSessionQueues' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf names
      `Prelude.seq` Prelude.rnf limit

instance Data.ToHeaders DescribeGameSessionQueues where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.DescribeGameSessionQueues" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeGameSessionQueues where
  toJSON DescribeGameSessionQueues' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Names" Data..=) Prelude.<$> names,
            ("Limit" Data..=) Prelude.<$> limit
          ]
      )

instance Data.ToPath DescribeGameSessionQueues where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeGameSessionQueues where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newDescribeGameSessionQueuesResponse' smart constructor.
data DescribeGameSessionQueuesResponse = DescribeGameSessionQueuesResponse'
  { -- | A token that indicates where to resume retrieving results on the next
    -- call to this operation. If no token is returned, these results represent
    -- the end of the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A collection of objects that describe the requested game session queues.
    gameSessionQueues :: Prelude.Maybe [GameSessionQueue],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGameSessionQueuesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeGameSessionQueuesResponse_nextToken' - A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
--
-- 'gameSessionQueues', 'describeGameSessionQueuesResponse_gameSessionQueues' - A collection of objects that describe the requested game session queues.
--
-- 'httpStatus', 'describeGameSessionQueuesResponse_httpStatus' - The response's http status code.
newDescribeGameSessionQueuesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeGameSessionQueuesResponse
newDescribeGameSessionQueuesResponse pHttpStatus_ =
  DescribeGameSessionQueuesResponse'
    { nextToken =
        Prelude.Nothing,
      gameSessionQueues = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
describeGameSessionQueuesResponse_nextToken :: Lens.Lens' DescribeGameSessionQueuesResponse (Prelude.Maybe Prelude.Text)
describeGameSessionQueuesResponse_nextToken = Lens.lens (\DescribeGameSessionQueuesResponse' {nextToken} -> nextToken) (\s@DescribeGameSessionQueuesResponse' {} a -> s {nextToken = a} :: DescribeGameSessionQueuesResponse)

-- | A collection of objects that describe the requested game session queues.
describeGameSessionQueuesResponse_gameSessionQueues :: Lens.Lens' DescribeGameSessionQueuesResponse (Prelude.Maybe [GameSessionQueue])
describeGameSessionQueuesResponse_gameSessionQueues = Lens.lens (\DescribeGameSessionQueuesResponse' {gameSessionQueues} -> gameSessionQueues) (\s@DescribeGameSessionQueuesResponse' {} a -> s {gameSessionQueues = a} :: DescribeGameSessionQueuesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeGameSessionQueuesResponse_httpStatus :: Lens.Lens' DescribeGameSessionQueuesResponse Prelude.Int
describeGameSessionQueuesResponse_httpStatus = Lens.lens (\DescribeGameSessionQueuesResponse' {httpStatus} -> httpStatus) (\s@DescribeGameSessionQueuesResponse' {} a -> s {httpStatus = a} :: DescribeGameSessionQueuesResponse)

instance
  Prelude.NFData
    DescribeGameSessionQueuesResponse
  where
  rnf DescribeGameSessionQueuesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf gameSessionQueues
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Amazonka.GameLift.ListGameServerGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the GameLift FleetIQ solution and game
-- server groups.__
--
-- Retrieves information on all game servers groups that exist in the
-- current Amazon Web Services account for the selected Region. Use the
-- pagination parameters to retrieve results in a set of sequential
-- segments.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/fleetiqguide/gsg-intro.html GameLift FleetIQ Guide>
--
-- __Related actions__
--
-- CreateGameServerGroup | ListGameServerGroups | DescribeGameServerGroup |
-- UpdateGameServerGroup | DeleteGameServerGroup | ResumeGameServerGroup |
-- SuspendGameServerGroup | DescribeGameServerInstances |
-- <https://docs.aws.amazon.com/gamelift/latest/fleetiqguide/reference-awssdk-fleetiq.html All APIs by task>
--
-- This operation returns paginated results.
module Amazonka.GameLift.ListGameServerGroups
  ( -- * Creating a Request
    ListGameServerGroups (..),
    newListGameServerGroups,

    -- * Request Lenses
    listGameServerGroups_nextToken,
    listGameServerGroups_limit,

    -- * Destructuring the Response
    ListGameServerGroupsResponse (..),
    newListGameServerGroupsResponse,

    -- * Response Lenses
    listGameServerGroupsResponse_nextToken,
    listGameServerGroupsResponse_gameServerGroups,
    listGameServerGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListGameServerGroups' smart constructor.
data ListGameServerGroups = ListGameServerGroups'
  { -- | A token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages.
    limit :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGameServerGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listGameServerGroups_nextToken' - A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
--
-- 'limit', 'listGameServerGroups_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
newListGameServerGroups ::
  ListGameServerGroups
newListGameServerGroups =
  ListGameServerGroups'
    { nextToken = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
listGameServerGroups_nextToken :: Lens.Lens' ListGameServerGroups (Prelude.Maybe Prelude.Text)
listGameServerGroups_nextToken = Lens.lens (\ListGameServerGroups' {nextToken} -> nextToken) (\s@ListGameServerGroups' {} a -> s {nextToken = a} :: ListGameServerGroups)

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
listGameServerGroups_limit :: Lens.Lens' ListGameServerGroups (Prelude.Maybe Prelude.Natural)
listGameServerGroups_limit = Lens.lens (\ListGameServerGroups' {limit} -> limit) (\s@ListGameServerGroups' {} a -> s {limit = a} :: ListGameServerGroups)

instance Core.AWSPager ListGameServerGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listGameServerGroupsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listGameServerGroupsResponse_gameServerGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listGameServerGroups_nextToken
          Lens..~ rs
          Lens.^? listGameServerGroupsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListGameServerGroups where
  type
    AWSResponse ListGameServerGroups =
      ListGameServerGroupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGameServerGroupsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "GameServerGroups"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListGameServerGroups where
  hashWithSalt _salt ListGameServerGroups' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` limit

instance Prelude.NFData ListGameServerGroups where
  rnf ListGameServerGroups' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf limit

instance Data.ToHeaders ListGameServerGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.ListGameServerGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListGameServerGroups where
  toJSON ListGameServerGroups' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Limit" Data..=) Prelude.<$> limit
          ]
      )

instance Data.ToPath ListGameServerGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery ListGameServerGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListGameServerGroupsResponse' smart constructor.
data ListGameServerGroupsResponse = ListGameServerGroupsResponse'
  { -- | A token that indicates where to resume retrieving results on the next
    -- call to this operation. If no token is returned, these results represent
    -- the end of the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A collection of game server group objects that match the request.
    gameServerGroups :: Prelude.Maybe [GameServerGroup],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGameServerGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listGameServerGroupsResponse_nextToken' - A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
--
-- 'gameServerGroups', 'listGameServerGroupsResponse_gameServerGroups' - A collection of game server group objects that match the request.
--
-- 'httpStatus', 'listGameServerGroupsResponse_httpStatus' - The response's http status code.
newListGameServerGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListGameServerGroupsResponse
newListGameServerGroupsResponse pHttpStatus_ =
  ListGameServerGroupsResponse'
    { nextToken =
        Prelude.Nothing,
      gameServerGroups = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
listGameServerGroupsResponse_nextToken :: Lens.Lens' ListGameServerGroupsResponse (Prelude.Maybe Prelude.Text)
listGameServerGroupsResponse_nextToken = Lens.lens (\ListGameServerGroupsResponse' {nextToken} -> nextToken) (\s@ListGameServerGroupsResponse' {} a -> s {nextToken = a} :: ListGameServerGroupsResponse)

-- | A collection of game server group objects that match the request.
listGameServerGroupsResponse_gameServerGroups :: Lens.Lens' ListGameServerGroupsResponse (Prelude.Maybe [GameServerGroup])
listGameServerGroupsResponse_gameServerGroups = Lens.lens (\ListGameServerGroupsResponse' {gameServerGroups} -> gameServerGroups) (\s@ListGameServerGroupsResponse' {} a -> s {gameServerGroups = a} :: ListGameServerGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listGameServerGroupsResponse_httpStatus :: Lens.Lens' ListGameServerGroupsResponse Prelude.Int
listGameServerGroupsResponse_httpStatus = Lens.lens (\ListGameServerGroupsResponse' {httpStatus} -> httpStatus) (\s@ListGameServerGroupsResponse' {} a -> s {httpStatus = a} :: ListGameServerGroupsResponse)

instance Prelude.NFData ListGameServerGroupsResponse where
  rnf ListGameServerGroupsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf gameServerGroups
      `Prelude.seq` Prelude.rnf httpStatus

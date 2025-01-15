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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists a game server groups.
--
-- This operation returns paginated results.
module Amazonka.GameLift.ListGameServerGroups
  ( -- * Creating a Request
    ListGameServerGroups (..),
    newListGameServerGroups,

    -- * Request Lenses
    listGameServerGroups_limit,
    listGameServerGroups_nextToken,

    -- * Destructuring the Response
    ListGameServerGroupsResponse (..),
    newListGameServerGroupsResponse,

    -- * Response Lenses
    listGameServerGroupsResponse_gameServerGroups,
    listGameServerGroupsResponse_nextToken,
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
  { -- | The game server groups\' limit.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | Specify the pagination token from a previous request to retrieve the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'limit', 'listGameServerGroups_limit' - The game server groups\' limit.
--
-- 'nextToken', 'listGameServerGroups_nextToken' - Specify the pagination token from a previous request to retrieve the
-- next page of results.
newListGameServerGroups ::
  ListGameServerGroups
newListGameServerGroups =
  ListGameServerGroups'
    { limit = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The game server groups\' limit.
listGameServerGroups_limit :: Lens.Lens' ListGameServerGroups (Prelude.Maybe Prelude.Natural)
listGameServerGroups_limit = Lens.lens (\ListGameServerGroups' {limit} -> limit) (\s@ListGameServerGroups' {} a -> s {limit = a} :: ListGameServerGroups)

-- | Specify the pagination token from a previous request to retrieve the
-- next page of results.
listGameServerGroups_nextToken :: Lens.Lens' ListGameServerGroups (Prelude.Maybe Prelude.Text)
listGameServerGroups_nextToken = Lens.lens (\ListGameServerGroups' {nextToken} -> nextToken) (\s@ListGameServerGroups' {} a -> s {nextToken = a} :: ListGameServerGroups)

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
            Prelude.<$> ( x
                            Data..?> "GameServerGroups"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListGameServerGroups where
  hashWithSalt _salt ListGameServerGroups' {..} =
    _salt
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListGameServerGroups where
  rnf ListGameServerGroups' {..} =
    Prelude.rnf limit `Prelude.seq`
      Prelude.rnf nextToken

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
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListGameServerGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery ListGameServerGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListGameServerGroupsResponse' smart constructor.
data ListGameServerGroupsResponse = ListGameServerGroupsResponse'
  { -- | The game server groups\' game server groups.
    gameServerGroups :: Prelude.Maybe [GameServerGroup],
    -- | Specify the pagination token from a previous request to retrieve the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'gameServerGroups', 'listGameServerGroupsResponse_gameServerGroups' - The game server groups\' game server groups.
--
-- 'nextToken', 'listGameServerGroupsResponse_nextToken' - Specify the pagination token from a previous request to retrieve the
-- next page of results.
--
-- 'httpStatus', 'listGameServerGroupsResponse_httpStatus' - The response's http status code.
newListGameServerGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListGameServerGroupsResponse
newListGameServerGroupsResponse pHttpStatus_ =
  ListGameServerGroupsResponse'
    { gameServerGroups =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The game server groups\' game server groups.
listGameServerGroupsResponse_gameServerGroups :: Lens.Lens' ListGameServerGroupsResponse (Prelude.Maybe [GameServerGroup])
listGameServerGroupsResponse_gameServerGroups = Lens.lens (\ListGameServerGroupsResponse' {gameServerGroups} -> gameServerGroups) (\s@ListGameServerGroupsResponse' {} a -> s {gameServerGroups = a} :: ListGameServerGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Specify the pagination token from a previous request to retrieve the
-- next page of results.
listGameServerGroupsResponse_nextToken :: Lens.Lens' ListGameServerGroupsResponse (Prelude.Maybe Prelude.Text)
listGameServerGroupsResponse_nextToken = Lens.lens (\ListGameServerGroupsResponse' {nextToken} -> nextToken) (\s@ListGameServerGroupsResponse' {} a -> s {nextToken = a} :: ListGameServerGroupsResponse)

-- | The response's http status code.
listGameServerGroupsResponse_httpStatus :: Lens.Lens' ListGameServerGroupsResponse Prelude.Int
listGameServerGroupsResponse_httpStatus = Lens.lens (\ListGameServerGroupsResponse' {httpStatus} -> httpStatus) (\s@ListGameServerGroupsResponse' {} a -> s {httpStatus = a} :: ListGameServerGroupsResponse)

instance Prelude.NFData ListGameServerGroupsResponse where
  rnf ListGameServerGroupsResponse' {..} =
    Prelude.rnf gameServerGroups `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus

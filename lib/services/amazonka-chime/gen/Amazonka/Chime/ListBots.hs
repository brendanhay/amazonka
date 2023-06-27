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
-- Module      : Amazonka.Chime.ListBots
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the bots associated with the administrator\'s Amazon Chime
-- Enterprise account ID.
module Amazonka.Chime.ListBots
  ( -- * Creating a Request
    ListBots (..),
    newListBots,

    -- * Request Lenses
    listBots_maxResults,
    listBots_nextToken,
    listBots_accountId,

    -- * Destructuring the Response
    ListBotsResponse (..),
    newListBotsResponse,

    -- * Response Lenses
    listBotsResponse_bots,
    listBotsResponse_nextToken,
    listBotsResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListBots' smart constructor.
data ListBots = ListBots'
  { -- | The maximum number of results to return in a single call. The default is
    -- 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to use to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Chime account ID.
    accountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBots' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listBots_maxResults' - The maximum number of results to return in a single call. The default is
-- 10.
--
-- 'nextToken', 'listBots_nextToken' - The token to use to retrieve the next page of results.
--
-- 'accountId', 'listBots_accountId' - The Amazon Chime account ID.
newListBots ::
  -- | 'accountId'
  Prelude.Text ->
  ListBots
newListBots pAccountId_ =
  ListBots'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      accountId = pAccountId_
    }

-- | The maximum number of results to return in a single call. The default is
-- 10.
listBots_maxResults :: Lens.Lens' ListBots (Prelude.Maybe Prelude.Natural)
listBots_maxResults = Lens.lens (\ListBots' {maxResults} -> maxResults) (\s@ListBots' {} a -> s {maxResults = a} :: ListBots)

-- | The token to use to retrieve the next page of results.
listBots_nextToken :: Lens.Lens' ListBots (Prelude.Maybe Prelude.Text)
listBots_nextToken = Lens.lens (\ListBots' {nextToken} -> nextToken) (\s@ListBots' {} a -> s {nextToken = a} :: ListBots)

-- | The Amazon Chime account ID.
listBots_accountId :: Lens.Lens' ListBots Prelude.Text
listBots_accountId = Lens.lens (\ListBots' {accountId} -> accountId) (\s@ListBots' {} a -> s {accountId = a} :: ListBots)

instance Core.AWSRequest ListBots where
  type AWSResponse ListBots = ListBotsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBotsResponse'
            Prelude.<$> (x Data..?> "Bots" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBots where
  hashWithSalt _salt ListBots' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` accountId

instance Prelude.NFData ListBots where
  rnf ListBots' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf accountId

instance Data.ToHeaders ListBots where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListBots where
  toPath ListBots' {..} =
    Prelude.mconcat
      ["/accounts/", Data.toBS accountId, "/bots"]

instance Data.ToQuery ListBots where
  toQuery ListBots' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken
      ]

-- | /See:/ 'newListBotsResponse' smart constructor.
data ListBotsResponse = ListBotsResponse'
  { -- | List of bots and bot details.
    bots :: Prelude.Maybe [Bot],
    -- | The token to use to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBotsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bots', 'listBotsResponse_bots' - List of bots and bot details.
--
-- 'nextToken', 'listBotsResponse_nextToken' - The token to use to retrieve the next page of results.
--
-- 'httpStatus', 'listBotsResponse_httpStatus' - The response's http status code.
newListBotsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBotsResponse
newListBotsResponse pHttpStatus_ =
  ListBotsResponse'
    { bots = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of bots and bot details.
listBotsResponse_bots :: Lens.Lens' ListBotsResponse (Prelude.Maybe [Bot])
listBotsResponse_bots = Lens.lens (\ListBotsResponse' {bots} -> bots) (\s@ListBotsResponse' {} a -> s {bots = a} :: ListBotsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results.
listBotsResponse_nextToken :: Lens.Lens' ListBotsResponse (Prelude.Maybe Prelude.Text)
listBotsResponse_nextToken = Lens.lens (\ListBotsResponse' {nextToken} -> nextToken) (\s@ListBotsResponse' {} a -> s {nextToken = a} :: ListBotsResponse)

-- | The response's http status code.
listBotsResponse_httpStatus :: Lens.Lens' ListBotsResponse Prelude.Int
listBotsResponse_httpStatus = Lens.lens (\ListBotsResponse' {httpStatus} -> httpStatus) (\s@ListBotsResponse' {} a -> s {httpStatus = a} :: ListBotsResponse)

instance Prelude.NFData ListBotsResponse where
  rnf ListBotsResponse' {..} =
    Prelude.rnf bots
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Network.AWS.Connect.ListLexBots
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Returns a paginated list of all the Amazon Lex bots currently associated
-- with the instance.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListLexBots
  ( -- * Creating a Request
    ListLexBots (..),
    newListLexBots,

    -- * Request Lenses
    listLexBots_nextToken,
    listLexBots_maxResults,
    listLexBots_instanceId,

    -- * Destructuring the Response
    ListLexBotsResponse (..),
    newListLexBotsResponse,

    -- * Response Lenses
    listLexBotsResponse_nextToken,
    listLexBotsResponse_lexBots,
    listLexBotsResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListLexBots' smart constructor.
data ListLexBots = ListLexBots'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLexBots' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listLexBots_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'maxResults', 'listLexBots_maxResults' - The maximum number of results to return per page.
--
-- 'instanceId', 'listLexBots_instanceId' - The identifier of the Amazon Connect instance.
newListLexBots ::
  -- | 'instanceId'
  Prelude.Text ->
  ListLexBots
newListLexBots pInstanceId_ =
  ListLexBots'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listLexBots_nextToken :: Lens.Lens' ListLexBots (Prelude.Maybe Prelude.Text)
listLexBots_nextToken = Lens.lens (\ListLexBots' {nextToken} -> nextToken) (\s@ListLexBots' {} a -> s {nextToken = a} :: ListLexBots)

-- | The maximum number of results to return per page.
listLexBots_maxResults :: Lens.Lens' ListLexBots (Prelude.Maybe Prelude.Natural)
listLexBots_maxResults = Lens.lens (\ListLexBots' {maxResults} -> maxResults) (\s@ListLexBots' {} a -> s {maxResults = a} :: ListLexBots)

-- | The identifier of the Amazon Connect instance.
listLexBots_instanceId :: Lens.Lens' ListLexBots Prelude.Text
listLexBots_instanceId = Lens.lens (\ListLexBots' {instanceId} -> instanceId) (\s@ListLexBots' {} a -> s {instanceId = a} :: ListLexBots)

instance Core.AWSPager ListLexBots where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listLexBotsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listLexBotsResponse_lexBots Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listLexBots_nextToken
          Lens..~ rs
          Lens.^? listLexBotsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListLexBots where
  type AWSResponse ListLexBots = ListLexBotsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLexBotsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "LexBots" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLexBots

instance Prelude.NFData ListLexBots

instance Core.ToHeaders ListLexBots where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListLexBots where
  toPath ListLexBots' {..} =
    Prelude.mconcat
      ["/instance/", Core.toBS instanceId, "/lex-bots"]

instance Core.ToQuery ListLexBots where
  toQuery ListLexBots' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListLexBotsResponse' smart constructor.
data ListLexBotsResponse = ListLexBotsResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The names and Regions of the Amazon Lex bots associated with the
    -- specified instance.
    lexBots :: Prelude.Maybe [LexBot],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLexBotsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listLexBotsResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'lexBots', 'listLexBotsResponse_lexBots' - The names and Regions of the Amazon Lex bots associated with the
-- specified instance.
--
-- 'httpStatus', 'listLexBotsResponse_httpStatus' - The response's http status code.
newListLexBotsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLexBotsResponse
newListLexBotsResponse pHttpStatus_ =
  ListLexBotsResponse'
    { nextToken = Prelude.Nothing,
      lexBots = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listLexBotsResponse_nextToken :: Lens.Lens' ListLexBotsResponse (Prelude.Maybe Prelude.Text)
listLexBotsResponse_nextToken = Lens.lens (\ListLexBotsResponse' {nextToken} -> nextToken) (\s@ListLexBotsResponse' {} a -> s {nextToken = a} :: ListLexBotsResponse)

-- | The names and Regions of the Amazon Lex bots associated with the
-- specified instance.
listLexBotsResponse_lexBots :: Lens.Lens' ListLexBotsResponse (Prelude.Maybe [LexBot])
listLexBotsResponse_lexBots = Lens.lens (\ListLexBotsResponse' {lexBots} -> lexBots) (\s@ListLexBotsResponse' {} a -> s {lexBots = a} :: ListLexBotsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listLexBotsResponse_httpStatus :: Lens.Lens' ListLexBotsResponse Prelude.Int
listLexBotsResponse_httpStatus = Lens.lens (\ListLexBotsResponse' {httpStatus} -> httpStatus) (\s@ListLexBotsResponse' {} a -> s {httpStatus = a} :: ListLexBotsResponse)

instance Prelude.NFData ListLexBotsResponse

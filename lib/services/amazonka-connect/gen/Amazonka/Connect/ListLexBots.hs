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
-- Module      : Amazonka.Connect.ListLexBots
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Returns a paginated list of all the Amazon Lex V1 bots currently
-- associated with the instance. To return both Amazon Lex V1 and V2 bots,
-- use the
-- <https://docs.aws.amazon.com/connect/latest/APIReference/API_ListBots.html ListBots>
-- API.
--
-- This operation returns paginated results.
module Amazonka.Connect.ListLexBots
  ( -- * Creating a Request
    ListLexBots (..),
    newListLexBots,

    -- * Request Lenses
    listLexBots_maxResults,
    listLexBots_nextToken,
    listLexBots_instanceId,

    -- * Destructuring the Response
    ListLexBotsResponse (..),
    newListLexBotsResponse,

    -- * Response Lenses
    listLexBotsResponse_lexBots,
    listLexBotsResponse_nextToken,
    listLexBotsResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListLexBots' smart constructor.
data ListLexBots = ListLexBots'
  { -- | The maximum number of results to return per page. If no value is
    -- specified, the default is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
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
-- 'maxResults', 'listLexBots_maxResults' - The maximum number of results to return per page. If no value is
-- specified, the default is 10.
--
-- 'nextToken', 'listLexBots_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'instanceId', 'listLexBots_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
newListLexBots ::
  -- | 'instanceId'
  Prelude.Text ->
  ListLexBots
newListLexBots pInstanceId_ =
  ListLexBots'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | The maximum number of results to return per page. If no value is
-- specified, the default is 10.
listLexBots_maxResults :: Lens.Lens' ListLexBots (Prelude.Maybe Prelude.Natural)
listLexBots_maxResults = Lens.lens (\ListLexBots' {maxResults} -> maxResults) (\s@ListLexBots' {} a -> s {maxResults = a} :: ListLexBots)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listLexBots_nextToken :: Lens.Lens' ListLexBots (Prelude.Maybe Prelude.Text)
listLexBots_nextToken = Lens.lens (\ListLexBots' {nextToken} -> nextToken) (\s@ListLexBots' {} a -> s {nextToken = a} :: ListLexBots)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLexBotsResponse'
            Prelude.<$> (x Data..?> "LexBots" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLexBots where
  hashWithSalt _salt ListLexBots' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData ListLexBots where
  rnf ListLexBots' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToHeaders ListLexBots where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListLexBots where
  toPath ListLexBots' {..} =
    Prelude.mconcat
      ["/instance/", Data.toBS instanceId, "/lex-bots"]

instance Data.ToQuery ListLexBots where
  toQuery ListLexBots' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListLexBotsResponse' smart constructor.
data ListLexBotsResponse = ListLexBotsResponse'
  { -- | The names and Amazon Web Services Regions of the Amazon Lex bots
    -- associated with the specified instance.
    lexBots :: Prelude.Maybe [LexBot],
    -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'lexBots', 'listLexBotsResponse_lexBots' - The names and Amazon Web Services Regions of the Amazon Lex bots
-- associated with the specified instance.
--
-- 'nextToken', 'listLexBotsResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'httpStatus', 'listLexBotsResponse_httpStatus' - The response's http status code.
newListLexBotsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLexBotsResponse
newListLexBotsResponse pHttpStatus_ =
  ListLexBotsResponse'
    { lexBots = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The names and Amazon Web Services Regions of the Amazon Lex bots
-- associated with the specified instance.
listLexBotsResponse_lexBots :: Lens.Lens' ListLexBotsResponse (Prelude.Maybe [LexBot])
listLexBotsResponse_lexBots = Lens.lens (\ListLexBotsResponse' {lexBots} -> lexBots) (\s@ListLexBotsResponse' {} a -> s {lexBots = a} :: ListLexBotsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If there are additional results, this is the token for the next set of
-- results.
listLexBotsResponse_nextToken :: Lens.Lens' ListLexBotsResponse (Prelude.Maybe Prelude.Text)
listLexBotsResponse_nextToken = Lens.lens (\ListLexBotsResponse' {nextToken} -> nextToken) (\s@ListLexBotsResponse' {} a -> s {nextToken = a} :: ListLexBotsResponse)

-- | The response's http status code.
listLexBotsResponse_httpStatus :: Lens.Lens' ListLexBotsResponse Prelude.Int
listLexBotsResponse_httpStatus = Lens.lens (\ListLexBotsResponse' {httpStatus} -> httpStatus) (\s@ListLexBotsResponse' {} a -> s {httpStatus = a} :: ListLexBotsResponse)

instance Prelude.NFData ListLexBotsResponse where
  rnf ListLexBotsResponse' {..} =
    Prelude.rnf lexBots
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

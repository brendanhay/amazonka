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
-- Module      : Amazonka.Connect.ListBots
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- For the specified version of Amazon Lex, returns a paginated list of all
-- the Amazon Lex bots currently associated with the instance. Use this API
-- to returns both Amazon Lex V1 and V2 bots.
--
-- This operation returns paginated results.
module Amazonka.Connect.ListBots
  ( -- * Creating a Request
    ListBots (..),
    newListBots,

    -- * Request Lenses
    listBots_maxResults,
    listBots_nextToken,
    listBots_instanceId,
    listBots_lexVersion,

    -- * Destructuring the Response
    ListBotsResponse (..),
    newListBotsResponse,

    -- * Response Lenses
    listBotsResponse_lexBots,
    listBotsResponse_nextToken,
    listBotsResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListBots' smart constructor.
data ListBots = ListBots'
  { -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The version of Amazon Lex or Amazon Lex V2.
    lexVersion :: LexVersion
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
-- 'maxResults', 'listBots_maxResults' - The maximum number of results to return per page.
--
-- 'nextToken', 'listBots_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'instanceId', 'listBots_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'lexVersion', 'listBots_lexVersion' - The version of Amazon Lex or Amazon Lex V2.
newListBots ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'lexVersion'
  LexVersion ->
  ListBots
newListBots pInstanceId_ pLexVersion_ =
  ListBots'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      instanceId = pInstanceId_,
      lexVersion = pLexVersion_
    }

-- | The maximum number of results to return per page.
listBots_maxResults :: Lens.Lens' ListBots (Prelude.Maybe Prelude.Natural)
listBots_maxResults = Lens.lens (\ListBots' {maxResults} -> maxResults) (\s@ListBots' {} a -> s {maxResults = a} :: ListBots)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listBots_nextToken :: Lens.Lens' ListBots (Prelude.Maybe Prelude.Text)
listBots_nextToken = Lens.lens (\ListBots' {nextToken} -> nextToken) (\s@ListBots' {} a -> s {nextToken = a} :: ListBots)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
listBots_instanceId :: Lens.Lens' ListBots Prelude.Text
listBots_instanceId = Lens.lens (\ListBots' {instanceId} -> instanceId) (\s@ListBots' {} a -> s {instanceId = a} :: ListBots)

-- | The version of Amazon Lex or Amazon Lex V2.
listBots_lexVersion :: Lens.Lens' ListBots LexVersion
listBots_lexVersion = Lens.lens (\ListBots' {lexVersion} -> lexVersion) (\s@ListBots' {} a -> s {lexVersion = a} :: ListBots)

instance Core.AWSPager ListBots where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBotsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listBotsResponse_lexBots
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listBots_nextToken
          Lens..~ rs
          Lens.^? listBotsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListBots where
  type AWSResponse ListBots = ListBotsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBotsResponse'
            Prelude.<$> (x Data..?> "LexBots" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBots where
  hashWithSalt _salt ListBots' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` lexVersion

instance Prelude.NFData ListBots where
  rnf ListBots' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf lexVersion

instance Data.ToHeaders ListBots where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListBots where
  toPath ListBots' {..} =
    Prelude.mconcat
      ["/instance/", Data.toBS instanceId, "/bots"]

instance Data.ToQuery ListBots where
  toQuery ListBots' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "lexVersion" Data.=: lexVersion
      ]

-- | /See:/ 'newListBotsResponse' smart constructor.
data ListBotsResponse = ListBotsResponse'
  { -- | The names and Amazon Web Services Regions of the Amazon Lex or Amazon
    -- Lex V2 bots associated with the specified instance.
    lexBots :: Prelude.Maybe [LexBotConfig],
    -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBotsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lexBots', 'listBotsResponse_lexBots' - The names and Amazon Web Services Regions of the Amazon Lex or Amazon
-- Lex V2 bots associated with the specified instance.
--
-- 'nextToken', 'listBotsResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'httpStatus', 'listBotsResponse_httpStatus' - The response's http status code.
newListBotsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBotsResponse
newListBotsResponse pHttpStatus_ =
  ListBotsResponse'
    { lexBots = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The names and Amazon Web Services Regions of the Amazon Lex or Amazon
-- Lex V2 bots associated with the specified instance.
listBotsResponse_lexBots :: Lens.Lens' ListBotsResponse (Prelude.Maybe [LexBotConfig])
listBotsResponse_lexBots = Lens.lens (\ListBotsResponse' {lexBots} -> lexBots) (\s@ListBotsResponse' {} a -> s {lexBots = a} :: ListBotsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If there are additional results, this is the token for the next set of
-- results.
listBotsResponse_nextToken :: Lens.Lens' ListBotsResponse (Prelude.Maybe Prelude.Text)
listBotsResponse_nextToken = Lens.lens (\ListBotsResponse' {nextToken} -> nextToken) (\s@ListBotsResponse' {} a -> s {nextToken = a} :: ListBotsResponse)

-- | The response's http status code.
listBotsResponse_httpStatus :: Lens.Lens' ListBotsResponse Prelude.Int
listBotsResponse_httpStatus = Lens.lens (\ListBotsResponse' {httpStatus} -> httpStatus) (\s@ListBotsResponse' {} a -> s {httpStatus = a} :: ListBotsResponse)

instance Prelude.NFData ListBotsResponse where
  rnf ListBotsResponse' {..} =
    Prelude.rnf lexBots
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

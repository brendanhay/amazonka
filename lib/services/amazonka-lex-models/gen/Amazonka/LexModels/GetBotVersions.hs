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
-- Module      : Amazonka.LexModels.GetBotVersions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about all of the versions of a bot.
--
-- The @GetBotVersions@ operation returns a @BotMetadata@ object for each
-- version of a bot. For example, if a bot has three numbered versions, the
-- @GetBotVersions@ operation returns four @BotMetadata@ objects in the
-- response, one for each numbered version and one for the @$LATEST@
-- version.
--
-- The @GetBotVersions@ operation always returns at least one version, the
-- @$LATEST@ version.
--
-- This operation requires permissions for the @lex:GetBotVersions@ action.
--
-- This operation returns paginated results.
module Amazonka.LexModels.GetBotVersions
  ( -- * Creating a Request
    GetBotVersions (..),
    newGetBotVersions,

    -- * Request Lenses
    getBotVersions_maxResults,
    getBotVersions_nextToken,
    getBotVersions_name,

    -- * Destructuring the Response
    GetBotVersionsResponse (..),
    newGetBotVersionsResponse,

    -- * Response Lenses
    getBotVersionsResponse_bots,
    getBotVersionsResponse_nextToken,
    getBotVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexModels.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetBotVersions' smart constructor.
data GetBotVersions = GetBotVersions'
  { -- | The maximum number of bot versions to return in the response. The
    -- default is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A pagination token for fetching the next page of bot versions. If the
    -- response to this call is truncated, Amazon Lex returns a pagination
    -- token in the response. To fetch the next page of versions, specify the
    -- pagination token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the bot for which versions should be returned.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBotVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getBotVersions_maxResults' - The maximum number of bot versions to return in the response. The
-- default is 10.
--
-- 'nextToken', 'getBotVersions_nextToken' - A pagination token for fetching the next page of bot versions. If the
-- response to this call is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch the next page of versions, specify the
-- pagination token in the next request.
--
-- 'name', 'getBotVersions_name' - The name of the bot for which versions should be returned.
newGetBotVersions ::
  -- | 'name'
  Prelude.Text ->
  GetBotVersions
newGetBotVersions pName_ =
  GetBotVersions'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      name = pName_
    }

-- | The maximum number of bot versions to return in the response. The
-- default is 10.
getBotVersions_maxResults :: Lens.Lens' GetBotVersions (Prelude.Maybe Prelude.Natural)
getBotVersions_maxResults = Lens.lens (\GetBotVersions' {maxResults} -> maxResults) (\s@GetBotVersions' {} a -> s {maxResults = a} :: GetBotVersions)

-- | A pagination token for fetching the next page of bot versions. If the
-- response to this call is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch the next page of versions, specify the
-- pagination token in the next request.
getBotVersions_nextToken :: Lens.Lens' GetBotVersions (Prelude.Maybe Prelude.Text)
getBotVersions_nextToken = Lens.lens (\GetBotVersions' {nextToken} -> nextToken) (\s@GetBotVersions' {} a -> s {nextToken = a} :: GetBotVersions)

-- | The name of the bot for which versions should be returned.
getBotVersions_name :: Lens.Lens' GetBotVersions Prelude.Text
getBotVersions_name = Lens.lens (\GetBotVersions' {name} -> name) (\s@GetBotVersions' {} a -> s {name = a} :: GetBotVersions)

instance Core.AWSPager GetBotVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getBotVersionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getBotVersionsResponse_bots
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& getBotVersions_nextToken
              Lens..~ rs
              Lens.^? getBotVersionsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest GetBotVersions where
  type
    AWSResponse GetBotVersions =
      GetBotVersionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBotVersionsResponse'
            Prelude.<$> (x Data..?> "bots" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBotVersions where
  hashWithSalt _salt GetBotVersions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` name

instance Prelude.NFData GetBotVersions where
  rnf GetBotVersions' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf name

instance Data.ToHeaders GetBotVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetBotVersions where
  toPath GetBotVersions' {..} =
    Prelude.mconcat
      ["/bots/", Data.toBS name, "/versions/"]

instance Data.ToQuery GetBotVersions where
  toQuery GetBotVersions' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newGetBotVersionsResponse' smart constructor.
data GetBotVersionsResponse = GetBotVersionsResponse'
  { -- | An array of @BotMetadata@ objects, one for each numbered version of the
    -- bot plus one for the @$LATEST@ version.
    bots :: Prelude.Maybe [BotMetadata],
    -- | A pagination token for fetching the next page of bot versions. If the
    -- response to this call is truncated, Amazon Lex returns a pagination
    -- token in the response. To fetch the next page of versions, specify the
    -- pagination token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBotVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bots', 'getBotVersionsResponse_bots' - An array of @BotMetadata@ objects, one for each numbered version of the
-- bot plus one for the @$LATEST@ version.
--
-- 'nextToken', 'getBotVersionsResponse_nextToken' - A pagination token for fetching the next page of bot versions. If the
-- response to this call is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch the next page of versions, specify the
-- pagination token in the next request.
--
-- 'httpStatus', 'getBotVersionsResponse_httpStatus' - The response's http status code.
newGetBotVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBotVersionsResponse
newGetBotVersionsResponse pHttpStatus_ =
  GetBotVersionsResponse'
    { bots = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @BotMetadata@ objects, one for each numbered version of the
-- bot plus one for the @$LATEST@ version.
getBotVersionsResponse_bots :: Lens.Lens' GetBotVersionsResponse (Prelude.Maybe [BotMetadata])
getBotVersionsResponse_bots = Lens.lens (\GetBotVersionsResponse' {bots} -> bots) (\s@GetBotVersionsResponse' {} a -> s {bots = a} :: GetBotVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A pagination token for fetching the next page of bot versions. If the
-- response to this call is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch the next page of versions, specify the
-- pagination token in the next request.
getBotVersionsResponse_nextToken :: Lens.Lens' GetBotVersionsResponse (Prelude.Maybe Prelude.Text)
getBotVersionsResponse_nextToken = Lens.lens (\GetBotVersionsResponse' {nextToken} -> nextToken) (\s@GetBotVersionsResponse' {} a -> s {nextToken = a} :: GetBotVersionsResponse)

-- | The response's http status code.
getBotVersionsResponse_httpStatus :: Lens.Lens' GetBotVersionsResponse Prelude.Int
getBotVersionsResponse_httpStatus = Lens.lens (\GetBotVersionsResponse' {httpStatus} -> httpStatus) (\s@GetBotVersionsResponse' {} a -> s {httpStatus = a} :: GetBotVersionsResponse)

instance Prelude.NFData GetBotVersionsResponse where
  rnf GetBotVersionsResponse' {..} =
    Prelude.rnf bots `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus

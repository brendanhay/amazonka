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
-- Module      : Network.AWS.LexModels.GetBotAliases
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of aliases for a specified Amazon Lex bot.
--
-- This operation requires permissions for the @lex:GetBotAliases@ action.
--
-- This operation returns paginated results.
module Network.AWS.LexModels.GetBotAliases
  ( -- * Creating a Request
    GetBotAliases (..),
    newGetBotAliases,

    -- * Request Lenses
    getBotAliases_nextToken,
    getBotAliases_nameContains,
    getBotAliases_maxResults,
    getBotAliases_botName,

    -- * Destructuring the Response
    GetBotAliasesResponse (..),
    newGetBotAliasesResponse,

    -- * Response Lenses
    getBotAliasesResponse_nextToken,
    getBotAliasesResponse_botAliases,
    getBotAliasesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetBotAliases' smart constructor.
data GetBotAliases = GetBotAliases'
  { -- | A pagination token for fetching the next page of aliases. If the
    -- response to this call is truncated, Amazon Lex returns a pagination
    -- token in the response. To fetch the next page of aliases, specify the
    -- pagination token in the next request.
    nextToken :: Core.Maybe Core.Text,
    -- | Substring to match in bot alias names. An alias will be returned if any
    -- part of its name matches the substring. For example, \"xyz\" matches
    -- both \"xyzabc\" and \"abcxyz.\"
    nameContains :: Core.Maybe Core.Text,
    -- | The maximum number of aliases to return in the response. The default is
    -- 50. .
    maxResults :: Core.Maybe Core.Natural,
    -- | The name of the bot.
    botName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetBotAliases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getBotAliases_nextToken' - A pagination token for fetching the next page of aliases. If the
-- response to this call is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch the next page of aliases, specify the
-- pagination token in the next request.
--
-- 'nameContains', 'getBotAliases_nameContains' - Substring to match in bot alias names. An alias will be returned if any
-- part of its name matches the substring. For example, \"xyz\" matches
-- both \"xyzabc\" and \"abcxyz.\"
--
-- 'maxResults', 'getBotAliases_maxResults' - The maximum number of aliases to return in the response. The default is
-- 50. .
--
-- 'botName', 'getBotAliases_botName' - The name of the bot.
newGetBotAliases ::
  -- | 'botName'
  Core.Text ->
  GetBotAliases
newGetBotAliases pBotName_ =
  GetBotAliases'
    { nextToken = Core.Nothing,
      nameContains = Core.Nothing,
      maxResults = Core.Nothing,
      botName = pBotName_
    }

-- | A pagination token for fetching the next page of aliases. If the
-- response to this call is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch the next page of aliases, specify the
-- pagination token in the next request.
getBotAliases_nextToken :: Lens.Lens' GetBotAliases (Core.Maybe Core.Text)
getBotAliases_nextToken = Lens.lens (\GetBotAliases' {nextToken} -> nextToken) (\s@GetBotAliases' {} a -> s {nextToken = a} :: GetBotAliases)

-- | Substring to match in bot alias names. An alias will be returned if any
-- part of its name matches the substring. For example, \"xyz\" matches
-- both \"xyzabc\" and \"abcxyz.\"
getBotAliases_nameContains :: Lens.Lens' GetBotAliases (Core.Maybe Core.Text)
getBotAliases_nameContains = Lens.lens (\GetBotAliases' {nameContains} -> nameContains) (\s@GetBotAliases' {} a -> s {nameContains = a} :: GetBotAliases)

-- | The maximum number of aliases to return in the response. The default is
-- 50. .
getBotAliases_maxResults :: Lens.Lens' GetBotAliases (Core.Maybe Core.Natural)
getBotAliases_maxResults = Lens.lens (\GetBotAliases' {maxResults} -> maxResults) (\s@GetBotAliases' {} a -> s {maxResults = a} :: GetBotAliases)

-- | The name of the bot.
getBotAliases_botName :: Lens.Lens' GetBotAliases Core.Text
getBotAliases_botName = Lens.lens (\GetBotAliases' {botName} -> botName) (\s@GetBotAliases' {} a -> s {botName = a} :: GetBotAliases)

instance Core.AWSPager GetBotAliases where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getBotAliasesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getBotAliasesResponse_botAliases Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getBotAliases_nextToken
          Lens..~ rs
          Lens.^? getBotAliasesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest GetBotAliases where
  type
    AWSResponse GetBotAliases =
      GetBotAliasesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBotAliasesResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "BotAliases" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetBotAliases

instance Core.NFData GetBotAliases

instance Core.ToHeaders GetBotAliases where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetBotAliases where
  toPath GetBotAliases' {..} =
    Core.mconcat
      ["/bots/", Core.toBS botName, "/aliases/"]

instance Core.ToQuery GetBotAliases where
  toQuery GetBotAliases' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "nameContains" Core.=: nameContains,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newGetBotAliasesResponse' smart constructor.
data GetBotAliasesResponse = GetBotAliasesResponse'
  { -- | A pagination token for fetching next page of aliases. If the response to
    -- this call is truncated, Amazon Lex returns a pagination token in the
    -- response. To fetch the next page of aliases, specify the pagination
    -- token in the next request.
    nextToken :: Core.Maybe Core.Text,
    -- | An array of @BotAliasMetadata@ objects, each describing a bot alias.
    botAliases :: Core.Maybe [BotAliasMetadata],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetBotAliasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getBotAliasesResponse_nextToken' - A pagination token for fetching next page of aliases. If the response to
-- this call is truncated, Amazon Lex returns a pagination token in the
-- response. To fetch the next page of aliases, specify the pagination
-- token in the next request.
--
-- 'botAliases', 'getBotAliasesResponse_botAliases' - An array of @BotAliasMetadata@ objects, each describing a bot alias.
--
-- 'httpStatus', 'getBotAliasesResponse_httpStatus' - The response's http status code.
newGetBotAliasesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetBotAliasesResponse
newGetBotAliasesResponse pHttpStatus_ =
  GetBotAliasesResponse'
    { nextToken = Core.Nothing,
      botAliases = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A pagination token for fetching next page of aliases. If the response to
-- this call is truncated, Amazon Lex returns a pagination token in the
-- response. To fetch the next page of aliases, specify the pagination
-- token in the next request.
getBotAliasesResponse_nextToken :: Lens.Lens' GetBotAliasesResponse (Core.Maybe Core.Text)
getBotAliasesResponse_nextToken = Lens.lens (\GetBotAliasesResponse' {nextToken} -> nextToken) (\s@GetBotAliasesResponse' {} a -> s {nextToken = a} :: GetBotAliasesResponse)

-- | An array of @BotAliasMetadata@ objects, each describing a bot alias.
getBotAliasesResponse_botAliases :: Lens.Lens' GetBotAliasesResponse (Core.Maybe [BotAliasMetadata])
getBotAliasesResponse_botAliases = Lens.lens (\GetBotAliasesResponse' {botAliases} -> botAliases) (\s@GetBotAliasesResponse' {} a -> s {botAliases = a} :: GetBotAliasesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getBotAliasesResponse_httpStatus :: Lens.Lens' GetBotAliasesResponse Core.Int
getBotAliasesResponse_httpStatus = Lens.lens (\GetBotAliasesResponse' {httpStatus} -> httpStatus) (\s@GetBotAliasesResponse' {} a -> s {httpStatus = a} :: GetBotAliasesResponse)

instance Core.NFData GetBotAliasesResponse

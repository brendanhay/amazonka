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
-- Module      : Network.AWS.LexModels.GetBotVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.LexModels.GetBotVersions
  ( -- * Creating a Request
    GetBotVersions (..),
    newGetBotVersions,

    -- * Request Lenses
    getBotVersions_nextToken,
    getBotVersions_maxResults,
    getBotVersions_name,

    -- * Destructuring the Response
    GetBotVersionsResponse (..),
    newGetBotVersionsResponse,

    -- * Response Lenses
    getBotVersionsResponse_nextToken,
    getBotVersionsResponse_bots,
    getBotVersionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetBotVersions' smart constructor.
data GetBotVersions = GetBotVersions'
  { -- | A pagination token for fetching the next page of bot versions. If the
    -- response to this call is truncated, Amazon Lex returns a pagination
    -- token in the response. To fetch the next page of versions, specify the
    -- pagination token in the next request.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of bot versions to return in the response. The
    -- default is 10.
    maxResults :: Core.Maybe Core.Natural,
    -- | The name of the bot for which versions should be returned.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetBotVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getBotVersions_nextToken' - A pagination token for fetching the next page of bot versions. If the
-- response to this call is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch the next page of versions, specify the
-- pagination token in the next request.
--
-- 'maxResults', 'getBotVersions_maxResults' - The maximum number of bot versions to return in the response. The
-- default is 10.
--
-- 'name', 'getBotVersions_name' - The name of the bot for which versions should be returned.
newGetBotVersions ::
  -- | 'name'
  Core.Text ->
  GetBotVersions
newGetBotVersions pName_ =
  GetBotVersions'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      name = pName_
    }

-- | A pagination token for fetching the next page of bot versions. If the
-- response to this call is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch the next page of versions, specify the
-- pagination token in the next request.
getBotVersions_nextToken :: Lens.Lens' GetBotVersions (Core.Maybe Core.Text)
getBotVersions_nextToken = Lens.lens (\GetBotVersions' {nextToken} -> nextToken) (\s@GetBotVersions' {} a -> s {nextToken = a} :: GetBotVersions)

-- | The maximum number of bot versions to return in the response. The
-- default is 10.
getBotVersions_maxResults :: Lens.Lens' GetBotVersions (Core.Maybe Core.Natural)
getBotVersions_maxResults = Lens.lens (\GetBotVersions' {maxResults} -> maxResults) (\s@GetBotVersions' {} a -> s {maxResults = a} :: GetBotVersions)

-- | The name of the bot for which versions should be returned.
getBotVersions_name :: Lens.Lens' GetBotVersions Core.Text
getBotVersions_name = Lens.lens (\GetBotVersions' {name} -> name) (\s@GetBotVersions' {} a -> s {name = a} :: GetBotVersions)

instance Core.AWSPager GetBotVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getBotVersionsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getBotVersionsResponse_bots Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getBotVersions_nextToken
          Lens..~ rs
          Lens.^? getBotVersionsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest GetBotVersions where
  type
    AWSResponse GetBotVersions =
      GetBotVersionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBotVersionsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "bots" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetBotVersions

instance Core.NFData GetBotVersions

instance Core.ToHeaders GetBotVersions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetBotVersions where
  toPath GetBotVersions' {..} =
    Core.mconcat
      ["/bots/", Core.toBS name, "/versions/"]

instance Core.ToQuery GetBotVersions where
  toQuery GetBotVersions' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newGetBotVersionsResponse' smart constructor.
data GetBotVersionsResponse = GetBotVersionsResponse'
  { -- | A pagination token for fetching the next page of bot versions. If the
    -- response to this call is truncated, Amazon Lex returns a pagination
    -- token in the response. To fetch the next page of versions, specify the
    -- pagination token in the next request.
    nextToken :: Core.Maybe Core.Text,
    -- | An array of @BotMetadata@ objects, one for each numbered version of the
    -- bot plus one for the @$LATEST@ version.
    bots :: Core.Maybe [BotMetadata],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetBotVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getBotVersionsResponse_nextToken' - A pagination token for fetching the next page of bot versions. If the
-- response to this call is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch the next page of versions, specify the
-- pagination token in the next request.
--
-- 'bots', 'getBotVersionsResponse_bots' - An array of @BotMetadata@ objects, one for each numbered version of the
-- bot plus one for the @$LATEST@ version.
--
-- 'httpStatus', 'getBotVersionsResponse_httpStatus' - The response's http status code.
newGetBotVersionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetBotVersionsResponse
newGetBotVersionsResponse pHttpStatus_ =
  GetBotVersionsResponse'
    { nextToken = Core.Nothing,
      bots = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A pagination token for fetching the next page of bot versions. If the
-- response to this call is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch the next page of versions, specify the
-- pagination token in the next request.
getBotVersionsResponse_nextToken :: Lens.Lens' GetBotVersionsResponse (Core.Maybe Core.Text)
getBotVersionsResponse_nextToken = Lens.lens (\GetBotVersionsResponse' {nextToken} -> nextToken) (\s@GetBotVersionsResponse' {} a -> s {nextToken = a} :: GetBotVersionsResponse)

-- | An array of @BotMetadata@ objects, one for each numbered version of the
-- bot plus one for the @$LATEST@ version.
getBotVersionsResponse_bots :: Lens.Lens' GetBotVersionsResponse (Core.Maybe [BotMetadata])
getBotVersionsResponse_bots = Lens.lens (\GetBotVersionsResponse' {bots} -> bots) (\s@GetBotVersionsResponse' {} a -> s {bots = a} :: GetBotVersionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getBotVersionsResponse_httpStatus :: Lens.Lens' GetBotVersionsResponse Core.Int
getBotVersionsResponse_httpStatus = Lens.lens (\GetBotVersionsResponse' {httpStatus} -> httpStatus) (\s@GetBotVersionsResponse' {} a -> s {httpStatus = a} :: GetBotVersionsResponse)

instance Core.NFData GetBotVersionsResponse

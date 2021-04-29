{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetBotVersions' smart constructor.
data GetBotVersions = GetBotVersions'
  { -- | A pagination token for fetching the next page of bot versions. If the
    -- response to this call is truncated, Amazon Lex returns a pagination
    -- token in the response. To fetch the next page of versions, specify the
    -- pagination token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of bot versions to return in the response. The
    -- default is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the bot for which versions should be returned.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  GetBotVersions
newGetBotVersions pName_ =
  GetBotVersions'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      name = pName_
    }

-- | A pagination token for fetching the next page of bot versions. If the
-- response to this call is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch the next page of versions, specify the
-- pagination token in the next request.
getBotVersions_nextToken :: Lens.Lens' GetBotVersions (Prelude.Maybe Prelude.Text)
getBotVersions_nextToken = Lens.lens (\GetBotVersions' {nextToken} -> nextToken) (\s@GetBotVersions' {} a -> s {nextToken = a} :: GetBotVersions)

-- | The maximum number of bot versions to return in the response. The
-- default is 10.
getBotVersions_maxResults :: Lens.Lens' GetBotVersions (Prelude.Maybe Prelude.Natural)
getBotVersions_maxResults = Lens.lens (\GetBotVersions' {maxResults} -> maxResults) (\s@GetBotVersions' {} a -> s {maxResults = a} :: GetBotVersions)

-- | The name of the bot for which versions should be returned.
getBotVersions_name :: Lens.Lens' GetBotVersions Prelude.Text
getBotVersions_name = Lens.lens (\GetBotVersions' {name} -> name) (\s@GetBotVersions' {} a -> s {name = a} :: GetBotVersions)

instance Pager.AWSPager GetBotVersions where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? getBotVersionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? getBotVersionsResponse_bots Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& getBotVersions_nextToken
          Lens..~ rs
          Lens.^? getBotVersionsResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest GetBotVersions where
  type Rs GetBotVersions = GetBotVersionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBotVersionsResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> (x Prelude..?> "bots" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBotVersions

instance Prelude.NFData GetBotVersions

instance Prelude.ToHeaders GetBotVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath GetBotVersions where
  toPath GetBotVersions' {..} =
    Prelude.mconcat
      ["/bots/", Prelude.toBS name, "/versions/"]

instance Prelude.ToQuery GetBotVersions where
  toQuery GetBotVersions' {..} =
    Prelude.mconcat
      [ "nextToken" Prelude.=: nextToken,
        "maxResults" Prelude.=: maxResults
      ]

-- | /See:/ 'newGetBotVersionsResponse' smart constructor.
data GetBotVersionsResponse = GetBotVersionsResponse'
  { -- | A pagination token for fetching the next page of bot versions. If the
    -- response to this call is truncated, Amazon Lex returns a pagination
    -- token in the response. To fetch the next page of versions, specify the
    -- pagination token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of @BotMetadata@ objects, one for each numbered version of the
    -- bot plus one for the @$LATEST@ version.
    bots :: Prelude.Maybe [BotMetadata],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetBotVersionsResponse
newGetBotVersionsResponse pHttpStatus_ =
  GetBotVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      bots = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A pagination token for fetching the next page of bot versions. If the
-- response to this call is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch the next page of versions, specify the
-- pagination token in the next request.
getBotVersionsResponse_nextToken :: Lens.Lens' GetBotVersionsResponse (Prelude.Maybe Prelude.Text)
getBotVersionsResponse_nextToken = Lens.lens (\GetBotVersionsResponse' {nextToken} -> nextToken) (\s@GetBotVersionsResponse' {} a -> s {nextToken = a} :: GetBotVersionsResponse)

-- | An array of @BotMetadata@ objects, one for each numbered version of the
-- bot plus one for the @$LATEST@ version.
getBotVersionsResponse_bots :: Lens.Lens' GetBotVersionsResponse (Prelude.Maybe [BotMetadata])
getBotVersionsResponse_bots = Lens.lens (\GetBotVersionsResponse' {bots} -> bots) (\s@GetBotVersionsResponse' {} a -> s {bots = a} :: GetBotVersionsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
getBotVersionsResponse_httpStatus :: Lens.Lens' GetBotVersionsResponse Prelude.Int
getBotVersionsResponse_httpStatus = Lens.lens (\GetBotVersionsResponse' {httpStatus} -> httpStatus) (\s@GetBotVersionsResponse' {} a -> s {httpStatus = a} :: GetBotVersionsResponse)

instance Prelude.NFData GetBotVersionsResponse

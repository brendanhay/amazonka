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
-- Module      : Amazonka.LexModels.GetIntents
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns intent information as follows:
--
-- -   If you specify the @nameContains@ field, returns the @$LATEST@
--     version of all intents that contain the specified string.
--
-- -   If you don\'t specify the @nameContains@ field, returns information
--     about the @$LATEST@ version of all intents.
--
-- The operation requires permission for the @lex:GetIntents@ action.
--
-- This operation returns paginated results.
module Amazonka.LexModels.GetIntents
  ( -- * Creating a Request
    GetIntents (..),
    newGetIntents,

    -- * Request Lenses
    getIntents_nextToken,
    getIntents_nameContains,
    getIntents_maxResults,

    -- * Destructuring the Response
    GetIntentsResponse (..),
    newGetIntentsResponse,

    -- * Response Lenses
    getIntentsResponse_nextToken,
    getIntentsResponse_intents,
    getIntentsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexModels.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetIntents' smart constructor.
data GetIntents = GetIntents'
  { -- | A pagination token that fetches the next page of intents. If the
    -- response to this API call is truncated, Amazon Lex returns a pagination
    -- token in the response. To fetch the next page of intents, specify the
    -- pagination token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Substring to match in intent names. An intent will be returned if any
    -- part of its name matches the substring. For example, \"xyz\" matches
    -- both \"xyzabc\" and \"abcxyz.\"
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of intents to return in the response. The default is
    -- 10.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIntents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getIntents_nextToken' - A pagination token that fetches the next page of intents. If the
-- response to this API call is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch the next page of intents, specify the
-- pagination token in the next request.
--
-- 'nameContains', 'getIntents_nameContains' - Substring to match in intent names. An intent will be returned if any
-- part of its name matches the substring. For example, \"xyz\" matches
-- both \"xyzabc\" and \"abcxyz.\"
--
-- 'maxResults', 'getIntents_maxResults' - The maximum number of intents to return in the response. The default is
-- 10.
newGetIntents ::
  GetIntents
newGetIntents =
  GetIntents'
    { nextToken = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A pagination token that fetches the next page of intents. If the
-- response to this API call is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch the next page of intents, specify the
-- pagination token in the next request.
getIntents_nextToken :: Lens.Lens' GetIntents (Prelude.Maybe Prelude.Text)
getIntents_nextToken = Lens.lens (\GetIntents' {nextToken} -> nextToken) (\s@GetIntents' {} a -> s {nextToken = a} :: GetIntents)

-- | Substring to match in intent names. An intent will be returned if any
-- part of its name matches the substring. For example, \"xyz\" matches
-- both \"xyzabc\" and \"abcxyz.\"
getIntents_nameContains :: Lens.Lens' GetIntents (Prelude.Maybe Prelude.Text)
getIntents_nameContains = Lens.lens (\GetIntents' {nameContains} -> nameContains) (\s@GetIntents' {} a -> s {nameContains = a} :: GetIntents)

-- | The maximum number of intents to return in the response. The default is
-- 10.
getIntents_maxResults :: Lens.Lens' GetIntents (Prelude.Maybe Prelude.Natural)
getIntents_maxResults = Lens.lens (\GetIntents' {maxResults} -> maxResults) (\s@GetIntents' {} a -> s {maxResults = a} :: GetIntents)

instance Core.AWSPager GetIntents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getIntentsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getIntentsResponse_intents Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getIntents_nextToken
          Lens..~ rs
          Lens.^? getIntentsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest GetIntents where
  type AWSResponse GetIntents = GetIntentsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetIntentsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "intents" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetIntents where
  hashWithSalt _salt GetIntents' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` nameContains
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData GetIntents where
  rnf GetIntents' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf nameContains
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders GetIntents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetIntents where
  toPath = Prelude.const "/intents/"

instance Core.ToQuery GetIntents where
  toQuery GetIntents' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "nameContains" Core.=: nameContains,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newGetIntentsResponse' smart constructor.
data GetIntentsResponse = GetIntentsResponse'
  { -- | If the response is truncated, the response includes a pagination token
    -- that you can specify in your next request to fetch the next page of
    -- intents.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of @Intent@ objects. For more information, see PutBot.
    intents :: Prelude.Maybe [IntentMetadata],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIntentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getIntentsResponse_nextToken' - If the response is truncated, the response includes a pagination token
-- that you can specify in your next request to fetch the next page of
-- intents.
--
-- 'intents', 'getIntentsResponse_intents' - An array of @Intent@ objects. For more information, see PutBot.
--
-- 'httpStatus', 'getIntentsResponse_httpStatus' - The response's http status code.
newGetIntentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetIntentsResponse
newGetIntentsResponse pHttpStatus_ =
  GetIntentsResponse'
    { nextToken = Prelude.Nothing,
      intents = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the response is truncated, the response includes a pagination token
-- that you can specify in your next request to fetch the next page of
-- intents.
getIntentsResponse_nextToken :: Lens.Lens' GetIntentsResponse (Prelude.Maybe Prelude.Text)
getIntentsResponse_nextToken = Lens.lens (\GetIntentsResponse' {nextToken} -> nextToken) (\s@GetIntentsResponse' {} a -> s {nextToken = a} :: GetIntentsResponse)

-- | An array of @Intent@ objects. For more information, see PutBot.
getIntentsResponse_intents :: Lens.Lens' GetIntentsResponse (Prelude.Maybe [IntentMetadata])
getIntentsResponse_intents = Lens.lens (\GetIntentsResponse' {intents} -> intents) (\s@GetIntentsResponse' {} a -> s {intents = a} :: GetIntentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getIntentsResponse_httpStatus :: Lens.Lens' GetIntentsResponse Prelude.Int
getIntentsResponse_httpStatus = Lens.lens (\GetIntentsResponse' {httpStatus} -> httpStatus) (\s@GetIntentsResponse' {} a -> s {httpStatus = a} :: GetIntentsResponse)

instance Prelude.NFData GetIntentsResponse where
  rnf GetIntentsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf intents
      `Prelude.seq` Prelude.rnf httpStatus

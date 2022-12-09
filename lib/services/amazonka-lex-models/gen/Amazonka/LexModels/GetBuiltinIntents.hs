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
-- Module      : Amazonka.LexModels.GetBuiltinIntents
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of built-in intents that meet the specified criteria.
--
-- This operation requires permission for the @lex:GetBuiltinIntents@
-- action.
--
-- This operation returns paginated results.
module Amazonka.LexModels.GetBuiltinIntents
  ( -- * Creating a Request
    GetBuiltinIntents (..),
    newGetBuiltinIntents,

    -- * Request Lenses
    getBuiltinIntents_locale,
    getBuiltinIntents_maxResults,
    getBuiltinIntents_nextToken,
    getBuiltinIntents_signatureContains,

    -- * Destructuring the Response
    GetBuiltinIntentsResponse (..),
    newGetBuiltinIntentsResponse,

    -- * Response Lenses
    getBuiltinIntentsResponse_intents,
    getBuiltinIntentsResponse_nextToken,
    getBuiltinIntentsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexModels.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetBuiltinIntents' smart constructor.
data GetBuiltinIntents = GetBuiltinIntents'
  { -- | A list of locales that the intent supports.
    locale :: Prelude.Maybe Locale,
    -- | The maximum number of intents to return in the response. The default is
    -- 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A pagination token that fetches the next page of intents. If this API
    -- call is truncated, Amazon Lex returns a pagination token in the
    -- response. To fetch the next page of intents, use the pagination token in
    -- the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Substring to match in built-in intent signatures. An intent will be
    -- returned if any part of its signature matches the substring. For
    -- example, \"xyz\" matches both \"xyzabc\" and \"abcxyz.\" To find the
    -- signature for an intent, see
    -- <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents>
    -- in the /Alexa Skills Kit/.
    signatureContains :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBuiltinIntents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locale', 'getBuiltinIntents_locale' - A list of locales that the intent supports.
--
-- 'maxResults', 'getBuiltinIntents_maxResults' - The maximum number of intents to return in the response. The default is
-- 10.
--
-- 'nextToken', 'getBuiltinIntents_nextToken' - A pagination token that fetches the next page of intents. If this API
-- call is truncated, Amazon Lex returns a pagination token in the
-- response. To fetch the next page of intents, use the pagination token in
-- the next request.
--
-- 'signatureContains', 'getBuiltinIntents_signatureContains' - Substring to match in built-in intent signatures. An intent will be
-- returned if any part of its signature matches the substring. For
-- example, \"xyz\" matches both \"xyzabc\" and \"abcxyz.\" To find the
-- signature for an intent, see
-- <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents>
-- in the /Alexa Skills Kit/.
newGetBuiltinIntents ::
  GetBuiltinIntents
newGetBuiltinIntents =
  GetBuiltinIntents'
    { locale = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      signatureContains = Prelude.Nothing
    }

-- | A list of locales that the intent supports.
getBuiltinIntents_locale :: Lens.Lens' GetBuiltinIntents (Prelude.Maybe Locale)
getBuiltinIntents_locale = Lens.lens (\GetBuiltinIntents' {locale} -> locale) (\s@GetBuiltinIntents' {} a -> s {locale = a} :: GetBuiltinIntents)

-- | The maximum number of intents to return in the response. The default is
-- 10.
getBuiltinIntents_maxResults :: Lens.Lens' GetBuiltinIntents (Prelude.Maybe Prelude.Natural)
getBuiltinIntents_maxResults = Lens.lens (\GetBuiltinIntents' {maxResults} -> maxResults) (\s@GetBuiltinIntents' {} a -> s {maxResults = a} :: GetBuiltinIntents)

-- | A pagination token that fetches the next page of intents. If this API
-- call is truncated, Amazon Lex returns a pagination token in the
-- response. To fetch the next page of intents, use the pagination token in
-- the next request.
getBuiltinIntents_nextToken :: Lens.Lens' GetBuiltinIntents (Prelude.Maybe Prelude.Text)
getBuiltinIntents_nextToken = Lens.lens (\GetBuiltinIntents' {nextToken} -> nextToken) (\s@GetBuiltinIntents' {} a -> s {nextToken = a} :: GetBuiltinIntents)

-- | Substring to match in built-in intent signatures. An intent will be
-- returned if any part of its signature matches the substring. For
-- example, \"xyz\" matches both \"xyzabc\" and \"abcxyz.\" To find the
-- signature for an intent, see
-- <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents>
-- in the /Alexa Skills Kit/.
getBuiltinIntents_signatureContains :: Lens.Lens' GetBuiltinIntents (Prelude.Maybe Prelude.Text)
getBuiltinIntents_signatureContains = Lens.lens (\GetBuiltinIntents' {signatureContains} -> signatureContains) (\s@GetBuiltinIntents' {} a -> s {signatureContains = a} :: GetBuiltinIntents)

instance Core.AWSPager GetBuiltinIntents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getBuiltinIntentsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getBuiltinIntentsResponse_intents
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getBuiltinIntents_nextToken
          Lens..~ rs
          Lens.^? getBuiltinIntentsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetBuiltinIntents where
  type
    AWSResponse GetBuiltinIntents =
      GetBuiltinIntentsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBuiltinIntentsResponse'
            Prelude.<$> (x Data..?> "intents" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBuiltinIntents where
  hashWithSalt _salt GetBuiltinIntents' {..} =
    _salt `Prelude.hashWithSalt` locale
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` signatureContains

instance Prelude.NFData GetBuiltinIntents where
  rnf GetBuiltinIntents' {..} =
    Prelude.rnf locale
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf signatureContains

instance Data.ToHeaders GetBuiltinIntents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetBuiltinIntents where
  toPath = Prelude.const "/builtins/intents/"

instance Data.ToQuery GetBuiltinIntents where
  toQuery GetBuiltinIntents' {..} =
    Prelude.mconcat
      [ "locale" Data.=: locale,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "signatureContains" Data.=: signatureContains
      ]

-- | /See:/ 'newGetBuiltinIntentsResponse' smart constructor.
data GetBuiltinIntentsResponse = GetBuiltinIntentsResponse'
  { -- | An array of @builtinIntentMetadata@ objects, one for each intent in the
    -- response.
    intents :: Prelude.Maybe [BuiltinIntentMetadata],
    -- | A pagination token that fetches the next page of intents. If the
    -- response to this API call is truncated, Amazon Lex returns a pagination
    -- token in the response. To fetch the next page of intents, specify the
    -- pagination token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBuiltinIntentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'intents', 'getBuiltinIntentsResponse_intents' - An array of @builtinIntentMetadata@ objects, one for each intent in the
-- response.
--
-- 'nextToken', 'getBuiltinIntentsResponse_nextToken' - A pagination token that fetches the next page of intents. If the
-- response to this API call is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch the next page of intents, specify the
-- pagination token in the next request.
--
-- 'httpStatus', 'getBuiltinIntentsResponse_httpStatus' - The response's http status code.
newGetBuiltinIntentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBuiltinIntentsResponse
newGetBuiltinIntentsResponse pHttpStatus_ =
  GetBuiltinIntentsResponse'
    { intents =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @builtinIntentMetadata@ objects, one for each intent in the
-- response.
getBuiltinIntentsResponse_intents :: Lens.Lens' GetBuiltinIntentsResponse (Prelude.Maybe [BuiltinIntentMetadata])
getBuiltinIntentsResponse_intents = Lens.lens (\GetBuiltinIntentsResponse' {intents} -> intents) (\s@GetBuiltinIntentsResponse' {} a -> s {intents = a} :: GetBuiltinIntentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A pagination token that fetches the next page of intents. If the
-- response to this API call is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch the next page of intents, specify the
-- pagination token in the next request.
getBuiltinIntentsResponse_nextToken :: Lens.Lens' GetBuiltinIntentsResponse (Prelude.Maybe Prelude.Text)
getBuiltinIntentsResponse_nextToken = Lens.lens (\GetBuiltinIntentsResponse' {nextToken} -> nextToken) (\s@GetBuiltinIntentsResponse' {} a -> s {nextToken = a} :: GetBuiltinIntentsResponse)

-- | The response's http status code.
getBuiltinIntentsResponse_httpStatus :: Lens.Lens' GetBuiltinIntentsResponse Prelude.Int
getBuiltinIntentsResponse_httpStatus = Lens.lens (\GetBuiltinIntentsResponse' {httpStatus} -> httpStatus) (\s@GetBuiltinIntentsResponse' {} a -> s {httpStatus = a} :: GetBuiltinIntentsResponse)

instance Prelude.NFData GetBuiltinIntentsResponse where
  rnf GetBuiltinIntentsResponse' {..} =
    Prelude.rnf intents
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

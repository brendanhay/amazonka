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
-- Module      : Network.AWS.LexModels.GetBuiltinIntents
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.LexModels.GetBuiltinIntents
  ( -- * Creating a Request
    GetBuiltinIntents (..),
    newGetBuiltinIntents,

    -- * Request Lenses
    getBuiltinIntents_signatureContains,
    getBuiltinIntents_nextToken,
    getBuiltinIntents_maxResults,
    getBuiltinIntents_locale,

    -- * Destructuring the Response
    GetBuiltinIntentsResponse (..),
    newGetBuiltinIntentsResponse,

    -- * Response Lenses
    getBuiltinIntentsResponse_nextToken,
    getBuiltinIntentsResponse_intents,
    getBuiltinIntentsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetBuiltinIntents' smart constructor.
data GetBuiltinIntents = GetBuiltinIntents'
  { -- | Substring to match in built-in intent signatures. An intent will be
    -- returned if any part of its signature matches the substring. For
    -- example, \"xyz\" matches both \"xyzabc\" and \"abcxyz.\" To find the
    -- signature for an intent, see
    -- <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents>
    -- in the /Alexa Skills Kit/.
    signatureContains :: Core.Maybe Core.Text,
    -- | A pagination token that fetches the next page of intents. If this API
    -- call is truncated, Amazon Lex returns a pagination token in the
    -- response. To fetch the next page of intents, use the pagination token in
    -- the next request.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of intents to return in the response. The default is
    -- 10.
    maxResults :: Core.Maybe Core.Natural,
    -- | A list of locales that the intent supports.
    locale :: Core.Maybe Locale
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetBuiltinIntents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'signatureContains', 'getBuiltinIntents_signatureContains' - Substring to match in built-in intent signatures. An intent will be
-- returned if any part of its signature matches the substring. For
-- example, \"xyz\" matches both \"xyzabc\" and \"abcxyz.\" To find the
-- signature for an intent, see
-- <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents>
-- in the /Alexa Skills Kit/.
--
-- 'nextToken', 'getBuiltinIntents_nextToken' - A pagination token that fetches the next page of intents. If this API
-- call is truncated, Amazon Lex returns a pagination token in the
-- response. To fetch the next page of intents, use the pagination token in
-- the next request.
--
-- 'maxResults', 'getBuiltinIntents_maxResults' - The maximum number of intents to return in the response. The default is
-- 10.
--
-- 'locale', 'getBuiltinIntents_locale' - A list of locales that the intent supports.
newGetBuiltinIntents ::
  GetBuiltinIntents
newGetBuiltinIntents =
  GetBuiltinIntents'
    { signatureContains =
        Core.Nothing,
      nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      locale = Core.Nothing
    }

-- | Substring to match in built-in intent signatures. An intent will be
-- returned if any part of its signature matches the substring. For
-- example, \"xyz\" matches both \"xyzabc\" and \"abcxyz.\" To find the
-- signature for an intent, see
-- <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents>
-- in the /Alexa Skills Kit/.
getBuiltinIntents_signatureContains :: Lens.Lens' GetBuiltinIntents (Core.Maybe Core.Text)
getBuiltinIntents_signatureContains = Lens.lens (\GetBuiltinIntents' {signatureContains} -> signatureContains) (\s@GetBuiltinIntents' {} a -> s {signatureContains = a} :: GetBuiltinIntents)

-- | A pagination token that fetches the next page of intents. If this API
-- call is truncated, Amazon Lex returns a pagination token in the
-- response. To fetch the next page of intents, use the pagination token in
-- the next request.
getBuiltinIntents_nextToken :: Lens.Lens' GetBuiltinIntents (Core.Maybe Core.Text)
getBuiltinIntents_nextToken = Lens.lens (\GetBuiltinIntents' {nextToken} -> nextToken) (\s@GetBuiltinIntents' {} a -> s {nextToken = a} :: GetBuiltinIntents)

-- | The maximum number of intents to return in the response. The default is
-- 10.
getBuiltinIntents_maxResults :: Lens.Lens' GetBuiltinIntents (Core.Maybe Core.Natural)
getBuiltinIntents_maxResults = Lens.lens (\GetBuiltinIntents' {maxResults} -> maxResults) (\s@GetBuiltinIntents' {} a -> s {maxResults = a} :: GetBuiltinIntents)

-- | A list of locales that the intent supports.
getBuiltinIntents_locale :: Lens.Lens' GetBuiltinIntents (Core.Maybe Locale)
getBuiltinIntents_locale = Lens.lens (\GetBuiltinIntents' {locale} -> locale) (\s@GetBuiltinIntents' {} a -> s {locale = a} :: GetBuiltinIntents)

instance Core.AWSPager GetBuiltinIntents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getBuiltinIntentsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getBuiltinIntentsResponse_intents Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getBuiltinIntents_nextToken
          Lens..~ rs
          Lens.^? getBuiltinIntentsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest GetBuiltinIntents where
  type
    AWSResponse GetBuiltinIntents =
      GetBuiltinIntentsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBuiltinIntentsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "intents" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetBuiltinIntents

instance Core.NFData GetBuiltinIntents

instance Core.ToHeaders GetBuiltinIntents where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetBuiltinIntents where
  toPath = Core.const "/builtins/intents/"

instance Core.ToQuery GetBuiltinIntents where
  toQuery GetBuiltinIntents' {..} =
    Core.mconcat
      [ "signatureContains" Core.=: signatureContains,
        "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "locale" Core.=: locale
      ]

-- | /See:/ 'newGetBuiltinIntentsResponse' smart constructor.
data GetBuiltinIntentsResponse = GetBuiltinIntentsResponse'
  { -- | A pagination token that fetches the next page of intents. If the
    -- response to this API call is truncated, Amazon Lex returns a pagination
    -- token in the response. To fetch the next page of intents, specify the
    -- pagination token in the next request.
    nextToken :: Core.Maybe Core.Text,
    -- | An array of @builtinIntentMetadata@ objects, one for each intent in the
    -- response.
    intents :: Core.Maybe [BuiltinIntentMetadata],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetBuiltinIntentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getBuiltinIntentsResponse_nextToken' - A pagination token that fetches the next page of intents. If the
-- response to this API call is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch the next page of intents, specify the
-- pagination token in the next request.
--
-- 'intents', 'getBuiltinIntentsResponse_intents' - An array of @builtinIntentMetadata@ objects, one for each intent in the
-- response.
--
-- 'httpStatus', 'getBuiltinIntentsResponse_httpStatus' - The response's http status code.
newGetBuiltinIntentsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetBuiltinIntentsResponse
newGetBuiltinIntentsResponse pHttpStatus_ =
  GetBuiltinIntentsResponse'
    { nextToken =
        Core.Nothing,
      intents = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A pagination token that fetches the next page of intents. If the
-- response to this API call is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch the next page of intents, specify the
-- pagination token in the next request.
getBuiltinIntentsResponse_nextToken :: Lens.Lens' GetBuiltinIntentsResponse (Core.Maybe Core.Text)
getBuiltinIntentsResponse_nextToken = Lens.lens (\GetBuiltinIntentsResponse' {nextToken} -> nextToken) (\s@GetBuiltinIntentsResponse' {} a -> s {nextToken = a} :: GetBuiltinIntentsResponse)

-- | An array of @builtinIntentMetadata@ objects, one for each intent in the
-- response.
getBuiltinIntentsResponse_intents :: Lens.Lens' GetBuiltinIntentsResponse (Core.Maybe [BuiltinIntentMetadata])
getBuiltinIntentsResponse_intents = Lens.lens (\GetBuiltinIntentsResponse' {intents} -> intents) (\s@GetBuiltinIntentsResponse' {} a -> s {intents = a} :: GetBuiltinIntentsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getBuiltinIntentsResponse_httpStatus :: Lens.Lens' GetBuiltinIntentsResponse Core.Int
getBuiltinIntentsResponse_httpStatus = Lens.lens (\GetBuiltinIntentsResponse' {httpStatus} -> httpStatus) (\s@GetBuiltinIntentsResponse' {} a -> s {httpStatus = a} :: GetBuiltinIntentsResponse)

instance Core.NFData GetBuiltinIntentsResponse

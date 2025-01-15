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
-- Module      : Amazonka.LexModels.GetBuiltinSlotTypes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of built-in slot types that meet the specified criteria.
--
-- For a list of built-in slot types, see
-- <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/slot-type-reference Slot Type Reference>
-- in the /Alexa Skills Kit/.
--
-- This operation requires permission for the @lex:GetBuiltInSlotTypes@
-- action.
--
-- This operation returns paginated results.
module Amazonka.LexModels.GetBuiltinSlotTypes
  ( -- * Creating a Request
    GetBuiltinSlotTypes (..),
    newGetBuiltinSlotTypes,

    -- * Request Lenses
    getBuiltinSlotTypes_locale,
    getBuiltinSlotTypes_maxResults,
    getBuiltinSlotTypes_nextToken,
    getBuiltinSlotTypes_signatureContains,

    -- * Destructuring the Response
    GetBuiltinSlotTypesResponse (..),
    newGetBuiltinSlotTypesResponse,

    -- * Response Lenses
    getBuiltinSlotTypesResponse_nextToken,
    getBuiltinSlotTypesResponse_slotTypes,
    getBuiltinSlotTypesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexModels.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetBuiltinSlotTypes' smart constructor.
data GetBuiltinSlotTypes = GetBuiltinSlotTypes'
  { -- | A list of locales that the slot type supports.
    locale :: Prelude.Maybe Locale,
    -- | The maximum number of slot types to return in the response. The default
    -- is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A pagination token that fetches the next page of slot types. If the
    -- response to this API call is truncated, Amazon Lex returns a pagination
    -- token in the response. To fetch the next page of slot types, specify the
    -- pagination token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Substring to match in built-in slot type signatures. A slot type will be
    -- returned if any part of its signature matches the substring. For
    -- example, \"xyz\" matches both \"xyzabc\" and \"abcxyz.\"
    signatureContains :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBuiltinSlotTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locale', 'getBuiltinSlotTypes_locale' - A list of locales that the slot type supports.
--
-- 'maxResults', 'getBuiltinSlotTypes_maxResults' - The maximum number of slot types to return in the response. The default
-- is 10.
--
-- 'nextToken', 'getBuiltinSlotTypes_nextToken' - A pagination token that fetches the next page of slot types. If the
-- response to this API call is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch the next page of slot types, specify the
-- pagination token in the next request.
--
-- 'signatureContains', 'getBuiltinSlotTypes_signatureContains' - Substring to match in built-in slot type signatures. A slot type will be
-- returned if any part of its signature matches the substring. For
-- example, \"xyz\" matches both \"xyzabc\" and \"abcxyz.\"
newGetBuiltinSlotTypes ::
  GetBuiltinSlotTypes
newGetBuiltinSlotTypes =
  GetBuiltinSlotTypes'
    { locale = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      signatureContains = Prelude.Nothing
    }

-- | A list of locales that the slot type supports.
getBuiltinSlotTypes_locale :: Lens.Lens' GetBuiltinSlotTypes (Prelude.Maybe Locale)
getBuiltinSlotTypes_locale = Lens.lens (\GetBuiltinSlotTypes' {locale} -> locale) (\s@GetBuiltinSlotTypes' {} a -> s {locale = a} :: GetBuiltinSlotTypes)

-- | The maximum number of slot types to return in the response. The default
-- is 10.
getBuiltinSlotTypes_maxResults :: Lens.Lens' GetBuiltinSlotTypes (Prelude.Maybe Prelude.Natural)
getBuiltinSlotTypes_maxResults = Lens.lens (\GetBuiltinSlotTypes' {maxResults} -> maxResults) (\s@GetBuiltinSlotTypes' {} a -> s {maxResults = a} :: GetBuiltinSlotTypes)

-- | A pagination token that fetches the next page of slot types. If the
-- response to this API call is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch the next page of slot types, specify the
-- pagination token in the next request.
getBuiltinSlotTypes_nextToken :: Lens.Lens' GetBuiltinSlotTypes (Prelude.Maybe Prelude.Text)
getBuiltinSlotTypes_nextToken = Lens.lens (\GetBuiltinSlotTypes' {nextToken} -> nextToken) (\s@GetBuiltinSlotTypes' {} a -> s {nextToken = a} :: GetBuiltinSlotTypes)

-- | Substring to match in built-in slot type signatures. A slot type will be
-- returned if any part of its signature matches the substring. For
-- example, \"xyz\" matches both \"xyzabc\" and \"abcxyz.\"
getBuiltinSlotTypes_signatureContains :: Lens.Lens' GetBuiltinSlotTypes (Prelude.Maybe Prelude.Text)
getBuiltinSlotTypes_signatureContains = Lens.lens (\GetBuiltinSlotTypes' {signatureContains} -> signatureContains) (\s@GetBuiltinSlotTypes' {} a -> s {signatureContains = a} :: GetBuiltinSlotTypes)

instance Core.AWSPager GetBuiltinSlotTypes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getBuiltinSlotTypesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getBuiltinSlotTypesResponse_slotTypes
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& getBuiltinSlotTypes_nextToken
              Lens..~ rs
              Lens.^? getBuiltinSlotTypesResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest GetBuiltinSlotTypes where
  type
    AWSResponse GetBuiltinSlotTypes =
      GetBuiltinSlotTypesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBuiltinSlotTypesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "slotTypes" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBuiltinSlotTypes where
  hashWithSalt _salt GetBuiltinSlotTypes' {..} =
    _salt
      `Prelude.hashWithSalt` locale
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` signatureContains

instance Prelude.NFData GetBuiltinSlotTypes where
  rnf GetBuiltinSlotTypes' {..} =
    Prelude.rnf locale `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf signatureContains

instance Data.ToHeaders GetBuiltinSlotTypes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetBuiltinSlotTypes where
  toPath = Prelude.const "/builtins/slottypes/"

instance Data.ToQuery GetBuiltinSlotTypes where
  toQuery GetBuiltinSlotTypes' {..} =
    Prelude.mconcat
      [ "locale" Data.=: locale,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "signatureContains" Data.=: signatureContains
      ]

-- | /See:/ 'newGetBuiltinSlotTypesResponse' smart constructor.
data GetBuiltinSlotTypesResponse = GetBuiltinSlotTypesResponse'
  { -- | If the response is truncated, the response includes a pagination token
    -- that you can use in your next request to fetch the next page of slot
    -- types.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of @BuiltInSlotTypeMetadata@ objects, one entry for each slot
    -- type returned.
    slotTypes :: Prelude.Maybe [BuiltinSlotTypeMetadata],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBuiltinSlotTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getBuiltinSlotTypesResponse_nextToken' - If the response is truncated, the response includes a pagination token
-- that you can use in your next request to fetch the next page of slot
-- types.
--
-- 'slotTypes', 'getBuiltinSlotTypesResponse_slotTypes' - An array of @BuiltInSlotTypeMetadata@ objects, one entry for each slot
-- type returned.
--
-- 'httpStatus', 'getBuiltinSlotTypesResponse_httpStatus' - The response's http status code.
newGetBuiltinSlotTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBuiltinSlotTypesResponse
newGetBuiltinSlotTypesResponse pHttpStatus_ =
  GetBuiltinSlotTypesResponse'
    { nextToken =
        Prelude.Nothing,
      slotTypes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the response is truncated, the response includes a pagination token
-- that you can use in your next request to fetch the next page of slot
-- types.
getBuiltinSlotTypesResponse_nextToken :: Lens.Lens' GetBuiltinSlotTypesResponse (Prelude.Maybe Prelude.Text)
getBuiltinSlotTypesResponse_nextToken = Lens.lens (\GetBuiltinSlotTypesResponse' {nextToken} -> nextToken) (\s@GetBuiltinSlotTypesResponse' {} a -> s {nextToken = a} :: GetBuiltinSlotTypesResponse)

-- | An array of @BuiltInSlotTypeMetadata@ objects, one entry for each slot
-- type returned.
getBuiltinSlotTypesResponse_slotTypes :: Lens.Lens' GetBuiltinSlotTypesResponse (Prelude.Maybe [BuiltinSlotTypeMetadata])
getBuiltinSlotTypesResponse_slotTypes = Lens.lens (\GetBuiltinSlotTypesResponse' {slotTypes} -> slotTypes) (\s@GetBuiltinSlotTypesResponse' {} a -> s {slotTypes = a} :: GetBuiltinSlotTypesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getBuiltinSlotTypesResponse_httpStatus :: Lens.Lens' GetBuiltinSlotTypesResponse Prelude.Int
getBuiltinSlotTypesResponse_httpStatus = Lens.lens (\GetBuiltinSlotTypesResponse' {httpStatus} -> httpStatus) (\s@GetBuiltinSlotTypesResponse' {} a -> s {httpStatus = a} :: GetBuiltinSlotTypesResponse)

instance Prelude.NFData GetBuiltinSlotTypesResponse where
  rnf GetBuiltinSlotTypesResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf slotTypes `Prelude.seq`
        Prelude.rnf httpStatus

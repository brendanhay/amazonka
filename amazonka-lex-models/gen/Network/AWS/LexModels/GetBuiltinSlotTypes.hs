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
-- Module      : Network.AWS.LexModels.GetBuiltinSlotTypes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.LexModels.GetBuiltinSlotTypes
  ( -- * Creating a Request
    GetBuiltinSlotTypes (..),
    newGetBuiltinSlotTypes,

    -- * Request Lenses
    getBuiltinSlotTypes_signatureContains,
    getBuiltinSlotTypes_nextToken,
    getBuiltinSlotTypes_maxResults,
    getBuiltinSlotTypes_locale,

    -- * Destructuring the Response
    GetBuiltinSlotTypesResponse (..),
    newGetBuiltinSlotTypesResponse,

    -- * Response Lenses
    getBuiltinSlotTypesResponse_slotTypes,
    getBuiltinSlotTypesResponse_nextToken,
    getBuiltinSlotTypesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetBuiltinSlotTypes' smart constructor.
data GetBuiltinSlotTypes = GetBuiltinSlotTypes'
  { -- | Substring to match in built-in slot type signatures. A slot type will be
    -- returned if any part of its signature matches the substring. For
    -- example, \"xyz\" matches both \"xyzabc\" and \"abcxyz.\"
    signatureContains :: Prelude.Maybe Prelude.Text,
    -- | A pagination token that fetches the next page of slot types. If the
    -- response to this API call is truncated, Amazon Lex returns a pagination
    -- token in the response. To fetch the next page of slot types, specify the
    -- pagination token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of slot types to return in the response. The default
    -- is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A list of locales that the slot type supports.
    locale :: Prelude.Maybe Locale
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
-- 'signatureContains', 'getBuiltinSlotTypes_signatureContains' - Substring to match in built-in slot type signatures. A slot type will be
-- returned if any part of its signature matches the substring. For
-- example, \"xyz\" matches both \"xyzabc\" and \"abcxyz.\"
--
-- 'nextToken', 'getBuiltinSlotTypes_nextToken' - A pagination token that fetches the next page of slot types. If the
-- response to this API call is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch the next page of slot types, specify the
-- pagination token in the next request.
--
-- 'maxResults', 'getBuiltinSlotTypes_maxResults' - The maximum number of slot types to return in the response. The default
-- is 10.
--
-- 'locale', 'getBuiltinSlotTypes_locale' - A list of locales that the slot type supports.
newGetBuiltinSlotTypes ::
  GetBuiltinSlotTypes
newGetBuiltinSlotTypes =
  GetBuiltinSlotTypes'
    { signatureContains =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      locale = Prelude.Nothing
    }

-- | Substring to match in built-in slot type signatures. A slot type will be
-- returned if any part of its signature matches the substring. For
-- example, \"xyz\" matches both \"xyzabc\" and \"abcxyz.\"
getBuiltinSlotTypes_signatureContains :: Lens.Lens' GetBuiltinSlotTypes (Prelude.Maybe Prelude.Text)
getBuiltinSlotTypes_signatureContains = Lens.lens (\GetBuiltinSlotTypes' {signatureContains} -> signatureContains) (\s@GetBuiltinSlotTypes' {} a -> s {signatureContains = a} :: GetBuiltinSlotTypes)

-- | A pagination token that fetches the next page of slot types. If the
-- response to this API call is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch the next page of slot types, specify the
-- pagination token in the next request.
getBuiltinSlotTypes_nextToken :: Lens.Lens' GetBuiltinSlotTypes (Prelude.Maybe Prelude.Text)
getBuiltinSlotTypes_nextToken = Lens.lens (\GetBuiltinSlotTypes' {nextToken} -> nextToken) (\s@GetBuiltinSlotTypes' {} a -> s {nextToken = a} :: GetBuiltinSlotTypes)

-- | The maximum number of slot types to return in the response. The default
-- is 10.
getBuiltinSlotTypes_maxResults :: Lens.Lens' GetBuiltinSlotTypes (Prelude.Maybe Prelude.Natural)
getBuiltinSlotTypes_maxResults = Lens.lens (\GetBuiltinSlotTypes' {maxResults} -> maxResults) (\s@GetBuiltinSlotTypes' {} a -> s {maxResults = a} :: GetBuiltinSlotTypes)

-- | A list of locales that the slot type supports.
getBuiltinSlotTypes_locale :: Lens.Lens' GetBuiltinSlotTypes (Prelude.Maybe Locale)
getBuiltinSlotTypes_locale = Lens.lens (\GetBuiltinSlotTypes' {locale} -> locale) (\s@GetBuiltinSlotTypes' {} a -> s {locale = a} :: GetBuiltinSlotTypes)

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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBuiltinSlotTypesResponse'
            Prelude.<$> (x Core..?> "slotTypes" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBuiltinSlotTypes

instance Prelude.NFData GetBuiltinSlotTypes

instance Core.ToHeaders GetBuiltinSlotTypes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetBuiltinSlotTypes where
  toPath = Prelude.const "/builtins/slottypes/"

instance Core.ToQuery GetBuiltinSlotTypes where
  toQuery GetBuiltinSlotTypes' {..} =
    Prelude.mconcat
      [ "signatureContains" Core.=: signatureContains,
        "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "locale" Core.=: locale
      ]

-- | /See:/ 'newGetBuiltinSlotTypesResponse' smart constructor.
data GetBuiltinSlotTypesResponse = GetBuiltinSlotTypesResponse'
  { -- | An array of @BuiltInSlotTypeMetadata@ objects, one entry for each slot
    -- type returned.
    slotTypes :: Prelude.Maybe [BuiltinSlotTypeMetadata],
    -- | If the response is truncated, the response includes a pagination token
    -- that you can use in your next request to fetch the next page of slot
    -- types.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'slotTypes', 'getBuiltinSlotTypesResponse_slotTypes' - An array of @BuiltInSlotTypeMetadata@ objects, one entry for each slot
-- type returned.
--
-- 'nextToken', 'getBuiltinSlotTypesResponse_nextToken' - If the response is truncated, the response includes a pagination token
-- that you can use in your next request to fetch the next page of slot
-- types.
--
-- 'httpStatus', 'getBuiltinSlotTypesResponse_httpStatus' - The response's http status code.
newGetBuiltinSlotTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBuiltinSlotTypesResponse
newGetBuiltinSlotTypesResponse pHttpStatus_ =
  GetBuiltinSlotTypesResponse'
    { slotTypes =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @BuiltInSlotTypeMetadata@ objects, one entry for each slot
-- type returned.
getBuiltinSlotTypesResponse_slotTypes :: Lens.Lens' GetBuiltinSlotTypesResponse (Prelude.Maybe [BuiltinSlotTypeMetadata])
getBuiltinSlotTypesResponse_slotTypes = Lens.lens (\GetBuiltinSlotTypesResponse' {slotTypes} -> slotTypes) (\s@GetBuiltinSlotTypesResponse' {} a -> s {slotTypes = a} :: GetBuiltinSlotTypesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | If the response is truncated, the response includes a pagination token
-- that you can use in your next request to fetch the next page of slot
-- types.
getBuiltinSlotTypesResponse_nextToken :: Lens.Lens' GetBuiltinSlotTypesResponse (Prelude.Maybe Prelude.Text)
getBuiltinSlotTypesResponse_nextToken = Lens.lens (\GetBuiltinSlotTypesResponse' {nextToken} -> nextToken) (\s@GetBuiltinSlotTypesResponse' {} a -> s {nextToken = a} :: GetBuiltinSlotTypesResponse)

-- | The response's http status code.
getBuiltinSlotTypesResponse_httpStatus :: Lens.Lens' GetBuiltinSlotTypesResponse Prelude.Int
getBuiltinSlotTypesResponse_httpStatus = Lens.lens (\GetBuiltinSlotTypesResponse' {httpStatus} -> httpStatus) (\s@GetBuiltinSlotTypesResponse' {} a -> s {httpStatus = a} :: GetBuiltinSlotTypesResponse)

instance Prelude.NFData GetBuiltinSlotTypesResponse

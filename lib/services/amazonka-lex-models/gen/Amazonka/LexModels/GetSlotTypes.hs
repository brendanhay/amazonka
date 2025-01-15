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
-- Module      : Amazonka.LexModels.GetSlotTypes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns slot type information as follows:
--
-- -   If you specify the @nameContains@ field, returns the @$LATEST@
--     version of all slot types that contain the specified string.
--
-- -   If you don\'t specify the @nameContains@ field, returns information
--     about the @$LATEST@ version of all slot types.
--
-- The operation requires permission for the @lex:GetSlotTypes@ action.
--
-- This operation returns paginated results.
module Amazonka.LexModels.GetSlotTypes
  ( -- * Creating a Request
    GetSlotTypes (..),
    newGetSlotTypes,

    -- * Request Lenses
    getSlotTypes_maxResults,
    getSlotTypes_nameContains,
    getSlotTypes_nextToken,

    -- * Destructuring the Response
    GetSlotTypesResponse (..),
    newGetSlotTypesResponse,

    -- * Response Lenses
    getSlotTypesResponse_nextToken,
    getSlotTypesResponse_slotTypes,
    getSlotTypesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexModels.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSlotTypes' smart constructor.
data GetSlotTypes = GetSlotTypes'
  { -- | The maximum number of slot types to return in the response. The default
    -- is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Substring to match in slot type names. A slot type will be returned if
    -- any part of its name matches the substring. For example, \"xyz\" matches
    -- both \"xyzabc\" and \"abcxyz.\"
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | A pagination token that fetches the next page of slot types. If the
    -- response to this API call is truncated, Amazon Lex returns a pagination
    -- token in the response. To fetch next page of slot types, specify the
    -- pagination token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSlotTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getSlotTypes_maxResults' - The maximum number of slot types to return in the response. The default
-- is 10.
--
-- 'nameContains', 'getSlotTypes_nameContains' - Substring to match in slot type names. A slot type will be returned if
-- any part of its name matches the substring. For example, \"xyz\" matches
-- both \"xyzabc\" and \"abcxyz.\"
--
-- 'nextToken', 'getSlotTypes_nextToken' - A pagination token that fetches the next page of slot types. If the
-- response to this API call is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch next page of slot types, specify the
-- pagination token in the next request.
newGetSlotTypes ::
  GetSlotTypes
newGetSlotTypes =
  GetSlotTypes'
    { maxResults = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of slot types to return in the response. The default
-- is 10.
getSlotTypes_maxResults :: Lens.Lens' GetSlotTypes (Prelude.Maybe Prelude.Natural)
getSlotTypes_maxResults = Lens.lens (\GetSlotTypes' {maxResults} -> maxResults) (\s@GetSlotTypes' {} a -> s {maxResults = a} :: GetSlotTypes)

-- | Substring to match in slot type names. A slot type will be returned if
-- any part of its name matches the substring. For example, \"xyz\" matches
-- both \"xyzabc\" and \"abcxyz.\"
getSlotTypes_nameContains :: Lens.Lens' GetSlotTypes (Prelude.Maybe Prelude.Text)
getSlotTypes_nameContains = Lens.lens (\GetSlotTypes' {nameContains} -> nameContains) (\s@GetSlotTypes' {} a -> s {nameContains = a} :: GetSlotTypes)

-- | A pagination token that fetches the next page of slot types. If the
-- response to this API call is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch next page of slot types, specify the
-- pagination token in the next request.
getSlotTypes_nextToken :: Lens.Lens' GetSlotTypes (Prelude.Maybe Prelude.Text)
getSlotTypes_nextToken = Lens.lens (\GetSlotTypes' {nextToken} -> nextToken) (\s@GetSlotTypes' {} a -> s {nextToken = a} :: GetSlotTypes)

instance Core.AWSPager GetSlotTypes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getSlotTypesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getSlotTypesResponse_slotTypes
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& getSlotTypes_nextToken
              Lens..~ rs
              Lens.^? getSlotTypesResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest GetSlotTypes where
  type AWSResponse GetSlotTypes = GetSlotTypesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSlotTypesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "slotTypes" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSlotTypes where
  hashWithSalt _salt GetSlotTypes' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nameContains
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData GetSlotTypes where
  rnf GetSlotTypes' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nameContains `Prelude.seq`
        Prelude.rnf nextToken

instance Data.ToHeaders GetSlotTypes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetSlotTypes where
  toPath = Prelude.const "/slottypes/"

instance Data.ToQuery GetSlotTypes where
  toQuery GetSlotTypes' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nameContains" Data.=: nameContains,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newGetSlotTypesResponse' smart constructor.
data GetSlotTypesResponse = GetSlotTypesResponse'
  { -- | If the response is truncated, it includes a pagination token that you
    -- can specify in your next request to fetch the next page of slot types.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects, one for each slot type, that provides information
    -- such as the name of the slot type, the version, and a description.
    slotTypes :: Prelude.Maybe [SlotTypeMetadata],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSlotTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getSlotTypesResponse_nextToken' - If the response is truncated, it includes a pagination token that you
-- can specify in your next request to fetch the next page of slot types.
--
-- 'slotTypes', 'getSlotTypesResponse_slotTypes' - An array of objects, one for each slot type, that provides information
-- such as the name of the slot type, the version, and a description.
--
-- 'httpStatus', 'getSlotTypesResponse_httpStatus' - The response's http status code.
newGetSlotTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSlotTypesResponse
newGetSlotTypesResponse pHttpStatus_ =
  GetSlotTypesResponse'
    { nextToken = Prelude.Nothing,
      slotTypes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the response is truncated, it includes a pagination token that you
-- can specify in your next request to fetch the next page of slot types.
getSlotTypesResponse_nextToken :: Lens.Lens' GetSlotTypesResponse (Prelude.Maybe Prelude.Text)
getSlotTypesResponse_nextToken = Lens.lens (\GetSlotTypesResponse' {nextToken} -> nextToken) (\s@GetSlotTypesResponse' {} a -> s {nextToken = a} :: GetSlotTypesResponse)

-- | An array of objects, one for each slot type, that provides information
-- such as the name of the slot type, the version, and a description.
getSlotTypesResponse_slotTypes :: Lens.Lens' GetSlotTypesResponse (Prelude.Maybe [SlotTypeMetadata])
getSlotTypesResponse_slotTypes = Lens.lens (\GetSlotTypesResponse' {slotTypes} -> slotTypes) (\s@GetSlotTypesResponse' {} a -> s {slotTypes = a} :: GetSlotTypesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getSlotTypesResponse_httpStatus :: Lens.Lens' GetSlotTypesResponse Prelude.Int
getSlotTypesResponse_httpStatus = Lens.lens (\GetSlotTypesResponse' {httpStatus} -> httpStatus) (\s@GetSlotTypesResponse' {} a -> s {httpStatus = a} :: GetSlotTypesResponse)

instance Prelude.NFData GetSlotTypesResponse where
  rnf GetSlotTypesResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf slotTypes `Prelude.seq`
        Prelude.rnf httpStatus

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
-- Module      : Network.AWS.LexModels.GetSlotTypes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.LexModels.GetSlotTypes
  ( -- * Creating a Request
    GetSlotTypes (..),
    newGetSlotTypes,

    -- * Request Lenses
    getSlotTypes_nextToken,
    getSlotTypes_nameContains,
    getSlotTypes_maxResults,

    -- * Destructuring the Response
    GetSlotTypesResponse (..),
    newGetSlotTypesResponse,

    -- * Response Lenses
    getSlotTypesResponse_slotTypes,
    getSlotTypesResponse_nextToken,
    getSlotTypesResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetSlotTypes' smart constructor.
data GetSlotTypes = GetSlotTypes'
  { -- | A pagination token that fetches the next page of slot types. If the
    -- response to this API call is truncated, Amazon Lex returns a pagination
    -- token in the response. To fetch next page of slot types, specify the
    -- pagination token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Substring to match in slot type names. A slot type will be returned if
    -- any part of its name matches the substring. For example, \"xyz\" matches
    -- both \"xyzabc\" and \"abcxyz.\"
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of slot types to return in the response. The default
    -- is 10.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetSlotTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getSlotTypes_nextToken' - A pagination token that fetches the next page of slot types. If the
-- response to this API call is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch next page of slot types, specify the
-- pagination token in the next request.
--
-- 'nameContains', 'getSlotTypes_nameContains' - Substring to match in slot type names. A slot type will be returned if
-- any part of its name matches the substring. For example, \"xyz\" matches
-- both \"xyzabc\" and \"abcxyz.\"
--
-- 'maxResults', 'getSlotTypes_maxResults' - The maximum number of slot types to return in the response. The default
-- is 10.
newGetSlotTypes ::
  GetSlotTypes
newGetSlotTypes =
  GetSlotTypes'
    { nextToken = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A pagination token that fetches the next page of slot types. If the
-- response to this API call is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch next page of slot types, specify the
-- pagination token in the next request.
getSlotTypes_nextToken :: Lens.Lens' GetSlotTypes (Prelude.Maybe Prelude.Text)
getSlotTypes_nextToken = Lens.lens (\GetSlotTypes' {nextToken} -> nextToken) (\s@GetSlotTypes' {} a -> s {nextToken = a} :: GetSlotTypes)

-- | Substring to match in slot type names. A slot type will be returned if
-- any part of its name matches the substring. For example, \"xyz\" matches
-- both \"xyzabc\" and \"abcxyz.\"
getSlotTypes_nameContains :: Lens.Lens' GetSlotTypes (Prelude.Maybe Prelude.Text)
getSlotTypes_nameContains = Lens.lens (\GetSlotTypes' {nameContains} -> nameContains) (\s@GetSlotTypes' {} a -> s {nameContains = a} :: GetSlotTypes)

-- | The maximum number of slot types to return in the response. The default
-- is 10.
getSlotTypes_maxResults :: Lens.Lens' GetSlotTypes (Prelude.Maybe Prelude.Natural)
getSlotTypes_maxResults = Lens.lens (\GetSlotTypes' {maxResults} -> maxResults) (\s@GetSlotTypes' {} a -> s {maxResults = a} :: GetSlotTypes)

instance Pager.AWSPager GetSlotTypes where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? getSlotTypesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? getSlotTypesResponse_slotTypes Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& getSlotTypes_nextToken
          Lens..~ rs
          Lens.^? getSlotTypesResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest GetSlotTypes where
  type Rs GetSlotTypes = GetSlotTypesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSlotTypesResponse'
            Prelude.<$> ( x Prelude..?> "slotTypes"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSlotTypes

instance Prelude.NFData GetSlotTypes

instance Prelude.ToHeaders GetSlotTypes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath GetSlotTypes where
  toPath = Prelude.const "/slottypes/"

instance Prelude.ToQuery GetSlotTypes where
  toQuery GetSlotTypes' {..} =
    Prelude.mconcat
      [ "nextToken" Prelude.=: nextToken,
        "nameContains" Prelude.=: nameContains,
        "maxResults" Prelude.=: maxResults
      ]

-- | /See:/ 'newGetSlotTypesResponse' smart constructor.
data GetSlotTypesResponse = GetSlotTypesResponse'
  { -- | An array of objects, one for each slot type, that provides information
    -- such as the name of the slot type, the version, and a description.
    slotTypes :: Prelude.Maybe [SlotTypeMetadata],
    -- | If the response is truncated, it includes a pagination token that you
    -- can specify in your next request to fetch the next page of slot types.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetSlotTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'slotTypes', 'getSlotTypesResponse_slotTypes' - An array of objects, one for each slot type, that provides information
-- such as the name of the slot type, the version, and a description.
--
-- 'nextToken', 'getSlotTypesResponse_nextToken' - If the response is truncated, it includes a pagination token that you
-- can specify in your next request to fetch the next page of slot types.
--
-- 'httpStatus', 'getSlotTypesResponse_httpStatus' - The response's http status code.
newGetSlotTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSlotTypesResponse
newGetSlotTypesResponse pHttpStatus_ =
  GetSlotTypesResponse'
    { slotTypes = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects, one for each slot type, that provides information
-- such as the name of the slot type, the version, and a description.
getSlotTypesResponse_slotTypes :: Lens.Lens' GetSlotTypesResponse (Prelude.Maybe [SlotTypeMetadata])
getSlotTypesResponse_slotTypes = Lens.lens (\GetSlotTypesResponse' {slotTypes} -> slotTypes) (\s@GetSlotTypesResponse' {} a -> s {slotTypes = a} :: GetSlotTypesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | If the response is truncated, it includes a pagination token that you
-- can specify in your next request to fetch the next page of slot types.
getSlotTypesResponse_nextToken :: Lens.Lens' GetSlotTypesResponse (Prelude.Maybe Prelude.Text)
getSlotTypesResponse_nextToken = Lens.lens (\GetSlotTypesResponse' {nextToken} -> nextToken) (\s@GetSlotTypesResponse' {} a -> s {nextToken = a} :: GetSlotTypesResponse)

-- | The response's http status code.
getSlotTypesResponse_httpStatus :: Lens.Lens' GetSlotTypesResponse Prelude.Int
getSlotTypesResponse_httpStatus = Lens.lens (\GetSlotTypesResponse' {httpStatus} -> httpStatus) (\s@GetSlotTypesResponse' {} a -> s {httpStatus = a} :: GetSlotTypesResponse)

instance Prelude.NFData GetSlotTypesResponse

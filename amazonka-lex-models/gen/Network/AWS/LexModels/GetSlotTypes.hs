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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetSlotTypes' smart constructor.
data GetSlotTypes = GetSlotTypes'
  { -- | A pagination token that fetches the next page of slot types. If the
    -- response to this API call is truncated, Amazon Lex returns a pagination
    -- token in the response. To fetch next page of slot types, specify the
    -- pagination token in the next request.
    nextToken :: Core.Maybe Core.Text,
    -- | Substring to match in slot type names. A slot type will be returned if
    -- any part of its name matches the substring. For example, \"xyz\" matches
    -- both \"xyzabc\" and \"abcxyz.\"
    nameContains :: Core.Maybe Core.Text,
    -- | The maximum number of slot types to return in the response. The default
    -- is 10.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { nextToken = Core.Nothing,
      nameContains = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | A pagination token that fetches the next page of slot types. If the
-- response to this API call is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch next page of slot types, specify the
-- pagination token in the next request.
getSlotTypes_nextToken :: Lens.Lens' GetSlotTypes (Core.Maybe Core.Text)
getSlotTypes_nextToken = Lens.lens (\GetSlotTypes' {nextToken} -> nextToken) (\s@GetSlotTypes' {} a -> s {nextToken = a} :: GetSlotTypes)

-- | Substring to match in slot type names. A slot type will be returned if
-- any part of its name matches the substring. For example, \"xyz\" matches
-- both \"xyzabc\" and \"abcxyz.\"
getSlotTypes_nameContains :: Lens.Lens' GetSlotTypes (Core.Maybe Core.Text)
getSlotTypes_nameContains = Lens.lens (\GetSlotTypes' {nameContains} -> nameContains) (\s@GetSlotTypes' {} a -> s {nameContains = a} :: GetSlotTypes)

-- | The maximum number of slot types to return in the response. The default
-- is 10.
getSlotTypes_maxResults :: Lens.Lens' GetSlotTypes (Core.Maybe Core.Natural)
getSlotTypes_maxResults = Lens.lens (\GetSlotTypes' {maxResults} -> maxResults) (\s@GetSlotTypes' {} a -> s {maxResults = a} :: GetSlotTypes)

instance Core.AWSPager GetSlotTypes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getSlotTypesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getSlotTypesResponse_slotTypes Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getSlotTypes_nextToken
          Lens..~ rs
          Lens.^? getSlotTypesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest GetSlotTypes where
  type AWSResponse GetSlotTypes = GetSlotTypesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSlotTypesResponse'
            Core.<$> (x Core..?> "slotTypes" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetSlotTypes

instance Core.NFData GetSlotTypes

instance Core.ToHeaders GetSlotTypes where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetSlotTypes where
  toPath = Core.const "/slottypes/"

instance Core.ToQuery GetSlotTypes where
  toQuery GetSlotTypes' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "nameContains" Core.=: nameContains,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newGetSlotTypesResponse' smart constructor.
data GetSlotTypesResponse = GetSlotTypesResponse'
  { -- | An array of objects, one for each slot type, that provides information
    -- such as the name of the slot type, the version, and a description.
    slotTypes :: Core.Maybe [SlotTypeMetadata],
    -- | If the response is truncated, it includes a pagination token that you
    -- can specify in your next request to fetch the next page of slot types.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetSlotTypesResponse
newGetSlotTypesResponse pHttpStatus_ =
  GetSlotTypesResponse'
    { slotTypes = Core.Nothing,
      nextToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects, one for each slot type, that provides information
-- such as the name of the slot type, the version, and a description.
getSlotTypesResponse_slotTypes :: Lens.Lens' GetSlotTypesResponse (Core.Maybe [SlotTypeMetadata])
getSlotTypesResponse_slotTypes = Lens.lens (\GetSlotTypesResponse' {slotTypes} -> slotTypes) (\s@GetSlotTypesResponse' {} a -> s {slotTypes = a} :: GetSlotTypesResponse) Core.. Lens.mapping Lens._Coerce

-- | If the response is truncated, it includes a pagination token that you
-- can specify in your next request to fetch the next page of slot types.
getSlotTypesResponse_nextToken :: Lens.Lens' GetSlotTypesResponse (Core.Maybe Core.Text)
getSlotTypesResponse_nextToken = Lens.lens (\GetSlotTypesResponse' {nextToken} -> nextToken) (\s@GetSlotTypesResponse' {} a -> s {nextToken = a} :: GetSlotTypesResponse)

-- | The response's http status code.
getSlotTypesResponse_httpStatus :: Lens.Lens' GetSlotTypesResponse Core.Int
getSlotTypesResponse_httpStatus = Lens.lens (\GetSlotTypesResponse' {httpStatus} -> httpStatus) (\s@GetSlotTypesResponse' {} a -> s {httpStatus = a} :: GetSlotTypesResponse)

instance Core.NFData GetSlotTypesResponse

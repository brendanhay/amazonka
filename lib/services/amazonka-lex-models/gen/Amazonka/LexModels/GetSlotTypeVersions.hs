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
-- Module      : Amazonka.LexModels.GetSlotTypeVersions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about all versions of a slot type.
--
-- The @GetSlotTypeVersions@ operation returns a @SlotTypeMetadata@ object
-- for each version of a slot type. For example, if a slot type has three
-- numbered versions, the @GetSlotTypeVersions@ operation returns four
-- @SlotTypeMetadata@ objects in the response, one for each numbered
-- version and one for the @$LATEST@ version.
--
-- The @GetSlotTypeVersions@ operation always returns at least one version,
-- the @$LATEST@ version.
--
-- This operation requires permissions for the @lex:GetSlotTypeVersions@
-- action.
--
-- This operation returns paginated results.
module Amazonka.LexModels.GetSlotTypeVersions
  ( -- * Creating a Request
    GetSlotTypeVersions (..),
    newGetSlotTypeVersions,

    -- * Request Lenses
    getSlotTypeVersions_nextToken,
    getSlotTypeVersions_maxResults,
    getSlotTypeVersions_name,

    -- * Destructuring the Response
    GetSlotTypeVersionsResponse (..),
    newGetSlotTypeVersionsResponse,

    -- * Response Lenses
    getSlotTypeVersionsResponse_nextToken,
    getSlotTypeVersionsResponse_slotTypes,
    getSlotTypeVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexModels.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSlotTypeVersions' smart constructor.
data GetSlotTypeVersions = GetSlotTypeVersions'
  { -- | A pagination token for fetching the next page of slot type versions. If
    -- the response to this call is truncated, Amazon Lex returns a pagination
    -- token in the response. To fetch the next page of versions, specify the
    -- pagination token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of slot type versions to return in the response. The
    -- default is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the slot type for which versions should be returned.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSlotTypeVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getSlotTypeVersions_nextToken' - A pagination token for fetching the next page of slot type versions. If
-- the response to this call is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch the next page of versions, specify the
-- pagination token in the next request.
--
-- 'maxResults', 'getSlotTypeVersions_maxResults' - The maximum number of slot type versions to return in the response. The
-- default is 10.
--
-- 'name', 'getSlotTypeVersions_name' - The name of the slot type for which versions should be returned.
newGetSlotTypeVersions ::
  -- | 'name'
  Prelude.Text ->
  GetSlotTypeVersions
newGetSlotTypeVersions pName_ =
  GetSlotTypeVersions'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      name = pName_
    }

-- | A pagination token for fetching the next page of slot type versions. If
-- the response to this call is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch the next page of versions, specify the
-- pagination token in the next request.
getSlotTypeVersions_nextToken :: Lens.Lens' GetSlotTypeVersions (Prelude.Maybe Prelude.Text)
getSlotTypeVersions_nextToken = Lens.lens (\GetSlotTypeVersions' {nextToken} -> nextToken) (\s@GetSlotTypeVersions' {} a -> s {nextToken = a} :: GetSlotTypeVersions)

-- | The maximum number of slot type versions to return in the response. The
-- default is 10.
getSlotTypeVersions_maxResults :: Lens.Lens' GetSlotTypeVersions (Prelude.Maybe Prelude.Natural)
getSlotTypeVersions_maxResults = Lens.lens (\GetSlotTypeVersions' {maxResults} -> maxResults) (\s@GetSlotTypeVersions' {} a -> s {maxResults = a} :: GetSlotTypeVersions)

-- | The name of the slot type for which versions should be returned.
getSlotTypeVersions_name :: Lens.Lens' GetSlotTypeVersions Prelude.Text
getSlotTypeVersions_name = Lens.lens (\GetSlotTypeVersions' {name} -> name) (\s@GetSlotTypeVersions' {} a -> s {name = a} :: GetSlotTypeVersions)

instance Core.AWSPager GetSlotTypeVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getSlotTypeVersionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getSlotTypeVersionsResponse_slotTypes
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getSlotTypeVersions_nextToken
          Lens..~ rs
          Lens.^? getSlotTypeVersionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetSlotTypeVersions where
  type
    AWSResponse GetSlotTypeVersions =
      GetSlotTypeVersionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSlotTypeVersionsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "slotTypes" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSlotTypeVersions where
  hashWithSalt _salt GetSlotTypeVersions' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` name

instance Prelude.NFData GetSlotTypeVersions where
  rnf GetSlotTypeVersions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf name

instance Core.ToHeaders GetSlotTypeVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetSlotTypeVersions where
  toPath GetSlotTypeVersions' {..} =
    Prelude.mconcat
      ["/slottypes/", Core.toBS name, "/versions/"]

instance Core.ToQuery GetSlotTypeVersions where
  toQuery GetSlotTypeVersions' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newGetSlotTypeVersionsResponse' smart constructor.
data GetSlotTypeVersionsResponse = GetSlotTypeVersionsResponse'
  { -- | A pagination token for fetching the next page of slot type versions. If
    -- the response to this call is truncated, Amazon Lex returns a pagination
    -- token in the response. To fetch the next page of versions, specify the
    -- pagination token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of @SlotTypeMetadata@ objects, one for each numbered version of
    -- the slot type plus one for the @$LATEST@ version.
    slotTypes :: Prelude.Maybe [SlotTypeMetadata],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSlotTypeVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getSlotTypeVersionsResponse_nextToken' - A pagination token for fetching the next page of slot type versions. If
-- the response to this call is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch the next page of versions, specify the
-- pagination token in the next request.
--
-- 'slotTypes', 'getSlotTypeVersionsResponse_slotTypes' - An array of @SlotTypeMetadata@ objects, one for each numbered version of
-- the slot type plus one for the @$LATEST@ version.
--
-- 'httpStatus', 'getSlotTypeVersionsResponse_httpStatus' - The response's http status code.
newGetSlotTypeVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSlotTypeVersionsResponse
newGetSlotTypeVersionsResponse pHttpStatus_ =
  GetSlotTypeVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      slotTypes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A pagination token for fetching the next page of slot type versions. If
-- the response to this call is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch the next page of versions, specify the
-- pagination token in the next request.
getSlotTypeVersionsResponse_nextToken :: Lens.Lens' GetSlotTypeVersionsResponse (Prelude.Maybe Prelude.Text)
getSlotTypeVersionsResponse_nextToken = Lens.lens (\GetSlotTypeVersionsResponse' {nextToken} -> nextToken) (\s@GetSlotTypeVersionsResponse' {} a -> s {nextToken = a} :: GetSlotTypeVersionsResponse)

-- | An array of @SlotTypeMetadata@ objects, one for each numbered version of
-- the slot type plus one for the @$LATEST@ version.
getSlotTypeVersionsResponse_slotTypes :: Lens.Lens' GetSlotTypeVersionsResponse (Prelude.Maybe [SlotTypeMetadata])
getSlotTypeVersionsResponse_slotTypes = Lens.lens (\GetSlotTypeVersionsResponse' {slotTypes} -> slotTypes) (\s@GetSlotTypeVersionsResponse' {} a -> s {slotTypes = a} :: GetSlotTypeVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getSlotTypeVersionsResponse_httpStatus :: Lens.Lens' GetSlotTypeVersionsResponse Prelude.Int
getSlotTypeVersionsResponse_httpStatus = Lens.lens (\GetSlotTypeVersionsResponse' {httpStatus} -> httpStatus) (\s@GetSlotTypeVersionsResponse' {} a -> s {httpStatus = a} :: GetSlotTypeVersionsResponse)

instance Prelude.NFData GetSlotTypeVersionsResponse where
  rnf GetSlotTypeVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf slotTypes
      `Prelude.seq` Prelude.rnf httpStatus

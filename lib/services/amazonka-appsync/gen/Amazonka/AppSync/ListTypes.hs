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
-- Module      : Amazonka.AppSync.ListTypes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the types for a given API.
--
-- This operation returns paginated results.
module Amazonka.AppSync.ListTypes
  ( -- * Creating a Request
    ListTypes (..),
    newListTypes,

    -- * Request Lenses
    listTypes_maxResults,
    listTypes_nextToken,
    listTypes_apiId,
    listTypes_format,

    -- * Destructuring the Response
    ListTypesResponse (..),
    newListTypesResponse,

    -- * Response Lenses
    listTypesResponse_nextToken,
    listTypesResponse_types,
    listTypesResponse_httpStatus,
  )
where

import Amazonka.AppSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTypes' smart constructor.
data ListTypes = ListTypes'
  { -- | The maximum number of results that you want the request to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | An identifier that was returned from the previous call to this
    -- operation, which you can use to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The API ID.
    apiId :: Prelude.Text,
    -- | The type format: SDL or JSON.
    format :: TypeDefinitionFormat
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listTypes_maxResults' - The maximum number of results that you want the request to return.
--
-- 'nextToken', 'listTypes_nextToken' - An identifier that was returned from the previous call to this
-- operation, which you can use to return the next set of items in the
-- list.
--
-- 'apiId', 'listTypes_apiId' - The API ID.
--
-- 'format', 'listTypes_format' - The type format: SDL or JSON.
newListTypes ::
  -- | 'apiId'
  Prelude.Text ->
  -- | 'format'
  TypeDefinitionFormat ->
  ListTypes
newListTypes pApiId_ pFormat_ =
  ListTypes'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      apiId = pApiId_,
      format = pFormat_
    }

-- | The maximum number of results that you want the request to return.
listTypes_maxResults :: Lens.Lens' ListTypes (Prelude.Maybe Prelude.Natural)
listTypes_maxResults = Lens.lens (\ListTypes' {maxResults} -> maxResults) (\s@ListTypes' {} a -> s {maxResults = a} :: ListTypes)

-- | An identifier that was returned from the previous call to this
-- operation, which you can use to return the next set of items in the
-- list.
listTypes_nextToken :: Lens.Lens' ListTypes (Prelude.Maybe Prelude.Text)
listTypes_nextToken = Lens.lens (\ListTypes' {nextToken} -> nextToken) (\s@ListTypes' {} a -> s {nextToken = a} :: ListTypes)

-- | The API ID.
listTypes_apiId :: Lens.Lens' ListTypes Prelude.Text
listTypes_apiId = Lens.lens (\ListTypes' {apiId} -> apiId) (\s@ListTypes' {} a -> s {apiId = a} :: ListTypes)

-- | The type format: SDL or JSON.
listTypes_format :: Lens.Lens' ListTypes TypeDefinitionFormat
listTypes_format = Lens.lens (\ListTypes' {format} -> format) (\s@ListTypes' {} a -> s {format = a} :: ListTypes)

instance Core.AWSPager ListTypes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTypesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTypesResponse_types
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listTypes_nextToken
          Lens..~ rs
          Lens.^? listTypesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListTypes where
  type AWSResponse ListTypes = ListTypesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTypesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "types" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTypes where
  hashWithSalt _salt ListTypes' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` format

instance Prelude.NFData ListTypes where
  rnf ListTypes' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf apiId
      `Prelude.seq` Prelude.rnf format

instance Data.ToHeaders ListTypes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListTypes where
  toPath ListTypes' {..} =
    Prelude.mconcat
      ["/v1/apis/", Data.toBS apiId, "/types"]

instance Data.ToQuery ListTypes where
  toQuery ListTypes' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "format" Data.=: format
      ]

-- | /See:/ 'newListTypesResponse' smart constructor.
data ListTypesResponse = ListTypesResponse'
  { -- | An identifier to pass in the next request to this operation to return
    -- the next set of items in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The @Type@ objects.
    types :: Prelude.Maybe [Type],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTypesResponse_nextToken' - An identifier to pass in the next request to this operation to return
-- the next set of items in the list.
--
-- 'types', 'listTypesResponse_types' - The @Type@ objects.
--
-- 'httpStatus', 'listTypesResponse_httpStatus' - The response's http status code.
newListTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTypesResponse
newListTypesResponse pHttpStatus_ =
  ListTypesResponse'
    { nextToken = Prelude.Nothing,
      types = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identifier to pass in the next request to this operation to return
-- the next set of items in the list.
listTypesResponse_nextToken :: Lens.Lens' ListTypesResponse (Prelude.Maybe Prelude.Text)
listTypesResponse_nextToken = Lens.lens (\ListTypesResponse' {nextToken} -> nextToken) (\s@ListTypesResponse' {} a -> s {nextToken = a} :: ListTypesResponse)

-- | The @Type@ objects.
listTypesResponse_types :: Lens.Lens' ListTypesResponse (Prelude.Maybe [Type])
listTypesResponse_types = Lens.lens (\ListTypesResponse' {types} -> types) (\s@ListTypesResponse' {} a -> s {types = a} :: ListTypesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTypesResponse_httpStatus :: Lens.Lens' ListTypesResponse Prelude.Int
listTypesResponse_httpStatus = Lens.lens (\ListTypesResponse' {httpStatus} -> httpStatus) (\s@ListTypesResponse' {} a -> s {httpStatus = a} :: ListTypesResponse)

instance Prelude.NFData ListTypesResponse where
  rnf ListTypesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf types
      `Prelude.seq` Prelude.rnf httpStatus

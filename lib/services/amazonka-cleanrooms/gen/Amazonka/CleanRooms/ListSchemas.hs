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
-- Module      : Amazonka.CleanRooms.ListSchemas
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the schemas for relations within a collaboration.
--
-- This operation returns paginated results.
module Amazonka.CleanRooms.ListSchemas
  ( -- * Creating a Request
    ListSchemas (..),
    newListSchemas,

    -- * Request Lenses
    listSchemas_maxResults,
    listSchemas_nextToken,
    listSchemas_schemaType,
    listSchemas_collaborationIdentifier,

    -- * Destructuring the Response
    ListSchemasResponse (..),
    newListSchemasResponse,

    -- * Response Lenses
    listSchemasResponse_nextToken,
    listSchemasResponse_httpStatus,
    listSchemasResponse_schemaSummaries,
  )
where

import Amazonka.CleanRooms.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSchemas' smart constructor.
data ListSchemas = ListSchemas'
  { -- | The maximum size of the results that is returned per call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token value retrieved from a previous call to access the next page
    -- of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | If present, filter schemas by schema type. The only valid schema type is
    -- currently \`TABLE\`.
    schemaType :: Prelude.Maybe SchemaType,
    -- | A unique identifier for the collaboration that the schema belongs to.
    -- Currently accepts a collaboration ID.
    collaborationIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSchemas' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listSchemas_maxResults' - The maximum size of the results that is returned per call.
--
-- 'nextToken', 'listSchemas_nextToken' - The token value retrieved from a previous call to access the next page
-- of results.
--
-- 'schemaType', 'listSchemas_schemaType' - If present, filter schemas by schema type. The only valid schema type is
-- currently \`TABLE\`.
--
-- 'collaborationIdentifier', 'listSchemas_collaborationIdentifier' - A unique identifier for the collaboration that the schema belongs to.
-- Currently accepts a collaboration ID.
newListSchemas ::
  -- | 'collaborationIdentifier'
  Prelude.Text ->
  ListSchemas
newListSchemas pCollaborationIdentifier_ =
  ListSchemas'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      schemaType = Prelude.Nothing,
      collaborationIdentifier = pCollaborationIdentifier_
    }

-- | The maximum size of the results that is returned per call.
listSchemas_maxResults :: Lens.Lens' ListSchemas (Prelude.Maybe Prelude.Natural)
listSchemas_maxResults = Lens.lens (\ListSchemas' {maxResults} -> maxResults) (\s@ListSchemas' {} a -> s {maxResults = a} :: ListSchemas)

-- | The token value retrieved from a previous call to access the next page
-- of results.
listSchemas_nextToken :: Lens.Lens' ListSchemas (Prelude.Maybe Prelude.Text)
listSchemas_nextToken = Lens.lens (\ListSchemas' {nextToken} -> nextToken) (\s@ListSchemas' {} a -> s {nextToken = a} :: ListSchemas)

-- | If present, filter schemas by schema type. The only valid schema type is
-- currently \`TABLE\`.
listSchemas_schemaType :: Lens.Lens' ListSchemas (Prelude.Maybe SchemaType)
listSchemas_schemaType = Lens.lens (\ListSchemas' {schemaType} -> schemaType) (\s@ListSchemas' {} a -> s {schemaType = a} :: ListSchemas)

-- | A unique identifier for the collaboration that the schema belongs to.
-- Currently accepts a collaboration ID.
listSchemas_collaborationIdentifier :: Lens.Lens' ListSchemas Prelude.Text
listSchemas_collaborationIdentifier = Lens.lens (\ListSchemas' {collaborationIdentifier} -> collaborationIdentifier) (\s@ListSchemas' {} a -> s {collaborationIdentifier = a} :: ListSchemas)

instance Core.AWSPager ListSchemas where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSchemasResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. listSchemasResponse_schemaSummaries) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listSchemas_nextToken
          Lens..~ rs
          Lens.^? listSchemasResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListSchemas where
  type AWSResponse ListSchemas = ListSchemasResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSchemasResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "schemaSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListSchemas where
  hashWithSalt _salt ListSchemas' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` schemaType
      `Prelude.hashWithSalt` collaborationIdentifier

instance Prelude.NFData ListSchemas where
  rnf ListSchemas' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf schemaType
      `Prelude.seq` Prelude.rnf collaborationIdentifier

instance Data.ToHeaders ListSchemas where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListSchemas where
  toPath ListSchemas' {..} =
    Prelude.mconcat
      [ "/collaborations/",
        Data.toBS collaborationIdentifier,
        "/schemas"
      ]

instance Data.ToQuery ListSchemas where
  toQuery ListSchemas' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "schemaType" Data.=: schemaType
      ]

-- | /See:/ 'newListSchemasResponse' smart constructor.
data ListSchemasResponse = ListSchemasResponse'
  { -- | The token value retrieved from a previous call to access the next page
    -- of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The retrieved list of schemas.
    schemaSummaries :: [SchemaSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSchemasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSchemasResponse_nextToken' - The token value retrieved from a previous call to access the next page
-- of results.
--
-- 'httpStatus', 'listSchemasResponse_httpStatus' - The response's http status code.
--
-- 'schemaSummaries', 'listSchemasResponse_schemaSummaries' - The retrieved list of schemas.
newListSchemasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSchemasResponse
newListSchemasResponse pHttpStatus_ =
  ListSchemasResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      schemaSummaries = Prelude.mempty
    }

-- | The token value retrieved from a previous call to access the next page
-- of results.
listSchemasResponse_nextToken :: Lens.Lens' ListSchemasResponse (Prelude.Maybe Prelude.Text)
listSchemasResponse_nextToken = Lens.lens (\ListSchemasResponse' {nextToken} -> nextToken) (\s@ListSchemasResponse' {} a -> s {nextToken = a} :: ListSchemasResponse)

-- | The response's http status code.
listSchemasResponse_httpStatus :: Lens.Lens' ListSchemasResponse Prelude.Int
listSchemasResponse_httpStatus = Lens.lens (\ListSchemasResponse' {httpStatus} -> httpStatus) (\s@ListSchemasResponse' {} a -> s {httpStatus = a} :: ListSchemasResponse)

-- | The retrieved list of schemas.
listSchemasResponse_schemaSummaries :: Lens.Lens' ListSchemasResponse [SchemaSummary]
listSchemasResponse_schemaSummaries = Lens.lens (\ListSchemasResponse' {schemaSummaries} -> schemaSummaries) (\s@ListSchemasResponse' {} a -> s {schemaSummaries = a} :: ListSchemasResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListSchemasResponse where
  rnf ListSchemasResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf schemaSummaries

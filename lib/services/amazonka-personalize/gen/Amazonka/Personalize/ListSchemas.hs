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
-- Module      : Amazonka.Personalize.ListSchemas
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of schemas associated with the account. The response
-- provides the properties for each schema, including the Amazon Resource
-- Name (ARN). For more information on schemas, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_CreateSchema.html CreateSchema>.
--
-- This operation returns paginated results.
module Amazonka.Personalize.ListSchemas
  ( -- * Creating a Request
    ListSchemas (..),
    newListSchemas,

    -- * Request Lenses
    listSchemas_maxResults,
    listSchemas_nextToken,

    -- * Destructuring the Response
    ListSchemasResponse (..),
    newListSchemasResponse,

    -- * Response Lenses
    listSchemasResponse_nextToken,
    listSchemasResponse_schemas,
    listSchemasResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSchemas' smart constructor.
data ListSchemas = ListSchemas'
  { -- | The maximum number of schemas to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token returned from the previous call to @ListSchemas@ for getting the
    -- next set of schemas (if they exist).
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'maxResults', 'listSchemas_maxResults' - The maximum number of schemas to return.
--
-- 'nextToken', 'listSchemas_nextToken' - A token returned from the previous call to @ListSchemas@ for getting the
-- next set of schemas (if they exist).
newListSchemas ::
  ListSchemas
newListSchemas =
  ListSchemas'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of schemas to return.
listSchemas_maxResults :: Lens.Lens' ListSchemas (Prelude.Maybe Prelude.Natural)
listSchemas_maxResults = Lens.lens (\ListSchemas' {maxResults} -> maxResults) (\s@ListSchemas' {} a -> s {maxResults = a} :: ListSchemas)

-- | A token returned from the previous call to @ListSchemas@ for getting the
-- next set of schemas (if they exist).
listSchemas_nextToken :: Lens.Lens' ListSchemas (Prelude.Maybe Prelude.Text)
listSchemas_nextToken = Lens.lens (\ListSchemas' {nextToken} -> nextToken) (\s@ListSchemas' {} a -> s {nextToken = a} :: ListSchemas)

instance Core.AWSPager ListSchemas where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSchemasResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSchemasResponse_schemas
            Prelude.. Lens._Just
        ) =
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
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSchemasResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "schemas" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSchemas where
  hashWithSalt _salt ListSchemas' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListSchemas where
  rnf ListSchemas' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListSchemas where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.ListSchemas" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListSchemas where
  toJSON ListSchemas' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListSchemas where
  toPath = Prelude.const "/"

instance Data.ToQuery ListSchemas where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSchemasResponse' smart constructor.
data ListSchemasResponse = ListSchemasResponse'
  { -- | A token used to get the next set of schemas (if they exist).
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of schemas.
    schemas :: Prelude.Maybe [DatasetSchemaSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'nextToken', 'listSchemasResponse_nextToken' - A token used to get the next set of schemas (if they exist).
--
-- 'schemas', 'listSchemasResponse_schemas' - A list of schemas.
--
-- 'httpStatus', 'listSchemasResponse_httpStatus' - The response's http status code.
newListSchemasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSchemasResponse
newListSchemasResponse pHttpStatus_ =
  ListSchemasResponse'
    { nextToken = Prelude.Nothing,
      schemas = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token used to get the next set of schemas (if they exist).
listSchemasResponse_nextToken :: Lens.Lens' ListSchemasResponse (Prelude.Maybe Prelude.Text)
listSchemasResponse_nextToken = Lens.lens (\ListSchemasResponse' {nextToken} -> nextToken) (\s@ListSchemasResponse' {} a -> s {nextToken = a} :: ListSchemasResponse)

-- | A list of schemas.
listSchemasResponse_schemas :: Lens.Lens' ListSchemasResponse (Prelude.Maybe [DatasetSchemaSummary])
listSchemasResponse_schemas = Lens.lens (\ListSchemasResponse' {schemas} -> schemas) (\s@ListSchemasResponse' {} a -> s {schemas = a} :: ListSchemasResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSchemasResponse_httpStatus :: Lens.Lens' ListSchemasResponse Prelude.Int
listSchemasResponse_httpStatus = Lens.lens (\ListSchemasResponse' {httpStatus} -> httpStatus) (\s@ListSchemasResponse' {} a -> s {httpStatus = a} :: ListSchemasResponse)

instance Prelude.NFData ListSchemasResponse where
  rnf ListSchemasResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf schemas
      `Prelude.seq` Prelude.rnf httpStatus

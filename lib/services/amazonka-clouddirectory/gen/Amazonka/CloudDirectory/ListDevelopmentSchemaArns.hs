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
-- Module      : Amazonka.CloudDirectory.ListDevelopmentSchemaArns
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves each Amazon Resource Name (ARN) of schemas in the development
-- state.
--
-- This operation returns paginated results.
module Amazonka.CloudDirectory.ListDevelopmentSchemaArns
  ( -- * Creating a Request
    ListDevelopmentSchemaArns (..),
    newListDevelopmentSchemaArns,

    -- * Request Lenses
    listDevelopmentSchemaArns_nextToken,
    listDevelopmentSchemaArns_maxResults,

    -- * Destructuring the Response
    ListDevelopmentSchemaArnsResponse (..),
    newListDevelopmentSchemaArnsResponse,

    -- * Response Lenses
    listDevelopmentSchemaArnsResponse_nextToken,
    listDevelopmentSchemaArnsResponse_schemaArns,
    listDevelopmentSchemaArnsResponse_httpStatus,
  )
where

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDevelopmentSchemaArns' smart constructor.
data ListDevelopmentSchemaArns = ListDevelopmentSchemaArns'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to retrieve.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDevelopmentSchemaArns' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDevelopmentSchemaArns_nextToken' - The pagination token.
--
-- 'maxResults', 'listDevelopmentSchemaArns_maxResults' - The maximum number of results to retrieve.
newListDevelopmentSchemaArns ::
  ListDevelopmentSchemaArns
newListDevelopmentSchemaArns =
  ListDevelopmentSchemaArns'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The pagination token.
listDevelopmentSchemaArns_nextToken :: Lens.Lens' ListDevelopmentSchemaArns (Prelude.Maybe Prelude.Text)
listDevelopmentSchemaArns_nextToken = Lens.lens (\ListDevelopmentSchemaArns' {nextToken} -> nextToken) (\s@ListDevelopmentSchemaArns' {} a -> s {nextToken = a} :: ListDevelopmentSchemaArns)

-- | The maximum number of results to retrieve.
listDevelopmentSchemaArns_maxResults :: Lens.Lens' ListDevelopmentSchemaArns (Prelude.Maybe Prelude.Natural)
listDevelopmentSchemaArns_maxResults = Lens.lens (\ListDevelopmentSchemaArns' {maxResults} -> maxResults) (\s@ListDevelopmentSchemaArns' {} a -> s {maxResults = a} :: ListDevelopmentSchemaArns)

instance Core.AWSPager ListDevelopmentSchemaArns where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDevelopmentSchemaArnsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDevelopmentSchemaArnsResponse_schemaArns
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDevelopmentSchemaArns_nextToken
          Lens..~ rs
          Lens.^? listDevelopmentSchemaArnsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListDevelopmentSchemaArns where
  type
    AWSResponse ListDevelopmentSchemaArns =
      ListDevelopmentSchemaArnsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDevelopmentSchemaArnsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "SchemaArns" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDevelopmentSchemaArns where
  hashWithSalt _salt ListDevelopmentSchemaArns' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListDevelopmentSchemaArns where
  rnf ListDevelopmentSchemaArns' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListDevelopmentSchemaArns where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON ListDevelopmentSchemaArns where
  toJSON ListDevelopmentSchemaArns' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListDevelopmentSchemaArns where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/schema/development"

instance Data.ToQuery ListDevelopmentSchemaArns where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDevelopmentSchemaArnsResponse' smart constructor.
data ListDevelopmentSchemaArnsResponse = ListDevelopmentSchemaArnsResponse'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARNs of retrieved development schemas.
    schemaArns :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDevelopmentSchemaArnsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDevelopmentSchemaArnsResponse_nextToken' - The pagination token.
--
-- 'schemaArns', 'listDevelopmentSchemaArnsResponse_schemaArns' - The ARNs of retrieved development schemas.
--
-- 'httpStatus', 'listDevelopmentSchemaArnsResponse_httpStatus' - The response's http status code.
newListDevelopmentSchemaArnsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDevelopmentSchemaArnsResponse
newListDevelopmentSchemaArnsResponse pHttpStatus_ =
  ListDevelopmentSchemaArnsResponse'
    { nextToken =
        Prelude.Nothing,
      schemaArns = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token.
listDevelopmentSchemaArnsResponse_nextToken :: Lens.Lens' ListDevelopmentSchemaArnsResponse (Prelude.Maybe Prelude.Text)
listDevelopmentSchemaArnsResponse_nextToken = Lens.lens (\ListDevelopmentSchemaArnsResponse' {nextToken} -> nextToken) (\s@ListDevelopmentSchemaArnsResponse' {} a -> s {nextToken = a} :: ListDevelopmentSchemaArnsResponse)

-- | The ARNs of retrieved development schemas.
listDevelopmentSchemaArnsResponse_schemaArns :: Lens.Lens' ListDevelopmentSchemaArnsResponse (Prelude.Maybe [Prelude.Text])
listDevelopmentSchemaArnsResponse_schemaArns = Lens.lens (\ListDevelopmentSchemaArnsResponse' {schemaArns} -> schemaArns) (\s@ListDevelopmentSchemaArnsResponse' {} a -> s {schemaArns = a} :: ListDevelopmentSchemaArnsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDevelopmentSchemaArnsResponse_httpStatus :: Lens.Lens' ListDevelopmentSchemaArnsResponse Prelude.Int
listDevelopmentSchemaArnsResponse_httpStatus = Lens.lens (\ListDevelopmentSchemaArnsResponse' {httpStatus} -> httpStatus) (\s@ListDevelopmentSchemaArnsResponse' {} a -> s {httpStatus = a} :: ListDevelopmentSchemaArnsResponse)

instance
  Prelude.NFData
    ListDevelopmentSchemaArnsResponse
  where
  rnf ListDevelopmentSchemaArnsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf schemaArns
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Amazonka.CloudDirectory.ListPublishedSchemaArns
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the major version families of each published schema. If a major
-- version ARN is provided as @SchemaArn@, the minor version revisions in
-- that family are listed instead.
--
-- This operation returns paginated results.
module Amazonka.CloudDirectory.ListPublishedSchemaArns
  ( -- * Creating a Request
    ListPublishedSchemaArns (..),
    newListPublishedSchemaArns,

    -- * Request Lenses
    listPublishedSchemaArns_maxResults,
    listPublishedSchemaArns_nextToken,
    listPublishedSchemaArns_schemaArn,

    -- * Destructuring the Response
    ListPublishedSchemaArnsResponse (..),
    newListPublishedSchemaArnsResponse,

    -- * Response Lenses
    listPublishedSchemaArnsResponse_nextToken,
    listPublishedSchemaArnsResponse_schemaArns,
    listPublishedSchemaArnsResponse_httpStatus,
  )
where

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPublishedSchemaArns' smart constructor.
data ListPublishedSchemaArns = ListPublishedSchemaArns'
  { -- | The maximum number of results to retrieve.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response for @ListPublishedSchemaArns@ when this parameter is used
    -- will list all minor version ARNs for a major version.
    schemaArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPublishedSchemaArns' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listPublishedSchemaArns_maxResults' - The maximum number of results to retrieve.
--
-- 'nextToken', 'listPublishedSchemaArns_nextToken' - The pagination token.
--
-- 'schemaArn', 'listPublishedSchemaArns_schemaArn' - The response for @ListPublishedSchemaArns@ when this parameter is used
-- will list all minor version ARNs for a major version.
newListPublishedSchemaArns ::
  ListPublishedSchemaArns
newListPublishedSchemaArns =
  ListPublishedSchemaArns'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      schemaArn = Prelude.Nothing
    }

-- | The maximum number of results to retrieve.
listPublishedSchemaArns_maxResults :: Lens.Lens' ListPublishedSchemaArns (Prelude.Maybe Prelude.Natural)
listPublishedSchemaArns_maxResults = Lens.lens (\ListPublishedSchemaArns' {maxResults} -> maxResults) (\s@ListPublishedSchemaArns' {} a -> s {maxResults = a} :: ListPublishedSchemaArns)

-- | The pagination token.
listPublishedSchemaArns_nextToken :: Lens.Lens' ListPublishedSchemaArns (Prelude.Maybe Prelude.Text)
listPublishedSchemaArns_nextToken = Lens.lens (\ListPublishedSchemaArns' {nextToken} -> nextToken) (\s@ListPublishedSchemaArns' {} a -> s {nextToken = a} :: ListPublishedSchemaArns)

-- | The response for @ListPublishedSchemaArns@ when this parameter is used
-- will list all minor version ARNs for a major version.
listPublishedSchemaArns_schemaArn :: Lens.Lens' ListPublishedSchemaArns (Prelude.Maybe Prelude.Text)
listPublishedSchemaArns_schemaArn = Lens.lens (\ListPublishedSchemaArns' {schemaArn} -> schemaArn) (\s@ListPublishedSchemaArns' {} a -> s {schemaArn = a} :: ListPublishedSchemaArns)

instance Core.AWSPager ListPublishedSchemaArns where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPublishedSchemaArnsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPublishedSchemaArnsResponse_schemaArns
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listPublishedSchemaArns_nextToken
              Lens..~ rs
              Lens.^? listPublishedSchemaArnsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListPublishedSchemaArns where
  type
    AWSResponse ListPublishedSchemaArns =
      ListPublishedSchemaArnsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPublishedSchemaArnsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "SchemaArns" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPublishedSchemaArns where
  hashWithSalt _salt ListPublishedSchemaArns' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` schemaArn

instance Prelude.NFData ListPublishedSchemaArns where
  rnf ListPublishedSchemaArns' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf schemaArn

instance Data.ToHeaders ListPublishedSchemaArns where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON ListPublishedSchemaArns where
  toJSON ListPublishedSchemaArns' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SchemaArn" Data..=) Prelude.<$> schemaArn
          ]
      )

instance Data.ToPath ListPublishedSchemaArns where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/schema/published"

instance Data.ToQuery ListPublishedSchemaArns where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPublishedSchemaArnsResponse' smart constructor.
data ListPublishedSchemaArnsResponse = ListPublishedSchemaArnsResponse'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARNs of published schemas.
    schemaArns :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPublishedSchemaArnsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPublishedSchemaArnsResponse_nextToken' - The pagination token.
--
-- 'schemaArns', 'listPublishedSchemaArnsResponse_schemaArns' - The ARNs of published schemas.
--
-- 'httpStatus', 'listPublishedSchemaArnsResponse_httpStatus' - The response's http status code.
newListPublishedSchemaArnsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPublishedSchemaArnsResponse
newListPublishedSchemaArnsResponse pHttpStatus_ =
  ListPublishedSchemaArnsResponse'
    { nextToken =
        Prelude.Nothing,
      schemaArns = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token.
listPublishedSchemaArnsResponse_nextToken :: Lens.Lens' ListPublishedSchemaArnsResponse (Prelude.Maybe Prelude.Text)
listPublishedSchemaArnsResponse_nextToken = Lens.lens (\ListPublishedSchemaArnsResponse' {nextToken} -> nextToken) (\s@ListPublishedSchemaArnsResponse' {} a -> s {nextToken = a} :: ListPublishedSchemaArnsResponse)

-- | The ARNs of published schemas.
listPublishedSchemaArnsResponse_schemaArns :: Lens.Lens' ListPublishedSchemaArnsResponse (Prelude.Maybe [Prelude.Text])
listPublishedSchemaArnsResponse_schemaArns = Lens.lens (\ListPublishedSchemaArnsResponse' {schemaArns} -> schemaArns) (\s@ListPublishedSchemaArnsResponse' {} a -> s {schemaArns = a} :: ListPublishedSchemaArnsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPublishedSchemaArnsResponse_httpStatus :: Lens.Lens' ListPublishedSchemaArnsResponse Prelude.Int
listPublishedSchemaArnsResponse_httpStatus = Lens.lens (\ListPublishedSchemaArnsResponse' {httpStatus} -> httpStatus) (\s@ListPublishedSchemaArnsResponse' {} a -> s {httpStatus = a} :: ListPublishedSchemaArnsResponse)

instance
  Prelude.NFData
    ListPublishedSchemaArnsResponse
  where
  rnf ListPublishedSchemaArnsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf schemaArns `Prelude.seq`
        Prelude.rnf httpStatus

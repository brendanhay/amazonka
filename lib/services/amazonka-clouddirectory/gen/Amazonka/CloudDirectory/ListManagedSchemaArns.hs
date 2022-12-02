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
-- Module      : Amazonka.CloudDirectory.ListManagedSchemaArns
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the major version families of each managed schema. If a major
-- version ARN is provided as SchemaArn, the minor version revisions in
-- that family are listed instead.
--
-- This operation returns paginated results.
module Amazonka.CloudDirectory.ListManagedSchemaArns
  ( -- * Creating a Request
    ListManagedSchemaArns (..),
    newListManagedSchemaArns,

    -- * Request Lenses
    listManagedSchemaArns_nextToken,
    listManagedSchemaArns_schemaArn,
    listManagedSchemaArns_maxResults,

    -- * Destructuring the Response
    ListManagedSchemaArnsResponse (..),
    newListManagedSchemaArnsResponse,

    -- * Response Lenses
    listManagedSchemaArnsResponse_nextToken,
    listManagedSchemaArnsResponse_schemaArns,
    listManagedSchemaArnsResponse_httpStatus,
  )
where

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListManagedSchemaArns' smart constructor.
data ListManagedSchemaArns = ListManagedSchemaArns'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response for ListManagedSchemaArns. When this parameter is used, all
    -- minor version ARNs for a major version are listed.
    schemaArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to retrieve.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListManagedSchemaArns' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listManagedSchemaArns_nextToken' - The pagination token.
--
-- 'schemaArn', 'listManagedSchemaArns_schemaArn' - The response for ListManagedSchemaArns. When this parameter is used, all
-- minor version ARNs for a major version are listed.
--
-- 'maxResults', 'listManagedSchemaArns_maxResults' - The maximum number of results to retrieve.
newListManagedSchemaArns ::
  ListManagedSchemaArns
newListManagedSchemaArns =
  ListManagedSchemaArns'
    { nextToken = Prelude.Nothing,
      schemaArn = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The pagination token.
listManagedSchemaArns_nextToken :: Lens.Lens' ListManagedSchemaArns (Prelude.Maybe Prelude.Text)
listManagedSchemaArns_nextToken = Lens.lens (\ListManagedSchemaArns' {nextToken} -> nextToken) (\s@ListManagedSchemaArns' {} a -> s {nextToken = a} :: ListManagedSchemaArns)

-- | The response for ListManagedSchemaArns. When this parameter is used, all
-- minor version ARNs for a major version are listed.
listManagedSchemaArns_schemaArn :: Lens.Lens' ListManagedSchemaArns (Prelude.Maybe Prelude.Text)
listManagedSchemaArns_schemaArn = Lens.lens (\ListManagedSchemaArns' {schemaArn} -> schemaArn) (\s@ListManagedSchemaArns' {} a -> s {schemaArn = a} :: ListManagedSchemaArns)

-- | The maximum number of results to retrieve.
listManagedSchemaArns_maxResults :: Lens.Lens' ListManagedSchemaArns (Prelude.Maybe Prelude.Natural)
listManagedSchemaArns_maxResults = Lens.lens (\ListManagedSchemaArns' {maxResults} -> maxResults) (\s@ListManagedSchemaArns' {} a -> s {maxResults = a} :: ListManagedSchemaArns)

instance Core.AWSPager ListManagedSchemaArns where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listManagedSchemaArnsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listManagedSchemaArnsResponse_schemaArns
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listManagedSchemaArns_nextToken
          Lens..~ rs
          Lens.^? listManagedSchemaArnsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListManagedSchemaArns where
  type
    AWSResponse ListManagedSchemaArns =
      ListManagedSchemaArnsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListManagedSchemaArnsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "SchemaArns" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListManagedSchemaArns where
  hashWithSalt _salt ListManagedSchemaArns' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` schemaArn
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListManagedSchemaArns where
  rnf ListManagedSchemaArns' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf schemaArn
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListManagedSchemaArns where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON ListManagedSchemaArns where
  toJSON ListManagedSchemaArns' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SchemaArn" Data..=) Prelude.<$> schemaArn,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListManagedSchemaArns where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/schema/managed"

instance Data.ToQuery ListManagedSchemaArns where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListManagedSchemaArnsResponse' smart constructor.
data ListManagedSchemaArnsResponse = ListManagedSchemaArnsResponse'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARNs for all AWS managed schemas.
    schemaArns :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListManagedSchemaArnsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listManagedSchemaArnsResponse_nextToken' - The pagination token.
--
-- 'schemaArns', 'listManagedSchemaArnsResponse_schemaArns' - The ARNs for all AWS managed schemas.
--
-- 'httpStatus', 'listManagedSchemaArnsResponse_httpStatus' - The response's http status code.
newListManagedSchemaArnsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListManagedSchemaArnsResponse
newListManagedSchemaArnsResponse pHttpStatus_ =
  ListManagedSchemaArnsResponse'
    { nextToken =
        Prelude.Nothing,
      schemaArns = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token.
listManagedSchemaArnsResponse_nextToken :: Lens.Lens' ListManagedSchemaArnsResponse (Prelude.Maybe Prelude.Text)
listManagedSchemaArnsResponse_nextToken = Lens.lens (\ListManagedSchemaArnsResponse' {nextToken} -> nextToken) (\s@ListManagedSchemaArnsResponse' {} a -> s {nextToken = a} :: ListManagedSchemaArnsResponse)

-- | The ARNs for all AWS managed schemas.
listManagedSchemaArnsResponse_schemaArns :: Lens.Lens' ListManagedSchemaArnsResponse (Prelude.Maybe [Prelude.Text])
listManagedSchemaArnsResponse_schemaArns = Lens.lens (\ListManagedSchemaArnsResponse' {schemaArns} -> schemaArns) (\s@ListManagedSchemaArnsResponse' {} a -> s {schemaArns = a} :: ListManagedSchemaArnsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listManagedSchemaArnsResponse_httpStatus :: Lens.Lens' ListManagedSchemaArnsResponse Prelude.Int
listManagedSchemaArnsResponse_httpStatus = Lens.lens (\ListManagedSchemaArnsResponse' {httpStatus} -> httpStatus) (\s@ListManagedSchemaArnsResponse' {} a -> s {httpStatus = a} :: ListManagedSchemaArnsResponse)

instance Prelude.NFData ListManagedSchemaArnsResponse where
  rnf ListManagedSchemaArnsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf schemaArns
      `Prelude.seq` Prelude.rnf httpStatus

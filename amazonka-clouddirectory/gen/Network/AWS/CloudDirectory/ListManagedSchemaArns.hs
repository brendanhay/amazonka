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
-- Module      : Network.AWS.CloudDirectory.ListManagedSchemaArns
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.CloudDirectory.ListManagedSchemaArns
  ( -- * Creating a Request
    ListManagedSchemaArns (..),
    newListManagedSchemaArns,

    -- * Request Lenses
    listManagedSchemaArns_schemaArn,
    listManagedSchemaArns_nextToken,
    listManagedSchemaArns_maxResults,

    -- * Destructuring the Response
    ListManagedSchemaArnsResponse (..),
    newListManagedSchemaArnsResponse,

    -- * Response Lenses
    listManagedSchemaArnsResponse_schemaArns,
    listManagedSchemaArnsResponse_nextToken,
    listManagedSchemaArnsResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListManagedSchemaArns' smart constructor.
data ListManagedSchemaArns = ListManagedSchemaArns'
  { -- | The response for ListManagedSchemaArns. When this parameter is used, all
    -- minor version ARNs for a major version are listed.
    schemaArn :: Prelude.Maybe Prelude.Text,
    -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'schemaArn', 'listManagedSchemaArns_schemaArn' - The response for ListManagedSchemaArns. When this parameter is used, all
-- minor version ARNs for a major version are listed.
--
-- 'nextToken', 'listManagedSchemaArns_nextToken' - The pagination token.
--
-- 'maxResults', 'listManagedSchemaArns_maxResults' - The maximum number of results to retrieve.
newListManagedSchemaArns ::
  ListManagedSchemaArns
newListManagedSchemaArns =
  ListManagedSchemaArns'
    { schemaArn = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The response for ListManagedSchemaArns. When this parameter is used, all
-- minor version ARNs for a major version are listed.
listManagedSchemaArns_schemaArn :: Lens.Lens' ListManagedSchemaArns (Prelude.Maybe Prelude.Text)
listManagedSchemaArns_schemaArn = Lens.lens (\ListManagedSchemaArns' {schemaArn} -> schemaArn) (\s@ListManagedSchemaArns' {} a -> s {schemaArn = a} :: ListManagedSchemaArns)

-- | The pagination token.
listManagedSchemaArns_nextToken :: Lens.Lens' ListManagedSchemaArns (Prelude.Maybe Prelude.Text)
listManagedSchemaArns_nextToken = Lens.lens (\ListManagedSchemaArns' {nextToken} -> nextToken) (\s@ListManagedSchemaArns' {} a -> s {nextToken = a} :: ListManagedSchemaArns)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListManagedSchemaArnsResponse'
            Prelude.<$> (x Core..?> "SchemaArns" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListManagedSchemaArns

instance Prelude.NFData ListManagedSchemaArns

instance Core.ToHeaders ListManagedSchemaArns where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON ListManagedSchemaArns where
  toJSON ListManagedSchemaArns' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SchemaArn" Core..=) Prelude.<$> schemaArn,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListManagedSchemaArns where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/schema/managed"

instance Core.ToQuery ListManagedSchemaArns where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListManagedSchemaArnsResponse' smart constructor.
data ListManagedSchemaArnsResponse = ListManagedSchemaArnsResponse'
  { -- | The ARNs for all AWS managed schemas.
    schemaArns :: Prelude.Maybe [Prelude.Text],
    -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'schemaArns', 'listManagedSchemaArnsResponse_schemaArns' - The ARNs for all AWS managed schemas.
--
-- 'nextToken', 'listManagedSchemaArnsResponse_nextToken' - The pagination token.
--
-- 'httpStatus', 'listManagedSchemaArnsResponse_httpStatus' - The response's http status code.
newListManagedSchemaArnsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListManagedSchemaArnsResponse
newListManagedSchemaArnsResponse pHttpStatus_ =
  ListManagedSchemaArnsResponse'
    { schemaArns =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARNs for all AWS managed schemas.
listManagedSchemaArnsResponse_schemaArns :: Lens.Lens' ListManagedSchemaArnsResponse (Prelude.Maybe [Prelude.Text])
listManagedSchemaArnsResponse_schemaArns = Lens.lens (\ListManagedSchemaArnsResponse' {schemaArns} -> schemaArns) (\s@ListManagedSchemaArnsResponse' {} a -> s {schemaArns = a} :: ListManagedSchemaArnsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The pagination token.
listManagedSchemaArnsResponse_nextToken :: Lens.Lens' ListManagedSchemaArnsResponse (Prelude.Maybe Prelude.Text)
listManagedSchemaArnsResponse_nextToken = Lens.lens (\ListManagedSchemaArnsResponse' {nextToken} -> nextToken) (\s@ListManagedSchemaArnsResponse' {} a -> s {nextToken = a} :: ListManagedSchemaArnsResponse)

-- | The response's http status code.
listManagedSchemaArnsResponse_httpStatus :: Lens.Lens' ListManagedSchemaArnsResponse Prelude.Int
listManagedSchemaArnsResponse_httpStatus = Lens.lens (\ListManagedSchemaArnsResponse' {httpStatus} -> httpStatus) (\s@ListManagedSchemaArnsResponse' {} a -> s {httpStatus = a} :: ListManagedSchemaArnsResponse)

instance Prelude.NFData ListManagedSchemaArnsResponse

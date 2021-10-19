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
-- Module      : Network.AWS.CloudDirectory.ListPublishedSchemaArns
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the major version families of each published schema. If a major
-- version ARN is provided as @SchemaArn@, the minor version revisions in
-- that family are listed instead.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListPublishedSchemaArns
  ( -- * Creating a Request
    ListPublishedSchemaArns (..),
    newListPublishedSchemaArns,

    -- * Request Lenses
    listPublishedSchemaArns_nextToken,
    listPublishedSchemaArns_schemaArn,
    listPublishedSchemaArns_maxResults,

    -- * Destructuring the Response
    ListPublishedSchemaArnsResponse (..),
    newListPublishedSchemaArnsResponse,

    -- * Response Lenses
    listPublishedSchemaArnsResponse_schemaArns,
    listPublishedSchemaArnsResponse_nextToken,
    listPublishedSchemaArnsResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListPublishedSchemaArns' smart constructor.
data ListPublishedSchemaArns = ListPublishedSchemaArns'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response for @ListPublishedSchemaArns@ when this parameter is used
    -- will list all minor version ARNs for a major version.
    schemaArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to retrieve.
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'nextToken', 'listPublishedSchemaArns_nextToken' - The pagination token.
--
-- 'schemaArn', 'listPublishedSchemaArns_schemaArn' - The response for @ListPublishedSchemaArns@ when this parameter is used
-- will list all minor version ARNs for a major version.
--
-- 'maxResults', 'listPublishedSchemaArns_maxResults' - The maximum number of results to retrieve.
newListPublishedSchemaArns ::
  ListPublishedSchemaArns
newListPublishedSchemaArns =
  ListPublishedSchemaArns'
    { nextToken =
        Prelude.Nothing,
      schemaArn = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The pagination token.
listPublishedSchemaArns_nextToken :: Lens.Lens' ListPublishedSchemaArns (Prelude.Maybe Prelude.Text)
listPublishedSchemaArns_nextToken = Lens.lens (\ListPublishedSchemaArns' {nextToken} -> nextToken) (\s@ListPublishedSchemaArns' {} a -> s {nextToken = a} :: ListPublishedSchemaArns)

-- | The response for @ListPublishedSchemaArns@ when this parameter is used
-- will list all minor version ARNs for a major version.
listPublishedSchemaArns_schemaArn :: Lens.Lens' ListPublishedSchemaArns (Prelude.Maybe Prelude.Text)
listPublishedSchemaArns_schemaArn = Lens.lens (\ListPublishedSchemaArns' {schemaArn} -> schemaArn) (\s@ListPublishedSchemaArns' {} a -> s {schemaArn = a} :: ListPublishedSchemaArns)

-- | The maximum number of results to retrieve.
listPublishedSchemaArns_maxResults :: Lens.Lens' ListPublishedSchemaArns (Prelude.Maybe Prelude.Natural)
listPublishedSchemaArns_maxResults = Lens.lens (\ListPublishedSchemaArns' {maxResults} -> maxResults) (\s@ListPublishedSchemaArns' {} a -> s {maxResults = a} :: ListPublishedSchemaArns)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPublishedSchemaArnsResponse'
            Prelude.<$> (x Core..?> "SchemaArns" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPublishedSchemaArns

instance Prelude.NFData ListPublishedSchemaArns

instance Core.ToHeaders ListPublishedSchemaArns where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON ListPublishedSchemaArns where
  toJSON ListPublishedSchemaArns' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("SchemaArn" Core..=) Prelude.<$> schemaArn,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListPublishedSchemaArns where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/schema/published"

instance Core.ToQuery ListPublishedSchemaArns where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPublishedSchemaArnsResponse' smart constructor.
data ListPublishedSchemaArnsResponse = ListPublishedSchemaArnsResponse'
  { -- | The ARNs of published schemas.
    schemaArns :: Prelude.Maybe [Prelude.Text],
    -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'schemaArns', 'listPublishedSchemaArnsResponse_schemaArns' - The ARNs of published schemas.
--
-- 'nextToken', 'listPublishedSchemaArnsResponse_nextToken' - The pagination token.
--
-- 'httpStatus', 'listPublishedSchemaArnsResponse_httpStatus' - The response's http status code.
newListPublishedSchemaArnsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPublishedSchemaArnsResponse
newListPublishedSchemaArnsResponse pHttpStatus_ =
  ListPublishedSchemaArnsResponse'
    { schemaArns =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARNs of published schemas.
listPublishedSchemaArnsResponse_schemaArns :: Lens.Lens' ListPublishedSchemaArnsResponse (Prelude.Maybe [Prelude.Text])
listPublishedSchemaArnsResponse_schemaArns = Lens.lens (\ListPublishedSchemaArnsResponse' {schemaArns} -> schemaArns) (\s@ListPublishedSchemaArnsResponse' {} a -> s {schemaArns = a} :: ListPublishedSchemaArnsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token.
listPublishedSchemaArnsResponse_nextToken :: Lens.Lens' ListPublishedSchemaArnsResponse (Prelude.Maybe Prelude.Text)
listPublishedSchemaArnsResponse_nextToken = Lens.lens (\ListPublishedSchemaArnsResponse' {nextToken} -> nextToken) (\s@ListPublishedSchemaArnsResponse' {} a -> s {nextToken = a} :: ListPublishedSchemaArnsResponse)

-- | The response's http status code.
listPublishedSchemaArnsResponse_httpStatus :: Lens.Lens' ListPublishedSchemaArnsResponse Prelude.Int
listPublishedSchemaArnsResponse_httpStatus = Lens.lens (\ListPublishedSchemaArnsResponse' {httpStatus} -> httpStatus) (\s@ListPublishedSchemaArnsResponse' {} a -> s {httpStatus = a} :: ListPublishedSchemaArnsResponse)

instance
  Prelude.NFData
    ListPublishedSchemaArnsResponse

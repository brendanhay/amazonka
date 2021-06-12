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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListManagedSchemaArns' smart constructor.
data ListManagedSchemaArns = ListManagedSchemaArns'
  { -- | The response for ListManagedSchemaArns. When this parameter is used, all
    -- minor version ARNs for a major version are listed.
    schemaArn :: Core.Maybe Core.Text,
    -- | The pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to retrieve.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { schemaArn = Core.Nothing,
      nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The response for ListManagedSchemaArns. When this parameter is used, all
-- minor version ARNs for a major version are listed.
listManagedSchemaArns_schemaArn :: Lens.Lens' ListManagedSchemaArns (Core.Maybe Core.Text)
listManagedSchemaArns_schemaArn = Lens.lens (\ListManagedSchemaArns' {schemaArn} -> schemaArn) (\s@ListManagedSchemaArns' {} a -> s {schemaArn = a} :: ListManagedSchemaArns)

-- | The pagination token.
listManagedSchemaArns_nextToken :: Lens.Lens' ListManagedSchemaArns (Core.Maybe Core.Text)
listManagedSchemaArns_nextToken = Lens.lens (\ListManagedSchemaArns' {nextToken} -> nextToken) (\s@ListManagedSchemaArns' {} a -> s {nextToken = a} :: ListManagedSchemaArns)

-- | The maximum number of results to retrieve.
listManagedSchemaArns_maxResults :: Lens.Lens' ListManagedSchemaArns (Core.Maybe Core.Natural)
listManagedSchemaArns_maxResults = Lens.lens (\ListManagedSchemaArns' {maxResults} -> maxResults) (\s@ListManagedSchemaArns' {} a -> s {maxResults = a} :: ListManagedSchemaArns)

instance Core.AWSPager ListManagedSchemaArns where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listManagedSchemaArnsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listManagedSchemaArnsResponse_schemaArns
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listManagedSchemaArns_nextToken
          Lens..~ rs
          Lens.^? listManagedSchemaArnsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListManagedSchemaArns where
  type
    AWSResponse ListManagedSchemaArns =
      ListManagedSchemaArnsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListManagedSchemaArnsResponse'
            Core.<$> (x Core..?> "SchemaArns" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListManagedSchemaArns

instance Core.NFData ListManagedSchemaArns

instance Core.ToHeaders ListManagedSchemaArns where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON ListManagedSchemaArns where
  toJSON ListManagedSchemaArns' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SchemaArn" Core..=) Core.<$> schemaArn,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListManagedSchemaArns where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/schema/managed"

instance Core.ToQuery ListManagedSchemaArns where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListManagedSchemaArnsResponse' smart constructor.
data ListManagedSchemaArnsResponse = ListManagedSchemaArnsResponse'
  { -- | The ARNs for all AWS managed schemas.
    schemaArns :: Core.Maybe [Core.Text],
    -- | The pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListManagedSchemaArnsResponse
newListManagedSchemaArnsResponse pHttpStatus_ =
  ListManagedSchemaArnsResponse'
    { schemaArns =
        Core.Nothing,
      nextToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARNs for all AWS managed schemas.
listManagedSchemaArnsResponse_schemaArns :: Lens.Lens' ListManagedSchemaArnsResponse (Core.Maybe [Core.Text])
listManagedSchemaArnsResponse_schemaArns = Lens.lens (\ListManagedSchemaArnsResponse' {schemaArns} -> schemaArns) (\s@ListManagedSchemaArnsResponse' {} a -> s {schemaArns = a} :: ListManagedSchemaArnsResponse) Core.. Lens.mapping Lens._Coerce

-- | The pagination token.
listManagedSchemaArnsResponse_nextToken :: Lens.Lens' ListManagedSchemaArnsResponse (Core.Maybe Core.Text)
listManagedSchemaArnsResponse_nextToken = Lens.lens (\ListManagedSchemaArnsResponse' {nextToken} -> nextToken) (\s@ListManagedSchemaArnsResponse' {} a -> s {nextToken = a} :: ListManagedSchemaArnsResponse)

-- | The response's http status code.
listManagedSchemaArnsResponse_httpStatus :: Lens.Lens' ListManagedSchemaArnsResponse Core.Int
listManagedSchemaArnsResponse_httpStatus = Lens.lens (\ListManagedSchemaArnsResponse' {httpStatus} -> httpStatus) (\s@ListManagedSchemaArnsResponse' {} a -> s {httpStatus = a} :: ListManagedSchemaArnsResponse)

instance Core.NFData ListManagedSchemaArnsResponse

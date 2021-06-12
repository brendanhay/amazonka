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
-- Module      : Network.AWS.CloudDirectory.ListDevelopmentSchemaArns
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves each Amazon Resource Name (ARN) of schemas in the development
-- state.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListDevelopmentSchemaArns
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
    listDevelopmentSchemaArnsResponse_schemaArns,
    listDevelopmentSchemaArnsResponse_nextToken,
    listDevelopmentSchemaArnsResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDevelopmentSchemaArns' smart constructor.
data ListDevelopmentSchemaArns = ListDevelopmentSchemaArns'
  { -- | The pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to retrieve.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The pagination token.
listDevelopmentSchemaArns_nextToken :: Lens.Lens' ListDevelopmentSchemaArns (Core.Maybe Core.Text)
listDevelopmentSchemaArns_nextToken = Lens.lens (\ListDevelopmentSchemaArns' {nextToken} -> nextToken) (\s@ListDevelopmentSchemaArns' {} a -> s {nextToken = a} :: ListDevelopmentSchemaArns)

-- | The maximum number of results to retrieve.
listDevelopmentSchemaArns_maxResults :: Lens.Lens' ListDevelopmentSchemaArns (Core.Maybe Core.Natural)
listDevelopmentSchemaArns_maxResults = Lens.lens (\ListDevelopmentSchemaArns' {maxResults} -> maxResults) (\s@ListDevelopmentSchemaArns' {} a -> s {maxResults = a} :: ListDevelopmentSchemaArns)

instance Core.AWSPager ListDevelopmentSchemaArns where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDevelopmentSchemaArnsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listDevelopmentSchemaArnsResponse_schemaArns
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listDevelopmentSchemaArns_nextToken
          Lens..~ rs
          Lens.^? listDevelopmentSchemaArnsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListDevelopmentSchemaArns where
  type
    AWSResponse ListDevelopmentSchemaArns =
      ListDevelopmentSchemaArnsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDevelopmentSchemaArnsResponse'
            Core.<$> (x Core..?> "SchemaArns" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListDevelopmentSchemaArns

instance Core.NFData ListDevelopmentSchemaArns

instance Core.ToHeaders ListDevelopmentSchemaArns where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON ListDevelopmentSchemaArns where
  toJSON ListDevelopmentSchemaArns' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListDevelopmentSchemaArns where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/schema/development"

instance Core.ToQuery ListDevelopmentSchemaArns where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListDevelopmentSchemaArnsResponse' smart constructor.
data ListDevelopmentSchemaArnsResponse = ListDevelopmentSchemaArnsResponse'
  { -- | The ARNs of retrieved development schemas.
    schemaArns :: Core.Maybe [Core.Text],
    -- | The pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDevelopmentSchemaArnsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaArns', 'listDevelopmentSchemaArnsResponse_schemaArns' - The ARNs of retrieved development schemas.
--
-- 'nextToken', 'listDevelopmentSchemaArnsResponse_nextToken' - The pagination token.
--
-- 'httpStatus', 'listDevelopmentSchemaArnsResponse_httpStatus' - The response's http status code.
newListDevelopmentSchemaArnsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListDevelopmentSchemaArnsResponse
newListDevelopmentSchemaArnsResponse pHttpStatus_ =
  ListDevelopmentSchemaArnsResponse'
    { schemaArns =
        Core.Nothing,
      nextToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARNs of retrieved development schemas.
listDevelopmentSchemaArnsResponse_schemaArns :: Lens.Lens' ListDevelopmentSchemaArnsResponse (Core.Maybe [Core.Text])
listDevelopmentSchemaArnsResponse_schemaArns = Lens.lens (\ListDevelopmentSchemaArnsResponse' {schemaArns} -> schemaArns) (\s@ListDevelopmentSchemaArnsResponse' {} a -> s {schemaArns = a} :: ListDevelopmentSchemaArnsResponse) Core.. Lens.mapping Lens._Coerce

-- | The pagination token.
listDevelopmentSchemaArnsResponse_nextToken :: Lens.Lens' ListDevelopmentSchemaArnsResponse (Core.Maybe Core.Text)
listDevelopmentSchemaArnsResponse_nextToken = Lens.lens (\ListDevelopmentSchemaArnsResponse' {nextToken} -> nextToken) (\s@ListDevelopmentSchemaArnsResponse' {} a -> s {nextToken = a} :: ListDevelopmentSchemaArnsResponse)

-- | The response's http status code.
listDevelopmentSchemaArnsResponse_httpStatus :: Lens.Lens' ListDevelopmentSchemaArnsResponse Core.Int
listDevelopmentSchemaArnsResponse_httpStatus = Lens.lens (\ListDevelopmentSchemaArnsResponse' {httpStatus} -> httpStatus) (\s@ListDevelopmentSchemaArnsResponse' {} a -> s {httpStatus = a} :: ListDevelopmentSchemaArnsResponse)

instance
  Core.NFData
    ListDevelopmentSchemaArnsResponse

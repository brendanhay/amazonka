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
-- Module      : Network.AWS.Greengrass.ListResourceDefinitions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of resource definitions.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListResourceDefinitions
  ( -- * Creating a Request
    ListResourceDefinitions (..),
    newListResourceDefinitions,

    -- * Request Lenses
    listResourceDefinitions_nextToken,
    listResourceDefinitions_maxResults,

    -- * Destructuring the Response
    ListResourceDefinitionsResponse (..),
    newListResourceDefinitionsResponse,

    -- * Response Lenses
    listResourceDefinitionsResponse_nextToken,
    listResourceDefinitionsResponse_definitions,
    listResourceDefinitionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListResourceDefinitions' smart constructor.
data ListResourceDefinitions = ListResourceDefinitions'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListResourceDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResourceDefinitions_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'maxResults', 'listResourceDefinitions_maxResults' - The maximum number of results to be returned per request.
newListResourceDefinitions ::
  ListResourceDefinitions
newListResourceDefinitions =
  ListResourceDefinitions'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listResourceDefinitions_nextToken :: Lens.Lens' ListResourceDefinitions (Core.Maybe Core.Text)
listResourceDefinitions_nextToken = Lens.lens (\ListResourceDefinitions' {nextToken} -> nextToken) (\s@ListResourceDefinitions' {} a -> s {nextToken = a} :: ListResourceDefinitions)

-- | The maximum number of results to be returned per request.
listResourceDefinitions_maxResults :: Lens.Lens' ListResourceDefinitions (Core.Maybe Core.Text)
listResourceDefinitions_maxResults = Lens.lens (\ListResourceDefinitions' {maxResults} -> maxResults) (\s@ListResourceDefinitions' {} a -> s {maxResults = a} :: ListResourceDefinitions)

instance Core.AWSPager ListResourceDefinitions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listResourceDefinitionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listResourceDefinitionsResponse_definitions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listResourceDefinitions_nextToken
          Lens..~ rs
          Lens.^? listResourceDefinitionsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListResourceDefinitions where
  type
    AWSResponse ListResourceDefinitions =
      ListResourceDefinitionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourceDefinitionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Definitions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListResourceDefinitions

instance Core.NFData ListResourceDefinitions

instance Core.ToHeaders ListResourceDefinitions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListResourceDefinitions where
  toPath =
    Core.const "/greengrass/definition/resources"

instance Core.ToQuery ListResourceDefinitions where
  toQuery ListResourceDefinitions' {..} =
    Core.mconcat
      [ "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListResourceDefinitionsResponse' smart constructor.
data ListResourceDefinitionsResponse = ListResourceDefinitionsResponse'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about a definition.
    definitions :: Core.Maybe [DefinitionInformation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListResourceDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResourceDefinitionsResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'definitions', 'listResourceDefinitionsResponse_definitions' - Information about a definition.
--
-- 'httpStatus', 'listResourceDefinitionsResponse_httpStatus' - The response's http status code.
newListResourceDefinitionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListResourceDefinitionsResponse
newListResourceDefinitionsResponse pHttpStatus_ =
  ListResourceDefinitionsResponse'
    { nextToken =
        Core.Nothing,
      definitions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listResourceDefinitionsResponse_nextToken :: Lens.Lens' ListResourceDefinitionsResponse (Core.Maybe Core.Text)
listResourceDefinitionsResponse_nextToken = Lens.lens (\ListResourceDefinitionsResponse' {nextToken} -> nextToken) (\s@ListResourceDefinitionsResponse' {} a -> s {nextToken = a} :: ListResourceDefinitionsResponse)

-- | Information about a definition.
listResourceDefinitionsResponse_definitions :: Lens.Lens' ListResourceDefinitionsResponse (Core.Maybe [DefinitionInformation])
listResourceDefinitionsResponse_definitions = Lens.lens (\ListResourceDefinitionsResponse' {definitions} -> definitions) (\s@ListResourceDefinitionsResponse' {} a -> s {definitions = a} :: ListResourceDefinitionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listResourceDefinitionsResponse_httpStatus :: Lens.Lens' ListResourceDefinitionsResponse Core.Int
listResourceDefinitionsResponse_httpStatus = Lens.lens (\ListResourceDefinitionsResponse' {httpStatus} -> httpStatus) (\s@ListResourceDefinitionsResponse' {} a -> s {httpStatus = a} :: ListResourceDefinitionsResponse)

instance Core.NFData ListResourceDefinitionsResponse

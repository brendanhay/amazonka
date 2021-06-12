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
-- Module      : Network.AWS.Greengrass.ListFunctionDefinitions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of Lambda function definitions.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListFunctionDefinitions
  ( -- * Creating a Request
    ListFunctionDefinitions (..),
    newListFunctionDefinitions,

    -- * Request Lenses
    listFunctionDefinitions_nextToken,
    listFunctionDefinitions_maxResults,

    -- * Destructuring the Response
    ListFunctionDefinitionsResponse (..),
    newListFunctionDefinitionsResponse,

    -- * Response Lenses
    listFunctionDefinitionsResponse_nextToken,
    listFunctionDefinitionsResponse_definitions,
    listFunctionDefinitionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListFunctionDefinitions' smart constructor.
data ListFunctionDefinitions = ListFunctionDefinitions'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListFunctionDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFunctionDefinitions_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'maxResults', 'listFunctionDefinitions_maxResults' - The maximum number of results to be returned per request.
newListFunctionDefinitions ::
  ListFunctionDefinitions
newListFunctionDefinitions =
  ListFunctionDefinitions'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listFunctionDefinitions_nextToken :: Lens.Lens' ListFunctionDefinitions (Core.Maybe Core.Text)
listFunctionDefinitions_nextToken = Lens.lens (\ListFunctionDefinitions' {nextToken} -> nextToken) (\s@ListFunctionDefinitions' {} a -> s {nextToken = a} :: ListFunctionDefinitions)

-- | The maximum number of results to be returned per request.
listFunctionDefinitions_maxResults :: Lens.Lens' ListFunctionDefinitions (Core.Maybe Core.Text)
listFunctionDefinitions_maxResults = Lens.lens (\ListFunctionDefinitions' {maxResults} -> maxResults) (\s@ListFunctionDefinitions' {} a -> s {maxResults = a} :: ListFunctionDefinitions)

instance Core.AWSPager ListFunctionDefinitions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFunctionDefinitionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listFunctionDefinitionsResponse_definitions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listFunctionDefinitions_nextToken
          Lens..~ rs
          Lens.^? listFunctionDefinitionsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListFunctionDefinitions where
  type
    AWSResponse ListFunctionDefinitions =
      ListFunctionDefinitionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFunctionDefinitionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Definitions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListFunctionDefinitions

instance Core.NFData ListFunctionDefinitions

instance Core.ToHeaders ListFunctionDefinitions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListFunctionDefinitions where
  toPath =
    Core.const "/greengrass/definition/functions"

instance Core.ToQuery ListFunctionDefinitions where
  toQuery ListFunctionDefinitions' {..} =
    Core.mconcat
      [ "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListFunctionDefinitionsResponse' smart constructor.
data ListFunctionDefinitionsResponse = ListFunctionDefinitionsResponse'
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
-- Create a value of 'ListFunctionDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFunctionDefinitionsResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'definitions', 'listFunctionDefinitionsResponse_definitions' - Information about a definition.
--
-- 'httpStatus', 'listFunctionDefinitionsResponse_httpStatus' - The response's http status code.
newListFunctionDefinitionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListFunctionDefinitionsResponse
newListFunctionDefinitionsResponse pHttpStatus_ =
  ListFunctionDefinitionsResponse'
    { nextToken =
        Core.Nothing,
      definitions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listFunctionDefinitionsResponse_nextToken :: Lens.Lens' ListFunctionDefinitionsResponse (Core.Maybe Core.Text)
listFunctionDefinitionsResponse_nextToken = Lens.lens (\ListFunctionDefinitionsResponse' {nextToken} -> nextToken) (\s@ListFunctionDefinitionsResponse' {} a -> s {nextToken = a} :: ListFunctionDefinitionsResponse)

-- | Information about a definition.
listFunctionDefinitionsResponse_definitions :: Lens.Lens' ListFunctionDefinitionsResponse (Core.Maybe [DefinitionInformation])
listFunctionDefinitionsResponse_definitions = Lens.lens (\ListFunctionDefinitionsResponse' {definitions} -> definitions) (\s@ListFunctionDefinitionsResponse' {} a -> s {definitions = a} :: ListFunctionDefinitionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listFunctionDefinitionsResponse_httpStatus :: Lens.Lens' ListFunctionDefinitionsResponse Core.Int
listFunctionDefinitionsResponse_httpStatus = Lens.lens (\ListFunctionDefinitionsResponse' {httpStatus} -> httpStatus) (\s@ListFunctionDefinitionsResponse' {} a -> s {httpStatus = a} :: ListFunctionDefinitionsResponse)

instance Core.NFData ListFunctionDefinitionsResponse

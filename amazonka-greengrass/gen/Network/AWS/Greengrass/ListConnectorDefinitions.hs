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
-- Module      : Network.AWS.Greengrass.ListConnectorDefinitions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of connector definitions.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListConnectorDefinitions
  ( -- * Creating a Request
    ListConnectorDefinitions (..),
    newListConnectorDefinitions,

    -- * Request Lenses
    listConnectorDefinitions_nextToken,
    listConnectorDefinitions_maxResults,

    -- * Destructuring the Response
    ListConnectorDefinitionsResponse (..),
    newListConnectorDefinitionsResponse,

    -- * Response Lenses
    listConnectorDefinitionsResponse_nextToken,
    listConnectorDefinitionsResponse_definitions,
    listConnectorDefinitionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListConnectorDefinitions' smart constructor.
data ListConnectorDefinitions = ListConnectorDefinitions'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListConnectorDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listConnectorDefinitions_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'maxResults', 'listConnectorDefinitions_maxResults' - The maximum number of results to be returned per request.
newListConnectorDefinitions ::
  ListConnectorDefinitions
newListConnectorDefinitions =
  ListConnectorDefinitions'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listConnectorDefinitions_nextToken :: Lens.Lens' ListConnectorDefinitions (Core.Maybe Core.Text)
listConnectorDefinitions_nextToken = Lens.lens (\ListConnectorDefinitions' {nextToken} -> nextToken) (\s@ListConnectorDefinitions' {} a -> s {nextToken = a} :: ListConnectorDefinitions)

-- | The maximum number of results to be returned per request.
listConnectorDefinitions_maxResults :: Lens.Lens' ListConnectorDefinitions (Core.Maybe Core.Text)
listConnectorDefinitions_maxResults = Lens.lens (\ListConnectorDefinitions' {maxResults} -> maxResults) (\s@ListConnectorDefinitions' {} a -> s {maxResults = a} :: ListConnectorDefinitions)

instance Core.AWSPager ListConnectorDefinitions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listConnectorDefinitionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listConnectorDefinitionsResponse_definitions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listConnectorDefinitions_nextToken
          Lens..~ rs
          Lens.^? listConnectorDefinitionsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListConnectorDefinitions where
  type
    AWSResponse ListConnectorDefinitions =
      ListConnectorDefinitionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListConnectorDefinitionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Definitions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListConnectorDefinitions

instance Core.NFData ListConnectorDefinitions

instance Core.ToHeaders ListConnectorDefinitions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListConnectorDefinitions where
  toPath =
    Core.const "/greengrass/definition/connectors"

instance Core.ToQuery ListConnectorDefinitions where
  toQuery ListConnectorDefinitions' {..} =
    Core.mconcat
      [ "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListConnectorDefinitionsResponse' smart constructor.
data ListConnectorDefinitionsResponse = ListConnectorDefinitionsResponse'
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
-- Create a value of 'ListConnectorDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listConnectorDefinitionsResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'definitions', 'listConnectorDefinitionsResponse_definitions' - Information about a definition.
--
-- 'httpStatus', 'listConnectorDefinitionsResponse_httpStatus' - The response's http status code.
newListConnectorDefinitionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListConnectorDefinitionsResponse
newListConnectorDefinitionsResponse pHttpStatus_ =
  ListConnectorDefinitionsResponse'
    { nextToken =
        Core.Nothing,
      definitions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listConnectorDefinitionsResponse_nextToken :: Lens.Lens' ListConnectorDefinitionsResponse (Core.Maybe Core.Text)
listConnectorDefinitionsResponse_nextToken = Lens.lens (\ListConnectorDefinitionsResponse' {nextToken} -> nextToken) (\s@ListConnectorDefinitionsResponse' {} a -> s {nextToken = a} :: ListConnectorDefinitionsResponse)

-- | Information about a definition.
listConnectorDefinitionsResponse_definitions :: Lens.Lens' ListConnectorDefinitionsResponse (Core.Maybe [DefinitionInformation])
listConnectorDefinitionsResponse_definitions = Lens.lens (\ListConnectorDefinitionsResponse' {definitions} -> definitions) (\s@ListConnectorDefinitionsResponse' {} a -> s {definitions = a} :: ListConnectorDefinitionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listConnectorDefinitionsResponse_httpStatus :: Lens.Lens' ListConnectorDefinitionsResponse Core.Int
listConnectorDefinitionsResponse_httpStatus = Lens.lens (\ListConnectorDefinitionsResponse' {httpStatus} -> httpStatus) (\s@ListConnectorDefinitionsResponse' {} a -> s {httpStatus = a} :: ListConnectorDefinitionsResponse)

instance Core.NFData ListConnectorDefinitionsResponse

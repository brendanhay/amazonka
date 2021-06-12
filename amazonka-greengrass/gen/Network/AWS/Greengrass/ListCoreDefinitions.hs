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
-- Module      : Network.AWS.Greengrass.ListCoreDefinitions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of core definitions.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListCoreDefinitions
  ( -- * Creating a Request
    ListCoreDefinitions (..),
    newListCoreDefinitions,

    -- * Request Lenses
    listCoreDefinitions_nextToken,
    listCoreDefinitions_maxResults,

    -- * Destructuring the Response
    ListCoreDefinitionsResponse (..),
    newListCoreDefinitionsResponse,

    -- * Response Lenses
    listCoreDefinitionsResponse_nextToken,
    listCoreDefinitionsResponse_definitions,
    listCoreDefinitionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListCoreDefinitions' smart constructor.
data ListCoreDefinitions = ListCoreDefinitions'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListCoreDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCoreDefinitions_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'maxResults', 'listCoreDefinitions_maxResults' - The maximum number of results to be returned per request.
newListCoreDefinitions ::
  ListCoreDefinitions
newListCoreDefinitions =
  ListCoreDefinitions'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listCoreDefinitions_nextToken :: Lens.Lens' ListCoreDefinitions (Core.Maybe Core.Text)
listCoreDefinitions_nextToken = Lens.lens (\ListCoreDefinitions' {nextToken} -> nextToken) (\s@ListCoreDefinitions' {} a -> s {nextToken = a} :: ListCoreDefinitions)

-- | The maximum number of results to be returned per request.
listCoreDefinitions_maxResults :: Lens.Lens' ListCoreDefinitions (Core.Maybe Core.Text)
listCoreDefinitions_maxResults = Lens.lens (\ListCoreDefinitions' {maxResults} -> maxResults) (\s@ListCoreDefinitions' {} a -> s {maxResults = a} :: ListCoreDefinitions)

instance Core.AWSPager ListCoreDefinitions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCoreDefinitionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listCoreDefinitionsResponse_definitions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listCoreDefinitions_nextToken
          Lens..~ rs
          Lens.^? listCoreDefinitionsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListCoreDefinitions where
  type
    AWSResponse ListCoreDefinitions =
      ListCoreDefinitionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCoreDefinitionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Definitions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListCoreDefinitions

instance Core.NFData ListCoreDefinitions

instance Core.ToHeaders ListCoreDefinitions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListCoreDefinitions where
  toPath = Core.const "/greengrass/definition/cores"

instance Core.ToQuery ListCoreDefinitions where
  toQuery ListCoreDefinitions' {..} =
    Core.mconcat
      [ "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListCoreDefinitionsResponse' smart constructor.
data ListCoreDefinitionsResponse = ListCoreDefinitionsResponse'
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
-- Create a value of 'ListCoreDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCoreDefinitionsResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'definitions', 'listCoreDefinitionsResponse_definitions' - Information about a definition.
--
-- 'httpStatus', 'listCoreDefinitionsResponse_httpStatus' - The response's http status code.
newListCoreDefinitionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListCoreDefinitionsResponse
newListCoreDefinitionsResponse pHttpStatus_ =
  ListCoreDefinitionsResponse'
    { nextToken =
        Core.Nothing,
      definitions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listCoreDefinitionsResponse_nextToken :: Lens.Lens' ListCoreDefinitionsResponse (Core.Maybe Core.Text)
listCoreDefinitionsResponse_nextToken = Lens.lens (\ListCoreDefinitionsResponse' {nextToken} -> nextToken) (\s@ListCoreDefinitionsResponse' {} a -> s {nextToken = a} :: ListCoreDefinitionsResponse)

-- | Information about a definition.
listCoreDefinitionsResponse_definitions :: Lens.Lens' ListCoreDefinitionsResponse (Core.Maybe [DefinitionInformation])
listCoreDefinitionsResponse_definitions = Lens.lens (\ListCoreDefinitionsResponse' {definitions} -> definitions) (\s@ListCoreDefinitionsResponse' {} a -> s {definitions = a} :: ListCoreDefinitionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listCoreDefinitionsResponse_httpStatus :: Lens.Lens' ListCoreDefinitionsResponse Core.Int
listCoreDefinitionsResponse_httpStatus = Lens.lens (\ListCoreDefinitionsResponse' {httpStatus} -> httpStatus) (\s@ListCoreDefinitionsResponse' {} a -> s {httpStatus = a} :: ListCoreDefinitionsResponse)

instance Core.NFData ListCoreDefinitionsResponse

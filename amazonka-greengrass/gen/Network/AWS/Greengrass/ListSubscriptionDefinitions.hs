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
-- Module      : Network.AWS.Greengrass.ListSubscriptionDefinitions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of subscription definitions.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListSubscriptionDefinitions
  ( -- * Creating a Request
    ListSubscriptionDefinitions (..),
    newListSubscriptionDefinitions,

    -- * Request Lenses
    listSubscriptionDefinitions_nextToken,
    listSubscriptionDefinitions_maxResults,

    -- * Destructuring the Response
    ListSubscriptionDefinitionsResponse (..),
    newListSubscriptionDefinitionsResponse,

    -- * Response Lenses
    listSubscriptionDefinitionsResponse_nextToken,
    listSubscriptionDefinitionsResponse_definitions,
    listSubscriptionDefinitionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListSubscriptionDefinitions' smart constructor.
data ListSubscriptionDefinitions = ListSubscriptionDefinitions'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListSubscriptionDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSubscriptionDefinitions_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'maxResults', 'listSubscriptionDefinitions_maxResults' - The maximum number of results to be returned per request.
newListSubscriptionDefinitions ::
  ListSubscriptionDefinitions
newListSubscriptionDefinitions =
  ListSubscriptionDefinitions'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listSubscriptionDefinitions_nextToken :: Lens.Lens' ListSubscriptionDefinitions (Core.Maybe Core.Text)
listSubscriptionDefinitions_nextToken = Lens.lens (\ListSubscriptionDefinitions' {nextToken} -> nextToken) (\s@ListSubscriptionDefinitions' {} a -> s {nextToken = a} :: ListSubscriptionDefinitions)

-- | The maximum number of results to be returned per request.
listSubscriptionDefinitions_maxResults :: Lens.Lens' ListSubscriptionDefinitions (Core.Maybe Core.Text)
listSubscriptionDefinitions_maxResults = Lens.lens (\ListSubscriptionDefinitions' {maxResults} -> maxResults) (\s@ListSubscriptionDefinitions' {} a -> s {maxResults = a} :: ListSubscriptionDefinitions)

instance Core.AWSPager ListSubscriptionDefinitions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSubscriptionDefinitionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listSubscriptionDefinitionsResponse_definitions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listSubscriptionDefinitions_nextToken
          Lens..~ rs
          Lens.^? listSubscriptionDefinitionsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListSubscriptionDefinitions where
  type
    AWSResponse ListSubscriptionDefinitions =
      ListSubscriptionDefinitionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSubscriptionDefinitionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Definitions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListSubscriptionDefinitions

instance Core.NFData ListSubscriptionDefinitions

instance Core.ToHeaders ListSubscriptionDefinitions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListSubscriptionDefinitions where
  toPath =
    Core.const "/greengrass/definition/subscriptions"

instance Core.ToQuery ListSubscriptionDefinitions where
  toQuery ListSubscriptionDefinitions' {..} =
    Core.mconcat
      [ "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListSubscriptionDefinitionsResponse' smart constructor.
data ListSubscriptionDefinitionsResponse = ListSubscriptionDefinitionsResponse'
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
-- Create a value of 'ListSubscriptionDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSubscriptionDefinitionsResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'definitions', 'listSubscriptionDefinitionsResponse_definitions' - Information about a definition.
--
-- 'httpStatus', 'listSubscriptionDefinitionsResponse_httpStatus' - The response's http status code.
newListSubscriptionDefinitionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListSubscriptionDefinitionsResponse
newListSubscriptionDefinitionsResponse pHttpStatus_ =
  ListSubscriptionDefinitionsResponse'
    { nextToken =
        Core.Nothing,
      definitions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listSubscriptionDefinitionsResponse_nextToken :: Lens.Lens' ListSubscriptionDefinitionsResponse (Core.Maybe Core.Text)
listSubscriptionDefinitionsResponse_nextToken = Lens.lens (\ListSubscriptionDefinitionsResponse' {nextToken} -> nextToken) (\s@ListSubscriptionDefinitionsResponse' {} a -> s {nextToken = a} :: ListSubscriptionDefinitionsResponse)

-- | Information about a definition.
listSubscriptionDefinitionsResponse_definitions :: Lens.Lens' ListSubscriptionDefinitionsResponse (Core.Maybe [DefinitionInformation])
listSubscriptionDefinitionsResponse_definitions = Lens.lens (\ListSubscriptionDefinitionsResponse' {definitions} -> definitions) (\s@ListSubscriptionDefinitionsResponse' {} a -> s {definitions = a} :: ListSubscriptionDefinitionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listSubscriptionDefinitionsResponse_httpStatus :: Lens.Lens' ListSubscriptionDefinitionsResponse Core.Int
listSubscriptionDefinitionsResponse_httpStatus = Lens.lens (\ListSubscriptionDefinitionsResponse' {httpStatus} -> httpStatus) (\s@ListSubscriptionDefinitionsResponse' {} a -> s {httpStatus = a} :: ListSubscriptionDefinitionsResponse)

instance
  Core.NFData
    ListSubscriptionDefinitionsResponse

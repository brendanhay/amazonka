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
-- Module      : Network.AWS.Greengrass.ListDeviceDefinitions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of device definitions.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListDeviceDefinitions
  ( -- * Creating a Request
    ListDeviceDefinitions (..),
    newListDeviceDefinitions,

    -- * Request Lenses
    listDeviceDefinitions_nextToken,
    listDeviceDefinitions_maxResults,

    -- * Destructuring the Response
    ListDeviceDefinitionsResponse (..),
    newListDeviceDefinitionsResponse,

    -- * Response Lenses
    listDeviceDefinitionsResponse_nextToken,
    listDeviceDefinitionsResponse_definitions,
    listDeviceDefinitionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDeviceDefinitions' smart constructor.
data ListDeviceDefinitions = ListDeviceDefinitions'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDeviceDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDeviceDefinitions_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'maxResults', 'listDeviceDefinitions_maxResults' - The maximum number of results to be returned per request.
newListDeviceDefinitions ::
  ListDeviceDefinitions
newListDeviceDefinitions =
  ListDeviceDefinitions'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listDeviceDefinitions_nextToken :: Lens.Lens' ListDeviceDefinitions (Core.Maybe Core.Text)
listDeviceDefinitions_nextToken = Lens.lens (\ListDeviceDefinitions' {nextToken} -> nextToken) (\s@ListDeviceDefinitions' {} a -> s {nextToken = a} :: ListDeviceDefinitions)

-- | The maximum number of results to be returned per request.
listDeviceDefinitions_maxResults :: Lens.Lens' ListDeviceDefinitions (Core.Maybe Core.Text)
listDeviceDefinitions_maxResults = Lens.lens (\ListDeviceDefinitions' {maxResults} -> maxResults) (\s@ListDeviceDefinitions' {} a -> s {maxResults = a} :: ListDeviceDefinitions)

instance Core.AWSPager ListDeviceDefinitions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDeviceDefinitionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listDeviceDefinitionsResponse_definitions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listDeviceDefinitions_nextToken
          Lens..~ rs
          Lens.^? listDeviceDefinitionsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListDeviceDefinitions where
  type
    AWSResponse ListDeviceDefinitions =
      ListDeviceDefinitionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeviceDefinitionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Definitions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListDeviceDefinitions

instance Core.NFData ListDeviceDefinitions

instance Core.ToHeaders ListDeviceDefinitions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListDeviceDefinitions where
  toPath = Core.const "/greengrass/definition/devices"

instance Core.ToQuery ListDeviceDefinitions where
  toQuery ListDeviceDefinitions' {..} =
    Core.mconcat
      [ "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListDeviceDefinitionsResponse' smart constructor.
data ListDeviceDefinitionsResponse = ListDeviceDefinitionsResponse'
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
-- Create a value of 'ListDeviceDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDeviceDefinitionsResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'definitions', 'listDeviceDefinitionsResponse_definitions' - Information about a definition.
--
-- 'httpStatus', 'listDeviceDefinitionsResponse_httpStatus' - The response's http status code.
newListDeviceDefinitionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListDeviceDefinitionsResponse
newListDeviceDefinitionsResponse pHttpStatus_ =
  ListDeviceDefinitionsResponse'
    { nextToken =
        Core.Nothing,
      definitions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listDeviceDefinitionsResponse_nextToken :: Lens.Lens' ListDeviceDefinitionsResponse (Core.Maybe Core.Text)
listDeviceDefinitionsResponse_nextToken = Lens.lens (\ListDeviceDefinitionsResponse' {nextToken} -> nextToken) (\s@ListDeviceDefinitionsResponse' {} a -> s {nextToken = a} :: ListDeviceDefinitionsResponse)

-- | Information about a definition.
listDeviceDefinitionsResponse_definitions :: Lens.Lens' ListDeviceDefinitionsResponse (Core.Maybe [DefinitionInformation])
listDeviceDefinitionsResponse_definitions = Lens.lens (\ListDeviceDefinitionsResponse' {definitions} -> definitions) (\s@ListDeviceDefinitionsResponse' {} a -> s {definitions = a} :: ListDeviceDefinitionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listDeviceDefinitionsResponse_httpStatus :: Lens.Lens' ListDeviceDefinitionsResponse Core.Int
listDeviceDefinitionsResponse_httpStatus = Lens.lens (\ListDeviceDefinitionsResponse' {httpStatus} -> httpStatus) (\s@ListDeviceDefinitionsResponse' {} a -> s {httpStatus = a} :: ListDeviceDefinitionsResponse)

instance Core.NFData ListDeviceDefinitionsResponse

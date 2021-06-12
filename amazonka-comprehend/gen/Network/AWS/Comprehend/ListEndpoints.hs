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
-- Module      : Network.AWS.Comprehend.ListEndpoints
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of all existing endpoints that you\'ve created.
module Network.AWS.Comprehend.ListEndpoints
  ( -- * Creating a Request
    ListEndpoints (..),
    newListEndpoints,

    -- * Request Lenses
    listEndpoints_nextToken,
    listEndpoints_maxResults,
    listEndpoints_filter,

    -- * Destructuring the Response
    ListEndpointsResponse (..),
    newListEndpointsResponse,

    -- * Response Lenses
    listEndpointsResponse_nextToken,
    listEndpointsResponse_endpointPropertiesList,
    listEndpointsResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListEndpoints' smart constructor.
data ListEndpoints = ListEndpoints'
  { -- | Identifies the next page of results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return in each page. The default is
    -- 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | Filters the endpoints that are returned. You can filter endpoints on
    -- their name, model, status, or the date and time that they were created.
    -- You can only set one filter at a time.
    filter' :: Core.Maybe EndpointFilter
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEndpoints_nextToken' - Identifies the next page of results to return.
--
-- 'maxResults', 'listEndpoints_maxResults' - The maximum number of results to return in each page. The default is
-- 100.
--
-- 'filter'', 'listEndpoints_filter' - Filters the endpoints that are returned. You can filter endpoints on
-- their name, model, status, or the date and time that they were created.
-- You can only set one filter at a time.
newListEndpoints ::
  ListEndpoints
newListEndpoints =
  ListEndpoints'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      filter' = Core.Nothing
    }

-- | Identifies the next page of results to return.
listEndpoints_nextToken :: Lens.Lens' ListEndpoints (Core.Maybe Core.Text)
listEndpoints_nextToken = Lens.lens (\ListEndpoints' {nextToken} -> nextToken) (\s@ListEndpoints' {} a -> s {nextToken = a} :: ListEndpoints)

-- | The maximum number of results to return in each page. The default is
-- 100.
listEndpoints_maxResults :: Lens.Lens' ListEndpoints (Core.Maybe Core.Natural)
listEndpoints_maxResults = Lens.lens (\ListEndpoints' {maxResults} -> maxResults) (\s@ListEndpoints' {} a -> s {maxResults = a} :: ListEndpoints)

-- | Filters the endpoints that are returned. You can filter endpoints on
-- their name, model, status, or the date and time that they were created.
-- You can only set one filter at a time.
listEndpoints_filter :: Lens.Lens' ListEndpoints (Core.Maybe EndpointFilter)
listEndpoints_filter = Lens.lens (\ListEndpoints' {filter'} -> filter') (\s@ListEndpoints' {} a -> s {filter' = a} :: ListEndpoints)

instance Core.AWSRequest ListEndpoints where
  type
    AWSResponse ListEndpoints =
      ListEndpointsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEndpointsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "EndpointPropertiesList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListEndpoints

instance Core.NFData ListEndpoints

instance Core.ToHeaders ListEndpoints where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.ListEndpoints" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListEndpoints where
  toJSON ListEndpoints' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filter" Core..=) Core.<$> filter'
          ]
      )

instance Core.ToPath ListEndpoints where
  toPath = Core.const "/"

instance Core.ToQuery ListEndpoints where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListEndpointsResponse' smart constructor.
data ListEndpointsResponse = ListEndpointsResponse'
  { -- | Identifies the next page of results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Displays a list of endpoint properties being retrieved by the service in
    -- response to the request.
    endpointPropertiesList :: Core.Maybe [EndpointProperties],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListEndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEndpointsResponse_nextToken' - Identifies the next page of results to return.
--
-- 'endpointPropertiesList', 'listEndpointsResponse_endpointPropertiesList' - Displays a list of endpoint properties being retrieved by the service in
-- response to the request.
--
-- 'httpStatus', 'listEndpointsResponse_httpStatus' - The response's http status code.
newListEndpointsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListEndpointsResponse
newListEndpointsResponse pHttpStatus_ =
  ListEndpointsResponse'
    { nextToken = Core.Nothing,
      endpointPropertiesList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Identifies the next page of results to return.
listEndpointsResponse_nextToken :: Lens.Lens' ListEndpointsResponse (Core.Maybe Core.Text)
listEndpointsResponse_nextToken = Lens.lens (\ListEndpointsResponse' {nextToken} -> nextToken) (\s@ListEndpointsResponse' {} a -> s {nextToken = a} :: ListEndpointsResponse)

-- | Displays a list of endpoint properties being retrieved by the service in
-- response to the request.
listEndpointsResponse_endpointPropertiesList :: Lens.Lens' ListEndpointsResponse (Core.Maybe [EndpointProperties])
listEndpointsResponse_endpointPropertiesList = Lens.lens (\ListEndpointsResponse' {endpointPropertiesList} -> endpointPropertiesList) (\s@ListEndpointsResponse' {} a -> s {endpointPropertiesList = a} :: ListEndpointsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listEndpointsResponse_httpStatus :: Lens.Lens' ListEndpointsResponse Core.Int
listEndpointsResponse_httpStatus = Lens.lens (\ListEndpointsResponse' {httpStatus} -> httpStatus) (\s@ListEndpointsResponse' {} a -> s {httpStatus = a} :: ListEndpointsResponse)

instance Core.NFData ListEndpointsResponse

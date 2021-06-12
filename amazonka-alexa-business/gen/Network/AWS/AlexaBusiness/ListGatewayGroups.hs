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
-- Module      : Network.AWS.AlexaBusiness.ListGatewayGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of gateway group summaries. Use GetGatewayGroup to
-- retrieve details of a specific gateway group.
module Network.AWS.AlexaBusiness.ListGatewayGroups
  ( -- * Creating a Request
    ListGatewayGroups (..),
    newListGatewayGroups,

    -- * Request Lenses
    listGatewayGroups_nextToken,
    listGatewayGroups_maxResults,

    -- * Destructuring the Response
    ListGatewayGroupsResponse (..),
    newListGatewayGroupsResponse,

    -- * Response Lenses
    listGatewayGroupsResponse_nextToken,
    listGatewayGroupsResponse_gatewayGroups,
    listGatewayGroupsResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListGatewayGroups' smart constructor.
data ListGatewayGroups = ListGatewayGroups'
  { -- | The token used to paginate though multiple pages of gateway group
    -- summaries.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of gateway group summaries to return. The default is
    -- 50.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListGatewayGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listGatewayGroups_nextToken' - The token used to paginate though multiple pages of gateway group
-- summaries.
--
-- 'maxResults', 'listGatewayGroups_maxResults' - The maximum number of gateway group summaries to return. The default is
-- 50.
newListGatewayGroups ::
  ListGatewayGroups
newListGatewayGroups =
  ListGatewayGroups'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The token used to paginate though multiple pages of gateway group
-- summaries.
listGatewayGroups_nextToken :: Lens.Lens' ListGatewayGroups (Core.Maybe Core.Text)
listGatewayGroups_nextToken = Lens.lens (\ListGatewayGroups' {nextToken} -> nextToken) (\s@ListGatewayGroups' {} a -> s {nextToken = a} :: ListGatewayGroups)

-- | The maximum number of gateway group summaries to return. The default is
-- 50.
listGatewayGroups_maxResults :: Lens.Lens' ListGatewayGroups (Core.Maybe Core.Natural)
listGatewayGroups_maxResults = Lens.lens (\ListGatewayGroups' {maxResults} -> maxResults) (\s@ListGatewayGroups' {} a -> s {maxResults = a} :: ListGatewayGroups)

instance Core.AWSRequest ListGatewayGroups where
  type
    AWSResponse ListGatewayGroups =
      ListGatewayGroupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGatewayGroupsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "GatewayGroups" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListGatewayGroups

instance Core.NFData ListGatewayGroups

instance Core.ToHeaders ListGatewayGroups where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.ListGatewayGroups" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListGatewayGroups where
  toJSON ListGatewayGroups' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListGatewayGroups where
  toPath = Core.const "/"

instance Core.ToQuery ListGatewayGroups where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListGatewayGroupsResponse' smart constructor.
data ListGatewayGroupsResponse = ListGatewayGroupsResponse'
  { -- | The token used to paginate though multiple pages of gateway group
    -- summaries.
    nextToken :: Core.Maybe Core.Text,
    -- | The gateway groups in the list.
    gatewayGroups :: Core.Maybe [GatewayGroupSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListGatewayGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listGatewayGroupsResponse_nextToken' - The token used to paginate though multiple pages of gateway group
-- summaries.
--
-- 'gatewayGroups', 'listGatewayGroupsResponse_gatewayGroups' - The gateway groups in the list.
--
-- 'httpStatus', 'listGatewayGroupsResponse_httpStatus' - The response's http status code.
newListGatewayGroupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListGatewayGroupsResponse
newListGatewayGroupsResponse pHttpStatus_ =
  ListGatewayGroupsResponse'
    { nextToken =
        Core.Nothing,
      gatewayGroups = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token used to paginate though multiple pages of gateway group
-- summaries.
listGatewayGroupsResponse_nextToken :: Lens.Lens' ListGatewayGroupsResponse (Core.Maybe Core.Text)
listGatewayGroupsResponse_nextToken = Lens.lens (\ListGatewayGroupsResponse' {nextToken} -> nextToken) (\s@ListGatewayGroupsResponse' {} a -> s {nextToken = a} :: ListGatewayGroupsResponse)

-- | The gateway groups in the list.
listGatewayGroupsResponse_gatewayGroups :: Lens.Lens' ListGatewayGroupsResponse (Core.Maybe [GatewayGroupSummary])
listGatewayGroupsResponse_gatewayGroups = Lens.lens (\ListGatewayGroupsResponse' {gatewayGroups} -> gatewayGroups) (\s@ListGatewayGroupsResponse' {} a -> s {gatewayGroups = a} :: ListGatewayGroupsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listGatewayGroupsResponse_httpStatus :: Lens.Lens' ListGatewayGroupsResponse Core.Int
listGatewayGroupsResponse_httpStatus = Lens.lens (\ListGatewayGroupsResponse' {httpStatus} -> httpStatus) (\s@ListGatewayGroupsResponse' {} a -> s {httpStatus = a} :: ListGatewayGroupsResponse)

instance Core.NFData ListGatewayGroupsResponse

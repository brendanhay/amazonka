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
-- Module      : Network.AWS.SMS.GetConnectors
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the connectors registered with the AWS SMS.
--
-- This operation returns paginated results.
module Network.AWS.SMS.GetConnectors
  ( -- * Creating a Request
    GetConnectors (..),
    newGetConnectors,

    -- * Request Lenses
    getConnectors_nextToken,
    getConnectors_maxResults,

    -- * Destructuring the Response
    GetConnectorsResponse (..),
    newGetConnectorsResponse,

    -- * Response Lenses
    getConnectorsResponse_nextToken,
    getConnectorsResponse_connectorList,
    getConnectorsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newGetConnectors' smart constructor.
data GetConnectors = GetConnectors'
  { -- | The token for the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return in a single call. The default
    -- value is 50. To retrieve the remaining results, make another call with
    -- the returned @NextToken@ value.
    maxResults :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetConnectors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getConnectors_nextToken' - The token for the next set of results.
--
-- 'maxResults', 'getConnectors_maxResults' - The maximum number of results to return in a single call. The default
-- value is 50. To retrieve the remaining results, make another call with
-- the returned @NextToken@ value.
newGetConnectors ::
  GetConnectors
newGetConnectors =
  GetConnectors'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The token for the next set of results.
getConnectors_nextToken :: Lens.Lens' GetConnectors (Core.Maybe Core.Text)
getConnectors_nextToken = Lens.lens (\GetConnectors' {nextToken} -> nextToken) (\s@GetConnectors' {} a -> s {nextToken = a} :: GetConnectors)

-- | The maximum number of results to return in a single call. The default
-- value is 50. To retrieve the remaining results, make another call with
-- the returned @NextToken@ value.
getConnectors_maxResults :: Lens.Lens' GetConnectors (Core.Maybe Core.Int)
getConnectors_maxResults = Lens.lens (\GetConnectors' {maxResults} -> maxResults) (\s@GetConnectors' {} a -> s {maxResults = a} :: GetConnectors)

instance Core.AWSPager GetConnectors where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getConnectorsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getConnectorsResponse_connectorList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getConnectors_nextToken
          Lens..~ rs
          Lens.^? getConnectorsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest GetConnectors where
  type
    AWSResponse GetConnectors =
      GetConnectorsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConnectorsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "connectorList" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetConnectors

instance Core.NFData GetConnectors

instance Core.ToHeaders GetConnectors where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSServerMigrationService_V2016_10_24.GetConnectors" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetConnectors where
  toJSON GetConnectors' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath GetConnectors where
  toPath = Core.const "/"

instance Core.ToQuery GetConnectors where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetConnectorsResponse' smart constructor.
data GetConnectorsResponse = GetConnectorsResponse'
  { -- | The token required to retrieve the next set of results. This value is
    -- null when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the registered connectors.
    connectorList :: Core.Maybe [Connector],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetConnectorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getConnectorsResponse_nextToken' - The token required to retrieve the next set of results. This value is
-- null when there are no more results to return.
--
-- 'connectorList', 'getConnectorsResponse_connectorList' - Information about the registered connectors.
--
-- 'httpStatus', 'getConnectorsResponse_httpStatus' - The response's http status code.
newGetConnectorsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetConnectorsResponse
newGetConnectorsResponse pHttpStatus_ =
  GetConnectorsResponse'
    { nextToken = Core.Nothing,
      connectorList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token required to retrieve the next set of results. This value is
-- null when there are no more results to return.
getConnectorsResponse_nextToken :: Lens.Lens' GetConnectorsResponse (Core.Maybe Core.Text)
getConnectorsResponse_nextToken = Lens.lens (\GetConnectorsResponse' {nextToken} -> nextToken) (\s@GetConnectorsResponse' {} a -> s {nextToken = a} :: GetConnectorsResponse)

-- | Information about the registered connectors.
getConnectorsResponse_connectorList :: Lens.Lens' GetConnectorsResponse (Core.Maybe [Connector])
getConnectorsResponse_connectorList = Lens.lens (\GetConnectorsResponse' {connectorList} -> connectorList) (\s@GetConnectorsResponse' {} a -> s {connectorList = a} :: GetConnectorsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getConnectorsResponse_httpStatus :: Lens.Lens' GetConnectorsResponse Core.Int
getConnectorsResponse_httpStatus = Lens.lens (\GetConnectorsResponse' {httpStatus} -> httpStatus) (\s@GetConnectorsResponse' {} a -> s {httpStatus = a} :: GetConnectorsResponse)

instance Core.NFData GetConnectorsResponse

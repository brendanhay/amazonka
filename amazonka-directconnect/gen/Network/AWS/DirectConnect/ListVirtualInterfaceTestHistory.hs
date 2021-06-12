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
-- Module      : Network.AWS.DirectConnect.ListVirtualInterfaceTestHistory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the virtual interface failover test history.
module Network.AWS.DirectConnect.ListVirtualInterfaceTestHistory
  ( -- * Creating a Request
    ListVirtualInterfaceTestHistory (..),
    newListVirtualInterfaceTestHistory,

    -- * Request Lenses
    listVirtualInterfaceTestHistory_bgpPeers,
    listVirtualInterfaceTestHistory_nextToken,
    listVirtualInterfaceTestHistory_status,
    listVirtualInterfaceTestHistory_maxResults,
    listVirtualInterfaceTestHistory_testId,
    listVirtualInterfaceTestHistory_virtualInterfaceId,

    -- * Destructuring the Response
    ListVirtualInterfaceTestHistoryResponse (..),
    newListVirtualInterfaceTestHistoryResponse,

    -- * Response Lenses
    listVirtualInterfaceTestHistoryResponse_nextToken,
    listVirtualInterfaceTestHistoryResponse_virtualInterfaceTestHistory,
    listVirtualInterfaceTestHistoryResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListVirtualInterfaceTestHistory' smart constructor.
data ListVirtualInterfaceTestHistory = ListVirtualInterfaceTestHistory'
  { -- | The BGP peers that were placed in the DOWN state during the virtual
    -- interface failover test.
    bgpPeers :: Core.Maybe [Core.Text],
    -- | The token for the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The status of the virtual interface failover test.
    status :: Core.Maybe Core.Text,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    --
    -- If @MaxResults@ is given a value larger than 100, only 100 results are
    -- returned.
    maxResults :: Core.Maybe Core.Int,
    -- | The ID of the virtual interface failover test.
    testId :: Core.Maybe Core.Text,
    -- | The ID of the virtual interface that was tested.
    virtualInterfaceId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListVirtualInterfaceTestHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bgpPeers', 'listVirtualInterfaceTestHistory_bgpPeers' - The BGP peers that were placed in the DOWN state during the virtual
-- interface failover test.
--
-- 'nextToken', 'listVirtualInterfaceTestHistory_nextToken' - The token for the next page of results.
--
-- 'status', 'listVirtualInterfaceTestHistory_status' - The status of the virtual interface failover test.
--
-- 'maxResults', 'listVirtualInterfaceTestHistory_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- If @MaxResults@ is given a value larger than 100, only 100 results are
-- returned.
--
-- 'testId', 'listVirtualInterfaceTestHistory_testId' - The ID of the virtual interface failover test.
--
-- 'virtualInterfaceId', 'listVirtualInterfaceTestHistory_virtualInterfaceId' - The ID of the virtual interface that was tested.
newListVirtualInterfaceTestHistory ::
  ListVirtualInterfaceTestHistory
newListVirtualInterfaceTestHistory =
  ListVirtualInterfaceTestHistory'
    { bgpPeers =
        Core.Nothing,
      nextToken = Core.Nothing,
      status = Core.Nothing,
      maxResults = Core.Nothing,
      testId = Core.Nothing,
      virtualInterfaceId = Core.Nothing
    }

-- | The BGP peers that were placed in the DOWN state during the virtual
-- interface failover test.
listVirtualInterfaceTestHistory_bgpPeers :: Lens.Lens' ListVirtualInterfaceTestHistory (Core.Maybe [Core.Text])
listVirtualInterfaceTestHistory_bgpPeers = Lens.lens (\ListVirtualInterfaceTestHistory' {bgpPeers} -> bgpPeers) (\s@ListVirtualInterfaceTestHistory' {} a -> s {bgpPeers = a} :: ListVirtualInterfaceTestHistory) Core.. Lens.mapping Lens._Coerce

-- | The token for the next page of results.
listVirtualInterfaceTestHistory_nextToken :: Lens.Lens' ListVirtualInterfaceTestHistory (Core.Maybe Core.Text)
listVirtualInterfaceTestHistory_nextToken = Lens.lens (\ListVirtualInterfaceTestHistory' {nextToken} -> nextToken) (\s@ListVirtualInterfaceTestHistory' {} a -> s {nextToken = a} :: ListVirtualInterfaceTestHistory)

-- | The status of the virtual interface failover test.
listVirtualInterfaceTestHistory_status :: Lens.Lens' ListVirtualInterfaceTestHistory (Core.Maybe Core.Text)
listVirtualInterfaceTestHistory_status = Lens.lens (\ListVirtualInterfaceTestHistory' {status} -> status) (\s@ListVirtualInterfaceTestHistory' {} a -> s {status = a} :: ListVirtualInterfaceTestHistory)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- If @MaxResults@ is given a value larger than 100, only 100 results are
-- returned.
listVirtualInterfaceTestHistory_maxResults :: Lens.Lens' ListVirtualInterfaceTestHistory (Core.Maybe Core.Int)
listVirtualInterfaceTestHistory_maxResults = Lens.lens (\ListVirtualInterfaceTestHistory' {maxResults} -> maxResults) (\s@ListVirtualInterfaceTestHistory' {} a -> s {maxResults = a} :: ListVirtualInterfaceTestHistory)

-- | The ID of the virtual interface failover test.
listVirtualInterfaceTestHistory_testId :: Lens.Lens' ListVirtualInterfaceTestHistory (Core.Maybe Core.Text)
listVirtualInterfaceTestHistory_testId = Lens.lens (\ListVirtualInterfaceTestHistory' {testId} -> testId) (\s@ListVirtualInterfaceTestHistory' {} a -> s {testId = a} :: ListVirtualInterfaceTestHistory)

-- | The ID of the virtual interface that was tested.
listVirtualInterfaceTestHistory_virtualInterfaceId :: Lens.Lens' ListVirtualInterfaceTestHistory (Core.Maybe Core.Text)
listVirtualInterfaceTestHistory_virtualInterfaceId = Lens.lens (\ListVirtualInterfaceTestHistory' {virtualInterfaceId} -> virtualInterfaceId) (\s@ListVirtualInterfaceTestHistory' {} a -> s {virtualInterfaceId = a} :: ListVirtualInterfaceTestHistory)

instance
  Core.AWSRequest
    ListVirtualInterfaceTestHistory
  where
  type
    AWSResponse ListVirtualInterfaceTestHistory =
      ListVirtualInterfaceTestHistoryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVirtualInterfaceTestHistoryResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> ( x Core..?> "virtualInterfaceTestHistory"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    ListVirtualInterfaceTestHistory

instance Core.NFData ListVirtualInterfaceTestHistory

instance
  Core.ToHeaders
    ListVirtualInterfaceTestHistory
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.ListVirtualInterfaceTestHistory" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListVirtualInterfaceTestHistory where
  toJSON ListVirtualInterfaceTestHistory' {..} =
    Core.object
      ( Core.catMaybes
          [ ("bgpPeers" Core..=) Core.<$> bgpPeers,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("status" Core..=) Core.<$> status,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("testId" Core..=) Core.<$> testId,
            ("virtualInterfaceId" Core..=)
              Core.<$> virtualInterfaceId
          ]
      )

instance Core.ToPath ListVirtualInterfaceTestHistory where
  toPath = Core.const "/"

instance Core.ToQuery ListVirtualInterfaceTestHistory where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListVirtualInterfaceTestHistoryResponse' smart constructor.
data ListVirtualInterfaceTestHistoryResponse = ListVirtualInterfaceTestHistoryResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The ID of the tested virtual interface.
    virtualInterfaceTestHistory :: Core.Maybe [VirtualInterfaceTestHistory],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListVirtualInterfaceTestHistoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVirtualInterfaceTestHistoryResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'virtualInterfaceTestHistory', 'listVirtualInterfaceTestHistoryResponse_virtualInterfaceTestHistory' - The ID of the tested virtual interface.
--
-- 'httpStatus', 'listVirtualInterfaceTestHistoryResponse_httpStatus' - The response's http status code.
newListVirtualInterfaceTestHistoryResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListVirtualInterfaceTestHistoryResponse
newListVirtualInterfaceTestHistoryResponse
  pHttpStatus_ =
    ListVirtualInterfaceTestHistoryResponse'
      { nextToken =
          Core.Nothing,
        virtualInterfaceTestHistory =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
listVirtualInterfaceTestHistoryResponse_nextToken :: Lens.Lens' ListVirtualInterfaceTestHistoryResponse (Core.Maybe Core.Text)
listVirtualInterfaceTestHistoryResponse_nextToken = Lens.lens (\ListVirtualInterfaceTestHistoryResponse' {nextToken} -> nextToken) (\s@ListVirtualInterfaceTestHistoryResponse' {} a -> s {nextToken = a} :: ListVirtualInterfaceTestHistoryResponse)

-- | The ID of the tested virtual interface.
listVirtualInterfaceTestHistoryResponse_virtualInterfaceTestHistory :: Lens.Lens' ListVirtualInterfaceTestHistoryResponse (Core.Maybe [VirtualInterfaceTestHistory])
listVirtualInterfaceTestHistoryResponse_virtualInterfaceTestHistory = Lens.lens (\ListVirtualInterfaceTestHistoryResponse' {virtualInterfaceTestHistory} -> virtualInterfaceTestHistory) (\s@ListVirtualInterfaceTestHistoryResponse' {} a -> s {virtualInterfaceTestHistory = a} :: ListVirtualInterfaceTestHistoryResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listVirtualInterfaceTestHistoryResponse_httpStatus :: Lens.Lens' ListVirtualInterfaceTestHistoryResponse Core.Int
listVirtualInterfaceTestHistoryResponse_httpStatus = Lens.lens (\ListVirtualInterfaceTestHistoryResponse' {httpStatus} -> httpStatus) (\s@ListVirtualInterfaceTestHistoryResponse' {} a -> s {httpStatus = a} :: ListVirtualInterfaceTestHistoryResponse)

instance
  Core.NFData
    ListVirtualInterfaceTestHistoryResponse

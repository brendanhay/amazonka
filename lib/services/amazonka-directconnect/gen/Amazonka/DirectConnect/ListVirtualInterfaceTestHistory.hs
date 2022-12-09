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
-- Module      : Amazonka.DirectConnect.ListVirtualInterfaceTestHistory
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the virtual interface failover test history.
module Amazonka.DirectConnect.ListVirtualInterfaceTestHistory
  ( -- * Creating a Request
    ListVirtualInterfaceTestHistory (..),
    newListVirtualInterfaceTestHistory,

    -- * Request Lenses
    listVirtualInterfaceTestHistory_bgpPeers,
    listVirtualInterfaceTestHistory_maxResults,
    listVirtualInterfaceTestHistory_nextToken,
    listVirtualInterfaceTestHistory_status,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListVirtualInterfaceTestHistory' smart constructor.
data ListVirtualInterfaceTestHistory = ListVirtualInterfaceTestHistory'
  { -- | The BGP peers that were placed in the DOWN state during the virtual
    -- interface failover test.
    bgpPeers :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    --
    -- If @MaxResults@ is given a value larger than 100, only 100 results are
    -- returned.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The status of the virtual interface failover test.
    status :: Prelude.Maybe Prelude.Text,
    -- | The ID of the virtual interface failover test.
    testId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the virtual interface that was tested.
    virtualInterfaceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'maxResults', 'listVirtualInterfaceTestHistory_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- If @MaxResults@ is given a value larger than 100, only 100 results are
-- returned.
--
-- 'nextToken', 'listVirtualInterfaceTestHistory_nextToken' - The token for the next page of results.
--
-- 'status', 'listVirtualInterfaceTestHistory_status' - The status of the virtual interface failover test.
--
-- 'testId', 'listVirtualInterfaceTestHistory_testId' - The ID of the virtual interface failover test.
--
-- 'virtualInterfaceId', 'listVirtualInterfaceTestHistory_virtualInterfaceId' - The ID of the virtual interface that was tested.
newListVirtualInterfaceTestHistory ::
  ListVirtualInterfaceTestHistory
newListVirtualInterfaceTestHistory =
  ListVirtualInterfaceTestHistory'
    { bgpPeers =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      status = Prelude.Nothing,
      testId = Prelude.Nothing,
      virtualInterfaceId = Prelude.Nothing
    }

-- | The BGP peers that were placed in the DOWN state during the virtual
-- interface failover test.
listVirtualInterfaceTestHistory_bgpPeers :: Lens.Lens' ListVirtualInterfaceTestHistory (Prelude.Maybe [Prelude.Text])
listVirtualInterfaceTestHistory_bgpPeers = Lens.lens (\ListVirtualInterfaceTestHistory' {bgpPeers} -> bgpPeers) (\s@ListVirtualInterfaceTestHistory' {} a -> s {bgpPeers = a} :: ListVirtualInterfaceTestHistory) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- If @MaxResults@ is given a value larger than 100, only 100 results are
-- returned.
listVirtualInterfaceTestHistory_maxResults :: Lens.Lens' ListVirtualInterfaceTestHistory (Prelude.Maybe Prelude.Int)
listVirtualInterfaceTestHistory_maxResults = Lens.lens (\ListVirtualInterfaceTestHistory' {maxResults} -> maxResults) (\s@ListVirtualInterfaceTestHistory' {} a -> s {maxResults = a} :: ListVirtualInterfaceTestHistory)

-- | The token for the next page of results.
listVirtualInterfaceTestHistory_nextToken :: Lens.Lens' ListVirtualInterfaceTestHistory (Prelude.Maybe Prelude.Text)
listVirtualInterfaceTestHistory_nextToken = Lens.lens (\ListVirtualInterfaceTestHistory' {nextToken} -> nextToken) (\s@ListVirtualInterfaceTestHistory' {} a -> s {nextToken = a} :: ListVirtualInterfaceTestHistory)

-- | The status of the virtual interface failover test.
listVirtualInterfaceTestHistory_status :: Lens.Lens' ListVirtualInterfaceTestHistory (Prelude.Maybe Prelude.Text)
listVirtualInterfaceTestHistory_status = Lens.lens (\ListVirtualInterfaceTestHistory' {status} -> status) (\s@ListVirtualInterfaceTestHistory' {} a -> s {status = a} :: ListVirtualInterfaceTestHistory)

-- | The ID of the virtual interface failover test.
listVirtualInterfaceTestHistory_testId :: Lens.Lens' ListVirtualInterfaceTestHistory (Prelude.Maybe Prelude.Text)
listVirtualInterfaceTestHistory_testId = Lens.lens (\ListVirtualInterfaceTestHistory' {testId} -> testId) (\s@ListVirtualInterfaceTestHistory' {} a -> s {testId = a} :: ListVirtualInterfaceTestHistory)

-- | The ID of the virtual interface that was tested.
listVirtualInterfaceTestHistory_virtualInterfaceId :: Lens.Lens' ListVirtualInterfaceTestHistory (Prelude.Maybe Prelude.Text)
listVirtualInterfaceTestHistory_virtualInterfaceId = Lens.lens (\ListVirtualInterfaceTestHistory' {virtualInterfaceId} -> virtualInterfaceId) (\s@ListVirtualInterfaceTestHistory' {} a -> s {virtualInterfaceId = a} :: ListVirtualInterfaceTestHistory)

instance
  Core.AWSRequest
    ListVirtualInterfaceTestHistory
  where
  type
    AWSResponse ListVirtualInterfaceTestHistory =
      ListVirtualInterfaceTestHistoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVirtualInterfaceTestHistoryResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x Data..?> "virtualInterfaceTestHistory"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListVirtualInterfaceTestHistory
  where
  hashWithSalt
    _salt
    ListVirtualInterfaceTestHistory' {..} =
      _salt `Prelude.hashWithSalt` bgpPeers
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` testId
        `Prelude.hashWithSalt` virtualInterfaceId

instance
  Prelude.NFData
    ListVirtualInterfaceTestHistory
  where
  rnf ListVirtualInterfaceTestHistory' {..} =
    Prelude.rnf bgpPeers
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf testId
      `Prelude.seq` Prelude.rnf virtualInterfaceId

instance
  Data.ToHeaders
    ListVirtualInterfaceTestHistory
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OvertureService.ListVirtualInterfaceTestHistory" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListVirtualInterfaceTestHistory where
  toJSON ListVirtualInterfaceTestHistory' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bgpPeers" Data..=) Prelude.<$> bgpPeers,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("status" Data..=) Prelude.<$> status,
            ("testId" Data..=) Prelude.<$> testId,
            ("virtualInterfaceId" Data..=)
              Prelude.<$> virtualInterfaceId
          ]
      )

instance Data.ToPath ListVirtualInterfaceTestHistory where
  toPath = Prelude.const "/"

instance Data.ToQuery ListVirtualInterfaceTestHistory where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListVirtualInterfaceTestHistoryResponse' smart constructor.
data ListVirtualInterfaceTestHistoryResponse = ListVirtualInterfaceTestHistoryResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the tested virtual interface.
    virtualInterfaceTestHistory :: Prelude.Maybe [VirtualInterfaceTestHistory],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListVirtualInterfaceTestHistoryResponse
newListVirtualInterfaceTestHistoryResponse
  pHttpStatus_ =
    ListVirtualInterfaceTestHistoryResponse'
      { nextToken =
          Prelude.Nothing,
        virtualInterfaceTestHistory =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
listVirtualInterfaceTestHistoryResponse_nextToken :: Lens.Lens' ListVirtualInterfaceTestHistoryResponse (Prelude.Maybe Prelude.Text)
listVirtualInterfaceTestHistoryResponse_nextToken = Lens.lens (\ListVirtualInterfaceTestHistoryResponse' {nextToken} -> nextToken) (\s@ListVirtualInterfaceTestHistoryResponse' {} a -> s {nextToken = a} :: ListVirtualInterfaceTestHistoryResponse)

-- | The ID of the tested virtual interface.
listVirtualInterfaceTestHistoryResponse_virtualInterfaceTestHistory :: Lens.Lens' ListVirtualInterfaceTestHistoryResponse (Prelude.Maybe [VirtualInterfaceTestHistory])
listVirtualInterfaceTestHistoryResponse_virtualInterfaceTestHistory = Lens.lens (\ListVirtualInterfaceTestHistoryResponse' {virtualInterfaceTestHistory} -> virtualInterfaceTestHistory) (\s@ListVirtualInterfaceTestHistoryResponse' {} a -> s {virtualInterfaceTestHistory = a} :: ListVirtualInterfaceTestHistoryResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listVirtualInterfaceTestHistoryResponse_httpStatus :: Lens.Lens' ListVirtualInterfaceTestHistoryResponse Prelude.Int
listVirtualInterfaceTestHistoryResponse_httpStatus = Lens.lens (\ListVirtualInterfaceTestHistoryResponse' {httpStatus} -> httpStatus) (\s@ListVirtualInterfaceTestHistoryResponse' {} a -> s {httpStatus = a} :: ListVirtualInterfaceTestHistoryResponse)

instance
  Prelude.NFData
    ListVirtualInterfaceTestHistoryResponse
  where
  rnf ListVirtualInterfaceTestHistoryResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf virtualInterfaceTestHistory
      `Prelude.seq` Prelude.rnf httpStatus

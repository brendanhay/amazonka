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
-- Module      : Amazonka.SMS.GetConnectors
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the connectors registered with the Server Migration Service.
--
-- This operation returns paginated results.
module Amazonka.SMS.GetConnectors
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SMS.Types

-- | /See:/ 'newGetConnectors' smart constructor.
data GetConnectors = GetConnectors'
  { -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in a single call. The default
    -- value is 50. To retrieve the remaining results, make another call with
    -- the returned @NextToken@ value.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token for the next set of results.
getConnectors_nextToken :: Lens.Lens' GetConnectors (Prelude.Maybe Prelude.Text)
getConnectors_nextToken = Lens.lens (\GetConnectors' {nextToken} -> nextToken) (\s@GetConnectors' {} a -> s {nextToken = a} :: GetConnectors)

-- | The maximum number of results to return in a single call. The default
-- value is 50. To retrieve the remaining results, make another call with
-- the returned @NextToken@ value.
getConnectors_maxResults :: Lens.Lens' GetConnectors (Prelude.Maybe Prelude.Int)
getConnectors_maxResults = Lens.lens (\GetConnectors' {maxResults} -> maxResults) (\s@GetConnectors' {} a -> s {maxResults = a} :: GetConnectors)

instance Core.AWSPager GetConnectors where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getConnectorsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getConnectorsResponse_connectorList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getConnectors_nextToken
          Lens..~ rs
          Lens.^? getConnectorsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest GetConnectors where
  type
    AWSResponse GetConnectors =
      GetConnectorsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConnectorsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "connectorList" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetConnectors where
  hashWithSalt _salt GetConnectors' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData GetConnectors where
  rnf GetConnectors' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders GetConnectors where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSServerMigrationService_V2016_10_24.GetConnectors" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetConnectors where
  toJSON GetConnectors' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath GetConnectors where
  toPath = Prelude.const "/"

instance Core.ToQuery GetConnectors where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetConnectorsResponse' smart constructor.
data GetConnectorsResponse = GetConnectorsResponse'
  { -- | The token required to retrieve the next set of results. This value is
    -- null when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the registered connectors.
    connectorList :: Prelude.Maybe [Connector],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetConnectorsResponse
newGetConnectorsResponse pHttpStatus_ =
  GetConnectorsResponse'
    { nextToken = Prelude.Nothing,
      connectorList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token required to retrieve the next set of results. This value is
-- null when there are no more results to return.
getConnectorsResponse_nextToken :: Lens.Lens' GetConnectorsResponse (Prelude.Maybe Prelude.Text)
getConnectorsResponse_nextToken = Lens.lens (\GetConnectorsResponse' {nextToken} -> nextToken) (\s@GetConnectorsResponse' {} a -> s {nextToken = a} :: GetConnectorsResponse)

-- | Information about the registered connectors.
getConnectorsResponse_connectorList :: Lens.Lens' GetConnectorsResponse (Prelude.Maybe [Connector])
getConnectorsResponse_connectorList = Lens.lens (\GetConnectorsResponse' {connectorList} -> connectorList) (\s@GetConnectorsResponse' {} a -> s {connectorList = a} :: GetConnectorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getConnectorsResponse_httpStatus :: Lens.Lens' GetConnectorsResponse Prelude.Int
getConnectorsResponse_httpStatus = Lens.lens (\GetConnectorsResponse' {httpStatus} -> httpStatus) (\s@GetConnectorsResponse' {} a -> s {httpStatus = a} :: GetConnectorsResponse)

instance Prelude.NFData GetConnectorsResponse where
  rnf GetConnectorsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf connectorList
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Amazonka.AlexaBusiness.ListGateways
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of gateway summaries. Use GetGateway to retrieve
-- details of a specific gateway. An optional gateway group ARN can be
-- provided to only retrieve gateway summaries of gateways that are
-- associated with that gateway group ARN.
module Amazonka.AlexaBusiness.ListGateways
  ( -- * Creating a Request
    ListGateways (..),
    newListGateways,

    -- * Request Lenses
    listGateways_nextToken,
    listGateways_gatewayGroupArn,
    listGateways_maxResults,

    -- * Destructuring the Response
    ListGatewaysResponse (..),
    newListGatewaysResponse,

    -- * Response Lenses
    listGatewaysResponse_nextToken,
    listGatewaysResponse_gateways,
    listGatewaysResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListGateways' smart constructor.
data ListGateways = ListGateways'
  { -- | The token used to paginate though multiple pages of gateway summaries.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The gateway group ARN for which to list gateways.
    gatewayGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of gateway summaries to return. The default is 50.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGateways' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listGateways_nextToken' - The token used to paginate though multiple pages of gateway summaries.
--
-- 'gatewayGroupArn', 'listGateways_gatewayGroupArn' - The gateway group ARN for which to list gateways.
--
-- 'maxResults', 'listGateways_maxResults' - The maximum number of gateway summaries to return. The default is 50.
newListGateways ::
  ListGateways
newListGateways =
  ListGateways'
    { nextToken = Prelude.Nothing,
      gatewayGroupArn = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token used to paginate though multiple pages of gateway summaries.
listGateways_nextToken :: Lens.Lens' ListGateways (Prelude.Maybe Prelude.Text)
listGateways_nextToken = Lens.lens (\ListGateways' {nextToken} -> nextToken) (\s@ListGateways' {} a -> s {nextToken = a} :: ListGateways)

-- | The gateway group ARN for which to list gateways.
listGateways_gatewayGroupArn :: Lens.Lens' ListGateways (Prelude.Maybe Prelude.Text)
listGateways_gatewayGroupArn = Lens.lens (\ListGateways' {gatewayGroupArn} -> gatewayGroupArn) (\s@ListGateways' {} a -> s {gatewayGroupArn = a} :: ListGateways)

-- | The maximum number of gateway summaries to return. The default is 50.
listGateways_maxResults :: Lens.Lens' ListGateways (Prelude.Maybe Prelude.Natural)
listGateways_maxResults = Lens.lens (\ListGateways' {maxResults} -> maxResults) (\s@ListGateways' {} a -> s {maxResults = a} :: ListGateways)

instance Core.AWSRequest ListGateways where
  type AWSResponse ListGateways = ListGatewaysResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGatewaysResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Gateways" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListGateways where
  hashWithSalt _salt ListGateways' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` gatewayGroupArn
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListGateways where
  rnf ListGateways' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf gatewayGroupArn
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListGateways where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.ListGateways" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListGateways where
  toJSON ListGateways' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("GatewayGroupArn" Data..=)
              Prelude.<$> gatewayGroupArn,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListGateways where
  toPath = Prelude.const "/"

instance Data.ToQuery ListGateways where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListGatewaysResponse' smart constructor.
data ListGatewaysResponse = ListGatewaysResponse'
  { -- | The token used to paginate though multiple pages of gateway summaries.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The gateways in the list.
    gateways :: Prelude.Maybe [GatewaySummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGatewaysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listGatewaysResponse_nextToken' - The token used to paginate though multiple pages of gateway summaries.
--
-- 'gateways', 'listGatewaysResponse_gateways' - The gateways in the list.
--
-- 'httpStatus', 'listGatewaysResponse_httpStatus' - The response's http status code.
newListGatewaysResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListGatewaysResponse
newListGatewaysResponse pHttpStatus_ =
  ListGatewaysResponse'
    { nextToken = Prelude.Nothing,
      gateways = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token used to paginate though multiple pages of gateway summaries.
listGatewaysResponse_nextToken :: Lens.Lens' ListGatewaysResponse (Prelude.Maybe Prelude.Text)
listGatewaysResponse_nextToken = Lens.lens (\ListGatewaysResponse' {nextToken} -> nextToken) (\s@ListGatewaysResponse' {} a -> s {nextToken = a} :: ListGatewaysResponse)

-- | The gateways in the list.
listGatewaysResponse_gateways :: Lens.Lens' ListGatewaysResponse (Prelude.Maybe [GatewaySummary])
listGatewaysResponse_gateways = Lens.lens (\ListGatewaysResponse' {gateways} -> gateways) (\s@ListGatewaysResponse' {} a -> s {gateways = a} :: ListGatewaysResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listGatewaysResponse_httpStatus :: Lens.Lens' ListGatewaysResponse Prelude.Int
listGatewaysResponse_httpStatus = Lens.lens (\ListGatewaysResponse' {httpStatus} -> httpStatus) (\s@ListGatewaysResponse' {} a -> s {httpStatus = a} :: ListGatewaysResponse)

instance Prelude.NFData ListGatewaysResponse where
  rnf ListGatewaysResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf gateways
      `Prelude.seq` Prelude.rnf httpStatus

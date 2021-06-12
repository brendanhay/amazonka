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
-- Module      : Network.AWS.AlexaBusiness.ListGateways
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of gateway summaries. Use GetGateway to retrieve
-- details of a specific gateway. An optional gateway group ARN can be
-- provided to only retrieve gateway summaries of gateways that are
-- associated with that gateway group ARN.
module Network.AWS.AlexaBusiness.ListGateways
  ( -- * Creating a Request
    ListGateways (..),
    newListGateways,

    -- * Request Lenses
    listGateways_nextToken,
    listGateways_maxResults,
    listGateways_gatewayGroupArn,

    -- * Destructuring the Response
    ListGatewaysResponse (..),
    newListGatewaysResponse,

    -- * Response Lenses
    listGatewaysResponse_nextToken,
    listGatewaysResponse_gateways,
    listGatewaysResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListGateways' smart constructor.
data ListGateways = ListGateways'
  { -- | The token used to paginate though multiple pages of gateway summaries.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of gateway summaries to return. The default is 50.
    maxResults :: Core.Maybe Core.Natural,
    -- | The gateway group ARN for which to list gateways.
    gatewayGroupArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'maxResults', 'listGateways_maxResults' - The maximum number of gateway summaries to return. The default is 50.
--
-- 'gatewayGroupArn', 'listGateways_gatewayGroupArn' - The gateway group ARN for which to list gateways.
newListGateways ::
  ListGateways
newListGateways =
  ListGateways'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      gatewayGroupArn = Core.Nothing
    }

-- | The token used to paginate though multiple pages of gateway summaries.
listGateways_nextToken :: Lens.Lens' ListGateways (Core.Maybe Core.Text)
listGateways_nextToken = Lens.lens (\ListGateways' {nextToken} -> nextToken) (\s@ListGateways' {} a -> s {nextToken = a} :: ListGateways)

-- | The maximum number of gateway summaries to return. The default is 50.
listGateways_maxResults :: Lens.Lens' ListGateways (Core.Maybe Core.Natural)
listGateways_maxResults = Lens.lens (\ListGateways' {maxResults} -> maxResults) (\s@ListGateways' {} a -> s {maxResults = a} :: ListGateways)

-- | The gateway group ARN for which to list gateways.
listGateways_gatewayGroupArn :: Lens.Lens' ListGateways (Core.Maybe Core.Text)
listGateways_gatewayGroupArn = Lens.lens (\ListGateways' {gatewayGroupArn} -> gatewayGroupArn) (\s@ListGateways' {} a -> s {gatewayGroupArn = a} :: ListGateways)

instance Core.AWSRequest ListGateways where
  type AWSResponse ListGateways = ListGatewaysResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGatewaysResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Gateways" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListGateways

instance Core.NFData ListGateways

instance Core.ToHeaders ListGateways where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AlexaForBusiness.ListGateways" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListGateways where
  toJSON ListGateways' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("GatewayGroupArn" Core..=)
              Core.<$> gatewayGroupArn
          ]
      )

instance Core.ToPath ListGateways where
  toPath = Core.const "/"

instance Core.ToQuery ListGateways where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListGatewaysResponse' smart constructor.
data ListGatewaysResponse = ListGatewaysResponse'
  { -- | The token used to paginate though multiple pages of gateway summaries.
    nextToken :: Core.Maybe Core.Text,
    -- | The gateways in the list.
    gateways :: Core.Maybe [GatewaySummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListGatewaysResponse
newListGatewaysResponse pHttpStatus_ =
  ListGatewaysResponse'
    { nextToken = Core.Nothing,
      gateways = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token used to paginate though multiple pages of gateway summaries.
listGatewaysResponse_nextToken :: Lens.Lens' ListGatewaysResponse (Core.Maybe Core.Text)
listGatewaysResponse_nextToken = Lens.lens (\ListGatewaysResponse' {nextToken} -> nextToken) (\s@ListGatewaysResponse' {} a -> s {nextToken = a} :: ListGatewaysResponse)

-- | The gateways in the list.
listGatewaysResponse_gateways :: Lens.Lens' ListGatewaysResponse (Core.Maybe [GatewaySummary])
listGatewaysResponse_gateways = Lens.lens (\ListGatewaysResponse' {gateways} -> gateways) (\s@ListGatewaysResponse' {} a -> s {gateways = a} :: ListGatewaysResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listGatewaysResponse_httpStatus :: Lens.Lens' ListGatewaysResponse Core.Int
listGatewaysResponse_httpStatus = Lens.lens (\ListGatewaysResponse' {httpStatus} -> httpStatus) (\s@ListGatewaysResponse' {} a -> s {httpStatus = a} :: ListGatewaysResponse)

instance Core.NFData ListGatewaysResponse

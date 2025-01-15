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
-- Module      : Amazonka.Route53RecoveryCluster.ListRoutingControls
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List routing control names and Amazon Resource Names (ARNs), as well as
-- the routing control state for each routing control, along with the
-- control panel name and control panel ARN for the routing controls. If
-- you specify a control panel ARN, this call lists the routing controls in
-- the control panel. Otherwise, it lists all the routing controls in the
-- cluster.
--
-- A routing control is a simple on\/off switch in Route 53 ARC that you
-- can use to route traffic to cells. When a routing control state is On,
-- traffic flows to a cell. When the state is Off, traffic does not flow.
--
-- Before you can create a routing control, you must first create a
-- cluster, and then host the control in a control panel on the cluster.
-- For more information, see
-- <https://docs.aws.amazon.com/r53recovery/latest/dg/routing-control.create.html Create routing control structures>
-- in the Amazon Route 53 Application Recovery Controller Developer Guide.
-- You access one of the endpoints for the cluster to get or update the
-- routing control state to redirect traffic for your application.
--
-- /You must specify Regional endpoints when you work with API cluster
-- operations to use this API operation to list routing controls in Route
-- 53 ARC./
--
-- Learn more about working with routing controls in the following topics
-- in the Amazon Route 53 Application Recovery Controller Developer Guide:
--
-- -   <https://docs.aws.amazon.com/r53recovery/latest/dg/routing-control.update.html Viewing and updating routing control states>
--
-- -   <https://docs.aws.amazon.com/r53recovery/latest/dg/routing-control.html Working with routing controls in Route 53 ARC>
--
-- This operation returns paginated results.
module Amazonka.Route53RecoveryCluster.ListRoutingControls
  ( -- * Creating a Request
    ListRoutingControls (..),
    newListRoutingControls,

    -- * Request Lenses
    listRoutingControls_controlPanelArn,
    listRoutingControls_maxResults,
    listRoutingControls_nextToken,

    -- * Destructuring the Response
    ListRoutingControlsResponse (..),
    newListRoutingControlsResponse,

    -- * Response Lenses
    listRoutingControlsResponse_nextToken,
    listRoutingControlsResponse_httpStatus,
    listRoutingControlsResponse_routingControls,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryCluster.Types

-- | /See:/ 'newListRoutingControls' smart constructor.
data ListRoutingControls = ListRoutingControls'
  { -- | The Amazon Resource Name (ARN) of the control panel of the routing
    -- controls to list.
    controlPanelArn :: Prelude.Maybe Prelude.Text,
    -- | The number of routing controls objects that you want to return with this
    -- call. The default value is 500.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. You receive this token from a
    -- previous call.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRoutingControls' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'controlPanelArn', 'listRoutingControls_controlPanelArn' - The Amazon Resource Name (ARN) of the control panel of the routing
-- controls to list.
--
-- 'maxResults', 'listRoutingControls_maxResults' - The number of routing controls objects that you want to return with this
-- call. The default value is 500.
--
-- 'nextToken', 'listRoutingControls_nextToken' - The token for the next set of results. You receive this token from a
-- previous call.
newListRoutingControls ::
  ListRoutingControls
newListRoutingControls =
  ListRoutingControls'
    { controlPanelArn =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the control panel of the routing
-- controls to list.
listRoutingControls_controlPanelArn :: Lens.Lens' ListRoutingControls (Prelude.Maybe Prelude.Text)
listRoutingControls_controlPanelArn = Lens.lens (\ListRoutingControls' {controlPanelArn} -> controlPanelArn) (\s@ListRoutingControls' {} a -> s {controlPanelArn = a} :: ListRoutingControls)

-- | The number of routing controls objects that you want to return with this
-- call. The default value is 500.
listRoutingControls_maxResults :: Lens.Lens' ListRoutingControls (Prelude.Maybe Prelude.Natural)
listRoutingControls_maxResults = Lens.lens (\ListRoutingControls' {maxResults} -> maxResults) (\s@ListRoutingControls' {} a -> s {maxResults = a} :: ListRoutingControls)

-- | The token for the next set of results. You receive this token from a
-- previous call.
listRoutingControls_nextToken :: Lens.Lens' ListRoutingControls (Prelude.Maybe Prelude.Text)
listRoutingControls_nextToken = Lens.lens (\ListRoutingControls' {nextToken} -> nextToken) (\s@ListRoutingControls' {} a -> s {nextToken = a} :: ListRoutingControls)

instance Core.AWSPager ListRoutingControls where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRoutingControlsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listRoutingControlsResponse_routingControls
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listRoutingControls_nextToken
              Lens..~ rs
              Lens.^? listRoutingControlsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListRoutingControls where
  type
    AWSResponse ListRoutingControls =
      ListRoutingControlsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRoutingControlsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "RoutingControls"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListRoutingControls where
  hashWithSalt _salt ListRoutingControls' {..} =
    _salt
      `Prelude.hashWithSalt` controlPanelArn
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListRoutingControls where
  rnf ListRoutingControls' {..} =
    Prelude.rnf controlPanelArn `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken

instance Data.ToHeaders ListRoutingControls where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ToggleCustomerAPI.ListRoutingControls" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListRoutingControls where
  toJSON ListRoutingControls' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ControlPanelArn" Data..=)
              Prelude.<$> controlPanelArn,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListRoutingControls where
  toPath = Prelude.const "/"

instance Data.ToQuery ListRoutingControls where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRoutingControlsResponse' smart constructor.
data ListRoutingControlsResponse = ListRoutingControlsResponse'
  { -- | The token for the next set of results. You receive this token from a
    -- previous call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of routing controls.
    routingControls :: [RoutingControl]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRoutingControlsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRoutingControlsResponse_nextToken' - The token for the next set of results. You receive this token from a
-- previous call.
--
-- 'httpStatus', 'listRoutingControlsResponse_httpStatus' - The response's http status code.
--
-- 'routingControls', 'listRoutingControlsResponse_routingControls' - The list of routing controls.
newListRoutingControlsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRoutingControlsResponse
newListRoutingControlsResponse pHttpStatus_ =
  ListRoutingControlsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      routingControls = Prelude.mempty
    }

-- | The token for the next set of results. You receive this token from a
-- previous call.
listRoutingControlsResponse_nextToken :: Lens.Lens' ListRoutingControlsResponse (Prelude.Maybe Prelude.Text)
listRoutingControlsResponse_nextToken = Lens.lens (\ListRoutingControlsResponse' {nextToken} -> nextToken) (\s@ListRoutingControlsResponse' {} a -> s {nextToken = a} :: ListRoutingControlsResponse)

-- | The response's http status code.
listRoutingControlsResponse_httpStatus :: Lens.Lens' ListRoutingControlsResponse Prelude.Int
listRoutingControlsResponse_httpStatus = Lens.lens (\ListRoutingControlsResponse' {httpStatus} -> httpStatus) (\s@ListRoutingControlsResponse' {} a -> s {httpStatus = a} :: ListRoutingControlsResponse)

-- | The list of routing controls.
listRoutingControlsResponse_routingControls :: Lens.Lens' ListRoutingControlsResponse [RoutingControl]
listRoutingControlsResponse_routingControls = Lens.lens (\ListRoutingControlsResponse' {routingControls} -> routingControls) (\s@ListRoutingControlsResponse' {} a -> s {routingControls = a} :: ListRoutingControlsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListRoutingControlsResponse where
  rnf ListRoutingControlsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf routingControls

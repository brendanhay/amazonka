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
-- Module      : Amazonka.Route53RecoveryControlConfig.ListRoutingControls
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of routing controls for a control panel. A routing
-- control is an Amazon Route 53 Application Recovery Controller construct
-- that has one of two states: ON and OFF. You can map the routing control
-- state to the state of an Amazon Route 53 health check, which can be used
-- to control routing.
--
-- This operation returns paginated results.
module Amazonka.Route53RecoveryControlConfig.ListRoutingControls
  ( -- * Creating a Request
    ListRoutingControls (..),
    newListRoutingControls,

    -- * Request Lenses
    listRoutingControls_maxResults,
    listRoutingControls_nextToken,
    listRoutingControls_controlPanelArn,

    -- * Destructuring the Response
    ListRoutingControlsResponse (..),
    newListRoutingControlsResponse,

    -- * Response Lenses
    listRoutingControlsResponse_nextToken,
    listRoutingControlsResponse_routingControls,
    listRoutingControlsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryControlConfig.Types

-- | /See:/ 'newListRoutingControls' smart constructor.
data ListRoutingControls = ListRoutingControls'
  { -- | The number of objects that you want to return with this call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token that identifies which batch of results you want to see.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the control panel.
    controlPanelArn :: Prelude.Text
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
-- 'maxResults', 'listRoutingControls_maxResults' - The number of objects that you want to return with this call.
--
-- 'nextToken', 'listRoutingControls_nextToken' - The token that identifies which batch of results you want to see.
--
-- 'controlPanelArn', 'listRoutingControls_controlPanelArn' - The Amazon Resource Name (ARN) of the control panel.
newListRoutingControls ::
  -- | 'controlPanelArn'
  Prelude.Text ->
  ListRoutingControls
newListRoutingControls pControlPanelArn_ =
  ListRoutingControls'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      controlPanelArn = pControlPanelArn_
    }

-- | The number of objects that you want to return with this call.
listRoutingControls_maxResults :: Lens.Lens' ListRoutingControls (Prelude.Maybe Prelude.Natural)
listRoutingControls_maxResults = Lens.lens (\ListRoutingControls' {maxResults} -> maxResults) (\s@ListRoutingControls' {} a -> s {maxResults = a} :: ListRoutingControls)

-- | The token that identifies which batch of results you want to see.
listRoutingControls_nextToken :: Lens.Lens' ListRoutingControls (Prelude.Maybe Prelude.Text)
listRoutingControls_nextToken = Lens.lens (\ListRoutingControls' {nextToken} -> nextToken) (\s@ListRoutingControls' {} a -> s {nextToken = a} :: ListRoutingControls)

-- | The Amazon Resource Name (ARN) of the control panel.
listRoutingControls_controlPanelArn :: Lens.Lens' ListRoutingControls Prelude.Text
listRoutingControls_controlPanelArn = Lens.lens (\ListRoutingControls' {controlPanelArn} -> controlPanelArn) (\s@ListRoutingControls' {} a -> s {controlPanelArn = a} :: ListRoutingControls)

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
            Lens.^? listRoutingControlsResponse_routingControls
              Prelude.. Lens._Just
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
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRoutingControlsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "RoutingControls"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRoutingControls where
  hashWithSalt _salt ListRoutingControls' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` controlPanelArn

instance Prelude.NFData ListRoutingControls where
  rnf ListRoutingControls' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf controlPanelArn

instance Data.ToHeaders ListRoutingControls where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListRoutingControls where
  toPath ListRoutingControls' {..} =
    Prelude.mconcat
      [ "/controlpanel/",
        Data.toBS controlPanelArn,
        "/routingcontrols"
      ]

instance Data.ToQuery ListRoutingControls where
  toQuery ListRoutingControls' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListRoutingControlsResponse' smart constructor.
data ListRoutingControlsResponse = ListRoutingControlsResponse'
  { -- | The token that identifies which batch of results you want to see.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of routing controls.
    routingControls :: Prelude.Maybe [RoutingControl],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'nextToken', 'listRoutingControlsResponse_nextToken' - The token that identifies which batch of results you want to see.
--
-- 'routingControls', 'listRoutingControlsResponse_routingControls' - An array of routing controls.
--
-- 'httpStatus', 'listRoutingControlsResponse_httpStatus' - The response's http status code.
newListRoutingControlsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRoutingControlsResponse
newListRoutingControlsResponse pHttpStatus_ =
  ListRoutingControlsResponse'
    { nextToken =
        Prelude.Nothing,
      routingControls = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token that identifies which batch of results you want to see.
listRoutingControlsResponse_nextToken :: Lens.Lens' ListRoutingControlsResponse (Prelude.Maybe Prelude.Text)
listRoutingControlsResponse_nextToken = Lens.lens (\ListRoutingControlsResponse' {nextToken} -> nextToken) (\s@ListRoutingControlsResponse' {} a -> s {nextToken = a} :: ListRoutingControlsResponse)

-- | An array of routing controls.
listRoutingControlsResponse_routingControls :: Lens.Lens' ListRoutingControlsResponse (Prelude.Maybe [RoutingControl])
listRoutingControlsResponse_routingControls = Lens.lens (\ListRoutingControlsResponse' {routingControls} -> routingControls) (\s@ListRoutingControlsResponse' {} a -> s {routingControls = a} :: ListRoutingControlsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRoutingControlsResponse_httpStatus :: Lens.Lens' ListRoutingControlsResponse Prelude.Int
listRoutingControlsResponse_httpStatus = Lens.lens (\ListRoutingControlsResponse' {httpStatus} -> httpStatus) (\s@ListRoutingControlsResponse' {} a -> s {httpStatus = a} :: ListRoutingControlsResponse)

instance Prelude.NFData ListRoutingControlsResponse where
  rnf ListRoutingControlsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf routingControls
      `Prelude.seq` Prelude.rnf httpStatus

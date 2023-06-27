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
-- Module      : Amazonka.ELBV2.DescribeListeners
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified listeners or the listeners for the specified
-- Application Load Balancer, Network Load Balancer, or Gateway Load
-- Balancer. You must specify either a load balancer or one or more
-- listeners.
--
-- This operation returns paginated results.
module Amazonka.ELBV2.DescribeListeners
  ( -- * Creating a Request
    DescribeListeners (..),
    newDescribeListeners,

    -- * Request Lenses
    describeListeners_listenerArns,
    describeListeners_loadBalancerArn,
    describeListeners_marker,
    describeListeners_pageSize,

    -- * Destructuring the Response
    DescribeListenersResponse (..),
    newDescribeListenersResponse,

    -- * Response Lenses
    describeListenersResponse_listeners,
    describeListenersResponse_nextMarker,
    describeListenersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELBV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeListeners' smart constructor.
data DescribeListeners = DescribeListeners'
  { -- | The Amazon Resource Names (ARN) of the listeners.
    listenerArns :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of the load balancer.
    loadBalancerArn :: Prelude.Maybe Prelude.Text,
    -- | The marker for the next set of results. (You received this marker from a
    -- previous call.)
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return with this call.
    pageSize :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeListeners' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'listenerArns', 'describeListeners_listenerArns' - The Amazon Resource Names (ARN) of the listeners.
--
-- 'loadBalancerArn', 'describeListeners_loadBalancerArn' - The Amazon Resource Name (ARN) of the load balancer.
--
-- 'marker', 'describeListeners_marker' - The marker for the next set of results. (You received this marker from a
-- previous call.)
--
-- 'pageSize', 'describeListeners_pageSize' - The maximum number of results to return with this call.
newDescribeListeners ::
  DescribeListeners
newDescribeListeners =
  DescribeListeners'
    { listenerArns = Prelude.Nothing,
      loadBalancerArn = Prelude.Nothing,
      marker = Prelude.Nothing,
      pageSize = Prelude.Nothing
    }

-- | The Amazon Resource Names (ARN) of the listeners.
describeListeners_listenerArns :: Lens.Lens' DescribeListeners (Prelude.Maybe [Prelude.Text])
describeListeners_listenerArns = Lens.lens (\DescribeListeners' {listenerArns} -> listenerArns) (\s@DescribeListeners' {} a -> s {listenerArns = a} :: DescribeListeners) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the load balancer.
describeListeners_loadBalancerArn :: Lens.Lens' DescribeListeners (Prelude.Maybe Prelude.Text)
describeListeners_loadBalancerArn = Lens.lens (\DescribeListeners' {loadBalancerArn} -> loadBalancerArn) (\s@DescribeListeners' {} a -> s {loadBalancerArn = a} :: DescribeListeners)

-- | The marker for the next set of results. (You received this marker from a
-- previous call.)
describeListeners_marker :: Lens.Lens' DescribeListeners (Prelude.Maybe Prelude.Text)
describeListeners_marker = Lens.lens (\DescribeListeners' {marker} -> marker) (\s@DescribeListeners' {} a -> s {marker = a} :: DescribeListeners)

-- | The maximum number of results to return with this call.
describeListeners_pageSize :: Lens.Lens' DescribeListeners (Prelude.Maybe Prelude.Natural)
describeListeners_pageSize = Lens.lens (\DescribeListeners' {pageSize} -> pageSize) (\s@DescribeListeners' {} a -> s {pageSize = a} :: DescribeListeners)

instance Core.AWSPager DescribeListeners where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeListenersResponse_nextMarker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeListenersResponse_listeners
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeListeners_marker
          Lens..~ rs
          Lens.^? describeListenersResponse_nextMarker
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeListeners where
  type
    AWSResponse DescribeListeners =
      DescribeListenersResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeListenersResult"
      ( \s h x ->
          DescribeListenersResponse'
            Prelude.<$> ( x
                            Data..@? "Listeners"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (x Data..@? "NextMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeListeners where
  hashWithSalt _salt DescribeListeners' {..} =
    _salt
      `Prelude.hashWithSalt` listenerArns
      `Prelude.hashWithSalt` loadBalancerArn
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` pageSize

instance Prelude.NFData DescribeListeners where
  rnf DescribeListeners' {..} =
    Prelude.rnf listenerArns
      `Prelude.seq` Prelude.rnf loadBalancerArn
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf pageSize

instance Data.ToHeaders DescribeListeners where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeListeners where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeListeners where
  toQuery DescribeListeners' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeListeners" :: Prelude.ByteString),
        "Version"
          Data.=: ("2015-12-01" :: Prelude.ByteString),
        "ListenerArns"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> listenerArns),
        "LoadBalancerArn" Data.=: loadBalancerArn,
        "Marker" Data.=: marker,
        "PageSize" Data.=: pageSize
      ]

-- | /See:/ 'newDescribeListenersResponse' smart constructor.
data DescribeListenersResponse = DescribeListenersResponse'
  { -- | Information about the listeners.
    listeners :: Prelude.Maybe [Listener],
    -- | If there are additional results, this is the marker for the next set of
    -- results. Otherwise, this is null.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeListenersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'listeners', 'describeListenersResponse_listeners' - Information about the listeners.
--
-- 'nextMarker', 'describeListenersResponse_nextMarker' - If there are additional results, this is the marker for the next set of
-- results. Otherwise, this is null.
--
-- 'httpStatus', 'describeListenersResponse_httpStatus' - The response's http status code.
newDescribeListenersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeListenersResponse
newDescribeListenersResponse pHttpStatus_ =
  DescribeListenersResponse'
    { listeners =
        Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the listeners.
describeListenersResponse_listeners :: Lens.Lens' DescribeListenersResponse (Prelude.Maybe [Listener])
describeListenersResponse_listeners = Lens.lens (\DescribeListenersResponse' {listeners} -> listeners) (\s@DescribeListenersResponse' {} a -> s {listeners = a} :: DescribeListenersResponse) Prelude.. Lens.mapping Lens.coerced

-- | If there are additional results, this is the marker for the next set of
-- results. Otherwise, this is null.
describeListenersResponse_nextMarker :: Lens.Lens' DescribeListenersResponse (Prelude.Maybe Prelude.Text)
describeListenersResponse_nextMarker = Lens.lens (\DescribeListenersResponse' {nextMarker} -> nextMarker) (\s@DescribeListenersResponse' {} a -> s {nextMarker = a} :: DescribeListenersResponse)

-- | The response's http status code.
describeListenersResponse_httpStatus :: Lens.Lens' DescribeListenersResponse Prelude.Int
describeListenersResponse_httpStatus = Lens.lens (\DescribeListenersResponse' {httpStatus} -> httpStatus) (\s@DescribeListenersResponse' {} a -> s {httpStatus = a} :: DescribeListenersResponse)

instance Prelude.NFData DescribeListenersResponse where
  rnf DescribeListenersResponse' {..} =
    Prelude.rnf listeners
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf httpStatus

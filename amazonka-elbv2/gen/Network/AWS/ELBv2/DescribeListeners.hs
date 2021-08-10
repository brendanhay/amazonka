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
-- Module      : Network.AWS.ELBv2.DescribeListeners
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.ELBv2.DescribeListeners
  ( -- * Creating a Request
    DescribeListeners (..),
    newDescribeListeners,

    -- * Request Lenses
    describeListeners_loadBalancerArn,
    describeListeners_pageSize,
    describeListeners_listenerArns,
    describeListeners_marker,

    -- * Destructuring the Response
    DescribeListenersResponse (..),
    newDescribeListenersResponse,

    -- * Response Lenses
    describeListenersResponse_nextMarker,
    describeListenersResponse_listeners,
    describeListenersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeListeners' smart constructor.
data DescribeListeners = DescribeListeners'
  { -- | The Amazon Resource Name (ARN) of the load balancer.
    loadBalancerArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return with this call.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Names (ARN) of the listeners.
    listenerArns :: Prelude.Maybe [Prelude.Text],
    -- | The marker for the next set of results. (You received this marker from a
    -- previous call.)
    marker :: Prelude.Maybe Prelude.Text
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
-- 'loadBalancerArn', 'describeListeners_loadBalancerArn' - The Amazon Resource Name (ARN) of the load balancer.
--
-- 'pageSize', 'describeListeners_pageSize' - The maximum number of results to return with this call.
--
-- 'listenerArns', 'describeListeners_listenerArns' - The Amazon Resource Names (ARN) of the listeners.
--
-- 'marker', 'describeListeners_marker' - The marker for the next set of results. (You received this marker from a
-- previous call.)
newDescribeListeners ::
  DescribeListeners
newDescribeListeners =
  DescribeListeners'
    { loadBalancerArn =
        Prelude.Nothing,
      pageSize = Prelude.Nothing,
      listenerArns = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the load balancer.
describeListeners_loadBalancerArn :: Lens.Lens' DescribeListeners (Prelude.Maybe Prelude.Text)
describeListeners_loadBalancerArn = Lens.lens (\DescribeListeners' {loadBalancerArn} -> loadBalancerArn) (\s@DescribeListeners' {} a -> s {loadBalancerArn = a} :: DescribeListeners)

-- | The maximum number of results to return with this call.
describeListeners_pageSize :: Lens.Lens' DescribeListeners (Prelude.Maybe Prelude.Natural)
describeListeners_pageSize = Lens.lens (\DescribeListeners' {pageSize} -> pageSize) (\s@DescribeListeners' {} a -> s {pageSize = a} :: DescribeListeners)

-- | The Amazon Resource Names (ARN) of the listeners.
describeListeners_listenerArns :: Lens.Lens' DescribeListeners (Prelude.Maybe [Prelude.Text])
describeListeners_listenerArns = Lens.lens (\DescribeListeners' {listenerArns} -> listenerArns) (\s@DescribeListeners' {} a -> s {listenerArns = a} :: DescribeListeners) Prelude.. Lens.mapping Lens._Coerce

-- | The marker for the next set of results. (You received this marker from a
-- previous call.)
describeListeners_marker :: Lens.Lens' DescribeListeners (Prelude.Maybe Prelude.Text)
describeListeners_marker = Lens.lens (\DescribeListeners' {marker} -> marker) (\s@DescribeListeners' {} a -> s {marker = a} :: DescribeListeners)

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
      Prelude.Just Prelude.$
        rq
          Prelude.& describeListeners_marker
          Lens..~ rs
          Lens.^? describeListenersResponse_nextMarker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeListeners where
  type
    AWSResponse DescribeListeners =
      DescribeListenersResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeListenersResult"
      ( \s h x ->
          DescribeListenersResponse'
            Prelude.<$> (x Core..@? "NextMarker")
            Prelude.<*> ( x Core..@? "Listeners" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeListeners

instance Prelude.NFData DescribeListeners

instance Core.ToHeaders DescribeListeners where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeListeners where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeListeners where
  toQuery DescribeListeners' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeListeners" :: Prelude.ByteString),
        "Version"
          Core.=: ("2015-12-01" :: Prelude.ByteString),
        "LoadBalancerArn" Core.=: loadBalancerArn,
        "PageSize" Core.=: pageSize,
        "ListenerArns"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> listenerArns),
        "Marker" Core.=: marker
      ]

-- | /See:/ 'newDescribeListenersResponse' smart constructor.
data DescribeListenersResponse = DescribeListenersResponse'
  { -- | If there are additional results, this is the marker for the next set of
    -- results. Otherwise, this is null.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | Information about the listeners.
    listeners :: Prelude.Maybe [Listener],
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
-- 'nextMarker', 'describeListenersResponse_nextMarker' - If there are additional results, this is the marker for the next set of
-- results. Otherwise, this is null.
--
-- 'listeners', 'describeListenersResponse_listeners' - Information about the listeners.
--
-- 'httpStatus', 'describeListenersResponse_httpStatus' - The response's http status code.
newDescribeListenersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeListenersResponse
newDescribeListenersResponse pHttpStatus_ =
  DescribeListenersResponse'
    { nextMarker =
        Prelude.Nothing,
      listeners = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the marker for the next set of
-- results. Otherwise, this is null.
describeListenersResponse_nextMarker :: Lens.Lens' DescribeListenersResponse (Prelude.Maybe Prelude.Text)
describeListenersResponse_nextMarker = Lens.lens (\DescribeListenersResponse' {nextMarker} -> nextMarker) (\s@DescribeListenersResponse' {} a -> s {nextMarker = a} :: DescribeListenersResponse)

-- | Information about the listeners.
describeListenersResponse_listeners :: Lens.Lens' DescribeListenersResponse (Prelude.Maybe [Listener])
describeListenersResponse_listeners = Lens.lens (\DescribeListenersResponse' {listeners} -> listeners) (\s@DescribeListenersResponse' {} a -> s {listeners = a} :: DescribeListenersResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeListenersResponse_httpStatus :: Lens.Lens' DescribeListenersResponse Prelude.Int
describeListenersResponse_httpStatus = Lens.lens (\DescribeListenersResponse' {httpStatus} -> httpStatus) (\s@DescribeListenersResponse' {} a -> s {httpStatus = a} :: DescribeListenersResponse)

instance Prelude.NFData DescribeListenersResponse

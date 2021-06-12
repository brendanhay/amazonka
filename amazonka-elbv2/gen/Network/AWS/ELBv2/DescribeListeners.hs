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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeListeners' smart constructor.
data DescribeListeners = DescribeListeners'
  { -- | The Amazon Resource Name (ARN) of the load balancer.
    loadBalancerArn :: Core.Maybe Core.Text,
    -- | The maximum number of results to return with this call.
    pageSize :: Core.Maybe Core.Natural,
    -- | The Amazon Resource Names (ARN) of the listeners.
    listenerArns :: Core.Maybe [Core.Text],
    -- | The marker for the next set of results. (You received this marker from a
    -- previous call.)
    marker :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { loadBalancerArn = Core.Nothing,
      pageSize = Core.Nothing,
      listenerArns = Core.Nothing,
      marker = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the load balancer.
describeListeners_loadBalancerArn :: Lens.Lens' DescribeListeners (Core.Maybe Core.Text)
describeListeners_loadBalancerArn = Lens.lens (\DescribeListeners' {loadBalancerArn} -> loadBalancerArn) (\s@DescribeListeners' {} a -> s {loadBalancerArn = a} :: DescribeListeners)

-- | The maximum number of results to return with this call.
describeListeners_pageSize :: Lens.Lens' DescribeListeners (Core.Maybe Core.Natural)
describeListeners_pageSize = Lens.lens (\DescribeListeners' {pageSize} -> pageSize) (\s@DescribeListeners' {} a -> s {pageSize = a} :: DescribeListeners)

-- | The Amazon Resource Names (ARN) of the listeners.
describeListeners_listenerArns :: Lens.Lens' DescribeListeners (Core.Maybe [Core.Text])
describeListeners_listenerArns = Lens.lens (\DescribeListeners' {listenerArns} -> listenerArns) (\s@DescribeListeners' {} a -> s {listenerArns = a} :: DescribeListeners) Core.. Lens.mapping Lens._Coerce

-- | The marker for the next set of results. (You received this marker from a
-- previous call.)
describeListeners_marker :: Lens.Lens' DescribeListeners (Core.Maybe Core.Text)
describeListeners_marker = Lens.lens (\DescribeListeners' {marker} -> marker) (\s@DescribeListeners' {} a -> s {marker = a} :: DescribeListeners)

instance Core.AWSPager DescribeListeners where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeListenersResponse_nextMarker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeListenersResponse_listeners
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeListeners_marker
          Lens..~ rs
          Lens.^? describeListenersResponse_nextMarker
            Core.. Lens._Just

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
            Core.<$> (x Core..@? "NextMarker")
            Core.<*> ( x Core..@? "Listeners" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeListeners

instance Core.NFData DescribeListeners

instance Core.ToHeaders DescribeListeners where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeListeners where
  toPath = Core.const "/"

instance Core.ToQuery DescribeListeners where
  toQuery DescribeListeners' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeListeners" :: Core.ByteString),
        "Version" Core.=: ("2015-12-01" :: Core.ByteString),
        "LoadBalancerArn" Core.=: loadBalancerArn,
        "PageSize" Core.=: pageSize,
        "ListenerArns"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> listenerArns),
        "Marker" Core.=: marker
      ]

-- | /See:/ 'newDescribeListenersResponse' smart constructor.
data DescribeListenersResponse = DescribeListenersResponse'
  { -- | If there are additional results, this is the marker for the next set of
    -- results. Otherwise, this is null.
    nextMarker :: Core.Maybe Core.Text,
    -- | Information about the listeners.
    listeners :: Core.Maybe [Listener],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeListenersResponse
newDescribeListenersResponse pHttpStatus_ =
  DescribeListenersResponse'
    { nextMarker =
        Core.Nothing,
      listeners = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the marker for the next set of
-- results. Otherwise, this is null.
describeListenersResponse_nextMarker :: Lens.Lens' DescribeListenersResponse (Core.Maybe Core.Text)
describeListenersResponse_nextMarker = Lens.lens (\DescribeListenersResponse' {nextMarker} -> nextMarker) (\s@DescribeListenersResponse' {} a -> s {nextMarker = a} :: DescribeListenersResponse)

-- | Information about the listeners.
describeListenersResponse_listeners :: Lens.Lens' DescribeListenersResponse (Core.Maybe [Listener])
describeListenersResponse_listeners = Lens.lens (\DescribeListenersResponse' {listeners} -> listeners) (\s@DescribeListenersResponse' {} a -> s {listeners = a} :: DescribeListenersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeListenersResponse_httpStatus :: Lens.Lens' DescribeListenersResponse Core.Int
describeListenersResponse_httpStatus = Lens.lens (\DescribeListenersResponse' {httpStatus} -> httpStatus) (\s@DescribeListenersResponse' {} a -> s {httpStatus = a} :: DescribeListenersResponse)

instance Core.NFData DescribeListenersResponse

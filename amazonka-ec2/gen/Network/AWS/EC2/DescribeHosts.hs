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
-- Module      : Network.AWS.EC2.DescribeHosts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified Dedicated Hosts or all your Dedicated Hosts.
--
-- The results describe only the Dedicated Hosts in the Region you\'re
-- currently using. All listed instances consume capacity on your Dedicated
-- Host. Dedicated Hosts that have recently been released are listed with
-- the state @released@.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeHosts
  ( -- * Creating a Request
    DescribeHosts (..),
    newDescribeHosts,

    -- * Request Lenses
    describeHosts_nextToken,
    describeHosts_maxResults,
    describeHosts_hostIds,
    describeHosts_filter,

    -- * Destructuring the Response
    DescribeHostsResponse (..),
    newDescribeHostsResponse,

    -- * Response Lenses
    describeHostsResponse_nextToken,
    describeHostsResponse_hosts,
    describeHostsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeHosts' smart constructor.
data DescribeHosts = DescribeHosts'
  { -- | The token to use to retrieve the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return for the request in a single
    -- page. The remaining results can be seen by sending another request with
    -- the returned @nextToken@ value. This value can be between 5 and 500. If
    -- @maxResults@ is given a larger value than 500, you receive an error.
    --
    -- You cannot specify this parameter and the host IDs parameter in the same
    -- request.
    maxResults :: Core.Maybe Core.Int,
    -- | The IDs of the Dedicated Hosts. The IDs are used for targeted instance
    -- launches.
    hostIds :: Core.Maybe [Core.Text],
    -- | The filters.
    --
    -- -   @auto-placement@ - Whether auto-placement is enabled or disabled
    --     (@on@ | @off@).
    --
    -- -   @availability-zone@ - The Availability Zone of the host.
    --
    -- -   @client-token@ - The idempotency token that you provided when you
    --     allocated the host.
    --
    -- -   @host-reservation-id@ - The ID of the reservation assigned to this
    --     host.
    --
    -- -   @instance-type@ - The instance type size that the Dedicated Host is
    --     configured to support.
    --
    -- -   @state@ - The allocation state of the Dedicated Host (@available@ |
    --     @under-assessment@ | @permanent-failure@ | @released@ |
    --     @released-permanent-failure@).
    --
    -- -   @tag-key@ - The key of a tag assigned to the resource. Use this
    --     filter to find all resources assigned a tag with a specific key,
    --     regardless of the tag value.
    filter' :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeHosts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeHosts_nextToken' - The token to use to retrieve the next page of results.
--
-- 'maxResults', 'describeHosts_maxResults' - The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the returned @nextToken@ value. This value can be between 5 and 500. If
-- @maxResults@ is given a larger value than 500, you receive an error.
--
-- You cannot specify this parameter and the host IDs parameter in the same
-- request.
--
-- 'hostIds', 'describeHosts_hostIds' - The IDs of the Dedicated Hosts. The IDs are used for targeted instance
-- launches.
--
-- 'filter'', 'describeHosts_filter' - The filters.
--
-- -   @auto-placement@ - Whether auto-placement is enabled or disabled
--     (@on@ | @off@).
--
-- -   @availability-zone@ - The Availability Zone of the host.
--
-- -   @client-token@ - The idempotency token that you provided when you
--     allocated the host.
--
-- -   @host-reservation-id@ - The ID of the reservation assigned to this
--     host.
--
-- -   @instance-type@ - The instance type size that the Dedicated Host is
--     configured to support.
--
-- -   @state@ - The allocation state of the Dedicated Host (@available@ |
--     @under-assessment@ | @permanent-failure@ | @released@ |
--     @released-permanent-failure@).
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
newDescribeHosts ::
  DescribeHosts
newDescribeHosts =
  DescribeHosts'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      hostIds = Core.Nothing,
      filter' = Core.Nothing
    }

-- | The token to use to retrieve the next page of results.
describeHosts_nextToken :: Lens.Lens' DescribeHosts (Core.Maybe Core.Text)
describeHosts_nextToken = Lens.lens (\DescribeHosts' {nextToken} -> nextToken) (\s@DescribeHosts' {} a -> s {nextToken = a} :: DescribeHosts)

-- | The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the returned @nextToken@ value. This value can be between 5 and 500. If
-- @maxResults@ is given a larger value than 500, you receive an error.
--
-- You cannot specify this parameter and the host IDs parameter in the same
-- request.
describeHosts_maxResults :: Lens.Lens' DescribeHosts (Core.Maybe Core.Int)
describeHosts_maxResults = Lens.lens (\DescribeHosts' {maxResults} -> maxResults) (\s@DescribeHosts' {} a -> s {maxResults = a} :: DescribeHosts)

-- | The IDs of the Dedicated Hosts. The IDs are used for targeted instance
-- launches.
describeHosts_hostIds :: Lens.Lens' DescribeHosts (Core.Maybe [Core.Text])
describeHosts_hostIds = Lens.lens (\DescribeHosts' {hostIds} -> hostIds) (\s@DescribeHosts' {} a -> s {hostIds = a} :: DescribeHosts) Core.. Lens.mapping Lens._Coerce

-- | The filters.
--
-- -   @auto-placement@ - Whether auto-placement is enabled or disabled
--     (@on@ | @off@).
--
-- -   @availability-zone@ - The Availability Zone of the host.
--
-- -   @client-token@ - The idempotency token that you provided when you
--     allocated the host.
--
-- -   @host-reservation-id@ - The ID of the reservation assigned to this
--     host.
--
-- -   @instance-type@ - The instance type size that the Dedicated Host is
--     configured to support.
--
-- -   @state@ - The allocation state of the Dedicated Host (@available@ |
--     @under-assessment@ | @permanent-failure@ | @released@ |
--     @released-permanent-failure@).
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
describeHosts_filter :: Lens.Lens' DescribeHosts (Core.Maybe [Filter])
describeHosts_filter = Lens.lens (\DescribeHosts' {filter'} -> filter') (\s@DescribeHosts' {} a -> s {filter' = a} :: DescribeHosts) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeHosts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeHostsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeHostsResponse_hosts Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeHosts_nextToken
          Lens..~ rs
          Lens.^? describeHostsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest DescribeHosts where
  type
    AWSResponse DescribeHosts =
      DescribeHostsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeHostsResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "hostSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeHosts

instance Core.NFData DescribeHosts

instance Core.ToHeaders DescribeHosts where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeHosts where
  toPath = Core.const "/"

instance Core.ToQuery DescribeHosts where
  toQuery DescribeHosts' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeHosts" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          (Core.toQueryList "HostId" Core.<$> hostIds),
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filter')
      ]

-- | /See:/ 'newDescribeHostsResponse' smart constructor.
data DescribeHostsResponse = DescribeHostsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the Dedicated Hosts.
    hosts :: Core.Maybe [Host],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeHostsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeHostsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'hosts', 'describeHostsResponse_hosts' - Information about the Dedicated Hosts.
--
-- 'httpStatus', 'describeHostsResponse_httpStatus' - The response's http status code.
newDescribeHostsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeHostsResponse
newDescribeHostsResponse pHttpStatus_ =
  DescribeHostsResponse'
    { nextToken = Core.Nothing,
      hosts = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeHostsResponse_nextToken :: Lens.Lens' DescribeHostsResponse (Core.Maybe Core.Text)
describeHostsResponse_nextToken = Lens.lens (\DescribeHostsResponse' {nextToken} -> nextToken) (\s@DescribeHostsResponse' {} a -> s {nextToken = a} :: DescribeHostsResponse)

-- | Information about the Dedicated Hosts.
describeHostsResponse_hosts :: Lens.Lens' DescribeHostsResponse (Core.Maybe [Host])
describeHostsResponse_hosts = Lens.lens (\DescribeHostsResponse' {hosts} -> hosts) (\s@DescribeHostsResponse' {} a -> s {hosts = a} :: DescribeHostsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeHostsResponse_httpStatus :: Lens.Lens' DescribeHostsResponse Core.Int
describeHostsResponse_httpStatus = Lens.lens (\DescribeHostsResponse' {httpStatus} -> httpStatus) (\s@DescribeHostsResponse' {} a -> s {httpStatus = a} :: DescribeHostsResponse)

instance Core.NFData DescribeHostsResponse

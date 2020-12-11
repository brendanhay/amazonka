{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeHosts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified Dedicated Hosts or all your Dedicated Hosts.
--
-- The results describe only the Dedicated Hosts in the Region you're currently using. All listed instances consume capacity on your Dedicated Host. Dedicated Hosts that have recently been released are listed with the state @released@ .
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeHosts
  ( -- * Creating a request
    DescribeHosts (..),
    mkDescribeHosts,

    -- ** Request lenses
    dhNextToken,
    dhFilter,
    dhHostIds,
    dhMaxResults,

    -- * Destructuring the response
    DescribeHostsResponse (..),
    mkDescribeHostsResponse,

    -- ** Response lenses
    dhrsHosts,
    dhrsNextToken,
    dhrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeHosts' smart constructor.
data DescribeHosts = DescribeHosts'
  { nextToken ::
      Lude.Maybe Lude.Text,
    filter :: Lude.Maybe [Filter],
    hostIds :: Lude.Maybe [Lude.Text],
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeHosts' with the minimum fields required to make a request.
--
-- * 'filter' - The filters.
--
--
--     * @auto-placement@ - Whether auto-placement is enabled or disabled (@on@ | @off@ ).
--
--
--     * @availability-zone@ - The Availability Zone of the host.
--
--
--     * @client-token@ - The idempotency token that you provided when you allocated the host.
--
--
--     * @host-reservation-id@ - The ID of the reservation assigned to this host.
--
--
--     * @instance-type@ - The instance type size that the Dedicated Host is configured to support.
--
--
--     * @state@ - The allocation state of the Dedicated Host (@available@ | @under-assessment@ | @permanent-failure@ | @released@ | @released-permanent-failure@ ).
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
-- * 'hostIds' - The IDs of the Dedicated Hosts. The IDs are used for targeted instance launches.
-- * 'maxResults' - The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @nextToken@ value. This value can be between 5 and 500. If @maxResults@ is given a larger value than 500, you receive an error.
--
-- You cannot specify this parameter and the host IDs parameter in the same request.
-- * 'nextToken' - The token to use to retrieve the next page of results.
mkDescribeHosts ::
  DescribeHosts
mkDescribeHosts =
  DescribeHosts'
    { nextToken = Lude.Nothing,
      filter = Lude.Nothing,
      hostIds = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The token to use to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhNextToken :: Lens.Lens' DescribeHosts (Lude.Maybe Lude.Text)
dhNextToken = Lens.lens (nextToken :: DescribeHosts -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeHosts)
{-# DEPRECATED dhNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The filters.
--
--
--     * @auto-placement@ - Whether auto-placement is enabled or disabled (@on@ | @off@ ).
--
--
--     * @availability-zone@ - The Availability Zone of the host.
--
--
--     * @client-token@ - The idempotency token that you provided when you allocated the host.
--
--
--     * @host-reservation-id@ - The ID of the reservation assigned to this host.
--
--
--     * @instance-type@ - The instance type size that the Dedicated Host is configured to support.
--
--
--     * @state@ - The allocation state of the Dedicated Host (@available@ | @under-assessment@ | @permanent-failure@ | @released@ | @released-permanent-failure@ ).
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhFilter :: Lens.Lens' DescribeHosts (Lude.Maybe [Filter])
dhFilter = Lens.lens (filter :: DescribeHosts -> Lude.Maybe [Filter]) (\s a -> s {filter = a} :: DescribeHosts)
{-# DEPRECATED dhFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The IDs of the Dedicated Hosts. The IDs are used for targeted instance launches.
--
-- /Note:/ Consider using 'hostIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhHostIds :: Lens.Lens' DescribeHosts (Lude.Maybe [Lude.Text])
dhHostIds = Lens.lens (hostIds :: DescribeHosts -> Lude.Maybe [Lude.Text]) (\s a -> s {hostIds = a} :: DescribeHosts)
{-# DEPRECATED dhHostIds "Use generic-lens or generic-optics with 'hostIds' instead." #-}

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @nextToken@ value. This value can be between 5 and 500. If @maxResults@ is given a larger value than 500, you receive an error.
--
-- You cannot specify this parameter and the host IDs parameter in the same request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhMaxResults :: Lens.Lens' DescribeHosts (Lude.Maybe Lude.Int)
dhMaxResults = Lens.lens (maxResults :: DescribeHosts -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeHosts)
{-# DEPRECATED dhMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeHosts where
  page rq rs
    | Page.stop (rs Lens.^. dhrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dhrsHosts) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dhNextToken Lens..~ rs Lens.^. dhrsNextToken

instance Lude.AWSRequest DescribeHosts where
  type Rs DescribeHosts = DescribeHostsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeHostsResponse'
            Lude.<$> ( x Lude..@? "hostSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeHosts where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeHosts where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeHosts where
  toQuery DescribeHosts' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeHosts" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken,
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filter),
        Lude.toQuery (Lude.toQueryList "HostId" Lude.<$> hostIds),
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeHostsResponse' smart constructor.
data DescribeHostsResponse = DescribeHostsResponse'
  { hosts ::
      Lude.Maybe [Host],
    nextToken :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeHostsResponse' with the minimum fields required to make a request.
--
-- * 'hosts' - Information about the Dedicated Hosts.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeHostsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeHostsResponse
mkDescribeHostsResponse pResponseStatus_ =
  DescribeHostsResponse'
    { hosts = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the Dedicated Hosts.
--
-- /Note:/ Consider using 'hosts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrsHosts :: Lens.Lens' DescribeHostsResponse (Lude.Maybe [Host])
dhrsHosts = Lens.lens (hosts :: DescribeHostsResponse -> Lude.Maybe [Host]) (\s a -> s {hosts = a} :: DescribeHostsResponse)
{-# DEPRECATED dhrsHosts "Use generic-lens or generic-optics with 'hosts' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrsNextToken :: Lens.Lens' DescribeHostsResponse (Lude.Maybe Lude.Text)
dhrsNextToken = Lens.lens (nextToken :: DescribeHostsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeHostsResponse)
{-# DEPRECATED dhrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrsResponseStatus :: Lens.Lens' DescribeHostsResponse Lude.Int
dhrsResponseStatus = Lens.lens (responseStatus :: DescribeHostsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeHostsResponse)
{-# DEPRECATED dhrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

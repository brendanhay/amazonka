{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeDHCPOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your DHCP options sets.
--
-- For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_DHCP_Options.html DHCP Options Sets> in the /Amazon Virtual Private Cloud User Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeDHCPOptions
  ( -- * Creating a request
    DescribeDHCPOptions (..),
    mkDescribeDHCPOptions,

    -- ** Request lenses
    ddhcpoFilters,
    ddhcpoDHCPOptionsIds,
    ddhcpoNextToken,
    ddhcpoDryRun,
    ddhcpoMaxResults,

    -- * Destructuring the response
    DescribeDHCPOptionsResponse (..),
    mkDescribeDHCPOptionsResponse,

    -- ** Response lenses
    ddorsDHCPOptions,
    ddorsNextToken,
    ddorsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeDHCPOptions' smart constructor.
data DescribeDHCPOptions = DescribeDHCPOptions'
  { -- | One or more filters.
    --
    --
    --     * @dhcp-options-id@ - The ID of a DHCP options set.
    --
    --
    --     * @key@ - The key for one of the options (for example, @domain-name@ ).
    --
    --
    --     * @value@ - The value for one of the options.
    --
    --
    --     * @owner-id@ - The ID of the AWS account that owns the DHCP options set.
    --
    --
    --     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
    --
    --
    --     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
    filters :: Lude.Maybe [Filter],
    -- | The IDs of one or more DHCP options sets.
    --
    -- Default: Describes all your DHCP options sets.
    dhcpOptionsIds :: Lude.Maybe [Lude.Text],
    -- | The token for the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDHCPOptions' with the minimum fields required to make a request.
--
-- * 'filters' - One or more filters.
--
--
--     * @dhcp-options-id@ - The ID of a DHCP options set.
--
--
--     * @key@ - The key for one of the options (for example, @domain-name@ ).
--
--
--     * @value@ - The value for one of the options.
--
--
--     * @owner-id@ - The ID of the AWS account that owns the DHCP options set.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
-- * 'dhcpOptionsIds' - The IDs of one or more DHCP options sets.
--
-- Default: Describes all your DHCP options sets.
-- * 'nextToken' - The token for the next page of results.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
mkDescribeDHCPOptions ::
  DescribeDHCPOptions
mkDescribeDHCPOptions =
  DescribeDHCPOptions'
    { filters = Lude.Nothing,
      dhcpOptionsIds = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters.
--
--
--     * @dhcp-options-id@ - The ID of a DHCP options set.
--
--
--     * @key@ - The key for one of the options (for example, @domain-name@ ).
--
--
--     * @value@ - The value for one of the options.
--
--
--     * @owner-id@ - The ID of the AWS account that owns the DHCP options set.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddhcpoFilters :: Lens.Lens' DescribeDHCPOptions (Lude.Maybe [Filter])
ddhcpoFilters = Lens.lens (filters :: DescribeDHCPOptions -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeDHCPOptions)
{-# DEPRECATED ddhcpoFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The IDs of one or more DHCP options sets.
--
-- Default: Describes all your DHCP options sets.
--
-- /Note:/ Consider using 'dhcpOptionsIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddhcpoDHCPOptionsIds :: Lens.Lens' DescribeDHCPOptions (Lude.Maybe [Lude.Text])
ddhcpoDHCPOptionsIds = Lens.lens (dhcpOptionsIds :: DescribeDHCPOptions -> Lude.Maybe [Lude.Text]) (\s a -> s {dhcpOptionsIds = a} :: DescribeDHCPOptions)
{-# DEPRECATED ddhcpoDHCPOptionsIds "Use generic-lens or generic-optics with 'dhcpOptionsIds' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddhcpoNextToken :: Lens.Lens' DescribeDHCPOptions (Lude.Maybe Lude.Text)
ddhcpoNextToken = Lens.lens (nextToken :: DescribeDHCPOptions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeDHCPOptions)
{-# DEPRECATED ddhcpoNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddhcpoDryRun :: Lens.Lens' DescribeDHCPOptions (Lude.Maybe Lude.Bool)
ddhcpoDryRun = Lens.lens (dryRun :: DescribeDHCPOptions -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeDHCPOptions)
{-# DEPRECATED ddhcpoDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddhcpoMaxResults :: Lens.Lens' DescribeDHCPOptions (Lude.Maybe Lude.Natural)
ddhcpoMaxResults = Lens.lens (maxResults :: DescribeDHCPOptions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeDHCPOptions)
{-# DEPRECATED ddhcpoMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeDHCPOptions where
  page rq rs
    | Page.stop (rs Lens.^. ddorsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ddorsDHCPOptions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ddhcpoNextToken Lens..~ rs Lens.^. ddorsNextToken

instance Lude.AWSRequest DescribeDHCPOptions where
  type Rs DescribeDHCPOptions = DescribeDHCPOptionsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeDHCPOptionsResponse'
            Lude.<$> ( x Lude..@? "dhcpOptionsSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDHCPOptions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeDHCPOptions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDHCPOptions where
  toQuery DescribeDHCPOptions' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeDhcpOptions" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        Lude.toQuery
          (Lude.toQueryList "DhcpOptionsId" Lude.<$> dhcpOptionsIds),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeDHCPOptionsResponse' smart constructor.
data DescribeDHCPOptionsResponse = DescribeDHCPOptionsResponse'
  { -- | Information about one or more DHCP options sets.
    dhcpOptions :: Lude.Maybe [DHCPOptions],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDHCPOptionsResponse' with the minimum fields required to make a request.
--
-- * 'dhcpOptions' - Information about one or more DHCP options sets.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeDHCPOptionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDHCPOptionsResponse
mkDescribeDHCPOptionsResponse pResponseStatus_ =
  DescribeDHCPOptionsResponse'
    { dhcpOptions = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about one or more DHCP options sets.
--
-- /Note:/ Consider using 'dhcpOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddorsDHCPOptions :: Lens.Lens' DescribeDHCPOptionsResponse (Lude.Maybe [DHCPOptions])
ddorsDHCPOptions = Lens.lens (dhcpOptions :: DescribeDHCPOptionsResponse -> Lude.Maybe [DHCPOptions]) (\s a -> s {dhcpOptions = a} :: DescribeDHCPOptionsResponse)
{-# DEPRECATED ddorsDHCPOptions "Use generic-lens or generic-optics with 'dhcpOptions' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddorsNextToken :: Lens.Lens' DescribeDHCPOptionsResponse (Lude.Maybe Lude.Text)
ddorsNextToken = Lens.lens (nextToken :: DescribeDHCPOptionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeDHCPOptionsResponse)
{-# DEPRECATED ddorsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddorsResponseStatus :: Lens.Lens' DescribeDHCPOptionsResponse Lude.Int
ddorsResponseStatus = Lens.lens (responseStatus :: DescribeDHCPOptionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDHCPOptionsResponse)
{-# DEPRECATED ddorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeClientVPNAuthorizationRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the authorization rules for a specified Client VPN endpoint.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeClientVPNAuthorizationRules
  ( -- * Creating a request
    DescribeClientVPNAuthorizationRules (..),
    mkDescribeClientVPNAuthorizationRules,

    -- ** Request lenses
    dcvarFilters,
    dcvarNextToken,
    dcvarDryRun,
    dcvarMaxResults,
    dcvarClientVPNEndpointId,

    -- * Destructuring the response
    DescribeClientVPNAuthorizationRulesResponse (..),
    mkDescribeClientVPNAuthorizationRulesResponse,

    -- ** Response lenses
    dcvarrsAuthorizationRules,
    dcvarrsNextToken,
    dcvarrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeClientVPNAuthorizationRules' smart constructor.
data DescribeClientVPNAuthorizationRules = DescribeClientVPNAuthorizationRules'
  { filters ::
      Lude.Maybe [Filter],
    nextToken ::
      Lude.Maybe
        Lude.Text,
    dryRun ::
      Lude.Maybe
        Lude.Bool,
    maxResults ::
      Lude.Maybe
        Lude.Natural,
    clientVPNEndpointId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeClientVPNAuthorizationRules' with the minimum fields required to make a request.
--
-- * 'clientVPNEndpointId' - The ID of the Client VPN endpoint.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - One or more filters. Filter names and values are case-sensitive.
--
--
--     * @description@ - The description of the authorization rule.
--
--
--     * @destination-cidr@ - The CIDR of the network to which the authorization rule applies.
--
--
--     * @group-id@ - The ID of the Active Directory group to which the authorization rule grants access.
--
--
-- * 'maxResults' - The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the nextToken value.
-- * 'nextToken' - The token to retrieve the next page of results.
mkDescribeClientVPNAuthorizationRules ::
  -- | 'clientVPNEndpointId'
  Lude.Text ->
  DescribeClientVPNAuthorizationRules
mkDescribeClientVPNAuthorizationRules pClientVPNEndpointId_ =
  DescribeClientVPNAuthorizationRules'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing,
      clientVPNEndpointId = pClientVPNEndpointId_
    }

-- | One or more filters. Filter names and values are case-sensitive.
--
--
--     * @description@ - The description of the authorization rule.
--
--
--     * @destination-cidr@ - The CIDR of the network to which the authorization rule applies.
--
--
--     * @group-id@ - The ID of the Active Directory group to which the authorization rule grants access.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvarFilters :: Lens.Lens' DescribeClientVPNAuthorizationRules (Lude.Maybe [Filter])
dcvarFilters = Lens.lens (filters :: DescribeClientVPNAuthorizationRules -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeClientVPNAuthorizationRules)
{-# DEPRECATED dcvarFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvarNextToken :: Lens.Lens' DescribeClientVPNAuthorizationRules (Lude.Maybe Lude.Text)
dcvarNextToken = Lens.lens (nextToken :: DescribeClientVPNAuthorizationRules -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeClientVPNAuthorizationRules)
{-# DEPRECATED dcvarNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvarDryRun :: Lens.Lens' DescribeClientVPNAuthorizationRules (Lude.Maybe Lude.Bool)
dcvarDryRun = Lens.lens (dryRun :: DescribeClientVPNAuthorizationRules -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeClientVPNAuthorizationRules)
{-# DEPRECATED dcvarDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the nextToken value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvarMaxResults :: Lens.Lens' DescribeClientVPNAuthorizationRules (Lude.Maybe Lude.Natural)
dcvarMaxResults = Lens.lens (maxResults :: DescribeClientVPNAuthorizationRules -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeClientVPNAuthorizationRules)
{-# DEPRECATED dcvarMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The ID of the Client VPN endpoint.
--
-- /Note:/ Consider using 'clientVPNEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvarClientVPNEndpointId :: Lens.Lens' DescribeClientVPNAuthorizationRules Lude.Text
dcvarClientVPNEndpointId = Lens.lens (clientVPNEndpointId :: DescribeClientVPNAuthorizationRules -> Lude.Text) (\s a -> s {clientVPNEndpointId = a} :: DescribeClientVPNAuthorizationRules)
{-# DEPRECATED dcvarClientVPNEndpointId "Use generic-lens or generic-optics with 'clientVPNEndpointId' instead." #-}

instance Page.AWSPager DescribeClientVPNAuthorizationRules where
  page rq rs
    | Page.stop (rs Lens.^. dcvarrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dcvarrsAuthorizationRules) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dcvarNextToken Lens..~ rs Lens.^. dcvarrsNextToken

instance Lude.AWSRequest DescribeClientVPNAuthorizationRules where
  type
    Rs DescribeClientVPNAuthorizationRules =
      DescribeClientVPNAuthorizationRulesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeClientVPNAuthorizationRulesResponse'
            Lude.<$> ( x Lude..@? "authorizationRule" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeClientVPNAuthorizationRules where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeClientVPNAuthorizationRules where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeClientVPNAuthorizationRules where
  toQuery DescribeClientVPNAuthorizationRules' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeClientVpnAuthorizationRules" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults,
        "ClientVpnEndpointId" Lude.=: clientVPNEndpointId
      ]

-- | /See:/ 'mkDescribeClientVPNAuthorizationRulesResponse' smart constructor.
data DescribeClientVPNAuthorizationRulesResponse = DescribeClientVPNAuthorizationRulesResponse'
  { authorizationRules ::
      Lude.Maybe
        [AuthorizationRule],
    nextToken ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeClientVPNAuthorizationRulesResponse' with the minimum fields required to make a request.
--
-- * 'authorizationRules' - Information about the authorization rules.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeClientVPNAuthorizationRulesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeClientVPNAuthorizationRulesResponse
mkDescribeClientVPNAuthorizationRulesResponse pResponseStatus_ =
  DescribeClientVPNAuthorizationRulesResponse'
    { authorizationRules =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the authorization rules.
--
-- /Note:/ Consider using 'authorizationRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvarrsAuthorizationRules :: Lens.Lens' DescribeClientVPNAuthorizationRulesResponse (Lude.Maybe [AuthorizationRule])
dcvarrsAuthorizationRules = Lens.lens (authorizationRules :: DescribeClientVPNAuthorizationRulesResponse -> Lude.Maybe [AuthorizationRule]) (\s a -> s {authorizationRules = a} :: DescribeClientVPNAuthorizationRulesResponse)
{-# DEPRECATED dcvarrsAuthorizationRules "Use generic-lens or generic-optics with 'authorizationRules' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvarrsNextToken :: Lens.Lens' DescribeClientVPNAuthorizationRulesResponse (Lude.Maybe Lude.Text)
dcvarrsNextToken = Lens.lens (nextToken :: DescribeClientVPNAuthorizationRulesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeClientVPNAuthorizationRulesResponse)
{-# DEPRECATED dcvarrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvarrsResponseStatus :: Lens.Lens' DescribeClientVPNAuthorizationRulesResponse Lude.Int
dcvarrsResponseStatus = Lens.lens (responseStatus :: DescribeClientVPNAuthorizationRulesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeClientVPNAuthorizationRulesResponse)
{-# DEPRECATED dcvarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

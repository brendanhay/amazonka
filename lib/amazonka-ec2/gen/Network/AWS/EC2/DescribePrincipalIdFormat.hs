{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribePrincipalIdFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the ID format settings for the root user and all IAM roles and IAM users that have explicitly specified a longer ID (17-character ID) preference.
--
-- By default, all IAM roles and IAM users default to the same ID settings as the root user, unless they explicitly override the settings. This request is useful for identifying those IAM users and IAM roles that have overridden the default ID settings.
-- The following resource types support longer IDs: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @instance@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @reservation@ | @route-table@ | @route-table-association@ | @security-group@ | @snapshot@ | @subnet@ | @subnet-cidr-block-association@ | @volume@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@ .
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribePrincipalIdFormat
  ( -- * Creating a request
    DescribePrincipalIdFormat (..),
    mkDescribePrincipalIdFormat,

    -- ** Request lenses
    dpifResources,
    dpifNextToken,
    dpifDryRun,
    dpifMaxResults,

    -- * Destructuring the response
    DescribePrincipalIdFormatResponse (..),
    mkDescribePrincipalIdFormatResponse,

    -- ** Response lenses
    dpifrsPrincipals,
    dpifrsNextToken,
    dpifrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribePrincipalIdFormat' smart constructor.
data DescribePrincipalIdFormat = DescribePrincipalIdFormat'
  { -- | The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @instance@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @reservation@ | @route-table@ | @route-table-association@ | @security-group@ | @snapshot@ | @subnet@ | @subnet-cidr-block-association@ | @volume@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@
    resources :: Lude.Maybe [Lude.Text],
    -- | The token to request the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned NextToken value.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePrincipalIdFormat' with the minimum fields required to make a request.
--
-- * 'resources' - The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @instance@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @reservation@ | @route-table@ | @route-table-association@ | @security-group@ | @snapshot@ | @subnet@ | @subnet-cidr-block-association@ | @volume@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@
-- * 'nextToken' - The token to request the next page of results.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned NextToken value.
mkDescribePrincipalIdFormat ::
  DescribePrincipalIdFormat
mkDescribePrincipalIdFormat =
  DescribePrincipalIdFormat'
    { resources = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @instance@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @reservation@ | @route-table@ | @route-table-association@ | @security-group@ | @snapshot@ | @subnet@ | @subnet-cidr-block-association@ | @volume@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpifResources :: Lens.Lens' DescribePrincipalIdFormat (Lude.Maybe [Lude.Text])
dpifResources = Lens.lens (resources :: DescribePrincipalIdFormat -> Lude.Maybe [Lude.Text]) (\s a -> s {resources = a} :: DescribePrincipalIdFormat)
{-# DEPRECATED dpifResources "Use generic-lens or generic-optics with 'resources' instead." #-}

-- | The token to request the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpifNextToken :: Lens.Lens' DescribePrincipalIdFormat (Lude.Maybe Lude.Text)
dpifNextToken = Lens.lens (nextToken :: DescribePrincipalIdFormat -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribePrincipalIdFormat)
{-# DEPRECATED dpifNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpifDryRun :: Lens.Lens' DescribePrincipalIdFormat (Lude.Maybe Lude.Bool)
dpifDryRun = Lens.lens (dryRun :: DescribePrincipalIdFormat -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribePrincipalIdFormat)
{-# DEPRECATED dpifDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned NextToken value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpifMaxResults :: Lens.Lens' DescribePrincipalIdFormat (Lude.Maybe Lude.Natural)
dpifMaxResults = Lens.lens (maxResults :: DescribePrincipalIdFormat -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribePrincipalIdFormat)
{-# DEPRECATED dpifMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribePrincipalIdFormat where
  page rq rs
    | Page.stop (rs Lens.^. dpifrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dpifrsPrincipals) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dpifNextToken Lens..~ rs Lens.^. dpifrsNextToken

instance Lude.AWSRequest DescribePrincipalIdFormat where
  type
    Rs DescribePrincipalIdFormat =
      DescribePrincipalIdFormatResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribePrincipalIdFormatResponse'
            Lude.<$> ( x Lude..@? "principalSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribePrincipalIdFormat where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribePrincipalIdFormat where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribePrincipalIdFormat where
  toQuery DescribePrincipalIdFormat' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribePrincipalIdFormat" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Resource" Lude.<$> resources),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribePrincipalIdFormatResponse' smart constructor.
data DescribePrincipalIdFormatResponse = DescribePrincipalIdFormatResponse'
  { -- | Information about the ID format settings for the ARN.
    principals :: Lude.Maybe [PrincipalIdFormat],
    -- | The token to use to retrieve the next page of results. This value is null when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePrincipalIdFormatResponse' with the minimum fields required to make a request.
--
-- * 'principals' - Information about the ID format settings for the ARN.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is null when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribePrincipalIdFormatResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribePrincipalIdFormatResponse
mkDescribePrincipalIdFormatResponse pResponseStatus_ =
  DescribePrincipalIdFormatResponse'
    { principals = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the ID format settings for the ARN.
--
-- /Note:/ Consider using 'principals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpifrsPrincipals :: Lens.Lens' DescribePrincipalIdFormatResponse (Lude.Maybe [PrincipalIdFormat])
dpifrsPrincipals = Lens.lens (principals :: DescribePrincipalIdFormatResponse -> Lude.Maybe [PrincipalIdFormat]) (\s a -> s {principals = a} :: DescribePrincipalIdFormatResponse)
{-# DEPRECATED dpifrsPrincipals "Use generic-lens or generic-optics with 'principals' instead." #-}

-- | The token to use to retrieve the next page of results. This value is null when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpifrsNextToken :: Lens.Lens' DescribePrincipalIdFormatResponse (Lude.Maybe Lude.Text)
dpifrsNextToken = Lens.lens (nextToken :: DescribePrincipalIdFormatResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribePrincipalIdFormatResponse)
{-# DEPRECATED dpifrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpifrsResponseStatus :: Lens.Lens' DescribePrincipalIdFormatResponse Lude.Int
dpifrsResponseStatus = Lens.lens (responseStatus :: DescribePrincipalIdFormatResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribePrincipalIdFormatResponse)
{-# DEPRECATED dpifrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

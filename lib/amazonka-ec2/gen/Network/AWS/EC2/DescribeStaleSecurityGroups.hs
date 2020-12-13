{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeStaleSecurityGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- [VPC only] Describes the stale security group rules for security groups in a specified VPC. Rules are stale when they reference a deleted security group in a peer VPC, or a security group in a peer VPC for which the VPC peering connection has been deleted.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeStaleSecurityGroups
  ( -- * Creating a request
    DescribeStaleSecurityGroups (..),
    mkDescribeStaleSecurityGroups,

    -- ** Request lenses
    dssgVPCId,
    dssgNextToken,
    dssgDryRun,
    dssgMaxResults,

    -- * Destructuring the response
    DescribeStaleSecurityGroupsResponse (..),
    mkDescribeStaleSecurityGroupsResponse,

    -- ** Response lenses
    dssgrsStaleSecurityGroupSet,
    dssgrsNextToken,
    dssgrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeStaleSecurityGroups' smart constructor.
data DescribeStaleSecurityGroups = DescribeStaleSecurityGroups'
  { -- | The ID of the VPC.
    vpcId :: Lude.Text,
    -- | The token for the next set of items to return. (You received this token from a prior call.)
    nextToken :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of items to return for this request. The request returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStaleSecurityGroups' with the minimum fields required to make a request.
--
-- * 'vpcId' - The ID of the VPC.
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a prior call.)
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of items to return for this request. The request returns a token that you can specify in a subsequent call to get the next set of results.
mkDescribeStaleSecurityGroups ::
  -- | 'vpcId'
  Lude.Text ->
  DescribeStaleSecurityGroups
mkDescribeStaleSecurityGroups pVPCId_ =
  DescribeStaleSecurityGroups'
    { vpcId = pVPCId_,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssgVPCId :: Lens.Lens' DescribeStaleSecurityGroups Lude.Text
dssgVPCId = Lens.lens (vpcId :: DescribeStaleSecurityGroups -> Lude.Text) (\s a -> s {vpcId = a} :: DescribeStaleSecurityGroups)
{-# DEPRECATED dssgVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The token for the next set of items to return. (You received this token from a prior call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssgNextToken :: Lens.Lens' DescribeStaleSecurityGroups (Lude.Maybe Lude.Text)
dssgNextToken = Lens.lens (nextToken :: DescribeStaleSecurityGroups -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeStaleSecurityGroups)
{-# DEPRECATED dssgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssgDryRun :: Lens.Lens' DescribeStaleSecurityGroups (Lude.Maybe Lude.Bool)
dssgDryRun = Lens.lens (dryRun :: DescribeStaleSecurityGroups -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeStaleSecurityGroups)
{-# DEPRECATED dssgDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of items to return for this request. The request returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssgMaxResults :: Lens.Lens' DescribeStaleSecurityGroups (Lude.Maybe Lude.Natural)
dssgMaxResults = Lens.lens (maxResults :: DescribeStaleSecurityGroups -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeStaleSecurityGroups)
{-# DEPRECATED dssgMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeStaleSecurityGroups where
  page rq rs
    | Page.stop (rs Lens.^. dssgrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dssgrsStaleSecurityGroupSet) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dssgNextToken Lens..~ rs Lens.^. dssgrsNextToken

instance Lude.AWSRequest DescribeStaleSecurityGroups where
  type
    Rs DescribeStaleSecurityGroups =
      DescribeStaleSecurityGroupsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeStaleSecurityGroupsResponse'
            Lude.<$> ( x Lude..@? "staleSecurityGroupSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeStaleSecurityGroups where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeStaleSecurityGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeStaleSecurityGroups where
  toQuery DescribeStaleSecurityGroups' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeStaleSecurityGroups" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "VpcId" Lude.=: vpcId,
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeStaleSecurityGroupsResponse' smart constructor.
data DescribeStaleSecurityGroupsResponse = DescribeStaleSecurityGroupsResponse'
  { -- | Information about the stale security groups.
    staleSecurityGroupSet :: Lude.Maybe [StaleSecurityGroup],
    -- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStaleSecurityGroupsResponse' with the minimum fields required to make a request.
--
-- * 'staleSecurityGroupSet' - Information about the stale security groups.
-- * 'nextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
-- * 'responseStatus' - The response status code.
mkDescribeStaleSecurityGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeStaleSecurityGroupsResponse
mkDescribeStaleSecurityGroupsResponse pResponseStatus_ =
  DescribeStaleSecurityGroupsResponse'
    { staleSecurityGroupSet =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the stale security groups.
--
-- /Note:/ Consider using 'staleSecurityGroupSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssgrsStaleSecurityGroupSet :: Lens.Lens' DescribeStaleSecurityGroupsResponse (Lude.Maybe [StaleSecurityGroup])
dssgrsStaleSecurityGroupSet = Lens.lens (staleSecurityGroupSet :: DescribeStaleSecurityGroupsResponse -> Lude.Maybe [StaleSecurityGroup]) (\s a -> s {staleSecurityGroupSet = a} :: DescribeStaleSecurityGroupsResponse)
{-# DEPRECATED dssgrsStaleSecurityGroupSet "Use generic-lens or generic-optics with 'staleSecurityGroupSet' instead." #-}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssgrsNextToken :: Lens.Lens' DescribeStaleSecurityGroupsResponse (Lude.Maybe Lude.Text)
dssgrsNextToken = Lens.lens (nextToken :: DescribeStaleSecurityGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeStaleSecurityGroupsResponse)
{-# DEPRECATED dssgrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssgrsResponseStatus :: Lens.Lens' DescribeStaleSecurityGroupsResponse Lude.Int
dssgrsResponseStatus = Lens.lens (responseStatus :: DescribeStaleSecurityGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeStaleSecurityGroupsResponse)
{-# DEPRECATED dssgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

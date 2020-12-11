{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.DescribeSubnetGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of subnet group descriptions. If a subnet group name is specified, the list will contain only the description of that group.
--
-- This operation returns paginated results.
module Network.AWS.DAX.DescribeSubnetGroups
  ( -- * Creating a request
    DescribeSubnetGroups (..),
    mkDescribeSubnetGroups,

    -- ** Request lenses
    dsgSubnetGroupNames,
    dsgNextToken,
    dsgMaxResults,

    -- * Destructuring the response
    DescribeSubnetGroupsResponse (..),
    mkDescribeSubnetGroupsResponse,

    -- ** Response lenses
    dsgsrsSubnetGroups,
    dsgsrsNextToken,
    dsgsrsResponseStatus,
  )
where

import Network.AWS.DAX.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeSubnetGroups' smart constructor.
data DescribeSubnetGroups = DescribeSubnetGroups'
  { subnetGroupNames ::
      Lude.Maybe [Lude.Text],
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeSubnetGroups' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
--
-- The value for @MaxResults@ must be between 20 and 100.
-- * 'nextToken' - An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
-- * 'subnetGroupNames' - The name of the subnet group.
mkDescribeSubnetGroups ::
  DescribeSubnetGroups
mkDescribeSubnetGroups =
  DescribeSubnetGroups'
    { subnetGroupNames = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The name of the subnet group.
--
-- /Note:/ Consider using 'subnetGroupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgSubnetGroupNames :: Lens.Lens' DescribeSubnetGroups (Lude.Maybe [Lude.Text])
dsgSubnetGroupNames = Lens.lens (subnetGroupNames :: DescribeSubnetGroups -> Lude.Maybe [Lude.Text]) (\s a -> s {subnetGroupNames = a} :: DescribeSubnetGroups)
{-# DEPRECATED dsgSubnetGroupNames "Use generic-lens or generic-optics with 'subnetGroupNames' instead." #-}

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgNextToken :: Lens.Lens' DescribeSubnetGroups (Lude.Maybe Lude.Text)
dsgNextToken = Lens.lens (nextToken :: DescribeSubnetGroups -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeSubnetGroups)
{-# DEPRECATED dsgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
--
-- The value for @MaxResults@ must be between 20 and 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgMaxResults :: Lens.Lens' DescribeSubnetGroups (Lude.Maybe Lude.Int)
dsgMaxResults = Lens.lens (maxResults :: DescribeSubnetGroups -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeSubnetGroups)
{-# DEPRECATED dsgMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeSubnetGroups where
  page rq rs
    | Page.stop (rs Lens.^. dsgsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dsgsrsSubnetGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dsgNextToken Lens..~ rs Lens.^. dsgsrsNextToken

instance Lude.AWSRequest DescribeSubnetGroups where
  type Rs DescribeSubnetGroups = DescribeSubnetGroupsResponse
  request = Req.postJSON daxService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeSubnetGroupsResponse'
            Lude.<$> (x Lude..?> "SubnetGroups" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeSubnetGroups where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDAXV3.DescribeSubnetGroups" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeSubnetGroups where
  toJSON DescribeSubnetGroups' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SubnetGroupNames" Lude..=) Lude.<$> subnetGroupNames,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeSubnetGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeSubnetGroups where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeSubnetGroupsResponse' smart constructor.
data DescribeSubnetGroupsResponse = DescribeSubnetGroupsResponse'
  { subnetGroups ::
      Lude.Maybe [SubnetGroup],
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

-- | Creates a value of 'DescribeSubnetGroupsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Provides an identifier to allow retrieval of paginated results.
-- * 'responseStatus' - The response status code.
-- * 'subnetGroups' - An array of subnet groups. Each element in the array represents a single subnet group.
mkDescribeSubnetGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeSubnetGroupsResponse
mkDescribeSubnetGroupsResponse pResponseStatus_ =
  DescribeSubnetGroupsResponse'
    { subnetGroups = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of subnet groups. Each element in the array represents a single subnet group.
--
-- /Note:/ Consider using 'subnetGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgsrsSubnetGroups :: Lens.Lens' DescribeSubnetGroupsResponse (Lude.Maybe [SubnetGroup])
dsgsrsSubnetGroups = Lens.lens (subnetGroups :: DescribeSubnetGroupsResponse -> Lude.Maybe [SubnetGroup]) (\s a -> s {subnetGroups = a} :: DescribeSubnetGroupsResponse)
{-# DEPRECATED dsgsrsSubnetGroups "Use generic-lens or generic-optics with 'subnetGroups' instead." #-}

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgsrsNextToken :: Lens.Lens' DescribeSubnetGroupsResponse (Lude.Maybe Lude.Text)
dsgsrsNextToken = Lens.lens (nextToken :: DescribeSubnetGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeSubnetGroupsResponse)
{-# DEPRECATED dsgsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgsrsResponseStatus :: Lens.Lens' DescribeSubnetGroupsResponse Lude.Int
dsgsrsResponseStatus = Lens.lens (responseStatus :: DescribeSubnetGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSubnetGroupsResponse)
{-# DEPRECATED dsgsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

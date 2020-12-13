{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.DescribeParameterGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of parameter group descriptions. If a parameter group name is specified, the list will contain only the descriptions for that group.
--
-- This operation returns paginated results.
module Network.AWS.DAX.DescribeParameterGroups
  ( -- * Creating a request
    DescribeParameterGroups (..),
    mkDescribeParameterGroups,

    -- ** Request lenses
    dpgNextToken,
    dpgParameterGroupNames,
    dpgMaxResults,

    -- * Destructuring the response
    DescribeParameterGroupsResponse (..),
    mkDescribeParameterGroupsResponse,

    -- ** Response lenses
    dpgsrsNextToken,
    dpgsrsParameterGroups,
    dpgsrsResponseStatus,
  )
where

import Network.AWS.DAX.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeParameterGroups' smart constructor.
data DescribeParameterGroups = DescribeParameterGroups'
  { -- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
    nextToken :: Lude.Maybe Lude.Text,
    -- | The names of the parameter groups.
    parameterGroupNames :: Lude.Maybe [Lude.Text],
    -- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
    --
    -- The value for @MaxResults@ must be between 20 and 100.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeParameterGroups' with the minimum fields required to make a request.
--
-- * 'nextToken' - An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
-- * 'parameterGroupNames' - The names of the parameter groups.
-- * 'maxResults' - The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
--
-- The value for @MaxResults@ must be between 20 and 100.
mkDescribeParameterGroups ::
  DescribeParameterGroups
mkDescribeParameterGroups =
  DescribeParameterGroups'
    { nextToken = Lude.Nothing,
      parameterGroupNames = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgNextToken :: Lens.Lens' DescribeParameterGroups (Lude.Maybe Lude.Text)
dpgNextToken = Lens.lens (nextToken :: DescribeParameterGroups -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeParameterGroups)
{-# DEPRECATED dpgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The names of the parameter groups.
--
-- /Note:/ Consider using 'parameterGroupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgParameterGroupNames :: Lens.Lens' DescribeParameterGroups (Lude.Maybe [Lude.Text])
dpgParameterGroupNames = Lens.lens (parameterGroupNames :: DescribeParameterGroups -> Lude.Maybe [Lude.Text]) (\s a -> s {parameterGroupNames = a} :: DescribeParameterGroups)
{-# DEPRECATED dpgParameterGroupNames "Use generic-lens or generic-optics with 'parameterGroupNames' instead." #-}

-- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
--
-- The value for @MaxResults@ must be between 20 and 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgMaxResults :: Lens.Lens' DescribeParameterGroups (Lude.Maybe Lude.Int)
dpgMaxResults = Lens.lens (maxResults :: DescribeParameterGroups -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeParameterGroups)
{-# DEPRECATED dpgMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeParameterGroups where
  page rq rs
    | Page.stop (rs Lens.^. dpgsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dpgsrsParameterGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dpgNextToken Lens..~ rs Lens.^. dpgsrsNextToken

instance Lude.AWSRequest DescribeParameterGroups where
  type Rs DescribeParameterGroups = DescribeParameterGroupsResponse
  request = Req.postJSON daxService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeParameterGroupsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "ParameterGroups" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeParameterGroups where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDAXV3.DescribeParameterGroups" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeParameterGroups where
  toJSON DescribeParameterGroups' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("ParameterGroupNames" Lude..=) Lude.<$> parameterGroupNames,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeParameterGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeParameterGroups where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeParameterGroupsResponse' smart constructor.
data DescribeParameterGroupsResponse = DescribeParameterGroupsResponse'
  { -- | Provides an identifier to allow retrieval of paginated results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | An array of parameter groups. Each element in the array represents one parameter group.
    parameterGroups :: Lude.Maybe [ParameterGroup],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeParameterGroupsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Provides an identifier to allow retrieval of paginated results.
-- * 'parameterGroups' - An array of parameter groups. Each element in the array represents one parameter group.
-- * 'responseStatus' - The response status code.
mkDescribeParameterGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeParameterGroupsResponse
mkDescribeParameterGroupsResponse pResponseStatus_ =
  DescribeParameterGroupsResponse'
    { nextToken = Lude.Nothing,
      parameterGroups = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgsrsNextToken :: Lens.Lens' DescribeParameterGroupsResponse (Lude.Maybe Lude.Text)
dpgsrsNextToken = Lens.lens (nextToken :: DescribeParameterGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeParameterGroupsResponse)
{-# DEPRECATED dpgsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of parameter groups. Each element in the array represents one parameter group.
--
-- /Note:/ Consider using 'parameterGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgsrsParameterGroups :: Lens.Lens' DescribeParameterGroupsResponse (Lude.Maybe [ParameterGroup])
dpgsrsParameterGroups = Lens.lens (parameterGroups :: DescribeParameterGroupsResponse -> Lude.Maybe [ParameterGroup]) (\s a -> s {parameterGroups = a} :: DescribeParameterGroupsResponse)
{-# DEPRECATED dpgsrsParameterGroups "Use generic-lens or generic-optics with 'parameterGroups' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgsrsResponseStatus :: Lens.Lens' DescribeParameterGroupsResponse Lude.Int
dpgsrsResponseStatus = Lens.lens (responseStatus :: DescribeParameterGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeParameterGroupsResponse)
{-# DEPRECATED dpgsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

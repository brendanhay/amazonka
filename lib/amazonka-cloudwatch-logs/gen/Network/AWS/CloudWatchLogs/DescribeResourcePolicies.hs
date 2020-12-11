{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DescribeResourcePolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the resource policies in this account.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchLogs.DescribeResourcePolicies
  ( -- * Creating a request
    DescribeResourcePolicies (..),
    mkDescribeResourcePolicies,

    -- ** Request lenses
    drpNextToken,
    drpLimit,

    -- * Destructuring the response
    DescribeResourcePoliciesResponse (..),
    mkDescribeResourcePoliciesResponse,

    -- ** Response lenses
    drprsResourcePolicies,
    drprsNextToken,
    drprsResponseStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeResourcePolicies' smart constructor.
data DescribeResourcePolicies = DescribeResourcePolicies'
  { nextToken ::
      Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeResourcePolicies' with the minimum fields required to make a request.
--
-- * 'limit' - The maximum number of resource policies to be displayed with one call of this API.
-- * 'nextToken' - Undocumented field.
mkDescribeResourcePolicies ::
  DescribeResourcePolicies
mkDescribeResourcePolicies =
  DescribeResourcePolicies'
    { nextToken = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpNextToken :: Lens.Lens' DescribeResourcePolicies (Lude.Maybe Lude.Text)
drpNextToken = Lens.lens (nextToken :: DescribeResourcePolicies -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeResourcePolicies)
{-# DEPRECATED drpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of resource policies to be displayed with one call of this API.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpLimit :: Lens.Lens' DescribeResourcePolicies (Lude.Maybe Lude.Natural)
drpLimit = Lens.lens (limit :: DescribeResourcePolicies -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeResourcePolicies)
{-# DEPRECATED drpLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager DescribeResourcePolicies where
  page rq rs
    | Page.stop (rs Lens.^. drprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. drprsResourcePolicies) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& drpNextToken Lens..~ rs Lens.^. drprsNextToken

instance Lude.AWSRequest DescribeResourcePolicies where
  type Rs DescribeResourcePolicies = DescribeResourcePoliciesResponse
  request = Req.postJSON cloudWatchLogsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeResourcePoliciesResponse'
            Lude.<$> (x Lude..?> "resourcePolicies" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeResourcePolicies where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.DescribeResourcePolicies" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeResourcePolicies where
  toJSON DescribeResourcePolicies' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            ("limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath DescribeResourcePolicies where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeResourcePolicies where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeResourcePoliciesResponse' smart constructor.
data DescribeResourcePoliciesResponse = DescribeResourcePoliciesResponse'
  { resourcePolicies ::
      Lude.Maybe
        [ResourcePolicy],
    nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeResourcePoliciesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Undocumented field.
-- * 'resourcePolicies' - The resource policies that exist in this account.
-- * 'responseStatus' - The response status code.
mkDescribeResourcePoliciesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeResourcePoliciesResponse
mkDescribeResourcePoliciesResponse pResponseStatus_ =
  DescribeResourcePoliciesResponse'
    { resourcePolicies =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The resource policies that exist in this account.
--
-- /Note:/ Consider using 'resourcePolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprsResourcePolicies :: Lens.Lens' DescribeResourcePoliciesResponse (Lude.Maybe [ResourcePolicy])
drprsResourcePolicies = Lens.lens (resourcePolicies :: DescribeResourcePoliciesResponse -> Lude.Maybe [ResourcePolicy]) (\s a -> s {resourcePolicies = a} :: DescribeResourcePoliciesResponse)
{-# DEPRECATED drprsResourcePolicies "Use generic-lens or generic-optics with 'resourcePolicies' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprsNextToken :: Lens.Lens' DescribeResourcePoliciesResponse (Lude.Maybe Lude.Text)
drprsNextToken = Lens.lens (nextToken :: DescribeResourcePoliciesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeResourcePoliciesResponse)
{-# DEPRECATED drprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprsResponseStatus :: Lens.Lens' DescribeResourcePoliciesResponse Lude.Int
drprsResponseStatus = Lens.lens (responseStatus :: DescribeResourcePoliciesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeResourcePoliciesResponse)
{-# DEPRECATED drprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

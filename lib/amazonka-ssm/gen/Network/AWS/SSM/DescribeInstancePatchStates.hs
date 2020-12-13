{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeInstancePatchStates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the high-level patch state of one or more instances.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeInstancePatchStates
  ( -- * Creating a request
    DescribeInstancePatchStates (..),
    mkDescribeInstancePatchStates,

    -- ** Request lenses
    dipsNextToken,
    dipsInstanceIds,
    dipsMaxResults,

    -- * Destructuring the response
    DescribeInstancePatchStatesResponse (..),
    mkDescribeInstancePatchStatesResponse,

    -- ** Response lenses
    dipsrsNextToken,
    dipsrsInstancePatchStates,
    dipsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDescribeInstancePatchStates' smart constructor.
data DescribeInstancePatchStates = DescribeInstancePatchStates'
  { -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Lude.Maybe Lude.Text,
    -- | The ID of the instance whose patch state information should be retrieved.
    instanceIds :: [Lude.Text],
    -- | The maximum number of instances to return (per page).
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInstancePatchStates' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
-- * 'instanceIds' - The ID of the instance whose patch state information should be retrieved.
-- * 'maxResults' - The maximum number of instances to return (per page).
mkDescribeInstancePatchStates ::
  DescribeInstancePatchStates
mkDescribeInstancePatchStates =
  DescribeInstancePatchStates'
    { nextToken = Lude.Nothing,
      instanceIds = Lude.mempty,
      maxResults = Lude.Nothing
    }

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsNextToken :: Lens.Lens' DescribeInstancePatchStates (Lude.Maybe Lude.Text)
dipsNextToken = Lens.lens (nextToken :: DescribeInstancePatchStates -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeInstancePatchStates)
{-# DEPRECATED dipsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the instance whose patch state information should be retrieved.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsInstanceIds :: Lens.Lens' DescribeInstancePatchStates [Lude.Text]
dipsInstanceIds = Lens.lens (instanceIds :: DescribeInstancePatchStates -> [Lude.Text]) (\s a -> s {instanceIds = a} :: DescribeInstancePatchStates)
{-# DEPRECATED dipsInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

-- | The maximum number of instances to return (per page).
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsMaxResults :: Lens.Lens' DescribeInstancePatchStates (Lude.Maybe Lude.Natural)
dipsMaxResults = Lens.lens (maxResults :: DescribeInstancePatchStates -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeInstancePatchStates)
{-# DEPRECATED dipsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeInstancePatchStates where
  page rq rs
    | Page.stop (rs Lens.^. dipsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dipsrsInstancePatchStates) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dipsNextToken Lens..~ rs Lens.^. dipsrsNextToken

instance Lude.AWSRequest DescribeInstancePatchStates where
  type
    Rs DescribeInstancePatchStates =
      DescribeInstancePatchStatesResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeInstancePatchStatesResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "InstancePatchStates" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeInstancePatchStates where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.DescribeInstancePatchStates" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeInstancePatchStates where
  toJSON DescribeInstancePatchStates' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("InstanceIds" Lude..= instanceIds),
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeInstancePatchStates where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeInstancePatchStates where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeInstancePatchStatesResponse' smart constructor.
data DescribeInstancePatchStatesResponse = DescribeInstancePatchStatesResponse'
  { -- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The high-level patch state for the requested instances.
    instancePatchStates :: Lude.Maybe [InstancePatchState],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInstancePatchStatesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
-- * 'instancePatchStates' - The high-level patch state for the requested instances.
-- * 'responseStatus' - The response status code.
mkDescribeInstancePatchStatesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeInstancePatchStatesResponse
mkDescribeInstancePatchStatesResponse pResponseStatus_ =
  DescribeInstancePatchStatesResponse'
    { nextToken = Lude.Nothing,
      instancePatchStates = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsrsNextToken :: Lens.Lens' DescribeInstancePatchStatesResponse (Lude.Maybe Lude.Text)
dipsrsNextToken = Lens.lens (nextToken :: DescribeInstancePatchStatesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeInstancePatchStatesResponse)
{-# DEPRECATED dipsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The high-level patch state for the requested instances.
--
-- /Note:/ Consider using 'instancePatchStates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsrsInstancePatchStates :: Lens.Lens' DescribeInstancePatchStatesResponse (Lude.Maybe [InstancePatchState])
dipsrsInstancePatchStates = Lens.lens (instancePatchStates :: DescribeInstancePatchStatesResponse -> Lude.Maybe [InstancePatchState]) (\s a -> s {instancePatchStates = a} :: DescribeInstancePatchStatesResponse)
{-# DEPRECATED dipsrsInstancePatchStates "Use generic-lens or generic-optics with 'instancePatchStates' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsrsResponseStatus :: Lens.Lens' DescribeInstancePatchStatesResponse Lude.Int
dipsrsResponseStatus = Lens.lens (responseStatus :: DescribeInstancePatchStatesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeInstancePatchStatesResponse)
{-# DEPRECATED dipsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

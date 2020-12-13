{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeInstanceAssociationsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The status of the associations for the instance(s).
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeInstanceAssociationsStatus
  ( -- * Creating a request
    DescribeInstanceAssociationsStatus (..),
    mkDescribeInstanceAssociationsStatus,

    -- ** Request lenses
    diasInstanceId,
    diasNextToken,
    diasMaxResults,

    -- * Destructuring the response
    DescribeInstanceAssociationsStatusResponse (..),
    mkDescribeInstanceAssociationsStatusResponse,

    -- ** Response lenses
    diasrsInstanceAssociationStatusInfos,
    diasrsNextToken,
    diasrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDescribeInstanceAssociationsStatus' smart constructor.
data DescribeInstanceAssociationsStatus = DescribeInstanceAssociationsStatus'
  { -- | The instance IDs for which you want association status information.
    instanceId :: Lude.Text,
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInstanceAssociationsStatus' with the minimum fields required to make a request.
--
-- * 'instanceId' - The instance IDs for which you want association status information.
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
-- * 'maxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
mkDescribeInstanceAssociationsStatus ::
  -- | 'instanceId'
  Lude.Text ->
  DescribeInstanceAssociationsStatus
mkDescribeInstanceAssociationsStatus pInstanceId_ =
  DescribeInstanceAssociationsStatus'
    { instanceId = pInstanceId_,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The instance IDs for which you want association status information.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diasInstanceId :: Lens.Lens' DescribeInstanceAssociationsStatus Lude.Text
diasInstanceId = Lens.lens (instanceId :: DescribeInstanceAssociationsStatus -> Lude.Text) (\s a -> s {instanceId = a} :: DescribeInstanceAssociationsStatus)
{-# DEPRECATED diasInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diasNextToken :: Lens.Lens' DescribeInstanceAssociationsStatus (Lude.Maybe Lude.Text)
diasNextToken = Lens.lens (nextToken :: DescribeInstanceAssociationsStatus -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeInstanceAssociationsStatus)
{-# DEPRECATED diasNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diasMaxResults :: Lens.Lens' DescribeInstanceAssociationsStatus (Lude.Maybe Lude.Natural)
diasMaxResults = Lens.lens (maxResults :: DescribeInstanceAssociationsStatus -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeInstanceAssociationsStatus)
{-# DEPRECATED diasMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeInstanceAssociationsStatus where
  page rq rs
    | Page.stop (rs Lens.^. diasrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. diasrsInstanceAssociationStatusInfos) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& diasNextToken Lens..~ rs Lens.^. diasrsNextToken

instance Lude.AWSRequest DescribeInstanceAssociationsStatus where
  type
    Rs DescribeInstanceAssociationsStatus =
      DescribeInstanceAssociationsStatusResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeInstanceAssociationsStatusResponse'
            Lude.<$> (x Lude..?> "InstanceAssociationStatusInfos" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeInstanceAssociationsStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonSSM.DescribeInstanceAssociationsStatus" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeInstanceAssociationsStatus where
  toJSON DescribeInstanceAssociationsStatus' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("InstanceId" Lude..= instanceId),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeInstanceAssociationsStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeInstanceAssociationsStatus where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeInstanceAssociationsStatusResponse' smart constructor.
data DescribeInstanceAssociationsStatusResponse = DescribeInstanceAssociationsStatusResponse'
  { -- | Status information about the association.
    instanceAssociationStatusInfos :: Lude.Maybe [InstanceAssociationStatusInfo],
    -- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInstanceAssociationsStatusResponse' with the minimum fields required to make a request.
--
-- * 'instanceAssociationStatusInfos' - Status information about the association.
-- * 'nextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
-- * 'responseStatus' - The response status code.
mkDescribeInstanceAssociationsStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeInstanceAssociationsStatusResponse
mkDescribeInstanceAssociationsStatusResponse pResponseStatus_ =
  DescribeInstanceAssociationsStatusResponse'
    { instanceAssociationStatusInfos =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Status information about the association.
--
-- /Note:/ Consider using 'instanceAssociationStatusInfos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diasrsInstanceAssociationStatusInfos :: Lens.Lens' DescribeInstanceAssociationsStatusResponse (Lude.Maybe [InstanceAssociationStatusInfo])
diasrsInstanceAssociationStatusInfos = Lens.lens (instanceAssociationStatusInfos :: DescribeInstanceAssociationsStatusResponse -> Lude.Maybe [InstanceAssociationStatusInfo]) (\s a -> s {instanceAssociationStatusInfos = a} :: DescribeInstanceAssociationsStatusResponse)
{-# DEPRECATED diasrsInstanceAssociationStatusInfos "Use generic-lens or generic-optics with 'instanceAssociationStatusInfos' instead." #-}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diasrsNextToken :: Lens.Lens' DescribeInstanceAssociationsStatusResponse (Lude.Maybe Lude.Text)
diasrsNextToken = Lens.lens (nextToken :: DescribeInstanceAssociationsStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeInstanceAssociationsStatusResponse)
{-# DEPRECATED diasrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diasrsResponseStatus :: Lens.Lens' DescribeInstanceAssociationsStatusResponse Lude.Int
diasrsResponseStatus = Lens.lens (responseStatus :: DescribeInstanceAssociationsStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeInstanceAssociationsStatusResponse)
{-# DEPRECATED diasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

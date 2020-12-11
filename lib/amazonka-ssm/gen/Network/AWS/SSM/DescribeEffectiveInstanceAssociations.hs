{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeEffectiveInstanceAssociations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- All associations for the instance(s).
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeEffectiveInstanceAssociations
  ( -- * Creating a request
    DescribeEffectiveInstanceAssociations (..),
    mkDescribeEffectiveInstanceAssociations,

    -- ** Request lenses
    deiaNextToken,
    deiaMaxResults,
    deiaInstanceId,

    -- * Destructuring the response
    DescribeEffectiveInstanceAssociationsResponse (..),
    mkDescribeEffectiveInstanceAssociationsResponse,

    -- ** Response lenses
    deiarsNextToken,
    deiarsAssociations,
    deiarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDescribeEffectiveInstanceAssociations' smart constructor.
data DescribeEffectiveInstanceAssociations = DescribeEffectiveInstanceAssociations'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    maxResults ::
      Lude.Maybe
        Lude.Natural,
    instanceId ::
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

-- | Creates a value of 'DescribeEffectiveInstanceAssociations' with the minimum fields required to make a request.
--
-- * 'instanceId' - The instance ID for which you want to view all associations.
-- * 'maxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
mkDescribeEffectiveInstanceAssociations ::
  -- | 'instanceId'
  Lude.Text ->
  DescribeEffectiveInstanceAssociations
mkDescribeEffectiveInstanceAssociations pInstanceId_ =
  DescribeEffectiveInstanceAssociations'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      instanceId = pInstanceId_
    }

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deiaNextToken :: Lens.Lens' DescribeEffectiveInstanceAssociations (Lude.Maybe Lude.Text)
deiaNextToken = Lens.lens (nextToken :: DescribeEffectiveInstanceAssociations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeEffectiveInstanceAssociations)
{-# DEPRECATED deiaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deiaMaxResults :: Lens.Lens' DescribeEffectiveInstanceAssociations (Lude.Maybe Lude.Natural)
deiaMaxResults = Lens.lens (maxResults :: DescribeEffectiveInstanceAssociations -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeEffectiveInstanceAssociations)
{-# DEPRECATED deiaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The instance ID for which you want to view all associations.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deiaInstanceId :: Lens.Lens' DescribeEffectiveInstanceAssociations Lude.Text
deiaInstanceId = Lens.lens (instanceId :: DescribeEffectiveInstanceAssociations -> Lude.Text) (\s a -> s {instanceId = a} :: DescribeEffectiveInstanceAssociations)
{-# DEPRECATED deiaInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Page.AWSPager DescribeEffectiveInstanceAssociations where
  page rq rs
    | Page.stop (rs Lens.^. deiarsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. deiarsAssociations) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& deiaNextToken Lens..~ rs Lens.^. deiarsNextToken

instance Lude.AWSRequest DescribeEffectiveInstanceAssociations where
  type
    Rs DescribeEffectiveInstanceAssociations =
      DescribeEffectiveInstanceAssociationsResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeEffectiveInstanceAssociationsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Associations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEffectiveInstanceAssociations where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonSSM.DescribeEffectiveInstanceAssociations" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeEffectiveInstanceAssociations where
  toJSON DescribeEffectiveInstanceAssociations' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("InstanceId" Lude..= instanceId)
          ]
      )

instance Lude.ToPath DescribeEffectiveInstanceAssociations where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEffectiveInstanceAssociations where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeEffectiveInstanceAssociationsResponse' smart constructor.
data DescribeEffectiveInstanceAssociationsResponse = DescribeEffectiveInstanceAssociationsResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    associations ::
      Lude.Maybe
        [InstanceAssociation],
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
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'DescribeEffectiveInstanceAssociationsResponse' with the minimum fields required to make a request.
--
-- * 'associations' - The associations for the requested instance.
-- * 'nextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
-- * 'responseStatus' - The response status code.
mkDescribeEffectiveInstanceAssociationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEffectiveInstanceAssociationsResponse
mkDescribeEffectiveInstanceAssociationsResponse pResponseStatus_ =
  DescribeEffectiveInstanceAssociationsResponse'
    { nextToken =
        Lude.Nothing,
      associations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deiarsNextToken :: Lens.Lens' DescribeEffectiveInstanceAssociationsResponse (Lude.Maybe Lude.Text)
deiarsNextToken = Lens.lens (nextToken :: DescribeEffectiveInstanceAssociationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeEffectiveInstanceAssociationsResponse)
{-# DEPRECATED deiarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The associations for the requested instance.
--
-- /Note:/ Consider using 'associations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deiarsAssociations :: Lens.Lens' DescribeEffectiveInstanceAssociationsResponse (Lude.Maybe [InstanceAssociation])
deiarsAssociations = Lens.lens (associations :: DescribeEffectiveInstanceAssociationsResponse -> Lude.Maybe [InstanceAssociation]) (\s a -> s {associations = a} :: DescribeEffectiveInstanceAssociationsResponse)
{-# DEPRECATED deiarsAssociations "Use generic-lens or generic-optics with 'associations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deiarsResponseStatus :: Lens.Lens' DescribeEffectiveInstanceAssociationsResponse Lude.Int
deiarsResponseStatus = Lens.lens (responseStatus :: DescribeEffectiveInstanceAssociationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEffectiveInstanceAssociationsResponse)
{-# DEPRECATED deiarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

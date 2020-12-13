{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.ListTopics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the requester's topics. Each call returns a limited list of topics, up to 100. If there are more topics, a @NextToken@ is also returned. Use the @NextToken@ parameter in a new @ListTopics@ call to get further results.
--
-- This action is throttled at 30 transactions per second (TPS).
--
-- This operation returns paginated results.
module Network.AWS.SNS.ListTopics
  ( -- * Creating a request
    ListTopics (..),
    mkListTopics,

    -- ** Request lenses
    ltNextToken,

    -- * Destructuring the response
    ListTopicsResponse (..),
    mkListTopicsResponse,

    -- ** Response lenses
    ltrsTopics,
    ltrsNextToken,
    ltrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SNS.Types

-- | /See:/ 'mkListTopics' smart constructor.
newtype ListTopics = ListTopics'
  { -- | Token returned by the previous @ListTopics@ request.
    nextToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTopics' with the minimum fields required to make a request.
--
-- * 'nextToken' - Token returned by the previous @ListTopics@ request.
mkListTopics ::
  ListTopics
mkListTopics = ListTopics' {nextToken = Lude.Nothing}

-- | Token returned by the previous @ListTopics@ request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltNextToken :: Lens.Lens' ListTopics (Lude.Maybe Lude.Text)
ltNextToken = Lens.lens (nextToken :: ListTopics -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTopics)
{-# DEPRECATED ltNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager ListTopics where
  page rq rs
    | Page.stop (rs Lens.^. ltrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltrsTopics) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltNextToken Lens..~ rs Lens.^. ltrsNextToken

instance Lude.AWSRequest ListTopics where
  type Rs ListTopics = ListTopicsResponse
  request = Req.postQuery snsService
  response =
    Res.receiveXMLWrapper
      "ListTopicsResult"
      ( \s h x ->
          ListTopicsResponse'
            Lude.<$> ( x Lude..@? "Topics" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTopics where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListTopics where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTopics where
  toQuery ListTopics' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListTopics" :: Lude.ByteString),
        "Version" Lude.=: ("2010-03-31" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken
      ]

-- | Response for ListTopics action.
--
-- /See:/ 'mkListTopicsResponse' smart constructor.
data ListTopicsResponse = ListTopicsResponse'
  { -- | A list of topic ARNs.
    topics :: Lude.Maybe [Topic],
    -- | Token to pass along to the next @ListTopics@ request. This element is returned if there are additional topics to retrieve.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTopicsResponse' with the minimum fields required to make a request.
--
-- * 'topics' - A list of topic ARNs.
-- * 'nextToken' - Token to pass along to the next @ListTopics@ request. This element is returned if there are additional topics to retrieve.
-- * 'responseStatus' - The response status code.
mkListTopicsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTopicsResponse
mkListTopicsResponse pResponseStatus_ =
  ListTopicsResponse'
    { topics = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of topic ARNs.
--
-- /Note:/ Consider using 'topics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsTopics :: Lens.Lens' ListTopicsResponse (Lude.Maybe [Topic])
ltrsTopics = Lens.lens (topics :: ListTopicsResponse -> Lude.Maybe [Topic]) (\s a -> s {topics = a} :: ListTopicsResponse)
{-# DEPRECATED ltrsTopics "Use generic-lens or generic-optics with 'topics' instead." #-}

-- | Token to pass along to the next @ListTopics@ request. This element is returned if there are additional topics to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsNextToken :: Lens.Lens' ListTopicsResponse (Lude.Maybe Lude.Text)
ltrsNextToken = Lens.lens (nextToken :: ListTopicsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTopicsResponse)
{-# DEPRECATED ltrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsResponseStatus :: Lens.Lens' ListTopicsResponse Lude.Int
ltrsResponseStatus = Lens.lens (responseStatus :: ListTopicsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTopicsResponse)
{-# DEPRECATED ltrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

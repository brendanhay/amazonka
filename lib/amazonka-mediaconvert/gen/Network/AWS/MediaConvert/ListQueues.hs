{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.ListQueues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a JSON array of up to twenty of your queues. This will return the queues themselves, not just a list of them. To retrieve the next twenty queues, use the nextToken string returned with the array.
--
-- This operation returns paginated results.
module Network.AWS.MediaConvert.ListQueues
  ( -- * Creating a request
    ListQueues (..),
    mkListQueues,

    -- ** Request lenses
    lqListBy,
    lqNextToken,
    lqOrder,
    lqMaxResults,

    -- * Destructuring the response
    ListQueuesResponse (..),
    mkListQueuesResponse,

    -- ** Response lenses
    lqrsQueues,
    lqrsNextToken,
    lqrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListQueues' smart constructor.
data ListQueues = ListQueues'
  { -- | Optional. When you request a list of queues, you can choose to list them alphabetically by NAME or chronologically by CREATION_DATE. If you don't specify, the service will list them by creation date.
    listBy :: Lude.Maybe QueueListBy,
    -- | Use this string, provided with the response to a previous request, to request the next batch of queues.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Optional. When you request lists of resources, you can specify whether they are sorted in ASCENDING or DESCENDING order. Default varies by resource.
    order :: Lude.Maybe Order,
    -- | Optional. Number of queues, up to twenty, that will be returned at one time.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListQueues' with the minimum fields required to make a request.
--
-- * 'listBy' - Optional. When you request a list of queues, you can choose to list them alphabetically by NAME or chronologically by CREATION_DATE. If you don't specify, the service will list them by creation date.
-- * 'nextToken' - Use this string, provided with the response to a previous request, to request the next batch of queues.
-- * 'order' - Optional. When you request lists of resources, you can specify whether they are sorted in ASCENDING or DESCENDING order. Default varies by resource.
-- * 'maxResults' - Optional. Number of queues, up to twenty, that will be returned at one time.
mkListQueues ::
  ListQueues
mkListQueues =
  ListQueues'
    { listBy = Lude.Nothing,
      nextToken = Lude.Nothing,
      order = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Optional. When you request a list of queues, you can choose to list them alphabetically by NAME or chronologically by CREATION_DATE. If you don't specify, the service will list them by creation date.
--
-- /Note:/ Consider using 'listBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqListBy :: Lens.Lens' ListQueues (Lude.Maybe QueueListBy)
lqListBy = Lens.lens (listBy :: ListQueues -> Lude.Maybe QueueListBy) (\s a -> s {listBy = a} :: ListQueues)
{-# DEPRECATED lqListBy "Use generic-lens or generic-optics with 'listBy' instead." #-}

-- | Use this string, provided with the response to a previous request, to request the next batch of queues.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqNextToken :: Lens.Lens' ListQueues (Lude.Maybe Lude.Text)
lqNextToken = Lens.lens (nextToken :: ListQueues -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListQueues)
{-# DEPRECATED lqNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Optional. When you request lists of resources, you can specify whether they are sorted in ASCENDING or DESCENDING order. Default varies by resource.
--
-- /Note:/ Consider using 'order' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqOrder :: Lens.Lens' ListQueues (Lude.Maybe Order)
lqOrder = Lens.lens (order :: ListQueues -> Lude.Maybe Order) (\s a -> s {order = a} :: ListQueues)
{-# DEPRECATED lqOrder "Use generic-lens or generic-optics with 'order' instead." #-}

-- | Optional. Number of queues, up to twenty, that will be returned at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqMaxResults :: Lens.Lens' ListQueues (Lude.Maybe Lude.Natural)
lqMaxResults = Lens.lens (maxResults :: ListQueues -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListQueues)
{-# DEPRECATED lqMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListQueues where
  page rq rs
    | Page.stop (rs Lens.^. lqrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lqrsQueues) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lqNextToken Lens..~ rs Lens.^. lqrsNextToken

instance Lude.AWSRequest ListQueues where
  type Rs ListQueues = ListQueuesResponse
  request = Req.get mediaConvertService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListQueuesResponse'
            Lude.<$> (x Lude..?> "queues" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListQueues where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListQueues where
  toPath = Lude.const "/2017-08-29/queues"

instance Lude.ToQuery ListQueues where
  toQuery ListQueues' {..} =
    Lude.mconcat
      [ "listBy" Lude.=: listBy,
        "nextToken" Lude.=: nextToken,
        "order" Lude.=: order,
        "maxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkListQueuesResponse' smart constructor.
data ListQueuesResponse = ListQueuesResponse'
  { -- | List of queues.
    queues :: Lude.Maybe [Queue],
    -- | Use this string to request the next batch of queues.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListQueuesResponse' with the minimum fields required to make a request.
--
-- * 'queues' - List of queues.
-- * 'nextToken' - Use this string to request the next batch of queues.
-- * 'responseStatus' - The response status code.
mkListQueuesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListQueuesResponse
mkListQueuesResponse pResponseStatus_ =
  ListQueuesResponse'
    { queues = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | List of queues.
--
-- /Note:/ Consider using 'queues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqrsQueues :: Lens.Lens' ListQueuesResponse (Lude.Maybe [Queue])
lqrsQueues = Lens.lens (queues :: ListQueuesResponse -> Lude.Maybe [Queue]) (\s a -> s {queues = a} :: ListQueuesResponse)
{-# DEPRECATED lqrsQueues "Use generic-lens or generic-optics with 'queues' instead." #-}

-- | Use this string to request the next batch of queues.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqrsNextToken :: Lens.Lens' ListQueuesResponse (Lude.Maybe Lude.Text)
lqrsNextToken = Lens.lens (nextToken :: ListQueuesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListQueuesResponse)
{-# DEPRECATED lqrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqrsResponseStatus :: Lens.Lens' ListQueuesResponse Lude.Int
lqrsResponseStatus = Lens.lens (responseStatus :: ListQueuesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListQueuesResponse)
{-# DEPRECATED lqrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

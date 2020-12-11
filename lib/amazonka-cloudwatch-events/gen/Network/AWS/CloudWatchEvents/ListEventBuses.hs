{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.ListEventBuses
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the event buses in your account, including the default event bus, custom event buses, and partner event buses.
module Network.AWS.CloudWatchEvents.ListEventBuses
  ( -- * Creating a request
    ListEventBuses (..),
    mkListEventBuses,

    -- ** Request lenses
    lebNextToken,
    lebNamePrefix,
    lebLimit,

    -- * Destructuring the response
    ListEventBusesResponse (..),
    mkListEventBusesResponse,

    -- ** Response lenses
    lebrsEventBuses,
    lebrsNextToken,
    lebrsResponseStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListEventBuses' smart constructor.
data ListEventBuses = ListEventBuses'
  { nextToken ::
      Lude.Maybe Lude.Text,
    namePrefix :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListEventBuses' with the minimum fields required to make a request.
--
-- * 'limit' - Specifying this limits the number of results returned by this operation. The operation also returns a NextToken which you can use in a subsequent operation to retrieve the next set of results.
-- * 'namePrefix' - Specifying this limits the results to only those event buses with names that start with the specified prefix.
-- * 'nextToken' - The token returned by a previous call to retrieve the next set of results.
mkListEventBuses ::
  ListEventBuses
mkListEventBuses =
  ListEventBuses'
    { nextToken = Lude.Nothing,
      namePrefix = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | The token returned by a previous call to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lebNextToken :: Lens.Lens' ListEventBuses (Lude.Maybe Lude.Text)
lebNextToken = Lens.lens (nextToken :: ListEventBuses -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListEventBuses)
{-# DEPRECATED lebNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Specifying this limits the results to only those event buses with names that start with the specified prefix.
--
-- /Note:/ Consider using 'namePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lebNamePrefix :: Lens.Lens' ListEventBuses (Lude.Maybe Lude.Text)
lebNamePrefix = Lens.lens (namePrefix :: ListEventBuses -> Lude.Maybe Lude.Text) (\s a -> s {namePrefix = a} :: ListEventBuses)
{-# DEPRECATED lebNamePrefix "Use generic-lens or generic-optics with 'namePrefix' instead." #-}

-- | Specifying this limits the number of results returned by this operation. The operation also returns a NextToken which you can use in a subsequent operation to retrieve the next set of results.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lebLimit :: Lens.Lens' ListEventBuses (Lude.Maybe Lude.Natural)
lebLimit = Lens.lens (limit :: ListEventBuses -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListEventBuses)
{-# DEPRECATED lebLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Lude.AWSRequest ListEventBuses where
  type Rs ListEventBuses = ListEventBusesResponse
  request = Req.postJSON cloudWatchEventsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListEventBusesResponse'
            Lude.<$> (x Lude..?> "EventBuses" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListEventBuses where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSEvents.ListEventBuses" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListEventBuses where
  toJSON ListEventBuses' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("NamePrefix" Lude..=) Lude.<$> namePrefix,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListEventBuses where
  toPath = Lude.const "/"

instance Lude.ToQuery ListEventBuses where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListEventBusesResponse' smart constructor.
data ListEventBusesResponse = ListEventBusesResponse'
  { eventBuses ::
      Lude.Maybe [EventBus],
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

-- | Creates a value of 'ListEventBusesResponse' with the minimum fields required to make a request.
--
-- * 'eventBuses' - This list of event buses.
-- * 'nextToken' - A token you can use in a subsequent operation to retrieve the next set of results.
-- * 'responseStatus' - The response status code.
mkListEventBusesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListEventBusesResponse
mkListEventBusesResponse pResponseStatus_ =
  ListEventBusesResponse'
    { eventBuses = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | This list of event buses.
--
-- /Note:/ Consider using 'eventBuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lebrsEventBuses :: Lens.Lens' ListEventBusesResponse (Lude.Maybe [EventBus])
lebrsEventBuses = Lens.lens (eventBuses :: ListEventBusesResponse -> Lude.Maybe [EventBus]) (\s a -> s {eventBuses = a} :: ListEventBusesResponse)
{-# DEPRECATED lebrsEventBuses "Use generic-lens or generic-optics with 'eventBuses' instead." #-}

-- | A token you can use in a subsequent operation to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lebrsNextToken :: Lens.Lens' ListEventBusesResponse (Lude.Maybe Lude.Text)
lebrsNextToken = Lens.lens (nextToken :: ListEventBusesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListEventBusesResponse)
{-# DEPRECATED lebrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lebrsResponseStatus :: Lens.Lens' ListEventBusesResponse Lude.Int
lebrsResponseStatus = Lens.lens (responseStatus :: ListEventBusesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListEventBusesResponse)
{-# DEPRECATED lebrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

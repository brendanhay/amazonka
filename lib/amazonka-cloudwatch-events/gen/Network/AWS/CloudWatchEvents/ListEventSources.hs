{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.ListEventSources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You can use this to see all the partner event sources that have been shared with your AWS account. For more information about partner event sources, see 'CreateEventBus' .
module Network.AWS.CloudWatchEvents.ListEventSources
  ( -- * Creating a request
    ListEventSources (..),
    mkListEventSources,

    -- ** Request lenses
    lesNextToken,
    lesNamePrefix,
    lesLimit,

    -- * Destructuring the response
    ListEventSourcesResponse (..),
    mkListEventSourcesResponse,

    -- ** Response lenses
    lesrsNextToken,
    lesrsEventSources,
    lesrsResponseStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListEventSources' smart constructor.
data ListEventSources = ListEventSources'
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

-- | Creates a value of 'ListEventSources' with the minimum fields required to make a request.
--
-- * 'limit' - Specifying this limits the number of results returned by this operation. The operation also returns a NextToken which you can use in a subsequent operation to retrieve the next set of results.
-- * 'namePrefix' - Specifying this limits the results to only those partner event sources with names that start with the specified prefix.
-- * 'nextToken' - The token returned by a previous call to retrieve the next set of results.
mkListEventSources ::
  ListEventSources
mkListEventSources =
  ListEventSources'
    { nextToken = Lude.Nothing,
      namePrefix = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | The token returned by a previous call to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesNextToken :: Lens.Lens' ListEventSources (Lude.Maybe Lude.Text)
lesNextToken = Lens.lens (nextToken :: ListEventSources -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListEventSources)
{-# DEPRECATED lesNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Specifying this limits the results to only those partner event sources with names that start with the specified prefix.
--
-- /Note:/ Consider using 'namePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesNamePrefix :: Lens.Lens' ListEventSources (Lude.Maybe Lude.Text)
lesNamePrefix = Lens.lens (namePrefix :: ListEventSources -> Lude.Maybe Lude.Text) (\s a -> s {namePrefix = a} :: ListEventSources)
{-# DEPRECATED lesNamePrefix "Use generic-lens or generic-optics with 'namePrefix' instead." #-}

-- | Specifying this limits the number of results returned by this operation. The operation also returns a NextToken which you can use in a subsequent operation to retrieve the next set of results.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesLimit :: Lens.Lens' ListEventSources (Lude.Maybe Lude.Natural)
lesLimit = Lens.lens (limit :: ListEventSources -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListEventSources)
{-# DEPRECATED lesLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Lude.AWSRequest ListEventSources where
  type Rs ListEventSources = ListEventSourcesResponse
  request = Req.postJSON cloudWatchEventsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListEventSourcesResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "EventSources" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListEventSources where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSEvents.ListEventSources" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListEventSources where
  toJSON ListEventSources' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("NamePrefix" Lude..=) Lude.<$> namePrefix,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListEventSources where
  toPath = Lude.const "/"

instance Lude.ToQuery ListEventSources where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListEventSourcesResponse' smart constructor.
data ListEventSourcesResponse = ListEventSourcesResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    eventSources :: Lude.Maybe [EventSource],
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

-- | Creates a value of 'ListEventSourcesResponse' with the minimum fields required to make a request.
--
-- * 'eventSources' - The list of event sources.
-- * 'nextToken' - A token you can use in a subsequent operation to retrieve the next set of results.
-- * 'responseStatus' - The response status code.
mkListEventSourcesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListEventSourcesResponse
mkListEventSourcesResponse pResponseStatus_ =
  ListEventSourcesResponse'
    { nextToken = Lude.Nothing,
      eventSources = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A token you can use in a subsequent operation to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesrsNextToken :: Lens.Lens' ListEventSourcesResponse (Lude.Maybe Lude.Text)
lesrsNextToken = Lens.lens (nextToken :: ListEventSourcesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListEventSourcesResponse)
{-# DEPRECATED lesrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of event sources.
--
-- /Note:/ Consider using 'eventSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesrsEventSources :: Lens.Lens' ListEventSourcesResponse (Lude.Maybe [EventSource])
lesrsEventSources = Lens.lens (eventSources :: ListEventSourcesResponse -> Lude.Maybe [EventSource]) (\s a -> s {eventSources = a} :: ListEventSourcesResponse)
{-# DEPRECATED lesrsEventSources "Use generic-lens or generic-optics with 'eventSources' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesrsResponseStatus :: Lens.Lens' ListEventSourcesResponse Lude.Int
lesrsResponseStatus = Lens.lens (responseStatus :: ListEventSourcesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListEventSourcesResponse)
{-# DEPRECATED lesrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

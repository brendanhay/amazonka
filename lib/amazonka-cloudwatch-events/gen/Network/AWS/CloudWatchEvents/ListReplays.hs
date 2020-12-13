{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.ListReplays
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your replays. You can either list all the replays or you can provide a prefix to match to the replay names. Filter parameters are exclusive.
module Network.AWS.CloudWatchEvents.ListReplays
  ( -- * Creating a request
    ListReplays (..),
    mkListReplays,

    -- ** Request lenses
    lrEventSourceARN,
    lrState,
    lrNextToken,
    lrNamePrefix,
    lrLimit,

    -- * Destructuring the response
    ListReplaysResponse (..),
    mkListReplaysResponse,

    -- ** Response lenses
    lrsReplays,
    lrsNextToken,
    lrsResponseStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListReplays' smart constructor.
data ListReplays = ListReplays'
  { -- | The ARN of the event source associated with the replay.
    eventSourceARN :: Lude.Maybe Lude.Text,
    -- | The state of the replay.
    state :: Lude.Maybe ReplayState,
    -- | The token returned by a previous call to retrieve the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A name prefix to filter the replays returned. Only replays with name that match the prefix are returned.
    namePrefix :: Lude.Maybe Lude.Text,
    -- | The maximum number of replays to retrieve.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListReplays' with the minimum fields required to make a request.
--
-- * 'eventSourceARN' - The ARN of the event source associated with the replay.
-- * 'state' - The state of the replay.
-- * 'nextToken' - The token returned by a previous call to retrieve the next set of results.
-- * 'namePrefix' - A name prefix to filter the replays returned. Only replays with name that match the prefix are returned.
-- * 'limit' - The maximum number of replays to retrieve.
mkListReplays ::
  ListReplays
mkListReplays =
  ListReplays'
    { eventSourceARN = Lude.Nothing,
      state = Lude.Nothing,
      nextToken = Lude.Nothing,
      namePrefix = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | The ARN of the event source associated with the replay.
--
-- /Note:/ Consider using 'eventSourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrEventSourceARN :: Lens.Lens' ListReplays (Lude.Maybe Lude.Text)
lrEventSourceARN = Lens.lens (eventSourceARN :: ListReplays -> Lude.Maybe Lude.Text) (\s a -> s {eventSourceARN = a} :: ListReplays)
{-# DEPRECATED lrEventSourceARN "Use generic-lens or generic-optics with 'eventSourceARN' instead." #-}

-- | The state of the replay.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrState :: Lens.Lens' ListReplays (Lude.Maybe ReplayState)
lrState = Lens.lens (state :: ListReplays -> Lude.Maybe ReplayState) (\s a -> s {state = a} :: ListReplays)
{-# DEPRECATED lrState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The token returned by a previous call to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrNextToken :: Lens.Lens' ListReplays (Lude.Maybe Lude.Text)
lrNextToken = Lens.lens (nextToken :: ListReplays -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListReplays)
{-# DEPRECATED lrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A name prefix to filter the replays returned. Only replays with name that match the prefix are returned.
--
-- /Note:/ Consider using 'namePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrNamePrefix :: Lens.Lens' ListReplays (Lude.Maybe Lude.Text)
lrNamePrefix = Lens.lens (namePrefix :: ListReplays -> Lude.Maybe Lude.Text) (\s a -> s {namePrefix = a} :: ListReplays)
{-# DEPRECATED lrNamePrefix "Use generic-lens or generic-optics with 'namePrefix' instead." #-}

-- | The maximum number of replays to retrieve.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrLimit :: Lens.Lens' ListReplays (Lude.Maybe Lude.Natural)
lrLimit = Lens.lens (limit :: ListReplays -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListReplays)
{-# DEPRECATED lrLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Lude.AWSRequest ListReplays where
  type Rs ListReplays = ListReplaysResponse
  request = Req.postJSON cloudWatchEventsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListReplaysResponse'
            Lude.<$> (x Lude..?> "Replays" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListReplays where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSEvents.ListReplays" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListReplays where
  toJSON ListReplays' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EventSourceArn" Lude..=) Lude.<$> eventSourceARN,
            ("State" Lude..=) Lude.<$> state,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("NamePrefix" Lude..=) Lude.<$> namePrefix,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListReplays where
  toPath = Lude.const "/"

instance Lude.ToQuery ListReplays where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListReplaysResponse' smart constructor.
data ListReplaysResponse = ListReplaysResponse'
  { -- | An array of @Replay@ objects that contain information about the replay.
    replays :: Lude.Maybe [Replay],
    -- | The token returned by a previous call to retrieve the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListReplaysResponse' with the minimum fields required to make a request.
--
-- * 'replays' - An array of @Replay@ objects that contain information about the replay.
-- * 'nextToken' - The token returned by a previous call to retrieve the next set of results.
-- * 'responseStatus' - The response status code.
mkListReplaysResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListReplaysResponse
mkListReplaysResponse pResponseStatus_ =
  ListReplaysResponse'
    { replays = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of @Replay@ objects that contain information about the replay.
--
-- /Note:/ Consider using 'replays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsReplays :: Lens.Lens' ListReplaysResponse (Lude.Maybe [Replay])
lrsReplays = Lens.lens (replays :: ListReplaysResponse -> Lude.Maybe [Replay]) (\s a -> s {replays = a} :: ListReplaysResponse)
{-# DEPRECATED lrsReplays "Use generic-lens or generic-optics with 'replays' instead." #-}

-- | The token returned by a previous call to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextToken :: Lens.Lens' ListReplaysResponse (Lude.Maybe Lude.Text)
lrsNextToken = Lens.lens (nextToken :: ListReplaysResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListReplaysResponse)
{-# DEPRECATED lrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListReplaysResponse Lude.Int
lrsResponseStatus = Lens.lens (responseStatus :: ListReplaysResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListReplaysResponse)
{-# DEPRECATED lrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

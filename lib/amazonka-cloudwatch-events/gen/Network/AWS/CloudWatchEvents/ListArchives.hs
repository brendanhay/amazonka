{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.ListArchives
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your archives. You can either list all the archives or you can provide a prefix to match to the archive names. Filter parameters are exclusive.
module Network.AWS.CloudWatchEvents.ListArchives
  ( -- * Creating a request
    ListArchives (..),
    mkListArchives,

    -- ** Request lenses
    laEventSourceARN,
    laState,
    laNextToken,
    laNamePrefix,
    laLimit,

    -- * Destructuring the response
    ListArchivesResponse (..),
    mkListArchivesResponse,

    -- ** Response lenses
    larsArchives,
    larsNextToken,
    larsResponseStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListArchives' smart constructor.
data ListArchives = ListArchives'
  { eventSourceARN ::
      Lude.Maybe Lude.Text,
    state :: Lude.Maybe ArchiveState,
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListArchives' with the minimum fields required to make a request.
--
-- * 'eventSourceARN' - The ARN of the event source associated with the archive.
-- * 'limit' - The maximum number of results to return.
-- * 'namePrefix' - A name prefix to filter the archives returned. Only archives with name that match the prefix are returned.
-- * 'nextToken' - The token returned by a previous call to retrieve the next set of results.
-- * 'state' - The state of the archive.
mkListArchives ::
  ListArchives
mkListArchives =
  ListArchives'
    { eventSourceARN = Lude.Nothing,
      state = Lude.Nothing,
      nextToken = Lude.Nothing,
      namePrefix = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | The ARN of the event source associated with the archive.
--
-- /Note:/ Consider using 'eventSourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laEventSourceARN :: Lens.Lens' ListArchives (Lude.Maybe Lude.Text)
laEventSourceARN = Lens.lens (eventSourceARN :: ListArchives -> Lude.Maybe Lude.Text) (\s a -> s {eventSourceARN = a} :: ListArchives)
{-# DEPRECATED laEventSourceARN "Use generic-lens or generic-optics with 'eventSourceARN' instead." #-}

-- | The state of the archive.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laState :: Lens.Lens' ListArchives (Lude.Maybe ArchiveState)
laState = Lens.lens (state :: ListArchives -> Lude.Maybe ArchiveState) (\s a -> s {state = a} :: ListArchives)
{-# DEPRECATED laState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The token returned by a previous call to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laNextToken :: Lens.Lens' ListArchives (Lude.Maybe Lude.Text)
laNextToken = Lens.lens (nextToken :: ListArchives -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListArchives)
{-# DEPRECATED laNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A name prefix to filter the archives returned. Only archives with name that match the prefix are returned.
--
-- /Note:/ Consider using 'namePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laNamePrefix :: Lens.Lens' ListArchives (Lude.Maybe Lude.Text)
laNamePrefix = Lens.lens (namePrefix :: ListArchives -> Lude.Maybe Lude.Text) (\s a -> s {namePrefix = a} :: ListArchives)
{-# DEPRECATED laNamePrefix "Use generic-lens or generic-optics with 'namePrefix' instead." #-}

-- | The maximum number of results to return.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laLimit :: Lens.Lens' ListArchives (Lude.Maybe Lude.Natural)
laLimit = Lens.lens (limit :: ListArchives -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListArchives)
{-# DEPRECATED laLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Lude.AWSRequest ListArchives where
  type Rs ListArchives = ListArchivesResponse
  request = Req.postJSON cloudWatchEventsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListArchivesResponse'
            Lude.<$> (x Lude..?> "Archives" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListArchives where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSEvents.ListArchives" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListArchives where
  toJSON ListArchives' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EventSourceArn" Lude..=) Lude.<$> eventSourceARN,
            ("State" Lude..=) Lude.<$> state,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("NamePrefix" Lude..=) Lude.<$> namePrefix,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListArchives where
  toPath = Lude.const "/"

instance Lude.ToQuery ListArchives where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListArchivesResponse' smart constructor.
data ListArchivesResponse = ListArchivesResponse'
  { archives ::
      Lude.Maybe [Archive],
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

-- | Creates a value of 'ListArchivesResponse' with the minimum fields required to make a request.
--
-- * 'archives' - An array of @Archive@ objects that include details about an archive.
-- * 'nextToken' - The token returned by a previous call to retrieve the next set of results.
-- * 'responseStatus' - The response status code.
mkListArchivesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListArchivesResponse
mkListArchivesResponse pResponseStatus_ =
  ListArchivesResponse'
    { archives = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of @Archive@ objects that include details about an archive.
--
-- /Note:/ Consider using 'archives' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsArchives :: Lens.Lens' ListArchivesResponse (Lude.Maybe [Archive])
larsArchives = Lens.lens (archives :: ListArchivesResponse -> Lude.Maybe [Archive]) (\s a -> s {archives = a} :: ListArchivesResponse)
{-# DEPRECATED larsArchives "Use generic-lens or generic-optics with 'archives' instead." #-}

-- | The token returned by a previous call to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsNextToken :: Lens.Lens' ListArchivesResponse (Lude.Maybe Lude.Text)
larsNextToken = Lens.lens (nextToken :: ListArchivesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListArchivesResponse)
{-# DEPRECATED larsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsResponseStatus :: Lens.Lens' ListArchivesResponse Lude.Int
larsResponseStatus = Lens.lens (responseStatus :: ListArchivesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListArchivesResponse)
{-# DEPRECATED larsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

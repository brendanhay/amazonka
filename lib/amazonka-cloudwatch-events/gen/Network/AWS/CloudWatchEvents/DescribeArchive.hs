{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.DescribeArchive
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details about an archive.
module Network.AWS.CloudWatchEvents.DescribeArchive
  ( -- * Creating a request
    DescribeArchive (..),
    mkDescribeArchive,

    -- ** Request lenses
    daArchiveName,

    -- * Destructuring the response
    DescribeArchiveResponse (..),
    mkDescribeArchiveResponse,

    -- ** Response lenses
    darsCreationTime,
    darsSizeBytes,
    darsEventSourceARN,
    darsEventPattern,
    darsState,
    darsEventCount,
    darsArchiveName,
    darsRetentionDays,
    darsArchiveARN,
    darsStateReason,
    darsDescription,
    darsResponseStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeArchive' smart constructor.
newtype DescribeArchive = DescribeArchive'
  { -- | The name of the archive to retrieve.
    archiveName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeArchive' with the minimum fields required to make a request.
--
-- * 'archiveName' - The name of the archive to retrieve.
mkDescribeArchive ::
  -- | 'archiveName'
  Lude.Text ->
  DescribeArchive
mkDescribeArchive pArchiveName_ =
  DescribeArchive' {archiveName = pArchiveName_}

-- | The name of the archive to retrieve.
--
-- /Note:/ Consider using 'archiveName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daArchiveName :: Lens.Lens' DescribeArchive Lude.Text
daArchiveName = Lens.lens (archiveName :: DescribeArchive -> Lude.Text) (\s a -> s {archiveName = a} :: DescribeArchive)
{-# DEPRECATED daArchiveName "Use generic-lens or generic-optics with 'archiveName' instead." #-}

instance Lude.AWSRequest DescribeArchive where
  type Rs DescribeArchive = DescribeArchiveResponse
  request = Req.postJSON cloudWatchEventsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeArchiveResponse'
            Lude.<$> (x Lude..?> "CreationTime")
            Lude.<*> (x Lude..?> "SizeBytes")
            Lude.<*> (x Lude..?> "EventSourceArn")
            Lude.<*> (x Lude..?> "EventPattern")
            Lude.<*> (x Lude..?> "State")
            Lude.<*> (x Lude..?> "EventCount")
            Lude.<*> (x Lude..?> "ArchiveName")
            Lude.<*> (x Lude..?> "RetentionDays")
            Lude.<*> (x Lude..?> "ArchiveArn")
            Lude.<*> (x Lude..?> "StateReason")
            Lude.<*> (x Lude..?> "Description")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeArchive where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSEvents.DescribeArchive" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeArchive where
  toJSON DescribeArchive' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ArchiveName" Lude..= archiveName)])

instance Lude.ToPath DescribeArchive where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeArchive where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeArchiveResponse' smart constructor.
data DescribeArchiveResponse = DescribeArchiveResponse'
  { -- | The time at which the archive was created.
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | The size of the archive in bytes.
    sizeBytes :: Lude.Maybe Lude.Integer,
    -- | The ARN of the event source associated with the archive.
    eventSourceARN :: Lude.Maybe Lude.Text,
    -- | The event pattern used to filter events sent to the archive.
    eventPattern :: Lude.Maybe Lude.Text,
    -- | The state of the archive.
    state :: Lude.Maybe ArchiveState,
    -- | The number of events in the archive.
    eventCount :: Lude.Maybe Lude.Integer,
    -- | The name of the archive.
    archiveName :: Lude.Maybe Lude.Text,
    -- | The number of days to retain events for in the archive.
    retentionDays :: Lude.Maybe Lude.Natural,
    -- | The ARN of the archive.
    archiveARN :: Lude.Maybe Lude.Text,
    -- | The reason that the archive is in the state.
    stateReason :: Lude.Maybe Lude.Text,
    -- | The description of the archive.
    description :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeArchiveResponse' with the minimum fields required to make a request.
--
-- * 'creationTime' - The time at which the archive was created.
-- * 'sizeBytes' - The size of the archive in bytes.
-- * 'eventSourceARN' - The ARN of the event source associated with the archive.
-- * 'eventPattern' - The event pattern used to filter events sent to the archive.
-- * 'state' - The state of the archive.
-- * 'eventCount' - The number of events in the archive.
-- * 'archiveName' - The name of the archive.
-- * 'retentionDays' - The number of days to retain events for in the archive.
-- * 'archiveARN' - The ARN of the archive.
-- * 'stateReason' - The reason that the archive is in the state.
-- * 'description' - The description of the archive.
-- * 'responseStatus' - The response status code.
mkDescribeArchiveResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeArchiveResponse
mkDescribeArchiveResponse pResponseStatus_ =
  DescribeArchiveResponse'
    { creationTime = Lude.Nothing,
      sizeBytes = Lude.Nothing,
      eventSourceARN = Lude.Nothing,
      eventPattern = Lude.Nothing,
      state = Lude.Nothing,
      eventCount = Lude.Nothing,
      archiveName = Lude.Nothing,
      retentionDays = Lude.Nothing,
      archiveARN = Lude.Nothing,
      stateReason = Lude.Nothing,
      description = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The time at which the archive was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsCreationTime :: Lens.Lens' DescribeArchiveResponse (Lude.Maybe Lude.Timestamp)
darsCreationTime = Lens.lens (creationTime :: DescribeArchiveResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: DescribeArchiveResponse)
{-# DEPRECATED darsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The size of the archive in bytes.
--
-- /Note:/ Consider using 'sizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsSizeBytes :: Lens.Lens' DescribeArchiveResponse (Lude.Maybe Lude.Integer)
darsSizeBytes = Lens.lens (sizeBytes :: DescribeArchiveResponse -> Lude.Maybe Lude.Integer) (\s a -> s {sizeBytes = a} :: DescribeArchiveResponse)
{-# DEPRECATED darsSizeBytes "Use generic-lens or generic-optics with 'sizeBytes' instead." #-}

-- | The ARN of the event source associated with the archive.
--
-- /Note:/ Consider using 'eventSourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsEventSourceARN :: Lens.Lens' DescribeArchiveResponse (Lude.Maybe Lude.Text)
darsEventSourceARN = Lens.lens (eventSourceARN :: DescribeArchiveResponse -> Lude.Maybe Lude.Text) (\s a -> s {eventSourceARN = a} :: DescribeArchiveResponse)
{-# DEPRECATED darsEventSourceARN "Use generic-lens or generic-optics with 'eventSourceARN' instead." #-}

-- | The event pattern used to filter events sent to the archive.
--
-- /Note:/ Consider using 'eventPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsEventPattern :: Lens.Lens' DescribeArchiveResponse (Lude.Maybe Lude.Text)
darsEventPattern = Lens.lens (eventPattern :: DescribeArchiveResponse -> Lude.Maybe Lude.Text) (\s a -> s {eventPattern = a} :: DescribeArchiveResponse)
{-# DEPRECATED darsEventPattern "Use generic-lens or generic-optics with 'eventPattern' instead." #-}

-- | The state of the archive.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsState :: Lens.Lens' DescribeArchiveResponse (Lude.Maybe ArchiveState)
darsState = Lens.lens (state :: DescribeArchiveResponse -> Lude.Maybe ArchiveState) (\s a -> s {state = a} :: DescribeArchiveResponse)
{-# DEPRECATED darsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The number of events in the archive.
--
-- /Note:/ Consider using 'eventCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsEventCount :: Lens.Lens' DescribeArchiveResponse (Lude.Maybe Lude.Integer)
darsEventCount = Lens.lens (eventCount :: DescribeArchiveResponse -> Lude.Maybe Lude.Integer) (\s a -> s {eventCount = a} :: DescribeArchiveResponse)
{-# DEPRECATED darsEventCount "Use generic-lens or generic-optics with 'eventCount' instead." #-}

-- | The name of the archive.
--
-- /Note:/ Consider using 'archiveName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsArchiveName :: Lens.Lens' DescribeArchiveResponse (Lude.Maybe Lude.Text)
darsArchiveName = Lens.lens (archiveName :: DescribeArchiveResponse -> Lude.Maybe Lude.Text) (\s a -> s {archiveName = a} :: DescribeArchiveResponse)
{-# DEPRECATED darsArchiveName "Use generic-lens or generic-optics with 'archiveName' instead." #-}

-- | The number of days to retain events for in the archive.
--
-- /Note:/ Consider using 'retentionDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsRetentionDays :: Lens.Lens' DescribeArchiveResponse (Lude.Maybe Lude.Natural)
darsRetentionDays = Lens.lens (retentionDays :: DescribeArchiveResponse -> Lude.Maybe Lude.Natural) (\s a -> s {retentionDays = a} :: DescribeArchiveResponse)
{-# DEPRECATED darsRetentionDays "Use generic-lens or generic-optics with 'retentionDays' instead." #-}

-- | The ARN of the archive.
--
-- /Note:/ Consider using 'archiveARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsArchiveARN :: Lens.Lens' DescribeArchiveResponse (Lude.Maybe Lude.Text)
darsArchiveARN = Lens.lens (archiveARN :: DescribeArchiveResponse -> Lude.Maybe Lude.Text) (\s a -> s {archiveARN = a} :: DescribeArchiveResponse)
{-# DEPRECATED darsArchiveARN "Use generic-lens or generic-optics with 'archiveARN' instead." #-}

-- | The reason that the archive is in the state.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsStateReason :: Lens.Lens' DescribeArchiveResponse (Lude.Maybe Lude.Text)
darsStateReason = Lens.lens (stateReason :: DescribeArchiveResponse -> Lude.Maybe Lude.Text) (\s a -> s {stateReason = a} :: DescribeArchiveResponse)
{-# DEPRECATED darsStateReason "Use generic-lens or generic-optics with 'stateReason' instead." #-}

-- | The description of the archive.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsDescription :: Lens.Lens' DescribeArchiveResponse (Lude.Maybe Lude.Text)
darsDescription = Lens.lens (description :: DescribeArchiveResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: DescribeArchiveResponse)
{-# DEPRECATED darsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsResponseStatus :: Lens.Lens' DescribeArchiveResponse Lude.Int
darsResponseStatus = Lens.lens (responseStatus :: DescribeArchiveResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeArchiveResponse)
{-# DEPRECATED darsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

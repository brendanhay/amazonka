{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.CreateArchive
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an archive of events with the specified settings. When you create an archive, incoming events might not immediately start being sent to the archive. Allow a short period of time for changes to take effect. If you do not specify a pattern to filter events sent to the archive, all events are sent to the archive except replayed events. Replayed events are not sent to an archive.
module Network.AWS.CloudWatchEvents.CreateArchive
  ( -- * Creating a request
    CreateArchive (..),
    mkCreateArchive,

    -- ** Request lenses
    caEventSourceARN,
    caEventPattern,
    caArchiveName,
    caRetentionDays,
    caDescription,

    -- * Destructuring the response
    CreateArchiveResponse (..),
    mkCreateArchiveResponse,

    -- ** Response lenses
    carsCreationTime,
    carsState,
    carsArchiveARN,
    carsStateReason,
    carsResponseStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateArchive' smart constructor.
data CreateArchive = CreateArchive'
  { -- | The ARN of the event source associated with the archive.
    eventSourceARN :: Lude.Text,
    -- | An event pattern to use to filter events sent to the archive.
    eventPattern :: Lude.Maybe Lude.Text,
    -- | The name for the archive to create.
    archiveName :: Lude.Text,
    -- | The number of days to retain events for. Default value is 0. If set to 0, events are retained indefinitely
    retentionDays :: Lude.Maybe Lude.Natural,
    -- | A description for the archive.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateArchive' with the minimum fields required to make a request.
--
-- * 'eventSourceARN' - The ARN of the event source associated with the archive.
-- * 'eventPattern' - An event pattern to use to filter events sent to the archive.
-- * 'archiveName' - The name for the archive to create.
-- * 'retentionDays' - The number of days to retain events for. Default value is 0. If set to 0, events are retained indefinitely
-- * 'description' - A description for the archive.
mkCreateArchive ::
  -- | 'eventSourceARN'
  Lude.Text ->
  -- | 'archiveName'
  Lude.Text ->
  CreateArchive
mkCreateArchive pEventSourceARN_ pArchiveName_ =
  CreateArchive'
    { eventSourceARN = pEventSourceARN_,
      eventPattern = Lude.Nothing,
      archiveName = pArchiveName_,
      retentionDays = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The ARN of the event source associated with the archive.
--
-- /Note:/ Consider using 'eventSourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caEventSourceARN :: Lens.Lens' CreateArchive Lude.Text
caEventSourceARN = Lens.lens (eventSourceARN :: CreateArchive -> Lude.Text) (\s a -> s {eventSourceARN = a} :: CreateArchive)
{-# DEPRECATED caEventSourceARN "Use generic-lens or generic-optics with 'eventSourceARN' instead." #-}

-- | An event pattern to use to filter events sent to the archive.
--
-- /Note:/ Consider using 'eventPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caEventPattern :: Lens.Lens' CreateArchive (Lude.Maybe Lude.Text)
caEventPattern = Lens.lens (eventPattern :: CreateArchive -> Lude.Maybe Lude.Text) (\s a -> s {eventPattern = a} :: CreateArchive)
{-# DEPRECATED caEventPattern "Use generic-lens or generic-optics with 'eventPattern' instead." #-}

-- | The name for the archive to create.
--
-- /Note:/ Consider using 'archiveName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caArchiveName :: Lens.Lens' CreateArchive Lude.Text
caArchiveName = Lens.lens (archiveName :: CreateArchive -> Lude.Text) (\s a -> s {archiveName = a} :: CreateArchive)
{-# DEPRECATED caArchiveName "Use generic-lens or generic-optics with 'archiveName' instead." #-}

-- | The number of days to retain events for. Default value is 0. If set to 0, events are retained indefinitely
--
-- /Note:/ Consider using 'retentionDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caRetentionDays :: Lens.Lens' CreateArchive (Lude.Maybe Lude.Natural)
caRetentionDays = Lens.lens (retentionDays :: CreateArchive -> Lude.Maybe Lude.Natural) (\s a -> s {retentionDays = a} :: CreateArchive)
{-# DEPRECATED caRetentionDays "Use generic-lens or generic-optics with 'retentionDays' instead." #-}

-- | A description for the archive.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDescription :: Lens.Lens' CreateArchive (Lude.Maybe Lude.Text)
caDescription = Lens.lens (description :: CreateArchive -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateArchive)
{-# DEPRECATED caDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest CreateArchive where
  type Rs CreateArchive = CreateArchiveResponse
  request = Req.postJSON cloudWatchEventsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateArchiveResponse'
            Lude.<$> (x Lude..?> "CreationTime")
            Lude.<*> (x Lude..?> "State")
            Lude.<*> (x Lude..?> "ArchiveArn")
            Lude.<*> (x Lude..?> "StateReason")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateArchive where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSEvents.CreateArchive" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateArchive where
  toJSON CreateArchive' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("EventSourceArn" Lude..= eventSourceARN),
            ("EventPattern" Lude..=) Lude.<$> eventPattern,
            Lude.Just ("ArchiveName" Lude..= archiveName),
            ("RetentionDays" Lude..=) Lude.<$> retentionDays,
            ("Description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath CreateArchive where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateArchive where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateArchiveResponse' smart constructor.
data CreateArchiveResponse = CreateArchiveResponse'
  { -- | The time at which the archive was created.
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | The state of the archive that was created.
    state :: Lude.Maybe ArchiveState,
    -- | The ARN of the archive that was created.
    archiveARN :: Lude.Maybe Lude.Text,
    -- | The reason that the archive is in the state.
    stateReason :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateArchiveResponse' with the minimum fields required to make a request.
--
-- * 'creationTime' - The time at which the archive was created.
-- * 'state' - The state of the archive that was created.
-- * 'archiveARN' - The ARN of the archive that was created.
-- * 'stateReason' - The reason that the archive is in the state.
-- * 'responseStatus' - The response status code.
mkCreateArchiveResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateArchiveResponse
mkCreateArchiveResponse pResponseStatus_ =
  CreateArchiveResponse'
    { creationTime = Lude.Nothing,
      state = Lude.Nothing,
      archiveARN = Lude.Nothing,
      stateReason = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The time at which the archive was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsCreationTime :: Lens.Lens' CreateArchiveResponse (Lude.Maybe Lude.Timestamp)
carsCreationTime = Lens.lens (creationTime :: CreateArchiveResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: CreateArchiveResponse)
{-# DEPRECATED carsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The state of the archive that was created.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsState :: Lens.Lens' CreateArchiveResponse (Lude.Maybe ArchiveState)
carsState = Lens.lens (state :: CreateArchiveResponse -> Lude.Maybe ArchiveState) (\s a -> s {state = a} :: CreateArchiveResponse)
{-# DEPRECATED carsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ARN of the archive that was created.
--
-- /Note:/ Consider using 'archiveARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsArchiveARN :: Lens.Lens' CreateArchiveResponse (Lude.Maybe Lude.Text)
carsArchiveARN = Lens.lens (archiveARN :: CreateArchiveResponse -> Lude.Maybe Lude.Text) (\s a -> s {archiveARN = a} :: CreateArchiveResponse)
{-# DEPRECATED carsArchiveARN "Use generic-lens or generic-optics with 'archiveARN' instead." #-}

-- | The reason that the archive is in the state.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsStateReason :: Lens.Lens' CreateArchiveResponse (Lude.Maybe Lude.Text)
carsStateReason = Lens.lens (stateReason :: CreateArchiveResponse -> Lude.Maybe Lude.Text) (\s a -> s {stateReason = a} :: CreateArchiveResponse)
{-# DEPRECATED carsStateReason "Use generic-lens or generic-optics with 'stateReason' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsResponseStatus :: Lens.Lens' CreateArchiveResponse Lude.Int
carsResponseStatus = Lens.lens (responseStatus :: CreateArchiveResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateArchiveResponse)
{-# DEPRECATED carsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

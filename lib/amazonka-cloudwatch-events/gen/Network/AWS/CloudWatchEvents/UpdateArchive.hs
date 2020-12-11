{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.UpdateArchive
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified archive.
module Network.AWS.CloudWatchEvents.UpdateArchive
  ( -- * Creating a request
    UpdateArchive (..),
    mkUpdateArchive,

    -- ** Request lenses
    uaEventPattern,
    uaRetentionDays,
    uaDescription,
    uaArchiveName,

    -- * Destructuring the response
    UpdateArchiveResponse (..),
    mkUpdateArchiveResponse,

    -- ** Response lenses
    uarsCreationTime,
    uarsState,
    uarsArchiveARN,
    uarsStateReason,
    uarsResponseStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateArchive' smart constructor.
data UpdateArchive = UpdateArchive'
  { eventPattern ::
      Lude.Maybe Lude.Text,
    retentionDays :: Lude.Maybe Lude.Natural,
    description :: Lude.Maybe Lude.Text,
    archiveName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateArchive' with the minimum fields required to make a request.
--
-- * 'archiveName' - The name of the archive to update.
-- * 'description' - The description for the archive.
-- * 'eventPattern' - The event pattern to use to filter events sent to the archive.
-- * 'retentionDays' - The number of days to retain events in the archive.
mkUpdateArchive ::
  -- | 'archiveName'
  Lude.Text ->
  UpdateArchive
mkUpdateArchive pArchiveName_ =
  UpdateArchive'
    { eventPattern = Lude.Nothing,
      retentionDays = Lude.Nothing,
      description = Lude.Nothing,
      archiveName = pArchiveName_
    }

-- | The event pattern to use to filter events sent to the archive.
--
-- /Note:/ Consider using 'eventPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaEventPattern :: Lens.Lens' UpdateArchive (Lude.Maybe Lude.Text)
uaEventPattern = Lens.lens (eventPattern :: UpdateArchive -> Lude.Maybe Lude.Text) (\s a -> s {eventPattern = a} :: UpdateArchive)
{-# DEPRECATED uaEventPattern "Use generic-lens or generic-optics with 'eventPattern' instead." #-}

-- | The number of days to retain events in the archive.
--
-- /Note:/ Consider using 'retentionDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaRetentionDays :: Lens.Lens' UpdateArchive (Lude.Maybe Lude.Natural)
uaRetentionDays = Lens.lens (retentionDays :: UpdateArchive -> Lude.Maybe Lude.Natural) (\s a -> s {retentionDays = a} :: UpdateArchive)
{-# DEPRECATED uaRetentionDays "Use generic-lens or generic-optics with 'retentionDays' instead." #-}

-- | The description for the archive.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaDescription :: Lens.Lens' UpdateArchive (Lude.Maybe Lude.Text)
uaDescription = Lens.lens (description :: UpdateArchive -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateArchive)
{-# DEPRECATED uaDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the archive to update.
--
-- /Note:/ Consider using 'archiveName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaArchiveName :: Lens.Lens' UpdateArchive Lude.Text
uaArchiveName = Lens.lens (archiveName :: UpdateArchive -> Lude.Text) (\s a -> s {archiveName = a} :: UpdateArchive)
{-# DEPRECATED uaArchiveName "Use generic-lens or generic-optics with 'archiveName' instead." #-}

instance Lude.AWSRequest UpdateArchive where
  type Rs UpdateArchive = UpdateArchiveResponse
  request = Req.postJSON cloudWatchEventsService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateArchiveResponse'
            Lude.<$> (x Lude..?> "CreationTime")
            Lude.<*> (x Lude..?> "State")
            Lude.<*> (x Lude..?> "ArchiveArn")
            Lude.<*> (x Lude..?> "StateReason")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateArchive where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSEvents.UpdateArchive" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateArchive where
  toJSON UpdateArchive' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EventPattern" Lude..=) Lude.<$> eventPattern,
            ("RetentionDays" Lude..=) Lude.<$> retentionDays,
            ("Description" Lude..=) Lude.<$> description,
            Lude.Just ("ArchiveName" Lude..= archiveName)
          ]
      )

instance Lude.ToPath UpdateArchive where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateArchive where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateArchiveResponse' smart constructor.
data UpdateArchiveResponse = UpdateArchiveResponse'
  { creationTime ::
      Lude.Maybe Lude.Timestamp,
    state :: Lude.Maybe ArchiveState,
    archiveARN :: Lude.Maybe Lude.Text,
    stateReason :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'UpdateArchiveResponse' with the minimum fields required to make a request.
--
-- * 'archiveARN' - The ARN of the archive.
-- * 'creationTime' - The time at which the archive was updated.
-- * 'responseStatus' - The response status code.
-- * 'state' - The state of the archive.
-- * 'stateReason' - The reason that the archive is in the current state.
mkUpdateArchiveResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateArchiveResponse
mkUpdateArchiveResponse pResponseStatus_ =
  UpdateArchiveResponse'
    { creationTime = Lude.Nothing,
      state = Lude.Nothing,
      archiveARN = Lude.Nothing,
      stateReason = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The time at which the archive was updated.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarsCreationTime :: Lens.Lens' UpdateArchiveResponse (Lude.Maybe Lude.Timestamp)
uarsCreationTime = Lens.lens (creationTime :: UpdateArchiveResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: UpdateArchiveResponse)
{-# DEPRECATED uarsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The state of the archive.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarsState :: Lens.Lens' UpdateArchiveResponse (Lude.Maybe ArchiveState)
uarsState = Lens.lens (state :: UpdateArchiveResponse -> Lude.Maybe ArchiveState) (\s a -> s {state = a} :: UpdateArchiveResponse)
{-# DEPRECATED uarsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ARN of the archive.
--
-- /Note:/ Consider using 'archiveARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarsArchiveARN :: Lens.Lens' UpdateArchiveResponse (Lude.Maybe Lude.Text)
uarsArchiveARN = Lens.lens (archiveARN :: UpdateArchiveResponse -> Lude.Maybe Lude.Text) (\s a -> s {archiveARN = a} :: UpdateArchiveResponse)
{-# DEPRECATED uarsArchiveARN "Use generic-lens or generic-optics with 'archiveARN' instead." #-}

-- | The reason that the archive is in the current state.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarsStateReason :: Lens.Lens' UpdateArchiveResponse (Lude.Maybe Lude.Text)
uarsStateReason = Lens.lens (stateReason :: UpdateArchiveResponse -> Lude.Maybe Lude.Text) (\s a -> s {stateReason = a} :: UpdateArchiveResponse)
{-# DEPRECATED uarsStateReason "Use generic-lens or generic-optics with 'stateReason' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarsResponseStatus :: Lens.Lens' UpdateArchiveResponse Lude.Int
uarsResponseStatus = Lens.lens (responseStatus :: UpdateArchiveResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateArchiveResponse)
{-# DEPRECATED uarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

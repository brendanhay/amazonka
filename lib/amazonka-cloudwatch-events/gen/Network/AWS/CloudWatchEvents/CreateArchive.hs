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
    caArchiveName,
    caEventSourceArn,
    caDescription,
    caEventPattern,
    caRetentionDays,

    -- * Destructuring the response
    CreateArchiveResponse (..),
    mkCreateArchiveResponse,

    -- ** Response lenses
    carrsArchiveArn,
    carrsCreationTime,
    carrsState,
    carrsStateReason,
    carrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateArchive' smart constructor.
data CreateArchive = CreateArchive'
  { -- | The name for the archive to create.
    archiveName :: Types.ArchiveName,
    -- | The ARN of the event source associated with the archive.
    eventSourceArn :: Types.Arn,
    -- | A description for the archive.
    description :: Core.Maybe Types.ArchiveDescription,
    -- | An event pattern to use to filter events sent to the archive.
    eventPattern :: Core.Maybe Types.EventPattern,
    -- | The number of days to retain events for. Default value is 0. If set to 0, events are retained indefinitely
    retentionDays :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateArchive' value with any optional fields omitted.
mkCreateArchive ::
  -- | 'archiveName'
  Types.ArchiveName ->
  -- | 'eventSourceArn'
  Types.Arn ->
  CreateArchive
mkCreateArchive archiveName eventSourceArn =
  CreateArchive'
    { archiveName,
      eventSourceArn,
      description = Core.Nothing,
      eventPattern = Core.Nothing,
      retentionDays = Core.Nothing
    }

-- | The name for the archive to create.
--
-- /Note:/ Consider using 'archiveName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caArchiveName :: Lens.Lens' CreateArchive Types.ArchiveName
caArchiveName = Lens.field @"archiveName"
{-# DEPRECATED caArchiveName "Use generic-lens or generic-optics with 'archiveName' instead." #-}

-- | The ARN of the event source associated with the archive.
--
-- /Note:/ Consider using 'eventSourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caEventSourceArn :: Lens.Lens' CreateArchive Types.Arn
caEventSourceArn = Lens.field @"eventSourceArn"
{-# DEPRECATED caEventSourceArn "Use generic-lens or generic-optics with 'eventSourceArn' instead." #-}

-- | A description for the archive.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDescription :: Lens.Lens' CreateArchive (Core.Maybe Types.ArchiveDescription)
caDescription = Lens.field @"description"
{-# DEPRECATED caDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | An event pattern to use to filter events sent to the archive.
--
-- /Note:/ Consider using 'eventPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caEventPattern :: Lens.Lens' CreateArchive (Core.Maybe Types.EventPattern)
caEventPattern = Lens.field @"eventPattern"
{-# DEPRECATED caEventPattern "Use generic-lens or generic-optics with 'eventPattern' instead." #-}

-- | The number of days to retain events for. Default value is 0. If set to 0, events are retained indefinitely
--
-- /Note:/ Consider using 'retentionDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caRetentionDays :: Lens.Lens' CreateArchive (Core.Maybe Core.Natural)
caRetentionDays = Lens.field @"retentionDays"
{-# DEPRECATED caRetentionDays "Use generic-lens or generic-optics with 'retentionDays' instead." #-}

instance Core.FromJSON CreateArchive where
  toJSON CreateArchive {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ArchiveName" Core..= archiveName),
            Core.Just ("EventSourceArn" Core..= eventSourceArn),
            ("Description" Core..=) Core.<$> description,
            ("EventPattern" Core..=) Core.<$> eventPattern,
            ("RetentionDays" Core..=) Core.<$> retentionDays
          ]
      )

instance Core.AWSRequest CreateArchive where
  type Rs CreateArchive = CreateArchiveResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSEvents.CreateArchive")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateArchiveResponse'
            Core.<$> (x Core..:? "ArchiveArn")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "State")
            Core.<*> (x Core..:? "StateReason")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateArchiveResponse' smart constructor.
data CreateArchiveResponse = CreateArchiveResponse'
  { -- | The ARN of the archive that was created.
    archiveArn :: Core.Maybe Types.ArchiveArn,
    -- | The time at which the archive was created.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | The state of the archive that was created.
    state :: Core.Maybe Types.ArchiveState,
    -- | The reason that the archive is in the state.
    stateReason :: Core.Maybe Types.StateReason,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateArchiveResponse' value with any optional fields omitted.
mkCreateArchiveResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateArchiveResponse
mkCreateArchiveResponse responseStatus =
  CreateArchiveResponse'
    { archiveArn = Core.Nothing,
      creationTime = Core.Nothing,
      state = Core.Nothing,
      stateReason = Core.Nothing,
      responseStatus
    }

-- | The ARN of the archive that was created.
--
-- /Note:/ Consider using 'archiveArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsArchiveArn :: Lens.Lens' CreateArchiveResponse (Core.Maybe Types.ArchiveArn)
carrsArchiveArn = Lens.field @"archiveArn"
{-# DEPRECATED carrsArchiveArn "Use generic-lens or generic-optics with 'archiveArn' instead." #-}

-- | The time at which the archive was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsCreationTime :: Lens.Lens' CreateArchiveResponse (Core.Maybe Core.NominalDiffTime)
carrsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED carrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The state of the archive that was created.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsState :: Lens.Lens' CreateArchiveResponse (Core.Maybe Types.ArchiveState)
carrsState = Lens.field @"state"
{-# DEPRECATED carrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The reason that the archive is in the state.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsStateReason :: Lens.Lens' CreateArchiveResponse (Core.Maybe Types.StateReason)
carrsStateReason = Lens.field @"stateReason"
{-# DEPRECATED carrsStateReason "Use generic-lens or generic-optics with 'stateReason' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsResponseStatus :: Lens.Lens' CreateArchiveResponse Core.Int
carrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED carrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

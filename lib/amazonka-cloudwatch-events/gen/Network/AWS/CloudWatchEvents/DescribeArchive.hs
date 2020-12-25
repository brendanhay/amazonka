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
    darrsArchiveArn,
    darrsArchiveName,
    darrsCreationTime,
    darrsDescription,
    darrsEventCount,
    darrsEventPattern,
    darrsEventSourceArn,
    darrsRetentionDays,
    darrsSizeBytes,
    darrsState,
    darrsStateReason,
    darrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeArchive' smart constructor.
newtype DescribeArchive = DescribeArchive'
  { -- | The name of the archive to retrieve.
    archiveName :: Types.ArchiveName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeArchive' value with any optional fields omitted.
mkDescribeArchive ::
  -- | 'archiveName'
  Types.ArchiveName ->
  DescribeArchive
mkDescribeArchive archiveName = DescribeArchive' {archiveName}

-- | The name of the archive to retrieve.
--
-- /Note:/ Consider using 'archiveName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daArchiveName :: Lens.Lens' DescribeArchive Types.ArchiveName
daArchiveName = Lens.field @"archiveName"
{-# DEPRECATED daArchiveName "Use generic-lens or generic-optics with 'archiveName' instead." #-}

instance Core.FromJSON DescribeArchive where
  toJSON DescribeArchive {..} =
    Core.object
      (Core.catMaybes [Core.Just ("ArchiveName" Core..= archiveName)])

instance Core.AWSRequest DescribeArchive where
  type Rs DescribeArchive = DescribeArchiveResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSEvents.DescribeArchive")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeArchiveResponse'
            Core.<$> (x Core..:? "ArchiveArn")
            Core.<*> (x Core..:? "ArchiveName")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "EventCount")
            Core.<*> (x Core..:? "EventPattern")
            Core.<*> (x Core..:? "EventSourceArn")
            Core.<*> (x Core..:? "RetentionDays")
            Core.<*> (x Core..:? "SizeBytes")
            Core.<*> (x Core..:? "State")
            Core.<*> (x Core..:? "StateReason")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeArchiveResponse' smart constructor.
data DescribeArchiveResponse = DescribeArchiveResponse'
  { -- | The ARN of the archive.
    archiveArn :: Core.Maybe Types.ArchiveArn,
    -- | The name of the archive.
    archiveName :: Core.Maybe Types.ArchiveName,
    -- | The time at which the archive was created.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | The description of the archive.
    description :: Core.Maybe Types.Description,
    -- | The number of events in the archive.
    eventCount :: Core.Maybe Core.Integer,
    -- | The event pattern used to filter events sent to the archive.
    eventPattern :: Core.Maybe Types.EventPattern,
    -- | The ARN of the event source associated with the archive.
    eventSourceArn :: Core.Maybe Types.Arn,
    -- | The number of days to retain events for in the archive.
    retentionDays :: Core.Maybe Core.Natural,
    -- | The size of the archive in bytes.
    sizeBytes :: Core.Maybe Core.Integer,
    -- | The state of the archive.
    state :: Core.Maybe Types.ArchiveState,
    -- | The reason that the archive is in the state.
    stateReason :: Core.Maybe Types.ArchiveStateReason,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeArchiveResponse' value with any optional fields omitted.
mkDescribeArchiveResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeArchiveResponse
mkDescribeArchiveResponse responseStatus =
  DescribeArchiveResponse'
    { archiveArn = Core.Nothing,
      archiveName = Core.Nothing,
      creationTime = Core.Nothing,
      description = Core.Nothing,
      eventCount = Core.Nothing,
      eventPattern = Core.Nothing,
      eventSourceArn = Core.Nothing,
      retentionDays = Core.Nothing,
      sizeBytes = Core.Nothing,
      state = Core.Nothing,
      stateReason = Core.Nothing,
      responseStatus
    }

-- | The ARN of the archive.
--
-- /Note:/ Consider using 'archiveArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsArchiveArn :: Lens.Lens' DescribeArchiveResponse (Core.Maybe Types.ArchiveArn)
darrsArchiveArn = Lens.field @"archiveArn"
{-# DEPRECATED darrsArchiveArn "Use generic-lens or generic-optics with 'archiveArn' instead." #-}

-- | The name of the archive.
--
-- /Note:/ Consider using 'archiveName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsArchiveName :: Lens.Lens' DescribeArchiveResponse (Core.Maybe Types.ArchiveName)
darrsArchiveName = Lens.field @"archiveName"
{-# DEPRECATED darrsArchiveName "Use generic-lens or generic-optics with 'archiveName' instead." #-}

-- | The time at which the archive was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsCreationTime :: Lens.Lens' DescribeArchiveResponse (Core.Maybe Core.NominalDiffTime)
darrsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED darrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The description of the archive.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsDescription :: Lens.Lens' DescribeArchiveResponse (Core.Maybe Types.Description)
darrsDescription = Lens.field @"description"
{-# DEPRECATED darrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The number of events in the archive.
--
-- /Note:/ Consider using 'eventCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsEventCount :: Lens.Lens' DescribeArchiveResponse (Core.Maybe Core.Integer)
darrsEventCount = Lens.field @"eventCount"
{-# DEPRECATED darrsEventCount "Use generic-lens or generic-optics with 'eventCount' instead." #-}

-- | The event pattern used to filter events sent to the archive.
--
-- /Note:/ Consider using 'eventPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsEventPattern :: Lens.Lens' DescribeArchiveResponse (Core.Maybe Types.EventPattern)
darrsEventPattern = Lens.field @"eventPattern"
{-# DEPRECATED darrsEventPattern "Use generic-lens or generic-optics with 'eventPattern' instead." #-}

-- | The ARN of the event source associated with the archive.
--
-- /Note:/ Consider using 'eventSourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsEventSourceArn :: Lens.Lens' DescribeArchiveResponse (Core.Maybe Types.Arn)
darrsEventSourceArn = Lens.field @"eventSourceArn"
{-# DEPRECATED darrsEventSourceArn "Use generic-lens or generic-optics with 'eventSourceArn' instead." #-}

-- | The number of days to retain events for in the archive.
--
-- /Note:/ Consider using 'retentionDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsRetentionDays :: Lens.Lens' DescribeArchiveResponse (Core.Maybe Core.Natural)
darrsRetentionDays = Lens.field @"retentionDays"
{-# DEPRECATED darrsRetentionDays "Use generic-lens or generic-optics with 'retentionDays' instead." #-}

-- | The size of the archive in bytes.
--
-- /Note:/ Consider using 'sizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsSizeBytes :: Lens.Lens' DescribeArchiveResponse (Core.Maybe Core.Integer)
darrsSizeBytes = Lens.field @"sizeBytes"
{-# DEPRECATED darrsSizeBytes "Use generic-lens or generic-optics with 'sizeBytes' instead." #-}

-- | The state of the archive.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsState :: Lens.Lens' DescribeArchiveResponse (Core.Maybe Types.ArchiveState)
darrsState = Lens.field @"state"
{-# DEPRECATED darrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The reason that the archive is in the state.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsStateReason :: Lens.Lens' DescribeArchiveResponse (Core.Maybe Types.ArchiveStateReason)
darrsStateReason = Lens.field @"stateReason"
{-# DEPRECATED darrsStateReason "Use generic-lens or generic-optics with 'stateReason' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsResponseStatus :: Lens.Lens' DescribeArchiveResponse Core.Int
darrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED darrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

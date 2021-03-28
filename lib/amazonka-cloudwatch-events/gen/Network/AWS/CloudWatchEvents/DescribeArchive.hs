{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeArchive (..)
    , mkDescribeArchive
    -- ** Request lenses
    , daArchiveName

    -- * Destructuring the response
    , DescribeArchiveResponse (..)
    , mkDescribeArchiveResponse
    -- ** Response lenses
    , darrsArchiveArn
    , darrsArchiveName
    , darrsCreationTime
    , darrsDescription
    , darrsEventCount
    , darrsEventPattern
    , darrsEventSourceArn
    , darrsRetentionDays
    , darrsSizeBytes
    , darrsState
    , darrsStateReason
    , darrsResponseStatus
    ) where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeArchive' smart constructor.
newtype DescribeArchive = DescribeArchive'
  { archiveName :: Types.ArchiveName
    -- ^ The name of the archive to retrieve.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeArchive' value with any optional fields omitted.
mkDescribeArchive
    :: Types.ArchiveName -- ^ 'archiveName'
    -> DescribeArchive
mkDescribeArchive archiveName = DescribeArchive'{archiveName}

-- | The name of the archive to retrieve.
--
-- /Note:/ Consider using 'archiveName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daArchiveName :: Lens.Lens' DescribeArchive Types.ArchiveName
daArchiveName = Lens.field @"archiveName"
{-# INLINEABLE daArchiveName #-}
{-# DEPRECATED archiveName "Use generic-lens or generic-optics with 'archiveName' instead"  #-}

instance Core.ToQuery DescribeArchive where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeArchive where
        toHeaders DescribeArchive{..}
          = Core.pure ("X-Amz-Target", "AWSEvents.DescribeArchive") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeArchive where
        toJSON DescribeArchive{..}
          = Core.object
              (Core.catMaybes [Core.Just ("ArchiveName" Core..= archiveName)])

instance Core.AWSRequest DescribeArchive where
        type Rs DescribeArchive = DescribeArchiveResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeArchiveResponse' Core.<$>
                   (x Core..:? "ArchiveArn") Core.<*> x Core..:? "ArchiveName"
                     Core.<*> x Core..:? "CreationTime"
                     Core.<*> x Core..:? "Description"
                     Core.<*> x Core..:? "EventCount"
                     Core.<*> x Core..:? "EventPattern"
                     Core.<*> x Core..:? "EventSourceArn"
                     Core.<*> x Core..:? "RetentionDays"
                     Core.<*> x Core..:? "SizeBytes"
                     Core.<*> x Core..:? "State"
                     Core.<*> x Core..:? "StateReason"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeArchiveResponse' smart constructor.
data DescribeArchiveResponse = DescribeArchiveResponse'
  { archiveArn :: Core.Maybe Types.ArchiveArn
    -- ^ The ARN of the archive.
  , archiveName :: Core.Maybe Types.ArchiveName
    -- ^ The name of the archive.
  , creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time at which the archive was created.
  , description :: Core.Maybe Types.Description
    -- ^ The description of the archive.
  , eventCount :: Core.Maybe Core.Integer
    -- ^ The number of events in the archive.
  , eventPattern :: Core.Maybe Types.EventPattern
    -- ^ The event pattern used to filter events sent to the archive.
  , eventSourceArn :: Core.Maybe Types.Arn
    -- ^ The ARN of the event source associated with the archive.
  , retentionDays :: Core.Maybe Core.Natural
    -- ^ The number of days to retain events for in the archive.
  , sizeBytes :: Core.Maybe Core.Integer
    -- ^ The size of the archive in bytes.
  , state :: Core.Maybe Types.ArchiveState
    -- ^ The state of the archive.
  , stateReason :: Core.Maybe Types.ArchiveStateReason
    -- ^ The reason that the archive is in the state.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeArchiveResponse' value with any optional fields omitted.
mkDescribeArchiveResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeArchiveResponse
mkDescribeArchiveResponse responseStatus
  = DescribeArchiveResponse'{archiveArn = Core.Nothing,
                             archiveName = Core.Nothing, creationTime = Core.Nothing,
                             description = Core.Nothing, eventCount = Core.Nothing,
                             eventPattern = Core.Nothing, eventSourceArn = Core.Nothing,
                             retentionDays = Core.Nothing, sizeBytes = Core.Nothing,
                             state = Core.Nothing, stateReason = Core.Nothing, responseStatus}

-- | The ARN of the archive.
--
-- /Note:/ Consider using 'archiveArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsArchiveArn :: Lens.Lens' DescribeArchiveResponse (Core.Maybe Types.ArchiveArn)
darrsArchiveArn = Lens.field @"archiveArn"
{-# INLINEABLE darrsArchiveArn #-}
{-# DEPRECATED archiveArn "Use generic-lens or generic-optics with 'archiveArn' instead"  #-}

-- | The name of the archive.
--
-- /Note:/ Consider using 'archiveName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsArchiveName :: Lens.Lens' DescribeArchiveResponse (Core.Maybe Types.ArchiveName)
darrsArchiveName = Lens.field @"archiveName"
{-# INLINEABLE darrsArchiveName #-}
{-# DEPRECATED archiveName "Use generic-lens or generic-optics with 'archiveName' instead"  #-}

-- | The time at which the archive was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsCreationTime :: Lens.Lens' DescribeArchiveResponse (Core.Maybe Core.NominalDiffTime)
darrsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE darrsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The description of the archive.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsDescription :: Lens.Lens' DescribeArchiveResponse (Core.Maybe Types.Description)
darrsDescription = Lens.field @"description"
{-# INLINEABLE darrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The number of events in the archive.
--
-- /Note:/ Consider using 'eventCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsEventCount :: Lens.Lens' DescribeArchiveResponse (Core.Maybe Core.Integer)
darrsEventCount = Lens.field @"eventCount"
{-# INLINEABLE darrsEventCount #-}
{-# DEPRECATED eventCount "Use generic-lens or generic-optics with 'eventCount' instead"  #-}

-- | The event pattern used to filter events sent to the archive.
--
-- /Note:/ Consider using 'eventPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsEventPattern :: Lens.Lens' DescribeArchiveResponse (Core.Maybe Types.EventPattern)
darrsEventPattern = Lens.field @"eventPattern"
{-# INLINEABLE darrsEventPattern #-}
{-# DEPRECATED eventPattern "Use generic-lens or generic-optics with 'eventPattern' instead"  #-}

-- | The ARN of the event source associated with the archive.
--
-- /Note:/ Consider using 'eventSourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsEventSourceArn :: Lens.Lens' DescribeArchiveResponse (Core.Maybe Types.Arn)
darrsEventSourceArn = Lens.field @"eventSourceArn"
{-# INLINEABLE darrsEventSourceArn #-}
{-# DEPRECATED eventSourceArn "Use generic-lens or generic-optics with 'eventSourceArn' instead"  #-}

-- | The number of days to retain events for in the archive.
--
-- /Note:/ Consider using 'retentionDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsRetentionDays :: Lens.Lens' DescribeArchiveResponse (Core.Maybe Core.Natural)
darrsRetentionDays = Lens.field @"retentionDays"
{-# INLINEABLE darrsRetentionDays #-}
{-# DEPRECATED retentionDays "Use generic-lens or generic-optics with 'retentionDays' instead"  #-}

-- | The size of the archive in bytes.
--
-- /Note:/ Consider using 'sizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsSizeBytes :: Lens.Lens' DescribeArchiveResponse (Core.Maybe Core.Integer)
darrsSizeBytes = Lens.field @"sizeBytes"
{-# INLINEABLE darrsSizeBytes #-}
{-# DEPRECATED sizeBytes "Use generic-lens or generic-optics with 'sizeBytes' instead"  #-}

-- | The state of the archive.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsState :: Lens.Lens' DescribeArchiveResponse (Core.Maybe Types.ArchiveState)
darrsState = Lens.field @"state"
{-# INLINEABLE darrsState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The reason that the archive is in the state.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsStateReason :: Lens.Lens' DescribeArchiveResponse (Core.Maybe Types.ArchiveStateReason)
darrsStateReason = Lens.field @"stateReason"
{-# INLINEABLE darrsStateReason #-}
{-# DEPRECATED stateReason "Use generic-lens or generic-optics with 'stateReason' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsResponseStatus :: Lens.Lens' DescribeArchiveResponse Core.Int
darrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE darrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

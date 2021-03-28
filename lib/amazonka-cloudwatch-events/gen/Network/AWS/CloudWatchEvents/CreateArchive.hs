{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateArchive (..)
    , mkCreateArchive
    -- ** Request lenses
    , caArchiveName
    , caEventSourceArn
    , caDescription
    , caEventPattern
    , caRetentionDays

    -- * Destructuring the response
    , CreateArchiveResponse (..)
    , mkCreateArchiveResponse
    -- ** Response lenses
    , carrsArchiveArn
    , carrsCreationTime
    , carrsState
    , carrsStateReason
    , carrsResponseStatus
    ) where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateArchive' smart constructor.
data CreateArchive = CreateArchive'
  { archiveName :: Types.ArchiveName
    -- ^ The name for the archive to create.
  , eventSourceArn :: Types.Arn
    -- ^ The ARN of the event source associated with the archive.
  , description :: Core.Maybe Types.ArchiveDescription
    -- ^ A description for the archive.
  , eventPattern :: Core.Maybe Types.EventPattern
    -- ^ An event pattern to use to filter events sent to the archive.
  , retentionDays :: Core.Maybe Core.Natural
    -- ^ The number of days to retain events for. Default value is 0. If set to 0, events are retained indefinitely
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateArchive' value with any optional fields omitted.
mkCreateArchive
    :: Types.ArchiveName -- ^ 'archiveName'
    -> Types.Arn -- ^ 'eventSourceArn'
    -> CreateArchive
mkCreateArchive archiveName eventSourceArn
  = CreateArchive'{archiveName, eventSourceArn,
                   description = Core.Nothing, eventPattern = Core.Nothing,
                   retentionDays = Core.Nothing}

-- | The name for the archive to create.
--
-- /Note:/ Consider using 'archiveName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caArchiveName :: Lens.Lens' CreateArchive Types.ArchiveName
caArchiveName = Lens.field @"archiveName"
{-# INLINEABLE caArchiveName #-}
{-# DEPRECATED archiveName "Use generic-lens or generic-optics with 'archiveName' instead"  #-}

-- | The ARN of the event source associated with the archive.
--
-- /Note:/ Consider using 'eventSourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caEventSourceArn :: Lens.Lens' CreateArchive Types.Arn
caEventSourceArn = Lens.field @"eventSourceArn"
{-# INLINEABLE caEventSourceArn #-}
{-# DEPRECATED eventSourceArn "Use generic-lens or generic-optics with 'eventSourceArn' instead"  #-}

-- | A description for the archive.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDescription :: Lens.Lens' CreateArchive (Core.Maybe Types.ArchiveDescription)
caDescription = Lens.field @"description"
{-# INLINEABLE caDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | An event pattern to use to filter events sent to the archive.
--
-- /Note:/ Consider using 'eventPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caEventPattern :: Lens.Lens' CreateArchive (Core.Maybe Types.EventPattern)
caEventPattern = Lens.field @"eventPattern"
{-# INLINEABLE caEventPattern #-}
{-# DEPRECATED eventPattern "Use generic-lens or generic-optics with 'eventPattern' instead"  #-}

-- | The number of days to retain events for. Default value is 0. If set to 0, events are retained indefinitely
--
-- /Note:/ Consider using 'retentionDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caRetentionDays :: Lens.Lens' CreateArchive (Core.Maybe Core.Natural)
caRetentionDays = Lens.field @"retentionDays"
{-# INLINEABLE caRetentionDays #-}
{-# DEPRECATED retentionDays "Use generic-lens or generic-optics with 'retentionDays' instead"  #-}

instance Core.ToQuery CreateArchive where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateArchive where
        toHeaders CreateArchive{..}
          = Core.pure ("X-Amz-Target", "AWSEvents.CreateArchive") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateArchive where
        toJSON CreateArchive{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ArchiveName" Core..= archiveName),
                  Core.Just ("EventSourceArn" Core..= eventSourceArn),
                  ("Description" Core..=) Core.<$> description,
                  ("EventPattern" Core..=) Core.<$> eventPattern,
                  ("RetentionDays" Core..=) Core.<$> retentionDays])

instance Core.AWSRequest CreateArchive where
        type Rs CreateArchive = CreateArchiveResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateArchiveResponse' Core.<$>
                   (x Core..:? "ArchiveArn") Core.<*> x Core..:? "CreationTime"
                     Core.<*> x Core..:? "State"
                     Core.<*> x Core..:? "StateReason"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateArchiveResponse' smart constructor.
data CreateArchiveResponse = CreateArchiveResponse'
  { archiveArn :: Core.Maybe Types.ArchiveArn
    -- ^ The ARN of the archive that was created.
  , creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time at which the archive was created.
  , state :: Core.Maybe Types.ArchiveState
    -- ^ The state of the archive that was created.
  , stateReason :: Core.Maybe Types.StateReason
    -- ^ The reason that the archive is in the state.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateArchiveResponse' value with any optional fields omitted.
mkCreateArchiveResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateArchiveResponse
mkCreateArchiveResponse responseStatus
  = CreateArchiveResponse'{archiveArn = Core.Nothing,
                           creationTime = Core.Nothing, state = Core.Nothing,
                           stateReason = Core.Nothing, responseStatus}

-- | The ARN of the archive that was created.
--
-- /Note:/ Consider using 'archiveArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsArchiveArn :: Lens.Lens' CreateArchiveResponse (Core.Maybe Types.ArchiveArn)
carrsArchiveArn = Lens.field @"archiveArn"
{-# INLINEABLE carrsArchiveArn #-}
{-# DEPRECATED archiveArn "Use generic-lens or generic-optics with 'archiveArn' instead"  #-}

-- | The time at which the archive was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsCreationTime :: Lens.Lens' CreateArchiveResponse (Core.Maybe Core.NominalDiffTime)
carrsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE carrsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The state of the archive that was created.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsState :: Lens.Lens' CreateArchiveResponse (Core.Maybe Types.ArchiveState)
carrsState = Lens.field @"state"
{-# INLINEABLE carrsState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The reason that the archive is in the state.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsStateReason :: Lens.Lens' CreateArchiveResponse (Core.Maybe Types.StateReason)
carrsStateReason = Lens.field @"stateReason"
{-# INLINEABLE carrsStateReason #-}
{-# DEPRECATED stateReason "Use generic-lens or generic-optics with 'stateReason' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsResponseStatus :: Lens.Lens' CreateArchiveResponse Core.Int
carrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE carrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

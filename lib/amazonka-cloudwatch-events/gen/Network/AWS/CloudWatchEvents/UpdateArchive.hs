{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateArchive (..)
    , mkUpdateArchive
    -- ** Request lenses
    , uaArchiveName
    , uaDescription
    , uaEventPattern
    , uaRetentionDays

    -- * Destructuring the response
    , UpdateArchiveResponse (..)
    , mkUpdateArchiveResponse
    -- ** Response lenses
    , uarrsArchiveArn
    , uarrsCreationTime
    , uarrsState
    , uarrsStateReason
    , uarrsResponseStatus
    ) where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateArchive' smart constructor.
data UpdateArchive = UpdateArchive'
  { archiveName :: Types.ArchiveName
    -- ^ The name of the archive to update.
  , description :: Core.Maybe Types.ArchiveDescription
    -- ^ The description for the archive.
  , eventPattern :: Core.Maybe Types.EventPattern
    -- ^ The event pattern to use to filter events sent to the archive.
  , retentionDays :: Core.Maybe Core.Natural
    -- ^ The number of days to retain events in the archive.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateArchive' value with any optional fields omitted.
mkUpdateArchive
    :: Types.ArchiveName -- ^ 'archiveName'
    -> UpdateArchive
mkUpdateArchive archiveName
  = UpdateArchive'{archiveName, description = Core.Nothing,
                   eventPattern = Core.Nothing, retentionDays = Core.Nothing}

-- | The name of the archive to update.
--
-- /Note:/ Consider using 'archiveName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaArchiveName :: Lens.Lens' UpdateArchive Types.ArchiveName
uaArchiveName = Lens.field @"archiveName"
{-# INLINEABLE uaArchiveName #-}
{-# DEPRECATED archiveName "Use generic-lens or generic-optics with 'archiveName' instead"  #-}

-- | The description for the archive.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaDescription :: Lens.Lens' UpdateArchive (Core.Maybe Types.ArchiveDescription)
uaDescription = Lens.field @"description"
{-# INLINEABLE uaDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The event pattern to use to filter events sent to the archive.
--
-- /Note:/ Consider using 'eventPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaEventPattern :: Lens.Lens' UpdateArchive (Core.Maybe Types.EventPattern)
uaEventPattern = Lens.field @"eventPattern"
{-# INLINEABLE uaEventPattern #-}
{-# DEPRECATED eventPattern "Use generic-lens or generic-optics with 'eventPattern' instead"  #-}

-- | The number of days to retain events in the archive.
--
-- /Note:/ Consider using 'retentionDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaRetentionDays :: Lens.Lens' UpdateArchive (Core.Maybe Core.Natural)
uaRetentionDays = Lens.field @"retentionDays"
{-# INLINEABLE uaRetentionDays #-}
{-# DEPRECATED retentionDays "Use generic-lens or generic-optics with 'retentionDays' instead"  #-}

instance Core.ToQuery UpdateArchive where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateArchive where
        toHeaders UpdateArchive{..}
          = Core.pure ("X-Amz-Target", "AWSEvents.UpdateArchive") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateArchive where
        toJSON UpdateArchive{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ArchiveName" Core..= archiveName),
                  ("Description" Core..=) Core.<$> description,
                  ("EventPattern" Core..=) Core.<$> eventPattern,
                  ("RetentionDays" Core..=) Core.<$> retentionDays])

instance Core.AWSRequest UpdateArchive where
        type Rs UpdateArchive = UpdateArchiveResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateArchiveResponse' Core.<$>
                   (x Core..:? "ArchiveArn") Core.<*> x Core..:? "CreationTime"
                     Core.<*> x Core..:? "State"
                     Core.<*> x Core..:? "StateReason"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateArchiveResponse' smart constructor.
data UpdateArchiveResponse = UpdateArchiveResponse'
  { archiveArn :: Core.Maybe Types.ArchiveArn
    -- ^ The ARN of the archive.
  , creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time at which the archive was updated.
  , state :: Core.Maybe Types.ArchiveState
    -- ^ The state of the archive.
  , stateReason :: Core.Maybe Types.ArchiveStateReason
    -- ^ The reason that the archive is in the current state.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateArchiveResponse' value with any optional fields omitted.
mkUpdateArchiveResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateArchiveResponse
mkUpdateArchiveResponse responseStatus
  = UpdateArchiveResponse'{archiveArn = Core.Nothing,
                           creationTime = Core.Nothing, state = Core.Nothing,
                           stateReason = Core.Nothing, responseStatus}

-- | The ARN of the archive.
--
-- /Note:/ Consider using 'archiveArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarrsArchiveArn :: Lens.Lens' UpdateArchiveResponse (Core.Maybe Types.ArchiveArn)
uarrsArchiveArn = Lens.field @"archiveArn"
{-# INLINEABLE uarrsArchiveArn #-}
{-# DEPRECATED archiveArn "Use generic-lens or generic-optics with 'archiveArn' instead"  #-}

-- | The time at which the archive was updated.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarrsCreationTime :: Lens.Lens' UpdateArchiveResponse (Core.Maybe Core.NominalDiffTime)
uarrsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE uarrsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The state of the archive.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarrsState :: Lens.Lens' UpdateArchiveResponse (Core.Maybe Types.ArchiveState)
uarrsState = Lens.field @"state"
{-# INLINEABLE uarrsState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The reason that the archive is in the current state.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarrsStateReason :: Lens.Lens' UpdateArchiveResponse (Core.Maybe Types.ArchiveStateReason)
uarrsStateReason = Lens.field @"stateReason"
{-# INLINEABLE uarrsStateReason #-}
{-# DEPRECATED stateReason "Use generic-lens or generic-optics with 'stateReason' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarrsResponseStatus :: Lens.Lens' UpdateArchiveResponse Core.Int
uarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

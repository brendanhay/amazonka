{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.DescribeMigrationTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of all attributes associated with a specific migration task.
module Network.AWS.MigrationHub.DescribeMigrationTask
    (
    -- * Creating a request
      DescribeMigrationTask (..)
    , mkDescribeMigrationTask
    -- ** Request lenses
    , dmtProgressUpdateStream
    , dmtMigrationTaskName

    -- * Destructuring the response
    , DescribeMigrationTaskResponse (..)
    , mkDescribeMigrationTaskResponse
    -- ** Response lenses
    , dmtrrsMigrationTask
    , dmtrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MigrationHub.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeMigrationTask' smart constructor.
data DescribeMigrationTask = DescribeMigrationTask'
  { progressUpdateStream :: Types.ProgressUpdateStream
    -- ^ The name of the ProgressUpdateStream. 
  , migrationTaskName :: Types.MigrationTaskName
    -- ^ The identifier given to the MigrationTask. /Do not store personal data in this field./ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMigrationTask' value with any optional fields omitted.
mkDescribeMigrationTask
    :: Types.ProgressUpdateStream -- ^ 'progressUpdateStream'
    -> Types.MigrationTaskName -- ^ 'migrationTaskName'
    -> DescribeMigrationTask
mkDescribeMigrationTask progressUpdateStream migrationTaskName
  = DescribeMigrationTask'{progressUpdateStream, migrationTaskName}

-- | The name of the ProgressUpdateStream. 
--
-- /Note:/ Consider using 'progressUpdateStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmtProgressUpdateStream :: Lens.Lens' DescribeMigrationTask Types.ProgressUpdateStream
dmtProgressUpdateStream = Lens.field @"progressUpdateStream"
{-# INLINEABLE dmtProgressUpdateStream #-}
{-# DEPRECATED progressUpdateStream "Use generic-lens or generic-optics with 'progressUpdateStream' instead"  #-}

-- | The identifier given to the MigrationTask. /Do not store personal data in this field./ 
--
-- /Note:/ Consider using 'migrationTaskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmtMigrationTaskName :: Lens.Lens' DescribeMigrationTask Types.MigrationTaskName
dmtMigrationTaskName = Lens.field @"migrationTaskName"
{-# INLINEABLE dmtMigrationTaskName #-}
{-# DEPRECATED migrationTaskName "Use generic-lens or generic-optics with 'migrationTaskName' instead"  #-}

instance Core.ToQuery DescribeMigrationTask where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeMigrationTask where
        toHeaders DescribeMigrationTask{..}
          = Core.pure
              ("X-Amz-Target", "AWSMigrationHub.DescribeMigrationTask")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeMigrationTask where
        toJSON DescribeMigrationTask{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ProgressUpdateStream" Core..= progressUpdateStream),
                  Core.Just ("MigrationTaskName" Core..= migrationTaskName)])

instance Core.AWSRequest DescribeMigrationTask where
        type Rs DescribeMigrationTask = DescribeMigrationTaskResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeMigrationTaskResponse' Core.<$>
                   (x Core..:? "MigrationTask") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeMigrationTaskResponse' smart constructor.
data DescribeMigrationTaskResponse = DescribeMigrationTaskResponse'
  { migrationTask :: Core.Maybe Types.MigrationTask
    -- ^ Object encapsulating information about the migration task.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeMigrationTaskResponse' value with any optional fields omitted.
mkDescribeMigrationTaskResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeMigrationTaskResponse
mkDescribeMigrationTaskResponse responseStatus
  = DescribeMigrationTaskResponse'{migrationTask = Core.Nothing,
                                   responseStatus}

-- | Object encapsulating information about the migration task.
--
-- /Note:/ Consider using 'migrationTask' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmtrrsMigrationTask :: Lens.Lens' DescribeMigrationTaskResponse (Core.Maybe Types.MigrationTask)
dmtrrsMigrationTask = Lens.field @"migrationTask"
{-# INLINEABLE dmtrrsMigrationTask #-}
{-# DEPRECATED migrationTask "Use generic-lens or generic-optics with 'migrationTask' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmtrrsResponseStatus :: Lens.Lens' DescribeMigrationTaskResponse Core.Int
dmtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dmtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

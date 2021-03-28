{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.AssociateDiscoveredResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a discovered resource ID from Application Discovery Service with a migration task.
module Network.AWS.MigrationHub.AssociateDiscoveredResource
    (
    -- * Creating a request
      AssociateDiscoveredResource (..)
    , mkAssociateDiscoveredResource
    -- ** Request lenses
    , adrProgressUpdateStream
    , adrMigrationTaskName
    , adrDiscoveredResource
    , adrDryRun

    -- * Destructuring the response
    , AssociateDiscoveredResourceResponse (..)
    , mkAssociateDiscoveredResourceResponse
    -- ** Response lenses
    , adrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MigrationHub.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateDiscoveredResource' smart constructor.
data AssociateDiscoveredResource = AssociateDiscoveredResource'
  { progressUpdateStream :: Types.ProgressUpdateStream
    -- ^ The name of the ProgressUpdateStream.
  , migrationTaskName :: Types.MigrationTaskName
    -- ^ The identifier given to the MigrationTask. /Do not store personal data in this field./ 
  , discoveredResource :: Types.DiscoveredResource
    -- ^ Object representing a Resource.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateDiscoveredResource' value with any optional fields omitted.
mkAssociateDiscoveredResource
    :: Types.ProgressUpdateStream -- ^ 'progressUpdateStream'
    -> Types.MigrationTaskName -- ^ 'migrationTaskName'
    -> Types.DiscoveredResource -- ^ 'discoveredResource'
    -> AssociateDiscoveredResource
mkAssociateDiscoveredResource progressUpdateStream
  migrationTaskName discoveredResource
  = AssociateDiscoveredResource'{progressUpdateStream,
                                 migrationTaskName, discoveredResource, dryRun = Core.Nothing}

-- | The name of the ProgressUpdateStream.
--
-- /Note:/ Consider using 'progressUpdateStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adrProgressUpdateStream :: Lens.Lens' AssociateDiscoveredResource Types.ProgressUpdateStream
adrProgressUpdateStream = Lens.field @"progressUpdateStream"
{-# INLINEABLE adrProgressUpdateStream #-}
{-# DEPRECATED progressUpdateStream "Use generic-lens or generic-optics with 'progressUpdateStream' instead"  #-}

-- | The identifier given to the MigrationTask. /Do not store personal data in this field./ 
--
-- /Note:/ Consider using 'migrationTaskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adrMigrationTaskName :: Lens.Lens' AssociateDiscoveredResource Types.MigrationTaskName
adrMigrationTaskName = Lens.field @"migrationTaskName"
{-# INLINEABLE adrMigrationTaskName #-}
{-# DEPRECATED migrationTaskName "Use generic-lens or generic-optics with 'migrationTaskName' instead"  #-}

-- | Object representing a Resource.
--
-- /Note:/ Consider using 'discoveredResource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adrDiscoveredResource :: Lens.Lens' AssociateDiscoveredResource Types.DiscoveredResource
adrDiscoveredResource = Lens.field @"discoveredResource"
{-# INLINEABLE adrDiscoveredResource #-}
{-# DEPRECATED discoveredResource "Use generic-lens or generic-optics with 'discoveredResource' instead"  #-}

-- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adrDryRun :: Lens.Lens' AssociateDiscoveredResource (Core.Maybe Core.Bool)
adrDryRun = Lens.field @"dryRun"
{-# INLINEABLE adrDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery AssociateDiscoveredResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AssociateDiscoveredResource where
        toHeaders AssociateDiscoveredResource{..}
          = Core.pure
              ("X-Amz-Target", "AWSMigrationHub.AssociateDiscoveredResource")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AssociateDiscoveredResource where
        toJSON AssociateDiscoveredResource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ProgressUpdateStream" Core..= progressUpdateStream),
                  Core.Just ("MigrationTaskName" Core..= migrationTaskName),
                  Core.Just ("DiscoveredResource" Core..= discoveredResource),
                  ("DryRun" Core..=) Core.<$> dryRun])

instance Core.AWSRequest AssociateDiscoveredResource where
        type Rs AssociateDiscoveredResource =
             AssociateDiscoveredResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 AssociateDiscoveredResourceResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAssociateDiscoveredResourceResponse' smart constructor.
newtype AssociateDiscoveredResourceResponse = AssociateDiscoveredResourceResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateDiscoveredResourceResponse' value with any optional fields omitted.
mkAssociateDiscoveredResourceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AssociateDiscoveredResourceResponse
mkAssociateDiscoveredResourceResponse responseStatus
  = AssociateDiscoveredResourceResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adrrrsResponseStatus :: Lens.Lens' AssociateDiscoveredResourceResponse Core.Int
adrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE adrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

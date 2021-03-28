{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.DisassociateDiscoveredResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociate an Application Discovery Service discovered resource from a migration task.
module Network.AWS.MigrationHub.DisassociateDiscoveredResource
    (
    -- * Creating a request
      DisassociateDiscoveredResource (..)
    , mkDisassociateDiscoveredResource
    -- ** Request lenses
    , ddrProgressUpdateStream
    , ddrMigrationTaskName
    , ddrConfigurationId
    , ddrDryRun

    -- * Destructuring the response
    , DisassociateDiscoveredResourceResponse (..)
    , mkDisassociateDiscoveredResourceResponse
    -- ** Response lenses
    , ddrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MigrationHub.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateDiscoveredResource' smart constructor.
data DisassociateDiscoveredResource = DisassociateDiscoveredResource'
  { progressUpdateStream :: Types.ProgressUpdateStream
    -- ^ The name of the ProgressUpdateStream.
  , migrationTaskName :: Types.MigrationTaskName
    -- ^ The identifier given to the MigrationTask. /Do not store personal data in this field./ 
  , configurationId :: Types.ConfigurationId
    -- ^ ConfigurationId of the Application Discovery Service resource to be disassociated.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateDiscoveredResource' value with any optional fields omitted.
mkDisassociateDiscoveredResource
    :: Types.ProgressUpdateStream -- ^ 'progressUpdateStream'
    -> Types.MigrationTaskName -- ^ 'migrationTaskName'
    -> Types.ConfigurationId -- ^ 'configurationId'
    -> DisassociateDiscoveredResource
mkDisassociateDiscoveredResource progressUpdateStream
  migrationTaskName configurationId
  = DisassociateDiscoveredResource'{progressUpdateStream,
                                    migrationTaskName, configurationId, dryRun = Core.Nothing}

-- | The name of the ProgressUpdateStream.
--
-- /Note:/ Consider using 'progressUpdateStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrProgressUpdateStream :: Lens.Lens' DisassociateDiscoveredResource Types.ProgressUpdateStream
ddrProgressUpdateStream = Lens.field @"progressUpdateStream"
{-# INLINEABLE ddrProgressUpdateStream #-}
{-# DEPRECATED progressUpdateStream "Use generic-lens or generic-optics with 'progressUpdateStream' instead"  #-}

-- | The identifier given to the MigrationTask. /Do not store personal data in this field./ 
--
-- /Note:/ Consider using 'migrationTaskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrMigrationTaskName :: Lens.Lens' DisassociateDiscoveredResource Types.MigrationTaskName
ddrMigrationTaskName = Lens.field @"migrationTaskName"
{-# INLINEABLE ddrMigrationTaskName #-}
{-# DEPRECATED migrationTaskName "Use generic-lens or generic-optics with 'migrationTaskName' instead"  #-}

-- | ConfigurationId of the Application Discovery Service resource to be disassociated.
--
-- /Note:/ Consider using 'configurationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrConfigurationId :: Lens.Lens' DisassociateDiscoveredResource Types.ConfigurationId
ddrConfigurationId = Lens.field @"configurationId"
{-# INLINEABLE ddrConfigurationId #-}
{-# DEPRECATED configurationId "Use generic-lens or generic-optics with 'configurationId' instead"  #-}

-- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrDryRun :: Lens.Lens' DisassociateDiscoveredResource (Core.Maybe Core.Bool)
ddrDryRun = Lens.field @"dryRun"
{-# INLINEABLE ddrDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DisassociateDiscoveredResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DisassociateDiscoveredResource where
        toHeaders DisassociateDiscoveredResource{..}
          = Core.pure
              ("X-Amz-Target", "AWSMigrationHub.DisassociateDiscoveredResource")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DisassociateDiscoveredResource where
        toJSON DisassociateDiscoveredResource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ProgressUpdateStream" Core..= progressUpdateStream),
                  Core.Just ("MigrationTaskName" Core..= migrationTaskName),
                  Core.Just ("ConfigurationId" Core..= configurationId),
                  ("DryRun" Core..=) Core.<$> dryRun])

instance Core.AWSRequest DisassociateDiscoveredResource where
        type Rs DisassociateDiscoveredResource =
             DisassociateDiscoveredResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DisassociateDiscoveredResourceResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisassociateDiscoveredResourceResponse' smart constructor.
newtype DisassociateDiscoveredResourceResponse = DisassociateDiscoveredResourceResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateDiscoveredResourceResponse' value with any optional fields omitted.
mkDisassociateDiscoveredResourceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisassociateDiscoveredResourceResponse
mkDisassociateDiscoveredResourceResponse responseStatus
  = DisassociateDiscoveredResourceResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrrsResponseStatus :: Lens.Lens' DisassociateDiscoveredResourceResponse Core.Int
ddrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

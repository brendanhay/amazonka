{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DisassociateDiscoveredResource (..),
    mkDisassociateDiscoveredResource,

    -- ** Request lenses
    ddrProgressUpdateStream,
    ddrMigrationTaskName,
    ddrConfigurationId,
    ddrDryRun,

    -- * Destructuring the response
    DisassociateDiscoveredResourceResponse (..),
    mkDisassociateDiscoveredResourceResponse,

    -- ** Response lenses
    ddrrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MigrationHub.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateDiscoveredResource' smart constructor.
data DisassociateDiscoveredResource = DisassociateDiscoveredResource'
  { -- | The name of the ProgressUpdateStream.
    progressUpdateStream :: Types.ProgressUpdateStream,
    -- | The identifier given to the MigrationTask. /Do not store personal data in this field./
    migrationTaskName :: Types.MigrationTaskName,
    -- | ConfigurationId of the Application Discovery Service resource to be disassociated.
    configurationId :: Types.ConfigurationId,
    -- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateDiscoveredResource' value with any optional fields omitted.
mkDisassociateDiscoveredResource ::
  -- | 'progressUpdateStream'
  Types.ProgressUpdateStream ->
  -- | 'migrationTaskName'
  Types.MigrationTaskName ->
  -- | 'configurationId'
  Types.ConfigurationId ->
  DisassociateDiscoveredResource
mkDisassociateDiscoveredResource
  progressUpdateStream
  migrationTaskName
  configurationId =
    DisassociateDiscoveredResource'
      { progressUpdateStream,
        migrationTaskName,
        configurationId,
        dryRun = Core.Nothing
      }

-- | The name of the ProgressUpdateStream.
--
-- /Note:/ Consider using 'progressUpdateStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrProgressUpdateStream :: Lens.Lens' DisassociateDiscoveredResource Types.ProgressUpdateStream
ddrProgressUpdateStream = Lens.field @"progressUpdateStream"
{-# DEPRECATED ddrProgressUpdateStream "Use generic-lens or generic-optics with 'progressUpdateStream' instead." #-}

-- | The identifier given to the MigrationTask. /Do not store personal data in this field./
--
-- /Note:/ Consider using 'migrationTaskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrMigrationTaskName :: Lens.Lens' DisassociateDiscoveredResource Types.MigrationTaskName
ddrMigrationTaskName = Lens.field @"migrationTaskName"
{-# DEPRECATED ddrMigrationTaskName "Use generic-lens or generic-optics with 'migrationTaskName' instead." #-}

-- | ConfigurationId of the Application Discovery Service resource to be disassociated.
--
-- /Note:/ Consider using 'configurationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrConfigurationId :: Lens.Lens' DisassociateDiscoveredResource Types.ConfigurationId
ddrConfigurationId = Lens.field @"configurationId"
{-# DEPRECATED ddrConfigurationId "Use generic-lens or generic-optics with 'configurationId' instead." #-}

-- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrDryRun :: Lens.Lens' DisassociateDiscoveredResource (Core.Maybe Core.Bool)
ddrDryRun = Lens.field @"dryRun"
{-# DEPRECATED ddrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.FromJSON DisassociateDiscoveredResource where
  toJSON DisassociateDiscoveredResource {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ProgressUpdateStream" Core..= progressUpdateStream),
            Core.Just ("MigrationTaskName" Core..= migrationTaskName),
            Core.Just ("ConfigurationId" Core..= configurationId),
            ("DryRun" Core..=) Core.<$> dryRun
          ]
      )

instance Core.AWSRequest DisassociateDiscoveredResource where
  type
    Rs DisassociateDiscoveredResource =
      DisassociateDiscoveredResourceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSMigrationHub.DisassociateDiscoveredResource")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateDiscoveredResourceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDisassociateDiscoveredResourceResponse' smart constructor.
newtype DisassociateDiscoveredResourceResponse = DisassociateDiscoveredResourceResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateDiscoveredResourceResponse' value with any optional fields omitted.
mkDisassociateDiscoveredResourceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DisassociateDiscoveredResourceResponse
mkDisassociateDiscoveredResourceResponse responseStatus =
  DisassociateDiscoveredResourceResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrrsResponseStatus :: Lens.Lens' DisassociateDiscoveredResourceResponse Core.Int
ddrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    AssociateDiscoveredResource (..),
    mkAssociateDiscoveredResource,

    -- ** Request lenses
    adrProgressUpdateStream,
    adrMigrationTaskName,
    adrDiscoveredResource,
    adrDryRun,

    -- * Destructuring the response
    AssociateDiscoveredResourceResponse (..),
    mkAssociateDiscoveredResourceResponse,

    -- ** Response lenses
    adrrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MigrationHub.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateDiscoveredResource' smart constructor.
data AssociateDiscoveredResource = AssociateDiscoveredResource'
  { -- | The name of the ProgressUpdateStream.
    progressUpdateStream :: Types.ProgressUpdateStream,
    -- | The identifier given to the MigrationTask. /Do not store personal data in this field./
    migrationTaskName :: Types.MigrationTaskName,
    -- | Object representing a Resource.
    discoveredResource :: Types.DiscoveredResource,
    -- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateDiscoveredResource' value with any optional fields omitted.
mkAssociateDiscoveredResource ::
  -- | 'progressUpdateStream'
  Types.ProgressUpdateStream ->
  -- | 'migrationTaskName'
  Types.MigrationTaskName ->
  -- | 'discoveredResource'
  Types.DiscoveredResource ->
  AssociateDiscoveredResource
mkAssociateDiscoveredResource
  progressUpdateStream
  migrationTaskName
  discoveredResource =
    AssociateDiscoveredResource'
      { progressUpdateStream,
        migrationTaskName,
        discoveredResource,
        dryRun = Core.Nothing
      }

-- | The name of the ProgressUpdateStream.
--
-- /Note:/ Consider using 'progressUpdateStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adrProgressUpdateStream :: Lens.Lens' AssociateDiscoveredResource Types.ProgressUpdateStream
adrProgressUpdateStream = Lens.field @"progressUpdateStream"
{-# DEPRECATED adrProgressUpdateStream "Use generic-lens or generic-optics with 'progressUpdateStream' instead." #-}

-- | The identifier given to the MigrationTask. /Do not store personal data in this field./
--
-- /Note:/ Consider using 'migrationTaskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adrMigrationTaskName :: Lens.Lens' AssociateDiscoveredResource Types.MigrationTaskName
adrMigrationTaskName = Lens.field @"migrationTaskName"
{-# DEPRECATED adrMigrationTaskName "Use generic-lens or generic-optics with 'migrationTaskName' instead." #-}

-- | Object representing a Resource.
--
-- /Note:/ Consider using 'discoveredResource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adrDiscoveredResource :: Lens.Lens' AssociateDiscoveredResource Types.DiscoveredResource
adrDiscoveredResource = Lens.field @"discoveredResource"
{-# DEPRECATED adrDiscoveredResource "Use generic-lens or generic-optics with 'discoveredResource' instead." #-}

-- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adrDryRun :: Lens.Lens' AssociateDiscoveredResource (Core.Maybe Core.Bool)
adrDryRun = Lens.field @"dryRun"
{-# DEPRECATED adrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.FromJSON AssociateDiscoveredResource where
  toJSON AssociateDiscoveredResource {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ProgressUpdateStream" Core..= progressUpdateStream),
            Core.Just ("MigrationTaskName" Core..= migrationTaskName),
            Core.Just ("DiscoveredResource" Core..= discoveredResource),
            ("DryRun" Core..=) Core.<$> dryRun
          ]
      )

instance Core.AWSRequest AssociateDiscoveredResource where
  type
    Rs AssociateDiscoveredResource =
      AssociateDiscoveredResourceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSMigrationHub.AssociateDiscoveredResource")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateDiscoveredResourceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAssociateDiscoveredResourceResponse' smart constructor.
newtype AssociateDiscoveredResourceResponse = AssociateDiscoveredResourceResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateDiscoveredResourceResponse' value with any optional fields omitted.
mkAssociateDiscoveredResourceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AssociateDiscoveredResourceResponse
mkAssociateDiscoveredResourceResponse responseStatus =
  AssociateDiscoveredResourceResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adrrrsResponseStatus :: Lens.Lens' AssociateDiscoveredResourceResponse Core.Int
adrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED adrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

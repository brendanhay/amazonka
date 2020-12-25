{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ModifyGlobalCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify a setting for an Amazon Aurora global cluster. You can change one or more database configuration parameters by specifying these parameters and the new values in the request. For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./
module Network.AWS.RDS.ModifyGlobalCluster
  ( -- * Creating a request
    ModifyGlobalCluster (..),
    mkModifyGlobalCluster,

    -- ** Request lenses
    mgcDeletionProtection,
    mgcGlobalClusterIdentifier,
    mgcNewGlobalClusterIdentifier,

    -- * Destructuring the response
    ModifyGlobalClusterResponse (..),
    mkModifyGlobalClusterResponse,

    -- ** Response lenses
    mgcrrsGlobalCluster,
    mgcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyGlobalCluster' smart constructor.
data ModifyGlobalCluster = ModifyGlobalCluster'
  { -- | Indicates if the global database cluster has deletion protection enabled. The global database cluster can't be deleted when deletion protection is enabled.
    deletionProtection :: Core.Maybe Core.Bool,
    -- | The DB cluster identifier for the global cluster being modified. This parameter isn't case-sensitive.
    --
    -- Constraints:
    --
    --     * Must match the identifier of an existing global database cluster.
    globalClusterIdentifier :: Core.Maybe Types.String,
    -- | The new cluster identifier for the global database cluster when modifying a global database cluster. This value is stored as a lowercase string.
    --
    -- Constraints:
    --
    --     * Must contain from 1 to 63 letters, numbers, or hyphens
    --
    --
    --     * The first character must be a letter
    --
    --
    --     * Can't end with a hyphen or contain two consecutive hyphens
    --
    --
    -- Example: @my-cluster2@
    newGlobalClusterIdentifier :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyGlobalCluster' value with any optional fields omitted.
mkModifyGlobalCluster ::
  ModifyGlobalCluster
mkModifyGlobalCluster =
  ModifyGlobalCluster'
    { deletionProtection = Core.Nothing,
      globalClusterIdentifier = Core.Nothing,
      newGlobalClusterIdentifier = Core.Nothing
    }

-- | Indicates if the global database cluster has deletion protection enabled. The global database cluster can't be deleted when deletion protection is enabled.
--
-- /Note:/ Consider using 'deletionProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgcDeletionProtection :: Lens.Lens' ModifyGlobalCluster (Core.Maybe Core.Bool)
mgcDeletionProtection = Lens.field @"deletionProtection"
{-# DEPRECATED mgcDeletionProtection "Use generic-lens or generic-optics with 'deletionProtection' instead." #-}

-- | The DB cluster identifier for the global cluster being modified. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * Must match the identifier of an existing global database cluster.
--
--
--
-- /Note:/ Consider using 'globalClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgcGlobalClusterIdentifier :: Lens.Lens' ModifyGlobalCluster (Core.Maybe Types.String)
mgcGlobalClusterIdentifier = Lens.field @"globalClusterIdentifier"
{-# DEPRECATED mgcGlobalClusterIdentifier "Use generic-lens or generic-optics with 'globalClusterIdentifier' instead." #-}

-- | The new cluster identifier for the global database cluster when modifying a global database cluster. This value is stored as a lowercase string.
--
-- Constraints:
--
--     * Must contain from 1 to 63 letters, numbers, or hyphens
--
--
--     * The first character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
-- Example: @my-cluster2@
--
-- /Note:/ Consider using 'newGlobalClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgcNewGlobalClusterIdentifier :: Lens.Lens' ModifyGlobalCluster (Core.Maybe Types.String)
mgcNewGlobalClusterIdentifier = Lens.field @"newGlobalClusterIdentifier"
{-# DEPRECATED mgcNewGlobalClusterIdentifier "Use generic-lens or generic-optics with 'newGlobalClusterIdentifier' instead." #-}

instance Core.AWSRequest ModifyGlobalCluster where
  type Rs ModifyGlobalCluster = ModifyGlobalClusterResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ModifyGlobalCluster")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> ( Core.toQueryValue "DeletionProtection"
                            Core.<$> deletionProtection
                        )
                Core.<> ( Core.toQueryValue "GlobalClusterIdentifier"
                            Core.<$> globalClusterIdentifier
                        )
                Core.<> ( Core.toQueryValue "NewGlobalClusterIdentifier"
                            Core.<$> newGlobalClusterIdentifier
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "ModifyGlobalClusterResult"
      ( \s h x ->
          ModifyGlobalClusterResponse'
            Core.<$> (x Core..@? "GlobalCluster")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyGlobalClusterResponse' smart constructor.
data ModifyGlobalClusterResponse = ModifyGlobalClusterResponse'
  { globalCluster :: Core.Maybe Types.GlobalCluster,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyGlobalClusterResponse' value with any optional fields omitted.
mkModifyGlobalClusterResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyGlobalClusterResponse
mkModifyGlobalClusterResponse responseStatus =
  ModifyGlobalClusterResponse'
    { globalCluster = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'globalCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgcrrsGlobalCluster :: Lens.Lens' ModifyGlobalClusterResponse (Core.Maybe Types.GlobalCluster)
mgcrrsGlobalCluster = Lens.field @"globalCluster"
{-# DEPRECATED mgcrrsGlobalCluster "Use generic-lens or generic-optics with 'globalCluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgcrrsResponseStatus :: Lens.Lens' ModifyGlobalClusterResponse Core.Int
mgcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mgcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

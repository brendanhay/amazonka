{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ModifyGlobalCluster (..)
    , mkModifyGlobalCluster
    -- ** Request lenses
    , mgcDeletionProtection
    , mgcGlobalClusterIdentifier
    , mgcNewGlobalClusterIdentifier

    -- * Destructuring the response
    , ModifyGlobalClusterResponse (..)
    , mkModifyGlobalClusterResponse
    -- ** Response lenses
    , mgcrrsGlobalCluster
    , mgcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyGlobalCluster' smart constructor.
data ModifyGlobalCluster = ModifyGlobalCluster'
  { deletionProtection :: Core.Maybe Core.Bool
    -- ^ Indicates if the global database cluster has deletion protection enabled. The global database cluster can't be deleted when deletion protection is enabled. 
  , globalClusterIdentifier :: Core.Maybe Core.Text
    -- ^ The DB cluster identifier for the global cluster being modified. This parameter isn't case-sensitive. 
--
-- Constraints:
--
--     * Must match the identifier of an existing global database cluster.
--
--
  , newGlobalClusterIdentifier :: Core.Maybe Core.Text
    -- ^ The new cluster identifier for the global database cluster when modifying a global database cluster. This value is stored as a lowercase string. 
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyGlobalCluster' value with any optional fields omitted.
mkModifyGlobalCluster
    :: ModifyGlobalCluster
mkModifyGlobalCluster
  = ModifyGlobalCluster'{deletionProtection = Core.Nothing,
                         globalClusterIdentifier = Core.Nothing,
                         newGlobalClusterIdentifier = Core.Nothing}

-- | Indicates if the global database cluster has deletion protection enabled. The global database cluster can't be deleted when deletion protection is enabled. 
--
-- /Note:/ Consider using 'deletionProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgcDeletionProtection :: Lens.Lens' ModifyGlobalCluster (Core.Maybe Core.Bool)
mgcDeletionProtection = Lens.field @"deletionProtection"
{-# INLINEABLE mgcDeletionProtection #-}
{-# DEPRECATED deletionProtection "Use generic-lens or generic-optics with 'deletionProtection' instead"  #-}

-- | The DB cluster identifier for the global cluster being modified. This parameter isn't case-sensitive. 
--
-- Constraints:
--
--     * Must match the identifier of an existing global database cluster.
--
--
--
-- /Note:/ Consider using 'globalClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgcGlobalClusterIdentifier :: Lens.Lens' ModifyGlobalCluster (Core.Maybe Core.Text)
mgcGlobalClusterIdentifier = Lens.field @"globalClusterIdentifier"
{-# INLINEABLE mgcGlobalClusterIdentifier #-}
{-# DEPRECATED globalClusterIdentifier "Use generic-lens or generic-optics with 'globalClusterIdentifier' instead"  #-}

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
mgcNewGlobalClusterIdentifier :: Lens.Lens' ModifyGlobalCluster (Core.Maybe Core.Text)
mgcNewGlobalClusterIdentifier = Lens.field @"newGlobalClusterIdentifier"
{-# INLINEABLE mgcNewGlobalClusterIdentifier #-}
{-# DEPRECATED newGlobalClusterIdentifier "Use generic-lens or generic-optics with 'newGlobalClusterIdentifier' instead"  #-}

instance Core.ToQuery ModifyGlobalCluster where
        toQuery ModifyGlobalCluster{..}
          = Core.toQueryPair "Action" ("ModifyGlobalCluster" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DeletionProtection")
                deletionProtection
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "GlobalClusterIdentifier")
                globalClusterIdentifier
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "NewGlobalClusterIdentifier")
                newGlobalClusterIdentifier

instance Core.ToHeaders ModifyGlobalCluster where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyGlobalCluster where
        type Rs ModifyGlobalCluster = ModifyGlobalClusterResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "ModifyGlobalClusterResult"
              (\ s h x ->
                 ModifyGlobalClusterResponse' Core.<$>
                   (x Core..@? "GlobalCluster") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyGlobalClusterResponse' smart constructor.
data ModifyGlobalClusterResponse = ModifyGlobalClusterResponse'
  { globalCluster :: Core.Maybe Types.GlobalCluster
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyGlobalClusterResponse' value with any optional fields omitted.
mkModifyGlobalClusterResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyGlobalClusterResponse
mkModifyGlobalClusterResponse responseStatus
  = ModifyGlobalClusterResponse'{globalCluster = Core.Nothing,
                                 responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'globalCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgcrrsGlobalCluster :: Lens.Lens' ModifyGlobalClusterResponse (Core.Maybe Types.GlobalCluster)
mgcrrsGlobalCluster = Lens.field @"globalCluster"
{-# INLINEABLE mgcrrsGlobalCluster #-}
{-# DEPRECATED globalCluster "Use generic-lens or generic-optics with 'globalCluster' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgcrrsResponseStatus :: Lens.Lens' ModifyGlobalClusterResponse Core.Int
mgcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mgcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

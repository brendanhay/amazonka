{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.ModifyClusterDbRevision
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the database revision of a cluster. The database revision is a unique revision of the database running in a cluster.
module Network.AWS.Redshift.ModifyClusterDbRevision
    (
    -- * Creating a request
      ModifyClusterDbRevision (..)
    , mkModifyClusterDbRevision
    -- ** Request lenses
    , mcdrClusterIdentifier
    , mcdrRevisionTarget

    -- * Destructuring the response
    , ModifyClusterDbRevisionResponse (..)
    , mkModifyClusterDbRevisionResponse
    -- ** Response lenses
    , mcdrrrsCluster
    , mcdrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyClusterDbRevision' smart constructor.
data ModifyClusterDbRevision = ModifyClusterDbRevision'
  { clusterIdentifier :: Core.Text
    -- ^ The unique identifier of a cluster whose database revision you want to modify. 
--
-- Example: @examplecluster@ 
  , revisionTarget :: Core.Text
    -- ^ The identifier of the database revision. You can retrieve this value from the response to the 'DescribeClusterDbRevisions' request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyClusterDbRevision' value with any optional fields omitted.
mkModifyClusterDbRevision
    :: Core.Text -- ^ 'clusterIdentifier'
    -> Core.Text -- ^ 'revisionTarget'
    -> ModifyClusterDbRevision
mkModifyClusterDbRevision clusterIdentifier revisionTarget
  = ModifyClusterDbRevision'{clusterIdentifier, revisionTarget}

-- | The unique identifier of a cluster whose database revision you want to modify. 
--
-- Example: @examplecluster@ 
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcdrClusterIdentifier :: Lens.Lens' ModifyClusterDbRevision Core.Text
mcdrClusterIdentifier = Lens.field @"clusterIdentifier"
{-# INLINEABLE mcdrClusterIdentifier #-}
{-# DEPRECATED clusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead"  #-}

-- | The identifier of the database revision. You can retrieve this value from the response to the 'DescribeClusterDbRevisions' request.
--
-- /Note:/ Consider using 'revisionTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcdrRevisionTarget :: Lens.Lens' ModifyClusterDbRevision Core.Text
mcdrRevisionTarget = Lens.field @"revisionTarget"
{-# INLINEABLE mcdrRevisionTarget #-}
{-# DEPRECATED revisionTarget "Use generic-lens or generic-optics with 'revisionTarget' instead"  #-}

instance Core.ToQuery ModifyClusterDbRevision where
        toQuery ModifyClusterDbRevision{..}
          = Core.toQueryPair "Action"
              ("ModifyClusterDbRevision" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ClusterIdentifier" clusterIdentifier
              Core.<> Core.toQueryPair "RevisionTarget" revisionTarget

instance Core.ToHeaders ModifyClusterDbRevision where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyClusterDbRevision where
        type Rs ModifyClusterDbRevision = ModifyClusterDbRevisionResponse
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
          = Response.receiveXMLWrapper "ModifyClusterDbRevisionResult"
              (\ s h x ->
                 ModifyClusterDbRevisionResponse' Core.<$>
                   (x Core..@? "Cluster") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyClusterDbRevisionResponse' smart constructor.
data ModifyClusterDbRevisionResponse = ModifyClusterDbRevisionResponse'
  { cluster :: Core.Maybe Types.Cluster
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ModifyClusterDbRevisionResponse' value with any optional fields omitted.
mkModifyClusterDbRevisionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyClusterDbRevisionResponse
mkModifyClusterDbRevisionResponse responseStatus
  = ModifyClusterDbRevisionResponse'{cluster = Core.Nothing,
                                     responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcdrrrsCluster :: Lens.Lens' ModifyClusterDbRevisionResponse (Core.Maybe Types.Cluster)
mcdrrrsCluster = Lens.field @"cluster"
{-# INLINEABLE mcdrrrsCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcdrrrsResponseStatus :: Lens.Lens' ModifyClusterDbRevisionResponse Core.Int
mcdrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mcdrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

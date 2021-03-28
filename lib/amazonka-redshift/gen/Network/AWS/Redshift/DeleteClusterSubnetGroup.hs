{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DeleteClusterSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified cluster subnet group.
module Network.AWS.Redshift.DeleteClusterSubnetGroup
    (
    -- * Creating a request
      DeleteClusterSubnetGroup (..)
    , mkDeleteClusterSubnetGroup
    -- ** Request lenses
    , dcsgClusterSubnetGroupName

    -- * Destructuring the response
    , DeleteClusterSubnetGroupResponse (..)
    , mkDeleteClusterSubnetGroupResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDeleteClusterSubnetGroup' smart constructor.
newtype DeleteClusterSubnetGroup = DeleteClusterSubnetGroup'
  { clusterSubnetGroupName :: Core.Text
    -- ^ The name of the cluster subnet group name to be deleted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteClusterSubnetGroup' value with any optional fields omitted.
mkDeleteClusterSubnetGroup
    :: Core.Text -- ^ 'clusterSubnetGroupName'
    -> DeleteClusterSubnetGroup
mkDeleteClusterSubnetGroup clusterSubnetGroupName
  = DeleteClusterSubnetGroup'{clusterSubnetGroupName}

-- | The name of the cluster subnet group name to be deleted.
--
-- /Note:/ Consider using 'clusterSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgClusterSubnetGroupName :: Lens.Lens' DeleteClusterSubnetGroup Core.Text
dcsgClusterSubnetGroupName = Lens.field @"clusterSubnetGroupName"
{-# INLINEABLE dcsgClusterSubnetGroupName #-}
{-# DEPRECATED clusterSubnetGroupName "Use generic-lens or generic-optics with 'clusterSubnetGroupName' instead"  #-}

instance Core.ToQuery DeleteClusterSubnetGroup where
        toQuery DeleteClusterSubnetGroup{..}
          = Core.toQueryPair "Action"
              ("DeleteClusterSubnetGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "ClusterSubnetGroupName" clusterSubnetGroupName

instance Core.ToHeaders DeleteClusterSubnetGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteClusterSubnetGroup where
        type Rs DeleteClusterSubnetGroup = DeleteClusterSubnetGroupResponse
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
          = Response.receiveNull DeleteClusterSubnetGroupResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteClusterSubnetGroupResponse' smart constructor.
data DeleteClusterSubnetGroupResponse = DeleteClusterSubnetGroupResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteClusterSubnetGroupResponse' value with any optional fields omitted.
mkDeleteClusterSubnetGroupResponse
    :: DeleteClusterSubnetGroupResponse
mkDeleteClusterSubnetGroupResponse
  = DeleteClusterSubnetGroupResponse'

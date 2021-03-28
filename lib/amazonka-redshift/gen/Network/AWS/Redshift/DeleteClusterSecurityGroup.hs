{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DeleteClusterSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon Redshift security group.
--
-- For information about managing security groups, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-security-groups.html Amazon Redshift Cluster Security Groups> in the /Amazon Redshift Cluster Management Guide/ .
module Network.AWS.Redshift.DeleteClusterSecurityGroup
    (
    -- * Creating a request
      DeleteClusterSecurityGroup (..)
    , mkDeleteClusterSecurityGroup
    -- ** Request lenses
    , dClusterSecurityGroupName

    -- * Destructuring the response
    , DeleteClusterSecurityGroupResponse (..)
    , mkDeleteClusterSecurityGroupResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDeleteClusterSecurityGroup' smart constructor.
newtype DeleteClusterSecurityGroup = DeleteClusterSecurityGroup'
  { clusterSecurityGroupName :: Core.Text
    -- ^ The name of the cluster security group to be deleted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteClusterSecurityGroup' value with any optional fields omitted.
mkDeleteClusterSecurityGroup
    :: Core.Text -- ^ 'clusterSecurityGroupName'
    -> DeleteClusterSecurityGroup
mkDeleteClusterSecurityGroup clusterSecurityGroupName
  = DeleteClusterSecurityGroup'{clusterSecurityGroupName}

-- | The name of the cluster security group to be deleted.
--
-- /Note:/ Consider using 'clusterSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dClusterSecurityGroupName :: Lens.Lens' DeleteClusterSecurityGroup Core.Text
dClusterSecurityGroupName = Lens.field @"clusterSecurityGroupName"
{-# INLINEABLE dClusterSecurityGroupName #-}
{-# DEPRECATED clusterSecurityGroupName "Use generic-lens or generic-optics with 'clusterSecurityGroupName' instead"  #-}

instance Core.ToQuery DeleteClusterSecurityGroup where
        toQuery DeleteClusterSecurityGroup{..}
          = Core.toQueryPair "Action"
              ("DeleteClusterSecurityGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "ClusterSecurityGroupName"
                clusterSecurityGroupName

instance Core.ToHeaders DeleteClusterSecurityGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteClusterSecurityGroup where
        type Rs DeleteClusterSecurityGroup =
             DeleteClusterSecurityGroupResponse
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
          = Response.receiveNull DeleteClusterSecurityGroupResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteClusterSecurityGroupResponse' smart constructor.
data DeleteClusterSecurityGroupResponse = DeleteClusterSecurityGroupResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteClusterSecurityGroupResponse' value with any optional fields omitted.
mkDeleteClusterSecurityGroupResponse
    :: DeleteClusterSecurityGroupResponse
mkDeleteClusterSecurityGroupResponse
  = DeleteClusterSecurityGroupResponse'

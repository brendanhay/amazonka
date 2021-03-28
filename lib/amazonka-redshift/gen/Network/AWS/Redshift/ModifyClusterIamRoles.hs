{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.ModifyClusterIamRoles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the list of AWS Identity and Access Management (IAM) roles that can be used by the cluster to access other AWS services.
--
-- A cluster can have up to 10 IAM roles associated at any time.
module Network.AWS.Redshift.ModifyClusterIamRoles
    (
    -- * Creating a request
      ModifyClusterIamRoles (..)
    , mkModifyClusterIamRoles
    -- ** Request lenses
    , mcirClusterIdentifier
    , mcirAddIamRoles
    , mcirRemoveIamRoles

    -- * Destructuring the response
    , ModifyClusterIamRolesResponse (..)
    , mkModifyClusterIamRolesResponse
    -- ** Response lenses
    , mcirrrsCluster
    , mcirrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkModifyClusterIamRoles' smart constructor.
data ModifyClusterIamRoles = ModifyClusterIamRoles'
  { clusterIdentifier :: Core.Text
    -- ^ The unique identifier of the cluster for which you want to associate or disassociate IAM roles.
  , addIamRoles :: Core.Maybe [Core.Text]
    -- ^ Zero or more IAM roles to associate with the cluster. The roles must be in their Amazon Resource Name (ARN) format. You can associate up to 10 IAM roles with a single cluster in a single request.
  , removeIamRoles :: Core.Maybe [Core.Text]
    -- ^ Zero or more IAM roles in ARN format to disassociate from the cluster. You can disassociate up to 10 IAM roles from a single cluster in a single request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyClusterIamRoles' value with any optional fields omitted.
mkModifyClusterIamRoles
    :: Core.Text -- ^ 'clusterIdentifier'
    -> ModifyClusterIamRoles
mkModifyClusterIamRoles clusterIdentifier
  = ModifyClusterIamRoles'{clusterIdentifier,
                           addIamRoles = Core.Nothing, removeIamRoles = Core.Nothing}

-- | The unique identifier of the cluster for which you want to associate or disassociate IAM roles.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcirClusterIdentifier :: Lens.Lens' ModifyClusterIamRoles Core.Text
mcirClusterIdentifier = Lens.field @"clusterIdentifier"
{-# INLINEABLE mcirClusterIdentifier #-}
{-# DEPRECATED clusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead"  #-}

-- | Zero or more IAM roles to associate with the cluster. The roles must be in their Amazon Resource Name (ARN) format. You can associate up to 10 IAM roles with a single cluster in a single request.
--
-- /Note:/ Consider using 'addIamRoles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcirAddIamRoles :: Lens.Lens' ModifyClusterIamRoles (Core.Maybe [Core.Text])
mcirAddIamRoles = Lens.field @"addIamRoles"
{-# INLINEABLE mcirAddIamRoles #-}
{-# DEPRECATED addIamRoles "Use generic-lens or generic-optics with 'addIamRoles' instead"  #-}

-- | Zero or more IAM roles in ARN format to disassociate from the cluster. You can disassociate up to 10 IAM roles from a single cluster in a single request.
--
-- /Note:/ Consider using 'removeIamRoles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcirRemoveIamRoles :: Lens.Lens' ModifyClusterIamRoles (Core.Maybe [Core.Text])
mcirRemoveIamRoles = Lens.field @"removeIamRoles"
{-# INLINEABLE mcirRemoveIamRoles #-}
{-# DEPRECATED removeIamRoles "Use generic-lens or generic-optics with 'removeIamRoles' instead"  #-}

instance Core.ToQuery ModifyClusterIamRoles where
        toQuery ModifyClusterIamRoles{..}
          = Core.toQueryPair "Action" ("ModifyClusterIamRoles" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ClusterIdentifier" clusterIdentifier
              Core.<>
              Core.toQueryPair "AddIamRoles"
                (Core.maybe Core.mempty (Core.toQueryList "IamRoleArn")
                   addIamRoles)
              Core.<>
              Core.toQueryPair "RemoveIamRoles"
                (Core.maybe Core.mempty (Core.toQueryList "IamRoleArn")
                   removeIamRoles)

instance Core.ToHeaders ModifyClusterIamRoles where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyClusterIamRoles where
        type Rs ModifyClusterIamRoles = ModifyClusterIamRolesResponse
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
          = Response.receiveXMLWrapper "ModifyClusterIamRolesResult"
              (\ s h x ->
                 ModifyClusterIamRolesResponse' Core.<$>
                   (x Core..@? "Cluster") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyClusterIamRolesResponse' smart constructor.
data ModifyClusterIamRolesResponse = ModifyClusterIamRolesResponse'
  { cluster :: Core.Maybe Types.Cluster
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ModifyClusterIamRolesResponse' value with any optional fields omitted.
mkModifyClusterIamRolesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyClusterIamRolesResponse
mkModifyClusterIamRolesResponse responseStatus
  = ModifyClusterIamRolesResponse'{cluster = Core.Nothing,
                                   responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcirrrsCluster :: Lens.Lens' ModifyClusterIamRolesResponse (Core.Maybe Types.Cluster)
mcirrrsCluster = Lens.field @"cluster"
{-# INLINEABLE mcirrrsCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcirrrsResponseStatus :: Lens.Lens' ModifyClusterIamRolesResponse Core.Int
mcirrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mcirrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

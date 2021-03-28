{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.UpdateCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the settings for a DAX cluster. You can use this action to change one or more cluster configuration parameters by specifying the parameters and the new values.
module Network.AWS.DAX.UpdateCluster
    (
    -- * Creating a request
      UpdateCluster (..)
    , mkUpdateCluster
    -- ** Request lenses
    , ucClusterName
    , ucDescription
    , ucNotificationTopicArn
    , ucNotificationTopicStatus
    , ucParameterGroupName
    , ucPreferredMaintenanceWindow
    , ucSecurityGroupIds

    -- * Destructuring the response
    , UpdateClusterResponse (..)
    , mkUpdateClusterResponse
    -- ** Response lenses
    , ucrrsCluster
    , ucrrsResponseStatus
    ) where

import qualified Network.AWS.DAX.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateCluster' smart constructor.
data UpdateCluster = UpdateCluster'
  { clusterName :: Core.Text
    -- ^ The name of the DAX cluster to be modified.
  , description :: Core.Maybe Core.Text
    -- ^ A description of the changes being made to the cluster.
  , notificationTopicArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) that identifies the topic.
  , notificationTopicStatus :: Core.Maybe Core.Text
    -- ^ The current state of the topic.
  , parameterGroupName :: Core.Maybe Core.Text
    -- ^ The name of a parameter group for this cluster.
  , preferredMaintenanceWindow :: Core.Maybe Core.Text
    -- ^ A range of time when maintenance of DAX cluster software will be performed. For example: @sun:01:00-sun:09:00@ . Cluster maintenance normally takes less than 30 minutes, and is performed automatically within the maintenance window.
  , securityGroupIds :: Core.Maybe [Core.Text]
    -- ^ A list of user-specified security group IDs to be assigned to each node in the DAX cluster. If this parameter is not specified, DAX assigns the default VPC security group to each node.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCluster' value with any optional fields omitted.
mkUpdateCluster
    :: Core.Text -- ^ 'clusterName'
    -> UpdateCluster
mkUpdateCluster clusterName
  = UpdateCluster'{clusterName, description = Core.Nothing,
                   notificationTopicArn = Core.Nothing,
                   notificationTopicStatus = Core.Nothing,
                   parameterGroupName = Core.Nothing,
                   preferredMaintenanceWindow = Core.Nothing,
                   securityGroupIds = Core.Nothing}

-- | The name of the DAX cluster to be modified.
--
-- /Note:/ Consider using 'clusterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucClusterName :: Lens.Lens' UpdateCluster Core.Text
ucClusterName = Lens.field @"clusterName"
{-# INLINEABLE ucClusterName #-}
{-# DEPRECATED clusterName "Use generic-lens or generic-optics with 'clusterName' instead"  #-}

-- | A description of the changes being made to the cluster.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucDescription :: Lens.Lens' UpdateCluster (Core.Maybe Core.Text)
ucDescription = Lens.field @"description"
{-# INLINEABLE ucDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The Amazon Resource Name (ARN) that identifies the topic.
--
-- /Note:/ Consider using 'notificationTopicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucNotificationTopicArn :: Lens.Lens' UpdateCluster (Core.Maybe Core.Text)
ucNotificationTopicArn = Lens.field @"notificationTopicArn"
{-# INLINEABLE ucNotificationTopicArn #-}
{-# DEPRECATED notificationTopicArn "Use generic-lens or generic-optics with 'notificationTopicArn' instead"  #-}

-- | The current state of the topic.
--
-- /Note:/ Consider using 'notificationTopicStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucNotificationTopicStatus :: Lens.Lens' UpdateCluster (Core.Maybe Core.Text)
ucNotificationTopicStatus = Lens.field @"notificationTopicStatus"
{-# INLINEABLE ucNotificationTopicStatus #-}
{-# DEPRECATED notificationTopicStatus "Use generic-lens or generic-optics with 'notificationTopicStatus' instead"  #-}

-- | The name of a parameter group for this cluster.
--
-- /Note:/ Consider using 'parameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucParameterGroupName :: Lens.Lens' UpdateCluster (Core.Maybe Core.Text)
ucParameterGroupName = Lens.field @"parameterGroupName"
{-# INLINEABLE ucParameterGroupName #-}
{-# DEPRECATED parameterGroupName "Use generic-lens or generic-optics with 'parameterGroupName' instead"  #-}

-- | A range of time when maintenance of DAX cluster software will be performed. For example: @sun:01:00-sun:09:00@ . Cluster maintenance normally takes less than 30 minutes, and is performed automatically within the maintenance window.
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucPreferredMaintenanceWindow :: Lens.Lens' UpdateCluster (Core.Maybe Core.Text)
ucPreferredMaintenanceWindow = Lens.field @"preferredMaintenanceWindow"
{-# INLINEABLE ucPreferredMaintenanceWindow #-}
{-# DEPRECATED preferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead"  #-}

-- | A list of user-specified security group IDs to be assigned to each node in the DAX cluster. If this parameter is not specified, DAX assigns the default VPC security group to each node.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucSecurityGroupIds :: Lens.Lens' UpdateCluster (Core.Maybe [Core.Text])
ucSecurityGroupIds = Lens.field @"securityGroupIds"
{-# INLINEABLE ucSecurityGroupIds #-}
{-# DEPRECATED securityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead"  #-}

instance Core.ToQuery UpdateCluster where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateCluster where
        toHeaders UpdateCluster{..}
          = Core.pure ("X-Amz-Target", "AmazonDAXV3.UpdateCluster") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateCluster where
        toJSON UpdateCluster{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ClusterName" Core..= clusterName),
                  ("Description" Core..=) Core.<$> description,
                  ("NotificationTopicArn" Core..=) Core.<$> notificationTopicArn,
                  ("NotificationTopicStatus" Core..=) Core.<$>
                    notificationTopicStatus,
                  ("ParameterGroupName" Core..=) Core.<$> parameterGroupName,
                  ("PreferredMaintenanceWindow" Core..=) Core.<$>
                    preferredMaintenanceWindow,
                  ("SecurityGroupIds" Core..=) Core.<$> securityGroupIds])

instance Core.AWSRequest UpdateCluster where
        type Rs UpdateCluster = UpdateClusterResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateClusterResponse' Core.<$>
                   (x Core..:? "Cluster") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateClusterResponse' smart constructor.
data UpdateClusterResponse = UpdateClusterResponse'
  { cluster :: Core.Maybe Types.Cluster
    -- ^ A description of the DAX cluster, after it has been modified.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateClusterResponse' value with any optional fields omitted.
mkUpdateClusterResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateClusterResponse
mkUpdateClusterResponse responseStatus
  = UpdateClusterResponse'{cluster = Core.Nothing, responseStatus}

-- | A description of the DAX cluster, after it has been modified.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrrsCluster :: Lens.Lens' UpdateClusterResponse (Core.Maybe Types.Cluster)
ucrrsCluster = Lens.field @"cluster"
{-# INLINEABLE ucrrsCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrrsResponseStatus :: Lens.Lens' UpdateClusterResponse Core.Int
ucrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ucrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

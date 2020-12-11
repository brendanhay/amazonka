{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    UpdateCluster (..),
    mkUpdateCluster,

    -- ** Request lenses
    ucSecurityGroupIds,
    ucPreferredMaintenanceWindow,
    ucNotificationTopicStatus,
    ucDescription,
    ucNotificationTopicARN,
    ucParameterGroupName,
    ucClusterName,

    -- * Destructuring the response
    UpdateClusterResponse (..),
    mkUpdateClusterResponse,

    -- ** Response lenses
    ucrsCluster,
    ucrsResponseStatus,
  )
where

import Network.AWS.DAX.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateCluster' smart constructor.
data UpdateCluster = UpdateCluster'
  { securityGroupIds ::
      Lude.Maybe [Lude.Text],
    preferredMaintenanceWindow :: Lude.Maybe Lude.Text,
    notificationTopicStatus :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    notificationTopicARN :: Lude.Maybe Lude.Text,
    parameterGroupName :: Lude.Maybe Lude.Text,
    clusterName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateCluster' with the minimum fields required to make a request.
--
-- * 'clusterName' - The name of the DAX cluster to be modified.
-- * 'description' - A description of the changes being made to the cluster.
-- * 'notificationTopicARN' - The Amazon Resource Name (ARN) that identifies the topic.
-- * 'notificationTopicStatus' - The current state of the topic.
-- * 'parameterGroupName' - The name of a parameter group for this cluster.
-- * 'preferredMaintenanceWindow' - A range of time when maintenance of DAX cluster software will be performed. For example: @sun:01:00-sun:09:00@ . Cluster maintenance normally takes less than 30 minutes, and is performed automatically within the maintenance window.
-- * 'securityGroupIds' - A list of user-specified security group IDs to be assigned to each node in the DAX cluster. If this parameter is not specified, DAX assigns the default VPC security group to each node.
mkUpdateCluster ::
  -- | 'clusterName'
  Lude.Text ->
  UpdateCluster
mkUpdateCluster pClusterName_ =
  UpdateCluster'
    { securityGroupIds = Lude.Nothing,
      preferredMaintenanceWindow = Lude.Nothing,
      notificationTopicStatus = Lude.Nothing,
      description = Lude.Nothing,
      notificationTopicARN = Lude.Nothing,
      parameterGroupName = Lude.Nothing,
      clusterName = pClusterName_
    }

-- | A list of user-specified security group IDs to be assigned to each node in the DAX cluster. If this parameter is not specified, DAX assigns the default VPC security group to each node.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucSecurityGroupIds :: Lens.Lens' UpdateCluster (Lude.Maybe [Lude.Text])
ucSecurityGroupIds = Lens.lens (securityGroupIds :: UpdateCluster -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroupIds = a} :: UpdateCluster)
{-# DEPRECATED ucSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | A range of time when maintenance of DAX cluster software will be performed. For example: @sun:01:00-sun:09:00@ . Cluster maintenance normally takes less than 30 minutes, and is performed automatically within the maintenance window.
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucPreferredMaintenanceWindow :: Lens.Lens' UpdateCluster (Lude.Maybe Lude.Text)
ucPreferredMaintenanceWindow = Lens.lens (preferredMaintenanceWindow :: UpdateCluster -> Lude.Maybe Lude.Text) (\s a -> s {preferredMaintenanceWindow = a} :: UpdateCluster)
{-# DEPRECATED ucPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | The current state of the topic.
--
-- /Note:/ Consider using 'notificationTopicStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucNotificationTopicStatus :: Lens.Lens' UpdateCluster (Lude.Maybe Lude.Text)
ucNotificationTopicStatus = Lens.lens (notificationTopicStatus :: UpdateCluster -> Lude.Maybe Lude.Text) (\s a -> s {notificationTopicStatus = a} :: UpdateCluster)
{-# DEPRECATED ucNotificationTopicStatus "Use generic-lens or generic-optics with 'notificationTopicStatus' instead." #-}

-- | A description of the changes being made to the cluster.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucDescription :: Lens.Lens' UpdateCluster (Lude.Maybe Lude.Text)
ucDescription = Lens.lens (description :: UpdateCluster -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateCluster)
{-# DEPRECATED ucDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The Amazon Resource Name (ARN) that identifies the topic.
--
-- /Note:/ Consider using 'notificationTopicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucNotificationTopicARN :: Lens.Lens' UpdateCluster (Lude.Maybe Lude.Text)
ucNotificationTopicARN = Lens.lens (notificationTopicARN :: UpdateCluster -> Lude.Maybe Lude.Text) (\s a -> s {notificationTopicARN = a} :: UpdateCluster)
{-# DEPRECATED ucNotificationTopicARN "Use generic-lens or generic-optics with 'notificationTopicARN' instead." #-}

-- | The name of a parameter group for this cluster.
--
-- /Note:/ Consider using 'parameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucParameterGroupName :: Lens.Lens' UpdateCluster (Lude.Maybe Lude.Text)
ucParameterGroupName = Lens.lens (parameterGroupName :: UpdateCluster -> Lude.Maybe Lude.Text) (\s a -> s {parameterGroupName = a} :: UpdateCluster)
{-# DEPRECATED ucParameterGroupName "Use generic-lens or generic-optics with 'parameterGroupName' instead." #-}

-- | The name of the DAX cluster to be modified.
--
-- /Note:/ Consider using 'clusterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucClusterName :: Lens.Lens' UpdateCluster Lude.Text
ucClusterName = Lens.lens (clusterName :: UpdateCluster -> Lude.Text) (\s a -> s {clusterName = a} :: UpdateCluster)
{-# DEPRECATED ucClusterName "Use generic-lens or generic-optics with 'clusterName' instead." #-}

instance Lude.AWSRequest UpdateCluster where
  type Rs UpdateCluster = UpdateClusterResponse
  request = Req.postJSON daxService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateClusterResponse'
            Lude.<$> (x Lude..?> "Cluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateCluster where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDAXV3.UpdateCluster" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateCluster where
  toJSON UpdateCluster' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SecurityGroupIds" Lude..=) Lude.<$> securityGroupIds,
            ("PreferredMaintenanceWindow" Lude..=)
              Lude.<$> preferredMaintenanceWindow,
            ("NotificationTopicStatus" Lude..=)
              Lude.<$> notificationTopicStatus,
            ("Description" Lude..=) Lude.<$> description,
            ("NotificationTopicArn" Lude..=) Lude.<$> notificationTopicARN,
            ("ParameterGroupName" Lude..=) Lude.<$> parameterGroupName,
            Lude.Just ("ClusterName" Lude..= clusterName)
          ]
      )

instance Lude.ToPath UpdateCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateCluster where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateClusterResponse' smart constructor.
data UpdateClusterResponse = UpdateClusterResponse'
  { cluster ::
      Lude.Maybe Cluster,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateClusterResponse' with the minimum fields required to make a request.
--
-- * 'cluster' - A description of the DAX cluster, after it has been modified.
-- * 'responseStatus' - The response status code.
mkUpdateClusterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateClusterResponse
mkUpdateClusterResponse pResponseStatus_ =
  UpdateClusterResponse'
    { cluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A description of the DAX cluster, after it has been modified.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrsCluster :: Lens.Lens' UpdateClusterResponse (Lude.Maybe Cluster)
ucrsCluster = Lens.lens (cluster :: UpdateClusterResponse -> Lude.Maybe Cluster) (\s a -> s {cluster = a} :: UpdateClusterResponse)
{-# DEPRECATED ucrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrsResponseStatus :: Lens.Lens' UpdateClusterResponse Lude.Int
ucrsResponseStatus = Lens.lens (responseStatus :: UpdateClusterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateClusterResponse)
{-# DEPRECATED ucrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

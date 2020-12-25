{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Cluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Cluster
  ( Cluster (..),

    -- * Smart constructor
    mkCluster,

    -- * Lenses
    cActiveServicesCount,
    cAttachments,
    cAttachmentsStatus,
    cCapacityProviders,
    cClusterArn,
    cClusterName,
    cDefaultCapacityProviderStrategy,
    cPendingTasksCount,
    cRegisteredContainerInstancesCount,
    cRunningTasksCount,
    cSettings,
    cStatistics,
    cStatus,
    cTags,
  )
where

import qualified Network.AWS.ECS.Types.Attachment as Types
import qualified Network.AWS.ECS.Types.AttachmentsStatus as Types
import qualified Network.AWS.ECS.Types.CapacityProviderStrategyItem as Types
import qualified Network.AWS.ECS.Types.ClusterArn as Types
import qualified Network.AWS.ECS.Types.ClusterName as Types
import qualified Network.AWS.ECS.Types.ClusterSetting as Types
import qualified Network.AWS.ECS.Types.KeyValuePair as Types
import qualified Network.AWS.ECS.Types.Status as Types
import qualified Network.AWS.ECS.Types.String as Types
import qualified Network.AWS.ECS.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A regional grouping of one or more container instances on which you can run task requests. Each account receives a default cluster the first time you use the Amazon ECS service, but you may also create other clusters. Clusters may contain more than one instance type simultaneously.
--
-- /See:/ 'mkCluster' smart constructor.
data Cluster = Cluster'
  { -- | The number of services that are running on the cluster in an @ACTIVE@ state. You can view these services with 'ListServices' .
    activeServicesCount :: Core.Maybe Core.Int,
    -- | The resources attached to a cluster. When using a capacity provider with a cluster, the Auto Scaling plan that is created will be returned as a cluster attachment.
    attachments :: Core.Maybe [Types.Attachment],
    -- | The status of the capacity providers associated with the cluster. The following are the states that will be returned:
    --
    --
    --     * UPDATE_IN_PROGRESS
    --
    --     * The available capacity providers for the cluster are updating. This occurs when the Auto Scaling plan is provisioning or deprovisioning.
    --
    --
    --     * UPDATE_COMPLETE
    --
    --     * The capacity providers have successfully updated.
    --
    --
    --     * UPDATE_FAILED
    --
    --     * The capacity provider updates failed.
    attachmentsStatus :: Core.Maybe Types.AttachmentsStatus,
    -- | The capacity providers associated with the cluster.
    capacityProviders :: Core.Maybe [Types.String],
    -- | The Amazon Resource Name (ARN) that identifies the cluster. The ARN contains the @arn:aws:ecs@ namespace, followed by the Region of the cluster, the AWS account ID of the cluster owner, the @cluster@ namespace, and then the cluster name. For example, @arn:aws:ecs:region:012345678910:cluster/test@ .
    clusterArn :: Core.Maybe Types.ClusterArn,
    -- | A user-generated string that you use to identify your cluster.
    clusterName :: Core.Maybe Types.ClusterName,
    -- | The default capacity provider strategy for the cluster. When services or tasks are run in the cluster with no launch type or capacity provider strategy specified, the default capacity provider strategy is used.
    defaultCapacityProviderStrategy :: Core.Maybe [Types.CapacityProviderStrategyItem],
    -- | The number of tasks in the cluster that are in the @PENDING@ state.
    pendingTasksCount :: Core.Maybe Core.Int,
    -- | The number of container instances registered into the cluster. This includes container instances in both @ACTIVE@ and @DRAINING@ status.
    registeredContainerInstancesCount :: Core.Maybe Core.Int,
    -- | The number of tasks in the cluster that are in the @RUNNING@ state.
    runningTasksCount :: Core.Maybe Core.Int,
    -- | The settings for the cluster. This parameter indicates whether CloudWatch Container Insights is enabled or disabled for a cluster.
    settings :: Core.Maybe [Types.ClusterSetting],
    -- | Additional information about your clusters that are separated by launch type, including:
    --
    --
    --     * runningEC2TasksCount
    --
    --
    --     * RunningFargateTasksCount
    --
    --
    --     * pendingEC2TasksCount
    --
    --
    --     * pendingFargateTasksCount
    --
    --
    --     * activeEC2ServiceCount
    --
    --
    --     * activeFargateServiceCount
    --
    --
    --     * drainingEC2ServiceCount
    --
    --
    --     * drainingFargateServiceCount
    statistics :: Core.Maybe [Types.KeyValuePair],
    -- | The status of the cluster. The following are the possible states that will be returned.
    --
    --
    --     * ACTIVE
    --
    --     * The cluster is ready to accept tasks and if applicable you can register container instances with the cluster.
    --
    --
    --     * PROVISIONING
    --
    --     * The cluster has capacity providers associated with it and the resources needed for the capacity provider are being created.
    --
    --
    --     * DEPROVISIONING
    --
    --     * The cluster has capacity providers associated with it and the resources needed for the capacity provider are being deleted.
    --
    --
    --     * FAILED
    --
    --     * The cluster has capacity providers associated with it and the resources needed for the capacity provider have failed to create.
    --
    --
    --     * INACTIVE
    --
    --     * The cluster has been deleted. Clusters with an @INACTIVE@ status may remain discoverable in your account for a period of time. However, this behavior is subject to change in the future, so you should not rely on @INACTIVE@ clusters persisting.
    status :: Core.Maybe Types.Status,
    -- | The metadata that you apply to the cluster to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.
    --
    -- The following basic restrictions apply to tags:
    --
    --     * Maximum number of tags per resource - 50
    --
    --
    --     * For each resource, each tag key must be unique, and each tag key can have only one value.
    --
    --
    --     * Maximum key length - 128 Unicode characters in UTF-8
    --
    --
    --     * Maximum value length - 256 Unicode characters in UTF-8
    --
    --
    --     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.
    --
    --
    --     * Tag keys and values are case-sensitive.
    --
    --
    --     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Cluster' value with any optional fields omitted.
mkCluster ::
  Cluster
mkCluster =
  Cluster'
    { activeServicesCount = Core.Nothing,
      attachments = Core.Nothing,
      attachmentsStatus = Core.Nothing,
      capacityProviders = Core.Nothing,
      clusterArn = Core.Nothing,
      clusterName = Core.Nothing,
      defaultCapacityProviderStrategy = Core.Nothing,
      pendingTasksCount = Core.Nothing,
      registeredContainerInstancesCount = Core.Nothing,
      runningTasksCount = Core.Nothing,
      settings = Core.Nothing,
      statistics = Core.Nothing,
      status = Core.Nothing,
      tags = Core.Nothing
    }

-- | The number of services that are running on the cluster in an @ACTIVE@ state. You can view these services with 'ListServices' .
--
-- /Note:/ Consider using 'activeServicesCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cActiveServicesCount :: Lens.Lens' Cluster (Core.Maybe Core.Int)
cActiveServicesCount = Lens.field @"activeServicesCount"
{-# DEPRECATED cActiveServicesCount "Use generic-lens or generic-optics with 'activeServicesCount' instead." #-}

-- | The resources attached to a cluster. When using a capacity provider with a cluster, the Auto Scaling plan that is created will be returned as a cluster attachment.
--
-- /Note:/ Consider using 'attachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAttachments :: Lens.Lens' Cluster (Core.Maybe [Types.Attachment])
cAttachments = Lens.field @"attachments"
{-# DEPRECATED cAttachments "Use generic-lens or generic-optics with 'attachments' instead." #-}

-- | The status of the capacity providers associated with the cluster. The following are the states that will be returned:
--
--
--     * UPDATE_IN_PROGRESS
--
--     * The available capacity providers for the cluster are updating. This occurs when the Auto Scaling plan is provisioning or deprovisioning.
--
--
--     * UPDATE_COMPLETE
--
--     * The capacity providers have successfully updated.
--
--
--     * UPDATE_FAILED
--
--     * The capacity provider updates failed.
--
--
--
-- /Note:/ Consider using 'attachmentsStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAttachmentsStatus :: Lens.Lens' Cluster (Core.Maybe Types.AttachmentsStatus)
cAttachmentsStatus = Lens.field @"attachmentsStatus"
{-# DEPRECATED cAttachmentsStatus "Use generic-lens or generic-optics with 'attachmentsStatus' instead." #-}

-- | The capacity providers associated with the cluster.
--
-- /Note:/ Consider using 'capacityProviders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCapacityProviders :: Lens.Lens' Cluster (Core.Maybe [Types.String])
cCapacityProviders = Lens.field @"capacityProviders"
{-# DEPRECATED cCapacityProviders "Use generic-lens or generic-optics with 'capacityProviders' instead." #-}

-- | The Amazon Resource Name (ARN) that identifies the cluster. The ARN contains the @arn:aws:ecs@ namespace, followed by the Region of the cluster, the AWS account ID of the cluster owner, the @cluster@ namespace, and then the cluster name. For example, @arn:aws:ecs:region:012345678910:cluster/test@ .
--
-- /Note:/ Consider using 'clusterArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterArn :: Lens.Lens' Cluster (Core.Maybe Types.ClusterArn)
cClusterArn = Lens.field @"clusterArn"
{-# DEPRECATED cClusterArn "Use generic-lens or generic-optics with 'clusterArn' instead." #-}

-- | A user-generated string that you use to identify your cluster.
--
-- /Note:/ Consider using 'clusterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterName :: Lens.Lens' Cluster (Core.Maybe Types.ClusterName)
cClusterName = Lens.field @"clusterName"
{-# DEPRECATED cClusterName "Use generic-lens or generic-optics with 'clusterName' instead." #-}

-- | The default capacity provider strategy for the cluster. When services or tasks are run in the cluster with no launch type or capacity provider strategy specified, the default capacity provider strategy is used.
--
-- /Note:/ Consider using 'defaultCapacityProviderStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDefaultCapacityProviderStrategy :: Lens.Lens' Cluster (Core.Maybe [Types.CapacityProviderStrategyItem])
cDefaultCapacityProviderStrategy = Lens.field @"defaultCapacityProviderStrategy"
{-# DEPRECATED cDefaultCapacityProviderStrategy "Use generic-lens or generic-optics with 'defaultCapacityProviderStrategy' instead." #-}

-- | The number of tasks in the cluster that are in the @PENDING@ state.
--
-- /Note:/ Consider using 'pendingTasksCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPendingTasksCount :: Lens.Lens' Cluster (Core.Maybe Core.Int)
cPendingTasksCount = Lens.field @"pendingTasksCount"
{-# DEPRECATED cPendingTasksCount "Use generic-lens or generic-optics with 'pendingTasksCount' instead." #-}

-- | The number of container instances registered into the cluster. This includes container instances in both @ACTIVE@ and @DRAINING@ status.
--
-- /Note:/ Consider using 'registeredContainerInstancesCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cRegisteredContainerInstancesCount :: Lens.Lens' Cluster (Core.Maybe Core.Int)
cRegisteredContainerInstancesCount = Lens.field @"registeredContainerInstancesCount"
{-# DEPRECATED cRegisteredContainerInstancesCount "Use generic-lens or generic-optics with 'registeredContainerInstancesCount' instead." #-}

-- | The number of tasks in the cluster that are in the @RUNNING@ state.
--
-- /Note:/ Consider using 'runningTasksCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cRunningTasksCount :: Lens.Lens' Cluster (Core.Maybe Core.Int)
cRunningTasksCount = Lens.field @"runningTasksCount"
{-# DEPRECATED cRunningTasksCount "Use generic-lens or generic-optics with 'runningTasksCount' instead." #-}

-- | The settings for the cluster. This parameter indicates whether CloudWatch Container Insights is enabled or disabled for a cluster.
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSettings :: Lens.Lens' Cluster (Core.Maybe [Types.ClusterSetting])
cSettings = Lens.field @"settings"
{-# DEPRECATED cSettings "Use generic-lens or generic-optics with 'settings' instead." #-}

-- | Additional information about your clusters that are separated by launch type, including:
--
--
--     * runningEC2TasksCount
--
--
--     * RunningFargateTasksCount
--
--
--     * pendingEC2TasksCount
--
--
--     * pendingFargateTasksCount
--
--
--     * activeEC2ServiceCount
--
--
--     * activeFargateServiceCount
--
--
--     * drainingEC2ServiceCount
--
--
--     * drainingFargateServiceCount
--
--
--
-- /Note:/ Consider using 'statistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStatistics :: Lens.Lens' Cluster (Core.Maybe [Types.KeyValuePair])
cStatistics = Lens.field @"statistics"
{-# DEPRECATED cStatistics "Use generic-lens or generic-optics with 'statistics' instead." #-}

-- | The status of the cluster. The following are the possible states that will be returned.
--
--
--     * ACTIVE
--
--     * The cluster is ready to accept tasks and if applicable you can register container instances with the cluster.
--
--
--     * PROVISIONING
--
--     * The cluster has capacity providers associated with it and the resources needed for the capacity provider are being created.
--
--
--     * DEPROVISIONING
--
--     * The cluster has capacity providers associated with it and the resources needed for the capacity provider are being deleted.
--
--
--     * FAILED
--
--     * The cluster has capacity providers associated with it and the resources needed for the capacity provider have failed to create.
--
--
--     * INACTIVE
--
--     * The cluster has been deleted. Clusters with an @INACTIVE@ status may remain discoverable in your account for a period of time. However, this behavior is subject to change in the future, so you should not rely on @INACTIVE@ clusters persisting.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStatus :: Lens.Lens' Cluster (Core.Maybe Types.Status)
cStatus = Lens.field @"status"
{-# DEPRECATED cStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The metadata that you apply to the cluster to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.
--
-- The following basic restrictions apply to tags:
--
--     * Maximum number of tags per resource - 50
--
--
--     * For each resource, each tag key must be unique, and each tag key can have only one value.
--
--
--     * Maximum key length - 128 Unicode characters in UTF-8
--
--
--     * Maximum value length - 256 Unicode characters in UTF-8
--
--
--     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.
--
--
--     * Tag keys and values are case-sensitive.
--
--
--     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
--
--
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTags :: Lens.Lens' Cluster (Core.Maybe [Types.Tag])
cTags = Lens.field @"tags"
{-# DEPRECATED cTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON Cluster where
  parseJSON =
    Core.withObject "Cluster" Core.$
      \x ->
        Cluster'
          Core.<$> (x Core..:? "activeServicesCount")
          Core.<*> (x Core..:? "attachments")
          Core.<*> (x Core..:? "attachmentsStatus")
          Core.<*> (x Core..:? "capacityProviders")
          Core.<*> (x Core..:? "clusterArn")
          Core.<*> (x Core..:? "clusterName")
          Core.<*> (x Core..:? "defaultCapacityProviderStrategy")
          Core.<*> (x Core..:? "pendingTasksCount")
          Core.<*> (x Core..:? "registeredContainerInstancesCount")
          Core.<*> (x Core..:? "runningTasksCount")
          Core.<*> (x Core..:? "settings")
          Core.<*> (x Core..:? "statistics")
          Core.<*> (x Core..:? "status")
          Core.<*> (x Core..:? "tags")

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ContainerInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ContainerInstance
  ( ContainerInstance (..),

    -- * Smart constructor
    mkContainerInstance,

    -- * Lenses
    ciAgentConnected,
    ciAgentUpdateStatus,
    ciAttachments,
    ciAttributes,
    ciCapacityProviderName,
    ciContainerInstanceArn,
    ciEc2InstanceId,
    ciPendingTasksCount,
    ciRegisteredAt,
    ciRegisteredResources,
    ciRemainingResources,
    ciRunningTasksCount,
    ciStatus,
    ciStatusReason,
    ciTags,
    ciVersion,
    ciVersionInfo,
  )
where

import qualified Network.AWS.ECS.Types.AgentUpdateStatus as Types
import qualified Network.AWS.ECS.Types.Attachment as Types
import qualified Network.AWS.ECS.Types.Attribute as Types
import qualified Network.AWS.ECS.Types.Resource as Types
import qualified Network.AWS.ECS.Types.String as Types
import qualified Network.AWS.ECS.Types.Tag as Types
import qualified Network.AWS.ECS.Types.VersionInfo as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An EC2 instance that is running the Amazon ECS agent and has been registered with a cluster.
--
-- /See:/ 'mkContainerInstance' smart constructor.
data ContainerInstance = ContainerInstance'
  { -- | This parameter returns @true@ if the agent is connected to Amazon ECS. Registered instances with an agent that may be unhealthy or stopped return @false@ . Only instances connected to an agent can accept placement requests.
    agentConnected :: Core.Maybe Core.Bool,
    -- | The status of the most recent agent update. If an update has never been requested, this value is @NULL@ .
    agentUpdateStatus :: Core.Maybe Types.AgentUpdateStatus,
    -- | The resources attached to a container instance, such as elastic network interfaces.
    attachments :: Core.Maybe [Types.Attachment],
    -- | The attributes set for the container instance, either by the Amazon ECS container agent at instance registration or manually with the 'PutAttributes' operation.
    attributes :: Core.Maybe [Types.Attribute],
    -- | The capacity provider associated with the container instance.
    capacityProviderName :: Core.Maybe Types.String,
    -- | The Amazon Resource Name (ARN) of the container instance. The ARN contains the @arn:aws:ecs@ namespace, followed by the Region of the container instance, the AWS account ID of the container instance owner, the @container-instance@ namespace, and then the container instance ID. For example, @arn:aws:ecs:region:aws_account_id:container-instance/container_instance_ID@ .
    containerInstanceArn :: Core.Maybe Types.String,
    -- | The EC2 instance ID of the container instance.
    ec2InstanceId :: Core.Maybe Types.String,
    -- | The number of tasks on the container instance that are in the @PENDING@ status.
    pendingTasksCount :: Core.Maybe Core.Int,
    -- | The Unix timestamp for when the container instance was registered.
    registeredAt :: Core.Maybe Core.NominalDiffTime,
    -- | For CPU and memory resource types, this parameter describes the amount of each resource that was available on the container instance when the container agent registered it with Amazon ECS. This value represents the total amount of CPU and memory that can be allocated on this container instance to tasks. For port resource types, this parameter describes the ports that were reserved by the Amazon ECS container agent when it registered the container instance with Amazon ECS.
    registeredResources :: Core.Maybe [Types.Resource],
    -- | For CPU and memory resource types, this parameter describes the remaining CPU and memory that has not already been allocated to tasks and is therefore available for new tasks. For port resource types, this parameter describes the ports that were reserved by the Amazon ECS container agent (at instance registration time) and any task containers that have reserved port mappings on the host (with the @host@ or @bridge@ network mode). Any port that is not specified here is available for new tasks.
    remainingResources :: Core.Maybe [Types.Resource],
    -- | The number of tasks on the container instance that are in the @RUNNING@ status.
    runningTasksCount :: Core.Maybe Core.Int,
    -- | The status of the container instance. The valid values are @REGISTERING@ , @REGISTRATION_FAILED@ , @ACTIVE@ , @INACTIVE@ , @DEREGISTERING@ , or @DRAINING@ .
    --
    -- If your account has opted in to the @awsvpcTrunking@ account setting, then any newly registered container instance will transition to a @REGISTERING@ status while the trunk elastic network interface is provisioned for the instance. If the registration fails, the instance will transition to a @REGISTRATION_FAILED@ status. You can describe the container instance and see the reason for failure in the @statusReason@ parameter. Once the container instance is terminated, the instance transitions to a @DEREGISTERING@ status while the trunk elastic network interface is deprovisioned. The instance then transitions to an @INACTIVE@ status.
    -- The @ACTIVE@ status indicates that the container instance can accept tasks. The @DRAINING@ indicates that new tasks are not placed on the container instance and any service tasks running on the container instance are removed if possible. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/container-instance-draining.html Container Instance Draining> in the /Amazon Elastic Container Service Developer Guide/ .
    status :: Core.Maybe Types.String,
    -- | The reason that the container instance reached its current status.
    statusReason :: Core.Maybe Types.String,
    -- | The metadata that you apply to the container instance to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.
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
    tags :: Core.Maybe [Types.Tag],
    -- | The version counter for the container instance. Every time a container instance experiences a change that triggers a CloudWatch event, the version counter is incremented. If you are replicating your Amazon ECS container instance state with CloudWatch Events, you can compare the version of a container instance reported by the Amazon ECS APIs with the version reported in CloudWatch Events for the container instance (inside the @detail@ object) to verify that the version in your event stream is current.
    version :: Core.Maybe Core.Integer,
    -- | The version information for the Amazon ECS container agent and Docker daemon running on the container instance.
    versionInfo :: Core.Maybe Types.VersionInfo
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ContainerInstance' value with any optional fields omitted.
mkContainerInstance ::
  ContainerInstance
mkContainerInstance =
  ContainerInstance'
    { agentConnected = Core.Nothing,
      agentUpdateStatus = Core.Nothing,
      attachments = Core.Nothing,
      attributes = Core.Nothing,
      capacityProviderName = Core.Nothing,
      containerInstanceArn = Core.Nothing,
      ec2InstanceId = Core.Nothing,
      pendingTasksCount = Core.Nothing,
      registeredAt = Core.Nothing,
      registeredResources = Core.Nothing,
      remainingResources = Core.Nothing,
      runningTasksCount = Core.Nothing,
      status = Core.Nothing,
      statusReason = Core.Nothing,
      tags = Core.Nothing,
      version = Core.Nothing,
      versionInfo = Core.Nothing
    }

-- | This parameter returns @true@ if the agent is connected to Amazon ECS. Registered instances with an agent that may be unhealthy or stopped return @false@ . Only instances connected to an agent can accept placement requests.
--
-- /Note:/ Consider using 'agentConnected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciAgentConnected :: Lens.Lens' ContainerInstance (Core.Maybe Core.Bool)
ciAgentConnected = Lens.field @"agentConnected"
{-# DEPRECATED ciAgentConnected "Use generic-lens or generic-optics with 'agentConnected' instead." #-}

-- | The status of the most recent agent update. If an update has never been requested, this value is @NULL@ .
--
-- /Note:/ Consider using 'agentUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciAgentUpdateStatus :: Lens.Lens' ContainerInstance (Core.Maybe Types.AgentUpdateStatus)
ciAgentUpdateStatus = Lens.field @"agentUpdateStatus"
{-# DEPRECATED ciAgentUpdateStatus "Use generic-lens or generic-optics with 'agentUpdateStatus' instead." #-}

-- | The resources attached to a container instance, such as elastic network interfaces.
--
-- /Note:/ Consider using 'attachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciAttachments :: Lens.Lens' ContainerInstance (Core.Maybe [Types.Attachment])
ciAttachments = Lens.field @"attachments"
{-# DEPRECATED ciAttachments "Use generic-lens or generic-optics with 'attachments' instead." #-}

-- | The attributes set for the container instance, either by the Amazon ECS container agent at instance registration or manually with the 'PutAttributes' operation.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciAttributes :: Lens.Lens' ContainerInstance (Core.Maybe [Types.Attribute])
ciAttributes = Lens.field @"attributes"
{-# DEPRECATED ciAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The capacity provider associated with the container instance.
--
-- /Note:/ Consider using 'capacityProviderName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciCapacityProviderName :: Lens.Lens' ContainerInstance (Core.Maybe Types.String)
ciCapacityProviderName = Lens.field @"capacityProviderName"
{-# DEPRECATED ciCapacityProviderName "Use generic-lens or generic-optics with 'capacityProviderName' instead." #-}

-- | The Amazon Resource Name (ARN) of the container instance. The ARN contains the @arn:aws:ecs@ namespace, followed by the Region of the container instance, the AWS account ID of the container instance owner, the @container-instance@ namespace, and then the container instance ID. For example, @arn:aws:ecs:region:aws_account_id:container-instance/container_instance_ID@ .
--
-- /Note:/ Consider using 'containerInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciContainerInstanceArn :: Lens.Lens' ContainerInstance (Core.Maybe Types.String)
ciContainerInstanceArn = Lens.field @"containerInstanceArn"
{-# DEPRECATED ciContainerInstanceArn "Use generic-lens or generic-optics with 'containerInstanceArn' instead." #-}

-- | The EC2 instance ID of the container instance.
--
-- /Note:/ Consider using 'ec2InstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciEc2InstanceId :: Lens.Lens' ContainerInstance (Core.Maybe Types.String)
ciEc2InstanceId = Lens.field @"ec2InstanceId"
{-# DEPRECATED ciEc2InstanceId "Use generic-lens or generic-optics with 'ec2InstanceId' instead." #-}

-- | The number of tasks on the container instance that are in the @PENDING@ status.
--
-- /Note:/ Consider using 'pendingTasksCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciPendingTasksCount :: Lens.Lens' ContainerInstance (Core.Maybe Core.Int)
ciPendingTasksCount = Lens.field @"pendingTasksCount"
{-# DEPRECATED ciPendingTasksCount "Use generic-lens or generic-optics with 'pendingTasksCount' instead." #-}

-- | The Unix timestamp for when the container instance was registered.
--
-- /Note:/ Consider using 'registeredAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciRegisteredAt :: Lens.Lens' ContainerInstance (Core.Maybe Core.NominalDiffTime)
ciRegisteredAt = Lens.field @"registeredAt"
{-# DEPRECATED ciRegisteredAt "Use generic-lens or generic-optics with 'registeredAt' instead." #-}

-- | For CPU and memory resource types, this parameter describes the amount of each resource that was available on the container instance when the container agent registered it with Amazon ECS. This value represents the total amount of CPU and memory that can be allocated on this container instance to tasks. For port resource types, this parameter describes the ports that were reserved by the Amazon ECS container agent when it registered the container instance with Amazon ECS.
--
-- /Note:/ Consider using 'registeredResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciRegisteredResources :: Lens.Lens' ContainerInstance (Core.Maybe [Types.Resource])
ciRegisteredResources = Lens.field @"registeredResources"
{-# DEPRECATED ciRegisteredResources "Use generic-lens or generic-optics with 'registeredResources' instead." #-}

-- | For CPU and memory resource types, this parameter describes the remaining CPU and memory that has not already been allocated to tasks and is therefore available for new tasks. For port resource types, this parameter describes the ports that were reserved by the Amazon ECS container agent (at instance registration time) and any task containers that have reserved port mappings on the host (with the @host@ or @bridge@ network mode). Any port that is not specified here is available for new tasks.
--
-- /Note:/ Consider using 'remainingResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciRemainingResources :: Lens.Lens' ContainerInstance (Core.Maybe [Types.Resource])
ciRemainingResources = Lens.field @"remainingResources"
{-# DEPRECATED ciRemainingResources "Use generic-lens or generic-optics with 'remainingResources' instead." #-}

-- | The number of tasks on the container instance that are in the @RUNNING@ status.
--
-- /Note:/ Consider using 'runningTasksCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciRunningTasksCount :: Lens.Lens' ContainerInstance (Core.Maybe Core.Int)
ciRunningTasksCount = Lens.field @"runningTasksCount"
{-# DEPRECATED ciRunningTasksCount "Use generic-lens or generic-optics with 'runningTasksCount' instead." #-}

-- | The status of the container instance. The valid values are @REGISTERING@ , @REGISTRATION_FAILED@ , @ACTIVE@ , @INACTIVE@ , @DEREGISTERING@ , or @DRAINING@ .
--
-- If your account has opted in to the @awsvpcTrunking@ account setting, then any newly registered container instance will transition to a @REGISTERING@ status while the trunk elastic network interface is provisioned for the instance. If the registration fails, the instance will transition to a @REGISTRATION_FAILED@ status. You can describe the container instance and see the reason for failure in the @statusReason@ parameter. Once the container instance is terminated, the instance transitions to a @DEREGISTERING@ status while the trunk elastic network interface is deprovisioned. The instance then transitions to an @INACTIVE@ status.
-- The @ACTIVE@ status indicates that the container instance can accept tasks. The @DRAINING@ indicates that new tasks are not placed on the container instance and any service tasks running on the container instance are removed if possible. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/container-instance-draining.html Container Instance Draining> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciStatus :: Lens.Lens' ContainerInstance (Core.Maybe Types.String)
ciStatus = Lens.field @"status"
{-# DEPRECATED ciStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The reason that the container instance reached its current status.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciStatusReason :: Lens.Lens' ContainerInstance (Core.Maybe Types.String)
ciStatusReason = Lens.field @"statusReason"
{-# DEPRECATED ciStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

-- | The metadata that you apply to the container instance to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.
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
ciTags :: Lens.Lens' ContainerInstance (Core.Maybe [Types.Tag])
ciTags = Lens.field @"tags"
{-# DEPRECATED ciTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The version counter for the container instance. Every time a container instance experiences a change that triggers a CloudWatch event, the version counter is incremented. If you are replicating your Amazon ECS container instance state with CloudWatch Events, you can compare the version of a container instance reported by the Amazon ECS APIs with the version reported in CloudWatch Events for the container instance (inside the @detail@ object) to verify that the version in your event stream is current.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciVersion :: Lens.Lens' ContainerInstance (Core.Maybe Core.Integer)
ciVersion = Lens.field @"version"
{-# DEPRECATED ciVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The version information for the Amazon ECS container agent and Docker daemon running on the container instance.
--
-- /Note:/ Consider using 'versionInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciVersionInfo :: Lens.Lens' ContainerInstance (Core.Maybe Types.VersionInfo)
ciVersionInfo = Lens.field @"versionInfo"
{-# DEPRECATED ciVersionInfo "Use generic-lens or generic-optics with 'versionInfo' instead." #-}

instance Core.FromJSON ContainerInstance where
  parseJSON =
    Core.withObject "ContainerInstance" Core.$
      \x ->
        ContainerInstance'
          Core.<$> (x Core..:? "agentConnected")
          Core.<*> (x Core..:? "agentUpdateStatus")
          Core.<*> (x Core..:? "attachments")
          Core.<*> (x Core..:? "attributes")
          Core.<*> (x Core..:? "capacityProviderName")
          Core.<*> (x Core..:? "containerInstanceArn")
          Core.<*> (x Core..:? "ec2InstanceId")
          Core.<*> (x Core..:? "pendingTasksCount")
          Core.<*> (x Core..:? "registeredAt")
          Core.<*> (x Core..:? "registeredResources")
          Core.<*> (x Core..:? "remainingResources")
          Core.<*> (x Core..:? "runningTasksCount")
          Core.<*> (x Core..:? "status")
          Core.<*> (x Core..:? "statusReason")
          Core.<*> (x Core..:? "tags")
          Core.<*> (x Core..:? "version")
          Core.<*> (x Core..:? "versionInfo")

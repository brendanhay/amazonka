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
    ciStatus,
    ciAttachments,
    ciRunningTasksCount,
    ciRemainingResources,
    ciEc2InstanceId,
    ciContainerInstanceARN,
    ciAgentConnected,
    ciVersionInfo,
    ciAgentUpdateStatus,
    ciAttributes,
    ciVersion,
    ciPendingTasksCount,
    ciCapacityProviderName,
    ciRegisteredAt,
    ciStatusReason,
    ciTags,
    ciRegisteredResources,
  )
where

import Network.AWS.ECS.Types.AgentUpdateStatus
import Network.AWS.ECS.Types.Attachment
import Network.AWS.ECS.Types.Attribute
import Network.AWS.ECS.Types.Resource
import Network.AWS.ECS.Types.Tag
import Network.AWS.ECS.Types.VersionInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An EC2 instance that is running the Amazon ECS agent and has been registered with a cluster.
--
-- /See:/ 'mkContainerInstance' smart constructor.
data ContainerInstance = ContainerInstance'
  { status ::
      Lude.Maybe Lude.Text,
    attachments :: Lude.Maybe [Attachment],
    runningTasksCount :: Lude.Maybe Lude.Int,
    remainingResources :: Lude.Maybe [Resource],
    ec2InstanceId :: Lude.Maybe Lude.Text,
    containerInstanceARN :: Lude.Maybe Lude.Text,
    agentConnected :: Lude.Maybe Lude.Bool,
    versionInfo :: Lude.Maybe VersionInfo,
    agentUpdateStatus :: Lude.Maybe AgentUpdateStatus,
    attributes :: Lude.Maybe [Attribute],
    version :: Lude.Maybe Lude.Integer,
    pendingTasksCount :: Lude.Maybe Lude.Int,
    capacityProviderName :: Lude.Maybe Lude.Text,
    registeredAt :: Lude.Maybe Lude.Timestamp,
    statusReason :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    registeredResources :: Lude.Maybe [Resource]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContainerInstance' with the minimum fields required to make a request.
--
-- * 'agentConnected' - This parameter returns @true@ if the agent is connected to Amazon ECS. Registered instances with an agent that may be unhealthy or stopped return @false@ . Only instances connected to an agent can accept placement requests.
-- * 'agentUpdateStatus' - The status of the most recent agent update. If an update has never been requested, this value is @NULL@ .
-- * 'attachments' - The resources attached to a container instance, such as elastic network interfaces.
-- * 'attributes' - The attributes set for the container instance, either by the Amazon ECS container agent at instance registration or manually with the 'PutAttributes' operation.
-- * 'capacityProviderName' - The capacity provider associated with the container instance.
-- * 'containerInstanceARN' - The Amazon Resource Name (ARN) of the container instance. The ARN contains the @arn:aws:ecs@ namespace, followed by the Region of the container instance, the AWS account ID of the container instance owner, the @container-instance@ namespace, and then the container instance ID. For example, @arn:aws:ecs:region:aws_account_id:container-instance/container_instance_ID@ .
-- * 'ec2InstanceId' - The EC2 instance ID of the container instance.
-- * 'pendingTasksCount' - The number of tasks on the container instance that are in the @PENDING@ status.
-- * 'registeredAt' - The Unix timestamp for when the container instance was registered.
-- * 'registeredResources' - For CPU and memory resource types, this parameter describes the amount of each resource that was available on the container instance when the container agent registered it with Amazon ECS. This value represents the total amount of CPU and memory that can be allocated on this container instance to tasks. For port resource types, this parameter describes the ports that were reserved by the Amazon ECS container agent when it registered the container instance with Amazon ECS.
-- * 'remainingResources' - For CPU and memory resource types, this parameter describes the remaining CPU and memory that has not already been allocated to tasks and is therefore available for new tasks. For port resource types, this parameter describes the ports that were reserved by the Amazon ECS container agent (at instance registration time) and any task containers that have reserved port mappings on the host (with the @host@ or @bridge@ network mode). Any port that is not specified here is available for new tasks.
-- * 'runningTasksCount' - The number of tasks on the container instance that are in the @RUNNING@ status.
-- * 'status' - The status of the container instance. The valid values are @REGISTERING@ , @REGISTRATION_FAILED@ , @ACTIVE@ , @INACTIVE@ , @DEREGISTERING@ , or @DRAINING@ .
--
-- If your account has opted in to the @awsvpcTrunking@ account setting, then any newly registered container instance will transition to a @REGISTERING@ status while the trunk elastic network interface is provisioned for the instance. If the registration fails, the instance will transition to a @REGISTRATION_FAILED@ status. You can describe the container instance and see the reason for failure in the @statusReason@ parameter. Once the container instance is terminated, the instance transitions to a @DEREGISTERING@ status while the trunk elastic network interface is deprovisioned. The instance then transitions to an @INACTIVE@ status.
-- The @ACTIVE@ status indicates that the container instance can accept tasks. The @DRAINING@ indicates that new tasks are not placed on the container instance and any service tasks running on the container instance are removed if possible. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/container-instance-draining.html Container Instance Draining> in the /Amazon Elastic Container Service Developer Guide/ .
-- * 'statusReason' - The reason that the container instance reached its current status.
-- * 'tags' - The metadata that you apply to the container instance to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.
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
-- * 'version' - The version counter for the container instance. Every time a container instance experiences a change that triggers a CloudWatch event, the version counter is incremented. If you are replicating your Amazon ECS container instance state with CloudWatch Events, you can compare the version of a container instance reported by the Amazon ECS APIs with the version reported in CloudWatch Events for the container instance (inside the @detail@ object) to verify that the version in your event stream is current.
-- * 'versionInfo' - The version information for the Amazon ECS container agent and Docker daemon running on the container instance.
mkContainerInstance ::
  ContainerInstance
mkContainerInstance =
  ContainerInstance'
    { status = Lude.Nothing,
      attachments = Lude.Nothing,
      runningTasksCount = Lude.Nothing,
      remainingResources = Lude.Nothing,
      ec2InstanceId = Lude.Nothing,
      containerInstanceARN = Lude.Nothing,
      agentConnected = Lude.Nothing,
      versionInfo = Lude.Nothing,
      agentUpdateStatus = Lude.Nothing,
      attributes = Lude.Nothing,
      version = Lude.Nothing,
      pendingTasksCount = Lude.Nothing,
      capacityProviderName = Lude.Nothing,
      registeredAt = Lude.Nothing,
      statusReason = Lude.Nothing,
      tags = Lude.Nothing,
      registeredResources = Lude.Nothing
    }

-- | The status of the container instance. The valid values are @REGISTERING@ , @REGISTRATION_FAILED@ , @ACTIVE@ , @INACTIVE@ , @DEREGISTERING@ , or @DRAINING@ .
--
-- If your account has opted in to the @awsvpcTrunking@ account setting, then any newly registered container instance will transition to a @REGISTERING@ status while the trunk elastic network interface is provisioned for the instance. If the registration fails, the instance will transition to a @REGISTRATION_FAILED@ status. You can describe the container instance and see the reason for failure in the @statusReason@ parameter. Once the container instance is terminated, the instance transitions to a @DEREGISTERING@ status while the trunk elastic network interface is deprovisioned. The instance then transitions to an @INACTIVE@ status.
-- The @ACTIVE@ status indicates that the container instance can accept tasks. The @DRAINING@ indicates that new tasks are not placed on the container instance and any service tasks running on the container instance are removed if possible. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/container-instance-draining.html Container Instance Draining> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciStatus :: Lens.Lens' ContainerInstance (Lude.Maybe Lude.Text)
ciStatus = Lens.lens (status :: ContainerInstance -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: ContainerInstance)
{-# DEPRECATED ciStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The resources attached to a container instance, such as elastic network interfaces.
--
-- /Note:/ Consider using 'attachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciAttachments :: Lens.Lens' ContainerInstance (Lude.Maybe [Attachment])
ciAttachments = Lens.lens (attachments :: ContainerInstance -> Lude.Maybe [Attachment]) (\s a -> s {attachments = a} :: ContainerInstance)
{-# DEPRECATED ciAttachments "Use generic-lens or generic-optics with 'attachments' instead." #-}

-- | The number of tasks on the container instance that are in the @RUNNING@ status.
--
-- /Note:/ Consider using 'runningTasksCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciRunningTasksCount :: Lens.Lens' ContainerInstance (Lude.Maybe Lude.Int)
ciRunningTasksCount = Lens.lens (runningTasksCount :: ContainerInstance -> Lude.Maybe Lude.Int) (\s a -> s {runningTasksCount = a} :: ContainerInstance)
{-# DEPRECATED ciRunningTasksCount "Use generic-lens or generic-optics with 'runningTasksCount' instead." #-}

-- | For CPU and memory resource types, this parameter describes the remaining CPU and memory that has not already been allocated to tasks and is therefore available for new tasks. For port resource types, this parameter describes the ports that were reserved by the Amazon ECS container agent (at instance registration time) and any task containers that have reserved port mappings on the host (with the @host@ or @bridge@ network mode). Any port that is not specified here is available for new tasks.
--
-- /Note:/ Consider using 'remainingResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciRemainingResources :: Lens.Lens' ContainerInstance (Lude.Maybe [Resource])
ciRemainingResources = Lens.lens (remainingResources :: ContainerInstance -> Lude.Maybe [Resource]) (\s a -> s {remainingResources = a} :: ContainerInstance)
{-# DEPRECATED ciRemainingResources "Use generic-lens or generic-optics with 'remainingResources' instead." #-}

-- | The EC2 instance ID of the container instance.
--
-- /Note:/ Consider using 'ec2InstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciEc2InstanceId :: Lens.Lens' ContainerInstance (Lude.Maybe Lude.Text)
ciEc2InstanceId = Lens.lens (ec2InstanceId :: ContainerInstance -> Lude.Maybe Lude.Text) (\s a -> s {ec2InstanceId = a} :: ContainerInstance)
{-# DEPRECATED ciEc2InstanceId "Use generic-lens or generic-optics with 'ec2InstanceId' instead." #-}

-- | The Amazon Resource Name (ARN) of the container instance. The ARN contains the @arn:aws:ecs@ namespace, followed by the Region of the container instance, the AWS account ID of the container instance owner, the @container-instance@ namespace, and then the container instance ID. For example, @arn:aws:ecs:region:aws_account_id:container-instance/container_instance_ID@ .
--
-- /Note:/ Consider using 'containerInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciContainerInstanceARN :: Lens.Lens' ContainerInstance (Lude.Maybe Lude.Text)
ciContainerInstanceARN = Lens.lens (containerInstanceARN :: ContainerInstance -> Lude.Maybe Lude.Text) (\s a -> s {containerInstanceARN = a} :: ContainerInstance)
{-# DEPRECATED ciContainerInstanceARN "Use generic-lens or generic-optics with 'containerInstanceARN' instead." #-}

-- | This parameter returns @true@ if the agent is connected to Amazon ECS. Registered instances with an agent that may be unhealthy or stopped return @false@ . Only instances connected to an agent can accept placement requests.
--
-- /Note:/ Consider using 'agentConnected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciAgentConnected :: Lens.Lens' ContainerInstance (Lude.Maybe Lude.Bool)
ciAgentConnected = Lens.lens (agentConnected :: ContainerInstance -> Lude.Maybe Lude.Bool) (\s a -> s {agentConnected = a} :: ContainerInstance)
{-# DEPRECATED ciAgentConnected "Use generic-lens or generic-optics with 'agentConnected' instead." #-}

-- | The version information for the Amazon ECS container agent and Docker daemon running on the container instance.
--
-- /Note:/ Consider using 'versionInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciVersionInfo :: Lens.Lens' ContainerInstance (Lude.Maybe VersionInfo)
ciVersionInfo = Lens.lens (versionInfo :: ContainerInstance -> Lude.Maybe VersionInfo) (\s a -> s {versionInfo = a} :: ContainerInstance)
{-# DEPRECATED ciVersionInfo "Use generic-lens or generic-optics with 'versionInfo' instead." #-}

-- | The status of the most recent agent update. If an update has never been requested, this value is @NULL@ .
--
-- /Note:/ Consider using 'agentUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciAgentUpdateStatus :: Lens.Lens' ContainerInstance (Lude.Maybe AgentUpdateStatus)
ciAgentUpdateStatus = Lens.lens (agentUpdateStatus :: ContainerInstance -> Lude.Maybe AgentUpdateStatus) (\s a -> s {agentUpdateStatus = a} :: ContainerInstance)
{-# DEPRECATED ciAgentUpdateStatus "Use generic-lens or generic-optics with 'agentUpdateStatus' instead." #-}

-- | The attributes set for the container instance, either by the Amazon ECS container agent at instance registration or manually with the 'PutAttributes' operation.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciAttributes :: Lens.Lens' ContainerInstance (Lude.Maybe [Attribute])
ciAttributes = Lens.lens (attributes :: ContainerInstance -> Lude.Maybe [Attribute]) (\s a -> s {attributes = a} :: ContainerInstance)
{-# DEPRECATED ciAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The version counter for the container instance. Every time a container instance experiences a change that triggers a CloudWatch event, the version counter is incremented. If you are replicating your Amazon ECS container instance state with CloudWatch Events, you can compare the version of a container instance reported by the Amazon ECS APIs with the version reported in CloudWatch Events for the container instance (inside the @detail@ object) to verify that the version in your event stream is current.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciVersion :: Lens.Lens' ContainerInstance (Lude.Maybe Lude.Integer)
ciVersion = Lens.lens (version :: ContainerInstance -> Lude.Maybe Lude.Integer) (\s a -> s {version = a} :: ContainerInstance)
{-# DEPRECATED ciVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The number of tasks on the container instance that are in the @PENDING@ status.
--
-- /Note:/ Consider using 'pendingTasksCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciPendingTasksCount :: Lens.Lens' ContainerInstance (Lude.Maybe Lude.Int)
ciPendingTasksCount = Lens.lens (pendingTasksCount :: ContainerInstance -> Lude.Maybe Lude.Int) (\s a -> s {pendingTasksCount = a} :: ContainerInstance)
{-# DEPRECATED ciPendingTasksCount "Use generic-lens or generic-optics with 'pendingTasksCount' instead." #-}

-- | The capacity provider associated with the container instance.
--
-- /Note:/ Consider using 'capacityProviderName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciCapacityProviderName :: Lens.Lens' ContainerInstance (Lude.Maybe Lude.Text)
ciCapacityProviderName = Lens.lens (capacityProviderName :: ContainerInstance -> Lude.Maybe Lude.Text) (\s a -> s {capacityProviderName = a} :: ContainerInstance)
{-# DEPRECATED ciCapacityProviderName "Use generic-lens or generic-optics with 'capacityProviderName' instead." #-}

-- | The Unix timestamp for when the container instance was registered.
--
-- /Note:/ Consider using 'registeredAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciRegisteredAt :: Lens.Lens' ContainerInstance (Lude.Maybe Lude.Timestamp)
ciRegisteredAt = Lens.lens (registeredAt :: ContainerInstance -> Lude.Maybe Lude.Timestamp) (\s a -> s {registeredAt = a} :: ContainerInstance)
{-# DEPRECATED ciRegisteredAt "Use generic-lens or generic-optics with 'registeredAt' instead." #-}

-- | The reason that the container instance reached its current status.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciStatusReason :: Lens.Lens' ContainerInstance (Lude.Maybe Lude.Text)
ciStatusReason = Lens.lens (statusReason :: ContainerInstance -> Lude.Maybe Lude.Text) (\s a -> s {statusReason = a} :: ContainerInstance)
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
ciTags :: Lens.Lens' ContainerInstance (Lude.Maybe [Tag])
ciTags = Lens.lens (tags :: ContainerInstance -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ContainerInstance)
{-# DEPRECATED ciTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | For CPU and memory resource types, this parameter describes the amount of each resource that was available on the container instance when the container agent registered it with Amazon ECS. This value represents the total amount of CPU and memory that can be allocated on this container instance to tasks. For port resource types, this parameter describes the ports that were reserved by the Amazon ECS container agent when it registered the container instance with Amazon ECS.
--
-- /Note:/ Consider using 'registeredResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciRegisteredResources :: Lens.Lens' ContainerInstance (Lude.Maybe [Resource])
ciRegisteredResources = Lens.lens (registeredResources :: ContainerInstance -> Lude.Maybe [Resource]) (\s a -> s {registeredResources = a} :: ContainerInstance)
{-# DEPRECATED ciRegisteredResources "Use generic-lens or generic-optics with 'registeredResources' instead." #-}

instance Lude.FromJSON ContainerInstance where
  parseJSON =
    Lude.withObject
      "ContainerInstance"
      ( \x ->
          ContainerInstance'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "attachments" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "runningTasksCount")
            Lude.<*> (x Lude..:? "remainingResources" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ec2InstanceId")
            Lude.<*> (x Lude..:? "containerInstanceArn")
            Lude.<*> (x Lude..:? "agentConnected")
            Lude.<*> (x Lude..:? "versionInfo")
            Lude.<*> (x Lude..:? "agentUpdateStatus")
            Lude.<*> (x Lude..:? "attributes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "version")
            Lude.<*> (x Lude..:? "pendingTasksCount")
            Lude.<*> (x Lude..:? "capacityProviderName")
            Lude.<*> (x Lude..:? "registeredAt")
            Lude.<*> (x Lude..:? "statusReason")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "registeredResources" Lude..!= Lude.mempty)
      )

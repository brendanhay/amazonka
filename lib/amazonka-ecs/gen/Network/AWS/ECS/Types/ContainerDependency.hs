{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ContainerDependency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ContainerDependency
  ( ContainerDependency (..),

    -- * Smart constructor
    mkContainerDependency,

    -- * Lenses
    cdContainerName,
    cdCondition,
  )
where

import Network.AWS.ECS.Types.ContainerCondition
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The dependencies defined for container startup and shutdown. A container can contain multiple dependencies. When a dependency is defined for container startup, for container shutdown it is reversed.
--
-- Your Amazon ECS container instances require at least version 1.26.0 of the container agent to enable container dependencies. However, we recommend using the latest container agent version. For information about checking your agent version and updating to the latest version, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html Updating the Amazon ECS Container Agent> in the /Amazon Elastic Container Service Developer Guide/ . If you are using an Amazon ECS-optimized Linux AMI, your instance needs at least version 1.26.0-1 of the @ecs-init@ package. If your container instances are launched from version @20190301@ or later, then they contain the required versions of the container agent and @ecs-init@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized Linux AMI> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /See:/ 'mkContainerDependency' smart constructor.
data ContainerDependency = ContainerDependency'
  { containerName ::
      Lude.Text,
    condition :: ContainerCondition
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContainerDependency' with the minimum fields required to make a request.
--
-- * 'condition' - The dependency condition of the container. The following are the available conditions and their behavior:
--
--
--     * @START@ - This condition emulates the behavior of links and volumes today. It validates that a dependent container is started before permitting other containers to start.
--
--
--     * @COMPLETE@ - This condition validates that a dependent container runs to completion (exits) before permitting other containers to start. This can be useful for nonessential containers that run a script and then exit. This condition cannot be set on an essential container.
--
--
--     * @SUCCESS@ - This condition is the same as @COMPLETE@ , but it also requires that the container exits with a @zero@ status. This condition cannot be set on an essential container.
--
--
--     * @HEALTHY@ - This condition validates that the dependent container passes its Docker health check before permitting other containers to start. This requires that the dependent container has health checks configured. This condition is confirmed only at task startup.
--
--
-- * 'containerName' - The name of a container.
mkContainerDependency ::
  -- | 'containerName'
  Lude.Text ->
  -- | 'condition'
  ContainerCondition ->
  ContainerDependency
mkContainerDependency pContainerName_ pCondition_ =
  ContainerDependency'
    { containerName = pContainerName_,
      condition = pCondition_
    }

-- | The name of a container.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdContainerName :: Lens.Lens' ContainerDependency Lude.Text
cdContainerName = Lens.lens (containerName :: ContainerDependency -> Lude.Text) (\s a -> s {containerName = a} :: ContainerDependency)
{-# DEPRECATED cdContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

-- | The dependency condition of the container. The following are the available conditions and their behavior:
--
--
--     * @START@ - This condition emulates the behavior of links and volumes today. It validates that a dependent container is started before permitting other containers to start.
--
--
--     * @COMPLETE@ - This condition validates that a dependent container runs to completion (exits) before permitting other containers to start. This can be useful for nonessential containers that run a script and then exit. This condition cannot be set on an essential container.
--
--
--     * @SUCCESS@ - This condition is the same as @COMPLETE@ , but it also requires that the container exits with a @zero@ status. This condition cannot be set on an essential container.
--
--
--     * @HEALTHY@ - This condition validates that the dependent container passes its Docker health check before permitting other containers to start. This requires that the dependent container has health checks configured. This condition is confirmed only at task startup.
--
--
--
-- /Note:/ Consider using 'condition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCondition :: Lens.Lens' ContainerDependency ContainerCondition
cdCondition = Lens.lens (condition :: ContainerDependency -> ContainerCondition) (\s a -> s {condition = a} :: ContainerDependency)
{-# DEPRECATED cdCondition "Use generic-lens or generic-optics with 'condition' instead." #-}

instance Lude.FromJSON ContainerDependency where
  parseJSON =
    Lude.withObject
      "ContainerDependency"
      ( \x ->
          ContainerDependency'
            Lude.<$> (x Lude..: "containerName") Lude.<*> (x Lude..: "condition")
      )

instance Lude.ToJSON ContainerDependency where
  toJSON ContainerDependency' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("containerName" Lude..= containerName),
            Lude.Just ("condition" Lude..= condition)
          ]
      )

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ContainerDependency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.ContainerDependency
  ( ContainerDependency (..)
  -- * Smart constructor
  , mkContainerDependency
  -- * Lenses
  , cdContainerName
  , cdCondition
  ) where

import qualified Network.AWS.ECS.Types.ContainerCondition as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The dependencies defined for container startup and shutdown. A container can contain multiple dependencies. When a dependency is defined for container startup, for container shutdown it is reversed.
--
-- Your Amazon ECS container instances require at least version 1.26.0 of the container agent to enable container dependencies. However, we recommend using the latest container agent version. For information about checking your agent version and updating to the latest version, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html Updating the Amazon ECS Container Agent> in the /Amazon Elastic Container Service Developer Guide/ . If you are using an Amazon ECS-optimized Linux AMI, your instance needs at least version 1.26.0-1 of the @ecs-init@ package. If your container instances are launched from version @20190301@ or later, then they contain the required versions of the container agent and @ecs-init@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized Linux AMI> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /See:/ 'mkContainerDependency' smart constructor.
data ContainerDependency = ContainerDependency'
  { containerName :: Core.Text
    -- ^ The name of a container.
  , condition :: Types.ContainerCondition
    -- ^ The dependency condition of the container. The following are the available conditions and their behavior:
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ContainerDependency' value with any optional fields omitted.
mkContainerDependency
    :: Core.Text -- ^ 'containerName'
    -> Types.ContainerCondition -- ^ 'condition'
    -> ContainerDependency
mkContainerDependency containerName condition
  = ContainerDependency'{containerName, condition}

-- | The name of a container.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdContainerName :: Lens.Lens' ContainerDependency Core.Text
cdContainerName = Lens.field @"containerName"
{-# INLINEABLE cdContainerName #-}
{-# DEPRECATED containerName "Use generic-lens or generic-optics with 'containerName' instead"  #-}

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
cdCondition :: Lens.Lens' ContainerDependency Types.ContainerCondition
cdCondition = Lens.field @"condition"
{-# INLINEABLE cdCondition #-}
{-# DEPRECATED condition "Use generic-lens or generic-optics with 'condition' instead"  #-}

instance Core.FromJSON ContainerDependency where
        toJSON ContainerDependency{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("containerName" Core..= containerName),
                  Core.Just ("condition" Core..= condition)])

instance Core.FromJSON ContainerDependency where
        parseJSON
          = Core.withObject "ContainerDependency" Core.$
              \ x ->
                ContainerDependency' Core.<$>
                  (x Core..: "containerName") Core.<*> x Core..: "condition"

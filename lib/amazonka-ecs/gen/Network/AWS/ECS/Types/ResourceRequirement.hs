{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ResourceRequirement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ResourceRequirement
  ( ResourceRequirement (..),

    -- * Smart constructor
    mkResourceRequirement,

    -- * Lenses
    rrValue,
    rrType,
  )
where

import qualified Network.AWS.ECS.Types.ResourceType as Types
import qualified Network.AWS.ECS.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The type and amount of a resource to assign to a container. The supported resource types are GPUs and Elastic Inference accelerators. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-gpu.html Working with GPUs on Amazon ECS> or <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-eia.html Working with Amazon Elastic Inference on Amazon ECS> in the /Amazon Elastic Container Service Developer Guide/
--
-- /See:/ 'mkResourceRequirement' smart constructor.
data ResourceRequirement = ResourceRequirement'
  { -- | The value for the specified resource type.
    --
    -- If the @GPU@ type is used, the value is the number of physical @GPUs@ the Amazon ECS container agent will reserve for the container. The number of GPUs reserved for all containers in a task should not exceed the number of available GPUs on the container instance the task is launched on.
    -- If the @InferenceAccelerator@ type is used, the @value@ should match the @deviceName@ for an 'InferenceAccelerator' specified in a task definition.
    value :: Types.String,
    -- | The type of resource to assign to a container. The supported values are @GPU@ or @InferenceAccelerator@ .
    type' :: Types.ResourceType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceRequirement' value with any optional fields omitted.
mkResourceRequirement ::
  -- | 'value'
  Types.String ->
  -- | 'type\''
  Types.ResourceType ->
  ResourceRequirement
mkResourceRequirement value type' =
  ResourceRequirement' {value, type'}

-- | The value for the specified resource type.
--
-- If the @GPU@ type is used, the value is the number of physical @GPUs@ the Amazon ECS container agent will reserve for the container. The number of GPUs reserved for all containers in a task should not exceed the number of available GPUs on the container instance the task is launched on.
-- If the @InferenceAccelerator@ type is used, the @value@ should match the @deviceName@ for an 'InferenceAccelerator' specified in a task definition.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrValue :: Lens.Lens' ResourceRequirement Types.String
rrValue = Lens.field @"value"
{-# DEPRECATED rrValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The type of resource to assign to a container. The supported values are @GPU@ or @InferenceAccelerator@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrType :: Lens.Lens' ResourceRequirement Types.ResourceType
rrType = Lens.field @"type'"
{-# DEPRECATED rrType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON ResourceRequirement where
  toJSON ResourceRequirement {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("value" Core..= value),
            Core.Just ("type" Core..= type')
          ]
      )

instance Core.FromJSON ResourceRequirement where
  parseJSON =
    Core.withObject "ResourceRequirement" Core.$
      \x ->
        ResourceRequirement'
          Core.<$> (x Core..: "value") Core.<*> (x Core..: "type")

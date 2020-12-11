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

import Network.AWS.ECS.Types.ResourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The type and amount of a resource to assign to a container. The supported resource types are GPUs and Elastic Inference accelerators. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-gpu.html Working with GPUs on Amazon ECS> or <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-eia.html Working with Amazon Elastic Inference on Amazon ECS> in the /Amazon Elastic Container Service Developer Guide/
--
-- /See:/ 'mkResourceRequirement' smart constructor.
data ResourceRequirement = ResourceRequirement'
  { value :: Lude.Text,
    type' :: ResourceType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceRequirement' with the minimum fields required to make a request.
--
-- * 'type'' - The type of resource to assign to a container. The supported values are @GPU@ or @InferenceAccelerator@ .
-- * 'value' - The value for the specified resource type.
--
-- If the @GPU@ type is used, the value is the number of physical @GPUs@ the Amazon ECS container agent will reserve for the container. The number of GPUs reserved for all containers in a task should not exceed the number of available GPUs on the container instance the task is launched on.
-- If the @InferenceAccelerator@ type is used, the @value@ should match the @deviceName@ for an 'InferenceAccelerator' specified in a task definition.
mkResourceRequirement ::
  -- | 'value'
  Lude.Text ->
  -- | 'type''
  ResourceType ->
  ResourceRequirement
mkResourceRequirement pValue_ pType_ =
  ResourceRequirement' {value = pValue_, type' = pType_}

-- | The value for the specified resource type.
--
-- If the @GPU@ type is used, the value is the number of physical @GPUs@ the Amazon ECS container agent will reserve for the container. The number of GPUs reserved for all containers in a task should not exceed the number of available GPUs on the container instance the task is launched on.
-- If the @InferenceAccelerator@ type is used, the @value@ should match the @deviceName@ for an 'InferenceAccelerator' specified in a task definition.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrValue :: Lens.Lens' ResourceRequirement Lude.Text
rrValue = Lens.lens (value :: ResourceRequirement -> Lude.Text) (\s a -> s {value = a} :: ResourceRequirement)
{-# DEPRECATED rrValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The type of resource to assign to a container. The supported values are @GPU@ or @InferenceAccelerator@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrType :: Lens.Lens' ResourceRequirement ResourceType
rrType = Lens.lens (type' :: ResourceRequirement -> ResourceType) (\s a -> s {type' = a} :: ResourceRequirement)
{-# DEPRECATED rrType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON ResourceRequirement where
  parseJSON =
    Lude.withObject
      "ResourceRequirement"
      ( \x ->
          ResourceRequirement'
            Lude.<$> (x Lude..: "value") Lude.<*> (x Lude..: "type")
      )

instance Lude.ToJSON ResourceRequirement where
  toJSON ResourceRequirement' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("value" Lude..= value),
            Lude.Just ("type" Lude..= type')
          ]
      )

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.ResourceRequirement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.ResourceRequirement
  ( ResourceRequirement (..),

    -- * Smart constructor
    mkResourceRequirement,

    -- * Lenses
    rrValue,
    rrType,
  )
where

import Network.AWS.Batch.Types.ResourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The type and amount of a resource to assign to a container. Currently, the only supported resource type is @GPU@ .
--
-- /See:/ 'mkResourceRequirement' smart constructor.
data ResourceRequirement = ResourceRequirement'
  { -- | The number of physical GPUs to reserve for the container. The number of GPUs reserved for all containers in a job should not exceed the number of available GPUs on the compute resource that the job is launched on.
    value :: Lude.Text,
    -- | The type of resource to assign to a container. Currently, the only supported resource type is @GPU@ .
    type' :: ResourceType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceRequirement' with the minimum fields required to make a request.
--
-- * 'value' - The number of physical GPUs to reserve for the container. The number of GPUs reserved for all containers in a job should not exceed the number of available GPUs on the compute resource that the job is launched on.
-- * 'type'' - The type of resource to assign to a container. Currently, the only supported resource type is @GPU@ .
mkResourceRequirement ::
  -- | 'value'
  Lude.Text ->
  -- | 'type''
  ResourceType ->
  ResourceRequirement
mkResourceRequirement pValue_ pType_ =
  ResourceRequirement' {value = pValue_, type' = pType_}

-- | The number of physical GPUs to reserve for the container. The number of GPUs reserved for all containers in a job should not exceed the number of available GPUs on the compute resource that the job is launched on.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrValue :: Lens.Lens' ResourceRequirement Lude.Text
rrValue = Lens.lens (value :: ResourceRequirement -> Lude.Text) (\s a -> s {value = a} :: ResourceRequirement)
{-# DEPRECATED rrValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The type of resource to assign to a container. Currently, the only supported resource type is @GPU@ .
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

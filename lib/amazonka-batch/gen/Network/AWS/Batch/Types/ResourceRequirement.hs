{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.ResourceRequirement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Batch.Types.ResourceRequirement
  ( ResourceRequirement (..)
  -- * Smart constructor
  , mkResourceRequirement
  -- * Lenses
  , rrValue
  , rrType
  ) where

import qualified Network.AWS.Batch.Types.ResourceType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The type and amount of a resource to assign to a container. Currently, the only supported resource type is @GPU@ .
--
-- /See:/ 'mkResourceRequirement' smart constructor.
data ResourceRequirement = ResourceRequirement'
  { value :: Core.Text
    -- ^ The number of physical GPUs to reserve for the container. The number of GPUs reserved for all containers in a job should not exceed the number of available GPUs on the compute resource that the job is launched on.
  , type' :: Types.ResourceType
    -- ^ The type of resource to assign to a container. Currently, the only supported resource type is @GPU@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceRequirement' value with any optional fields omitted.
mkResourceRequirement
    :: Core.Text -- ^ 'value'
    -> Types.ResourceType -- ^ 'type\''
    -> ResourceRequirement
mkResourceRequirement value type'
  = ResourceRequirement'{value, type'}

-- | The number of physical GPUs to reserve for the container. The number of GPUs reserved for all containers in a job should not exceed the number of available GPUs on the compute resource that the job is launched on.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrValue :: Lens.Lens' ResourceRequirement Core.Text
rrValue = Lens.field @"value"
{-# INLINEABLE rrValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

-- | The type of resource to assign to a container. Currently, the only supported resource type is @GPU@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrType :: Lens.Lens' ResourceRequirement Types.ResourceType
rrType = Lens.field @"type'"
{-# INLINEABLE rrType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON ResourceRequirement where
        toJSON ResourceRequirement{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("value" Core..= value),
                  Core.Just ("type" Core..= type')])

instance Core.FromJSON ResourceRequirement where
        parseJSON
          = Core.withObject "ResourceRequirement" Core.$
              \ x ->
                ResourceRequirement' Core.<$>
                  (x Core..: "value") Core.<*> x Core..: "type"

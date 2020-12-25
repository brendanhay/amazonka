{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ElasticInferenceAccelerator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ElasticInferenceAccelerator
  ( ElasticInferenceAccelerator (..),

    -- * Smart constructor
    mkElasticInferenceAccelerator,

    -- * Lenses
    eiaType,
    eiaCount,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an elastic inference accelerator.
--
-- /See:/ 'mkElasticInferenceAccelerator' smart constructor.
data ElasticInferenceAccelerator = ElasticInferenceAccelerator'
  { -- | The type of elastic inference accelerator. The possible values are @eia1.medium@ , @eia1.large@ , @eia1.xlarge@ , @eia2.medium@ , @eia2.large@ , and @eia2.xlarge@ .
    type' :: Types.String,
    -- | The number of elastic inference accelerators to attach to the instance.
    --
    -- Default: 1
    count :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ElasticInferenceAccelerator' value with any optional fields omitted.
mkElasticInferenceAccelerator ::
  -- | 'type\''
  Types.String ->
  ElasticInferenceAccelerator
mkElasticInferenceAccelerator type' =
  ElasticInferenceAccelerator' {type', count = Core.Nothing}

-- | The type of elastic inference accelerator. The possible values are @eia1.medium@ , @eia1.large@ , @eia1.xlarge@ , @eia2.medium@ , @eia2.large@ , and @eia2.xlarge@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaType :: Lens.Lens' ElasticInferenceAccelerator Types.String
eiaType = Lens.field @"type'"
{-# DEPRECATED eiaType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The number of elastic inference accelerators to attach to the instance.
--
-- Default: 1
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaCount :: Lens.Lens' ElasticInferenceAccelerator (Core.Maybe Core.Natural)
eiaCount = Lens.field @"count"
{-# DEPRECATED eiaCount "Use generic-lens or generic-optics with 'count' instead." #-}

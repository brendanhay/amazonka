{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ElasticInferenceAccelerator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ElasticInferenceAccelerator
  ( ElasticInferenceAccelerator (..)
  -- * Smart constructor
  , mkElasticInferenceAccelerator
  -- * Lenses
  , eiaType
  , eiaCount
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an elastic inference accelerator. 
--
-- /See:/ 'mkElasticInferenceAccelerator' smart constructor.
data ElasticInferenceAccelerator = ElasticInferenceAccelerator'
  { type' :: Core.Text
    -- ^ The type of elastic inference accelerator. The possible values are @eia1.medium@ , @eia1.large@ , @eia1.xlarge@ , @eia2.medium@ , @eia2.large@ , and @eia2.xlarge@ . 
  , count :: Core.Maybe Core.Natural
    -- ^ The number of elastic inference accelerators to attach to the instance. 
--
-- Default: 1
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ElasticInferenceAccelerator' value with any optional fields omitted.
mkElasticInferenceAccelerator
    :: Core.Text -- ^ 'type\''
    -> ElasticInferenceAccelerator
mkElasticInferenceAccelerator type'
  = ElasticInferenceAccelerator'{type', count = Core.Nothing}

-- | The type of elastic inference accelerator. The possible values are @eia1.medium@ , @eia1.large@ , @eia1.xlarge@ , @eia2.medium@ , @eia2.large@ , and @eia2.xlarge@ . 
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaType :: Lens.Lens' ElasticInferenceAccelerator Core.Text
eiaType = Lens.field @"type'"
{-# INLINEABLE eiaType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The number of elastic inference accelerators to attach to the instance. 
--
-- Default: 1
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaCount :: Lens.Lens' ElasticInferenceAccelerator (Core.Maybe Core.Natural)
eiaCount = Lens.field @"count"
{-# INLINEABLE eiaCount #-}
{-# DEPRECATED count "Use generic-lens or generic-optics with 'count' instead"  #-}

instance Core.ToQuery ElasticInferenceAccelerator where
        toQuery ElasticInferenceAccelerator{..}
          = Core.toQueryPair "Type" type' Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Count") count

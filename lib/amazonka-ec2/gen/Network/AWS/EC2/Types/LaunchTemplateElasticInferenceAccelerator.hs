{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateElasticInferenceAccelerator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LaunchTemplateElasticInferenceAccelerator
  ( LaunchTemplateElasticInferenceAccelerator (..)
  -- * Smart constructor
  , mkLaunchTemplateElasticInferenceAccelerator
  -- * Lenses
  , lteiaType
  , lteiaCount
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an elastic inference accelerator. 
--
-- /See:/ 'mkLaunchTemplateElasticInferenceAccelerator' smart constructor.
data LaunchTemplateElasticInferenceAccelerator = LaunchTemplateElasticInferenceAccelerator'
  { type' :: Core.Text
    -- ^ The type of elastic inference accelerator. The possible values are eia1.medium, eia1.large, and eia1.xlarge. 
  , count :: Core.Maybe Core.Natural
    -- ^ The number of elastic inference accelerators to attach to the instance. 
--
-- Default: 1
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchTemplateElasticInferenceAccelerator' value with any optional fields omitted.
mkLaunchTemplateElasticInferenceAccelerator
    :: Core.Text -- ^ 'type\''
    -> LaunchTemplateElasticInferenceAccelerator
mkLaunchTemplateElasticInferenceAccelerator type'
  = LaunchTemplateElasticInferenceAccelerator'{type',
                                               count = Core.Nothing}

-- | The type of elastic inference accelerator. The possible values are eia1.medium, eia1.large, and eia1.xlarge. 
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lteiaType :: Lens.Lens' LaunchTemplateElasticInferenceAccelerator Core.Text
lteiaType = Lens.field @"type'"
{-# INLINEABLE lteiaType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The number of elastic inference accelerators to attach to the instance. 
--
-- Default: 1
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lteiaCount :: Lens.Lens' LaunchTemplateElasticInferenceAccelerator (Core.Maybe Core.Natural)
lteiaCount = Lens.field @"count"
{-# INLINEABLE lteiaCount #-}
{-# DEPRECATED count "Use generic-lens or generic-optics with 'count' instead"  #-}

instance Core.ToQuery LaunchTemplateElasticInferenceAccelerator
         where
        toQuery LaunchTemplateElasticInferenceAccelerator{..}
          = Core.toQueryPair "Type" type' Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Count") count

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.InstanceCountLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.InstanceCountLimits
  ( InstanceCountLimits (..)
  -- * Smart constructor
  , mkInstanceCountLimits
  -- * Lenses
  , iclMaximumInstanceCount
  , iclMinimumInstanceCount
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | InstanceCountLimits represents the limits on number of instances that be created in Amazon Elasticsearch for given InstanceType. 
--
-- /See:/ 'mkInstanceCountLimits' smart constructor.
data InstanceCountLimits = InstanceCountLimits'
  { maximumInstanceCount :: Core.Maybe Core.Int
  , minimumInstanceCount :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceCountLimits' value with any optional fields omitted.
mkInstanceCountLimits
    :: InstanceCountLimits
mkInstanceCountLimits
  = InstanceCountLimits'{maximumInstanceCount = Core.Nothing,
                         minimumInstanceCount = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maximumInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iclMaximumInstanceCount :: Lens.Lens' InstanceCountLimits (Core.Maybe Core.Int)
iclMaximumInstanceCount = Lens.field @"maximumInstanceCount"
{-# INLINEABLE iclMaximumInstanceCount #-}
{-# DEPRECATED maximumInstanceCount "Use generic-lens or generic-optics with 'maximumInstanceCount' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'minimumInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iclMinimumInstanceCount :: Lens.Lens' InstanceCountLimits (Core.Maybe Core.Int)
iclMinimumInstanceCount = Lens.field @"minimumInstanceCount"
{-# INLINEABLE iclMinimumInstanceCount #-}
{-# DEPRECATED minimumInstanceCount "Use generic-lens or generic-optics with 'minimumInstanceCount' instead"  #-}

instance Core.FromJSON InstanceCountLimits where
        parseJSON
          = Core.withObject "InstanceCountLimits" Core.$
              \ x ->
                InstanceCountLimits' Core.<$>
                  (x Core..:? "MaximumInstanceCount") Core.<*>
                    x Core..:? "MinimumInstanceCount"

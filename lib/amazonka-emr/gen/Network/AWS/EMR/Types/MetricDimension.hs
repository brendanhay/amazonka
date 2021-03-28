{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.MetricDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.MetricDimension
  ( MetricDimension (..)
  -- * Smart constructor
  , mkMetricDimension
  -- * Lenses
  , mdKey
  , mdValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A CloudWatch dimension, which is specified using a @Key@ (known as a @Name@ in CloudWatch), @Value@ pair. By default, Amazon EMR uses one dimension whose @Key@ is @JobFlowID@ and @Value@ is a variable representing the cluster ID, which is @> {emr.clusterId}@ . This enables the rule to bootstrap when the cluster ID becomes available.
--
-- /See:/ 'mkMetricDimension' smart constructor.
data MetricDimension = MetricDimension'
  { key :: Core.Maybe Core.Text
    -- ^ The dimension name.
  , value :: Core.Maybe Core.Text
    -- ^ The dimension value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MetricDimension' value with any optional fields omitted.
mkMetricDimension
    :: MetricDimension
mkMetricDimension
  = MetricDimension'{key = Core.Nothing, value = Core.Nothing}

-- | The dimension name.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdKey :: Lens.Lens' MetricDimension (Core.Maybe Core.Text)
mdKey = Lens.field @"key"
{-# INLINEABLE mdKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The dimension value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdValue :: Lens.Lens' MetricDimension (Core.Maybe Core.Text)
mdValue = Lens.field @"value"
{-# INLINEABLE mdValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON MetricDimension where
        toJSON MetricDimension{..}
          = Core.object
              (Core.catMaybes
                 [("Key" Core..=) Core.<$> key, ("Value" Core..=) Core.<$> value])

instance Core.FromJSON MetricDimension where
        parseJSON
          = Core.withObject "MetricDimension" Core.$
              \ x ->
                MetricDimension' Core.<$>
                  (x Core..:? "Key") Core.<*> x Core..:? "Value"

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.MetricGranularityType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.MetricGranularityType
  ( MetricGranularityType (..)
  -- * Smart constructor
  , mkMetricGranularityType
  -- * Lenses
  , mgtGranularity
  ) where

import qualified Network.AWS.AutoScaling.Types.XmlStringMaxLen255 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a granularity of a metric.
--
-- /See:/ 'mkMetricGranularityType' smart constructor.
newtype MetricGranularityType = MetricGranularityType'
  { granularity :: Core.Maybe Types.XmlStringMaxLen255
    -- ^ The granularity. The only valid value is @1Minute@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'MetricGranularityType' value with any optional fields omitted.
mkMetricGranularityType
    :: MetricGranularityType
mkMetricGranularityType
  = MetricGranularityType'{granularity = Core.Nothing}

-- | The granularity. The only valid value is @1Minute@ .
--
-- /Note:/ Consider using 'granularity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgtGranularity :: Lens.Lens' MetricGranularityType (Core.Maybe Types.XmlStringMaxLen255)
mgtGranularity = Lens.field @"granularity"
{-# INLINEABLE mgtGranularity #-}
{-# DEPRECATED granularity "Use generic-lens or generic-optics with 'granularity' instead"  #-}

instance Core.FromXML MetricGranularityType where
        parseXML x
          = MetricGranularityType' Core.<$> (x Core..@? "Granularity")

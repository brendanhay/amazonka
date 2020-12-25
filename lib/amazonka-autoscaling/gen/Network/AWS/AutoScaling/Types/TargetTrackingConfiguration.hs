{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.TargetTrackingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.TargetTrackingConfiguration
  ( TargetTrackingConfiguration (..),

    -- * Smart constructor
    mkTargetTrackingConfiguration,

    -- * Lenses
    ttcTargetValue,
    ttcCustomizedMetricSpecification,
    ttcDisableScaleIn,
    ttcPredefinedMetricSpecification,
  )
where

import qualified Network.AWS.AutoScaling.Types.CustomizedMetricSpecification as Types
import qualified Network.AWS.AutoScaling.Types.PredefinedMetricSpecification as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a target tracking scaling policy configuration to use with Amazon EC2 Auto Scaling.
--
-- /See:/ 'mkTargetTrackingConfiguration' smart constructor.
data TargetTrackingConfiguration = TargetTrackingConfiguration'
  { -- | The target value for the metric.
    targetValue :: Core.Double,
    -- | A customized metric. You must specify either a predefined metric or a customized metric.
    customizedMetricSpecification :: Core.Maybe Types.CustomizedMetricSpecification,
    -- | Indicates whether scaling in by the target tracking scaling policy is disabled. If scaling in is disabled, the target tracking scaling policy doesn't remove instances from the Auto Scaling group. Otherwise, the target tracking scaling policy can remove instances from the Auto Scaling group. The default is @false@ .
    disableScaleIn :: Core.Maybe Core.Bool,
    -- | A predefined metric. You must specify either a predefined metric or a customized metric.
    predefinedMetricSpecification :: Core.Maybe Types.PredefinedMetricSpecification
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TargetTrackingConfiguration' value with any optional fields omitted.
mkTargetTrackingConfiguration ::
  -- | 'targetValue'
  Core.Double ->
  TargetTrackingConfiguration
mkTargetTrackingConfiguration targetValue =
  TargetTrackingConfiguration'
    { targetValue,
      customizedMetricSpecification = Core.Nothing,
      disableScaleIn = Core.Nothing,
      predefinedMetricSpecification = Core.Nothing
    }

-- | The target value for the metric.
--
-- /Note:/ Consider using 'targetValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttcTargetValue :: Lens.Lens' TargetTrackingConfiguration Core.Double
ttcTargetValue = Lens.field @"targetValue"
{-# DEPRECATED ttcTargetValue "Use generic-lens or generic-optics with 'targetValue' instead." #-}

-- | A customized metric. You must specify either a predefined metric or a customized metric.
--
-- /Note:/ Consider using 'customizedMetricSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttcCustomizedMetricSpecification :: Lens.Lens' TargetTrackingConfiguration (Core.Maybe Types.CustomizedMetricSpecification)
ttcCustomizedMetricSpecification = Lens.field @"customizedMetricSpecification"
{-# DEPRECATED ttcCustomizedMetricSpecification "Use generic-lens or generic-optics with 'customizedMetricSpecification' instead." #-}

-- | Indicates whether scaling in by the target tracking scaling policy is disabled. If scaling in is disabled, the target tracking scaling policy doesn't remove instances from the Auto Scaling group. Otherwise, the target tracking scaling policy can remove instances from the Auto Scaling group. The default is @false@ .
--
-- /Note:/ Consider using 'disableScaleIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttcDisableScaleIn :: Lens.Lens' TargetTrackingConfiguration (Core.Maybe Core.Bool)
ttcDisableScaleIn = Lens.field @"disableScaleIn"
{-# DEPRECATED ttcDisableScaleIn "Use generic-lens or generic-optics with 'disableScaleIn' instead." #-}

-- | A predefined metric. You must specify either a predefined metric or a customized metric.
--
-- /Note:/ Consider using 'predefinedMetricSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttcPredefinedMetricSpecification :: Lens.Lens' TargetTrackingConfiguration (Core.Maybe Types.PredefinedMetricSpecification)
ttcPredefinedMetricSpecification = Lens.field @"predefinedMetricSpecification"
{-# DEPRECATED ttcPredefinedMetricSpecification "Use generic-lens or generic-optics with 'predefinedMetricSpecification' instead." #-}

instance Core.FromXML TargetTrackingConfiguration where
  parseXML x =
    TargetTrackingConfiguration'
      Core.<$> (x Core..@ "TargetValue")
      Core.<*> (x Core..@? "CustomizedMetricSpecification")
      Core.<*> (x Core..@? "DisableScaleIn")
      Core.<*> (x Core..@? "PredefinedMetricSpecification")

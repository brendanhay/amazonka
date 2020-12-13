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
    ttcPredefinedMetricSpecification,
    ttcTargetValue,
    ttcCustomizedMetricSpecification,
    ttcDisableScaleIn,
  )
where

import Network.AWS.AutoScaling.Types.CustomizedMetricSpecification
import Network.AWS.AutoScaling.Types.PredefinedMetricSpecification
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a target tracking scaling policy configuration to use with Amazon EC2 Auto Scaling.
--
-- /See:/ 'mkTargetTrackingConfiguration' smart constructor.
data TargetTrackingConfiguration = TargetTrackingConfiguration'
  { -- | A predefined metric. You must specify either a predefined metric or a customized metric.
    predefinedMetricSpecification :: Lude.Maybe PredefinedMetricSpecification,
    -- | The target value for the metric.
    targetValue :: Lude.Double,
    -- | A customized metric. You must specify either a predefined metric or a customized metric.
    customizedMetricSpecification :: Lude.Maybe CustomizedMetricSpecification,
    -- | Indicates whether scaling in by the target tracking scaling policy is disabled. If scaling in is disabled, the target tracking scaling policy doesn't remove instances from the Auto Scaling group. Otherwise, the target tracking scaling policy can remove instances from the Auto Scaling group. The default is @false@ .
    disableScaleIn :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TargetTrackingConfiguration' with the minimum fields required to make a request.
--
-- * 'predefinedMetricSpecification' - A predefined metric. You must specify either a predefined metric or a customized metric.
-- * 'targetValue' - The target value for the metric.
-- * 'customizedMetricSpecification' - A customized metric. You must specify either a predefined metric or a customized metric.
-- * 'disableScaleIn' - Indicates whether scaling in by the target tracking scaling policy is disabled. If scaling in is disabled, the target tracking scaling policy doesn't remove instances from the Auto Scaling group. Otherwise, the target tracking scaling policy can remove instances from the Auto Scaling group. The default is @false@ .
mkTargetTrackingConfiguration ::
  -- | 'targetValue'
  Lude.Double ->
  TargetTrackingConfiguration
mkTargetTrackingConfiguration pTargetValue_ =
  TargetTrackingConfiguration'
    { predefinedMetricSpecification =
        Lude.Nothing,
      targetValue = pTargetValue_,
      customizedMetricSpecification = Lude.Nothing,
      disableScaleIn = Lude.Nothing
    }

-- | A predefined metric. You must specify either a predefined metric or a customized metric.
--
-- /Note:/ Consider using 'predefinedMetricSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttcPredefinedMetricSpecification :: Lens.Lens' TargetTrackingConfiguration (Lude.Maybe PredefinedMetricSpecification)
ttcPredefinedMetricSpecification = Lens.lens (predefinedMetricSpecification :: TargetTrackingConfiguration -> Lude.Maybe PredefinedMetricSpecification) (\s a -> s {predefinedMetricSpecification = a} :: TargetTrackingConfiguration)
{-# DEPRECATED ttcPredefinedMetricSpecification "Use generic-lens or generic-optics with 'predefinedMetricSpecification' instead." #-}

-- | The target value for the metric.
--
-- /Note:/ Consider using 'targetValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttcTargetValue :: Lens.Lens' TargetTrackingConfiguration Lude.Double
ttcTargetValue = Lens.lens (targetValue :: TargetTrackingConfiguration -> Lude.Double) (\s a -> s {targetValue = a} :: TargetTrackingConfiguration)
{-# DEPRECATED ttcTargetValue "Use generic-lens or generic-optics with 'targetValue' instead." #-}

-- | A customized metric. You must specify either a predefined metric or a customized metric.
--
-- /Note:/ Consider using 'customizedMetricSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttcCustomizedMetricSpecification :: Lens.Lens' TargetTrackingConfiguration (Lude.Maybe CustomizedMetricSpecification)
ttcCustomizedMetricSpecification = Lens.lens (customizedMetricSpecification :: TargetTrackingConfiguration -> Lude.Maybe CustomizedMetricSpecification) (\s a -> s {customizedMetricSpecification = a} :: TargetTrackingConfiguration)
{-# DEPRECATED ttcCustomizedMetricSpecification "Use generic-lens or generic-optics with 'customizedMetricSpecification' instead." #-}

-- | Indicates whether scaling in by the target tracking scaling policy is disabled. If scaling in is disabled, the target tracking scaling policy doesn't remove instances from the Auto Scaling group. Otherwise, the target tracking scaling policy can remove instances from the Auto Scaling group. The default is @false@ .
--
-- /Note:/ Consider using 'disableScaleIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttcDisableScaleIn :: Lens.Lens' TargetTrackingConfiguration (Lude.Maybe Lude.Bool)
ttcDisableScaleIn = Lens.lens (disableScaleIn :: TargetTrackingConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {disableScaleIn = a} :: TargetTrackingConfiguration)
{-# DEPRECATED ttcDisableScaleIn "Use generic-lens or generic-optics with 'disableScaleIn' instead." #-}

instance Lude.FromXML TargetTrackingConfiguration where
  parseXML x =
    TargetTrackingConfiguration'
      Lude.<$> (x Lude..@? "PredefinedMetricSpecification")
      Lude.<*> (x Lude..@ "TargetValue")
      Lude.<*> (x Lude..@? "CustomizedMetricSpecification")
      Lude.<*> (x Lude..@? "DisableScaleIn")

instance Lude.ToQuery TargetTrackingConfiguration where
  toQuery TargetTrackingConfiguration' {..} =
    Lude.mconcat
      [ "PredefinedMetricSpecification"
          Lude.=: predefinedMetricSpecification,
        "TargetValue" Lude.=: targetValue,
        "CustomizedMetricSpecification"
          Lude.=: customizedMetricSpecification,
        "DisableScaleIn" Lude.=: disableScaleIn
      ]

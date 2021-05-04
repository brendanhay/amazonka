{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.TargetTrackingConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.TargetTrackingConfiguration where

import Network.AWS.AutoScaling.Types.CustomizedMetricSpecification
import Network.AWS.AutoScaling.Types.PredefinedMetricSpecification
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a target tracking scaling policy configuration to use with
-- Amazon EC2 Auto Scaling.
--
-- /See:/ 'newTargetTrackingConfiguration' smart constructor.
data TargetTrackingConfiguration = TargetTrackingConfiguration'
  { -- | Indicates whether scaling in by the target tracking scaling policy is
    -- disabled. If scaling in is disabled, the target tracking scaling policy
    -- doesn\'t remove instances from the Auto Scaling group. Otherwise, the
    -- target tracking scaling policy can remove instances from the Auto
    -- Scaling group. The default is @false@.
    disableScaleIn :: Prelude.Maybe Prelude.Bool,
    -- | A predefined metric. You must specify either a predefined metric or a
    -- customized metric.
    predefinedMetricSpecification :: Prelude.Maybe PredefinedMetricSpecification,
    -- | A customized metric. You must specify either a predefined metric or a
    -- customized metric.
    customizedMetricSpecification :: Prelude.Maybe CustomizedMetricSpecification,
    -- | The target value for the metric.
    targetValue :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TargetTrackingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disableScaleIn', 'targetTrackingConfiguration_disableScaleIn' - Indicates whether scaling in by the target tracking scaling policy is
-- disabled. If scaling in is disabled, the target tracking scaling policy
-- doesn\'t remove instances from the Auto Scaling group. Otherwise, the
-- target tracking scaling policy can remove instances from the Auto
-- Scaling group. The default is @false@.
--
-- 'predefinedMetricSpecification', 'targetTrackingConfiguration_predefinedMetricSpecification' - A predefined metric. You must specify either a predefined metric or a
-- customized metric.
--
-- 'customizedMetricSpecification', 'targetTrackingConfiguration_customizedMetricSpecification' - A customized metric. You must specify either a predefined metric or a
-- customized metric.
--
-- 'targetValue', 'targetTrackingConfiguration_targetValue' - The target value for the metric.
newTargetTrackingConfiguration ::
  -- | 'targetValue'
  Prelude.Double ->
  TargetTrackingConfiguration
newTargetTrackingConfiguration pTargetValue_ =
  TargetTrackingConfiguration'
    { disableScaleIn =
        Prelude.Nothing,
      predefinedMetricSpecification =
        Prelude.Nothing,
      customizedMetricSpecification =
        Prelude.Nothing,
      targetValue = pTargetValue_
    }

-- | Indicates whether scaling in by the target tracking scaling policy is
-- disabled. If scaling in is disabled, the target tracking scaling policy
-- doesn\'t remove instances from the Auto Scaling group. Otherwise, the
-- target tracking scaling policy can remove instances from the Auto
-- Scaling group. The default is @false@.
targetTrackingConfiguration_disableScaleIn :: Lens.Lens' TargetTrackingConfiguration (Prelude.Maybe Prelude.Bool)
targetTrackingConfiguration_disableScaleIn = Lens.lens (\TargetTrackingConfiguration' {disableScaleIn} -> disableScaleIn) (\s@TargetTrackingConfiguration' {} a -> s {disableScaleIn = a} :: TargetTrackingConfiguration)

-- | A predefined metric. You must specify either a predefined metric or a
-- customized metric.
targetTrackingConfiguration_predefinedMetricSpecification :: Lens.Lens' TargetTrackingConfiguration (Prelude.Maybe PredefinedMetricSpecification)
targetTrackingConfiguration_predefinedMetricSpecification = Lens.lens (\TargetTrackingConfiguration' {predefinedMetricSpecification} -> predefinedMetricSpecification) (\s@TargetTrackingConfiguration' {} a -> s {predefinedMetricSpecification = a} :: TargetTrackingConfiguration)

-- | A customized metric. You must specify either a predefined metric or a
-- customized metric.
targetTrackingConfiguration_customizedMetricSpecification :: Lens.Lens' TargetTrackingConfiguration (Prelude.Maybe CustomizedMetricSpecification)
targetTrackingConfiguration_customizedMetricSpecification = Lens.lens (\TargetTrackingConfiguration' {customizedMetricSpecification} -> customizedMetricSpecification) (\s@TargetTrackingConfiguration' {} a -> s {customizedMetricSpecification = a} :: TargetTrackingConfiguration)

-- | The target value for the metric.
targetTrackingConfiguration_targetValue :: Lens.Lens' TargetTrackingConfiguration Prelude.Double
targetTrackingConfiguration_targetValue = Lens.lens (\TargetTrackingConfiguration' {targetValue} -> targetValue) (\s@TargetTrackingConfiguration' {} a -> s {targetValue = a} :: TargetTrackingConfiguration)

instance Prelude.FromXML TargetTrackingConfiguration where
  parseXML x =
    TargetTrackingConfiguration'
      Prelude.<$> (x Prelude..@? "DisableScaleIn")
      Prelude.<*> (x Prelude..@? "PredefinedMetricSpecification")
      Prelude.<*> (x Prelude..@? "CustomizedMetricSpecification")
      Prelude.<*> (x Prelude..@ "TargetValue")

instance Prelude.Hashable TargetTrackingConfiguration

instance Prelude.NFData TargetTrackingConfiguration

instance Prelude.ToQuery TargetTrackingConfiguration where
  toQuery TargetTrackingConfiguration' {..} =
    Prelude.mconcat
      [ "DisableScaleIn" Prelude.=: disableScaleIn,
        "PredefinedMetricSpecification"
          Prelude.=: predefinedMetricSpecification,
        "CustomizedMetricSpecification"
          Prelude.=: customizedMetricSpecification,
        "TargetValue" Prelude.=: targetValue
      ]

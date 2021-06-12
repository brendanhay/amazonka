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
-- Module      : Network.AWS.DynamoDB.Types.AutoScalingSettingsDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.AutoScalingSettingsDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.AutoScalingPolicyDescription
import qualified Network.AWS.Lens as Lens

-- | Represents the auto scaling settings for a global table or global
-- secondary index.
--
-- /See:/ 'newAutoScalingSettingsDescription' smart constructor.
data AutoScalingSettingsDescription = AutoScalingSettingsDescription'
  { -- | Information about the scaling policies.
    scalingPolicies :: Core.Maybe [AutoScalingPolicyDescription],
    -- | The minimum capacity units that a global table or global secondary index
    -- should be scaled down to.
    minimumUnits :: Core.Maybe Core.Natural,
    -- | The maximum capacity units that a global table or global secondary index
    -- should be scaled up to.
    maximumUnits :: Core.Maybe Core.Natural,
    -- | Role ARN used for configuring the auto scaling policy.
    autoScalingRoleArn :: Core.Maybe Core.Text,
    -- | Disabled auto scaling for this global table or global secondary index.
    autoScalingDisabled :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AutoScalingSettingsDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scalingPolicies', 'autoScalingSettingsDescription_scalingPolicies' - Information about the scaling policies.
--
-- 'minimumUnits', 'autoScalingSettingsDescription_minimumUnits' - The minimum capacity units that a global table or global secondary index
-- should be scaled down to.
--
-- 'maximumUnits', 'autoScalingSettingsDescription_maximumUnits' - The maximum capacity units that a global table or global secondary index
-- should be scaled up to.
--
-- 'autoScalingRoleArn', 'autoScalingSettingsDescription_autoScalingRoleArn' - Role ARN used for configuring the auto scaling policy.
--
-- 'autoScalingDisabled', 'autoScalingSettingsDescription_autoScalingDisabled' - Disabled auto scaling for this global table or global secondary index.
newAutoScalingSettingsDescription ::
  AutoScalingSettingsDescription
newAutoScalingSettingsDescription =
  AutoScalingSettingsDescription'
    { scalingPolicies =
        Core.Nothing,
      minimumUnits = Core.Nothing,
      maximumUnits = Core.Nothing,
      autoScalingRoleArn = Core.Nothing,
      autoScalingDisabled = Core.Nothing
    }

-- | Information about the scaling policies.
autoScalingSettingsDescription_scalingPolicies :: Lens.Lens' AutoScalingSettingsDescription (Core.Maybe [AutoScalingPolicyDescription])
autoScalingSettingsDescription_scalingPolicies = Lens.lens (\AutoScalingSettingsDescription' {scalingPolicies} -> scalingPolicies) (\s@AutoScalingSettingsDescription' {} a -> s {scalingPolicies = a} :: AutoScalingSettingsDescription) Core.. Lens.mapping Lens._Coerce

-- | The minimum capacity units that a global table or global secondary index
-- should be scaled down to.
autoScalingSettingsDescription_minimumUnits :: Lens.Lens' AutoScalingSettingsDescription (Core.Maybe Core.Natural)
autoScalingSettingsDescription_minimumUnits = Lens.lens (\AutoScalingSettingsDescription' {minimumUnits} -> minimumUnits) (\s@AutoScalingSettingsDescription' {} a -> s {minimumUnits = a} :: AutoScalingSettingsDescription)

-- | The maximum capacity units that a global table or global secondary index
-- should be scaled up to.
autoScalingSettingsDescription_maximumUnits :: Lens.Lens' AutoScalingSettingsDescription (Core.Maybe Core.Natural)
autoScalingSettingsDescription_maximumUnits = Lens.lens (\AutoScalingSettingsDescription' {maximumUnits} -> maximumUnits) (\s@AutoScalingSettingsDescription' {} a -> s {maximumUnits = a} :: AutoScalingSettingsDescription)

-- | Role ARN used for configuring the auto scaling policy.
autoScalingSettingsDescription_autoScalingRoleArn :: Lens.Lens' AutoScalingSettingsDescription (Core.Maybe Core.Text)
autoScalingSettingsDescription_autoScalingRoleArn = Lens.lens (\AutoScalingSettingsDescription' {autoScalingRoleArn} -> autoScalingRoleArn) (\s@AutoScalingSettingsDescription' {} a -> s {autoScalingRoleArn = a} :: AutoScalingSettingsDescription)

-- | Disabled auto scaling for this global table or global secondary index.
autoScalingSettingsDescription_autoScalingDisabled :: Lens.Lens' AutoScalingSettingsDescription (Core.Maybe Core.Bool)
autoScalingSettingsDescription_autoScalingDisabled = Lens.lens (\AutoScalingSettingsDescription' {autoScalingDisabled} -> autoScalingDisabled) (\s@AutoScalingSettingsDescription' {} a -> s {autoScalingDisabled = a} :: AutoScalingSettingsDescription)

instance Core.FromJSON AutoScalingSettingsDescription where
  parseJSON =
    Core.withObject
      "AutoScalingSettingsDescription"
      ( \x ->
          AutoScalingSettingsDescription'
            Core.<$> (x Core..:? "ScalingPolicies" Core..!= Core.mempty)
            Core.<*> (x Core..:? "MinimumUnits")
            Core.<*> (x Core..:? "MaximumUnits")
            Core.<*> (x Core..:? "AutoScalingRoleArn")
            Core.<*> (x Core..:? "AutoScalingDisabled")
      )

instance Core.Hashable AutoScalingSettingsDescription

instance Core.NFData AutoScalingSettingsDescription

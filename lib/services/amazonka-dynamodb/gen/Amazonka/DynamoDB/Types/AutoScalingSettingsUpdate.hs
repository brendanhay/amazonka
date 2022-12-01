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
-- Module      : Amazonka.DynamoDB.Types.AutoScalingSettingsUpdate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.AutoScalingSettingsUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.AutoScalingPolicyUpdate
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents the auto scaling settings to be modified for a global table
-- or global secondary index.
--
-- /See:/ 'newAutoScalingSettingsUpdate' smart constructor.
data AutoScalingSettingsUpdate = AutoScalingSettingsUpdate'
  { -- | The minimum capacity units that a global table or global secondary index
    -- should be scaled down to.
    minimumUnits :: Prelude.Maybe Prelude.Natural,
    -- | The scaling policy to apply for scaling target global table or global
    -- secondary index capacity units.
    scalingPolicyUpdate :: Prelude.Maybe AutoScalingPolicyUpdate,
    -- | Role ARN used for configuring auto scaling policy.
    autoScalingRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Disabled auto scaling for this global table or global secondary index.
    autoScalingDisabled :: Prelude.Maybe Prelude.Bool,
    -- | The maximum capacity units that a global table or global secondary index
    -- should be scaled up to.
    maximumUnits :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoScalingSettingsUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minimumUnits', 'autoScalingSettingsUpdate_minimumUnits' - The minimum capacity units that a global table or global secondary index
-- should be scaled down to.
--
-- 'scalingPolicyUpdate', 'autoScalingSettingsUpdate_scalingPolicyUpdate' - The scaling policy to apply for scaling target global table or global
-- secondary index capacity units.
--
-- 'autoScalingRoleArn', 'autoScalingSettingsUpdate_autoScalingRoleArn' - Role ARN used for configuring auto scaling policy.
--
-- 'autoScalingDisabled', 'autoScalingSettingsUpdate_autoScalingDisabled' - Disabled auto scaling for this global table or global secondary index.
--
-- 'maximumUnits', 'autoScalingSettingsUpdate_maximumUnits' - The maximum capacity units that a global table or global secondary index
-- should be scaled up to.
newAutoScalingSettingsUpdate ::
  AutoScalingSettingsUpdate
newAutoScalingSettingsUpdate =
  AutoScalingSettingsUpdate'
    { minimumUnits =
        Prelude.Nothing,
      scalingPolicyUpdate = Prelude.Nothing,
      autoScalingRoleArn = Prelude.Nothing,
      autoScalingDisabled = Prelude.Nothing,
      maximumUnits = Prelude.Nothing
    }

-- | The minimum capacity units that a global table or global secondary index
-- should be scaled down to.
autoScalingSettingsUpdate_minimumUnits :: Lens.Lens' AutoScalingSettingsUpdate (Prelude.Maybe Prelude.Natural)
autoScalingSettingsUpdate_minimumUnits = Lens.lens (\AutoScalingSettingsUpdate' {minimumUnits} -> minimumUnits) (\s@AutoScalingSettingsUpdate' {} a -> s {minimumUnits = a} :: AutoScalingSettingsUpdate)

-- | The scaling policy to apply for scaling target global table or global
-- secondary index capacity units.
autoScalingSettingsUpdate_scalingPolicyUpdate :: Lens.Lens' AutoScalingSettingsUpdate (Prelude.Maybe AutoScalingPolicyUpdate)
autoScalingSettingsUpdate_scalingPolicyUpdate = Lens.lens (\AutoScalingSettingsUpdate' {scalingPolicyUpdate} -> scalingPolicyUpdate) (\s@AutoScalingSettingsUpdate' {} a -> s {scalingPolicyUpdate = a} :: AutoScalingSettingsUpdate)

-- | Role ARN used for configuring auto scaling policy.
autoScalingSettingsUpdate_autoScalingRoleArn :: Lens.Lens' AutoScalingSettingsUpdate (Prelude.Maybe Prelude.Text)
autoScalingSettingsUpdate_autoScalingRoleArn = Lens.lens (\AutoScalingSettingsUpdate' {autoScalingRoleArn} -> autoScalingRoleArn) (\s@AutoScalingSettingsUpdate' {} a -> s {autoScalingRoleArn = a} :: AutoScalingSettingsUpdate)

-- | Disabled auto scaling for this global table or global secondary index.
autoScalingSettingsUpdate_autoScalingDisabled :: Lens.Lens' AutoScalingSettingsUpdate (Prelude.Maybe Prelude.Bool)
autoScalingSettingsUpdate_autoScalingDisabled = Lens.lens (\AutoScalingSettingsUpdate' {autoScalingDisabled} -> autoScalingDisabled) (\s@AutoScalingSettingsUpdate' {} a -> s {autoScalingDisabled = a} :: AutoScalingSettingsUpdate)

-- | The maximum capacity units that a global table or global secondary index
-- should be scaled up to.
autoScalingSettingsUpdate_maximumUnits :: Lens.Lens' AutoScalingSettingsUpdate (Prelude.Maybe Prelude.Natural)
autoScalingSettingsUpdate_maximumUnits = Lens.lens (\AutoScalingSettingsUpdate' {maximumUnits} -> maximumUnits) (\s@AutoScalingSettingsUpdate' {} a -> s {maximumUnits = a} :: AutoScalingSettingsUpdate)

instance Prelude.Hashable AutoScalingSettingsUpdate where
  hashWithSalt _salt AutoScalingSettingsUpdate' {..} =
    _salt `Prelude.hashWithSalt` minimumUnits
      `Prelude.hashWithSalt` scalingPolicyUpdate
      `Prelude.hashWithSalt` autoScalingRoleArn
      `Prelude.hashWithSalt` autoScalingDisabled
      `Prelude.hashWithSalt` maximumUnits

instance Prelude.NFData AutoScalingSettingsUpdate where
  rnf AutoScalingSettingsUpdate' {..} =
    Prelude.rnf minimumUnits
      `Prelude.seq` Prelude.rnf scalingPolicyUpdate
      `Prelude.seq` Prelude.rnf autoScalingRoleArn
      `Prelude.seq` Prelude.rnf autoScalingDisabled
      `Prelude.seq` Prelude.rnf maximumUnits

instance Core.ToJSON AutoScalingSettingsUpdate where
  toJSON AutoScalingSettingsUpdate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MinimumUnits" Core..=) Prelude.<$> minimumUnits,
            ("ScalingPolicyUpdate" Core..=)
              Prelude.<$> scalingPolicyUpdate,
            ("AutoScalingRoleArn" Core..=)
              Prelude.<$> autoScalingRoleArn,
            ("AutoScalingDisabled" Core..=)
              Prelude.<$> autoScalingDisabled,
            ("MaximumUnits" Core..=) Prelude.<$> maximumUnits
          ]
      )

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
-- Module      : Amazonka.DynamoDB.Types.AutoScalingSettingsDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.AutoScalingSettingsDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.AutoScalingPolicyDescription
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents the auto scaling settings for a global table or global
-- secondary index.
--
-- /See:/ 'newAutoScalingSettingsDescription' smart constructor.
data AutoScalingSettingsDescription = AutoScalingSettingsDescription'
  { -- | Disabled auto scaling for this global table or global secondary index.
    autoScalingDisabled :: Prelude.Maybe Prelude.Bool,
    -- | Role ARN used for configuring the auto scaling policy.
    autoScalingRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum capacity units that a global table or global secondary index
    -- should be scaled up to.
    maximumUnits :: Prelude.Maybe Prelude.Natural,
    -- | The minimum capacity units that a global table or global secondary index
    -- should be scaled down to.
    minimumUnits :: Prelude.Maybe Prelude.Natural,
    -- | Information about the scaling policies.
    scalingPolicies :: Prelude.Maybe [AutoScalingPolicyDescription]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoScalingSettingsDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingDisabled', 'autoScalingSettingsDescription_autoScalingDisabled' - Disabled auto scaling for this global table or global secondary index.
--
-- 'autoScalingRoleArn', 'autoScalingSettingsDescription_autoScalingRoleArn' - Role ARN used for configuring the auto scaling policy.
--
-- 'maximumUnits', 'autoScalingSettingsDescription_maximumUnits' - The maximum capacity units that a global table or global secondary index
-- should be scaled up to.
--
-- 'minimumUnits', 'autoScalingSettingsDescription_minimumUnits' - The minimum capacity units that a global table or global secondary index
-- should be scaled down to.
--
-- 'scalingPolicies', 'autoScalingSettingsDescription_scalingPolicies' - Information about the scaling policies.
newAutoScalingSettingsDescription ::
  AutoScalingSettingsDescription
newAutoScalingSettingsDescription =
  AutoScalingSettingsDescription'
    { autoScalingDisabled =
        Prelude.Nothing,
      autoScalingRoleArn = Prelude.Nothing,
      maximumUnits = Prelude.Nothing,
      minimumUnits = Prelude.Nothing,
      scalingPolicies = Prelude.Nothing
    }

-- | Disabled auto scaling for this global table or global secondary index.
autoScalingSettingsDescription_autoScalingDisabled :: Lens.Lens' AutoScalingSettingsDescription (Prelude.Maybe Prelude.Bool)
autoScalingSettingsDescription_autoScalingDisabled = Lens.lens (\AutoScalingSettingsDescription' {autoScalingDisabled} -> autoScalingDisabled) (\s@AutoScalingSettingsDescription' {} a -> s {autoScalingDisabled = a} :: AutoScalingSettingsDescription)

-- | Role ARN used for configuring the auto scaling policy.
autoScalingSettingsDescription_autoScalingRoleArn :: Lens.Lens' AutoScalingSettingsDescription (Prelude.Maybe Prelude.Text)
autoScalingSettingsDescription_autoScalingRoleArn = Lens.lens (\AutoScalingSettingsDescription' {autoScalingRoleArn} -> autoScalingRoleArn) (\s@AutoScalingSettingsDescription' {} a -> s {autoScalingRoleArn = a} :: AutoScalingSettingsDescription)

-- | The maximum capacity units that a global table or global secondary index
-- should be scaled up to.
autoScalingSettingsDescription_maximumUnits :: Lens.Lens' AutoScalingSettingsDescription (Prelude.Maybe Prelude.Natural)
autoScalingSettingsDescription_maximumUnits = Lens.lens (\AutoScalingSettingsDescription' {maximumUnits} -> maximumUnits) (\s@AutoScalingSettingsDescription' {} a -> s {maximumUnits = a} :: AutoScalingSettingsDescription)

-- | The minimum capacity units that a global table or global secondary index
-- should be scaled down to.
autoScalingSettingsDescription_minimumUnits :: Lens.Lens' AutoScalingSettingsDescription (Prelude.Maybe Prelude.Natural)
autoScalingSettingsDescription_minimumUnits = Lens.lens (\AutoScalingSettingsDescription' {minimumUnits} -> minimumUnits) (\s@AutoScalingSettingsDescription' {} a -> s {minimumUnits = a} :: AutoScalingSettingsDescription)

-- | Information about the scaling policies.
autoScalingSettingsDescription_scalingPolicies :: Lens.Lens' AutoScalingSettingsDescription (Prelude.Maybe [AutoScalingPolicyDescription])
autoScalingSettingsDescription_scalingPolicies = Lens.lens (\AutoScalingSettingsDescription' {scalingPolicies} -> scalingPolicies) (\s@AutoScalingSettingsDescription' {} a -> s {scalingPolicies = a} :: AutoScalingSettingsDescription) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AutoScalingSettingsDescription where
  parseJSON =
    Data.withObject
      "AutoScalingSettingsDescription"
      ( \x ->
          AutoScalingSettingsDescription'
            Prelude.<$> (x Data..:? "AutoScalingDisabled")
            Prelude.<*> (x Data..:? "AutoScalingRoleArn")
            Prelude.<*> (x Data..:? "MaximumUnits")
            Prelude.<*> (x Data..:? "MinimumUnits")
            Prelude.<*> ( x Data..:? "ScalingPolicies"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    AutoScalingSettingsDescription
  where
  hashWithSalt
    _salt
    AutoScalingSettingsDescription' {..} =
      _salt `Prelude.hashWithSalt` autoScalingDisabled
        `Prelude.hashWithSalt` autoScalingRoleArn
        `Prelude.hashWithSalt` maximumUnits
        `Prelude.hashWithSalt` minimumUnits
        `Prelude.hashWithSalt` scalingPolicies

instance
  Prelude.NFData
    AutoScalingSettingsDescription
  where
  rnf AutoScalingSettingsDescription' {..} =
    Prelude.rnf autoScalingDisabled
      `Prelude.seq` Prelude.rnf autoScalingRoleArn
      `Prelude.seq` Prelude.rnf maximumUnits
      `Prelude.seq` Prelude.rnf minimumUnits
      `Prelude.seq` Prelude.rnf scalingPolicies

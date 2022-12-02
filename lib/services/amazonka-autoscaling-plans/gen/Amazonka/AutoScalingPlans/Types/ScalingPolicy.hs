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
-- Module      : Amazonka.AutoScalingPlans.Types.ScalingPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScalingPlans.Types.ScalingPolicy where

import Amazonka.AutoScalingPlans.Types.PolicyType
import Amazonka.AutoScalingPlans.Types.TargetTrackingConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a scaling policy.
--
-- /See:/ 'newScalingPolicy' smart constructor.
data ScalingPolicy = ScalingPolicy'
  { -- | The target tracking scaling policy. Includes support for predefined or
    -- customized metrics.
    targetTrackingConfiguration :: Prelude.Maybe TargetTrackingConfiguration,
    -- | The name of the scaling policy.
    policyName :: Prelude.Text,
    -- | The type of scaling policy.
    policyType :: PolicyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScalingPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetTrackingConfiguration', 'scalingPolicy_targetTrackingConfiguration' - The target tracking scaling policy. Includes support for predefined or
-- customized metrics.
--
-- 'policyName', 'scalingPolicy_policyName' - The name of the scaling policy.
--
-- 'policyType', 'scalingPolicy_policyType' - The type of scaling policy.
newScalingPolicy ::
  -- | 'policyName'
  Prelude.Text ->
  -- | 'policyType'
  PolicyType ->
  ScalingPolicy
newScalingPolicy pPolicyName_ pPolicyType_ =
  ScalingPolicy'
    { targetTrackingConfiguration =
        Prelude.Nothing,
      policyName = pPolicyName_,
      policyType = pPolicyType_
    }

-- | The target tracking scaling policy. Includes support for predefined or
-- customized metrics.
scalingPolicy_targetTrackingConfiguration :: Lens.Lens' ScalingPolicy (Prelude.Maybe TargetTrackingConfiguration)
scalingPolicy_targetTrackingConfiguration = Lens.lens (\ScalingPolicy' {targetTrackingConfiguration} -> targetTrackingConfiguration) (\s@ScalingPolicy' {} a -> s {targetTrackingConfiguration = a} :: ScalingPolicy)

-- | The name of the scaling policy.
scalingPolicy_policyName :: Lens.Lens' ScalingPolicy Prelude.Text
scalingPolicy_policyName = Lens.lens (\ScalingPolicy' {policyName} -> policyName) (\s@ScalingPolicy' {} a -> s {policyName = a} :: ScalingPolicy)

-- | The type of scaling policy.
scalingPolicy_policyType :: Lens.Lens' ScalingPolicy PolicyType
scalingPolicy_policyType = Lens.lens (\ScalingPolicy' {policyType} -> policyType) (\s@ScalingPolicy' {} a -> s {policyType = a} :: ScalingPolicy)

instance Data.FromJSON ScalingPolicy where
  parseJSON =
    Data.withObject
      "ScalingPolicy"
      ( \x ->
          ScalingPolicy'
            Prelude.<$> (x Data..:? "TargetTrackingConfiguration")
            Prelude.<*> (x Data..: "PolicyName")
            Prelude.<*> (x Data..: "PolicyType")
      )

instance Prelude.Hashable ScalingPolicy where
  hashWithSalt _salt ScalingPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` targetTrackingConfiguration
      `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` policyType

instance Prelude.NFData ScalingPolicy where
  rnf ScalingPolicy' {..} =
    Prelude.rnf targetTrackingConfiguration
      `Prelude.seq` Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf policyType

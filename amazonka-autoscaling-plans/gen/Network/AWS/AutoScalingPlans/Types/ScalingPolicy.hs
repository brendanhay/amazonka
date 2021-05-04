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
-- Module      : Network.AWS.AutoScalingPlans.Types.ScalingPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.ScalingPolicy where

import Network.AWS.AutoScalingPlans.Types.PolicyType
import Network.AWS.AutoScalingPlans.Types.TargetTrackingConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON ScalingPolicy where
  parseJSON =
    Prelude.withObject
      "ScalingPolicy"
      ( \x ->
          ScalingPolicy'
            Prelude.<$> (x Prelude..:? "TargetTrackingConfiguration")
            Prelude.<*> (x Prelude..: "PolicyName")
            Prelude.<*> (x Prelude..: "PolicyType")
      )

instance Prelude.Hashable ScalingPolicy

instance Prelude.NFData ScalingPolicy

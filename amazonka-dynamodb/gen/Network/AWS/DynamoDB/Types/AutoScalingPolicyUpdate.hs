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
-- Module      : Network.AWS.DynamoDB.Types.AutoScalingPolicyUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.AutoScalingPolicyUpdate where

import Network.AWS.DynamoDB.Types.AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the auto scaling policy to be modified.
--
-- /See:/ 'newAutoScalingPolicyUpdate' smart constructor.
data AutoScalingPolicyUpdate = AutoScalingPolicyUpdate'
  { -- | The name of the scaling policy.
    policyName :: Prelude.Maybe Prelude.Text,
    -- | Represents a target tracking scaling policy configuration.
    targetTrackingScalingPolicyConfiguration :: AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AutoScalingPolicyUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyName', 'autoScalingPolicyUpdate_policyName' - The name of the scaling policy.
--
-- 'targetTrackingScalingPolicyConfiguration', 'autoScalingPolicyUpdate_targetTrackingScalingPolicyConfiguration' - Represents a target tracking scaling policy configuration.
newAutoScalingPolicyUpdate ::
  -- | 'targetTrackingScalingPolicyConfiguration'
  AutoScalingTargetTrackingScalingPolicyConfigurationUpdate ->
  AutoScalingPolicyUpdate
newAutoScalingPolicyUpdate
  pTargetTrackingScalingPolicyConfiguration_ =
    AutoScalingPolicyUpdate'
      { policyName =
          Prelude.Nothing,
        targetTrackingScalingPolicyConfiguration =
          pTargetTrackingScalingPolicyConfiguration_
      }

-- | The name of the scaling policy.
autoScalingPolicyUpdate_policyName :: Lens.Lens' AutoScalingPolicyUpdate (Prelude.Maybe Prelude.Text)
autoScalingPolicyUpdate_policyName = Lens.lens (\AutoScalingPolicyUpdate' {policyName} -> policyName) (\s@AutoScalingPolicyUpdate' {} a -> s {policyName = a} :: AutoScalingPolicyUpdate)

-- | Represents a target tracking scaling policy configuration.
autoScalingPolicyUpdate_targetTrackingScalingPolicyConfiguration :: Lens.Lens' AutoScalingPolicyUpdate AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
autoScalingPolicyUpdate_targetTrackingScalingPolicyConfiguration = Lens.lens (\AutoScalingPolicyUpdate' {targetTrackingScalingPolicyConfiguration} -> targetTrackingScalingPolicyConfiguration) (\s@AutoScalingPolicyUpdate' {} a -> s {targetTrackingScalingPolicyConfiguration = a} :: AutoScalingPolicyUpdate)

instance Prelude.Hashable AutoScalingPolicyUpdate

instance Prelude.NFData AutoScalingPolicyUpdate

instance Prelude.ToJSON AutoScalingPolicyUpdate where
  toJSON AutoScalingPolicyUpdate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("PolicyName" Prelude..=) Prelude.<$> policyName,
            Prelude.Just
              ( "TargetTrackingScalingPolicyConfiguration"
                  Prelude..= targetTrackingScalingPolicyConfiguration
              )
          ]
      )

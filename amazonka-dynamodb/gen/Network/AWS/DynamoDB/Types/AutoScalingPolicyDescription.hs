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
-- Module      : Network.AWS.DynamoDB.Types.AutoScalingPolicyDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.AutoScalingPolicyDescription where

import Network.AWS.DynamoDB.Types.AutoScalingTargetTrackingScalingPolicyConfigurationDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the properties of the scaling policy.
--
-- /See:/ 'newAutoScalingPolicyDescription' smart constructor.
data AutoScalingPolicyDescription = AutoScalingPolicyDescription'
  { -- | The name of the scaling policy.
    policyName :: Prelude.Maybe Prelude.Text,
    -- | Represents a target tracking scaling policy configuration.
    targetTrackingScalingPolicyConfiguration :: Prelude.Maybe AutoScalingTargetTrackingScalingPolicyConfigurationDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AutoScalingPolicyDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyName', 'autoScalingPolicyDescription_policyName' - The name of the scaling policy.
--
-- 'targetTrackingScalingPolicyConfiguration', 'autoScalingPolicyDescription_targetTrackingScalingPolicyConfiguration' - Represents a target tracking scaling policy configuration.
newAutoScalingPolicyDescription ::
  AutoScalingPolicyDescription
newAutoScalingPolicyDescription =
  AutoScalingPolicyDescription'
    { policyName =
        Prelude.Nothing,
      targetTrackingScalingPolicyConfiguration =
        Prelude.Nothing
    }

-- | The name of the scaling policy.
autoScalingPolicyDescription_policyName :: Lens.Lens' AutoScalingPolicyDescription (Prelude.Maybe Prelude.Text)
autoScalingPolicyDescription_policyName = Lens.lens (\AutoScalingPolicyDescription' {policyName} -> policyName) (\s@AutoScalingPolicyDescription' {} a -> s {policyName = a} :: AutoScalingPolicyDescription)

-- | Represents a target tracking scaling policy configuration.
autoScalingPolicyDescription_targetTrackingScalingPolicyConfiguration :: Lens.Lens' AutoScalingPolicyDescription (Prelude.Maybe AutoScalingTargetTrackingScalingPolicyConfigurationDescription)
autoScalingPolicyDescription_targetTrackingScalingPolicyConfiguration = Lens.lens (\AutoScalingPolicyDescription' {targetTrackingScalingPolicyConfiguration} -> targetTrackingScalingPolicyConfiguration) (\s@AutoScalingPolicyDescription' {} a -> s {targetTrackingScalingPolicyConfiguration = a} :: AutoScalingPolicyDescription)

instance
  Prelude.FromJSON
    AutoScalingPolicyDescription
  where
  parseJSON =
    Prelude.withObject
      "AutoScalingPolicyDescription"
      ( \x ->
          AutoScalingPolicyDescription'
            Prelude.<$> (x Prelude..:? "PolicyName")
            Prelude.<*> ( x
                            Prelude..:? "TargetTrackingScalingPolicyConfiguration"
                        )
      )

instance
  Prelude.Hashable
    AutoScalingPolicyDescription

instance Prelude.NFData AutoScalingPolicyDescription

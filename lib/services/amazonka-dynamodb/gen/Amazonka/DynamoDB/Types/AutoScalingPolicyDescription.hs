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
-- Module      : Amazonka.DynamoDB.Types.AutoScalingPolicyDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.AutoScalingPolicyDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.AutoScalingTargetTrackingScalingPolicyConfigurationDescription
import Amazonka.DynamoDB.Types.TransactWriteItem
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents the properties of the scaling policy.
--
-- /See:/ 'newAutoScalingPolicyDescription' smart constructor.
data AutoScalingPolicyDescription = AutoScalingPolicyDescription'
  { -- | The name of the scaling policy.
    policyName :: Prelude.Maybe Prelude.Text,
    -- | Represents a target tracking scaling policy configuration.
    targetTrackingScalingPolicyConfiguration :: Prelude.Maybe AutoScalingTargetTrackingScalingPolicyConfigurationDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON AutoScalingPolicyDescription where
  parseJSON =
    Data.withObject
      "AutoScalingPolicyDescription"
      ( \x ->
          AutoScalingPolicyDescription'
            Prelude.<$> (x Data..:? "PolicyName")
            Prelude.<*> ( x
                            Data..:? "TargetTrackingScalingPolicyConfiguration"
                        )
      )

instance
  Prelude.Hashable
    AutoScalingPolicyDescription
  where
  hashWithSalt _salt AutoScalingPolicyDescription' {..} =
    _salt
      `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` targetTrackingScalingPolicyConfiguration

instance Prelude.NFData AutoScalingPolicyDescription where
  rnf AutoScalingPolicyDescription' {..} =
    Prelude.rnf policyName `Prelude.seq`
      Prelude.rnf targetTrackingScalingPolicyConfiguration

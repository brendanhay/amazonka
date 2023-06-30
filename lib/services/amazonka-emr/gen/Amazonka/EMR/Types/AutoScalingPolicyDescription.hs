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
-- Module      : Amazonka.EMR.Types.AutoScalingPolicyDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.AutoScalingPolicyDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types.AutoScalingPolicyStatus
import Amazonka.EMR.Types.ScalingConstraints
import Amazonka.EMR.Types.ScalingRule
import qualified Amazonka.Prelude as Prelude

-- | An automatic scaling policy for a core instance group or task instance
-- group in an Amazon EMR cluster. The automatic scaling policy defines how
-- an instance group dynamically adds and terminates EC2 instances in
-- response to the value of a CloudWatch metric. See PutAutoScalingPolicy.
--
-- /See:/ 'newAutoScalingPolicyDescription' smart constructor.
data AutoScalingPolicyDescription = AutoScalingPolicyDescription'
  { -- | The upper and lower EC2 instance limits for an automatic scaling policy.
    -- Automatic scaling activity will not cause an instance group to grow
    -- above or below these limits.
    constraints :: Prelude.Maybe ScalingConstraints,
    -- | The scale-in and scale-out rules that comprise the automatic scaling
    -- policy.
    rules :: Prelude.Maybe [ScalingRule],
    -- | The status of an automatic scaling policy.
    status :: Prelude.Maybe AutoScalingPolicyStatus
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
-- 'constraints', 'autoScalingPolicyDescription_constraints' - The upper and lower EC2 instance limits for an automatic scaling policy.
-- Automatic scaling activity will not cause an instance group to grow
-- above or below these limits.
--
-- 'rules', 'autoScalingPolicyDescription_rules' - The scale-in and scale-out rules that comprise the automatic scaling
-- policy.
--
-- 'status', 'autoScalingPolicyDescription_status' - The status of an automatic scaling policy.
newAutoScalingPolicyDescription ::
  AutoScalingPolicyDescription
newAutoScalingPolicyDescription =
  AutoScalingPolicyDescription'
    { constraints =
        Prelude.Nothing,
      rules = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The upper and lower EC2 instance limits for an automatic scaling policy.
-- Automatic scaling activity will not cause an instance group to grow
-- above or below these limits.
autoScalingPolicyDescription_constraints :: Lens.Lens' AutoScalingPolicyDescription (Prelude.Maybe ScalingConstraints)
autoScalingPolicyDescription_constraints = Lens.lens (\AutoScalingPolicyDescription' {constraints} -> constraints) (\s@AutoScalingPolicyDescription' {} a -> s {constraints = a} :: AutoScalingPolicyDescription)

-- | The scale-in and scale-out rules that comprise the automatic scaling
-- policy.
autoScalingPolicyDescription_rules :: Lens.Lens' AutoScalingPolicyDescription (Prelude.Maybe [ScalingRule])
autoScalingPolicyDescription_rules = Lens.lens (\AutoScalingPolicyDescription' {rules} -> rules) (\s@AutoScalingPolicyDescription' {} a -> s {rules = a} :: AutoScalingPolicyDescription) Prelude.. Lens.mapping Lens.coerced

-- | The status of an automatic scaling policy.
autoScalingPolicyDescription_status :: Lens.Lens' AutoScalingPolicyDescription (Prelude.Maybe AutoScalingPolicyStatus)
autoScalingPolicyDescription_status = Lens.lens (\AutoScalingPolicyDescription' {status} -> status) (\s@AutoScalingPolicyDescription' {} a -> s {status = a} :: AutoScalingPolicyDescription)

instance Data.FromJSON AutoScalingPolicyDescription where
  parseJSON =
    Data.withObject
      "AutoScalingPolicyDescription"
      ( \x ->
          AutoScalingPolicyDescription'
            Prelude.<$> (x Data..:? "Constraints")
            Prelude.<*> (x Data..:? "Rules" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Status")
      )

instance
  Prelude.Hashable
    AutoScalingPolicyDescription
  where
  hashWithSalt _salt AutoScalingPolicyDescription' {..} =
    _salt
      `Prelude.hashWithSalt` constraints
      `Prelude.hashWithSalt` rules
      `Prelude.hashWithSalt` status

instance Prelude.NFData AutoScalingPolicyDescription where
  rnf AutoScalingPolicyDescription' {..} =
    Prelude.rnf constraints
      `Prelude.seq` Prelude.rnf rules
      `Prelude.seq` Prelude.rnf status

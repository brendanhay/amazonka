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
-- Module      : Amazonka.EMR.Types.AutoScalingPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.AutoScalingPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types.ScalingConstraints
import Amazonka.EMR.Types.ScalingRule
import qualified Amazonka.Prelude as Prelude

-- | An automatic scaling policy for a core instance group or task instance
-- group in an Amazon EMR cluster. An automatic scaling policy defines how
-- an instance group dynamically adds and terminates Amazon EC2 instances
-- in response to the value of a CloudWatch metric. See
-- PutAutoScalingPolicy.
--
-- /See:/ 'newAutoScalingPolicy' smart constructor.
data AutoScalingPolicy = AutoScalingPolicy'
  { -- | The upper and lower Amazon EC2 instance limits for an automatic scaling
    -- policy. Automatic scaling activity will not cause an instance group to
    -- grow above or below these limits.
    constraints :: ScalingConstraints,
    -- | The scale-in and scale-out rules that comprise the automatic scaling
    -- policy.
    rules :: [ScalingRule]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoScalingPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'constraints', 'autoScalingPolicy_constraints' - The upper and lower Amazon EC2 instance limits for an automatic scaling
-- policy. Automatic scaling activity will not cause an instance group to
-- grow above or below these limits.
--
-- 'rules', 'autoScalingPolicy_rules' - The scale-in and scale-out rules that comprise the automatic scaling
-- policy.
newAutoScalingPolicy ::
  -- | 'constraints'
  ScalingConstraints ->
  AutoScalingPolicy
newAutoScalingPolicy pConstraints_ =
  AutoScalingPolicy'
    { constraints = pConstraints_,
      rules = Prelude.mempty
    }

-- | The upper and lower Amazon EC2 instance limits for an automatic scaling
-- policy. Automatic scaling activity will not cause an instance group to
-- grow above or below these limits.
autoScalingPolicy_constraints :: Lens.Lens' AutoScalingPolicy ScalingConstraints
autoScalingPolicy_constraints = Lens.lens (\AutoScalingPolicy' {constraints} -> constraints) (\s@AutoScalingPolicy' {} a -> s {constraints = a} :: AutoScalingPolicy)

-- | The scale-in and scale-out rules that comprise the automatic scaling
-- policy.
autoScalingPolicy_rules :: Lens.Lens' AutoScalingPolicy [ScalingRule]
autoScalingPolicy_rules = Lens.lens (\AutoScalingPolicy' {rules} -> rules) (\s@AutoScalingPolicy' {} a -> s {rules = a} :: AutoScalingPolicy) Prelude.. Lens.coerced

instance Prelude.Hashable AutoScalingPolicy where
  hashWithSalt _salt AutoScalingPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` constraints
      `Prelude.hashWithSalt` rules

instance Prelude.NFData AutoScalingPolicy where
  rnf AutoScalingPolicy' {..} =
    Prelude.rnf constraints
      `Prelude.seq` Prelude.rnf rules

instance Data.ToJSON AutoScalingPolicy where
  toJSON AutoScalingPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Constraints" Data..= constraints),
            Prelude.Just ("Rules" Data..= rules)
          ]
      )

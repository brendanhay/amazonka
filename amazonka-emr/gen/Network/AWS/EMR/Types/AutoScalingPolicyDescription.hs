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
-- Module      : Network.AWS.EMR.Types.AutoScalingPolicyDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.AutoScalingPolicyDescription where

import Network.AWS.EMR.Types.AutoScalingPolicyStatus
import Network.AWS.EMR.Types.ScalingConstraints
import Network.AWS.EMR.Types.ScalingRule
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An automatic scaling policy for a core instance group or task instance
-- group in an Amazon EMR cluster. The automatic scaling policy defines how
-- an instance group dynamically adds and terminates EC2 instances in
-- response to the value of a CloudWatch metric. See PutAutoScalingPolicy.
--
-- /See:/ 'newAutoScalingPolicyDescription' smart constructor.
data AutoScalingPolicyDescription = AutoScalingPolicyDescription'
  { -- | The status of an automatic scaling policy.
    status :: Prelude.Maybe AutoScalingPolicyStatus,
    -- | The upper and lower EC2 instance limits for an automatic scaling policy.
    -- Automatic scaling activity will not cause an instance group to grow
    -- above or below these limits.
    constraints :: Prelude.Maybe ScalingConstraints,
    -- | The scale-in and scale-out rules that comprise the automatic scaling
    -- policy.
    rules :: Prelude.Maybe [ScalingRule]
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
-- 'status', 'autoScalingPolicyDescription_status' - The status of an automatic scaling policy.
--
-- 'constraints', 'autoScalingPolicyDescription_constraints' - The upper and lower EC2 instance limits for an automatic scaling policy.
-- Automatic scaling activity will not cause an instance group to grow
-- above or below these limits.
--
-- 'rules', 'autoScalingPolicyDescription_rules' - The scale-in and scale-out rules that comprise the automatic scaling
-- policy.
newAutoScalingPolicyDescription ::
  AutoScalingPolicyDescription
newAutoScalingPolicyDescription =
  AutoScalingPolicyDescription'
    { status =
        Prelude.Nothing,
      constraints = Prelude.Nothing,
      rules = Prelude.Nothing
    }

-- | The status of an automatic scaling policy.
autoScalingPolicyDescription_status :: Lens.Lens' AutoScalingPolicyDescription (Prelude.Maybe AutoScalingPolicyStatus)
autoScalingPolicyDescription_status = Lens.lens (\AutoScalingPolicyDescription' {status} -> status) (\s@AutoScalingPolicyDescription' {} a -> s {status = a} :: AutoScalingPolicyDescription)

-- | The upper and lower EC2 instance limits for an automatic scaling policy.
-- Automatic scaling activity will not cause an instance group to grow
-- above or below these limits.
autoScalingPolicyDescription_constraints :: Lens.Lens' AutoScalingPolicyDescription (Prelude.Maybe ScalingConstraints)
autoScalingPolicyDescription_constraints = Lens.lens (\AutoScalingPolicyDescription' {constraints} -> constraints) (\s@AutoScalingPolicyDescription' {} a -> s {constraints = a} :: AutoScalingPolicyDescription)

-- | The scale-in and scale-out rules that comprise the automatic scaling
-- policy.
autoScalingPolicyDescription_rules :: Lens.Lens' AutoScalingPolicyDescription (Prelude.Maybe [ScalingRule])
autoScalingPolicyDescription_rules = Lens.lens (\AutoScalingPolicyDescription' {rules} -> rules) (\s@AutoScalingPolicyDescription' {} a -> s {rules = a} :: AutoScalingPolicyDescription) Prelude.. Lens.mapping Prelude._Coerce

instance
  Prelude.FromJSON
    AutoScalingPolicyDescription
  where
  parseJSON =
    Prelude.withObject
      "AutoScalingPolicyDescription"
      ( \x ->
          AutoScalingPolicyDescription'
            Prelude.<$> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "Constraints")
            Prelude.<*> (x Prelude..:? "Rules" Prelude..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    AutoScalingPolicyDescription

instance Prelude.NFData AutoScalingPolicyDescription

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

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types.AutoScalingPolicyStatus
import Network.AWS.EMR.Types.ScalingConstraints
import Network.AWS.EMR.Types.ScalingRule
import qualified Network.AWS.Lens as Lens

-- | An automatic scaling policy for a core instance group or task instance
-- group in an Amazon EMR cluster. The automatic scaling policy defines how
-- an instance group dynamically adds and terminates EC2 instances in
-- response to the value of a CloudWatch metric. See PutAutoScalingPolicy.
--
-- /See:/ 'newAutoScalingPolicyDescription' smart constructor.
data AutoScalingPolicyDescription = AutoScalingPolicyDescription'
  { -- | The status of an automatic scaling policy.
    status :: Core.Maybe AutoScalingPolicyStatus,
    -- | The upper and lower EC2 instance limits for an automatic scaling policy.
    -- Automatic scaling activity will not cause an instance group to grow
    -- above or below these limits.
    constraints :: Core.Maybe ScalingConstraints,
    -- | The scale-in and scale-out rules that comprise the automatic scaling
    -- policy.
    rules :: Core.Maybe [ScalingRule]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      constraints = Core.Nothing,
      rules = Core.Nothing
    }

-- | The status of an automatic scaling policy.
autoScalingPolicyDescription_status :: Lens.Lens' AutoScalingPolicyDescription (Core.Maybe AutoScalingPolicyStatus)
autoScalingPolicyDescription_status = Lens.lens (\AutoScalingPolicyDescription' {status} -> status) (\s@AutoScalingPolicyDescription' {} a -> s {status = a} :: AutoScalingPolicyDescription)

-- | The upper and lower EC2 instance limits for an automatic scaling policy.
-- Automatic scaling activity will not cause an instance group to grow
-- above or below these limits.
autoScalingPolicyDescription_constraints :: Lens.Lens' AutoScalingPolicyDescription (Core.Maybe ScalingConstraints)
autoScalingPolicyDescription_constraints = Lens.lens (\AutoScalingPolicyDescription' {constraints} -> constraints) (\s@AutoScalingPolicyDescription' {} a -> s {constraints = a} :: AutoScalingPolicyDescription)

-- | The scale-in and scale-out rules that comprise the automatic scaling
-- policy.
autoScalingPolicyDescription_rules :: Lens.Lens' AutoScalingPolicyDescription (Core.Maybe [ScalingRule])
autoScalingPolicyDescription_rules = Lens.lens (\AutoScalingPolicyDescription' {rules} -> rules) (\s@AutoScalingPolicyDescription' {} a -> s {rules = a} :: AutoScalingPolicyDescription) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON AutoScalingPolicyDescription where
  parseJSON =
    Core.withObject
      "AutoScalingPolicyDescription"
      ( \x ->
          AutoScalingPolicyDescription'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "Constraints")
            Core.<*> (x Core..:? "Rules" Core..!= Core.mempty)
      )

instance Core.Hashable AutoScalingPolicyDescription

instance Core.NFData AutoScalingPolicyDescription

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
-- Module      : Network.AWS.EMR.Types.ScalingRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ScalingRule where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types.ScalingAction
import Network.AWS.EMR.Types.ScalingTrigger
import qualified Network.AWS.Lens as Lens

-- | A scale-in or scale-out rule that defines scaling activity, including
-- the CloudWatch metric alarm that triggers activity, how EC2 instances
-- are added or removed, and the periodicity of adjustments. The automatic
-- scaling policy for an instance group can comprise one or more automatic
-- scaling rules.
--
-- /See:/ 'newScalingRule' smart constructor.
data ScalingRule = ScalingRule'
  { -- | A friendly, more verbose description of the automatic scaling rule.
    description :: Core.Maybe Core.Text,
    -- | The name used to identify an automatic scaling rule. Rule names must be
    -- unique within a scaling policy.
    name :: Core.Text,
    -- | The conditions that trigger an automatic scaling activity.
    action :: ScalingAction,
    -- | The CloudWatch alarm definition that determines when automatic scaling
    -- activity is triggered.
    trigger :: ScalingTrigger
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ScalingRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'scalingRule_description' - A friendly, more verbose description of the automatic scaling rule.
--
-- 'name', 'scalingRule_name' - The name used to identify an automatic scaling rule. Rule names must be
-- unique within a scaling policy.
--
-- 'action', 'scalingRule_action' - The conditions that trigger an automatic scaling activity.
--
-- 'trigger', 'scalingRule_trigger' - The CloudWatch alarm definition that determines when automatic scaling
-- activity is triggered.
newScalingRule ::
  -- | 'name'
  Core.Text ->
  -- | 'action'
  ScalingAction ->
  -- | 'trigger'
  ScalingTrigger ->
  ScalingRule
newScalingRule pName_ pAction_ pTrigger_ =
  ScalingRule'
    { description = Core.Nothing,
      name = pName_,
      action = pAction_,
      trigger = pTrigger_
    }

-- | A friendly, more verbose description of the automatic scaling rule.
scalingRule_description :: Lens.Lens' ScalingRule (Core.Maybe Core.Text)
scalingRule_description = Lens.lens (\ScalingRule' {description} -> description) (\s@ScalingRule' {} a -> s {description = a} :: ScalingRule)

-- | The name used to identify an automatic scaling rule. Rule names must be
-- unique within a scaling policy.
scalingRule_name :: Lens.Lens' ScalingRule Core.Text
scalingRule_name = Lens.lens (\ScalingRule' {name} -> name) (\s@ScalingRule' {} a -> s {name = a} :: ScalingRule)

-- | The conditions that trigger an automatic scaling activity.
scalingRule_action :: Lens.Lens' ScalingRule ScalingAction
scalingRule_action = Lens.lens (\ScalingRule' {action} -> action) (\s@ScalingRule' {} a -> s {action = a} :: ScalingRule)

-- | The CloudWatch alarm definition that determines when automatic scaling
-- activity is triggered.
scalingRule_trigger :: Lens.Lens' ScalingRule ScalingTrigger
scalingRule_trigger = Lens.lens (\ScalingRule' {trigger} -> trigger) (\s@ScalingRule' {} a -> s {trigger = a} :: ScalingRule)

instance Core.FromJSON ScalingRule where
  parseJSON =
    Core.withObject
      "ScalingRule"
      ( \x ->
          ScalingRule'
            Core.<$> (x Core..:? "Description")
            Core.<*> (x Core..: "Name")
            Core.<*> (x Core..: "Action")
            Core.<*> (x Core..: "Trigger")
      )

instance Core.Hashable ScalingRule

instance Core.NFData ScalingRule

instance Core.ToJSON ScalingRule where
  toJSON ScalingRule' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Description" Core..=) Core.<$> description,
            Core.Just ("Name" Core..= name),
            Core.Just ("Action" Core..= action),
            Core.Just ("Trigger" Core..= trigger)
          ]
      )

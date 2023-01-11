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
-- Module      : Amazonka.Route53RecoveryControlConfig.Types.NewGatingRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryControlConfig.Types.NewGatingRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53RecoveryControlConfig.Types.RuleConfig

-- | A new gating rule for a control panel.
--
-- /See:/ 'newNewGatingRule' smart constructor.
data NewGatingRule = NewGatingRule'
  { -- | Routing controls that can only be set or unset if the specified
    -- RuleConfig evaluates to true for the specified GatingControls. For
    -- example, say you have three gating controls, one for each of three
    -- Amazon Web Services Regions. Now you specify ATLEAST 2 as your
    -- RuleConfig. With these settings, you can only change (set or unset) the
    -- routing controls that you have specified as TargetControls if that rule
    -- evaluates to true.
    --
    -- In other words, your ability to change the routing controls that you
    -- have specified as TargetControls is gated by the rule that you set for
    -- the routing controls in GatingControls.
    targetControls :: [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of the control panel.
    controlPanelArn :: Prelude.Text,
    -- | The gating controls for the new gating rule. That is, routing controls
    -- that are evaluated by the rule configuration that you specify.
    gatingControls :: [Prelude.Text],
    -- | The criteria that you set for specific gating controls (routing
    -- controls) that designate how many control states must be ON to allow you
    -- to change (set or unset) the target control states.
    ruleConfig :: RuleConfig,
    -- | An evaluation period, in milliseconds (ms), during which any request
    -- against the target routing controls will fail. This helps prevent
    -- \"flapping\" of state. The wait period is 5000 ms by default, but you
    -- can choose a custom value.
    waitPeriodMs :: Prelude.Int,
    -- | The name for the new gating rule.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NewGatingRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetControls', 'newGatingRule_targetControls' - Routing controls that can only be set or unset if the specified
-- RuleConfig evaluates to true for the specified GatingControls. For
-- example, say you have three gating controls, one for each of three
-- Amazon Web Services Regions. Now you specify ATLEAST 2 as your
-- RuleConfig. With these settings, you can only change (set or unset) the
-- routing controls that you have specified as TargetControls if that rule
-- evaluates to true.
--
-- In other words, your ability to change the routing controls that you
-- have specified as TargetControls is gated by the rule that you set for
-- the routing controls in GatingControls.
--
-- 'controlPanelArn', 'newGatingRule_controlPanelArn' - The Amazon Resource Name (ARN) of the control panel.
--
-- 'gatingControls', 'newGatingRule_gatingControls' - The gating controls for the new gating rule. That is, routing controls
-- that are evaluated by the rule configuration that you specify.
--
-- 'ruleConfig', 'newGatingRule_ruleConfig' - The criteria that you set for specific gating controls (routing
-- controls) that designate how many control states must be ON to allow you
-- to change (set or unset) the target control states.
--
-- 'waitPeriodMs', 'newGatingRule_waitPeriodMs' - An evaluation period, in milliseconds (ms), during which any request
-- against the target routing controls will fail. This helps prevent
-- \"flapping\" of state. The wait period is 5000 ms by default, but you
-- can choose a custom value.
--
-- 'name', 'newGatingRule_name' - The name for the new gating rule.
newNewGatingRule ::
  -- | 'controlPanelArn'
  Prelude.Text ->
  -- | 'ruleConfig'
  RuleConfig ->
  -- | 'waitPeriodMs'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  NewGatingRule
newNewGatingRule
  pControlPanelArn_
  pRuleConfig_
  pWaitPeriodMs_
  pName_ =
    NewGatingRule'
      { targetControls = Prelude.mempty,
        controlPanelArn = pControlPanelArn_,
        gatingControls = Prelude.mempty,
        ruleConfig = pRuleConfig_,
        waitPeriodMs = pWaitPeriodMs_,
        name = pName_
      }

-- | Routing controls that can only be set or unset if the specified
-- RuleConfig evaluates to true for the specified GatingControls. For
-- example, say you have three gating controls, one for each of three
-- Amazon Web Services Regions. Now you specify ATLEAST 2 as your
-- RuleConfig. With these settings, you can only change (set or unset) the
-- routing controls that you have specified as TargetControls if that rule
-- evaluates to true.
--
-- In other words, your ability to change the routing controls that you
-- have specified as TargetControls is gated by the rule that you set for
-- the routing controls in GatingControls.
newGatingRule_targetControls :: Lens.Lens' NewGatingRule [Prelude.Text]
newGatingRule_targetControls = Lens.lens (\NewGatingRule' {targetControls} -> targetControls) (\s@NewGatingRule' {} a -> s {targetControls = a} :: NewGatingRule) Prelude.. Lens.coerced

-- | The Amazon Resource Name (ARN) of the control panel.
newGatingRule_controlPanelArn :: Lens.Lens' NewGatingRule Prelude.Text
newGatingRule_controlPanelArn = Lens.lens (\NewGatingRule' {controlPanelArn} -> controlPanelArn) (\s@NewGatingRule' {} a -> s {controlPanelArn = a} :: NewGatingRule)

-- | The gating controls for the new gating rule. That is, routing controls
-- that are evaluated by the rule configuration that you specify.
newGatingRule_gatingControls :: Lens.Lens' NewGatingRule [Prelude.Text]
newGatingRule_gatingControls = Lens.lens (\NewGatingRule' {gatingControls} -> gatingControls) (\s@NewGatingRule' {} a -> s {gatingControls = a} :: NewGatingRule) Prelude.. Lens.coerced

-- | The criteria that you set for specific gating controls (routing
-- controls) that designate how many control states must be ON to allow you
-- to change (set or unset) the target control states.
newGatingRule_ruleConfig :: Lens.Lens' NewGatingRule RuleConfig
newGatingRule_ruleConfig = Lens.lens (\NewGatingRule' {ruleConfig} -> ruleConfig) (\s@NewGatingRule' {} a -> s {ruleConfig = a} :: NewGatingRule)

-- | An evaluation period, in milliseconds (ms), during which any request
-- against the target routing controls will fail. This helps prevent
-- \"flapping\" of state. The wait period is 5000 ms by default, but you
-- can choose a custom value.
newGatingRule_waitPeriodMs :: Lens.Lens' NewGatingRule Prelude.Int
newGatingRule_waitPeriodMs = Lens.lens (\NewGatingRule' {waitPeriodMs} -> waitPeriodMs) (\s@NewGatingRule' {} a -> s {waitPeriodMs = a} :: NewGatingRule)

-- | The name for the new gating rule.
newGatingRule_name :: Lens.Lens' NewGatingRule Prelude.Text
newGatingRule_name = Lens.lens (\NewGatingRule' {name} -> name) (\s@NewGatingRule' {} a -> s {name = a} :: NewGatingRule)

instance Prelude.Hashable NewGatingRule where
  hashWithSalt _salt NewGatingRule' {..} =
    _salt `Prelude.hashWithSalt` targetControls
      `Prelude.hashWithSalt` controlPanelArn
      `Prelude.hashWithSalt` gatingControls
      `Prelude.hashWithSalt` ruleConfig
      `Prelude.hashWithSalt` waitPeriodMs
      `Prelude.hashWithSalt` name

instance Prelude.NFData NewGatingRule where
  rnf NewGatingRule' {..} =
    Prelude.rnf targetControls
      `Prelude.seq` Prelude.rnf controlPanelArn
      `Prelude.seq` Prelude.rnf gatingControls
      `Prelude.seq` Prelude.rnf ruleConfig
      `Prelude.seq` Prelude.rnf waitPeriodMs
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON NewGatingRule where
  toJSON NewGatingRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("TargetControls" Data..= targetControls),
            Prelude.Just
              ("ControlPanelArn" Data..= controlPanelArn),
            Prelude.Just
              ("GatingControls" Data..= gatingControls),
            Prelude.Just ("RuleConfig" Data..= ruleConfig),
            Prelude.Just ("WaitPeriodMs" Data..= waitPeriodMs),
            Prelude.Just ("Name" Data..= name)
          ]
      )

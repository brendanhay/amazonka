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
-- Module      : Amazonka.Route53RecoveryControlConfig.Types.GatingRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryControlConfig.Types.GatingRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53RecoveryControlConfig.Types.RuleConfig
import Amazonka.Route53RecoveryControlConfig.Types.Status

-- | A gating rule verifies that a gating routing control or set of gating
-- routing controls, evaluates as true, based on a rule configuration that
-- you specify, which allows a set of routing control state changes to
-- complete.
--
-- For example, if you specify one gating routing control and you set the
-- Type in the rule configuration to OR, that indicates that you must set
-- the gating routing control to On for the rule to evaluate as true; that
-- is, for the gating control \"switch\" to be \"On\". When you do that,
-- then you can update the routing control states for the target routing
-- controls that you specify in the gating rule.
--
-- /See:/ 'newGatingRule' smart constructor.
data GatingRule = GatingRule'
  { -- | The deployment status of a gating rule. Status can be one of the
    -- following: PENDING, DEPLOYED, PENDING_DELETION.
    status :: Status,
    -- | An array of target routing control Amazon Resource Names (ARNs) for
    -- which the states can only be updated if the rule configuration that you
    -- specify evaluates to true for the gating routing control. As a simple
    -- example, if you have a single gating control, it acts as an overall
    -- \"on\/off\" switch for a set of target routing controls. You can use
    -- this to manually override automated failover, for example.
    targetControls :: [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of the control panel.
    controlPanelArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the gating rule.
    safetyRuleArn :: Prelude.Text,
    -- | An array of gating routing control Amazon Resource Names (ARNs). For a
    -- simple \"on\/off\" switch, specify the ARN for one routing control. The
    -- gating routing controls are evaluated by the rule configuration that you
    -- specify to determine if the target routing control states can be
    -- changed.
    gatingControls :: [Prelude.Text],
    -- | The criteria that you set for gating routing controls that designate how
    -- many of the routing control states must be ON to allow you to update
    -- target routing control states.
    ruleConfig :: RuleConfig,
    -- | An evaluation period, in milliseconds (ms), during which any request
    -- against the target routing controls will fail. This helps prevent
    -- \"flapping\" of state. The wait period is 5000 ms by default, but you
    -- can choose a custom value.
    waitPeriodMs :: Prelude.Int,
    -- | The name for the gating rule. You can use any non-white space character
    -- in the name.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GatingRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'gatingRule_status' - The deployment status of a gating rule. Status can be one of the
-- following: PENDING, DEPLOYED, PENDING_DELETION.
--
-- 'targetControls', 'gatingRule_targetControls' - An array of target routing control Amazon Resource Names (ARNs) for
-- which the states can only be updated if the rule configuration that you
-- specify evaluates to true for the gating routing control. As a simple
-- example, if you have a single gating control, it acts as an overall
-- \"on\/off\" switch for a set of target routing controls. You can use
-- this to manually override automated failover, for example.
--
-- 'controlPanelArn', 'gatingRule_controlPanelArn' - The Amazon Resource Name (ARN) of the control panel.
--
-- 'safetyRuleArn', 'gatingRule_safetyRuleArn' - The Amazon Resource Name (ARN) of the gating rule.
--
-- 'gatingControls', 'gatingRule_gatingControls' - An array of gating routing control Amazon Resource Names (ARNs). For a
-- simple \"on\/off\" switch, specify the ARN for one routing control. The
-- gating routing controls are evaluated by the rule configuration that you
-- specify to determine if the target routing control states can be
-- changed.
--
-- 'ruleConfig', 'gatingRule_ruleConfig' - The criteria that you set for gating routing controls that designate how
-- many of the routing control states must be ON to allow you to update
-- target routing control states.
--
-- 'waitPeriodMs', 'gatingRule_waitPeriodMs' - An evaluation period, in milliseconds (ms), during which any request
-- against the target routing controls will fail. This helps prevent
-- \"flapping\" of state. The wait period is 5000 ms by default, but you
-- can choose a custom value.
--
-- 'name', 'gatingRule_name' - The name for the gating rule. You can use any non-white space character
-- in the name.
newGatingRule ::
  -- | 'status'
  Status ->
  -- | 'controlPanelArn'
  Prelude.Text ->
  -- | 'safetyRuleArn'
  Prelude.Text ->
  -- | 'ruleConfig'
  RuleConfig ->
  -- | 'waitPeriodMs'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  GatingRule
newGatingRule
  pStatus_
  pControlPanelArn_
  pSafetyRuleArn_
  pRuleConfig_
  pWaitPeriodMs_
  pName_ =
    GatingRule'
      { status = pStatus_,
        targetControls = Prelude.mempty,
        controlPanelArn = pControlPanelArn_,
        safetyRuleArn = pSafetyRuleArn_,
        gatingControls = Prelude.mempty,
        ruleConfig = pRuleConfig_,
        waitPeriodMs = pWaitPeriodMs_,
        name = pName_
      }

-- | The deployment status of a gating rule. Status can be one of the
-- following: PENDING, DEPLOYED, PENDING_DELETION.
gatingRule_status :: Lens.Lens' GatingRule Status
gatingRule_status = Lens.lens (\GatingRule' {status} -> status) (\s@GatingRule' {} a -> s {status = a} :: GatingRule)

-- | An array of target routing control Amazon Resource Names (ARNs) for
-- which the states can only be updated if the rule configuration that you
-- specify evaluates to true for the gating routing control. As a simple
-- example, if you have a single gating control, it acts as an overall
-- \"on\/off\" switch for a set of target routing controls. You can use
-- this to manually override automated failover, for example.
gatingRule_targetControls :: Lens.Lens' GatingRule [Prelude.Text]
gatingRule_targetControls = Lens.lens (\GatingRule' {targetControls} -> targetControls) (\s@GatingRule' {} a -> s {targetControls = a} :: GatingRule) Prelude.. Lens.coerced

-- | The Amazon Resource Name (ARN) of the control panel.
gatingRule_controlPanelArn :: Lens.Lens' GatingRule Prelude.Text
gatingRule_controlPanelArn = Lens.lens (\GatingRule' {controlPanelArn} -> controlPanelArn) (\s@GatingRule' {} a -> s {controlPanelArn = a} :: GatingRule)

-- | The Amazon Resource Name (ARN) of the gating rule.
gatingRule_safetyRuleArn :: Lens.Lens' GatingRule Prelude.Text
gatingRule_safetyRuleArn = Lens.lens (\GatingRule' {safetyRuleArn} -> safetyRuleArn) (\s@GatingRule' {} a -> s {safetyRuleArn = a} :: GatingRule)

-- | An array of gating routing control Amazon Resource Names (ARNs). For a
-- simple \"on\/off\" switch, specify the ARN for one routing control. The
-- gating routing controls are evaluated by the rule configuration that you
-- specify to determine if the target routing control states can be
-- changed.
gatingRule_gatingControls :: Lens.Lens' GatingRule [Prelude.Text]
gatingRule_gatingControls = Lens.lens (\GatingRule' {gatingControls} -> gatingControls) (\s@GatingRule' {} a -> s {gatingControls = a} :: GatingRule) Prelude.. Lens.coerced

-- | The criteria that you set for gating routing controls that designate how
-- many of the routing control states must be ON to allow you to update
-- target routing control states.
gatingRule_ruleConfig :: Lens.Lens' GatingRule RuleConfig
gatingRule_ruleConfig = Lens.lens (\GatingRule' {ruleConfig} -> ruleConfig) (\s@GatingRule' {} a -> s {ruleConfig = a} :: GatingRule)

-- | An evaluation period, in milliseconds (ms), during which any request
-- against the target routing controls will fail. This helps prevent
-- \"flapping\" of state. The wait period is 5000 ms by default, but you
-- can choose a custom value.
gatingRule_waitPeriodMs :: Lens.Lens' GatingRule Prelude.Int
gatingRule_waitPeriodMs = Lens.lens (\GatingRule' {waitPeriodMs} -> waitPeriodMs) (\s@GatingRule' {} a -> s {waitPeriodMs = a} :: GatingRule)

-- | The name for the gating rule. You can use any non-white space character
-- in the name.
gatingRule_name :: Lens.Lens' GatingRule Prelude.Text
gatingRule_name = Lens.lens (\GatingRule' {name} -> name) (\s@GatingRule' {} a -> s {name = a} :: GatingRule)

instance Data.FromJSON GatingRule where
  parseJSON =
    Data.withObject
      "GatingRule"
      ( \x ->
          GatingRule'
            Prelude.<$> (x Data..: "Status")
            Prelude.<*> (x Data..:? "TargetControls" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "ControlPanelArn")
            Prelude.<*> (x Data..: "SafetyRuleArn")
            Prelude.<*> (x Data..:? "GatingControls" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "RuleConfig")
            Prelude.<*> (x Data..: "WaitPeriodMs")
            Prelude.<*> (x Data..: "Name")
      )

instance Prelude.Hashable GatingRule where
  hashWithSalt _salt GatingRule' {..} =
    _salt
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` targetControls
      `Prelude.hashWithSalt` controlPanelArn
      `Prelude.hashWithSalt` safetyRuleArn
      `Prelude.hashWithSalt` gatingControls
      `Prelude.hashWithSalt` ruleConfig
      `Prelude.hashWithSalt` waitPeriodMs
      `Prelude.hashWithSalt` name

instance Prelude.NFData GatingRule where
  rnf GatingRule' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf targetControls
      `Prelude.seq` Prelude.rnf controlPanelArn
      `Prelude.seq` Prelude.rnf safetyRuleArn
      `Prelude.seq` Prelude.rnf gatingControls
      `Prelude.seq` Prelude.rnf ruleConfig
      `Prelude.seq` Prelude.rnf waitPeriodMs
      `Prelude.seq` Prelude.rnf name

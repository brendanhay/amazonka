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
-- Module      : Amazonka.Route53RecoveryControlConfig.Types.AssertionRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryControlConfig.Types.AssertionRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53RecoveryControlConfig.Types.RuleConfig
import Amazonka.Route53RecoveryControlConfig.Types.Status

-- | An assertion rule enforces that, when you change a routing control
-- state, that the criteria that you set in the rule configuration is met.
-- Otherwise, the change to the routing control is not accepted. For
-- example, the criteria might be that at least one routing control state
-- is On after the transation so that traffic continues to flow to at least
-- one cell for the application. This ensures that you avoid a fail-open
-- scenario.
--
-- /See:/ 'newAssertionRule' smart constructor.
data AssertionRule = AssertionRule'
  { -- | The deployment status of an assertion rule. Status can be one of the
    -- following: PENDING, DEPLOYED, PENDING_DELETION.
    status :: Status,
    -- | The Amazon Resource Name (ARN) of the control panel.
    controlPanelArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the assertion rule.
    safetyRuleArn :: Prelude.Text,
    -- | The routing controls that are part of transactions that are evaluated to
    -- determine if a request to change a routing control state is allowed. For
    -- example, you might include three routing controls, one for each of three
    -- Amazon Web Services Regions.
    assertedControls :: [Prelude.Text],
    -- | The criteria that you set for specific assertion routing controls
    -- (AssertedControls) that designate how many routing control states must
    -- be ON as the result of a transaction. For example, if you have three
    -- assertion routing controls, you might specify atleast 2 for your rule
    -- configuration. This means that at least two assertion routing control
    -- states must be ON, so that at least two Amazon Web Services Regions have
    -- traffic flowing to them.
    ruleConfig :: RuleConfig,
    -- | An evaluation period, in milliseconds (ms), during which any request
    -- against the target routing controls will fail. This helps prevent
    -- \"flapping\" of state. The wait period is 5000 ms by default, but you
    -- can choose a custom value.
    waitPeriodMs :: Prelude.Int,
    -- | Name of the assertion rule. You can use any non-white space character in
    -- the name.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssertionRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'assertionRule_status' - The deployment status of an assertion rule. Status can be one of the
-- following: PENDING, DEPLOYED, PENDING_DELETION.
--
-- 'controlPanelArn', 'assertionRule_controlPanelArn' - The Amazon Resource Name (ARN) of the control panel.
--
-- 'safetyRuleArn', 'assertionRule_safetyRuleArn' - The Amazon Resource Name (ARN) of the assertion rule.
--
-- 'assertedControls', 'assertionRule_assertedControls' - The routing controls that are part of transactions that are evaluated to
-- determine if a request to change a routing control state is allowed. For
-- example, you might include three routing controls, one for each of three
-- Amazon Web Services Regions.
--
-- 'ruleConfig', 'assertionRule_ruleConfig' - The criteria that you set for specific assertion routing controls
-- (AssertedControls) that designate how many routing control states must
-- be ON as the result of a transaction. For example, if you have three
-- assertion routing controls, you might specify atleast 2 for your rule
-- configuration. This means that at least two assertion routing control
-- states must be ON, so that at least two Amazon Web Services Regions have
-- traffic flowing to them.
--
-- 'waitPeriodMs', 'assertionRule_waitPeriodMs' - An evaluation period, in milliseconds (ms), during which any request
-- against the target routing controls will fail. This helps prevent
-- \"flapping\" of state. The wait period is 5000 ms by default, but you
-- can choose a custom value.
--
-- 'name', 'assertionRule_name' - Name of the assertion rule. You can use any non-white space character in
-- the name.
newAssertionRule ::
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
  AssertionRule
newAssertionRule
  pStatus_
  pControlPanelArn_
  pSafetyRuleArn_
  pRuleConfig_
  pWaitPeriodMs_
  pName_ =
    AssertionRule'
      { status = pStatus_,
        controlPanelArn = pControlPanelArn_,
        safetyRuleArn = pSafetyRuleArn_,
        assertedControls = Prelude.mempty,
        ruleConfig = pRuleConfig_,
        waitPeriodMs = pWaitPeriodMs_,
        name = pName_
      }

-- | The deployment status of an assertion rule. Status can be one of the
-- following: PENDING, DEPLOYED, PENDING_DELETION.
assertionRule_status :: Lens.Lens' AssertionRule Status
assertionRule_status = Lens.lens (\AssertionRule' {status} -> status) (\s@AssertionRule' {} a -> s {status = a} :: AssertionRule)

-- | The Amazon Resource Name (ARN) of the control panel.
assertionRule_controlPanelArn :: Lens.Lens' AssertionRule Prelude.Text
assertionRule_controlPanelArn = Lens.lens (\AssertionRule' {controlPanelArn} -> controlPanelArn) (\s@AssertionRule' {} a -> s {controlPanelArn = a} :: AssertionRule)

-- | The Amazon Resource Name (ARN) of the assertion rule.
assertionRule_safetyRuleArn :: Lens.Lens' AssertionRule Prelude.Text
assertionRule_safetyRuleArn = Lens.lens (\AssertionRule' {safetyRuleArn} -> safetyRuleArn) (\s@AssertionRule' {} a -> s {safetyRuleArn = a} :: AssertionRule)

-- | The routing controls that are part of transactions that are evaluated to
-- determine if a request to change a routing control state is allowed. For
-- example, you might include three routing controls, one for each of three
-- Amazon Web Services Regions.
assertionRule_assertedControls :: Lens.Lens' AssertionRule [Prelude.Text]
assertionRule_assertedControls = Lens.lens (\AssertionRule' {assertedControls} -> assertedControls) (\s@AssertionRule' {} a -> s {assertedControls = a} :: AssertionRule) Prelude.. Lens.coerced

-- | The criteria that you set for specific assertion routing controls
-- (AssertedControls) that designate how many routing control states must
-- be ON as the result of a transaction. For example, if you have three
-- assertion routing controls, you might specify atleast 2 for your rule
-- configuration. This means that at least two assertion routing control
-- states must be ON, so that at least two Amazon Web Services Regions have
-- traffic flowing to them.
assertionRule_ruleConfig :: Lens.Lens' AssertionRule RuleConfig
assertionRule_ruleConfig = Lens.lens (\AssertionRule' {ruleConfig} -> ruleConfig) (\s@AssertionRule' {} a -> s {ruleConfig = a} :: AssertionRule)

-- | An evaluation period, in milliseconds (ms), during which any request
-- against the target routing controls will fail. This helps prevent
-- \"flapping\" of state. The wait period is 5000 ms by default, but you
-- can choose a custom value.
assertionRule_waitPeriodMs :: Lens.Lens' AssertionRule Prelude.Int
assertionRule_waitPeriodMs = Lens.lens (\AssertionRule' {waitPeriodMs} -> waitPeriodMs) (\s@AssertionRule' {} a -> s {waitPeriodMs = a} :: AssertionRule)

-- | Name of the assertion rule. You can use any non-white space character in
-- the name.
assertionRule_name :: Lens.Lens' AssertionRule Prelude.Text
assertionRule_name = Lens.lens (\AssertionRule' {name} -> name) (\s@AssertionRule' {} a -> s {name = a} :: AssertionRule)

instance Core.FromJSON AssertionRule where
  parseJSON =
    Core.withObject
      "AssertionRule"
      ( \x ->
          AssertionRule'
            Prelude.<$> (x Core..: "Status")
            Prelude.<*> (x Core..: "ControlPanelArn")
            Prelude.<*> (x Core..: "SafetyRuleArn")
            Prelude.<*> ( x Core..:? "AssertedControls"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "RuleConfig")
            Prelude.<*> (x Core..: "WaitPeriodMs")
            Prelude.<*> (x Core..: "Name")
      )

instance Prelude.Hashable AssertionRule where
  hashWithSalt _salt AssertionRule' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` controlPanelArn
      `Prelude.hashWithSalt` safetyRuleArn
      `Prelude.hashWithSalt` assertedControls
      `Prelude.hashWithSalt` ruleConfig
      `Prelude.hashWithSalt` waitPeriodMs
      `Prelude.hashWithSalt` name

instance Prelude.NFData AssertionRule where
  rnf AssertionRule' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf controlPanelArn
      `Prelude.seq` Prelude.rnf safetyRuleArn
      `Prelude.seq` Prelude.rnf assertedControls
      `Prelude.seq` Prelude.rnf ruleConfig
      `Prelude.seq` Prelude.rnf waitPeriodMs
      `Prelude.seq` Prelude.rnf name

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
-- Module      : Network.AWS.Route53RecoveryControlConfig.Types.NewAssertionRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53RecoveryControlConfig.Types.NewAssertionRule where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53RecoveryControlConfig.Types.RuleConfig

-- | A new assertion rule for a control panel.
--
-- /See:/ 'newNewAssertionRule' smart constructor.
data NewAssertionRule = NewAssertionRule'
  { -- | The Amazon Resource Name (ARN) for the control panel.
    controlPanelArn :: Prelude.Text,
    -- | The routing controls that are part of transactions that are evaluated to
    -- determine if a request to change a routing control state is allowed. For
    -- example, you might include three routing controls, one for each of three
    -- Amazon Web Services Regions.
    assertedControls :: [Prelude.Text],
    -- | The criteria that you set for specific assertion controls (routing
    -- controls) that designate how many controls must be enabled as the result
    -- of a transaction. For example, if you have three assertion controls, you
    -- might specify atleast 2 for your rule configuration. This means that at
    -- least two assertion controls must be enabled, so that at least two
    -- Amazon Web Services Regions are enabled.
    ruleConfig :: RuleConfig,
    -- | An evaluation period, in milliseconds (ms), during which any request
    -- against the target routing controls will fail. This helps prevent
    -- \"flapping\" of state. The wait period is 5000 ms by default, but you
    -- can choose a custom value.
    waitPeriodMs :: Prelude.Int,
    -- | The name of the assertion rule. You can use any non-white space
    -- character in the name.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NewAssertionRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'controlPanelArn', 'newAssertionRule_controlPanelArn' - The Amazon Resource Name (ARN) for the control panel.
--
-- 'assertedControls', 'newAssertionRule_assertedControls' - The routing controls that are part of transactions that are evaluated to
-- determine if a request to change a routing control state is allowed. For
-- example, you might include three routing controls, one for each of three
-- Amazon Web Services Regions.
--
-- 'ruleConfig', 'newAssertionRule_ruleConfig' - The criteria that you set for specific assertion controls (routing
-- controls) that designate how many controls must be enabled as the result
-- of a transaction. For example, if you have three assertion controls, you
-- might specify atleast 2 for your rule configuration. This means that at
-- least two assertion controls must be enabled, so that at least two
-- Amazon Web Services Regions are enabled.
--
-- 'waitPeriodMs', 'newAssertionRule_waitPeriodMs' - An evaluation period, in milliseconds (ms), during which any request
-- against the target routing controls will fail. This helps prevent
-- \"flapping\" of state. The wait period is 5000 ms by default, but you
-- can choose a custom value.
--
-- 'name', 'newAssertionRule_name' - The name of the assertion rule. You can use any non-white space
-- character in the name.
newNewAssertionRule ::
  -- | 'controlPanelArn'
  Prelude.Text ->
  -- | 'ruleConfig'
  RuleConfig ->
  -- | 'waitPeriodMs'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  NewAssertionRule
newNewAssertionRule
  pControlPanelArn_
  pRuleConfig_
  pWaitPeriodMs_
  pName_ =
    NewAssertionRule'
      { controlPanelArn =
          pControlPanelArn_,
        assertedControls = Prelude.mempty,
        ruleConfig = pRuleConfig_,
        waitPeriodMs = pWaitPeriodMs_,
        name = pName_
      }

-- | The Amazon Resource Name (ARN) for the control panel.
newAssertionRule_controlPanelArn :: Lens.Lens' NewAssertionRule Prelude.Text
newAssertionRule_controlPanelArn = Lens.lens (\NewAssertionRule' {controlPanelArn} -> controlPanelArn) (\s@NewAssertionRule' {} a -> s {controlPanelArn = a} :: NewAssertionRule)

-- | The routing controls that are part of transactions that are evaluated to
-- determine if a request to change a routing control state is allowed. For
-- example, you might include three routing controls, one for each of three
-- Amazon Web Services Regions.
newAssertionRule_assertedControls :: Lens.Lens' NewAssertionRule [Prelude.Text]
newAssertionRule_assertedControls = Lens.lens (\NewAssertionRule' {assertedControls} -> assertedControls) (\s@NewAssertionRule' {} a -> s {assertedControls = a} :: NewAssertionRule) Prelude.. Lens.coerced

-- | The criteria that you set for specific assertion controls (routing
-- controls) that designate how many controls must be enabled as the result
-- of a transaction. For example, if you have three assertion controls, you
-- might specify atleast 2 for your rule configuration. This means that at
-- least two assertion controls must be enabled, so that at least two
-- Amazon Web Services Regions are enabled.
newAssertionRule_ruleConfig :: Lens.Lens' NewAssertionRule RuleConfig
newAssertionRule_ruleConfig = Lens.lens (\NewAssertionRule' {ruleConfig} -> ruleConfig) (\s@NewAssertionRule' {} a -> s {ruleConfig = a} :: NewAssertionRule)

-- | An evaluation period, in milliseconds (ms), during which any request
-- against the target routing controls will fail. This helps prevent
-- \"flapping\" of state. The wait period is 5000 ms by default, but you
-- can choose a custom value.
newAssertionRule_waitPeriodMs :: Lens.Lens' NewAssertionRule Prelude.Int
newAssertionRule_waitPeriodMs = Lens.lens (\NewAssertionRule' {waitPeriodMs} -> waitPeriodMs) (\s@NewAssertionRule' {} a -> s {waitPeriodMs = a} :: NewAssertionRule)

-- | The name of the assertion rule. You can use any non-white space
-- character in the name.
newAssertionRule_name :: Lens.Lens' NewAssertionRule Prelude.Text
newAssertionRule_name = Lens.lens (\NewAssertionRule' {name} -> name) (\s@NewAssertionRule' {} a -> s {name = a} :: NewAssertionRule)

instance Prelude.Hashable NewAssertionRule

instance Prelude.NFData NewAssertionRule

instance Core.ToJSON NewAssertionRule where
  toJSON NewAssertionRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ControlPanelArn" Core..= controlPanelArn),
            Prelude.Just
              ("AssertedControls" Core..= assertedControls),
            Prelude.Just ("RuleConfig" Core..= ruleConfig),
            Prelude.Just ("WaitPeriodMs" Core..= waitPeriodMs),
            Prelude.Just ("Name" Core..= name)
          ]
      )

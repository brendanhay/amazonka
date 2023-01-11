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
-- Module      : Amazonka.Route53RecoveryControlConfig.Types.AssertionRuleUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryControlConfig.Types.AssertionRuleUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An update to an assertion rule. You can update the name or the
-- evaluation period (wait period). If you don\'t specify one of the items
-- to update, the item is unchanged.
--
-- /See:/ 'newAssertionRuleUpdate' smart constructor.
data AssertionRuleUpdate = AssertionRuleUpdate'
  { -- | The Amazon Resource Name (ARN) of the assertion rule.
    safetyRuleArn :: Prelude.Text,
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
-- Create a value of 'AssertionRuleUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'safetyRuleArn', 'assertionRuleUpdate_safetyRuleArn' - The Amazon Resource Name (ARN) of the assertion rule.
--
-- 'waitPeriodMs', 'assertionRuleUpdate_waitPeriodMs' - An evaluation period, in milliseconds (ms), during which any request
-- against the target routing controls will fail. This helps prevent
-- \"flapping\" of state. The wait period is 5000 ms by default, but you
-- can choose a custom value.
--
-- 'name', 'assertionRuleUpdate_name' - The name of the assertion rule. You can use any non-white space
-- character in the name.
newAssertionRuleUpdate ::
  -- | 'safetyRuleArn'
  Prelude.Text ->
  -- | 'waitPeriodMs'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  AssertionRuleUpdate
newAssertionRuleUpdate
  pSafetyRuleArn_
  pWaitPeriodMs_
  pName_ =
    AssertionRuleUpdate'
      { safetyRuleArn =
          pSafetyRuleArn_,
        waitPeriodMs = pWaitPeriodMs_,
        name = pName_
      }

-- | The Amazon Resource Name (ARN) of the assertion rule.
assertionRuleUpdate_safetyRuleArn :: Lens.Lens' AssertionRuleUpdate Prelude.Text
assertionRuleUpdate_safetyRuleArn = Lens.lens (\AssertionRuleUpdate' {safetyRuleArn} -> safetyRuleArn) (\s@AssertionRuleUpdate' {} a -> s {safetyRuleArn = a} :: AssertionRuleUpdate)

-- | An evaluation period, in milliseconds (ms), during which any request
-- against the target routing controls will fail. This helps prevent
-- \"flapping\" of state. The wait period is 5000 ms by default, but you
-- can choose a custom value.
assertionRuleUpdate_waitPeriodMs :: Lens.Lens' AssertionRuleUpdate Prelude.Int
assertionRuleUpdate_waitPeriodMs = Lens.lens (\AssertionRuleUpdate' {waitPeriodMs} -> waitPeriodMs) (\s@AssertionRuleUpdate' {} a -> s {waitPeriodMs = a} :: AssertionRuleUpdate)

-- | The name of the assertion rule. You can use any non-white space
-- character in the name.
assertionRuleUpdate_name :: Lens.Lens' AssertionRuleUpdate Prelude.Text
assertionRuleUpdate_name = Lens.lens (\AssertionRuleUpdate' {name} -> name) (\s@AssertionRuleUpdate' {} a -> s {name = a} :: AssertionRuleUpdate)

instance Prelude.Hashable AssertionRuleUpdate where
  hashWithSalt _salt AssertionRuleUpdate' {..} =
    _salt `Prelude.hashWithSalt` safetyRuleArn
      `Prelude.hashWithSalt` waitPeriodMs
      `Prelude.hashWithSalt` name

instance Prelude.NFData AssertionRuleUpdate where
  rnf AssertionRuleUpdate' {..} =
    Prelude.rnf safetyRuleArn
      `Prelude.seq` Prelude.rnf waitPeriodMs
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON AssertionRuleUpdate where
  toJSON AssertionRuleUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("SafetyRuleArn" Data..= safetyRuleArn),
            Prelude.Just ("WaitPeriodMs" Data..= waitPeriodMs),
            Prelude.Just ("Name" Data..= name)
          ]
      )

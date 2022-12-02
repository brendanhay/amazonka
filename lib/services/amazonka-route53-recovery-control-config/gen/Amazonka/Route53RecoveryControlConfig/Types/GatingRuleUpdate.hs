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
-- Module      : Amazonka.Route53RecoveryControlConfig.Types.GatingRuleUpdate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryControlConfig.Types.GatingRuleUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Update to a gating rule. You can update the name or the evaluation
-- period (wait period). If you don\'t specify one of the items to update,
-- the item is unchanged.
--
-- /See:/ 'newGatingRuleUpdate' smart constructor.
data GatingRuleUpdate = GatingRuleUpdate'
  { -- | The Amazon Resource Name (ARN) of the gating rule.
    safetyRuleArn :: Prelude.Text,
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
-- Create a value of 'GatingRuleUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'safetyRuleArn', 'gatingRuleUpdate_safetyRuleArn' - The Amazon Resource Name (ARN) of the gating rule.
--
-- 'waitPeriodMs', 'gatingRuleUpdate_waitPeriodMs' - An evaluation period, in milliseconds (ms), during which any request
-- against the target routing controls will fail. This helps prevent
-- \"flapping\" of state. The wait period is 5000 ms by default, but you
-- can choose a custom value.
--
-- 'name', 'gatingRuleUpdate_name' - The name for the gating rule. You can use any non-white space character
-- in the name.
newGatingRuleUpdate ::
  -- | 'safetyRuleArn'
  Prelude.Text ->
  -- | 'waitPeriodMs'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  GatingRuleUpdate
newGatingRuleUpdate
  pSafetyRuleArn_
  pWaitPeriodMs_
  pName_ =
    GatingRuleUpdate'
      { safetyRuleArn = pSafetyRuleArn_,
        waitPeriodMs = pWaitPeriodMs_,
        name = pName_
      }

-- | The Amazon Resource Name (ARN) of the gating rule.
gatingRuleUpdate_safetyRuleArn :: Lens.Lens' GatingRuleUpdate Prelude.Text
gatingRuleUpdate_safetyRuleArn = Lens.lens (\GatingRuleUpdate' {safetyRuleArn} -> safetyRuleArn) (\s@GatingRuleUpdate' {} a -> s {safetyRuleArn = a} :: GatingRuleUpdate)

-- | An evaluation period, in milliseconds (ms), during which any request
-- against the target routing controls will fail. This helps prevent
-- \"flapping\" of state. The wait period is 5000 ms by default, but you
-- can choose a custom value.
gatingRuleUpdate_waitPeriodMs :: Lens.Lens' GatingRuleUpdate Prelude.Int
gatingRuleUpdate_waitPeriodMs = Lens.lens (\GatingRuleUpdate' {waitPeriodMs} -> waitPeriodMs) (\s@GatingRuleUpdate' {} a -> s {waitPeriodMs = a} :: GatingRuleUpdate)

-- | The name for the gating rule. You can use any non-white space character
-- in the name.
gatingRuleUpdate_name :: Lens.Lens' GatingRuleUpdate Prelude.Text
gatingRuleUpdate_name = Lens.lens (\GatingRuleUpdate' {name} -> name) (\s@GatingRuleUpdate' {} a -> s {name = a} :: GatingRuleUpdate)

instance Prelude.Hashable GatingRuleUpdate where
  hashWithSalt _salt GatingRuleUpdate' {..} =
    _salt `Prelude.hashWithSalt` safetyRuleArn
      `Prelude.hashWithSalt` waitPeriodMs
      `Prelude.hashWithSalt` name

instance Prelude.NFData GatingRuleUpdate where
  rnf GatingRuleUpdate' {..} =
    Prelude.rnf safetyRuleArn
      `Prelude.seq` Prelude.rnf waitPeriodMs
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON GatingRuleUpdate where
  toJSON GatingRuleUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("SafetyRuleArn" Data..= safetyRuleArn),
            Prelude.Just ("WaitPeriodMs" Data..= waitPeriodMs),
            Prelude.Just ("Name" Data..= name)
          ]
      )

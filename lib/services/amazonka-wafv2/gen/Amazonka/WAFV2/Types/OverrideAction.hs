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
-- Module      : Amazonka.WAFV2.Types.OverrideAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.OverrideAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.CountAction
import Amazonka.WAFV2.Types.NoneAction

-- | The action to use in the place of the action that results from the rule
-- group evaluation. Set the override action to none to leave the result of
-- the rule group alone. Set it to count to override the result to count
-- only.
--
-- You can only use this for rule statements that reference a rule group,
-- like @RuleGroupReferenceStatement@ and @ManagedRuleGroupStatement@.
--
-- This option is usually set to none. It does not affect how the rules in
-- the rule group are evaluated. If you want the rules in the rule group to
-- only count matches, do not use this and instead use the rule action
-- override option, with @Count@ action, in your rule group reference
-- statement settings.
--
-- /See:/ 'newOverrideAction' smart constructor.
data OverrideAction = OverrideAction'
  { -- | Override the rule group evaluation result to count only.
    --
    -- This option is usually set to none. It does not affect how the rules in
    -- the rule group are evaluated. If you want the rules in the rule group to
    -- only count matches, do not use this and instead use the rule action
    -- override option, with @Count@ action, in your rule group reference
    -- statement settings.
    count :: Prelude.Maybe CountAction,
    -- | Don\'t override the rule group evaluation result. This is the most
    -- common setting.
    none :: Prelude.Maybe NoneAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OverrideAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'count', 'overrideAction_count' - Override the rule group evaluation result to count only.
--
-- This option is usually set to none. It does not affect how the rules in
-- the rule group are evaluated. If you want the rules in the rule group to
-- only count matches, do not use this and instead use the rule action
-- override option, with @Count@ action, in your rule group reference
-- statement settings.
--
-- 'none', 'overrideAction_none' - Don\'t override the rule group evaluation result. This is the most
-- common setting.
newOverrideAction ::
  OverrideAction
newOverrideAction =
  OverrideAction'
    { count = Prelude.Nothing,
      none = Prelude.Nothing
    }

-- | Override the rule group evaluation result to count only.
--
-- This option is usually set to none. It does not affect how the rules in
-- the rule group are evaluated. If you want the rules in the rule group to
-- only count matches, do not use this and instead use the rule action
-- override option, with @Count@ action, in your rule group reference
-- statement settings.
overrideAction_count :: Lens.Lens' OverrideAction (Prelude.Maybe CountAction)
overrideAction_count = Lens.lens (\OverrideAction' {count} -> count) (\s@OverrideAction' {} a -> s {count = a} :: OverrideAction)

-- | Don\'t override the rule group evaluation result. This is the most
-- common setting.
overrideAction_none :: Lens.Lens' OverrideAction (Prelude.Maybe NoneAction)
overrideAction_none = Lens.lens (\OverrideAction' {none} -> none) (\s@OverrideAction' {} a -> s {none = a} :: OverrideAction)

instance Data.FromJSON OverrideAction where
  parseJSON =
    Data.withObject
      "OverrideAction"
      ( \x ->
          OverrideAction'
            Prelude.<$> (x Data..:? "Count") Prelude.<*> (x Data..:? "None")
      )

instance Prelude.Hashable OverrideAction where
  hashWithSalt _salt OverrideAction' {..} =
    _salt `Prelude.hashWithSalt` count
      `Prelude.hashWithSalt` none

instance Prelude.NFData OverrideAction where
  rnf OverrideAction' {..} =
    Prelude.rnf count `Prelude.seq` Prelude.rnf none

instance Data.ToJSON OverrideAction where
  toJSON OverrideAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Count" Data..=) Prelude.<$> count,
            ("None" Data..=) Prelude.<$> none
          ]
      )

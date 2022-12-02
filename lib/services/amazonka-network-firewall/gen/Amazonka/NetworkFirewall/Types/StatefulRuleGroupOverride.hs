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
-- Module      : Amazonka.NetworkFirewall.Types.StatefulRuleGroupOverride
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.StatefulRuleGroupOverride where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types.OverrideAction
import qualified Amazonka.Prelude as Prelude

-- | The setting that allows the policy owner to change the behavior of the
-- rule group within a policy.
--
-- /See:/ 'newStatefulRuleGroupOverride' smart constructor.
data StatefulRuleGroupOverride = StatefulRuleGroupOverride'
  { -- | The action that changes the rule group from @DROP@ to @ALERT@. This only
    -- applies to managed rule groups.
    action :: Prelude.Maybe OverrideAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StatefulRuleGroupOverride' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'statefulRuleGroupOverride_action' - The action that changes the rule group from @DROP@ to @ALERT@. This only
-- applies to managed rule groups.
newStatefulRuleGroupOverride ::
  StatefulRuleGroupOverride
newStatefulRuleGroupOverride =
  StatefulRuleGroupOverride'
    { action =
        Prelude.Nothing
    }

-- | The action that changes the rule group from @DROP@ to @ALERT@. This only
-- applies to managed rule groups.
statefulRuleGroupOverride_action :: Lens.Lens' StatefulRuleGroupOverride (Prelude.Maybe OverrideAction)
statefulRuleGroupOverride_action = Lens.lens (\StatefulRuleGroupOverride' {action} -> action) (\s@StatefulRuleGroupOverride' {} a -> s {action = a} :: StatefulRuleGroupOverride)

instance Data.FromJSON StatefulRuleGroupOverride where
  parseJSON =
    Data.withObject
      "StatefulRuleGroupOverride"
      ( \x ->
          StatefulRuleGroupOverride'
            Prelude.<$> (x Data..:? "Action")
      )

instance Prelude.Hashable StatefulRuleGroupOverride where
  hashWithSalt _salt StatefulRuleGroupOverride' {..} =
    _salt `Prelude.hashWithSalt` action

instance Prelude.NFData StatefulRuleGroupOverride where
  rnf StatefulRuleGroupOverride' {..} =
    Prelude.rnf action

instance Data.ToJSON StatefulRuleGroupOverride where
  toJSON StatefulRuleGroupOverride' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Action" Data..=) Prelude.<$> action]
      )

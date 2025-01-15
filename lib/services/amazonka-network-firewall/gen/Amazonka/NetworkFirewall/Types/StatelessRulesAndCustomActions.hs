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
-- Module      : Amazonka.NetworkFirewall.Types.StatelessRulesAndCustomActions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.StatelessRulesAndCustomActions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types.CustomAction
import Amazonka.NetworkFirewall.Types.StatelessRule
import qualified Amazonka.Prelude as Prelude

-- | Stateless inspection criteria. Each stateless rule group uses exactly
-- one of these data types to define its stateless rules.
--
-- /See:/ 'newStatelessRulesAndCustomActions' smart constructor.
data StatelessRulesAndCustomActions = StatelessRulesAndCustomActions'
  { -- | Defines an array of individual custom action definitions that are
    -- available for use by the stateless rules in this
    -- @StatelessRulesAndCustomActions@ specification. You name each custom
    -- action that you define, and then you can use it by name in your
    -- StatelessRule RuleDefinition @Actions@ specification.
    customActions :: Prelude.Maybe [CustomAction],
    -- | Defines the set of stateless rules for use in a stateless rule group.
    statelessRules :: [StatelessRule]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StatelessRulesAndCustomActions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customActions', 'statelessRulesAndCustomActions_customActions' - Defines an array of individual custom action definitions that are
-- available for use by the stateless rules in this
-- @StatelessRulesAndCustomActions@ specification. You name each custom
-- action that you define, and then you can use it by name in your
-- StatelessRule RuleDefinition @Actions@ specification.
--
-- 'statelessRules', 'statelessRulesAndCustomActions_statelessRules' - Defines the set of stateless rules for use in a stateless rule group.
newStatelessRulesAndCustomActions ::
  StatelessRulesAndCustomActions
newStatelessRulesAndCustomActions =
  StatelessRulesAndCustomActions'
    { customActions =
        Prelude.Nothing,
      statelessRules = Prelude.mempty
    }

-- | Defines an array of individual custom action definitions that are
-- available for use by the stateless rules in this
-- @StatelessRulesAndCustomActions@ specification. You name each custom
-- action that you define, and then you can use it by name in your
-- StatelessRule RuleDefinition @Actions@ specification.
statelessRulesAndCustomActions_customActions :: Lens.Lens' StatelessRulesAndCustomActions (Prelude.Maybe [CustomAction])
statelessRulesAndCustomActions_customActions = Lens.lens (\StatelessRulesAndCustomActions' {customActions} -> customActions) (\s@StatelessRulesAndCustomActions' {} a -> s {customActions = a} :: StatelessRulesAndCustomActions) Prelude.. Lens.mapping Lens.coerced

-- | Defines the set of stateless rules for use in a stateless rule group.
statelessRulesAndCustomActions_statelessRules :: Lens.Lens' StatelessRulesAndCustomActions [StatelessRule]
statelessRulesAndCustomActions_statelessRules = Lens.lens (\StatelessRulesAndCustomActions' {statelessRules} -> statelessRules) (\s@StatelessRulesAndCustomActions' {} a -> s {statelessRules = a} :: StatelessRulesAndCustomActions) Prelude.. Lens.coerced

instance Data.FromJSON StatelessRulesAndCustomActions where
  parseJSON =
    Data.withObject
      "StatelessRulesAndCustomActions"
      ( \x ->
          StatelessRulesAndCustomActions'
            Prelude.<$> (x Data..:? "CustomActions" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "StatelessRules"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    StatelessRulesAndCustomActions
  where
  hashWithSalt
    _salt
    StatelessRulesAndCustomActions' {..} =
      _salt
        `Prelude.hashWithSalt` customActions
        `Prelude.hashWithSalt` statelessRules

instance
  Prelude.NFData
    StatelessRulesAndCustomActions
  where
  rnf StatelessRulesAndCustomActions' {..} =
    Prelude.rnf customActions `Prelude.seq`
      Prelude.rnf statelessRules

instance Data.ToJSON StatelessRulesAndCustomActions where
  toJSON StatelessRulesAndCustomActions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomActions" Data..=) Prelude.<$> customActions,
            Prelude.Just
              ("StatelessRules" Data..= statelessRules)
          ]
      )

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
-- Module      : Amazonka.SecurityHub.Types.RuleGroupSourceStatefulRulesOptionsDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.RuleGroupSourceStatefulRulesOptionsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A rule option for a stateful rule.
--
-- /See:/ 'newRuleGroupSourceStatefulRulesOptionsDetails' smart constructor.
data RuleGroupSourceStatefulRulesOptionsDetails = RuleGroupSourceStatefulRulesOptionsDetails'
  { -- | A list of settings.
    settings :: Prelude.Maybe [Prelude.Text],
    -- | A keyword to look for.
    keyword :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleGroupSourceStatefulRulesOptionsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'settings', 'ruleGroupSourceStatefulRulesOptionsDetails_settings' - A list of settings.
--
-- 'keyword', 'ruleGroupSourceStatefulRulesOptionsDetails_keyword' - A keyword to look for.
newRuleGroupSourceStatefulRulesOptionsDetails ::
  RuleGroupSourceStatefulRulesOptionsDetails
newRuleGroupSourceStatefulRulesOptionsDetails =
  RuleGroupSourceStatefulRulesOptionsDetails'
    { settings =
        Prelude.Nothing,
      keyword = Prelude.Nothing
    }

-- | A list of settings.
ruleGroupSourceStatefulRulesOptionsDetails_settings :: Lens.Lens' RuleGroupSourceStatefulRulesOptionsDetails (Prelude.Maybe [Prelude.Text])
ruleGroupSourceStatefulRulesOptionsDetails_settings = Lens.lens (\RuleGroupSourceStatefulRulesOptionsDetails' {settings} -> settings) (\s@RuleGroupSourceStatefulRulesOptionsDetails' {} a -> s {settings = a} :: RuleGroupSourceStatefulRulesOptionsDetails) Prelude.. Lens.mapping Lens.coerced

-- | A keyword to look for.
ruleGroupSourceStatefulRulesOptionsDetails_keyword :: Lens.Lens' RuleGroupSourceStatefulRulesOptionsDetails (Prelude.Maybe Prelude.Text)
ruleGroupSourceStatefulRulesOptionsDetails_keyword = Lens.lens (\RuleGroupSourceStatefulRulesOptionsDetails' {keyword} -> keyword) (\s@RuleGroupSourceStatefulRulesOptionsDetails' {} a -> s {keyword = a} :: RuleGroupSourceStatefulRulesOptionsDetails)

instance
  Data.FromJSON
    RuleGroupSourceStatefulRulesOptionsDetails
  where
  parseJSON =
    Data.withObject
      "RuleGroupSourceStatefulRulesOptionsDetails"
      ( \x ->
          RuleGroupSourceStatefulRulesOptionsDetails'
            Prelude.<$> (x Data..:? "Settings" Data..!= Prelude.mempty)
              Prelude.<*> (x Data..:? "Keyword")
      )

instance
  Prelude.Hashable
    RuleGroupSourceStatefulRulesOptionsDetails
  where
  hashWithSalt
    _salt
    RuleGroupSourceStatefulRulesOptionsDetails' {..} =
      _salt `Prelude.hashWithSalt` settings
        `Prelude.hashWithSalt` keyword

instance
  Prelude.NFData
    RuleGroupSourceStatefulRulesOptionsDetails
  where
  rnf RuleGroupSourceStatefulRulesOptionsDetails' {..} =
    Prelude.rnf settings
      `Prelude.seq` Prelude.rnf keyword

instance
  Data.ToJSON
    RuleGroupSourceStatefulRulesOptionsDetails
  where
  toJSON
    RuleGroupSourceStatefulRulesOptionsDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Settings" Data..=) Prelude.<$> settings,
              ("Keyword" Data..=) Prelude.<$> keyword
            ]
        )

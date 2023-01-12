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
-- Module      : Amazonka.NetworkFirewall.Types.RuleOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.RuleOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Additional settings for a stateful rule. This is part of the
-- StatefulRule configuration.
--
-- /See:/ 'newRuleOption' smart constructor.
data RuleOption = RuleOption'
  { settings :: Prelude.Maybe [Prelude.Text],
    keyword :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'settings', 'ruleOption_settings' -
--
-- 'keyword', 'ruleOption_keyword' -
newRuleOption ::
  -- | 'keyword'
  Prelude.Text ->
  RuleOption
newRuleOption pKeyword_ =
  RuleOption'
    { settings = Prelude.Nothing,
      keyword = pKeyword_
    }

-- |
ruleOption_settings :: Lens.Lens' RuleOption (Prelude.Maybe [Prelude.Text])
ruleOption_settings = Lens.lens (\RuleOption' {settings} -> settings) (\s@RuleOption' {} a -> s {settings = a} :: RuleOption) Prelude.. Lens.mapping Lens.coerced

-- |
ruleOption_keyword :: Lens.Lens' RuleOption Prelude.Text
ruleOption_keyword = Lens.lens (\RuleOption' {keyword} -> keyword) (\s@RuleOption' {} a -> s {keyword = a} :: RuleOption)

instance Data.FromJSON RuleOption where
  parseJSON =
    Data.withObject
      "RuleOption"
      ( \x ->
          RuleOption'
            Prelude.<$> (x Data..:? "Settings" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Keyword")
      )

instance Prelude.Hashable RuleOption where
  hashWithSalt _salt RuleOption' {..} =
    _salt `Prelude.hashWithSalt` settings
      `Prelude.hashWithSalt` keyword

instance Prelude.NFData RuleOption where
  rnf RuleOption' {..} =
    Prelude.rnf settings
      `Prelude.seq` Prelude.rnf keyword

instance Data.ToJSON RuleOption where
  toJSON RuleOption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Settings" Data..=) Prelude.<$> settings,
            Prelude.Just ("Keyword" Data..= keyword)
          ]
      )

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
-- Module      : Amazonka.EC2.Types.RuleOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.RuleOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes additional settings for a stateful rule.
--
-- /See:/ 'newRuleOption' smart constructor.
data RuleOption = RuleOption'
  { -- | The Suricata keyword.
    keyword :: Prelude.Maybe Prelude.Text,
    -- | The settings for the keyword.
    settings :: Prelude.Maybe [Prelude.Text]
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
-- 'keyword', 'ruleOption_keyword' - The Suricata keyword.
--
-- 'settings', 'ruleOption_settings' - The settings for the keyword.
newRuleOption ::
  RuleOption
newRuleOption =
  RuleOption'
    { keyword = Prelude.Nothing,
      settings = Prelude.Nothing
    }

-- | The Suricata keyword.
ruleOption_keyword :: Lens.Lens' RuleOption (Prelude.Maybe Prelude.Text)
ruleOption_keyword = Lens.lens (\RuleOption' {keyword} -> keyword) (\s@RuleOption' {} a -> s {keyword = a} :: RuleOption)

-- | The settings for the keyword.
ruleOption_settings :: Lens.Lens' RuleOption (Prelude.Maybe [Prelude.Text])
ruleOption_settings = Lens.lens (\RuleOption' {settings} -> settings) (\s@RuleOption' {} a -> s {settings = a} :: RuleOption) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML RuleOption where
  parseXML x =
    RuleOption'
      Prelude.<$> (x Data..@? "keyword")
      Prelude.<*> ( x
                      Data..@? "settingSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )

instance Prelude.Hashable RuleOption where
  hashWithSalt _salt RuleOption' {..} =
    _salt
      `Prelude.hashWithSalt` keyword
      `Prelude.hashWithSalt` settings

instance Prelude.NFData RuleOption where
  rnf RuleOption' {..} =
    Prelude.rnf keyword
      `Prelude.seq` Prelude.rnf settings

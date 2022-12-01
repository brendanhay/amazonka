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
-- Module      : Amazonka.SecurityHub.Types.WafExcludedRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.WafExcludedRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details about a rule to exclude from a rule group.
--
-- /See:/ 'newWafExcludedRule' smart constructor.
data WafExcludedRule = WafExcludedRule'
  { -- | The unique identifier for the rule to exclude from the rule group.
    ruleId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WafExcludedRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleId', 'wafExcludedRule_ruleId' - The unique identifier for the rule to exclude from the rule group.
newWafExcludedRule ::
  WafExcludedRule
newWafExcludedRule =
  WafExcludedRule' {ruleId = Prelude.Nothing}

-- | The unique identifier for the rule to exclude from the rule group.
wafExcludedRule_ruleId :: Lens.Lens' WafExcludedRule (Prelude.Maybe Prelude.Text)
wafExcludedRule_ruleId = Lens.lens (\WafExcludedRule' {ruleId} -> ruleId) (\s@WafExcludedRule' {} a -> s {ruleId = a} :: WafExcludedRule)

instance Core.FromJSON WafExcludedRule where
  parseJSON =
    Core.withObject
      "WafExcludedRule"
      ( \x ->
          WafExcludedRule' Prelude.<$> (x Core..:? "RuleId")
      )

instance Prelude.Hashable WafExcludedRule where
  hashWithSalt _salt WafExcludedRule' {..} =
    _salt `Prelude.hashWithSalt` ruleId

instance Prelude.NFData WafExcludedRule where
  rnf WafExcludedRule' {..} = Prelude.rnf ruleId

instance Core.ToJSON WafExcludedRule where
  toJSON WafExcludedRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [("RuleId" Core..=) Prelude.<$> ruleId]
      )

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
-- Module      : Amazonka.WorkMail.Types.ImpersonationMatchedRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMail.Types.ImpersonationMatchedRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The impersonation rule that matched the input.
--
-- /See:/ 'newImpersonationMatchedRule' smart constructor.
data ImpersonationMatchedRule = ImpersonationMatchedRule'
  { -- | The name of the rule that matched the input.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the rule that matched the input
    impersonationRuleId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImpersonationMatchedRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'impersonationMatchedRule_name' - The name of the rule that matched the input.
--
-- 'impersonationRuleId', 'impersonationMatchedRule_impersonationRuleId' - The ID of the rule that matched the input
newImpersonationMatchedRule ::
  ImpersonationMatchedRule
newImpersonationMatchedRule =
  ImpersonationMatchedRule'
    { name = Prelude.Nothing,
      impersonationRuleId = Prelude.Nothing
    }

-- | The name of the rule that matched the input.
impersonationMatchedRule_name :: Lens.Lens' ImpersonationMatchedRule (Prelude.Maybe Prelude.Text)
impersonationMatchedRule_name = Lens.lens (\ImpersonationMatchedRule' {name} -> name) (\s@ImpersonationMatchedRule' {} a -> s {name = a} :: ImpersonationMatchedRule)

-- | The ID of the rule that matched the input
impersonationMatchedRule_impersonationRuleId :: Lens.Lens' ImpersonationMatchedRule (Prelude.Maybe Prelude.Text)
impersonationMatchedRule_impersonationRuleId = Lens.lens (\ImpersonationMatchedRule' {impersonationRuleId} -> impersonationRuleId) (\s@ImpersonationMatchedRule' {} a -> s {impersonationRuleId = a} :: ImpersonationMatchedRule)

instance Core.FromJSON ImpersonationMatchedRule where
  parseJSON =
    Core.withObject
      "ImpersonationMatchedRule"
      ( \x ->
          ImpersonationMatchedRule'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "ImpersonationRuleId")
      )

instance Prelude.Hashable ImpersonationMatchedRule where
  hashWithSalt _salt ImpersonationMatchedRule' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` impersonationRuleId

instance Prelude.NFData ImpersonationMatchedRule where
  rnf ImpersonationMatchedRule' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf impersonationRuleId

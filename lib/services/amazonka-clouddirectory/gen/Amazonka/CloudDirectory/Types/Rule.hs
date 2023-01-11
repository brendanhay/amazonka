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
-- Module      : Amazonka.CloudDirectory.Types.Rule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.Rule where

import Amazonka.CloudDirectory.Types.RuleType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains an Amazon Resource Name (ARN) and parameters that are
-- associated with the rule.
--
-- /See:/ 'newRule' smart constructor.
data Rule = Rule'
  { -- | The minimum and maximum parameters that are associated with the rule.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The type of attribute validation rule.
    type' :: Prelude.Maybe RuleType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Rule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameters', 'rule_parameters' - The minimum and maximum parameters that are associated with the rule.
--
-- 'type'', 'rule_type' - The type of attribute validation rule.
newRule ::
  Rule
newRule =
  Rule'
    { parameters = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The minimum and maximum parameters that are associated with the rule.
rule_parameters :: Lens.Lens' Rule (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
rule_parameters = Lens.lens (\Rule' {parameters} -> parameters) (\s@Rule' {} a -> s {parameters = a} :: Rule) Prelude.. Lens.mapping Lens.coerced

-- | The type of attribute validation rule.
rule_type :: Lens.Lens' Rule (Prelude.Maybe RuleType)
rule_type = Lens.lens (\Rule' {type'} -> type') (\s@Rule' {} a -> s {type' = a} :: Rule)

instance Data.FromJSON Rule where
  parseJSON =
    Data.withObject
      "Rule"
      ( \x ->
          Rule'
            Prelude.<$> (x Data..:? "Parameters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable Rule where
  hashWithSalt _salt Rule' {..} =
    _salt `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Rule where
  rnf Rule' {..} =
    Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON Rule where
  toJSON Rule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Parameters" Data..=) Prelude.<$> parameters,
            ("Type" Data..=) Prelude.<$> type'
          ]
      )

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
-- Module      : Network.AWS.CloudDirectory.Types.Rule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.Rule where

import Network.AWS.CloudDirectory.Types.RuleType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains an Amazon Resource Name (ARN) and parameters that are
-- associated with the rule.
--
-- /See:/ 'newRule' smart constructor.
data Rule = Rule'
  { -- | The type of attribute validation rule.
    type' :: Prelude.Maybe RuleType,
    -- | The minimum and maximum parameters that are associated with the rule.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
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
-- 'type'', 'rule_type' - The type of attribute validation rule.
--
-- 'parameters', 'rule_parameters' - The minimum and maximum parameters that are associated with the rule.
newRule ::
  Rule
newRule =
  Rule'
    { type' = Prelude.Nothing,
      parameters = Prelude.Nothing
    }

-- | The type of attribute validation rule.
rule_type :: Lens.Lens' Rule (Prelude.Maybe RuleType)
rule_type = Lens.lens (\Rule' {type'} -> type') (\s@Rule' {} a -> s {type' = a} :: Rule)

-- | The minimum and maximum parameters that are associated with the rule.
rule_parameters :: Lens.Lens' Rule (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
rule_parameters = Lens.lens (\Rule' {parameters} -> parameters) (\s@Rule' {} a -> s {parameters = a} :: Rule) Prelude.. Lens.mapping Lens._Coerce

instance Core.FromJSON Rule where
  parseJSON =
    Core.withObject
      "Rule"
      ( \x ->
          Rule'
            Prelude.<$> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Parameters" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Rule

instance Prelude.NFData Rule

instance Core.ToJSON Rule where
  toJSON Rule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Type" Core..=) Prelude.<$> type',
            ("Parameters" Core..=) Prelude.<$> parameters
          ]
      )

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
-- Module      : Amazonka.NetworkFirewall.Types.RuleGroupMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.RuleGroupMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | High-level information about a rule group, returned by ListRuleGroups.
-- You can use the information provided in the metadata to retrieve and
-- manage a rule group.
--
-- /See:/ 'newRuleGroupMetadata' smart constructor.
data RuleGroupMetadata = RuleGroupMetadata'
  { -- | The descriptive name of the rule group. You can\'t change the name of a
    -- rule group after you create it.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the rule group.
    arn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleGroupMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'ruleGroupMetadata_name' - The descriptive name of the rule group. You can\'t change the name of a
-- rule group after you create it.
--
-- 'arn', 'ruleGroupMetadata_arn' - The Amazon Resource Name (ARN) of the rule group.
newRuleGroupMetadata ::
  RuleGroupMetadata
newRuleGroupMetadata =
  RuleGroupMetadata'
    { name = Prelude.Nothing,
      arn = Prelude.Nothing
    }

-- | The descriptive name of the rule group. You can\'t change the name of a
-- rule group after you create it.
ruleGroupMetadata_name :: Lens.Lens' RuleGroupMetadata (Prelude.Maybe Prelude.Text)
ruleGroupMetadata_name = Lens.lens (\RuleGroupMetadata' {name} -> name) (\s@RuleGroupMetadata' {} a -> s {name = a} :: RuleGroupMetadata)

-- | The Amazon Resource Name (ARN) of the rule group.
ruleGroupMetadata_arn :: Lens.Lens' RuleGroupMetadata (Prelude.Maybe Prelude.Text)
ruleGroupMetadata_arn = Lens.lens (\RuleGroupMetadata' {arn} -> arn) (\s@RuleGroupMetadata' {} a -> s {arn = a} :: RuleGroupMetadata)

instance Core.FromJSON RuleGroupMetadata where
  parseJSON =
    Core.withObject
      "RuleGroupMetadata"
      ( \x ->
          RuleGroupMetadata'
            Prelude.<$> (x Core..:? "Name") Prelude.<*> (x Core..:? "Arn")
      )

instance Prelude.Hashable RuleGroupMetadata where
  hashWithSalt _salt RuleGroupMetadata' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn

instance Prelude.NFData RuleGroupMetadata where
  rnf RuleGroupMetadata' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf arn

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
-- Module      : Amazonka.CodeGuruReviewer.Types.RuleMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruReviewer.Types.RuleMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Metadata about a rule. Rule metadata includes an ID, a name, a list of
-- tags, and a short and long description. CodeGuru Reviewer uses rules to
-- analyze code. A rule\'s recommendation is included in analysis results
-- if code is detected that violates the rule.
--
-- /See:/ 'newRuleMetadata' smart constructor.
data RuleMetadata = RuleMetadata'
  { -- | A long description of the rule.
    longDescription :: Prelude.Maybe Prelude.Text,
    -- | The ID of the rule.
    ruleId :: Prelude.Maybe Prelude.Text,
    -- | The name of the rule.
    ruleName :: Prelude.Maybe Prelude.Text,
    -- | Tags that are associated with the rule.
    ruleTags :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A short description of the rule.
    shortDescription :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'longDescription', 'ruleMetadata_longDescription' - A long description of the rule.
--
-- 'ruleId', 'ruleMetadata_ruleId' - The ID of the rule.
--
-- 'ruleName', 'ruleMetadata_ruleName' - The name of the rule.
--
-- 'ruleTags', 'ruleMetadata_ruleTags' - Tags that are associated with the rule.
--
-- 'shortDescription', 'ruleMetadata_shortDescription' - A short description of the rule.
newRuleMetadata ::
  RuleMetadata
newRuleMetadata =
  RuleMetadata'
    { longDescription = Prelude.Nothing,
      ruleId = Prelude.Nothing,
      ruleName = Prelude.Nothing,
      ruleTags = Prelude.Nothing,
      shortDescription = Prelude.Nothing
    }

-- | A long description of the rule.
ruleMetadata_longDescription :: Lens.Lens' RuleMetadata (Prelude.Maybe Prelude.Text)
ruleMetadata_longDescription = Lens.lens (\RuleMetadata' {longDescription} -> longDescription) (\s@RuleMetadata' {} a -> s {longDescription = a} :: RuleMetadata)

-- | The ID of the rule.
ruleMetadata_ruleId :: Lens.Lens' RuleMetadata (Prelude.Maybe Prelude.Text)
ruleMetadata_ruleId = Lens.lens (\RuleMetadata' {ruleId} -> ruleId) (\s@RuleMetadata' {} a -> s {ruleId = a} :: RuleMetadata)

-- | The name of the rule.
ruleMetadata_ruleName :: Lens.Lens' RuleMetadata (Prelude.Maybe Prelude.Text)
ruleMetadata_ruleName = Lens.lens (\RuleMetadata' {ruleName} -> ruleName) (\s@RuleMetadata' {} a -> s {ruleName = a} :: RuleMetadata)

-- | Tags that are associated with the rule.
ruleMetadata_ruleTags :: Lens.Lens' RuleMetadata (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
ruleMetadata_ruleTags = Lens.lens (\RuleMetadata' {ruleTags} -> ruleTags) (\s@RuleMetadata' {} a -> s {ruleTags = a} :: RuleMetadata) Prelude.. Lens.mapping Lens.coerced

-- | A short description of the rule.
ruleMetadata_shortDescription :: Lens.Lens' RuleMetadata (Prelude.Maybe Prelude.Text)
ruleMetadata_shortDescription = Lens.lens (\RuleMetadata' {shortDescription} -> shortDescription) (\s@RuleMetadata' {} a -> s {shortDescription = a} :: RuleMetadata)

instance Data.FromJSON RuleMetadata where
  parseJSON =
    Data.withObject
      "RuleMetadata"
      ( \x ->
          RuleMetadata'
            Prelude.<$> (x Data..:? "LongDescription")
            Prelude.<*> (x Data..:? "RuleId")
            Prelude.<*> (x Data..:? "RuleName")
            Prelude.<*> (x Data..:? "RuleTags")
            Prelude.<*> (x Data..:? "ShortDescription")
      )

instance Prelude.Hashable RuleMetadata where
  hashWithSalt _salt RuleMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` longDescription
      `Prelude.hashWithSalt` ruleId
      `Prelude.hashWithSalt` ruleName
      `Prelude.hashWithSalt` ruleTags
      `Prelude.hashWithSalt` shortDescription

instance Prelude.NFData RuleMetadata where
  rnf RuleMetadata' {..} =
    Prelude.rnf longDescription
      `Prelude.seq` Prelude.rnf ruleId
      `Prelude.seq` Prelude.rnf ruleName
      `Prelude.seq` Prelude.rnf ruleTags
      `Prelude.seq` Prelude.rnf shortDescription

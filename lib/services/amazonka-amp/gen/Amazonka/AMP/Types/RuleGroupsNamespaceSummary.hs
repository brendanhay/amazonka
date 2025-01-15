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
-- Module      : Amazonka.AMP.Types.RuleGroupsNamespaceSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AMP.Types.RuleGroupsNamespaceSummary where

import Amazonka.AMP.Types.RuleGroupsNamespaceStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a summary of the rule groups namespace.
--
-- /See:/ 'newRuleGroupsNamespaceSummary' smart constructor.
data RuleGroupsNamespaceSummary = RuleGroupsNamespaceSummary'
  { -- | The tags of this rule groups namespace.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Name (ARN) of this rule groups namespace.
    arn :: Prelude.Text,
    -- | The time when the rule groups namespace was created.
    createdAt :: Data.POSIX,
    -- | The time when the rule groups namespace was modified.
    modifiedAt :: Data.POSIX,
    -- | The rule groups namespace name.
    name :: Prelude.Text,
    -- | The status of rule groups namespace.
    status :: RuleGroupsNamespaceStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleGroupsNamespaceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'ruleGroupsNamespaceSummary_tags' - The tags of this rule groups namespace.
--
-- 'arn', 'ruleGroupsNamespaceSummary_arn' - The Amazon Resource Name (ARN) of this rule groups namespace.
--
-- 'createdAt', 'ruleGroupsNamespaceSummary_createdAt' - The time when the rule groups namespace was created.
--
-- 'modifiedAt', 'ruleGroupsNamespaceSummary_modifiedAt' - The time when the rule groups namespace was modified.
--
-- 'name', 'ruleGroupsNamespaceSummary_name' - The rule groups namespace name.
--
-- 'status', 'ruleGroupsNamespaceSummary_status' - The status of rule groups namespace.
newRuleGroupsNamespaceSummary ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'modifiedAt'
  Prelude.UTCTime ->
  -- | 'name'
  Prelude.Text ->
  -- | 'status'
  RuleGroupsNamespaceStatus ->
  RuleGroupsNamespaceSummary
newRuleGroupsNamespaceSummary
  pArn_
  pCreatedAt_
  pModifiedAt_
  pName_
  pStatus_ =
    RuleGroupsNamespaceSummary'
      { tags = Prelude.Nothing,
        arn = pArn_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        modifiedAt = Data._Time Lens.# pModifiedAt_,
        name = pName_,
        status = pStatus_
      }

-- | The tags of this rule groups namespace.
ruleGroupsNamespaceSummary_tags :: Lens.Lens' RuleGroupsNamespaceSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
ruleGroupsNamespaceSummary_tags = Lens.lens (\RuleGroupsNamespaceSummary' {tags} -> tags) (\s@RuleGroupsNamespaceSummary' {} a -> s {tags = a} :: RuleGroupsNamespaceSummary) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of this rule groups namespace.
ruleGroupsNamespaceSummary_arn :: Lens.Lens' RuleGroupsNamespaceSummary Prelude.Text
ruleGroupsNamespaceSummary_arn = Lens.lens (\RuleGroupsNamespaceSummary' {arn} -> arn) (\s@RuleGroupsNamespaceSummary' {} a -> s {arn = a} :: RuleGroupsNamespaceSummary)

-- | The time when the rule groups namespace was created.
ruleGroupsNamespaceSummary_createdAt :: Lens.Lens' RuleGroupsNamespaceSummary Prelude.UTCTime
ruleGroupsNamespaceSummary_createdAt = Lens.lens (\RuleGroupsNamespaceSummary' {createdAt} -> createdAt) (\s@RuleGroupsNamespaceSummary' {} a -> s {createdAt = a} :: RuleGroupsNamespaceSummary) Prelude.. Data._Time

-- | The time when the rule groups namespace was modified.
ruleGroupsNamespaceSummary_modifiedAt :: Lens.Lens' RuleGroupsNamespaceSummary Prelude.UTCTime
ruleGroupsNamespaceSummary_modifiedAt = Lens.lens (\RuleGroupsNamespaceSummary' {modifiedAt} -> modifiedAt) (\s@RuleGroupsNamespaceSummary' {} a -> s {modifiedAt = a} :: RuleGroupsNamespaceSummary) Prelude.. Data._Time

-- | The rule groups namespace name.
ruleGroupsNamespaceSummary_name :: Lens.Lens' RuleGroupsNamespaceSummary Prelude.Text
ruleGroupsNamespaceSummary_name = Lens.lens (\RuleGroupsNamespaceSummary' {name} -> name) (\s@RuleGroupsNamespaceSummary' {} a -> s {name = a} :: RuleGroupsNamespaceSummary)

-- | The status of rule groups namespace.
ruleGroupsNamespaceSummary_status :: Lens.Lens' RuleGroupsNamespaceSummary RuleGroupsNamespaceStatus
ruleGroupsNamespaceSummary_status = Lens.lens (\RuleGroupsNamespaceSummary' {status} -> status) (\s@RuleGroupsNamespaceSummary' {} a -> s {status = a} :: RuleGroupsNamespaceSummary)

instance Data.FromJSON RuleGroupsNamespaceSummary where
  parseJSON =
    Data.withObject
      "RuleGroupsNamespaceSummary"
      ( \x ->
          RuleGroupsNamespaceSummary'
            Prelude.<$> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "modifiedAt")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "status")
      )

instance Prelude.Hashable RuleGroupsNamespaceSummary where
  hashWithSalt _salt RuleGroupsNamespaceSummary' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` modifiedAt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData RuleGroupsNamespaceSummary where
  rnf RuleGroupsNamespaceSummary' {..} =
    Prelude.rnf tags `Prelude.seq`
      Prelude.rnf arn `Prelude.seq`
        Prelude.rnf createdAt `Prelude.seq`
          Prelude.rnf modifiedAt `Prelude.seq`
            Prelude.rnf name `Prelude.seq`
              Prelude.rnf status

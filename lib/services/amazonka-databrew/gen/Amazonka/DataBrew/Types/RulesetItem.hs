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
-- Module      : Amazonka.DataBrew.Types.RulesetItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.RulesetItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains metadata about the ruleset.
--
-- /See:/ 'newRulesetItem' smart constructor.
data RulesetItem = RulesetItem'
  { -- | Metadata tags that have been applied to the ruleset.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The modification date and time of the ruleset.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | The description of the ruleset.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the ruleset.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the ruleset was created.
    createDate :: Prelude.Maybe Data.POSIX,
    -- | The number of rules that are defined in the ruleset.
    ruleCount :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the user who last modified the
    -- ruleset.
    lastModifiedBy :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the ruleset.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the user who created the ruleset.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The name of the ruleset.
    name :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of a resource (dataset) that the ruleset
    -- is associated with.
    targetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RulesetItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'rulesetItem_tags' - Metadata tags that have been applied to the ruleset.
--
-- 'lastModifiedDate', 'rulesetItem_lastModifiedDate' - The modification date and time of the ruleset.
--
-- 'description', 'rulesetItem_description' - The description of the ruleset.
--
-- 'accountId', 'rulesetItem_accountId' - The ID of the Amazon Web Services account that owns the ruleset.
--
-- 'createDate', 'rulesetItem_createDate' - The date and time that the ruleset was created.
--
-- 'ruleCount', 'rulesetItem_ruleCount' - The number of rules that are defined in the ruleset.
--
-- 'lastModifiedBy', 'rulesetItem_lastModifiedBy' - The Amazon Resource Name (ARN) of the user who last modified the
-- ruleset.
--
-- 'resourceArn', 'rulesetItem_resourceArn' - The Amazon Resource Name (ARN) for the ruleset.
--
-- 'createdBy', 'rulesetItem_createdBy' - The Amazon Resource Name (ARN) of the user who created the ruleset.
--
-- 'name', 'rulesetItem_name' - The name of the ruleset.
--
-- 'targetArn', 'rulesetItem_targetArn' - The Amazon Resource Name (ARN) of a resource (dataset) that the ruleset
-- is associated with.
newRulesetItem ::
  -- | 'name'
  Prelude.Text ->
  -- | 'targetArn'
  Prelude.Text ->
  RulesetItem
newRulesetItem pName_ pTargetArn_ =
  RulesetItem'
    { tags = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      description = Prelude.Nothing,
      accountId = Prelude.Nothing,
      createDate = Prelude.Nothing,
      ruleCount = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      name = pName_,
      targetArn = pTargetArn_
    }

-- | Metadata tags that have been applied to the ruleset.
rulesetItem_tags :: Lens.Lens' RulesetItem (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
rulesetItem_tags = Lens.lens (\RulesetItem' {tags} -> tags) (\s@RulesetItem' {} a -> s {tags = a} :: RulesetItem) Prelude.. Lens.mapping Lens.coerced

-- | The modification date and time of the ruleset.
rulesetItem_lastModifiedDate :: Lens.Lens' RulesetItem (Prelude.Maybe Prelude.UTCTime)
rulesetItem_lastModifiedDate = Lens.lens (\RulesetItem' {lastModifiedDate} -> lastModifiedDate) (\s@RulesetItem' {} a -> s {lastModifiedDate = a} :: RulesetItem) Prelude.. Lens.mapping Data._Time

-- | The description of the ruleset.
rulesetItem_description :: Lens.Lens' RulesetItem (Prelude.Maybe Prelude.Text)
rulesetItem_description = Lens.lens (\RulesetItem' {description} -> description) (\s@RulesetItem' {} a -> s {description = a} :: RulesetItem)

-- | The ID of the Amazon Web Services account that owns the ruleset.
rulesetItem_accountId :: Lens.Lens' RulesetItem (Prelude.Maybe Prelude.Text)
rulesetItem_accountId = Lens.lens (\RulesetItem' {accountId} -> accountId) (\s@RulesetItem' {} a -> s {accountId = a} :: RulesetItem)

-- | The date and time that the ruleset was created.
rulesetItem_createDate :: Lens.Lens' RulesetItem (Prelude.Maybe Prelude.UTCTime)
rulesetItem_createDate = Lens.lens (\RulesetItem' {createDate} -> createDate) (\s@RulesetItem' {} a -> s {createDate = a} :: RulesetItem) Prelude.. Lens.mapping Data._Time

-- | The number of rules that are defined in the ruleset.
rulesetItem_ruleCount :: Lens.Lens' RulesetItem (Prelude.Maybe Prelude.Natural)
rulesetItem_ruleCount = Lens.lens (\RulesetItem' {ruleCount} -> ruleCount) (\s@RulesetItem' {} a -> s {ruleCount = a} :: RulesetItem)

-- | The Amazon Resource Name (ARN) of the user who last modified the
-- ruleset.
rulesetItem_lastModifiedBy :: Lens.Lens' RulesetItem (Prelude.Maybe Prelude.Text)
rulesetItem_lastModifiedBy = Lens.lens (\RulesetItem' {lastModifiedBy} -> lastModifiedBy) (\s@RulesetItem' {} a -> s {lastModifiedBy = a} :: RulesetItem)

-- | The Amazon Resource Name (ARN) for the ruleset.
rulesetItem_resourceArn :: Lens.Lens' RulesetItem (Prelude.Maybe Prelude.Text)
rulesetItem_resourceArn = Lens.lens (\RulesetItem' {resourceArn} -> resourceArn) (\s@RulesetItem' {} a -> s {resourceArn = a} :: RulesetItem)

-- | The Amazon Resource Name (ARN) of the user who created the ruleset.
rulesetItem_createdBy :: Lens.Lens' RulesetItem (Prelude.Maybe Prelude.Text)
rulesetItem_createdBy = Lens.lens (\RulesetItem' {createdBy} -> createdBy) (\s@RulesetItem' {} a -> s {createdBy = a} :: RulesetItem)

-- | The name of the ruleset.
rulesetItem_name :: Lens.Lens' RulesetItem Prelude.Text
rulesetItem_name = Lens.lens (\RulesetItem' {name} -> name) (\s@RulesetItem' {} a -> s {name = a} :: RulesetItem)

-- | The Amazon Resource Name (ARN) of a resource (dataset) that the ruleset
-- is associated with.
rulesetItem_targetArn :: Lens.Lens' RulesetItem Prelude.Text
rulesetItem_targetArn = Lens.lens (\RulesetItem' {targetArn} -> targetArn) (\s@RulesetItem' {} a -> s {targetArn = a} :: RulesetItem)

instance Data.FromJSON RulesetItem where
  parseJSON =
    Data.withObject
      "RulesetItem"
      ( \x ->
          RulesetItem'
            Prelude.<$> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "LastModifiedDate")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "AccountId")
            Prelude.<*> (x Data..:? "CreateDate")
            Prelude.<*> (x Data..:? "RuleCount")
            Prelude.<*> (x Data..:? "LastModifiedBy")
            Prelude.<*> (x Data..:? "ResourceArn")
            Prelude.<*> (x Data..:? "CreatedBy")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "TargetArn")
      )

instance Prelude.Hashable RulesetItem where
  hashWithSalt _salt RulesetItem' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` createDate
      `Prelude.hashWithSalt` ruleCount
      `Prelude.hashWithSalt` lastModifiedBy
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` targetArn

instance Prelude.NFData RulesetItem where
  rnf RulesetItem' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf ruleCount
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf targetArn

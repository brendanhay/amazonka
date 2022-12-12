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
-- Module      : Amazonka.WAFV2.Types.ManagedRuleSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.ManagedRuleSet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.ManagedRuleSetVersion

-- | A set of rules that is managed by Amazon Web Services and Amazon Web
-- Services Marketplace sellers to provide versioned managed rule groups
-- for customers of WAF.
--
-- This is intended for use only by vendors of managed rule sets. Vendors
-- are Amazon Web Services and Amazon Web Services Marketplace sellers.
--
-- Vendors, you can use the managed rule set APIs to provide controlled
-- rollout of your versioned managed rule group offerings for your
-- customers. The APIs are @ListManagedRuleSets@, @GetManagedRuleSet@,
-- @PutManagedRuleSetVersions@, and
-- @UpdateManagedRuleSetVersionExpiryDate@.
--
-- /See:/ 'newManagedRuleSet' smart constructor.
data ManagedRuleSet = ManagedRuleSet'
  { -- | A description of the set that helps with identification.
    description :: Prelude.Maybe Prelude.Text,
    -- | The label namespace prefix for the managed rule groups that are offered
    -- to customers from this managed rule set. All labels that are added by
    -- rules in the managed rule group have this prefix.
    --
    -- -   The syntax for the label namespace prefix for a managed rule group
    --     is the following:
    --
    --     @awswaf:managed:\<vendor>:\<rule group name>@:
    --
    -- -   When a rule with a label matches a web request, WAF adds the fully
    --     qualified label to the request. A fully qualified label is made up
    --     of the label namespace from the rule group or web ACL where the rule
    --     is defined and the label from the rule, separated by a colon:
    --
    --     @\<label namespace>:\<label from rule>@
    labelNamespace :: Prelude.Maybe Prelude.Text,
    -- | The versions of this managed rule set that are available for use by
    -- customers.
    publishedVersions :: Prelude.Maybe (Prelude.HashMap Prelude.Text ManagedRuleSetVersion),
    -- | The version that you would like your customers to use.
    recommendedVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the managed rule set. You use this, along with the rule set
    -- ID, to identify the rule set.
    --
    -- This name is assigned to the corresponding managed rule group, which
    -- your customers can access and use.
    name :: Prelude.Text,
    -- | A unique identifier for the managed rule set. The ID is returned in the
    -- responses to commands like @list@. You provide it to operations like
    -- @get@ and @update@.
    id :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the entity.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ManagedRuleSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'managedRuleSet_description' - A description of the set that helps with identification.
--
-- 'labelNamespace', 'managedRuleSet_labelNamespace' - The label namespace prefix for the managed rule groups that are offered
-- to customers from this managed rule set. All labels that are added by
-- rules in the managed rule group have this prefix.
--
-- -   The syntax for the label namespace prefix for a managed rule group
--     is the following:
--
--     @awswaf:managed:\<vendor>:\<rule group name>@:
--
-- -   When a rule with a label matches a web request, WAF adds the fully
--     qualified label to the request. A fully qualified label is made up
--     of the label namespace from the rule group or web ACL where the rule
--     is defined and the label from the rule, separated by a colon:
--
--     @\<label namespace>:\<label from rule>@
--
-- 'publishedVersions', 'managedRuleSet_publishedVersions' - The versions of this managed rule set that are available for use by
-- customers.
--
-- 'recommendedVersion', 'managedRuleSet_recommendedVersion' - The version that you would like your customers to use.
--
-- 'name', 'managedRuleSet_name' - The name of the managed rule set. You use this, along with the rule set
-- ID, to identify the rule set.
--
-- This name is assigned to the corresponding managed rule group, which
-- your customers can access and use.
--
-- 'id', 'managedRuleSet_id' - A unique identifier for the managed rule set. The ID is returned in the
-- responses to commands like @list@. You provide it to operations like
-- @get@ and @update@.
--
-- 'arn', 'managedRuleSet_arn' - The Amazon Resource Name (ARN) of the entity.
newManagedRuleSet ::
  -- | 'name'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  ManagedRuleSet
newManagedRuleSet pName_ pId_ pARN_ =
  ManagedRuleSet'
    { description = Prelude.Nothing,
      labelNamespace = Prelude.Nothing,
      publishedVersions = Prelude.Nothing,
      recommendedVersion = Prelude.Nothing,
      name = pName_,
      id = pId_,
      arn = pARN_
    }

-- | A description of the set that helps with identification.
managedRuleSet_description :: Lens.Lens' ManagedRuleSet (Prelude.Maybe Prelude.Text)
managedRuleSet_description = Lens.lens (\ManagedRuleSet' {description} -> description) (\s@ManagedRuleSet' {} a -> s {description = a} :: ManagedRuleSet)

-- | The label namespace prefix for the managed rule groups that are offered
-- to customers from this managed rule set. All labels that are added by
-- rules in the managed rule group have this prefix.
--
-- -   The syntax for the label namespace prefix for a managed rule group
--     is the following:
--
--     @awswaf:managed:\<vendor>:\<rule group name>@:
--
-- -   When a rule with a label matches a web request, WAF adds the fully
--     qualified label to the request. A fully qualified label is made up
--     of the label namespace from the rule group or web ACL where the rule
--     is defined and the label from the rule, separated by a colon:
--
--     @\<label namespace>:\<label from rule>@
managedRuleSet_labelNamespace :: Lens.Lens' ManagedRuleSet (Prelude.Maybe Prelude.Text)
managedRuleSet_labelNamespace = Lens.lens (\ManagedRuleSet' {labelNamespace} -> labelNamespace) (\s@ManagedRuleSet' {} a -> s {labelNamespace = a} :: ManagedRuleSet)

-- | The versions of this managed rule set that are available for use by
-- customers.
managedRuleSet_publishedVersions :: Lens.Lens' ManagedRuleSet (Prelude.Maybe (Prelude.HashMap Prelude.Text ManagedRuleSetVersion))
managedRuleSet_publishedVersions = Lens.lens (\ManagedRuleSet' {publishedVersions} -> publishedVersions) (\s@ManagedRuleSet' {} a -> s {publishedVersions = a} :: ManagedRuleSet) Prelude.. Lens.mapping Lens.coerced

-- | The version that you would like your customers to use.
managedRuleSet_recommendedVersion :: Lens.Lens' ManagedRuleSet (Prelude.Maybe Prelude.Text)
managedRuleSet_recommendedVersion = Lens.lens (\ManagedRuleSet' {recommendedVersion} -> recommendedVersion) (\s@ManagedRuleSet' {} a -> s {recommendedVersion = a} :: ManagedRuleSet)

-- | The name of the managed rule set. You use this, along with the rule set
-- ID, to identify the rule set.
--
-- This name is assigned to the corresponding managed rule group, which
-- your customers can access and use.
managedRuleSet_name :: Lens.Lens' ManagedRuleSet Prelude.Text
managedRuleSet_name = Lens.lens (\ManagedRuleSet' {name} -> name) (\s@ManagedRuleSet' {} a -> s {name = a} :: ManagedRuleSet)

-- | A unique identifier for the managed rule set. The ID is returned in the
-- responses to commands like @list@. You provide it to operations like
-- @get@ and @update@.
managedRuleSet_id :: Lens.Lens' ManagedRuleSet Prelude.Text
managedRuleSet_id = Lens.lens (\ManagedRuleSet' {id} -> id) (\s@ManagedRuleSet' {} a -> s {id = a} :: ManagedRuleSet)

-- | The Amazon Resource Name (ARN) of the entity.
managedRuleSet_arn :: Lens.Lens' ManagedRuleSet Prelude.Text
managedRuleSet_arn = Lens.lens (\ManagedRuleSet' {arn} -> arn) (\s@ManagedRuleSet' {} a -> s {arn = a} :: ManagedRuleSet)

instance Data.FromJSON ManagedRuleSet where
  parseJSON =
    Data.withObject
      "ManagedRuleSet"
      ( \x ->
          ManagedRuleSet'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "LabelNamespace")
            Prelude.<*> ( x Data..:? "PublishedVersions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "RecommendedVersion")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Id")
            Prelude.<*> (x Data..: "ARN")
      )

instance Prelude.Hashable ManagedRuleSet where
  hashWithSalt _salt ManagedRuleSet' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` labelNamespace
      `Prelude.hashWithSalt` publishedVersions
      `Prelude.hashWithSalt` recommendedVersion
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` arn

instance Prelude.NFData ManagedRuleSet where
  rnf ManagedRuleSet' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf labelNamespace
      `Prelude.seq` Prelude.rnf publishedVersions
      `Prelude.seq` Prelude.rnf recommendedVersion
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf arn

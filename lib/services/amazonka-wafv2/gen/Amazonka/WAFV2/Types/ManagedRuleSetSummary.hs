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
-- Module      : Amazonka.WAFV2.Types.ManagedRuleSetSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.ManagedRuleSetSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | High-level information for a managed rule set.
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
-- /See:/ 'newManagedRuleSetSummary' smart constructor.
data ManagedRuleSetSummary = ManagedRuleSetSummary'
  { -- | The Amazon Resource Name (ARN) of the entity.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A description of the set that helps with identification.
    description :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the managed rule set. The ID is returned in the
    -- responses to commands like @list@. You provide it to operations like
    -- @get@ and @update@.
    id :: Prelude.Maybe Prelude.Text,
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
    -- | A token used for optimistic locking. WAF returns a token to your @get@
    -- and @list@ requests, to mark the state of the entity at the time of the
    -- request. To make changes to the entity associated with the token, you
    -- provide the token to operations like @update@ and @delete@. WAF uses the
    -- token to ensure that no changes have been made to the entity since you
    -- last retrieved it. If a change has been made, the update fails with a
    -- @WAFOptimisticLockException@. If this happens, perform another @get@,
    -- and use the new token returned by that operation.
    lockToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the managed rule set. You use this, along with the rule set
    -- ID, to identify the rule set.
    --
    -- This name is assigned to the corresponding managed rule group, which
    -- your customers can access and use.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ManagedRuleSetSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'managedRuleSetSummary_arn' - The Amazon Resource Name (ARN) of the entity.
--
-- 'description', 'managedRuleSetSummary_description' - A description of the set that helps with identification.
--
-- 'id', 'managedRuleSetSummary_id' - A unique identifier for the managed rule set. The ID is returned in the
-- responses to commands like @list@. You provide it to operations like
-- @get@ and @update@.
--
-- 'labelNamespace', 'managedRuleSetSummary_labelNamespace' - The label namespace prefix for the managed rule groups that are offered
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
-- 'lockToken', 'managedRuleSetSummary_lockToken' - A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
--
-- 'name', 'managedRuleSetSummary_name' - The name of the managed rule set. You use this, along with the rule set
-- ID, to identify the rule set.
--
-- This name is assigned to the corresponding managed rule group, which
-- your customers can access and use.
newManagedRuleSetSummary ::
  ManagedRuleSetSummary
newManagedRuleSetSummary =
  ManagedRuleSetSummary'
    { arn = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      labelNamespace = Prelude.Nothing,
      lockToken = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the entity.
managedRuleSetSummary_arn :: Lens.Lens' ManagedRuleSetSummary (Prelude.Maybe Prelude.Text)
managedRuleSetSummary_arn = Lens.lens (\ManagedRuleSetSummary' {arn} -> arn) (\s@ManagedRuleSetSummary' {} a -> s {arn = a} :: ManagedRuleSetSummary)

-- | A description of the set that helps with identification.
managedRuleSetSummary_description :: Lens.Lens' ManagedRuleSetSummary (Prelude.Maybe Prelude.Text)
managedRuleSetSummary_description = Lens.lens (\ManagedRuleSetSummary' {description} -> description) (\s@ManagedRuleSetSummary' {} a -> s {description = a} :: ManagedRuleSetSummary)

-- | A unique identifier for the managed rule set. The ID is returned in the
-- responses to commands like @list@. You provide it to operations like
-- @get@ and @update@.
managedRuleSetSummary_id :: Lens.Lens' ManagedRuleSetSummary (Prelude.Maybe Prelude.Text)
managedRuleSetSummary_id = Lens.lens (\ManagedRuleSetSummary' {id} -> id) (\s@ManagedRuleSetSummary' {} a -> s {id = a} :: ManagedRuleSetSummary)

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
managedRuleSetSummary_labelNamespace :: Lens.Lens' ManagedRuleSetSummary (Prelude.Maybe Prelude.Text)
managedRuleSetSummary_labelNamespace = Lens.lens (\ManagedRuleSetSummary' {labelNamespace} -> labelNamespace) (\s@ManagedRuleSetSummary' {} a -> s {labelNamespace = a} :: ManagedRuleSetSummary)

-- | A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
managedRuleSetSummary_lockToken :: Lens.Lens' ManagedRuleSetSummary (Prelude.Maybe Prelude.Text)
managedRuleSetSummary_lockToken = Lens.lens (\ManagedRuleSetSummary' {lockToken} -> lockToken) (\s@ManagedRuleSetSummary' {} a -> s {lockToken = a} :: ManagedRuleSetSummary)

-- | The name of the managed rule set. You use this, along with the rule set
-- ID, to identify the rule set.
--
-- This name is assigned to the corresponding managed rule group, which
-- your customers can access and use.
managedRuleSetSummary_name :: Lens.Lens' ManagedRuleSetSummary (Prelude.Maybe Prelude.Text)
managedRuleSetSummary_name = Lens.lens (\ManagedRuleSetSummary' {name} -> name) (\s@ManagedRuleSetSummary' {} a -> s {name = a} :: ManagedRuleSetSummary)

instance Data.FromJSON ManagedRuleSetSummary where
  parseJSON =
    Data.withObject
      "ManagedRuleSetSummary"
      ( \x ->
          ManagedRuleSetSummary'
            Prelude.<$> (x Data..:? "ARN")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "LabelNamespace")
            Prelude.<*> (x Data..:? "LockToken")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable ManagedRuleSetSummary where
  hashWithSalt _salt ManagedRuleSetSummary' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` labelNamespace
      `Prelude.hashWithSalt` lockToken
      `Prelude.hashWithSalt` name

instance Prelude.NFData ManagedRuleSetSummary where
  rnf ManagedRuleSetSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf labelNamespace
      `Prelude.seq` Prelude.rnf lockToken
      `Prelude.seq` Prelude.rnf name

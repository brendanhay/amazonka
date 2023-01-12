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
-- Module      : Amazonka.WAFV2.Types.RuleGroupSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.RuleGroupSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | High-level information about a RuleGroup, returned by operations like
-- create and list. This provides information like the ID, that you can use
-- to retrieve and manage a @RuleGroup@, and the ARN, that you provide to
-- the RuleGroupReferenceStatement to use the rule group in a Rule.
--
-- /See:/ 'newRuleGroupSummary' smart constructor.
data RuleGroupSummary = RuleGroupSummary'
  { -- | The Amazon Resource Name (ARN) of the entity.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A description of the rule group that helps with identification.
    description :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the rule group. This ID is returned in the
    -- responses to create and list commands. You provide it to operations like
    -- update and delete.
    id :: Prelude.Maybe Prelude.Text,
    -- | A token used for optimistic locking. WAF returns a token to your @get@
    -- and @list@ requests, to mark the state of the entity at the time of the
    -- request. To make changes to the entity associated with the token, you
    -- provide the token to operations like @update@ and @delete@. WAF uses the
    -- token to ensure that no changes have been made to the entity since you
    -- last retrieved it. If a change has been made, the update fails with a
    -- @WAFOptimisticLockException@. If this happens, perform another @get@,
    -- and use the new token returned by that operation.
    lockToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the data type instance. You cannot change the name after you
    -- create the instance.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleGroupSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'ruleGroupSummary_arn' - The Amazon Resource Name (ARN) of the entity.
--
-- 'description', 'ruleGroupSummary_description' - A description of the rule group that helps with identification.
--
-- 'id', 'ruleGroupSummary_id' - A unique identifier for the rule group. This ID is returned in the
-- responses to create and list commands. You provide it to operations like
-- update and delete.
--
-- 'lockToken', 'ruleGroupSummary_lockToken' - A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
--
-- 'name', 'ruleGroupSummary_name' - The name of the data type instance. You cannot change the name after you
-- create the instance.
newRuleGroupSummary ::
  RuleGroupSummary
newRuleGroupSummary =
  RuleGroupSummary'
    { arn = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      lockToken = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the entity.
ruleGroupSummary_arn :: Lens.Lens' RuleGroupSummary (Prelude.Maybe Prelude.Text)
ruleGroupSummary_arn = Lens.lens (\RuleGroupSummary' {arn} -> arn) (\s@RuleGroupSummary' {} a -> s {arn = a} :: RuleGroupSummary)

-- | A description of the rule group that helps with identification.
ruleGroupSummary_description :: Lens.Lens' RuleGroupSummary (Prelude.Maybe Prelude.Text)
ruleGroupSummary_description = Lens.lens (\RuleGroupSummary' {description} -> description) (\s@RuleGroupSummary' {} a -> s {description = a} :: RuleGroupSummary)

-- | A unique identifier for the rule group. This ID is returned in the
-- responses to create and list commands. You provide it to operations like
-- update and delete.
ruleGroupSummary_id :: Lens.Lens' RuleGroupSummary (Prelude.Maybe Prelude.Text)
ruleGroupSummary_id = Lens.lens (\RuleGroupSummary' {id} -> id) (\s@RuleGroupSummary' {} a -> s {id = a} :: RuleGroupSummary)

-- | A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
ruleGroupSummary_lockToken :: Lens.Lens' RuleGroupSummary (Prelude.Maybe Prelude.Text)
ruleGroupSummary_lockToken = Lens.lens (\RuleGroupSummary' {lockToken} -> lockToken) (\s@RuleGroupSummary' {} a -> s {lockToken = a} :: RuleGroupSummary)

-- | The name of the data type instance. You cannot change the name after you
-- create the instance.
ruleGroupSummary_name :: Lens.Lens' RuleGroupSummary (Prelude.Maybe Prelude.Text)
ruleGroupSummary_name = Lens.lens (\RuleGroupSummary' {name} -> name) (\s@RuleGroupSummary' {} a -> s {name = a} :: RuleGroupSummary)

instance Data.FromJSON RuleGroupSummary where
  parseJSON =
    Data.withObject
      "RuleGroupSummary"
      ( \x ->
          RuleGroupSummary'
            Prelude.<$> (x Data..:? "ARN")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "LockToken")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable RuleGroupSummary where
  hashWithSalt _salt RuleGroupSummary' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lockToken
      `Prelude.hashWithSalt` name

instance Prelude.NFData RuleGroupSummary where
  rnf RuleGroupSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lockToken
      `Prelude.seq` Prelude.rnf name

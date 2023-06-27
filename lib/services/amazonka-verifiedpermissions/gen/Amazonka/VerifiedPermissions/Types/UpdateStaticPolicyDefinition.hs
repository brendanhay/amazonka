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
-- Module      : Amazonka.VerifiedPermissions.Types.UpdateStaticPolicyDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VerifiedPermissions.Types.UpdateStaticPolicyDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an update to a static policy.
--
-- /See:/ 'newUpdateStaticPolicyDefinition' smart constructor.
data UpdateStaticPolicyDefinition = UpdateStaticPolicyDefinition'
  { -- | Specifies the description to be added to or replaced on the static
    -- policy.
    description :: Prelude.Maybe Prelude.Text,
    -- | Specifies the Cedar policy language text to be added to or replaced on
    -- the static policy.
    --
    -- You can change only the following elements from the original content:
    --
    -- -   The @action@ referenced by the policy.
    --
    -- -   Any conditional clauses, such as @when@ or @unless@ clauses.
    --
    -- You __can\'t__ change the following elements:
    --
    -- -   Changing from @StaticPolicy@ to @TemplateLinkedPolicy@.
    --
    -- -   The effect (@permit@ or @forbid@) of the policy.
    --
    -- -   The @principal@ referenced by the policy.
    --
    -- -   The @resource@ referenced by the policy.
    statement :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStaticPolicyDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateStaticPolicyDefinition_description' - Specifies the description to be added to or replaced on the static
-- policy.
--
-- 'statement', 'updateStaticPolicyDefinition_statement' - Specifies the Cedar policy language text to be added to or replaced on
-- the static policy.
--
-- You can change only the following elements from the original content:
--
-- -   The @action@ referenced by the policy.
--
-- -   Any conditional clauses, such as @when@ or @unless@ clauses.
--
-- You __can\'t__ change the following elements:
--
-- -   Changing from @StaticPolicy@ to @TemplateLinkedPolicy@.
--
-- -   The effect (@permit@ or @forbid@) of the policy.
--
-- -   The @principal@ referenced by the policy.
--
-- -   The @resource@ referenced by the policy.
newUpdateStaticPolicyDefinition ::
  -- | 'statement'
  Prelude.Text ->
  UpdateStaticPolicyDefinition
newUpdateStaticPolicyDefinition pStatement_ =
  UpdateStaticPolicyDefinition'
    { description =
        Prelude.Nothing,
      statement = pStatement_
    }

-- | Specifies the description to be added to or replaced on the static
-- policy.
updateStaticPolicyDefinition_description :: Lens.Lens' UpdateStaticPolicyDefinition (Prelude.Maybe Prelude.Text)
updateStaticPolicyDefinition_description = Lens.lens (\UpdateStaticPolicyDefinition' {description} -> description) (\s@UpdateStaticPolicyDefinition' {} a -> s {description = a} :: UpdateStaticPolicyDefinition)

-- | Specifies the Cedar policy language text to be added to or replaced on
-- the static policy.
--
-- You can change only the following elements from the original content:
--
-- -   The @action@ referenced by the policy.
--
-- -   Any conditional clauses, such as @when@ or @unless@ clauses.
--
-- You __can\'t__ change the following elements:
--
-- -   Changing from @StaticPolicy@ to @TemplateLinkedPolicy@.
--
-- -   The effect (@permit@ or @forbid@) of the policy.
--
-- -   The @principal@ referenced by the policy.
--
-- -   The @resource@ referenced by the policy.
updateStaticPolicyDefinition_statement :: Lens.Lens' UpdateStaticPolicyDefinition Prelude.Text
updateStaticPolicyDefinition_statement = Lens.lens (\UpdateStaticPolicyDefinition' {statement} -> statement) (\s@UpdateStaticPolicyDefinition' {} a -> s {statement = a} :: UpdateStaticPolicyDefinition)

instance
  Prelude.Hashable
    UpdateStaticPolicyDefinition
  where
  hashWithSalt _salt UpdateStaticPolicyDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` statement

instance Prelude.NFData UpdateStaticPolicyDefinition where
  rnf UpdateStaticPolicyDefinition' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf statement

instance Data.ToJSON UpdateStaticPolicyDefinition where
  toJSON UpdateStaticPolicyDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            Prelude.Just ("statement" Data..= statement)
          ]
      )

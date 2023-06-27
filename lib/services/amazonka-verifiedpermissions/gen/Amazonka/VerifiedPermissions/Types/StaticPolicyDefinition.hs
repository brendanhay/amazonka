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
-- Module      : Amazonka.VerifiedPermissions.Types.StaticPolicyDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VerifiedPermissions.Types.StaticPolicyDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a static policy.
--
-- This data type is used as a field that is part of the
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_PolicyDefinitionDetail.html PolicyDefinitionDetail>
-- type.
--
-- /See:/ 'newStaticPolicyDefinition' smart constructor.
data StaticPolicyDefinition = StaticPolicyDefinition'
  { -- | The description of the static policy.
    description :: Prelude.Maybe Prelude.Text,
    -- | The policy content of the static policy, written in the Cedar policy
    -- language.
    statement :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StaticPolicyDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'staticPolicyDefinition_description' - The description of the static policy.
--
-- 'statement', 'staticPolicyDefinition_statement' - The policy content of the static policy, written in the Cedar policy
-- language.
newStaticPolicyDefinition ::
  -- | 'statement'
  Prelude.Text ->
  StaticPolicyDefinition
newStaticPolicyDefinition pStatement_ =
  StaticPolicyDefinition'
    { description =
        Prelude.Nothing,
      statement = pStatement_
    }

-- | The description of the static policy.
staticPolicyDefinition_description :: Lens.Lens' StaticPolicyDefinition (Prelude.Maybe Prelude.Text)
staticPolicyDefinition_description = Lens.lens (\StaticPolicyDefinition' {description} -> description) (\s@StaticPolicyDefinition' {} a -> s {description = a} :: StaticPolicyDefinition)

-- | The policy content of the static policy, written in the Cedar policy
-- language.
staticPolicyDefinition_statement :: Lens.Lens' StaticPolicyDefinition Prelude.Text
staticPolicyDefinition_statement = Lens.lens (\StaticPolicyDefinition' {statement} -> statement) (\s@StaticPolicyDefinition' {} a -> s {statement = a} :: StaticPolicyDefinition)

instance Prelude.Hashable StaticPolicyDefinition where
  hashWithSalt _salt StaticPolicyDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` statement

instance Prelude.NFData StaticPolicyDefinition where
  rnf StaticPolicyDefinition' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf statement

instance Data.ToJSON StaticPolicyDefinition where
  toJSON StaticPolicyDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            Prelude.Just ("statement" Data..= statement)
          ]
      )

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
-- Module      : Amazonka.VerifiedPermissions.Types.StaticPolicyDefinitionDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VerifiedPermissions.Types.StaticPolicyDefinitionDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains details about a static policy. It includes the
-- description and policy body.
--
-- This data type is used within a
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_PolicyDefinition.html PolicyDefinition>
-- structure as part of a request parameter for the
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_CreatePolicy.html CreatePolicy>
-- operation.
--
-- /See:/ 'newStaticPolicyDefinitionDetail' smart constructor.
data StaticPolicyDefinitionDetail = StaticPolicyDefinitionDetail'
  { -- | A description of the static policy.
    description :: Prelude.Maybe Prelude.Text,
    -- | The content of the static policy written in the Cedar policy language.
    statement :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StaticPolicyDefinitionDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'staticPolicyDefinitionDetail_description' - A description of the static policy.
--
-- 'statement', 'staticPolicyDefinitionDetail_statement' - The content of the static policy written in the Cedar policy language.
newStaticPolicyDefinitionDetail ::
  -- | 'statement'
  Prelude.Text ->
  StaticPolicyDefinitionDetail
newStaticPolicyDefinitionDetail pStatement_ =
  StaticPolicyDefinitionDetail'
    { description =
        Prelude.Nothing,
      statement = pStatement_
    }

-- | A description of the static policy.
staticPolicyDefinitionDetail_description :: Lens.Lens' StaticPolicyDefinitionDetail (Prelude.Maybe Prelude.Text)
staticPolicyDefinitionDetail_description = Lens.lens (\StaticPolicyDefinitionDetail' {description} -> description) (\s@StaticPolicyDefinitionDetail' {} a -> s {description = a} :: StaticPolicyDefinitionDetail)

-- | The content of the static policy written in the Cedar policy language.
staticPolicyDefinitionDetail_statement :: Lens.Lens' StaticPolicyDefinitionDetail Prelude.Text
staticPolicyDefinitionDetail_statement = Lens.lens (\StaticPolicyDefinitionDetail' {statement} -> statement) (\s@StaticPolicyDefinitionDetail' {} a -> s {statement = a} :: StaticPolicyDefinitionDetail)

instance Data.FromJSON StaticPolicyDefinitionDetail where
  parseJSON =
    Data.withObject
      "StaticPolicyDefinitionDetail"
      ( \x ->
          StaticPolicyDefinitionDetail'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..: "statement")
      )

instance
  Prelude.Hashable
    StaticPolicyDefinitionDetail
  where
  hashWithSalt _salt StaticPolicyDefinitionDetail' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` statement

instance Prelude.NFData StaticPolicyDefinitionDetail where
  rnf StaticPolicyDefinitionDetail' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf statement

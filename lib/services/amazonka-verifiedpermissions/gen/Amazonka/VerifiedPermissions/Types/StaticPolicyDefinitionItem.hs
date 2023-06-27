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
-- Module      : Amazonka.VerifiedPermissions.Types.StaticPolicyDefinitionItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VerifiedPermissions.Types.StaticPolicyDefinitionItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains details about a static policy. It includes the
-- description and policy statement.
--
-- This data type is used within a
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_PolicyDefinition.html PolicyDefinition>
-- structure as part of a request parameter for the
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_CreatePolicy.html CreatePolicy>
-- operation.
--
-- /See:/ 'newStaticPolicyDefinitionItem' smart constructor.
data StaticPolicyDefinitionItem = StaticPolicyDefinitionItem'
  { -- | A description of the static policy.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StaticPolicyDefinitionItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'staticPolicyDefinitionItem_description' - A description of the static policy.
newStaticPolicyDefinitionItem ::
  StaticPolicyDefinitionItem
newStaticPolicyDefinitionItem =
  StaticPolicyDefinitionItem'
    { description =
        Prelude.Nothing
    }

-- | A description of the static policy.
staticPolicyDefinitionItem_description :: Lens.Lens' StaticPolicyDefinitionItem (Prelude.Maybe Prelude.Text)
staticPolicyDefinitionItem_description = Lens.lens (\StaticPolicyDefinitionItem' {description} -> description) (\s@StaticPolicyDefinitionItem' {} a -> s {description = a} :: StaticPolicyDefinitionItem)

instance Data.FromJSON StaticPolicyDefinitionItem where
  parseJSON =
    Data.withObject
      "StaticPolicyDefinitionItem"
      ( \x ->
          StaticPolicyDefinitionItem'
            Prelude.<$> (x Data..:? "description")
      )

instance Prelude.Hashable StaticPolicyDefinitionItem where
  hashWithSalt _salt StaticPolicyDefinitionItem' {..} =
    _salt `Prelude.hashWithSalt` description

instance Prelude.NFData StaticPolicyDefinitionItem where
  rnf StaticPolicyDefinitionItem' {..} =
    Prelude.rnf description

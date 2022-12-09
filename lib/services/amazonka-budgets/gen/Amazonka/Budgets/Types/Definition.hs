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
-- Module      : Amazonka.Budgets.Types.Definition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Budgets.Types.Definition where

import Amazonka.Budgets.Types.IamActionDefinition
import Amazonka.Budgets.Types.ScpActionDefinition
import Amazonka.Budgets.Types.SsmActionDefinition
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies all of the type-specific parameters.
--
-- /See:/ 'newDefinition' smart constructor.
data Definition = Definition'
  { -- | The Identity and Access Management (IAM) action definition details.
    iamActionDefinition :: Prelude.Maybe IamActionDefinition,
    -- | The service control policies (SCPs) action definition details.
    scpActionDefinition :: Prelude.Maybe ScpActionDefinition,
    -- | The Amazon Web Services Systems Manager (SSM) action definition details.
    ssmActionDefinition :: Prelude.Maybe SsmActionDefinition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Definition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iamActionDefinition', 'definition_iamActionDefinition' - The Identity and Access Management (IAM) action definition details.
--
-- 'scpActionDefinition', 'definition_scpActionDefinition' - The service control policies (SCPs) action definition details.
--
-- 'ssmActionDefinition', 'definition_ssmActionDefinition' - The Amazon Web Services Systems Manager (SSM) action definition details.
newDefinition ::
  Definition
newDefinition =
  Definition'
    { iamActionDefinition = Prelude.Nothing,
      scpActionDefinition = Prelude.Nothing,
      ssmActionDefinition = Prelude.Nothing
    }

-- | The Identity and Access Management (IAM) action definition details.
definition_iamActionDefinition :: Lens.Lens' Definition (Prelude.Maybe IamActionDefinition)
definition_iamActionDefinition = Lens.lens (\Definition' {iamActionDefinition} -> iamActionDefinition) (\s@Definition' {} a -> s {iamActionDefinition = a} :: Definition)

-- | The service control policies (SCPs) action definition details.
definition_scpActionDefinition :: Lens.Lens' Definition (Prelude.Maybe ScpActionDefinition)
definition_scpActionDefinition = Lens.lens (\Definition' {scpActionDefinition} -> scpActionDefinition) (\s@Definition' {} a -> s {scpActionDefinition = a} :: Definition)

-- | The Amazon Web Services Systems Manager (SSM) action definition details.
definition_ssmActionDefinition :: Lens.Lens' Definition (Prelude.Maybe SsmActionDefinition)
definition_ssmActionDefinition = Lens.lens (\Definition' {ssmActionDefinition} -> ssmActionDefinition) (\s@Definition' {} a -> s {ssmActionDefinition = a} :: Definition)

instance Data.FromJSON Definition where
  parseJSON =
    Data.withObject
      "Definition"
      ( \x ->
          Definition'
            Prelude.<$> (x Data..:? "IamActionDefinition")
            Prelude.<*> (x Data..:? "ScpActionDefinition")
            Prelude.<*> (x Data..:? "SsmActionDefinition")
      )

instance Prelude.Hashable Definition where
  hashWithSalt _salt Definition' {..} =
    _salt `Prelude.hashWithSalt` iamActionDefinition
      `Prelude.hashWithSalt` scpActionDefinition
      `Prelude.hashWithSalt` ssmActionDefinition

instance Prelude.NFData Definition where
  rnf Definition' {..} =
    Prelude.rnf iamActionDefinition
      `Prelude.seq` Prelude.rnf scpActionDefinition
      `Prelude.seq` Prelude.rnf ssmActionDefinition

instance Data.ToJSON Definition where
  toJSON Definition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IamActionDefinition" Data..=)
              Prelude.<$> iamActionDefinition,
            ("ScpActionDefinition" Data..=)
              Prelude.<$> scpActionDefinition,
            ("SsmActionDefinition" Data..=)
              Prelude.<$> ssmActionDefinition
          ]
      )

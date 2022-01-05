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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Budgets.Types.Definition where

import Amazonka.Budgets.Types.IamActionDefinition
import Amazonka.Budgets.Types.ScpActionDefinition
import Amazonka.Budgets.Types.SsmActionDefinition
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies all of the type-specific parameters.
--
-- /See:/ 'newDefinition' smart constructor.
data Definition = Definition'
  { -- | The service control policies (SCPs) action definition details.
    scpActionDefinition :: Prelude.Maybe ScpActionDefinition,
    -- | The AWS Identity and Access Management (IAM) action definition details.
    iamActionDefinition :: Prelude.Maybe IamActionDefinition,
    -- | The AWS Systems Manager (SSM) action definition details.
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
-- 'scpActionDefinition', 'definition_scpActionDefinition' - The service control policies (SCPs) action definition details.
--
-- 'iamActionDefinition', 'definition_iamActionDefinition' - The AWS Identity and Access Management (IAM) action definition details.
--
-- 'ssmActionDefinition', 'definition_ssmActionDefinition' - The AWS Systems Manager (SSM) action definition details.
newDefinition ::
  Definition
newDefinition =
  Definition'
    { scpActionDefinition = Prelude.Nothing,
      iamActionDefinition = Prelude.Nothing,
      ssmActionDefinition = Prelude.Nothing
    }

-- | The service control policies (SCPs) action definition details.
definition_scpActionDefinition :: Lens.Lens' Definition (Prelude.Maybe ScpActionDefinition)
definition_scpActionDefinition = Lens.lens (\Definition' {scpActionDefinition} -> scpActionDefinition) (\s@Definition' {} a -> s {scpActionDefinition = a} :: Definition)

-- | The AWS Identity and Access Management (IAM) action definition details.
definition_iamActionDefinition :: Lens.Lens' Definition (Prelude.Maybe IamActionDefinition)
definition_iamActionDefinition = Lens.lens (\Definition' {iamActionDefinition} -> iamActionDefinition) (\s@Definition' {} a -> s {iamActionDefinition = a} :: Definition)

-- | The AWS Systems Manager (SSM) action definition details.
definition_ssmActionDefinition :: Lens.Lens' Definition (Prelude.Maybe SsmActionDefinition)
definition_ssmActionDefinition = Lens.lens (\Definition' {ssmActionDefinition} -> ssmActionDefinition) (\s@Definition' {} a -> s {ssmActionDefinition = a} :: Definition)

instance Core.FromJSON Definition where
  parseJSON =
    Core.withObject
      "Definition"
      ( \x ->
          Definition'
            Prelude.<$> (x Core..:? "ScpActionDefinition")
            Prelude.<*> (x Core..:? "IamActionDefinition")
            Prelude.<*> (x Core..:? "SsmActionDefinition")
      )

instance Prelude.Hashable Definition where
  hashWithSalt _salt Definition' {..} =
    _salt `Prelude.hashWithSalt` scpActionDefinition
      `Prelude.hashWithSalt` iamActionDefinition
      `Prelude.hashWithSalt` ssmActionDefinition

instance Prelude.NFData Definition where
  rnf Definition' {..} =
    Prelude.rnf scpActionDefinition
      `Prelude.seq` Prelude.rnf iamActionDefinition
      `Prelude.seq` Prelude.rnf ssmActionDefinition

instance Core.ToJSON Definition where
  toJSON Definition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ScpActionDefinition" Core..=)
              Prelude.<$> scpActionDefinition,
            ("IamActionDefinition" Core..=)
              Prelude.<$> iamActionDefinition,
            ("SsmActionDefinition" Core..=)
              Prelude.<$> ssmActionDefinition
          ]
      )

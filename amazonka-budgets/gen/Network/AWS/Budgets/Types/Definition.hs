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
-- Module      : Network.AWS.Budgets.Types.Definition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.Definition where

import Network.AWS.Budgets.Types.IamActionDefinition
import Network.AWS.Budgets.Types.ScpActionDefinition
import Network.AWS.Budgets.Types.SsmActionDefinition
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies all of the type-specific parameters.
--
-- /See:/ 'newDefinition' smart constructor.
data Definition = Definition'
  { -- | The AWS Identity and Access Management (IAM) action definition details.
    iamActionDefinition :: Core.Maybe IamActionDefinition,
    -- | The AWS Systems Manager (SSM) action definition details.
    ssmActionDefinition :: Core.Maybe SsmActionDefinition,
    -- | The service control policies (SCPs) action definition details.
    scpActionDefinition :: Core.Maybe ScpActionDefinition
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Definition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iamActionDefinition', 'definition_iamActionDefinition' - The AWS Identity and Access Management (IAM) action definition details.
--
-- 'ssmActionDefinition', 'definition_ssmActionDefinition' - The AWS Systems Manager (SSM) action definition details.
--
-- 'scpActionDefinition', 'definition_scpActionDefinition' - The service control policies (SCPs) action definition details.
newDefinition ::
  Definition
newDefinition =
  Definition'
    { iamActionDefinition = Core.Nothing,
      ssmActionDefinition = Core.Nothing,
      scpActionDefinition = Core.Nothing
    }

-- | The AWS Identity and Access Management (IAM) action definition details.
definition_iamActionDefinition :: Lens.Lens' Definition (Core.Maybe IamActionDefinition)
definition_iamActionDefinition = Lens.lens (\Definition' {iamActionDefinition} -> iamActionDefinition) (\s@Definition' {} a -> s {iamActionDefinition = a} :: Definition)

-- | The AWS Systems Manager (SSM) action definition details.
definition_ssmActionDefinition :: Lens.Lens' Definition (Core.Maybe SsmActionDefinition)
definition_ssmActionDefinition = Lens.lens (\Definition' {ssmActionDefinition} -> ssmActionDefinition) (\s@Definition' {} a -> s {ssmActionDefinition = a} :: Definition)

-- | The service control policies (SCPs) action definition details.
definition_scpActionDefinition :: Lens.Lens' Definition (Core.Maybe ScpActionDefinition)
definition_scpActionDefinition = Lens.lens (\Definition' {scpActionDefinition} -> scpActionDefinition) (\s@Definition' {} a -> s {scpActionDefinition = a} :: Definition)

instance Core.FromJSON Definition where
  parseJSON =
    Core.withObject
      "Definition"
      ( \x ->
          Definition'
            Core.<$> (x Core..:? "IamActionDefinition")
            Core.<*> (x Core..:? "SsmActionDefinition")
            Core.<*> (x Core..:? "ScpActionDefinition")
      )

instance Core.Hashable Definition

instance Core.NFData Definition

instance Core.ToJSON Definition where
  toJSON Definition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("IamActionDefinition" Core..=)
              Core.<$> iamActionDefinition,
            ("SsmActionDefinition" Core..=)
              Core.<$> ssmActionDefinition,
            ("ScpActionDefinition" Core..=)
              Core.<$> scpActionDefinition
          ]
      )

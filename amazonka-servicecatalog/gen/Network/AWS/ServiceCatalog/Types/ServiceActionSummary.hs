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
-- Module      : Network.AWS.ServiceCatalog.Types.ServiceActionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ServiceActionSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.ServiceCatalog.Types.ServiceActionDefinitionType

-- | Detailed information about the self-service action.
--
-- /See:/ 'newServiceActionSummary' smart constructor.
data ServiceActionSummary = ServiceActionSummary'
  { -- | The self-service action identifier.
    id :: Core.Maybe Core.Text,
    -- | The self-service action definition type. For example, @SSM_AUTOMATION@.
    definitionType :: Core.Maybe ServiceActionDefinitionType,
    -- | The self-service action name.
    name :: Core.Maybe Core.Text,
    -- | The self-service action description.
    description :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ServiceActionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'serviceActionSummary_id' - The self-service action identifier.
--
-- 'definitionType', 'serviceActionSummary_definitionType' - The self-service action definition type. For example, @SSM_AUTOMATION@.
--
-- 'name', 'serviceActionSummary_name' - The self-service action name.
--
-- 'description', 'serviceActionSummary_description' - The self-service action description.
newServiceActionSummary ::
  ServiceActionSummary
newServiceActionSummary =
  ServiceActionSummary'
    { id = Core.Nothing,
      definitionType = Core.Nothing,
      name = Core.Nothing,
      description = Core.Nothing
    }

-- | The self-service action identifier.
serviceActionSummary_id :: Lens.Lens' ServiceActionSummary (Core.Maybe Core.Text)
serviceActionSummary_id = Lens.lens (\ServiceActionSummary' {id} -> id) (\s@ServiceActionSummary' {} a -> s {id = a} :: ServiceActionSummary)

-- | The self-service action definition type. For example, @SSM_AUTOMATION@.
serviceActionSummary_definitionType :: Lens.Lens' ServiceActionSummary (Core.Maybe ServiceActionDefinitionType)
serviceActionSummary_definitionType = Lens.lens (\ServiceActionSummary' {definitionType} -> definitionType) (\s@ServiceActionSummary' {} a -> s {definitionType = a} :: ServiceActionSummary)

-- | The self-service action name.
serviceActionSummary_name :: Lens.Lens' ServiceActionSummary (Core.Maybe Core.Text)
serviceActionSummary_name = Lens.lens (\ServiceActionSummary' {name} -> name) (\s@ServiceActionSummary' {} a -> s {name = a} :: ServiceActionSummary)

-- | The self-service action description.
serviceActionSummary_description :: Lens.Lens' ServiceActionSummary (Core.Maybe Core.Text)
serviceActionSummary_description = Lens.lens (\ServiceActionSummary' {description} -> description) (\s@ServiceActionSummary' {} a -> s {description = a} :: ServiceActionSummary)

instance Core.FromJSON ServiceActionSummary where
  parseJSON =
    Core.withObject
      "ServiceActionSummary"
      ( \x ->
          ServiceActionSummary'
            Core.<$> (x Core..:? "Id")
            Core.<*> (x Core..:? "DefinitionType")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Description")
      )

instance Core.Hashable ServiceActionSummary

instance Core.NFData ServiceActionSummary

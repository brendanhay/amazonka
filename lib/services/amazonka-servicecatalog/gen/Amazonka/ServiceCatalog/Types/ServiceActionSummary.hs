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
-- Module      : Amazonka.ServiceCatalog.Types.ServiceActionSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.ServiceActionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceCatalog.Types.ServiceActionDefinitionType

-- | Detailed information about the self-service action.
--
-- /See:/ 'newServiceActionSummary' smart constructor.
data ServiceActionSummary = ServiceActionSummary'
  { -- | The self-service action definition type. For example, @SSM_AUTOMATION@.
    definitionType :: Prelude.Maybe ServiceActionDefinitionType,
    -- | The self-service action description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The self-service action identifier.
    id :: Prelude.Maybe Prelude.Text,
    -- | The self-service action name.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceActionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'definitionType', 'serviceActionSummary_definitionType' - The self-service action definition type. For example, @SSM_AUTOMATION@.
--
-- 'description', 'serviceActionSummary_description' - The self-service action description.
--
-- 'id', 'serviceActionSummary_id' - The self-service action identifier.
--
-- 'name', 'serviceActionSummary_name' - The self-service action name.
newServiceActionSummary ::
  ServiceActionSummary
newServiceActionSummary =
  ServiceActionSummary'
    { definitionType =
        Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The self-service action definition type. For example, @SSM_AUTOMATION@.
serviceActionSummary_definitionType :: Lens.Lens' ServiceActionSummary (Prelude.Maybe ServiceActionDefinitionType)
serviceActionSummary_definitionType = Lens.lens (\ServiceActionSummary' {definitionType} -> definitionType) (\s@ServiceActionSummary' {} a -> s {definitionType = a} :: ServiceActionSummary)

-- | The self-service action description.
serviceActionSummary_description :: Lens.Lens' ServiceActionSummary (Prelude.Maybe Prelude.Text)
serviceActionSummary_description = Lens.lens (\ServiceActionSummary' {description} -> description) (\s@ServiceActionSummary' {} a -> s {description = a} :: ServiceActionSummary)

-- | The self-service action identifier.
serviceActionSummary_id :: Lens.Lens' ServiceActionSummary (Prelude.Maybe Prelude.Text)
serviceActionSummary_id = Lens.lens (\ServiceActionSummary' {id} -> id) (\s@ServiceActionSummary' {} a -> s {id = a} :: ServiceActionSummary)

-- | The self-service action name.
serviceActionSummary_name :: Lens.Lens' ServiceActionSummary (Prelude.Maybe Prelude.Text)
serviceActionSummary_name = Lens.lens (\ServiceActionSummary' {name} -> name) (\s@ServiceActionSummary' {} a -> s {name = a} :: ServiceActionSummary)

instance Data.FromJSON ServiceActionSummary where
  parseJSON =
    Data.withObject
      "ServiceActionSummary"
      ( \x ->
          ServiceActionSummary'
            Prelude.<$> (x Data..:? "DefinitionType")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable ServiceActionSummary where
  hashWithSalt _salt ServiceActionSummary' {..} =
    _salt `Prelude.hashWithSalt` definitionType
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name

instance Prelude.NFData ServiceActionSummary where
  rnf ServiceActionSummary' {..} =
    Prelude.rnf definitionType
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name

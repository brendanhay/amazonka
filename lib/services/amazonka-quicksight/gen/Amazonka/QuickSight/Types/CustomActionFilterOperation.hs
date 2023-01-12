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
-- Module      : Amazonka.QuickSight.Types.CustomActionFilterOperation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.CustomActionFilterOperation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FilterOperationSelectedFieldsConfiguration
import Amazonka.QuickSight.Types.FilterOperationTargetVisualsConfiguration

-- | The filter operation that filters data included in a visual or in an
-- entire sheet.
--
-- /See:/ 'newCustomActionFilterOperation' smart constructor.
data CustomActionFilterOperation = CustomActionFilterOperation'
  { -- | The configuration that chooses the fields to be filtered.
    selectedFieldsConfiguration :: FilterOperationSelectedFieldsConfiguration,
    -- | The configuration that chooses the target visuals to be filtered.
    targetVisualsConfiguration :: FilterOperationTargetVisualsConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomActionFilterOperation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'selectedFieldsConfiguration', 'customActionFilterOperation_selectedFieldsConfiguration' - The configuration that chooses the fields to be filtered.
--
-- 'targetVisualsConfiguration', 'customActionFilterOperation_targetVisualsConfiguration' - The configuration that chooses the target visuals to be filtered.
newCustomActionFilterOperation ::
  -- | 'selectedFieldsConfiguration'
  FilterOperationSelectedFieldsConfiguration ->
  -- | 'targetVisualsConfiguration'
  FilterOperationTargetVisualsConfiguration ->
  CustomActionFilterOperation
newCustomActionFilterOperation
  pSelectedFieldsConfiguration_
  pTargetVisualsConfiguration_ =
    CustomActionFilterOperation'
      { selectedFieldsConfiguration =
          pSelectedFieldsConfiguration_,
        targetVisualsConfiguration =
          pTargetVisualsConfiguration_
      }

-- | The configuration that chooses the fields to be filtered.
customActionFilterOperation_selectedFieldsConfiguration :: Lens.Lens' CustomActionFilterOperation FilterOperationSelectedFieldsConfiguration
customActionFilterOperation_selectedFieldsConfiguration = Lens.lens (\CustomActionFilterOperation' {selectedFieldsConfiguration} -> selectedFieldsConfiguration) (\s@CustomActionFilterOperation' {} a -> s {selectedFieldsConfiguration = a} :: CustomActionFilterOperation)

-- | The configuration that chooses the target visuals to be filtered.
customActionFilterOperation_targetVisualsConfiguration :: Lens.Lens' CustomActionFilterOperation FilterOperationTargetVisualsConfiguration
customActionFilterOperation_targetVisualsConfiguration = Lens.lens (\CustomActionFilterOperation' {targetVisualsConfiguration} -> targetVisualsConfiguration) (\s@CustomActionFilterOperation' {} a -> s {targetVisualsConfiguration = a} :: CustomActionFilterOperation)

instance Data.FromJSON CustomActionFilterOperation where
  parseJSON =
    Data.withObject
      "CustomActionFilterOperation"
      ( \x ->
          CustomActionFilterOperation'
            Prelude.<$> (x Data..: "SelectedFieldsConfiguration")
            Prelude.<*> (x Data..: "TargetVisualsConfiguration")
      )

instance Prelude.Hashable CustomActionFilterOperation where
  hashWithSalt _salt CustomActionFilterOperation' {..} =
    _salt
      `Prelude.hashWithSalt` selectedFieldsConfiguration
      `Prelude.hashWithSalt` targetVisualsConfiguration

instance Prelude.NFData CustomActionFilterOperation where
  rnf CustomActionFilterOperation' {..} =
    Prelude.rnf selectedFieldsConfiguration
      `Prelude.seq` Prelude.rnf targetVisualsConfiguration

instance Data.ToJSON CustomActionFilterOperation where
  toJSON CustomActionFilterOperation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "SelectedFieldsConfiguration"
                  Data..= selectedFieldsConfiguration
              ),
            Prelude.Just
              ( "TargetVisualsConfiguration"
                  Data..= targetVisualsConfiguration
              )
          ]
      )

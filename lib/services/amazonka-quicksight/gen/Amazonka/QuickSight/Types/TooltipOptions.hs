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
-- Module      : Amazonka.QuickSight.Types.TooltipOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TooltipOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FieldBasedTooltip
import Amazonka.QuickSight.Types.SelectedTooltipType
import Amazonka.QuickSight.Types.Visibility

-- | The display options for the visual tooltip.
--
-- /See:/ 'newTooltipOptions' smart constructor.
data TooltipOptions = TooltipOptions'
  { -- | The setup for the detailed tooltip. The tooltip setup is always saved.
    -- The display type is decided based on the tooltip type.
    fieldBasedTooltip :: Prelude.Maybe FieldBasedTooltip,
    -- | The selected type for the tooltip. Choose one of the following options:
    --
    -- -   @BASIC@: A basic tooltip.
    --
    -- -   @DETAILED@: A detailed tooltip.
    selectedTooltipType :: Prelude.Maybe SelectedTooltipType,
    -- | Determines whether or not the tooltip is visible.
    tooltipVisibility :: Prelude.Maybe Visibility
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TooltipOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldBasedTooltip', 'tooltipOptions_fieldBasedTooltip' - The setup for the detailed tooltip. The tooltip setup is always saved.
-- The display type is decided based on the tooltip type.
--
-- 'selectedTooltipType', 'tooltipOptions_selectedTooltipType' - The selected type for the tooltip. Choose one of the following options:
--
-- -   @BASIC@: A basic tooltip.
--
-- -   @DETAILED@: A detailed tooltip.
--
-- 'tooltipVisibility', 'tooltipOptions_tooltipVisibility' - Determines whether or not the tooltip is visible.
newTooltipOptions ::
  TooltipOptions
newTooltipOptions =
  TooltipOptions'
    { fieldBasedTooltip =
        Prelude.Nothing,
      selectedTooltipType = Prelude.Nothing,
      tooltipVisibility = Prelude.Nothing
    }

-- | The setup for the detailed tooltip. The tooltip setup is always saved.
-- The display type is decided based on the tooltip type.
tooltipOptions_fieldBasedTooltip :: Lens.Lens' TooltipOptions (Prelude.Maybe FieldBasedTooltip)
tooltipOptions_fieldBasedTooltip = Lens.lens (\TooltipOptions' {fieldBasedTooltip} -> fieldBasedTooltip) (\s@TooltipOptions' {} a -> s {fieldBasedTooltip = a} :: TooltipOptions)

-- | The selected type for the tooltip. Choose one of the following options:
--
-- -   @BASIC@: A basic tooltip.
--
-- -   @DETAILED@: A detailed tooltip.
tooltipOptions_selectedTooltipType :: Lens.Lens' TooltipOptions (Prelude.Maybe SelectedTooltipType)
tooltipOptions_selectedTooltipType = Lens.lens (\TooltipOptions' {selectedTooltipType} -> selectedTooltipType) (\s@TooltipOptions' {} a -> s {selectedTooltipType = a} :: TooltipOptions)

-- | Determines whether or not the tooltip is visible.
tooltipOptions_tooltipVisibility :: Lens.Lens' TooltipOptions (Prelude.Maybe Visibility)
tooltipOptions_tooltipVisibility = Lens.lens (\TooltipOptions' {tooltipVisibility} -> tooltipVisibility) (\s@TooltipOptions' {} a -> s {tooltipVisibility = a} :: TooltipOptions)

instance Data.FromJSON TooltipOptions where
  parseJSON =
    Data.withObject
      "TooltipOptions"
      ( \x ->
          TooltipOptions'
            Prelude.<$> (x Data..:? "FieldBasedTooltip")
            Prelude.<*> (x Data..:? "SelectedTooltipType")
            Prelude.<*> (x Data..:? "TooltipVisibility")
      )

instance Prelude.Hashable TooltipOptions where
  hashWithSalt _salt TooltipOptions' {..} =
    _salt `Prelude.hashWithSalt` fieldBasedTooltip
      `Prelude.hashWithSalt` selectedTooltipType
      `Prelude.hashWithSalt` tooltipVisibility

instance Prelude.NFData TooltipOptions where
  rnf TooltipOptions' {..} =
    Prelude.rnf fieldBasedTooltip
      `Prelude.seq` Prelude.rnf selectedTooltipType
      `Prelude.seq` Prelude.rnf tooltipVisibility

instance Data.ToJSON TooltipOptions where
  toJSON TooltipOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FieldBasedTooltip" Data..=)
              Prelude.<$> fieldBasedTooltip,
            ("SelectedTooltipType" Data..=)
              Prelude.<$> selectedTooltipType,
            ("TooltipVisibility" Data..=)
              Prelude.<$> tooltipVisibility
          ]
      )

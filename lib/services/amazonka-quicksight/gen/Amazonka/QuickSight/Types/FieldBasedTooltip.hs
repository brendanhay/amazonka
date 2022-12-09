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
-- Module      : Amazonka.QuickSight.Types.FieldBasedTooltip
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FieldBasedTooltip where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TooltipItem
import Amazonka.QuickSight.Types.TooltipTitleType
import Amazonka.QuickSight.Types.Visibility

-- | The setup for the detailed tooltip.
--
-- /See:/ 'newFieldBasedTooltip' smart constructor.
data FieldBasedTooltip = FieldBasedTooltip'
  { -- | The visibility of @Show aggregations@.
    aggregationVisibility :: Prelude.Maybe Visibility,
    -- | The fields configuration in the tooltip.
    tooltipFields :: Prelude.Maybe [TooltipItem],
    -- | The type for the >tooltip title. Choose one of the following options:
    --
    -- -   @NONE@: Doesn\'t use the primary value as the title.
    --
    -- -   @PRIMARY_VALUE@: Uses primary value as the title.
    tooltipTitleType :: Prelude.Maybe TooltipTitleType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FieldBasedTooltip' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregationVisibility', 'fieldBasedTooltip_aggregationVisibility' - The visibility of @Show aggregations@.
--
-- 'tooltipFields', 'fieldBasedTooltip_tooltipFields' - The fields configuration in the tooltip.
--
-- 'tooltipTitleType', 'fieldBasedTooltip_tooltipTitleType' - The type for the >tooltip title. Choose one of the following options:
--
-- -   @NONE@: Doesn\'t use the primary value as the title.
--
-- -   @PRIMARY_VALUE@: Uses primary value as the title.
newFieldBasedTooltip ::
  FieldBasedTooltip
newFieldBasedTooltip =
  FieldBasedTooltip'
    { aggregationVisibility =
        Prelude.Nothing,
      tooltipFields = Prelude.Nothing,
      tooltipTitleType = Prelude.Nothing
    }

-- | The visibility of @Show aggregations@.
fieldBasedTooltip_aggregationVisibility :: Lens.Lens' FieldBasedTooltip (Prelude.Maybe Visibility)
fieldBasedTooltip_aggregationVisibility = Lens.lens (\FieldBasedTooltip' {aggregationVisibility} -> aggregationVisibility) (\s@FieldBasedTooltip' {} a -> s {aggregationVisibility = a} :: FieldBasedTooltip)

-- | The fields configuration in the tooltip.
fieldBasedTooltip_tooltipFields :: Lens.Lens' FieldBasedTooltip (Prelude.Maybe [TooltipItem])
fieldBasedTooltip_tooltipFields = Lens.lens (\FieldBasedTooltip' {tooltipFields} -> tooltipFields) (\s@FieldBasedTooltip' {} a -> s {tooltipFields = a} :: FieldBasedTooltip) Prelude.. Lens.mapping Lens.coerced

-- | The type for the >tooltip title. Choose one of the following options:
--
-- -   @NONE@: Doesn\'t use the primary value as the title.
--
-- -   @PRIMARY_VALUE@: Uses primary value as the title.
fieldBasedTooltip_tooltipTitleType :: Lens.Lens' FieldBasedTooltip (Prelude.Maybe TooltipTitleType)
fieldBasedTooltip_tooltipTitleType = Lens.lens (\FieldBasedTooltip' {tooltipTitleType} -> tooltipTitleType) (\s@FieldBasedTooltip' {} a -> s {tooltipTitleType = a} :: FieldBasedTooltip)

instance Data.FromJSON FieldBasedTooltip where
  parseJSON =
    Data.withObject
      "FieldBasedTooltip"
      ( \x ->
          FieldBasedTooltip'
            Prelude.<$> (x Data..:? "AggregationVisibility")
            Prelude.<*> (x Data..:? "TooltipFields" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "TooltipTitleType")
      )

instance Prelude.Hashable FieldBasedTooltip where
  hashWithSalt _salt FieldBasedTooltip' {..} =
    _salt `Prelude.hashWithSalt` aggregationVisibility
      `Prelude.hashWithSalt` tooltipFields
      `Prelude.hashWithSalt` tooltipTitleType

instance Prelude.NFData FieldBasedTooltip where
  rnf FieldBasedTooltip' {..} =
    Prelude.rnf aggregationVisibility
      `Prelude.seq` Prelude.rnf tooltipFields
      `Prelude.seq` Prelude.rnf tooltipTitleType

instance Data.ToJSON FieldBasedTooltip where
  toJSON FieldBasedTooltip' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AggregationVisibility" Data..=)
              Prelude.<$> aggregationVisibility,
            ("TooltipFields" Data..=) Prelude.<$> tooltipFields,
            ("TooltipTitleType" Data..=)
              Prelude.<$> tooltipTitleType
          ]
      )

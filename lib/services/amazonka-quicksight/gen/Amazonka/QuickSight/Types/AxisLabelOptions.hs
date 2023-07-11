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
-- Module      : Amazonka.QuickSight.Types.AxisLabelOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AxisLabelOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AxisLabelReferenceOptions
import Amazonka.QuickSight.Types.FontConfiguration

-- | The label options for a chart axis. You must specify the field that the
-- label is targeted to.
--
-- /See:/ 'newAxisLabelOptions' smart constructor.
data AxisLabelOptions = AxisLabelOptions'
  { -- | The options that indicate which field the label belongs to.
    applyTo :: Prelude.Maybe AxisLabelReferenceOptions,
    -- | The text for the axis label.
    customLabel :: Prelude.Maybe Prelude.Text,
    -- | The font configuration of the axis label.
    fontConfiguration :: Prelude.Maybe FontConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AxisLabelOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applyTo', 'axisLabelOptions_applyTo' - The options that indicate which field the label belongs to.
--
-- 'customLabel', 'axisLabelOptions_customLabel' - The text for the axis label.
--
-- 'fontConfiguration', 'axisLabelOptions_fontConfiguration' - The font configuration of the axis label.
newAxisLabelOptions ::
  AxisLabelOptions
newAxisLabelOptions =
  AxisLabelOptions'
    { applyTo = Prelude.Nothing,
      customLabel = Prelude.Nothing,
      fontConfiguration = Prelude.Nothing
    }

-- | The options that indicate which field the label belongs to.
axisLabelOptions_applyTo :: Lens.Lens' AxisLabelOptions (Prelude.Maybe AxisLabelReferenceOptions)
axisLabelOptions_applyTo = Lens.lens (\AxisLabelOptions' {applyTo} -> applyTo) (\s@AxisLabelOptions' {} a -> s {applyTo = a} :: AxisLabelOptions)

-- | The text for the axis label.
axisLabelOptions_customLabel :: Lens.Lens' AxisLabelOptions (Prelude.Maybe Prelude.Text)
axisLabelOptions_customLabel = Lens.lens (\AxisLabelOptions' {customLabel} -> customLabel) (\s@AxisLabelOptions' {} a -> s {customLabel = a} :: AxisLabelOptions)

-- | The font configuration of the axis label.
axisLabelOptions_fontConfiguration :: Lens.Lens' AxisLabelOptions (Prelude.Maybe FontConfiguration)
axisLabelOptions_fontConfiguration = Lens.lens (\AxisLabelOptions' {fontConfiguration} -> fontConfiguration) (\s@AxisLabelOptions' {} a -> s {fontConfiguration = a} :: AxisLabelOptions)

instance Data.FromJSON AxisLabelOptions where
  parseJSON =
    Data.withObject
      "AxisLabelOptions"
      ( \x ->
          AxisLabelOptions'
            Prelude.<$> (x Data..:? "ApplyTo")
            Prelude.<*> (x Data..:? "CustomLabel")
            Prelude.<*> (x Data..:? "FontConfiguration")
      )

instance Prelude.Hashable AxisLabelOptions where
  hashWithSalt _salt AxisLabelOptions' {..} =
    _salt
      `Prelude.hashWithSalt` applyTo
      `Prelude.hashWithSalt` customLabel
      `Prelude.hashWithSalt` fontConfiguration

instance Prelude.NFData AxisLabelOptions where
  rnf AxisLabelOptions' {..} =
    Prelude.rnf applyTo
      `Prelude.seq` Prelude.rnf customLabel
      `Prelude.seq` Prelude.rnf fontConfiguration

instance Data.ToJSON AxisLabelOptions where
  toJSON AxisLabelOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ApplyTo" Data..=) Prelude.<$> applyTo,
            ("CustomLabel" Data..=) Prelude.<$> customLabel,
            ("FontConfiguration" Data..=)
              Prelude.<$> fontConfiguration
          ]
      )

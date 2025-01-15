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
-- Module      : Amazonka.QuickSight.Types.LineChartLineStyleSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.LineChartLineStyleSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.LineChartLineStyle
import Amazonka.QuickSight.Types.LineInterpolation
import Amazonka.QuickSight.Types.Visibility

-- | Line styles options for a line series in @LineChartVisual@.
--
-- /See:/ 'newLineChartLineStyleSettings' smart constructor.
data LineChartLineStyleSettings = LineChartLineStyleSettings'
  { -- | Interpolation style for line series.
    --
    -- -   @LINEAR@: Show as default, linear style.
    --
    -- -   @SMOOTH@: Show as a smooth curve.
    --
    -- -   @STEPPED@: Show steps in line.
    lineInterpolation :: Prelude.Maybe LineInterpolation,
    -- | Line style for line series.
    --
    -- -   @SOLID@: Show as a solid line.
    --
    -- -   @DOTTED@: Show as a dotted line.
    --
    -- -   @DASHED@: Show as a dashed line.
    lineStyle :: Prelude.Maybe LineChartLineStyle,
    -- | Configuration option that determines whether to show the line for the
    -- series.
    lineVisibility :: Prelude.Maybe Visibility,
    -- | Width that determines the line thickness.
    lineWidth :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LineChartLineStyleSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lineInterpolation', 'lineChartLineStyleSettings_lineInterpolation' - Interpolation style for line series.
--
-- -   @LINEAR@: Show as default, linear style.
--
-- -   @SMOOTH@: Show as a smooth curve.
--
-- -   @STEPPED@: Show steps in line.
--
-- 'lineStyle', 'lineChartLineStyleSettings_lineStyle' - Line style for line series.
--
-- -   @SOLID@: Show as a solid line.
--
-- -   @DOTTED@: Show as a dotted line.
--
-- -   @DASHED@: Show as a dashed line.
--
-- 'lineVisibility', 'lineChartLineStyleSettings_lineVisibility' - Configuration option that determines whether to show the line for the
-- series.
--
-- 'lineWidth', 'lineChartLineStyleSettings_lineWidth' - Width that determines the line thickness.
newLineChartLineStyleSettings ::
  LineChartLineStyleSettings
newLineChartLineStyleSettings =
  LineChartLineStyleSettings'
    { lineInterpolation =
        Prelude.Nothing,
      lineStyle = Prelude.Nothing,
      lineVisibility = Prelude.Nothing,
      lineWidth = Prelude.Nothing
    }

-- | Interpolation style for line series.
--
-- -   @LINEAR@: Show as default, linear style.
--
-- -   @SMOOTH@: Show as a smooth curve.
--
-- -   @STEPPED@: Show steps in line.
lineChartLineStyleSettings_lineInterpolation :: Lens.Lens' LineChartLineStyleSettings (Prelude.Maybe LineInterpolation)
lineChartLineStyleSettings_lineInterpolation = Lens.lens (\LineChartLineStyleSettings' {lineInterpolation} -> lineInterpolation) (\s@LineChartLineStyleSettings' {} a -> s {lineInterpolation = a} :: LineChartLineStyleSettings)

-- | Line style for line series.
--
-- -   @SOLID@: Show as a solid line.
--
-- -   @DOTTED@: Show as a dotted line.
--
-- -   @DASHED@: Show as a dashed line.
lineChartLineStyleSettings_lineStyle :: Lens.Lens' LineChartLineStyleSettings (Prelude.Maybe LineChartLineStyle)
lineChartLineStyleSettings_lineStyle = Lens.lens (\LineChartLineStyleSettings' {lineStyle} -> lineStyle) (\s@LineChartLineStyleSettings' {} a -> s {lineStyle = a} :: LineChartLineStyleSettings)

-- | Configuration option that determines whether to show the line for the
-- series.
lineChartLineStyleSettings_lineVisibility :: Lens.Lens' LineChartLineStyleSettings (Prelude.Maybe Visibility)
lineChartLineStyleSettings_lineVisibility = Lens.lens (\LineChartLineStyleSettings' {lineVisibility} -> lineVisibility) (\s@LineChartLineStyleSettings' {} a -> s {lineVisibility = a} :: LineChartLineStyleSettings)

-- | Width that determines the line thickness.
lineChartLineStyleSettings_lineWidth :: Lens.Lens' LineChartLineStyleSettings (Prelude.Maybe Prelude.Text)
lineChartLineStyleSettings_lineWidth = Lens.lens (\LineChartLineStyleSettings' {lineWidth} -> lineWidth) (\s@LineChartLineStyleSettings' {} a -> s {lineWidth = a} :: LineChartLineStyleSettings)

instance Data.FromJSON LineChartLineStyleSettings where
  parseJSON =
    Data.withObject
      "LineChartLineStyleSettings"
      ( \x ->
          LineChartLineStyleSettings'
            Prelude.<$> (x Data..:? "LineInterpolation")
            Prelude.<*> (x Data..:? "LineStyle")
            Prelude.<*> (x Data..:? "LineVisibility")
            Prelude.<*> (x Data..:? "LineWidth")
      )

instance Prelude.Hashable LineChartLineStyleSettings where
  hashWithSalt _salt LineChartLineStyleSettings' {..} =
    _salt
      `Prelude.hashWithSalt` lineInterpolation
      `Prelude.hashWithSalt` lineStyle
      `Prelude.hashWithSalt` lineVisibility
      `Prelude.hashWithSalt` lineWidth

instance Prelude.NFData LineChartLineStyleSettings where
  rnf LineChartLineStyleSettings' {..} =
    Prelude.rnf lineInterpolation `Prelude.seq`
      Prelude.rnf lineStyle `Prelude.seq`
        Prelude.rnf lineVisibility `Prelude.seq`
          Prelude.rnf lineWidth

instance Data.ToJSON LineChartLineStyleSettings where
  toJSON LineChartLineStyleSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LineInterpolation" Data..=)
              Prelude.<$> lineInterpolation,
            ("LineStyle" Data..=) Prelude.<$> lineStyle,
            ("LineVisibility" Data..=)
              Prelude.<$> lineVisibility,
            ("LineWidth" Data..=) Prelude.<$> lineWidth
          ]
      )

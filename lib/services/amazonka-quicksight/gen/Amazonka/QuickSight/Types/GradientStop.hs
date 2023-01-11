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
-- Module      : Amazonka.QuickSight.Types.GradientStop
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.GradientStop where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Determines the gradient stop configuration.
--
-- /See:/ 'newGradientStop' smart constructor.
data GradientStop = GradientStop'
  { -- | Determines the color.
    color :: Prelude.Maybe Prelude.Text,
    -- | Determines the data value.
    dataValue :: Prelude.Maybe Prelude.Double,
    -- | Determines gradient offset value.
    gradientOffset :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GradientStop' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'color', 'gradientStop_color' - Determines the color.
--
-- 'dataValue', 'gradientStop_dataValue' - Determines the data value.
--
-- 'gradientOffset', 'gradientStop_gradientOffset' - Determines gradient offset value.
newGradientStop ::
  -- | 'gradientOffset'
  Prelude.Double ->
  GradientStop
newGradientStop pGradientOffset_ =
  GradientStop'
    { color = Prelude.Nothing,
      dataValue = Prelude.Nothing,
      gradientOffset = pGradientOffset_
    }

-- | Determines the color.
gradientStop_color :: Lens.Lens' GradientStop (Prelude.Maybe Prelude.Text)
gradientStop_color = Lens.lens (\GradientStop' {color} -> color) (\s@GradientStop' {} a -> s {color = a} :: GradientStop)

-- | Determines the data value.
gradientStop_dataValue :: Lens.Lens' GradientStop (Prelude.Maybe Prelude.Double)
gradientStop_dataValue = Lens.lens (\GradientStop' {dataValue} -> dataValue) (\s@GradientStop' {} a -> s {dataValue = a} :: GradientStop)

-- | Determines gradient offset value.
gradientStop_gradientOffset :: Lens.Lens' GradientStop Prelude.Double
gradientStop_gradientOffset = Lens.lens (\GradientStop' {gradientOffset} -> gradientOffset) (\s@GradientStop' {} a -> s {gradientOffset = a} :: GradientStop)

instance Data.FromJSON GradientStop where
  parseJSON =
    Data.withObject
      "GradientStop"
      ( \x ->
          GradientStop'
            Prelude.<$> (x Data..:? "Color")
            Prelude.<*> (x Data..:? "DataValue")
            Prelude.<*> (x Data..: "GradientOffset")
      )

instance Prelude.Hashable GradientStop where
  hashWithSalt _salt GradientStop' {..} =
    _salt `Prelude.hashWithSalt` color
      `Prelude.hashWithSalt` dataValue
      `Prelude.hashWithSalt` gradientOffset

instance Prelude.NFData GradientStop where
  rnf GradientStop' {..} =
    Prelude.rnf color
      `Prelude.seq` Prelude.rnf dataValue
      `Prelude.seq` Prelude.rnf gradientOffset

instance Data.ToJSON GradientStop where
  toJSON GradientStop' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Color" Data..=) Prelude.<$> color,
            ("DataValue" Data..=) Prelude.<$> dataValue,
            Prelude.Just
              ("GradientOffset" Data..= gradientOffset)
          ]
      )

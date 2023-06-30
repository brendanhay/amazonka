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
-- Module      : Amazonka.QuickSight.Types.DataColor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DataColor where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Determines the color that is applied to a particular data value.
--
-- /See:/ 'newDataColor' smart constructor.
data DataColor = DataColor'
  { -- | The color that is applied to the data value.
    color :: Prelude.Maybe Prelude.Text,
    -- | The data value that the color is applied to.
    dataValue :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataColor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'color', 'dataColor_color' - The color that is applied to the data value.
--
-- 'dataValue', 'dataColor_dataValue' - The data value that the color is applied to.
newDataColor ::
  DataColor
newDataColor =
  DataColor'
    { color = Prelude.Nothing,
      dataValue = Prelude.Nothing
    }

-- | The color that is applied to the data value.
dataColor_color :: Lens.Lens' DataColor (Prelude.Maybe Prelude.Text)
dataColor_color = Lens.lens (\DataColor' {color} -> color) (\s@DataColor' {} a -> s {color = a} :: DataColor)

-- | The data value that the color is applied to.
dataColor_dataValue :: Lens.Lens' DataColor (Prelude.Maybe Prelude.Double)
dataColor_dataValue = Lens.lens (\DataColor' {dataValue} -> dataValue) (\s@DataColor' {} a -> s {dataValue = a} :: DataColor)

instance Data.FromJSON DataColor where
  parseJSON =
    Data.withObject
      "DataColor"
      ( \x ->
          DataColor'
            Prelude.<$> (x Data..:? "Color")
            Prelude.<*> (x Data..:? "DataValue")
      )

instance Prelude.Hashable DataColor where
  hashWithSalt _salt DataColor' {..} =
    _salt
      `Prelude.hashWithSalt` color
      `Prelude.hashWithSalt` dataValue

instance Prelude.NFData DataColor where
  rnf DataColor' {..} =
    Prelude.rnf color
      `Prelude.seq` Prelude.rnf dataValue

instance Data.ToJSON DataColor where
  toJSON DataColor' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Color" Data..=) Prelude.<$> color,
            ("DataValue" Data..=) Prelude.<$> dataValue
          ]
      )

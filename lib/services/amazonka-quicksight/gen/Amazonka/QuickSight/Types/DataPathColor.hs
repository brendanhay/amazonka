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
-- Module      : Amazonka.QuickSight.Types.DataPathColor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DataPathColor where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DataPathValue
import Amazonka.QuickSight.Types.TimeGranularity

-- | The color map that determines the color options for a particular
-- element.
--
-- /See:/ 'newDataPathColor' smart constructor.
data DataPathColor = DataPathColor'
  { -- | The time granularity of the field that the color needs to be applied to.
    timeGranularity :: Prelude.Maybe TimeGranularity,
    -- | The element that the color needs to be applied to.
    element :: DataPathValue,
    -- | The color that needs to be applied to the element.
    color :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataPathColor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeGranularity', 'dataPathColor_timeGranularity' - The time granularity of the field that the color needs to be applied to.
--
-- 'element', 'dataPathColor_element' - The element that the color needs to be applied to.
--
-- 'color', 'dataPathColor_color' - The color that needs to be applied to the element.
newDataPathColor ::
  -- | 'element'
  DataPathValue ->
  -- | 'color'
  Prelude.Text ->
  DataPathColor
newDataPathColor pElement_ pColor_ =
  DataPathColor'
    { timeGranularity = Prelude.Nothing,
      element = pElement_,
      color = pColor_
    }

-- | The time granularity of the field that the color needs to be applied to.
dataPathColor_timeGranularity :: Lens.Lens' DataPathColor (Prelude.Maybe TimeGranularity)
dataPathColor_timeGranularity = Lens.lens (\DataPathColor' {timeGranularity} -> timeGranularity) (\s@DataPathColor' {} a -> s {timeGranularity = a} :: DataPathColor)

-- | The element that the color needs to be applied to.
dataPathColor_element :: Lens.Lens' DataPathColor DataPathValue
dataPathColor_element = Lens.lens (\DataPathColor' {element} -> element) (\s@DataPathColor' {} a -> s {element = a} :: DataPathColor)

-- | The color that needs to be applied to the element.
dataPathColor_color :: Lens.Lens' DataPathColor Prelude.Text
dataPathColor_color = Lens.lens (\DataPathColor' {color} -> color) (\s@DataPathColor' {} a -> s {color = a} :: DataPathColor)

instance Data.FromJSON DataPathColor where
  parseJSON =
    Data.withObject
      "DataPathColor"
      ( \x ->
          DataPathColor'
            Prelude.<$> (x Data..:? "TimeGranularity")
            Prelude.<*> (x Data..: "Element")
            Prelude.<*> (x Data..: "Color")
      )

instance Prelude.Hashable DataPathColor where
  hashWithSalt _salt DataPathColor' {..} =
    _salt
      `Prelude.hashWithSalt` timeGranularity
      `Prelude.hashWithSalt` element
      `Prelude.hashWithSalt` color

instance Prelude.NFData DataPathColor where
  rnf DataPathColor' {..} =
    Prelude.rnf timeGranularity
      `Prelude.seq` Prelude.rnf element
      `Prelude.seq` Prelude.rnf color

instance Data.ToJSON DataPathColor where
  toJSON DataPathColor' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TimeGranularity" Data..=)
              Prelude.<$> timeGranularity,
            Prelude.Just ("Element" Data..= element),
            Prelude.Just ("Color" Data..= color)
          ]
      )

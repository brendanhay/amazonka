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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.ViewOffNadirInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.ViewOffNadirInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The input structure for specifying ViewOffNadir property filter.
-- ViewOffNadir refers to the angle from the sensor between nadir (straight
-- down) and the scene center. Measured in degrees (0-90).
--
-- /See:/ 'newViewOffNadirInput' smart constructor.
data ViewOffNadirInput = ViewOffNadirInput'
  { -- | The minimum value for ViewOffNadir property filter. This filters items
    -- having ViewOffNadir greater than or equal to this value.
    lowerBound :: Prelude.Double,
    -- | The maximum value for ViewOffNadir property filter. This filters items
    -- having ViewOffNadir lesser than or equal to this value.
    upperBound :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ViewOffNadirInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lowerBound', 'viewOffNadirInput_lowerBound' - The minimum value for ViewOffNadir property filter. This filters items
-- having ViewOffNadir greater than or equal to this value.
--
-- 'upperBound', 'viewOffNadirInput_upperBound' - The maximum value for ViewOffNadir property filter. This filters items
-- having ViewOffNadir lesser than or equal to this value.
newViewOffNadirInput ::
  -- | 'lowerBound'
  Prelude.Double ->
  -- | 'upperBound'
  Prelude.Double ->
  ViewOffNadirInput
newViewOffNadirInput pLowerBound_ pUpperBound_ =
  ViewOffNadirInput'
    { lowerBound = pLowerBound_,
      upperBound = pUpperBound_
    }

-- | The minimum value for ViewOffNadir property filter. This filters items
-- having ViewOffNadir greater than or equal to this value.
viewOffNadirInput_lowerBound :: Lens.Lens' ViewOffNadirInput Prelude.Double
viewOffNadirInput_lowerBound = Lens.lens (\ViewOffNadirInput' {lowerBound} -> lowerBound) (\s@ViewOffNadirInput' {} a -> s {lowerBound = a} :: ViewOffNadirInput)

-- | The maximum value for ViewOffNadir property filter. This filters items
-- having ViewOffNadir lesser than or equal to this value.
viewOffNadirInput_upperBound :: Lens.Lens' ViewOffNadirInput Prelude.Double
viewOffNadirInput_upperBound = Lens.lens (\ViewOffNadirInput' {upperBound} -> upperBound) (\s@ViewOffNadirInput' {} a -> s {upperBound = a} :: ViewOffNadirInput)

instance Data.FromJSON ViewOffNadirInput where
  parseJSON =
    Data.withObject
      "ViewOffNadirInput"
      ( \x ->
          ViewOffNadirInput'
            Prelude.<$> (x Data..: "LowerBound")
            Prelude.<*> (x Data..: "UpperBound")
      )

instance Prelude.Hashable ViewOffNadirInput where
  hashWithSalt _salt ViewOffNadirInput' {..} =
    _salt
      `Prelude.hashWithSalt` lowerBound
      `Prelude.hashWithSalt` upperBound

instance Prelude.NFData ViewOffNadirInput where
  rnf ViewOffNadirInput' {..} =
    Prelude.rnf lowerBound
      `Prelude.seq` Prelude.rnf upperBound

instance Data.ToJSON ViewOffNadirInput where
  toJSON ViewOffNadirInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("LowerBound" Data..= lowerBound),
            Prelude.Just ("UpperBound" Data..= upperBound)
          ]
      )

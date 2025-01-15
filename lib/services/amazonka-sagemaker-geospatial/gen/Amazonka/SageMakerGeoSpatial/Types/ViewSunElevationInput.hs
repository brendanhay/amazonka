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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.ViewSunElevationInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.ViewSunElevationInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newViewSunElevationInput' smart constructor.
data ViewSunElevationInput = ViewSunElevationInput'
  { -- | The lower bound to view the sun elevation.
    lowerBound :: Prelude.Double,
    -- | The upper bound to view the sun elevation.
    upperBound :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ViewSunElevationInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lowerBound', 'viewSunElevationInput_lowerBound' - The lower bound to view the sun elevation.
--
-- 'upperBound', 'viewSunElevationInput_upperBound' - The upper bound to view the sun elevation.
newViewSunElevationInput ::
  -- | 'lowerBound'
  Prelude.Double ->
  -- | 'upperBound'
  Prelude.Double ->
  ViewSunElevationInput
newViewSunElevationInput pLowerBound_ pUpperBound_ =
  ViewSunElevationInput'
    { lowerBound = pLowerBound_,
      upperBound = pUpperBound_
    }

-- | The lower bound to view the sun elevation.
viewSunElevationInput_lowerBound :: Lens.Lens' ViewSunElevationInput Prelude.Double
viewSunElevationInput_lowerBound = Lens.lens (\ViewSunElevationInput' {lowerBound} -> lowerBound) (\s@ViewSunElevationInput' {} a -> s {lowerBound = a} :: ViewSunElevationInput)

-- | The upper bound to view the sun elevation.
viewSunElevationInput_upperBound :: Lens.Lens' ViewSunElevationInput Prelude.Double
viewSunElevationInput_upperBound = Lens.lens (\ViewSunElevationInput' {upperBound} -> upperBound) (\s@ViewSunElevationInput' {} a -> s {upperBound = a} :: ViewSunElevationInput)

instance Data.FromJSON ViewSunElevationInput where
  parseJSON =
    Data.withObject
      "ViewSunElevationInput"
      ( \x ->
          ViewSunElevationInput'
            Prelude.<$> (x Data..: "LowerBound")
            Prelude.<*> (x Data..: "UpperBound")
      )

instance Prelude.Hashable ViewSunElevationInput where
  hashWithSalt _salt ViewSunElevationInput' {..} =
    _salt
      `Prelude.hashWithSalt` lowerBound
      `Prelude.hashWithSalt` upperBound

instance Prelude.NFData ViewSunElevationInput where
  rnf ViewSunElevationInput' {..} =
    Prelude.rnf lowerBound `Prelude.seq`
      Prelude.rnf upperBound

instance Data.ToJSON ViewSunElevationInput where
  toJSON ViewSunElevationInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("LowerBound" Data..= lowerBound),
            Prelude.Just ("UpperBound" Data..= upperBound)
          ]
      )

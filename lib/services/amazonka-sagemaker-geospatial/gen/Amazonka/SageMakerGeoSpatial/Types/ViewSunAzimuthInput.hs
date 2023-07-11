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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.ViewSunAzimuthInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.ViewSunAzimuthInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newViewSunAzimuthInput' smart constructor.
data ViewSunAzimuthInput = ViewSunAzimuthInput'
  { lowerBound :: Prelude.Double,
    upperBound :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ViewSunAzimuthInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lowerBound', 'viewSunAzimuthInput_lowerBound' -
--
-- 'upperBound', 'viewSunAzimuthInput_upperBound' -
newViewSunAzimuthInput ::
  -- | 'lowerBound'
  Prelude.Double ->
  -- | 'upperBound'
  Prelude.Double ->
  ViewSunAzimuthInput
newViewSunAzimuthInput pLowerBound_ pUpperBound_ =
  ViewSunAzimuthInput'
    { lowerBound = pLowerBound_,
      upperBound = pUpperBound_
    }

viewSunAzimuthInput_lowerBound :: Lens.Lens' ViewSunAzimuthInput Prelude.Double
viewSunAzimuthInput_lowerBound = Lens.lens (\ViewSunAzimuthInput' {lowerBound} -> lowerBound) (\s@ViewSunAzimuthInput' {} a -> s {lowerBound = a} :: ViewSunAzimuthInput)

viewSunAzimuthInput_upperBound :: Lens.Lens' ViewSunAzimuthInput Prelude.Double
viewSunAzimuthInput_upperBound = Lens.lens (\ViewSunAzimuthInput' {upperBound} -> upperBound) (\s@ViewSunAzimuthInput' {} a -> s {upperBound = a} :: ViewSunAzimuthInput)

instance Data.FromJSON ViewSunAzimuthInput where
  parseJSON =
    Data.withObject
      "ViewSunAzimuthInput"
      ( \x ->
          ViewSunAzimuthInput'
            Prelude.<$> (x Data..: "LowerBound")
            Prelude.<*> (x Data..: "UpperBound")
      )

instance Prelude.Hashable ViewSunAzimuthInput where
  hashWithSalt _salt ViewSunAzimuthInput' {..} =
    _salt
      `Prelude.hashWithSalt` lowerBound
      `Prelude.hashWithSalt` upperBound

instance Prelude.NFData ViewSunAzimuthInput where
  rnf ViewSunAzimuthInput' {..} =
    Prelude.rnf lowerBound
      `Prelude.seq` Prelude.rnf upperBound

instance Data.ToJSON ViewSunAzimuthInput where
  toJSON ViewSunAzimuthInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("LowerBound" Data..= lowerBound),
            Prelude.Just ("UpperBound" Data..= upperBound)
          ]
      )

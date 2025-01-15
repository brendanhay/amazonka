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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.BandMathConfigInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.BandMathConfigInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.CustomIndicesInput

-- |
--
-- /See:/ 'newBandMathConfigInput' smart constructor.
data BandMathConfigInput = BandMathConfigInput'
  { customIndices :: Prelude.Maybe CustomIndicesInput,
    predefinedIndices :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BandMathConfigInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customIndices', 'bandMathConfigInput_customIndices' -
--
-- 'predefinedIndices', 'bandMathConfigInput_predefinedIndices' -
newBandMathConfigInput ::
  BandMathConfigInput
newBandMathConfigInput =
  BandMathConfigInput'
    { customIndices =
        Prelude.Nothing,
      predefinedIndices = Prelude.Nothing
    }

bandMathConfigInput_customIndices :: Lens.Lens' BandMathConfigInput (Prelude.Maybe CustomIndicesInput)
bandMathConfigInput_customIndices = Lens.lens (\BandMathConfigInput' {customIndices} -> customIndices) (\s@BandMathConfigInput' {} a -> s {customIndices = a} :: BandMathConfigInput)

bandMathConfigInput_predefinedIndices :: Lens.Lens' BandMathConfigInput (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
bandMathConfigInput_predefinedIndices = Lens.lens (\BandMathConfigInput' {predefinedIndices} -> predefinedIndices) (\s@BandMathConfigInput' {} a -> s {predefinedIndices = a} :: BandMathConfigInput) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON BandMathConfigInput where
  parseJSON =
    Data.withObject
      "BandMathConfigInput"
      ( \x ->
          BandMathConfigInput'
            Prelude.<$> (x Data..:? "CustomIndices")
            Prelude.<*> (x Data..:? "PredefinedIndices")
      )

instance Prelude.Hashable BandMathConfigInput where
  hashWithSalt _salt BandMathConfigInput' {..} =
    _salt
      `Prelude.hashWithSalt` customIndices
      `Prelude.hashWithSalt` predefinedIndices

instance Prelude.NFData BandMathConfigInput where
  rnf BandMathConfigInput' {..} =
    Prelude.rnf customIndices `Prelude.seq`
      Prelude.rnf predefinedIndices

instance Data.ToJSON BandMathConfigInput where
  toJSON BandMathConfigInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomIndices" Data..=) Prelude.<$> customIndices,
            ("PredefinedIndices" Data..=)
              Prelude.<$> predefinedIndices
          ]
      )

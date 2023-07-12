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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.ResamplingConfigInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.ResamplingConfigInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.AlgorithmNameResampling
import Amazonka.SageMakerGeoSpatial.Types.OutputResolutionResamplingInput

-- |
--
-- /See:/ 'newResamplingConfigInput' smart constructor.
data ResamplingConfigInput = ResamplingConfigInput'
  { -- | The name of the algorithm used for resampling.
    algorithmName :: Prelude.Maybe AlgorithmNameResampling,
    targetBands :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    outputResolution :: OutputResolutionResamplingInput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResamplingConfigInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'algorithmName', 'resamplingConfigInput_algorithmName' - The name of the algorithm used for resampling.
--
-- 'targetBands', 'resamplingConfigInput_targetBands' -
--
-- 'outputResolution', 'resamplingConfigInput_outputResolution' -
newResamplingConfigInput ::
  -- | 'outputResolution'
  OutputResolutionResamplingInput ->
  ResamplingConfigInput
newResamplingConfigInput pOutputResolution_ =
  ResamplingConfigInput'
    { algorithmName =
        Prelude.Nothing,
      targetBands = Prelude.Nothing,
      outputResolution = pOutputResolution_
    }

-- | The name of the algorithm used for resampling.
resamplingConfigInput_algorithmName :: Lens.Lens' ResamplingConfigInput (Prelude.Maybe AlgorithmNameResampling)
resamplingConfigInput_algorithmName = Lens.lens (\ResamplingConfigInput' {algorithmName} -> algorithmName) (\s@ResamplingConfigInput' {} a -> s {algorithmName = a} :: ResamplingConfigInput)

resamplingConfigInput_targetBands :: Lens.Lens' ResamplingConfigInput (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
resamplingConfigInput_targetBands = Lens.lens (\ResamplingConfigInput' {targetBands} -> targetBands) (\s@ResamplingConfigInput' {} a -> s {targetBands = a} :: ResamplingConfigInput) Prelude.. Lens.mapping Lens.coerced

resamplingConfigInput_outputResolution :: Lens.Lens' ResamplingConfigInput OutputResolutionResamplingInput
resamplingConfigInput_outputResolution = Lens.lens (\ResamplingConfigInput' {outputResolution} -> outputResolution) (\s@ResamplingConfigInput' {} a -> s {outputResolution = a} :: ResamplingConfigInput)

instance Data.FromJSON ResamplingConfigInput where
  parseJSON =
    Data.withObject
      "ResamplingConfigInput"
      ( \x ->
          ResamplingConfigInput'
            Prelude.<$> (x Data..:? "AlgorithmName")
            Prelude.<*> (x Data..:? "TargetBands")
            Prelude.<*> (x Data..: "OutputResolution")
      )

instance Prelude.Hashable ResamplingConfigInput where
  hashWithSalt _salt ResamplingConfigInput' {..} =
    _salt
      `Prelude.hashWithSalt` algorithmName
      `Prelude.hashWithSalt` targetBands
      `Prelude.hashWithSalt` outputResolution

instance Prelude.NFData ResamplingConfigInput where
  rnf ResamplingConfigInput' {..} =
    Prelude.rnf algorithmName
      `Prelude.seq` Prelude.rnf targetBands
      `Prelude.seq` Prelude.rnf outputResolution

instance Data.ToJSON ResamplingConfigInput where
  toJSON ResamplingConfigInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AlgorithmName" Data..=) Prelude.<$> algorithmName,
            ("TargetBands" Data..=) Prelude.<$> targetBands,
            Prelude.Just
              ("OutputResolution" Data..= outputResolution)
          ]
      )

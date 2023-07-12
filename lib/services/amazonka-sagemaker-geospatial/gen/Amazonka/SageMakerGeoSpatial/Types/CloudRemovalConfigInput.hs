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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.CloudRemovalConfigInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.CloudRemovalConfigInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.AlgorithmNameCloudRemoval

-- |
--
-- /See:/ 'newCloudRemovalConfigInput' smart constructor.
data CloudRemovalConfigInput = CloudRemovalConfigInput'
  { -- | The name of the algorithm used for cloud removal.
    algorithmName :: Prelude.Maybe AlgorithmNameCloudRemoval,
    -- | The interpolation value you provide for cloud removal.
    interpolationValue :: Prelude.Maybe Prelude.Text,
    targetBands :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudRemovalConfigInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'algorithmName', 'cloudRemovalConfigInput_algorithmName' - The name of the algorithm used for cloud removal.
--
-- 'interpolationValue', 'cloudRemovalConfigInput_interpolationValue' - The interpolation value you provide for cloud removal.
--
-- 'targetBands', 'cloudRemovalConfigInput_targetBands' -
newCloudRemovalConfigInput ::
  CloudRemovalConfigInput
newCloudRemovalConfigInput =
  CloudRemovalConfigInput'
    { algorithmName =
        Prelude.Nothing,
      interpolationValue = Prelude.Nothing,
      targetBands = Prelude.Nothing
    }

-- | The name of the algorithm used for cloud removal.
cloudRemovalConfigInput_algorithmName :: Lens.Lens' CloudRemovalConfigInput (Prelude.Maybe AlgorithmNameCloudRemoval)
cloudRemovalConfigInput_algorithmName = Lens.lens (\CloudRemovalConfigInput' {algorithmName} -> algorithmName) (\s@CloudRemovalConfigInput' {} a -> s {algorithmName = a} :: CloudRemovalConfigInput)

-- | The interpolation value you provide for cloud removal.
cloudRemovalConfigInput_interpolationValue :: Lens.Lens' CloudRemovalConfigInput (Prelude.Maybe Prelude.Text)
cloudRemovalConfigInput_interpolationValue = Lens.lens (\CloudRemovalConfigInput' {interpolationValue} -> interpolationValue) (\s@CloudRemovalConfigInput' {} a -> s {interpolationValue = a} :: CloudRemovalConfigInput)

cloudRemovalConfigInput_targetBands :: Lens.Lens' CloudRemovalConfigInput (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
cloudRemovalConfigInput_targetBands = Lens.lens (\CloudRemovalConfigInput' {targetBands} -> targetBands) (\s@CloudRemovalConfigInput' {} a -> s {targetBands = a} :: CloudRemovalConfigInput) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON CloudRemovalConfigInput where
  parseJSON =
    Data.withObject
      "CloudRemovalConfigInput"
      ( \x ->
          CloudRemovalConfigInput'
            Prelude.<$> (x Data..:? "AlgorithmName")
            Prelude.<*> (x Data..:? "InterpolationValue")
            Prelude.<*> (x Data..:? "TargetBands")
      )

instance Prelude.Hashable CloudRemovalConfigInput where
  hashWithSalt _salt CloudRemovalConfigInput' {..} =
    _salt
      `Prelude.hashWithSalt` algorithmName
      `Prelude.hashWithSalt` interpolationValue
      `Prelude.hashWithSalt` targetBands

instance Prelude.NFData CloudRemovalConfigInput where
  rnf CloudRemovalConfigInput' {..} =
    Prelude.rnf algorithmName
      `Prelude.seq` Prelude.rnf interpolationValue
      `Prelude.seq` Prelude.rnf targetBands

instance Data.ToJSON CloudRemovalConfigInput where
  toJSON CloudRemovalConfigInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AlgorithmName" Data..=) Prelude.<$> algorithmName,
            ("InterpolationValue" Data..=)
              Prelude.<$> interpolationValue,
            ("TargetBands" Data..=) Prelude.<$> targetBands
          ]
      )

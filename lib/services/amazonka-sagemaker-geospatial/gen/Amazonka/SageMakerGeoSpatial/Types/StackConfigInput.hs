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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.StackConfigInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.StackConfigInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.OutputResolutionStackInput

-- | The input structure for Stacking Operation.
--
-- /See:/ 'newStackConfigInput' smart constructor.
data StackConfigInput = StackConfigInput'
  { -- | The structure representing output resolution (in target georeferenced
    -- units) of the result of stacking operation.
    outputResolution :: Prelude.Maybe OutputResolutionStackInput,
    -- | A list of bands to be stacked in the specified order. When the parameter
    -- is not provided, all the available bands in the data collection are
    -- stacked in the alphabetical order of their asset names.
    targetBands :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StackConfigInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputResolution', 'stackConfigInput_outputResolution' - The structure representing output resolution (in target georeferenced
-- units) of the result of stacking operation.
--
-- 'targetBands', 'stackConfigInput_targetBands' - A list of bands to be stacked in the specified order. When the parameter
-- is not provided, all the available bands in the data collection are
-- stacked in the alphabetical order of their asset names.
newStackConfigInput ::
  StackConfigInput
newStackConfigInput =
  StackConfigInput'
    { outputResolution =
        Prelude.Nothing,
      targetBands = Prelude.Nothing
    }

-- | The structure representing output resolution (in target georeferenced
-- units) of the result of stacking operation.
stackConfigInput_outputResolution :: Lens.Lens' StackConfigInput (Prelude.Maybe OutputResolutionStackInput)
stackConfigInput_outputResolution = Lens.lens (\StackConfigInput' {outputResolution} -> outputResolution) (\s@StackConfigInput' {} a -> s {outputResolution = a} :: StackConfigInput)

-- | A list of bands to be stacked in the specified order. When the parameter
-- is not provided, all the available bands in the data collection are
-- stacked in the alphabetical order of their asset names.
stackConfigInput_targetBands :: Lens.Lens' StackConfigInput (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
stackConfigInput_targetBands = Lens.lens (\StackConfigInput' {targetBands} -> targetBands) (\s@StackConfigInput' {} a -> s {targetBands = a} :: StackConfigInput) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON StackConfigInput where
  parseJSON =
    Data.withObject
      "StackConfigInput"
      ( \x ->
          StackConfigInput'
            Prelude.<$> (x Data..:? "OutputResolution")
            Prelude.<*> (x Data..:? "TargetBands")
      )

instance Prelude.Hashable StackConfigInput where
  hashWithSalt _salt StackConfigInput' {..} =
    _salt
      `Prelude.hashWithSalt` outputResolution
      `Prelude.hashWithSalt` targetBands

instance Prelude.NFData StackConfigInput where
  rnf StackConfigInput' {..} =
    Prelude.rnf outputResolution
      `Prelude.seq` Prelude.rnf targetBands

instance Data.ToJSON StackConfigInput where
  toJSON StackConfigInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("OutputResolution" Data..=)
              Prelude.<$> outputResolution,
            ("TargetBands" Data..=) Prelude.<$> targetBands
          ]
      )

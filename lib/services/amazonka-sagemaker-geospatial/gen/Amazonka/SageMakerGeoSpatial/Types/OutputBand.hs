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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.OutputBand
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.OutputBand where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.OutputType

-- | A single EarthObservationJob output band.
--
-- /See:/ 'newOutputBand' smart constructor.
data OutputBand = OutputBand'
  { -- | The name of the band.
    bandName :: Prelude.Text,
    -- | The datatype of the output band.
    outputDataType :: OutputType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputBand' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bandName', 'outputBand_bandName' - The name of the band.
--
-- 'outputDataType', 'outputBand_outputDataType' - The datatype of the output band.
newOutputBand ::
  -- | 'bandName'
  Prelude.Text ->
  -- | 'outputDataType'
  OutputType ->
  OutputBand
newOutputBand pBandName_ pOutputDataType_ =
  OutputBand'
    { bandName = pBandName_,
      outputDataType = pOutputDataType_
    }

-- | The name of the band.
outputBand_bandName :: Lens.Lens' OutputBand Prelude.Text
outputBand_bandName = Lens.lens (\OutputBand' {bandName} -> bandName) (\s@OutputBand' {} a -> s {bandName = a} :: OutputBand)

-- | The datatype of the output band.
outputBand_outputDataType :: Lens.Lens' OutputBand OutputType
outputBand_outputDataType = Lens.lens (\OutputBand' {outputDataType} -> outputDataType) (\s@OutputBand' {} a -> s {outputDataType = a} :: OutputBand)

instance Data.FromJSON OutputBand where
  parseJSON =
    Data.withObject
      "OutputBand"
      ( \x ->
          OutputBand'
            Prelude.<$> (x Data..: "BandName")
            Prelude.<*> (x Data..: "OutputDataType")
      )

instance Prelude.Hashable OutputBand where
  hashWithSalt _salt OutputBand' {..} =
    _salt
      `Prelude.hashWithSalt` bandName
      `Prelude.hashWithSalt` outputDataType

instance Prelude.NFData OutputBand where
  rnf OutputBand' {..} =
    Prelude.rnf bandName
      `Prelude.seq` Prelude.rnf outputDataType

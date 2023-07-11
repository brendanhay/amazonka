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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.OutputConfigInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.OutputConfigInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.ExportS3DataInput

-- | The response structure for an OutputConfig returned by an
-- ExportEarthObservationJob.
--
-- /See:/ 'newOutputConfigInput' smart constructor.
data OutputConfigInput = OutputConfigInput'
  { -- | Path to Amazon S3 storage location for the output configuration file.
    s3Data :: ExportS3DataInput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputConfigInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Data', 'outputConfigInput_s3Data' - Path to Amazon S3 storage location for the output configuration file.
newOutputConfigInput ::
  -- | 's3Data'
  ExportS3DataInput ->
  OutputConfigInput
newOutputConfigInput pS3Data_ =
  OutputConfigInput' {s3Data = pS3Data_}

-- | Path to Amazon S3 storage location for the output configuration file.
outputConfigInput_s3Data :: Lens.Lens' OutputConfigInput ExportS3DataInput
outputConfigInput_s3Data = Lens.lens (\OutputConfigInput' {s3Data} -> s3Data) (\s@OutputConfigInput' {} a -> s {s3Data = a} :: OutputConfigInput)

instance Data.FromJSON OutputConfigInput where
  parseJSON =
    Data.withObject
      "OutputConfigInput"
      ( \x ->
          OutputConfigInput' Prelude.<$> (x Data..: "S3Data")
      )

instance Prelude.Hashable OutputConfigInput where
  hashWithSalt _salt OutputConfigInput' {..} =
    _salt `Prelude.hashWithSalt` s3Data

instance Prelude.NFData OutputConfigInput where
  rnf OutputConfigInput' {..} = Prelude.rnf s3Data

instance Data.ToJSON OutputConfigInput where
  toJSON OutputConfigInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("S3Data" Data..= s3Data)]
      )

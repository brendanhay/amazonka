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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.ExportVectorEnrichmentJobOutputConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.ExportVectorEnrichmentJobOutputConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobS3Data

-- | An object containing information about the output file.
--
-- /See:/ 'newExportVectorEnrichmentJobOutputConfig' smart constructor.
data ExportVectorEnrichmentJobOutputConfig = ExportVectorEnrichmentJobOutputConfig'
  { s3Data :: VectorEnrichmentJobS3Data
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportVectorEnrichmentJobOutputConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Data', 'exportVectorEnrichmentJobOutputConfig_s3Data' -
newExportVectorEnrichmentJobOutputConfig ::
  -- | 's3Data'
  VectorEnrichmentJobS3Data ->
  ExportVectorEnrichmentJobOutputConfig
newExportVectorEnrichmentJobOutputConfig pS3Data_ =
  ExportVectorEnrichmentJobOutputConfig'
    { s3Data =
        pS3Data_
    }

exportVectorEnrichmentJobOutputConfig_s3Data :: Lens.Lens' ExportVectorEnrichmentJobOutputConfig VectorEnrichmentJobS3Data
exportVectorEnrichmentJobOutputConfig_s3Data = Lens.lens (\ExportVectorEnrichmentJobOutputConfig' {s3Data} -> s3Data) (\s@ExportVectorEnrichmentJobOutputConfig' {} a -> s {s3Data = a} :: ExportVectorEnrichmentJobOutputConfig)

instance
  Data.FromJSON
    ExportVectorEnrichmentJobOutputConfig
  where
  parseJSON =
    Data.withObject
      "ExportVectorEnrichmentJobOutputConfig"
      ( \x ->
          ExportVectorEnrichmentJobOutputConfig'
            Prelude.<$> (x Data..: "S3Data")
      )

instance
  Prelude.Hashable
    ExportVectorEnrichmentJobOutputConfig
  where
  hashWithSalt
    _salt
    ExportVectorEnrichmentJobOutputConfig' {..} =
      _salt `Prelude.hashWithSalt` s3Data

instance
  Prelude.NFData
    ExportVectorEnrichmentJobOutputConfig
  where
  rnf ExportVectorEnrichmentJobOutputConfig' {..} =
    Prelude.rnf s3Data

instance
  Data.ToJSON
    ExportVectorEnrichmentJobOutputConfig
  where
  toJSON ExportVectorEnrichmentJobOutputConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("S3Data" Data..= s3Data)]
      )

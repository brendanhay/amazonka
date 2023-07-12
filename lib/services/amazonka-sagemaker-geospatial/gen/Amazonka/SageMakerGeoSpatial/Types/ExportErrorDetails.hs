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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.ExportErrorDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.ExportErrorDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.ExportErrorDetailsOutput

-- | The structure for returning the export error details in a
-- GetEarthObservationJob.
--
-- /See:/ 'newExportErrorDetails' smart constructor.
data ExportErrorDetails = ExportErrorDetails'
  { exportResults :: Prelude.Maybe ExportErrorDetailsOutput,
    exportSourceImages :: Prelude.Maybe ExportErrorDetailsOutput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportErrorDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportResults', 'exportErrorDetails_exportResults' -
--
-- 'exportSourceImages', 'exportErrorDetails_exportSourceImages' -
newExportErrorDetails ::
  ExportErrorDetails
newExportErrorDetails =
  ExportErrorDetails'
    { exportResults =
        Prelude.Nothing,
      exportSourceImages = Prelude.Nothing
    }

exportErrorDetails_exportResults :: Lens.Lens' ExportErrorDetails (Prelude.Maybe ExportErrorDetailsOutput)
exportErrorDetails_exportResults = Lens.lens (\ExportErrorDetails' {exportResults} -> exportResults) (\s@ExportErrorDetails' {} a -> s {exportResults = a} :: ExportErrorDetails)

exportErrorDetails_exportSourceImages :: Lens.Lens' ExportErrorDetails (Prelude.Maybe ExportErrorDetailsOutput)
exportErrorDetails_exportSourceImages = Lens.lens (\ExportErrorDetails' {exportSourceImages} -> exportSourceImages) (\s@ExportErrorDetails' {} a -> s {exportSourceImages = a} :: ExportErrorDetails)

instance Data.FromJSON ExportErrorDetails where
  parseJSON =
    Data.withObject
      "ExportErrorDetails"
      ( \x ->
          ExportErrorDetails'
            Prelude.<$> (x Data..:? "ExportResults")
            Prelude.<*> (x Data..:? "ExportSourceImages")
      )

instance Prelude.Hashable ExportErrorDetails where
  hashWithSalt _salt ExportErrorDetails' {..} =
    _salt
      `Prelude.hashWithSalt` exportResults
      `Prelude.hashWithSalt` exportSourceImages

instance Prelude.NFData ExportErrorDetails where
  rnf ExportErrorDetails' {..} =
    Prelude.rnf exportResults
      `Prelude.seq` Prelude.rnf exportSourceImages

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
-- Module      : Amazonka.CustomerProfiles.Types.ExportingLocation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.ExportingLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types.S3ExportingLocation
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The S3 location where Identity Resolution Jobs write result files.
--
-- /See:/ 'newExportingLocation' smart constructor.
data ExportingLocation = ExportingLocation'
  { -- | Information about the S3 location where Identity Resolution Jobs write
    -- result files.
    s3Exporting :: Prelude.Maybe S3ExportingLocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportingLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Exporting', 'exportingLocation_s3Exporting' - Information about the S3 location where Identity Resolution Jobs write
-- result files.
newExportingLocation ::
  ExportingLocation
newExportingLocation =
  ExportingLocation' {s3Exporting = Prelude.Nothing}

-- | Information about the S3 location where Identity Resolution Jobs write
-- result files.
exportingLocation_s3Exporting :: Lens.Lens' ExportingLocation (Prelude.Maybe S3ExportingLocation)
exportingLocation_s3Exporting = Lens.lens (\ExportingLocation' {s3Exporting} -> s3Exporting) (\s@ExportingLocation' {} a -> s {s3Exporting = a} :: ExportingLocation)

instance Data.FromJSON ExportingLocation where
  parseJSON =
    Data.withObject
      "ExportingLocation"
      ( \x ->
          ExportingLocation'
            Prelude.<$> (x Data..:? "S3Exporting")
      )

instance Prelude.Hashable ExportingLocation where
  hashWithSalt _salt ExportingLocation' {..} =
    _salt `Prelude.hashWithSalt` s3Exporting

instance Prelude.NFData ExportingLocation where
  rnf ExportingLocation' {..} = Prelude.rnf s3Exporting

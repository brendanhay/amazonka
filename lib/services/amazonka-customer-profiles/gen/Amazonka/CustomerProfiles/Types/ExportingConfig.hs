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
-- Module      : Amazonka.CustomerProfiles.Types.ExportingConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.ExportingConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types.S3ExportingConfig
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration information about the S3 bucket where Identity Resolution
-- Jobs writes result files.
--
-- You need to give Customer Profiles service principal write permission to
-- your S3 bucket. Otherwise, you\'ll get an exception in the API response.
-- For an example policy, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/cross-service-confused-deputy-prevention.html#customer-profiles-cross-service Amazon Connect Customer Profiles cross-service confused deputy prevention>.
--
-- /See:/ 'newExportingConfig' smart constructor.
data ExportingConfig = ExportingConfig'
  { -- | The S3 location where Identity Resolution Jobs write result files.
    s3Exporting :: Prelude.Maybe S3ExportingConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportingConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Exporting', 'exportingConfig_s3Exporting' - The S3 location where Identity Resolution Jobs write result files.
newExportingConfig ::
  ExportingConfig
newExportingConfig =
  ExportingConfig' {s3Exporting = Prelude.Nothing}

-- | The S3 location where Identity Resolution Jobs write result files.
exportingConfig_s3Exporting :: Lens.Lens' ExportingConfig (Prelude.Maybe S3ExportingConfig)
exportingConfig_s3Exporting = Lens.lens (\ExportingConfig' {s3Exporting} -> s3Exporting) (\s@ExportingConfig' {} a -> s {s3Exporting = a} :: ExportingConfig)

instance Data.FromJSON ExportingConfig where
  parseJSON =
    Data.withObject
      "ExportingConfig"
      ( \x ->
          ExportingConfig'
            Prelude.<$> (x Data..:? "S3Exporting")
      )

instance Prelude.Hashable ExportingConfig where
  hashWithSalt _salt ExportingConfig' {..} =
    _salt `Prelude.hashWithSalt` s3Exporting

instance Prelude.NFData ExportingConfig where
  rnf ExportingConfig' {..} = Prelude.rnf s3Exporting

instance Data.ToJSON ExportingConfig where
  toJSON ExportingConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [("S3Exporting" Data..=) Prelude.<$> s3Exporting]
      )

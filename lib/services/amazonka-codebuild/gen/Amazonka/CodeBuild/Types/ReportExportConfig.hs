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
-- Module      : Amazonka.CodeBuild.Types.ReportExportConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.ReportExportConfig where

import Amazonka.CodeBuild.Types.ReportExportConfigType
import Amazonka.CodeBuild.Types.S3ReportExportConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the location where the run of a report is exported.
--
-- /See:/ 'newReportExportConfig' smart constructor.
data ReportExportConfig = ReportExportConfig'
  { -- | The export configuration type. Valid values are:
    --
    -- -   @S3@: The report results are exported to an S3 bucket.
    --
    -- -   @NO_EXPORT@: The report results are not exported.
    exportConfigType :: Prelude.Maybe ReportExportConfigType,
    -- | A @S3ReportExportConfig@ object that contains information about the S3
    -- bucket where the run of a report is exported.
    s3Destination :: Prelude.Maybe S3ReportExportConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReportExportConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportConfigType', 'reportExportConfig_exportConfigType' - The export configuration type. Valid values are:
--
-- -   @S3@: The report results are exported to an S3 bucket.
--
-- -   @NO_EXPORT@: The report results are not exported.
--
-- 's3Destination', 'reportExportConfig_s3Destination' - A @S3ReportExportConfig@ object that contains information about the S3
-- bucket where the run of a report is exported.
newReportExportConfig ::
  ReportExportConfig
newReportExportConfig =
  ReportExportConfig'
    { exportConfigType =
        Prelude.Nothing,
      s3Destination = Prelude.Nothing
    }

-- | The export configuration type. Valid values are:
--
-- -   @S3@: The report results are exported to an S3 bucket.
--
-- -   @NO_EXPORT@: The report results are not exported.
reportExportConfig_exportConfigType :: Lens.Lens' ReportExportConfig (Prelude.Maybe ReportExportConfigType)
reportExportConfig_exportConfigType = Lens.lens (\ReportExportConfig' {exportConfigType} -> exportConfigType) (\s@ReportExportConfig' {} a -> s {exportConfigType = a} :: ReportExportConfig)

-- | A @S3ReportExportConfig@ object that contains information about the S3
-- bucket where the run of a report is exported.
reportExportConfig_s3Destination :: Lens.Lens' ReportExportConfig (Prelude.Maybe S3ReportExportConfig)
reportExportConfig_s3Destination = Lens.lens (\ReportExportConfig' {s3Destination} -> s3Destination) (\s@ReportExportConfig' {} a -> s {s3Destination = a} :: ReportExportConfig)

instance Data.FromJSON ReportExportConfig where
  parseJSON =
    Data.withObject
      "ReportExportConfig"
      ( \x ->
          ReportExportConfig'
            Prelude.<$> (x Data..:? "exportConfigType")
            Prelude.<*> (x Data..:? "s3Destination")
      )

instance Prelude.Hashable ReportExportConfig where
  hashWithSalt _salt ReportExportConfig' {..} =
    _salt
      `Prelude.hashWithSalt` exportConfigType
      `Prelude.hashWithSalt` s3Destination

instance Prelude.NFData ReportExportConfig where
  rnf ReportExportConfig' {..} =
    Prelude.rnf exportConfigType
      `Prelude.seq` Prelude.rnf s3Destination

instance Data.ToJSON ReportExportConfig where
  toJSON ReportExportConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("exportConfigType" Data..=)
              Prelude.<$> exportConfigType,
            ("s3Destination" Data..=) Prelude.<$> s3Destination
          ]
      )

{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CodeBuild.Types.ReportExportConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ReportExportConfig where

import Network.AWS.CodeBuild.Types.ReportExportConfigType
import Network.AWS.CodeBuild.Types.S3ReportExportConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the location where the run of a report is exported.
--
-- /See:/ 'newReportExportConfig' smart constructor.
data ReportExportConfig = ReportExportConfig'
  { -- | A @S3ReportExportConfig@ object that contains information about the S3
    -- bucket where the run of a report is exported.
    s3Destination :: Prelude.Maybe S3ReportExportConfig,
    -- | The export configuration type. Valid values are:
    --
    -- -   @S3@: The report results are exported to an S3 bucket.
    --
    -- -   @NO_EXPORT@: The report results are not exported.
    exportConfigType :: Prelude.Maybe ReportExportConfigType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReportExportConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Destination', 'reportExportConfig_s3Destination' - A @S3ReportExportConfig@ object that contains information about the S3
-- bucket where the run of a report is exported.
--
-- 'exportConfigType', 'reportExportConfig_exportConfigType' - The export configuration type. Valid values are:
--
-- -   @S3@: The report results are exported to an S3 bucket.
--
-- -   @NO_EXPORT@: The report results are not exported.
newReportExportConfig ::
  ReportExportConfig
newReportExportConfig =
  ReportExportConfig'
    { s3Destination =
        Prelude.Nothing,
      exportConfigType = Prelude.Nothing
    }

-- | A @S3ReportExportConfig@ object that contains information about the S3
-- bucket where the run of a report is exported.
reportExportConfig_s3Destination :: Lens.Lens' ReportExportConfig (Prelude.Maybe S3ReportExportConfig)
reportExportConfig_s3Destination = Lens.lens (\ReportExportConfig' {s3Destination} -> s3Destination) (\s@ReportExportConfig' {} a -> s {s3Destination = a} :: ReportExportConfig)

-- | The export configuration type. Valid values are:
--
-- -   @S3@: The report results are exported to an S3 bucket.
--
-- -   @NO_EXPORT@: The report results are not exported.
reportExportConfig_exportConfigType :: Lens.Lens' ReportExportConfig (Prelude.Maybe ReportExportConfigType)
reportExportConfig_exportConfigType = Lens.lens (\ReportExportConfig' {exportConfigType} -> exportConfigType) (\s@ReportExportConfig' {} a -> s {exportConfigType = a} :: ReportExportConfig)

instance Prelude.FromJSON ReportExportConfig where
  parseJSON =
    Prelude.withObject
      "ReportExportConfig"
      ( \x ->
          ReportExportConfig'
            Prelude.<$> (x Prelude..:? "s3Destination")
            Prelude.<*> (x Prelude..:? "exportConfigType")
      )

instance Prelude.Hashable ReportExportConfig

instance Prelude.NFData ReportExportConfig

instance Prelude.ToJSON ReportExportConfig where
  toJSON ReportExportConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("s3Destination" Prelude..=)
              Prelude.<$> s3Destination,
            ("exportConfigType" Prelude..=)
              Prelude.<$> exportConfigType
          ]
      )

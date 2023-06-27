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
-- Module      : Amazonka.QuickSight.Types.AssetBundleExportJobSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AssetBundleExportJobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AssetBundleExportFormat
import Amazonka.QuickSight.Types.AssetBundleExportJobStatus

-- | A summary of the export job that includes details of the job\'s
-- configuration and its current status.
--
-- /See:/ 'newAssetBundleExportJobSummary' smart constructor.
data AssetBundleExportJobSummary = AssetBundleExportJobSummary'
  { -- | The ARN of the export job.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the export job.
    assetBundleExportJobId :: Prelude.Maybe Prelude.Text,
    -- | The time that the export job was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The format for the export job.
    exportFormat :: Prelude.Maybe AssetBundleExportFormat,
    -- | The flag that determines the inclusion of resource dependencies in the
    -- returned asset bundle.
    includeAllDependencies :: Prelude.Maybe Prelude.Bool,
    -- | The current status of the export job.
    jobStatus :: Prelude.Maybe AssetBundleExportJobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetBundleExportJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'assetBundleExportJobSummary_arn' - The ARN of the export job.
--
-- 'assetBundleExportJobId', 'assetBundleExportJobSummary_assetBundleExportJobId' - The ID of the export job.
--
-- 'createdTime', 'assetBundleExportJobSummary_createdTime' - The time that the export job was created.
--
-- 'exportFormat', 'assetBundleExportJobSummary_exportFormat' - The format for the export job.
--
-- 'includeAllDependencies', 'assetBundleExportJobSummary_includeAllDependencies' - The flag that determines the inclusion of resource dependencies in the
-- returned asset bundle.
--
-- 'jobStatus', 'assetBundleExportJobSummary_jobStatus' - The current status of the export job.
newAssetBundleExportJobSummary ::
  AssetBundleExportJobSummary
newAssetBundleExportJobSummary =
  AssetBundleExportJobSummary'
    { arn = Prelude.Nothing,
      assetBundleExportJobId = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      exportFormat = Prelude.Nothing,
      includeAllDependencies = Prelude.Nothing,
      jobStatus = Prelude.Nothing
    }

-- | The ARN of the export job.
assetBundleExportJobSummary_arn :: Lens.Lens' AssetBundleExportJobSummary (Prelude.Maybe Prelude.Text)
assetBundleExportJobSummary_arn = Lens.lens (\AssetBundleExportJobSummary' {arn} -> arn) (\s@AssetBundleExportJobSummary' {} a -> s {arn = a} :: AssetBundleExportJobSummary)

-- | The ID of the export job.
assetBundleExportJobSummary_assetBundleExportJobId :: Lens.Lens' AssetBundleExportJobSummary (Prelude.Maybe Prelude.Text)
assetBundleExportJobSummary_assetBundleExportJobId = Lens.lens (\AssetBundleExportJobSummary' {assetBundleExportJobId} -> assetBundleExportJobId) (\s@AssetBundleExportJobSummary' {} a -> s {assetBundleExportJobId = a} :: AssetBundleExportJobSummary)

-- | The time that the export job was created.
assetBundleExportJobSummary_createdTime :: Lens.Lens' AssetBundleExportJobSummary (Prelude.Maybe Prelude.UTCTime)
assetBundleExportJobSummary_createdTime = Lens.lens (\AssetBundleExportJobSummary' {createdTime} -> createdTime) (\s@AssetBundleExportJobSummary' {} a -> s {createdTime = a} :: AssetBundleExportJobSummary) Prelude.. Lens.mapping Data._Time

-- | The format for the export job.
assetBundleExportJobSummary_exportFormat :: Lens.Lens' AssetBundleExportJobSummary (Prelude.Maybe AssetBundleExportFormat)
assetBundleExportJobSummary_exportFormat = Lens.lens (\AssetBundleExportJobSummary' {exportFormat} -> exportFormat) (\s@AssetBundleExportJobSummary' {} a -> s {exportFormat = a} :: AssetBundleExportJobSummary)

-- | The flag that determines the inclusion of resource dependencies in the
-- returned asset bundle.
assetBundleExportJobSummary_includeAllDependencies :: Lens.Lens' AssetBundleExportJobSummary (Prelude.Maybe Prelude.Bool)
assetBundleExportJobSummary_includeAllDependencies = Lens.lens (\AssetBundleExportJobSummary' {includeAllDependencies} -> includeAllDependencies) (\s@AssetBundleExportJobSummary' {} a -> s {includeAllDependencies = a} :: AssetBundleExportJobSummary)

-- | The current status of the export job.
assetBundleExportJobSummary_jobStatus :: Lens.Lens' AssetBundleExportJobSummary (Prelude.Maybe AssetBundleExportJobStatus)
assetBundleExportJobSummary_jobStatus = Lens.lens (\AssetBundleExportJobSummary' {jobStatus} -> jobStatus) (\s@AssetBundleExportJobSummary' {} a -> s {jobStatus = a} :: AssetBundleExportJobSummary)

instance Data.FromJSON AssetBundleExportJobSummary where
  parseJSON =
    Data.withObject
      "AssetBundleExportJobSummary"
      ( \x ->
          AssetBundleExportJobSummary'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "AssetBundleExportJobId")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "ExportFormat")
            Prelude.<*> (x Data..:? "IncludeAllDependencies")
            Prelude.<*> (x Data..:? "JobStatus")
      )

instance Prelude.Hashable AssetBundleExportJobSummary where
  hashWithSalt _salt AssetBundleExportJobSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` assetBundleExportJobId
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` exportFormat
      `Prelude.hashWithSalt` includeAllDependencies
      `Prelude.hashWithSalt` jobStatus

instance Prelude.NFData AssetBundleExportJobSummary where
  rnf AssetBundleExportJobSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf assetBundleExportJobId
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf exportFormat
      `Prelude.seq` Prelude.rnf includeAllDependencies
      `Prelude.seq` Prelude.rnf jobStatus

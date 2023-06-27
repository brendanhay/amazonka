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
-- Module      : Amazonka.QuickSight.Types.AssetBundleImportJobSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AssetBundleImportJobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AssetBundleImportFailureAction
import Amazonka.QuickSight.Types.AssetBundleImportJobStatus

-- | A summary of the import job that includes details of the requested
-- job\'s configuration and its current status.
--
-- /See:/ 'newAssetBundleImportJobSummary' smart constructor.
data AssetBundleImportJobSummary = AssetBundleImportJobSummary'
  { -- | The ARN of the import job.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the job. This ID is unique while the job is running. After the
    -- job is completed, you can reuse this ID for another job.
    assetBundleImportJobId :: Prelude.Maybe Prelude.Text,
    -- | The time that the import job was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The failure action for the import job.
    failureAction :: Prelude.Maybe AssetBundleImportFailureAction,
    -- | The current status of the import job.
    jobStatus :: Prelude.Maybe AssetBundleImportJobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetBundleImportJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'assetBundleImportJobSummary_arn' - The ARN of the import job.
--
-- 'assetBundleImportJobId', 'assetBundleImportJobSummary_assetBundleImportJobId' - The ID of the job. This ID is unique while the job is running. After the
-- job is completed, you can reuse this ID for another job.
--
-- 'createdTime', 'assetBundleImportJobSummary_createdTime' - The time that the import job was created.
--
-- 'failureAction', 'assetBundleImportJobSummary_failureAction' - The failure action for the import job.
--
-- 'jobStatus', 'assetBundleImportJobSummary_jobStatus' - The current status of the import job.
newAssetBundleImportJobSummary ::
  AssetBundleImportJobSummary
newAssetBundleImportJobSummary =
  AssetBundleImportJobSummary'
    { arn = Prelude.Nothing,
      assetBundleImportJobId = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      failureAction = Prelude.Nothing,
      jobStatus = Prelude.Nothing
    }

-- | The ARN of the import job.
assetBundleImportJobSummary_arn :: Lens.Lens' AssetBundleImportJobSummary (Prelude.Maybe Prelude.Text)
assetBundleImportJobSummary_arn = Lens.lens (\AssetBundleImportJobSummary' {arn} -> arn) (\s@AssetBundleImportJobSummary' {} a -> s {arn = a} :: AssetBundleImportJobSummary)

-- | The ID of the job. This ID is unique while the job is running. After the
-- job is completed, you can reuse this ID for another job.
assetBundleImportJobSummary_assetBundleImportJobId :: Lens.Lens' AssetBundleImportJobSummary (Prelude.Maybe Prelude.Text)
assetBundleImportJobSummary_assetBundleImportJobId = Lens.lens (\AssetBundleImportJobSummary' {assetBundleImportJobId} -> assetBundleImportJobId) (\s@AssetBundleImportJobSummary' {} a -> s {assetBundleImportJobId = a} :: AssetBundleImportJobSummary)

-- | The time that the import job was created.
assetBundleImportJobSummary_createdTime :: Lens.Lens' AssetBundleImportJobSummary (Prelude.Maybe Prelude.UTCTime)
assetBundleImportJobSummary_createdTime = Lens.lens (\AssetBundleImportJobSummary' {createdTime} -> createdTime) (\s@AssetBundleImportJobSummary' {} a -> s {createdTime = a} :: AssetBundleImportJobSummary) Prelude.. Lens.mapping Data._Time

-- | The failure action for the import job.
assetBundleImportJobSummary_failureAction :: Lens.Lens' AssetBundleImportJobSummary (Prelude.Maybe AssetBundleImportFailureAction)
assetBundleImportJobSummary_failureAction = Lens.lens (\AssetBundleImportJobSummary' {failureAction} -> failureAction) (\s@AssetBundleImportJobSummary' {} a -> s {failureAction = a} :: AssetBundleImportJobSummary)

-- | The current status of the import job.
assetBundleImportJobSummary_jobStatus :: Lens.Lens' AssetBundleImportJobSummary (Prelude.Maybe AssetBundleImportJobStatus)
assetBundleImportJobSummary_jobStatus = Lens.lens (\AssetBundleImportJobSummary' {jobStatus} -> jobStatus) (\s@AssetBundleImportJobSummary' {} a -> s {jobStatus = a} :: AssetBundleImportJobSummary)

instance Data.FromJSON AssetBundleImportJobSummary where
  parseJSON =
    Data.withObject
      "AssetBundleImportJobSummary"
      ( \x ->
          AssetBundleImportJobSummary'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "AssetBundleImportJobId")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "FailureAction")
            Prelude.<*> (x Data..:? "JobStatus")
      )

instance Prelude.Hashable AssetBundleImportJobSummary where
  hashWithSalt _salt AssetBundleImportJobSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` assetBundleImportJobId
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` failureAction
      `Prelude.hashWithSalt` jobStatus

instance Prelude.NFData AssetBundleImportJobSummary where
  rnf AssetBundleImportJobSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf assetBundleImportJobId
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf failureAction
      `Prelude.seq` Prelude.rnf jobStatus
